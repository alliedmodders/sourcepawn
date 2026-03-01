// vim: set sts=4 ts=8 sw=4 tw=99 et:
/*  Pawn compiler - Binary code generation (the "assembler")
 *
 *  Copyright (c) ITB CompuPhase, 1997-2006
 *
 *  This software is provided "as-is", without any express or implied warranty.
 *  In no event will the authors be held liable for any damages arising from
 *  the use of this software.
 *
 *  Permission is granted to anyone to use this software for any purpose,
 *  including commercial applications, and to alter it and redistribute it
 *  freely, subject to the following restrictions:
 *
 *  1.  The origin of this software must not be misrepresented; you must not
 *      claim that you wrote the original software. If you use this software in
 *      a product, an acknowledgment in the product documentation would be
 *      appreciated but is not required.
 *  2.  Altered source versions must be plainly marked as such, and must not be
 *      misrepresented as being the original software.
 *  3.  This notice may not be removed or altered from any source distribution.
 */
#include <assert.h>
#include <ctype.h>
#include <stddef.h> /* for macro offsetof() */
#include <stdio.h>
#include <stdlib.h> /* for macro max() */
#include <string.h>

#include <algorithm>
#include <unordered_set>
#include <vector>

#include <amtl/am-hashmap.h>
#include <amtl/am-string.h>
#include <smx/smx-v1-opcodes.h>
#include <smx/smx-v1.h>
#include <sp_vm_api.h>
#include <zlib/zlib.h>
#include "assembler.h"
#include "compile-context.h"
#include "compile-options.h"
#include "errors.h"
#include "lexer.h"
#include "parse-node.h"
#include "rtti-builder.h"
#include "sc.h"
#include "scopes.h"
#include "sctracker.h"
#include "symbols.h"
#include "types.h"

namespace sp {
namespace cc {

using namespace SourcePawn;
using namespace ke;

static void
FailedValidation(const std::string& message)
{
    fprintf(stderr, "Binary validation failed: %s\n", message.c_str());
    fprintf(stderr, "Internal compilation error detected. Please file a bug:\n");
    fprintf(stderr, "https://github.com/alliedmodders/sourcepawn/issues/new\n");
    exit(1);
}

static void
VerifyBinary(const char* file, void* buffer, size_t size)
{
    std::unique_ptr<ISourcePawnEnvironment> env(ISourcePawnEnvironment::New());
    if (!env)
        FailedValidation("could not initialize environment");

    auto api = env->APIv2();

    char msgbuf[255];
    std::unique_ptr<IPluginRuntime> rt(api->LoadBinaryFromMemory(file, (uint8_t*)buffer, size,
                                                                 nullptr, msgbuf,  sizeof(msgbuf)));
    if (!rt)
        FailedValidation(msgbuf);

    ExceptionHandler eh(api);
    if (!rt->PerformFullValidation()) {
        const char* message = eh.HasException() ? eh.Message() : "unknown error";
        FailedValidation(message);
    }
}

static bool
splat_to_binary(CompileContext& cc, const char* binfname, void* bytes, size_t size)
{
    if (cc.verify_output())
        VerifyBinary(binfname, bytes, size);

    // Note: error 161 will setjmp(), which skips destructors :(
    FILE* fp = fopen(binfname, "wb");
    if (!fp) {
        report(419) << binfname;
        return false;
    }
    if (fwrite(bytes, 1, size, fp) != size) {
        fclose(fp);
        report(419) << binfname;
        return false;
    }
    fclose(fp);
    return true;
}

bool
assemble(CompileContext& cc, CodeGenerator& cg, const char* binfname, int compression_level)
{
    SmxByteBuffer buffer;
    cg.smx().write(&buffer);

    // Buffer compression logic.
    sp_file_hdr_t* header = (sp_file_hdr_t*)buffer.bytes();

    if (compression_level) {
        size_t region_size = header->imagesize - header->dataoffs;
        size_t zbuf_max = compressBound(region_size);
        std::unique_ptr<Bytef[]> zbuf = std::make_unique<Bytef[]>(zbuf_max);

        uLong new_disksize = zbuf_max;
        int err = compress2(zbuf.get(), &new_disksize, (Bytef*)(buffer.bytes() + header->dataoffs),
                            region_size, compression_level);
        if (err == Z_OK) {
            header->disksize = new_disksize + header->dataoffs;
            header->compression = SmxConsts::FILE_COMPRESSION_GZ;

            ByteBuffer new_buffer;
            new_buffer.writeBytes(buffer.bytes(), header->dataoffs);
            new_buffer.writeBytes(zbuf.get(), new_disksize);

            return splat_to_binary(cc, binfname, new_buffer.bytes(), new_buffer.size());
        }

        printf("Unable to compress, error %d\n", err);
        printf("Falling back to no compression.\n");
    }

    header->disksize = 0;
    header->compression = SmxConsts::FILE_COMPRESSION_NONE;

    return splat_to_binary(cc, binfname, buffer.bytes(), buffer.size());
}

} // namespace cc
} // namespace sp
