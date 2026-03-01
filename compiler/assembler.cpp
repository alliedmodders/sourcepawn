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
 *
 *  Version: $Id$
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

struct function_entry {
    function_entry() {}

    function_entry(function_entry&& other) = default;
    function_entry& operator =(function_entry&& other) = default;

    function_entry(const function_entry& other) = delete;
    function_entry& operator =(const function_entry& other) = delete;

    FunctionDecl* decl = nullptr;
    std::string name;
};

typedef SmxListSection<sp_file_natives_t> SmxNativeSection;
typedef SmxListSection<sp_file_publics_t> SmxPublicSection;
typedef SmxListSection<sp_file_pubvars_t> SmxPubvarSection;
typedef SmxBlobSection<sp_file_data_t> SmxDataSection;
typedef SmxBlobSection<sp_file_code_t> SmxCodeSection;

Assembler::Assembler(CompileContext& cc, CodeGenerator& cg)
  : cc_(cc),
    cg_(cg)
{
}

void
Assembler::Assemble(SmxByteBuffer* buffer)
{
    SmxBuilder builder;
    RefPtr<SmxNativeSection> natives = new SmxNativeSection(".natives");
    RefPtr<SmxPublicSection> publics = new SmxPublicSection(".publics");
    RefPtr<SmxPubvarSection> pubvars = new SmxPubvarSection(".pubvars");
    RefPtr<SmxDataSection> data = new SmxDataSection(".data");
    RefPtr<SmxCodeSection> code = new SmxCodeSection(".code");

    RefPtr<SmxNameTable> names = cg_.names();
    auto& rtti = cg_.rtti();

    std::vector<function_entry> functions;
    std::unordered_set<Decl*> symbols;

    // Sort globals.
    std::vector<Decl*> global_symbols;
    cc_.globals()->ForEachSymbol([&](Decl* decl) -> void {
        global_symbols.push_back(decl);

        // This is only to assert that we embedded pointers properly in the assembly buffer.
        symbols.emplace(decl);
    });
    for (const auto& decl : cc_.functions()) {
        if (symbols.count(decl))
            continue;
        if (decl->canonical() != decl)
            continue;
        global_symbols.push_back(decl);
        symbols.emplace(decl);
    }

    std::sort(global_symbols.begin(), global_symbols.end(),
              [](const Decl* a, const Decl *b) -> bool {
        return a->name()->str() < b->name()->str();
    });

    // Build the easy symbol tables.
    for (const auto& decl : global_symbols) {
        if (auto fun = decl->as<FunctionDecl>()) {
            if (fun->is_native())
                continue;

            if (!fun->body())
                continue;
            if (!fun->is_live())
                continue;
            if (fun->canonical() != fun)
                continue;

            function_entry entry;
            entry.decl = fun;
            if (fun->is_public()) {
                entry.name = fun->name()->str();
            } else {
                // Create a private name.
                entry.name = ke::StringPrintf(".%d.%s", fun->cg()->label.offset(), fun->name()->chars());
            }

            functions.emplace_back(std::move(entry));
        } else if (auto var = decl->as<VarDecl>()) {
            if (var->is_public() || (var->is_used() && !var->as<ConstDecl>())) {
                sp_file_pubvars_t& pubvar = pubvars->add();
                pubvar.address = var->addr();
                pubvar.name = names->add(var->name());
            }
        }
    }

    // The public list must be sorted.
    std::sort(functions.begin(), functions.end(),
              [](const function_entry& a, const function_entry& b) -> bool {
        return a.name < b.name;
    });
    for (size_t i = 0; i < functions.size(); i++) {
        function_entry& f = functions[i];

        assert(f.decl->cg()->label.offset() > 0);
        assert(f.decl->impl());
        assert(f.decl->cg()->pcode_end > f.decl->cg()->label.offset());

        sp_file_publics_t& pubfunc = publics->add();
        pubfunc.address = f.decl->cg()->label.offset();
        pubfunc.name = names->add(*cc_.atoms(), f.name.c_str());

        auto id = (uint32_t(i) << 1) | 1;
        if (!Label::ValueFits(id))
            report(421);
        cg_.LinkPublicFunction(f.decl, id);

        rtti.add_method(f.decl);
    }

    // Populate the native table.
    for (size_t i = 0; i < cg_.native_list().size(); i++) {
        FunctionDecl* sym = cg_.native_list()[i];
        assert(size_t(sym->cg()->label.offset()) == i);

        sp_file_natives_t& entry = natives->add();
        entry.name = names->add(sym->name());

        rtti.add_native(sym);
    }

    // Set up the code section.
    code->header().codesize = cg_.code_size();
    code->header().cellsize = sizeof(cell);
    code->header().codeversion = SmxConsts::CODE_VERSION_FEATURE_MASK;
    code->header().flags = CODEFLAG_DEBUG;
    code->header().main = 0;
    code->header().code = sizeof(sp_file_code_t);
    code->header().features = SmxConsts::kCodeFeatureDirectArrays |
                              SmxConsts::kCodeFeatureHeapScopes |
                              SmxConsts::kCodeFeatureNullFunctions |
                              SmxConsts::kCodeFeatureTypedOps;
    code->setBlob(cg_.code_ptr(), cg_.code_size());

    // Set up the data section. Note pre-SourceMod 1.7, the |memsize| was
    // computed as AMX::stp, which included the entire memory size needed to
    // store the file. Here (in 1.7+), we allocate what is actually needed
    // by the plugin.
    data->header().datasize = cg_.data_size();
    data->header().memsize = cg_.data_size() + cg_.DynamicMemorySize();
    data->header().data = sizeof(sp_file_data_t);
    data->setBlob(cg_.data_ptr(), cg_.data_size());

    // Add tables in the same order SourceMod 1.6 added them.
    builder.add(code);
    builder.add(data);
    builder.add(publics);
    builder.add(pubvars);
    builder.add(natives);
    builder.add(names);
    rtti.finish(builder);

    builder.write(buffer);
}

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
    Assembler assembler(cc, cg);

    SmxByteBuffer buffer;
    assembler.Assemble(&buffer);

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
