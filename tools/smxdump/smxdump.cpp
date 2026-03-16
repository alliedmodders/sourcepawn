// vim: set sts=4 ts=8 sw=4 tw=99 et:
// 
// Copyright (C) 2026 AlliedModders LLC
// 
// This file is part of SourcePawn. SourcePawn is free software: you can
// redistribute it and/or modify it under the terms of the GNU General Public
// License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
//
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.
//
#include <sp_vm_api.h>
#include <amtl/experimental/am-argparser.h>
#include "vm/environment.h"
#include "vm/smx-image.h"
#include "vm/opcodes.h"

using namespace ke;
using namespace ke::args;
using namespace sp;
using namespace SourcePawn;

Environment* sEnv;

StringOption filename("file", "SMX file");
ToggleOption show_name_offsets(nullptr, "--show-name-offsets", Some(false),
                               "Show all name offsets");

class ShellDebugListener : public IDebugListener
{
public:
  void ReportError(const IErrorReport& report, IFrameIterator& iter) override {
    fprintf(stdout, "Exception thrown: %s\n", report.Message());
  }

  void OnDebugSpew(const char* msg, ...) override {
#if !defined(NDEBUG) && defined(DEBUG)
    va_list ap;
    va_start(ap, msg);
    vfprintf(stderr, msg, ap);
    va_end(ap);
#endif
  }
};

class DumpTool final {
  public:
    explicit DumpTool(const char* file, std::unique_ptr<SmxImage> smx)
      : smx_(std::move(smx)),
        file_(file)
    {}

    void Dump() {
        DumpHeaders();
        DumpPublics();
        DumpPubvars();
        DumpNatives();
        DumpData();
        DumpCode();
        DumpRttiMethods();

        if (!smx_->rtti_methods())
            DumpLegacyCode();
    }

    void DumpHeaders() {
        fprintf(stdout, "; %s\n", file_.c_str());
        fprintf(stdout, "; SMX Header\n");
        fprintf(stdout, ".magic = 0x%x\n", smx_->hdr()->magic);
        fprintf(stdout, ".version = 0x%x\n", smx_->hdr()->version);
        fprintf(stdout, ".compression = 0x%x\n", smx_->hdr()->compression);
        fprintf(stdout, ".disksize = 0x%x\n", smx_->hdr()->disksize);
        fprintf(stdout, ".imagesize = 0x%x\n", smx_->hdr()->imagesize);
        fprintf(stdout, ".sections = 0x%x\n", smx_->hdr()->imagesize);
        fprintf(stdout, ".stringtab = 0x%x\n", smx_->hdr()->imagesize);
        fprintf(stdout, ".dataoffs = 0x%x\n", smx_->hdr()->imagesize);
        fprintf(stdout, "\n");
    }

    void DumpPublics() {
        auto publics = smx_->publics();
        if (publics.length() == 0)
            return;

        fprintf(stdout, ".publics\n");
        fprintf(stdout, "{\n");
        for (uint32_t i = 0; i < publics.length(); i++) {
            const char* name = smx_->names() + publics[i].name;
            fprintf(stdout, "    %u: %s @ 0x%x", i, name, publics[i].address);
            if (show_name_offsets.value())
                fprintf(stdout, " ; name_offset = %u", publics[i].name);
            fprintf(stdout, "\n");
        }
        fprintf(stdout, "}\n");
    }

    void DumpNatives() {
        auto natives = smx_->natives();
        if (natives.length() == 0)
            return;

        fprintf(stdout, ".natives\n");
        fprintf(stdout, "{\n");
        for (uint32_t i = 0; i < natives.length(); i++) {
            const char* name = smx_->names() + natives[i].name;
            fprintf(stdout, "    %u: %s", i, name);
            if (show_name_offsets.value())
                fprintf(stdout, " ; name_offset = %u", natives[i].name);
            fprintf(stdout, "\n");
        }
        fprintf(stdout, "}\n");
    }

    void DumpPubvars() {
        auto pubvars = smx_->pubvars();
        if (pubvars.length() == 0)
            return;

        fprintf(stdout, ".pubvars\n");
        fprintf(stdout, "{\n");
        for (uint32_t i = 0; i < pubvars.length(); i++) {
            const char* name = smx_->names() + pubvars[i].name;
            fprintf(stdout, "    %u: %s @ 0x%x", i, name, pubvars[i].address);
            if (show_name_offsets.value())
                fprintf(stdout, " ; name_offset = %u", pubvars[i].name);
            fprintf(stdout, "\n");
        }
        fprintf(stdout, "}\n");
    }

    void DumpData() {
        auto data = smx_->data();

        fprintf(stdout, ".data\n");
        fprintf(stdout, "{\n");
        fprintf(stdout, "    .datasize: %u\n", data->datasize);
        fprintf(stdout, "    .memsize: %u\n", data->memsize);
        fprintf(stdout, "}\n");
    }

    void DumpCode() {
        auto code = smx_->code();

        fprintf(stdout, ".code\n");
        fprintf(stdout, "{\n");
        fprintf(stdout, "    .codesize = %u\n", code->codesize);
        fprintf(stdout, "    .cellsize = %u\n", code->cellsize);
        fprintf(stdout, "    .codeversion = %u\n", code->codeversion);
        if (code->main)
            fprintf(stdout, "    .main = 0x%x\n", code->main);
        fprintf(stdout, "    .code = 0x%x\n", code->code);
        if (code->flags & CODEFLAG_DEBUG)
            fprintf(stdout, "    .flags = debug\n");
        else if (code->flags)
            fprintf(stdout, "    .flags = %x ; unknown\n", code->flags);
        if (code->codeversion >= SmxConsts::CODE_VERSION_FEATURE_MASK) {
            if (code->features & SmxConsts::kCodeFeatureDeprecated0)
                fprintf(stdout, "    .feature = deprecated0\n");
            if (code->features & SmxConsts::kCodeFeatureDirectArrays)
                fprintf(stdout, "    .feature = direct_arrays\n");
            if (code->features & SmxConsts::kCodeFeatureHeapScopes)
                fprintf(stdout, "    .feature = heap_scopes\n");
            if (code->features & SmxConsts::kCodeFeatureNullFunctions)
                fprintf(stdout, "    .feature = null_functions\n");
            if (code->features & SmxConsts::kCodeFeatureTypedOps)
                fprintf(stdout, "    .feature = typed_ops\n");

            uint32_t known_features =
                SmxConsts::kCodeFeatureDeprecated0 |
                SmxConsts::kCodeFeatureDirectArrays |
                SmxConsts::kCodeFeatureHeapScopes |
                SmxConsts::kCodeFeatureNullFunctions |
                SmxConsts::kCodeFeatureTypedOps;
            if (code->features & ~known_features)
                fprintf(stdout, "    .feature = %x ; unknown\n", (code->features & ~known_features));
        }
        fprintf(stdout, "}\n");
    }

    void DumpRttiMethods() {
        auto methods = smx_->rtti_methods();
        if (!methods)
            return;

        for (uint32_t i = 0; i < methods->row_count; i++) {
            auto method = smx_->getRttiRow<smx_rtti_method>(methods, i);
            fprintf(stdout, "\n");
            fprintf(stdout, ".method %s ; index %u", smx_->names() + method->name, i);
            if (show_name_offsets.value())
                fprintf(stdout, ", name_offset = %u", method->name);
            fprintf(stdout, "\n");
            fprintf(stdout, "{\n");
            fprintf(stdout, "    .pcode_start = 0x%x\n", method->pcode_start);
            fprintf(stdout, "    .pcode_end = 0x%x\n", method->pcode_end);
            DumpCodeRange<false>(method->pcode_start, method->pcode_end);
            fprintf(stdout, "}\n");
        }
    }

    void DumpLegacyCode() {
        auto code = smx_->DescribeCode();
        DumpCodeRange<true>(0, code.length);
    }

    template <bool SearchForMethods>
    void DumpCodeRange(cell_t pcode_start, cell_t pcode_end) {
        auto code = smx_->DescribeCode();

        auto start = reinterpret_cast<const cell_t*>(code.bytes + pcode_start);
        auto cip = start;
        auto code_end = reinterpret_cast<const cell_t*>(code.bytes + pcode_end);
        auto method_start = cip;

        while (cip < code_end) {
            OPCODE op = (OPCODE)*cip;

            if (SearchForMethods && (cip == start || op == OP_PROC)) {
                std::string method_name;
                uint32_t offset = (cip - start) * sizeof(cell_t);
                if (auto name = smx_->LookupFunction(offset))
                    method_name = name;
                else
                    method_name = ke::StringPrintf("unknown_method_%u", offset);

                if (cip != start)
                    fprintf(stdout, "\n}\n");

                fprintf(stdout, ".method %s\n", method_name.c_str());
                fprintf(stdout, "{\n");
                fprintf(stdout, "    .pcode_start = 0x%x\n", offset);

                method_start = cip;
            }

            const char* name = nullptr;
            if (op < OPCODES_LAST)
                name = GetOpcodeName(op);

            // Terminate previous line.
            if (cip != method_start)
                fprintf(stdout, "\n");

            fprintf(stdout, "    %04x: ", (uint32_t)((cip - method_start) * sizeof(cell_t)));
            if (name)
                fprintf(stdout, "%s", name);
            else
                fprintf(stdout, "unknown_op_%u", op);

            DumpOpcode(method_start, cip, op);

            if (op == OP_CASETBL)
                cip += GetCaseTableSize(reinterpret_cast<const uint8_t*>(cip));
            else if (name)
                cip += GetOpcodeSize(op);
            else
                cip++;
        }
        if (SearchForMethods)
            fprintf(stdout, "\n}\n");
        fprintf(stdout, "\n");
    }

    void DumpOpcode(const cell_t* method_start, const cell_t* cip, OPCODE op) {
        switch (op) {
            case OP_PUSH_C:
            case OP_PUSH_ADR:
            case OP_SHL_C_PRI:
            case OP_SHL_C_ALT:
            case OP_ADD_C:
            case OP_SMUL_C:
            case OP_EQ_C_PRI:
            case OP_EQ_C_ALT:
            case OP_TRACKER_PUSH_C:
            case OP_STACK:
            case OP_PUSH_S:
            case OP_HEAP:
            case OP_GENARRAY:
            case OP_GENARRAY_Z:
            case OP_CONST_PRI:
            case OP_CONST_ALT:
            case OP_LOAD_S_PRI:
            case OP_LOAD_S_ALT:
            case OP_STOR_S_PRI:
            case OP_STOR_S_ALT:
            case OP_ADDR_PRI:
            case OP_ADDR_ALT:
            case OP_MOVS:
            case OP_CVT_I64:
            case OP_INVERT_I64:
            case OP_NEG_I64:
            case OP_SMUL_I64:
            case OP_ADD_I64:
            case OP_SUB_ALT_I64:
            case OP_SHL_I64:
            case OP_SSHR_I64:
            case OP_SHR_I64:
            case OP_EQ_I64:
            case OP_NEQ_I64:
            case OP_OR_I64:
            case OP_AND_I64:
            case OP_XOR_I64:
                fprintf(stdout, " %d", cip[1]);
                break;

            case OP_JUMP:
            case OP_JZER:
            case OP_JNZ:
            case OP_JEQ:
            case OP_JNEQ:
            case OP_JSLESS:
            case OP_JSGRTR:
            case OP_JSGEQ:
            case OP_JSLEQ:
            {
                auto target = smx_->DescribeCode().bytes + cip[1];
                auto diff = uint32_t(target - reinterpret_cast<const uint8_t*>(method_start));
                fprintf(stdout, " %04x ; %x", diff, cip[1]);
                break;
            }

            case OP_SYSREQ_C:
            case OP_SYSREQ_N:
            {
                uint32_t index = cip[1];
                if (index < smx_->natives().length())
                    fprintf(stdout, " %s", smx_->names() + smx_->natives()[index].name);
                else
                    fprintf(stdout, " unknown_native_%u", index);
                if (op == OP_SYSREQ_N)
                    fprintf(stdout, " (%u)", cip[2]);
                break;
            }

            case OP_PUSH2_C:
            case OP_PUSH2:
            case OP_PUSH2_S:
            case OP_PUSH2_ADR:
            case OP_SDIV_ALT_I64:
                fprintf(stdout, " %d, %d", cip[1], cip[2]);
                break;

            case OP_PUSH3_C:
            case OP_PUSH3:
            case OP_PUSH3_S:
            case OP_PUSH3_ADR:
            case OP_STOR_S_I64_C:
                fprintf(stdout, " %d, %d, %d", cip[1], cip[2], cip[3]);
                break;

            case OP_PUSH4_C:
            case OP_PUSH4:
            case OP_PUSH4_S:
            case OP_PUSH4_ADR:
                fprintf(stdout, " %d, %d, %d, %d", cip[1], cip[2], cip[3], cip[4]);
                break;

            case OP_PUSH5_C:
            case OP_PUSH5:
            case OP_PUSH5_S:
            case OP_PUSH5_ADR:
                fprintf(stdout, " %d, %d, %d, %d, %d", cip[1], cip[2], cip[3], cip[4], cip[5]);
                break;

            case OP_INITARRAY_PRI:
            case OP_INITARRAY_ALT:
                fprintf(stdout, " %d %d %d %d %d", cip[1], cip[2], cip[3], cip[4], cip[5]);
                break;

            default:
                break;
        }
    }

  private:
    std::unique_ptr<SmxImage> smx_;
    std::string file_;
};

static int Dump(const char* file) {
    std::unique_ptr<FILE, decltype(&::fclose)> fp(fopen(file, "rb"), ::fclose);
    auto smx = std::make_unique<SmxImage>(fp.get());
    if (!smx->validate()) {
        fprintf(stderr, "Could not parse %s: %s\n", file, smx->errorMessage());
        return 1;
    }

    DumpTool tool(file, std::move(smx));
    tool.Dump();
    return 0;
}

int main(int argc, char** argv)
{
  Parser parser("SourcePawn SMX disassembly");

  if (!parser.parse(argc, argv)) {
    parser.usage(stderr, argc, argv);
    return 1;
  }

  if ((sEnv = Environment::New()) == nullptr) {
    fprintf(stderr, "Could not initialize ISourcePawnEngine2\n");
    return 1;
  }

  ShellDebugListener debug;
  sEnv->SetDebugger(&debug);

  int errcode = Dump(filename.value().c_str());

  sEnv->Shutdown();
  delete sEnv;
  return errcode;
}
