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
#include <inttypes.h>
#include <sp_vm_api.h>
#include <amtl/experimental/am-argparser.h>
#include "vm/environment.h"
#include "vm/smx-image.h"
#include "vm/binary-reader.h"
#include "vm/legacy/opcodes.h"
#include "vm/v2/opcodes.h"

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
        DumpRttiGlobals();

        if (!smx_->rtti_methods())
            DumpLegacyCode();

        DumpRttiEnums();
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

        bool is_v2 = smx_->hdr()->version >= SmxConsts::SP_VERSION_2;

        for (uint32_t i = 0; i < methods->row_count; i++) {
            auto method = smx_->getRttiRow<smx_rtti_method>(methods, i);
            fprintf(stdout, ".method %s ; index %u", smx_->names() + method->name, i);
            if (show_name_offsets.value())
                fprintf(stdout, ", name_offset = %u", method->name);

            bool is_native = false;
            if (methods->row_size >= 24)
                is_native = !!(method->flags & kRttiMethod_Native);

            fprintf(stdout, "\n");
            fprintf(stdout, "{\n");
            if (!is_native) {
                fprintf(stdout, "    .pcode_start = 0x%x\n", method->pcode_start);
                fprintf(stdout, "    .pcode_end = 0x%x\n", method->pcode_end);
            }

            if (method->signature)
                DumpSignature(method->signature);

            if (methods->row_size >= 24) {
                if (method->flags & kRttiMethod_Native) {
                    fprintf(stdout, "    .flags = native\n");
                } else {
                    uint8_t visibility = method->flags & kRttiMethodVisibilityMask;
                    if (visibility == kRttiMethodVisibility_Private)
                        fprintf(stdout, "    .visibility = private\n");
                    else if (visibility == kRttiMethodVisibility_Public)
                        fprintf(stdout, "    .visibility = public\n");
                    else
                        fprintf(stdout, "    .visibility = unknown_%u\n", visibility);

                    if (method->flags & kRttiMethod_GlobalCtor)
                        fprintf(stdout, "    .flags = global_ctor\n");
                }
            }

            if (!is_native) {
                DumpLocals(method);
                if (is_v2)
                    DumpCodeRangeV2(method->pcode_start, method->pcode_end);
                else
                    DumpCodeRangeV1<false>(method->pcode_start, method->pcode_end);
            }
            fprintf(stdout, "}\n");
        }
    }

    void DumpSignature(uint32_t offset) {
        auto rtti = smx_->GetTypeParser(offset);

        uint32_t arg_count;
        if (!rtti.ReadFunctionSignatureArgCount(&arg_count))
            return;

        uint8_t b;
        bool variadic = false;
        if (rtti.GetByte(&b) && b == cb::kLegacyVariadic) {
            variadic = true;
            rtti.NextByte();
        }

        std::string ret_type = DumpType(rtti);
        fprintf(stdout, "    .signature %s(", ret_type.c_str());
        for (uint32_t j = 0; j < arg_count; j++) {
            if (j > 0)
                fprintf(stdout, ", ");
            fprintf(stdout, "%s", DumpType(rtti).c_str());
        }
        if (variadic) {
            if (arg_count > 0)
                fprintf(stdout, ", ");
            fprintf(stdout, "...");
        }
        fprintf(stdout, ")\n");
    }

    void DumpLocals(const smx_rtti_method* method) {
        if (smx_->code()->codeversion < SmxConsts::CODE_VERSION_TYPED_STACK || !method->locals)
            return;

        uint16_t locals;
        auto rtti = smx_->GetTypeParser(method->locals);
        if (!rtti.ReadLocalSlotCount(&locals)) {
            fprintf(stdout, "    .locals ERROR\n");
            return;
        }

        for (uint32_t i = 0; i < locals; i++) {
            fprintf(stdout, "    .locals %d ", i);
            auto type = DumpType(rtti);
            if (!type.empty())
                fprintf(stdout, "%s", type.c_str());
            else
                fprintf(stdout, "ERROR");
            fprintf(stdout, "\n");
        }
    }

    std::string DumpType(FastRtti& rtti) {
        bool is_const = false;
        uint8_t b;
        if (!rtti.GetNextByte(&b))
            return {};
        if (b == cb::kConst) {
            is_const = true;
            if (!rtti.GetNextByte(&b))
                return {};
        }

        bool by_ref = false;
        if (b == cb::kByRef) {
            by_ref = true;
            if (!rtti.GetNextByte(&b))
                return {};
        }

        std::string type_inner;
        std::string type_outer;
        while (type_inner.empty()) {
            switch (b) {
                case cb::kBool:
                    type_inner = "bool";
                    break;
                case cb::kInt32:
                    type_inner = "int";
                    break;
                case cb::kInt64:
                    type_inner = "int64";
                    break;
                case cb::kFloat32:
                    type_inner = "float";
                    break;
                case cb::kChar8:
                    type_inner = "char";
                    break;
                case cb::kAny:
                    type_inner = "any";
                    break;
                case cb::kVoid:
                    type_inner = "void";
                    break;
                case cb::kFlatArray:
                case cb::kFixedArray:
                {
                    uint32_t size;
                    if (!rtti.ReadUint32_Leb128(&size))
                        return {};
                    type_outer += ke::StringPrintf("[%u]", size);
                    if (!rtti.GetNextByte(&b))
                        return {};
                    continue;
                }
                case cb::kArray:
                    type_outer += "[]";
                    if (!rtti.GetNextByte(&b))
                        return {};
                    continue;
                case cb::kEnum:
                {
                    uint32_t value;
                    if (!rtti.ReadUint32_Leb128(&value))
                        return {};

                    if (!smx_->rtti_enums()) {
                        type_inner = ke::StringPrintf("enum_%u", value);
                    } else {
                        auto entry = smx_->getRttiRow<smx_rtti_enum>(smx_->rtti_enums(), value);
                        type_inner = ke::StringPrintf("enum %s", smx_->names() + entry->name);
                    }
                    break;
                }
                case cb::kTypeset:
                {
                    uint32_t value;
                    if (!rtti.ReadUint32_Leb128(&value))
                        return {};
                    type_inner = "typeset todo";
                    break;
                }
                case cb::kEnumStruct:
                {
                    uint32_t value;
                    if (!rtti.ReadUint32_Leb128(&value))
                        return {};
                    type_inner = "enum struct todo";
                    break;
                }
                case cb::kFunctionPtr:
                {
                    uint32_t value;
                    if (!rtti.ReadUint32_Leb128(&value))
                        return {};
                    type_inner = "fn todo";
                    break;
                }
                default:
                    assert(false);
            }
        }

        std::string out;
        if (is_const)
            out = "const ";
        out += type_inner;
        if (by_ref)
            out += "&";
        return out + type_outer;
    }

    void DumpRttiEnums() {
        auto rtti_enums = smx_->rtti_enums();
        if (!rtti_enums || rtti_enums->row_count == 0)
            return;

        fprintf(stdout, ".rtti_enums\n");
        fprintf(stdout, "{\n");
        for (uint32_t i = 0; i < rtti_enums->row_count; i++) {
            auto entry = smx_->getRttiRow<smx_rtti_enum>(rtti_enums, i);
            fprintf(stdout, "    %u: %s\n", i, smx_->names() + entry->name);
        }
        fprintf(stdout, "}\n");
    }

    void DumpRttiGlobals() {
        auto globals = smx_->rtti_globals();
        if (!globals)
            return;

        fprintf(stdout, ".rtti_globals\n");
        fprintf(stdout, "{\n");
        for (uint32_t i = 0; i < globals->row_count; i++) {
            auto global = smx_->getRttiRow<smx_rtti_global>(globals, i);
            fprintf(stdout, "    %u: %s ", i, smx_->names() + global->name);

            if (global->flags & kRttiGlobal_Public)
                fprintf(stdout, "[public] ");

            auto rtti = smx_->GetTypeIdParser(global->type_id);
            fprintf(stdout, "%s", DumpType(rtti).c_str());
            fprintf(stdout, "\n");
        }
        fprintf(stdout, "}\n");
    }

    void DumpLegacyCode() {
        auto code = smx_->DescribeCode();
        if (smx_->hdr()->version < SmxConsts::SP_VERSION_2)
            DumpCodeRangeV1<true>(0, code.length);
    }

    template <bool SearchForMethods>
    void DumpCodeRangeV1(cell_t pcode_start, cell_t pcode_end) {
        using namespace sp::v1;
        auto code = smx_->DescribeCode();

        auto start = reinterpret_cast<const cell_t*>(code.bytes + pcode_start);
        auto cip = start;
        auto code_end = reinterpret_cast<const cell_t*>(code.bytes + pcode_end);
        auto method_start = cip;

        while (cip < code_end) {
            OPCODE op = (OPCODE)*cip;

            if (SearchForMethods && (cip == start || op == OP_PROC)) {
                std::string method_name;
                uint32_t offset = (cip - (const cell_t*)code.bytes) * sizeof(cell_t);
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

            DumpOpcodeV1(method_start, cip, op);

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

    void DumpOpcodeV1(const cell_t* method_start, const cell_t* cip, v1::OPCODE op) {
        using namespace sp::v1;
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

            case OP_CALL:
            {
                const char* name = smx_->LookupFunction(cip[1]);
                if (name)
                    fprintf(stdout, " %s", name);
                else
                    fprintf(stdout, " unknown_function_%x", cip[1]);
                break;
            }

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
                if (op == OP_SYSREQ_N)
                    fprintf(stdout, " %u", cip[2]);
                if (index < smx_->natives().length())
                    fprintf(stdout, " %s", smx_->names() + smx_->natives()[index].name);
                else
                    fprintf(stdout, " unknown_native_%u", index);
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

    void DumpCodeRangeV2(uint32_t pcode_start, uint32_t pcode_end) {
        using namespace sp::v2;
        auto code = smx_->DescribeCode();

        auto start = code.bytes + pcode_start;
        auto cip = start;
        auto code_end = code.bytes + pcode_end;
        auto method_start = cip;

        while (cip < code_end) {
            OPCODE op = (OPCODE)*cip;

            const char* name = nullptr;
            if (op < OPCODES_LAST)
                name = GetOpcodeName(op);

            uint32_t line;
            uint32_t offset = uint32_t(cip - code.bytes);
            if (smx_->IsLineBoundary(offset) && smx_->LookupLine(offset, &line)) {
                if (cip != method_start)
                    fprintf(stdout, "\n");
                fprintf(stdout, "    ; line %u\n", line);
            } else if (cip != method_start) {
                fprintf(stdout, "\n");
            }

            fprintf(stdout, "    %04x: ", (uint32_t)(cip - method_start));
            if (name)
                fprintf(stdout, "%s", name);
            else
                fprintf(stdout, "unknown_op_%u", op);

            DumpOpcodeV2(method_start, cip, op);

            if (op == OP_SWITCH)
                cip += GetSwitchOpcodeSize(cip);
            else if (name)
                cip += GetOpcodeSize(op);
            else
                cip++;
        }
        fprintf(stdout, "\n");
    }

    void PrintEscaped(std::string_view s) {
        for (char c : s) {
            unsigned char uc = (unsigned char)c;
            if (uc == '\"') fprintf(stdout, "\\\"");
            else if (uc == '\\') fprintf(stdout, "\\\\");
            else if (uc == '\n') fprintf(stdout, "\\n");
            else if (uc == '\r') fprintf(stdout, "\\r");
            else if (uc == '\t') fprintf(stdout, "\\t");
            else if (uc >= 32 && uc <= 126) fputc(uc, stdout);
            else fprintf(stdout, "\\x%02x", uc);
        }
    }

    void DumpString(uint16_t index) {
        auto table = smx_->rtti_stringpool();
        if (!table || index >= table->row_count) {
            fprintf(stdout, " unknown_string_%u", index);
            return;
        }

        auto entry = smx_->getRttiRow<smx_rtti_string>(table, index);
        auto blob = smx_->ReadDataBlob(entry->offset);
        if (!blob) {
            fprintf(stdout, " <invalid_blob_0x%x>", entry->offset);
            return;
        }

        std::string_view s = *blob;
        fprintf(stdout, " \"");
        if (s.length() > 60) {
            PrintEscaped(s.substr(0, 57));
            fprintf(stdout, "...");
        } else {
            PrintEscaped(s);
        }
        fprintf(stdout, "\"");
    }

    void DumpOpcodeV2(const uint8_t* method_start, const uint8_t* cip, v2::OPCODE op) {
        using namespace sp::v2;
        BinaryReader reader(cip + 1);

        switch (op) {
            case OP_PUSH_C:
                fprintf(stdout, " %d", reader.read<cell_t>());
                break;

            case OP_LOAD_GLB:
            case OP_STOR_GLB:
            case OP_ADDR_GLB:
            {
                uint16_t index = reader.read<uint16_t>();
                auto globals = smx_->rtti_globals();
                if (globals && index < globals->row_count) {
                    auto global = smx_->getRttiRow<smx_rtti_global>(globals, index);
                    fprintf(stdout, " %s", smx_->names() + global->name);
                } else {
                    fprintf(stdout, " unknown_global_%u", index);
                }
                break;
            }

            case OP_LOAD_STR:
                DumpString(reader.read<uint16_t>());
                break;

            case OP_PUSH_C_I8:
                fprintf(stdout, " %d", (int)reader.read<int8_t>());
                break;

            case OP_PUSH_C_I64:
                fprintf(stdout, " %" PRId64, reader.read<int64_t>());
                break;

            case OP_PUSH_C_F32:
                fprintf(stdout, " %f", reader.read<float>());
                break;

            case OP_ADDR_S:
            case OP_LOAD_S:
            case OP_STOR_S:
                fprintf(stdout, " %d", reader.read<int16_t>());
                break;

            case OP_STOR_S_C: {
                int16_t offset = reader.read<int16_t>();
                cell_t value = reader.read<cell_t>();
                fprintf(stdout, " %d, %d", offset, value);
                break;
            }


            case OP_LOAD_FN:
            case OP_CALL:
            case OP_CALLN:
            {
                uint32_t method_index = reader.read<uint32_t>();
                if (auto method = smx_->GetMethod(method_index))
                    fprintf(stdout, " %s", smx_->names() + method->name);
                else
                    fprintf(stdout, " unknown_method_%u", method_index);

                if (op == OP_CALLN) {
                    uint8_t nargs = reader.read<uint8_t>();
                    fprintf(stdout, " %u", nargs);
                }
                break;
            }

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
                uint32_t target_offs = reader.read<uint32_t>();
                uint32_t diff = target_offs - (uint32_t)(method_start - smx_->DescribeCode().bytes);
                fprintf(stdout, " %04x ; %x", diff, target_offs);
                break;
            }

            case OP_NEWARRAY: {
                uint32_t type_id = reader.read<uint32_t>();
                auto rtti = smx_->GetTypeIdParser(type_id);
                fprintf(stdout, " %s", DumpType(rtti).c_str());
                break;
            }

            case OP_FILLARRAY: {
                uint32_t data_offs = reader.read<uint32_t>();
                fprintf(stdout, " 0x%x", data_offs);
                break;
            }

            case OP_POP:
            case OP_DUP:
            case OP_SWAP:
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
    if (!fp) {
        fprintf(stderr, "Could not open %s\n", file);
        return 1;
    }

    ExceptionHandler eh(sEnv);

    auto smx = std::make_unique<SmxImage>(fp.get());
    if (!smx->validate()) {
        fprintf(stderr, "Could not parse %s: %s\n", file,
                (eh.Message() ? eh.Message() : "unknown error"));
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
