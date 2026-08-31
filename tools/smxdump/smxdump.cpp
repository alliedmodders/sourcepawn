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
#include "smxdump.h"

#include <inttypes.h>

#include <capstone/capstone.h>

#include <amtl/experimental/am-argparser.h>
#include <sp_vm_api.h>
#if defined(SP_JIT_V2)
#include "v2/jit.h"
#endif
#include "v2/method-info.h"
#include "v2/runtime.h"
#include "vm/binary-reader.h"
#include "vm/environment.h"
#include "vm/legacy/opcodes.h"
#include "vm/smx-image.h"
#include "vm/v2/opcodes.h"

using namespace ke;
using namespace ke::args;
using namespace sp;
using namespace SourcePawn;

Environment* sEnv;

StringOption filename("file", "SMX file");
ToggleOption show_name_offsets(nullptr, "--show-name-offsets", Some(false),
                               "Show all name offsets");
ToggleOption show_lowered(nullptr, "--lower", Some(false),
                          "Show lowered opcodes");
ToggleOption show_jit(nullptr, "--jit", Some(false),
                      "Disassemble JIT compiled methods");

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

DumpTool::DumpTool(const char* file, std::unique_ptr<sp::SmxImage> smx, sp::v2::Runtime* runtime)
  : smx_(std::move(smx)),
    file_(file),
    runtime_(runtime)
{}

DumpTool::~DumpTool() = default;

sp::SmxImage* DumpTool::smx() const {
    return runtime_ ? runtime_->image() : smx_.get();
}

void DumpTool::Dump() {
    DumpHeaders();
    DumpPublics();
    DumpPubvars();
    DumpNatives();
    DumpData();
    DumpCode();
    DumpRttiMethods();
    DumpRttiGlobals();

    if (!smx()->rtti_methods())
        DumpLegacyCode();

    DumpRttiEnums();
    DumpRttiClassdefs();
}

void DumpTool::DumpHeaders() {
    fprintf(stdout, "; %s\n", file_.c_str());
    fprintf(stdout, "; SMX Header\n");
    fprintf(stdout, ".magic = 0x%x\n", smx()->hdr()->magic);
    fprintf(stdout, ".version = 0x%x\n", smx()->hdr()->version);
    fprintf(stdout, ".compression = 0x%x\n", smx()->hdr()->compression);
    fprintf(stdout, ".disksize = 0x%x\n", smx()->hdr()->disksize);
    fprintf(stdout, ".imagesize = 0x%x\n", smx()->hdr()->imagesize);
    fprintf(stdout, ".sections = 0x%x\n", smx()->hdr()->imagesize);
    fprintf(stdout, ".stringtab = 0x%x\n", smx()->hdr()->imagesize);
    fprintf(stdout, ".dataoffs = 0x%x\n", smx()->hdr()->imagesize);
    fprintf(stdout, "\n");
}

void DumpTool::DumpPublics() {
    auto publics = smx()->publics();
    if (publics.length() == 0)
        return;

    fprintf(stdout, ".publics\n");
    fprintf(stdout, "{\n");
    for (uint32_t i = 0; i < publics.length(); i++) {
        const char* name = smx()->names() + publics[i].name;
        fprintf(stdout, "    %u: %s @ 0x%x", i, name, publics[i].address);
        if (show_name_offsets.value())
            fprintf(stdout, " ; name_offset = %u", publics[i].name);
        fprintf(stdout, "\n");
    }
    fprintf(stdout, "}\n");
}

void DumpTool::DumpNatives() {
    auto natives = smx()->natives();
    if (natives.length() == 0)
        return;

    fprintf(stdout, ".natives\n");
    fprintf(stdout, "{\n");
    for (uint32_t i = 0; i < natives.length(); i++) {
        const char* name = smx()->names() + natives[i].name;
        fprintf(stdout, "    %u: %s", i, name);
        if (show_name_offsets.value())
            fprintf(stdout, " ; name_offset = %u", natives[i].name);
        fprintf(stdout, "\n");
    }
    fprintf(stdout, "}\n");
}

void DumpTool::DumpPubvars() {
    auto pubvars = smx()->pubvars();
    if (pubvars.length() == 0)
        return;

    fprintf(stdout, ".pubvars\n");
    fprintf(stdout, "{\n");
    for (uint32_t i = 0; i < pubvars.length(); i++) {
        const char* name = smx()->names() + pubvars[i].name;
        fprintf(stdout, "    %u: %s @ 0x%x", i, name, pubvars[i].address);
        if (show_name_offsets.value())
            fprintf(stdout, " ; name_offset = %u", pubvars[i].name);
        fprintf(stdout, "\n");
    }
    fprintf(stdout, "}\n");
}

void DumpTool::DumpData() {
    auto data = smx()->data();

    fprintf(stdout, ".data\n");
    fprintf(stdout, "{\n");
    fprintf(stdout, "    .datasize: %u\n", data->datasize);
    fprintf(stdout, "    .memsize: %u\n", data->memsize);
    fprintf(stdout, "}\n");
}

void DumpTool::DumpCode() {
    auto code = smx()->code();

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

void DumpTool::DumpRttiMethods() {
    auto methods = smx()->rtti_methods();
    if (!methods)
        return;

    bool is_v2 = smx()->hdr()->version >= SmxConsts::SP_VERSION_2;

    for (uint32_t i = 0; i < methods->row_count; i++) {
        auto method = smx()->getRttiRow<smx_rtti_method>(methods, i);

        fprintf(stdout, ".method %s ; index %u", smx()->names() + method->name, i);
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
            if (method->flags & kRttiMethod_Native)
                fprintf(stdout, "    .flags = native\n");
            if (method->flags & kRttiMethod_Closure)
                fprintf(stdout, "    .flags = closure\n");
            if (!(method->flags & kRttiMethod_Native)) {
                uint8_t visibility = method->flags & kRttiMethodVisibilityMask;
                if (visibility == kRttiMethodVisibility_Private)
                    fprintf(stdout, "    .visibility = private\n");
                else if (visibility == kRttiMethodVisibility_Public)
                    fprintf(stdout, "    .visibility = public\n");
                else
                    fprintf(stdout, "    .visibility = unknown_%u\n", visibility);
            }
        }

        if (!is_native) {
            DumpLocals(method);
            if (is_v2) {
                if (show_lowered.value())
                    DumpLoweredCode(i);
                else if (show_jit.value())
                    DumpJitCode(i);
                else
                    DumpCodeRangeV2(method->pcode_start, method->pcode_end);
            } else {
                DumpCodeRangeV1<false>(method->pcode_start, method->pcode_end);
            }
        }
        fprintf(stdout, "}\n");
    }
}

void DumpTool::DumpSignature(uint32_t offset) {
    auto rtti = smx()->GetTypeParser(offset);

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

void DumpTool::DumpUpvars(sp::FastRtti& rtti, uint16_t count) {
    for (uint16_t i = 0; i < count; i++) {
        fprintf(stdout, "    .upvar %d ", i);
        auto type = DumpType(rtti);
        if (!type.empty())
            fprintf(stdout, "%s", type.c_str());
        else
            fprintf(stdout, "ERROR");
        fprintf(stdout, "\n");
    }
}

void DumpTool::DumpLocals(const smx_rtti_method* method) {
    if (smx()->code()->codeversion < SmxConsts::CODE_VERSION_TYPED_STACK || !method->locals)
        return;

    auto rtti = smx()->GetTypeParser(method->locals);

    // For closure methods, upvar slots precede kLocalSlots.
    uint8_t b;
    if (rtti.GetByte(&b) && b == cb::kClosureSlots) {
        rtti.NextByte();
        union u {
            uint16_t value;
            uint8_t bytes[2];
        } u;
        if (rtti.GetNextByte(&u.bytes[0]) && rtti.GetNextByte(&u.bytes[1])) {
            DumpUpvars(rtti, u.value);
        }
    }

    uint16_t locals;
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

std::string DumpTool::DumpType(FastRtti& rtti) {
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

                if (!smx()->rtti_enums()) {
                    type_inner = ke::StringPrintf("enum_%u", value);
                } else {
                    auto entry = smx()->getRttiRow<smx_rtti_enum>(smx()->rtti_enums(), value);
                    type_inner = ke::StringPrintf("enum %s", smx()->names() + entry->name);
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
            case cb::kClassdef:
            case cb::kEnumStruct:
            case cb::kClass:
            {
                uint32_t value;
                if (!rtti.ReadUint32_Leb128(&value))
                     return {};

                if (b == cb::kEnumStruct && smx()->hdr()->version < SmxConsts::SP_VERSION_2) {
                    type_inner = ke::StringPrintf("enum_struct_%u", value);
                    return {};
                }

                if (auto entry = smx()->getClassdef(value)) {
                    const char* prefix = GetClassdefPrefix(entry->flags);
                    type_inner = ke::StringPrintf("%s %s", prefix, smx()->names() + entry->name);
                } else {
                    type_inner = ke::StringPrintf("classdef_%u", value);
                }
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

void DumpTool::DumpRttiEnums() {
    auto rtti_enums = smx()->rtti_enums();
    if (!rtti_enums || rtti_enums->row_count == 0)
        return;

    fprintf(stdout, ".rtti_enums\n");
    fprintf(stdout, "{\n");
    for (uint32_t i = 0; i < rtti_enums->row_count; i++) {
        auto entry = smx()->getRttiRow<smx_rtti_enum>(rtti_enums, i);
        fprintf(stdout, "    %u: %s\n", i, smx()->names() + entry->name);
    }
    fprintf(stdout, "}\n");
}

const char* DumpTool::GetClassdefPrefix(uint32_t flags) {
    if (flags == kClassType_Struct)
        return "struct";
    if (flags == kClassType_EnumStruct)
        return "enum struct";
    if (flags == kClassType_Class)
        return "class";
    return "unknown classdef";
}

void DumpTool::DumpRttiClassdefs() {
    auto rtti_classdefs = smx()->rtti_classdefs();
    if (!rtti_classdefs || rtti_classdefs->row_count == 0)
        return;

    for (uint32_t i = 0; i < rtti_classdefs->row_count; i++) {
        auto entry = smx()->getClassdef(i);
        if (!entry)
            continue;
        const char* type_str = GetClassdefPrefix(entry->flags);
        fprintf(stdout, ".classdef %s\n", smx()->names() + entry->name);
        fprintf(stdout, "{\n");
        fprintf(stdout, "    ; index = %u\n", i);
        fprintf(stdout, "    ; type = %s\n", type_str);
        fprintf(stdout, "    ; first_field = %u\n", entry->first_field);
        fprintf(stdout, "    ; last_field = %u\n", smx()->getClassdefFieldsEnd(i));
        uint32_t stopat = smx()->getClassdefFieldsEnd(i);
        for (uint32_t j = entry->first_field; j < stopat; j++) {
            if (auto field = smx()->getField(j)) {
                auto parser = smx()->GetTypeIdParser(field->type_id);
                fprintf(stdout, "    field %s: %s\n",
                        smx()->names() + field->name, DumpType(parser).c_str());
            }
        }
        fprintf(stdout, "}\n");
    }
}

void DumpTool::DumpRttiGlobals() {
    auto globals = smx()->rtti_globals();
    if (!globals)
        return;

    fprintf(stdout, ".rtti_globals\n");
    fprintf(stdout, "{\n");
    for (uint32_t i = 0; i < globals->row_count; i++) {
        auto global = smx()->getRttiRow<smx_rtti_global>(globals, i);
        fprintf(stdout, "    %u: %s ", i, smx()->names() + global->name);

        if (global->flags & kRttiGlobal_Public)
            fprintf(stdout, "[public] ");

        auto rtti = smx()->GetTypeIdParser(global->type_id);
        fprintf(stdout, "%s", DumpType(rtti).c_str());
        fprintf(stdout, "\n");
    }
    fprintf(stdout, "}\n");
}

void DumpTool::DumpLegacyCode() {
    auto code = smx()->DescribeCode();
    if (smx()->hdr()->version < SmxConsts::SP_VERSION_2)
        DumpCodeRangeV1<true>(0, code.length);
}

static int Dump(const char* file) {
    ExceptionHandler eh(sEnv);

    if (show_lowered.value() || show_jit.value()) {
        std::unique_ptr<BaseRuntime> rt(sEnv->LoadBinaryFromFile(file, true));
        if (!rt) {
            fprintf(stderr, "Could not load %s: %s\n", file,
                    (eh.Message() ? eh.Message() : "unknown error"));
            return 1;
        }
        if (rt->image()->hdr()->version < SmxConsts::SP_VERSION_2) {
            fprintf(stderr, "Lowering or JIT compilation are only supported for version 2+ binaries.\n");
            return 1;
        }
#if !defined(SP_JIT_V2)
        if (show_jit.value()) {
            fprintf(stderr, "JIT disassembly is not supported on this platform.\n");
            return 1;
        }
#endif
        auto v2_rt = rt->AsV2();
        DumpTool tool(file, nullptr, v2_rt);
        tool.Dump();
    } else {
        struct FileCloser {
            void operator()(FILE* fp) const {
                fclose(fp);
            }
        };
        std::unique_ptr<FILE, FileCloser> fp(fopen(file, "rb"));
        if (!fp) {
            fprintf(stderr, "Could not open %s\n", file);
            return 1;
        }

        auto smx = std::make_unique<SmxImage>(fp.get());
        if (!smx->validate()) {
            fprintf(stderr, "Warning: %s failed validation: %s\n", file,
                    (eh.Message() ? eh.Message() : "unknown error"));
        }

        DumpTool tool(file, std::move(smx));
        tool.Dump();
    }
    return 0;
}

#if defined(SP_JIT_V2)
#if defined(__x86_64__) || defined(_M_X64)
static constexpr cs_arch kCapstoneArch = CS_ARCH_X86;
static constexpr cs_mode kCapstoneMode = CS_MODE_64;
#elif defined(__i386__) || defined(_M_IX86)
static constexpr cs_arch kCapstoneArch = CS_ARCH_X86;
static constexpr cs_mode kCapstoneMode = CS_MODE_32;
#elif defined(__aarch64__) || defined(_M_ARM64)
static constexpr cs_arch kCapstoneArch = CS_ARCH_ARM64;
static constexpr cs_mode kCapstoneMode = CS_MODE_ARM;
#else
# error "Unsupported Capstone architecture for JIT disassembly"
#endif
#endif

void DumpTool::DumpJitCode(uint32_t method_index) {
#if defined(SP_JIT_V2)
    auto method = runtime_->AcquireMethod(method_index);
    if (!method) {
        fprintf(stdout, "    ; Method not found\n");
        return;
    }
    if (!method->jit() && !v2::CompilerBase::Compile(runtime_, method)) {
        fprintf(stdout, "    ; JIT compilation error\n");
        return;
    }

    CompiledFunction* jit = method->jit();
    void* entry = jit->GetEntryAddress();
    size_t size = jit->GetCodeSize();

    csh handle;

    if (int err = cs_open(kCapstoneArch, kCapstoneMode, &handle); err != CS_ERR_OK) {
        fprintf(stdout, "    ; capstone error %d\n", err);
        return;
    }
    cs_insn* insn;
    size_t count = cs_disasm(handle, reinterpret_cast<const uint8_t*>(entry), size, reinterpret_cast<uint64_t>(entry), 0, &insn);
    if (count > 0) {
        for (size_t j = 0; j < count; j++) {
            if (insn[j].op_str[0])
                fprintf(stdout, "    0x%" PRIx64 ": %s %s\n", insn[j].address, insn[j].mnemonic, insn[j].op_str);
            else
                fprintf(stdout, "    0x%" PRIx64 ": %s\n", insn[j].address, insn[j].mnemonic);
        }
        cs_free(insn, count);
    } else {
        fprintf(stdout, "    ; JIT compilation error\n");
    }
    cs_close(&handle);
#endif
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
