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

// Helper for parsing a debug string. Debug strings look like this:
//  L:40 10
class DebugString
{
  public:
    DebugString()
     : kind_('\0'),
       str_(nullptr)
    {}
    explicit DebugString(const char* str)
     : kind_(str[0]),
       str_(str)
    {
        assert(str_[1] == ':');
        str_ += 2;
    }
    char kind() const {
        return kind_;
    }
    ucell parse() {
        return strtoul(str_, const_cast<char**>(&str_), 16);
    }
    const char* skipspaces() {
        while (isspace(*str_))
            str_++;
        return str_;
    }
    void expect(char c) {
        assert(*str_ == c);
        str_++;
    }
    const char* skipto(char c) {
        str_ = strchr(str_, c);
        return str_;
    }
    char getc() {
        return *str_++;
    }

  private:
    char kind_;
    const char* str_;
};

typedef SmxBlobSection<sp_fdbg_info_t> SmxDebugInfoSection;
typedef SmxListSection<sp_fdbg_line_t> SmxDebugLineSection;
typedef SmxListSection<sp_fdbg_file_t> SmxDebugFileSection;

class RttiBuilder
{
  public:
    RttiBuilder(CompileContext& cc, CodeGenerator& cg, SmxNameTable* names);

    void finish(SmxBuilder& builder);
    void add_method(FunctionDecl* fun);
    void add_native(FunctionDecl* sym);

  private:
    uint32_t add_enum(Type* type);
    uint32_t add_funcenum(Type* type, funcenum_t* fe);
    uint32_t add_typeset(Type* type, funcenum_t* fe);
    uint32_t add_struct(Type* type);
    uint32_t add_enumstruct(Type* type);
    uint32_t encode_signature(FunctionDecl* decl);
    void encode_signature_into(std::vector<uint8_t>& bytes, FunctionType* ft);
    void encode_enum_into(std::vector<uint8_t>& bytes, Type* type);
    void encode_type_into(std::vector<uint8_t>& bytes, Type* type);
    void encode_type_into(std::vector<uint8_t>& bytes, QualType type);
    void encode_funcenum_into(std::vector<uint8_t>& bytes, Type* type, funcenum_t* fe);
    void encode_struct_into(std::vector<uint8_t>& bytes, Type* type);
    void encode_enumstruct_into(std::vector<uint8_t>& bytes, Type* type);

    uint32_t to_typeid(QualType type);
    uint32_t to_typeid(Type* type) {
        return to_typeid(QualType(type));
    }

    void add_debug_var(SmxRttiTable<smx_rtti_debug_var>* table, DebugString& str);
    void add_debug_line(DebugString& str);
    void build_debuginfo();

    uint8_t TypeToRttiBytecode(Type* type);

  private:
    CompileContext& cc_;
    CodeGenerator& cg_;
    TypeManager* types_ = nullptr;
    RefPtr<SmxNameTable> names_;
    DataPool type_pool_;
    RefPtr<SmxBlobSection<void>> data_;
    RefPtr<SmxRttiTable<smx_rtti_method>> methods_;
    RefPtr<SmxRttiTable<smx_rtti_native>> natives_;
    RefPtr<SmxRttiTable<smx_rtti_enum>> enums_;
    RefPtr<SmxRttiTable<smx_rtti_typedef>> typedefs_;
    RefPtr<SmxRttiTable<smx_rtti_typeset>> typesets_;
    RefPtr<SmxRttiTable<smx_rtti_classdef>> classdefs_;
    RefPtr<SmxRttiTable<smx_rtti_field>> fields_;
    RefPtr<SmxRttiTable<smx_rtti_enumstruct>> enumstructs_;
    RefPtr<SmxRttiTable<smx_rtti_es_field>> es_fields_;
    RefPtr<SmxDebugInfoSection> dbg_info_;
    RefPtr<SmxDebugLineSection> dbg_lines_;
    RefPtr<SmxDebugFileSection> dbg_files_;
    RefPtr<SmxRttiTable<smx_rtti_debug_method>> dbg_methods_;
    RefPtr<SmxRttiTable<smx_rtti_debug_var>> dbg_globals_;
    RefPtr<SmxRttiTable<smx_rtti_debug_var>> dbg_locals_;

    typedef ke::HashMap<Type*, uint32_t, ke::PointerPolicy<Type>> TypeIdCache;
    TypeIdCache typeid_cache_;
};

RttiBuilder::RttiBuilder(CompileContext& cc, CodeGenerator& cg, SmxNameTable* names)
 : cc_(cc),
   cg_(cg),
   names_(names)
{
    types_ = cc_.types();
    typeid_cache_.init(128);
    data_ = new SmxBlobSection<void>("rtti.data");
    methods_ = new SmxRttiTable<smx_rtti_method>("rtti.methods");
    natives_ = new SmxRttiTable<smx_rtti_native>("rtti.natives");
    enums_ = new SmxRttiTable<smx_rtti_enum>("rtti.enums");
    typedefs_ = new SmxRttiTable<smx_rtti_typedef>("rtti.typedefs");
    typesets_ = new SmxRttiTable<smx_rtti_typeset>("rtti.typesets");
    classdefs_ = new SmxRttiTable<smx_rtti_classdef>("rtti.classdefs");
    fields_ = new SmxRttiTable<smx_rtti_field>("rtti.fields");
    enumstructs_ = new SmxRttiTable<smx_rtti_enumstruct>("rtti.enumstructs");
    es_fields_ = new SmxRttiTable<smx_rtti_es_field>("rtti.enumstruct_fields");
    dbg_info_ = new SmxDebugInfoSection(".dbg.info");
    dbg_lines_ = new SmxDebugLineSection(".dbg.lines");
    dbg_files_ = new SmxDebugFileSection(".dbg.files");
    dbg_methods_ = new SmxRttiTable<smx_rtti_debug_method>(".dbg.methods");
    dbg_globals_ = new SmxRttiTable<smx_rtti_debug_var>(".dbg.globals");
    dbg_locals_ = new SmxRttiTable<smx_rtti_debug_var>(".dbg.locals");
}

void
RttiBuilder::finish(SmxBuilder& builder)
{
    build_debuginfo();

    const ByteBuffer& buffer = type_pool_.buffer();
    data_->add(buffer.bytes(), buffer.size());

    builder.add(data_);
    builder.add(methods_);
    builder.add(natives_);
    builder.addIfNotEmpty(enums_);
    builder.addIfNotEmpty(typedefs_);
    builder.addIfNotEmpty(typesets_);
    builder.addIfNotEmpty(classdefs_);
    builder.addIfNotEmpty(fields_);
    builder.addIfNotEmpty(enumstructs_);
    builder.addIfNotEmpty(es_fields_);
    builder.add(dbg_files_);
    builder.add(dbg_lines_);
    builder.add(dbg_info_);
    builder.add(dbg_methods_);
    builder.add(dbg_globals_);
    builder.add(dbg_locals_);
}

void
RttiBuilder::build_debuginfo()
{
    // State for tracking which file we're on. We replicate the original AMXDBG
    // behavior here which excludes duplicate addresses.
    ucell prev_file_addr = 0;
    const char* prev_file_name = nullptr;

    // Add debug data.
    for (const auto& line : cg_.debug_strings()) {
        DebugString str(line.c_str());
        switch (str.kind()) {
            case 'F': {
                ucell codeidx = str.parse();
                if (codeidx != prev_file_addr) {
                    if (prev_file_name) {
                        sp_fdbg_file_t& entry = dbg_files_->add();
                        entry.addr = prev_file_addr;
                        entry.name = names_->add(*cc_.atoms(), prev_file_name);
                    }
                    prev_file_addr = codeidx;
                }
                prev_file_name = str.skipspaces();
                break;
            }

            case 'L':
                add_debug_line(str);
                break;

            case 'S':
                add_debug_var(dbg_globals_, str);
                break;
        }
    }

    // Add the last file.
    if (prev_file_name) {
        sp_fdbg_file_t& entry = dbg_files_->add();
        entry.addr = prev_file_addr;
        entry.name = names_->add(*cc_.atoms(), prev_file_name);
    }

    // Make sure debug tables are sorted by address.
    std::sort(dbg_files_->list().begin(), dbg_files_->list().end(),
              [](const sp_fdbg_file_t& a, const sp_fdbg_file_t& b) -> bool {
                return a.addr < b.addr;
              });
    std::sort(dbg_lines_->list().begin(), dbg_lines_->list().end(),
              [](const sp_fdbg_line_t& a, const sp_fdbg_line_t& b) -> bool {
                return a.addr < b.addr;
              });

    // Finish up debug header statistics.
    dbg_info_->header().num_files = dbg_files_->count();
    dbg_info_->header().num_lines = dbg_lines_->count();
    dbg_info_->header().num_syms = 0;
    dbg_info_->header().num_arrays = 0;
}

void
RttiBuilder::add_debug_line(DebugString& str)
{
    auto addr = str.parse();
    auto line = str.parse();

    // Lines are zero-indexed for some reason.
    if (line > 0)
        line--;

    if (!dbg_lines_->list().empty()) {
        auto& last = dbg_lines_->list().back();
        if (last.addr == addr) {
            last.line = line;
            return;
        }
    }
    sp_fdbg_line_t& entry = dbg_lines_->add();
    entry.addr = addr;
    entry.line = line;
}

void
RttiBuilder::add_debug_var(SmxRttiTable<smx_rtti_debug_var>* table, DebugString& str)
{
    int address = str.parse();
    Type* type = cc_.types()->Get(str.parse());
    str.skipspaces();
    str.expect(':');
    const char* name_start = str.skipspaces();
    const char* name_end = str.skipto(' ');
    uint32_t code_start = str.parse();
    uint32_t code_end = str.parse();
    int ident = str.parse();
    int vclass = str.parse();
    bool is_const = !!str.parse();

    // We don't care about the ident type, we derive it from the tag.
    (void)ident;

    str.skipspaces();

    // Encode the type.
    uint32_t type_id = to_typeid(QualType(type, is_const));

    smx_rtti_debug_var& var = table->add();
    var.address = address;
    switch (vclass) {
        case sLOCAL:
            var.vclass = address < 0 ? kVarClass_Local : kVarClass_Arg;
            break;
        case sGLOBAL:
            var.vclass = kVarClass_Global;
            break;
        case sSTATIC:
            var.vclass = kVarClass_Static;
            break;
        case sARGUMENT:
            var.vclass = kVarClass_Arg;
            break;
        default:
            var.vclass = 0;
            assert(false);
    }
    var.name = names_->add(*cc_.atoms(), name_start, name_end - name_start);
    var.code_start = code_start;
    var.code_end = code_end;
    var.type_id = type_id;
}

void RttiBuilder::add_method(FunctionDecl* fun) {
    assert(fun->is_live());

    uint32_t index = methods_->count();
    smx_rtti_method& method = methods_->add();
    method.name = names_->add(fun->name());
    method.pcode_start = fun->cg()->label.offset();
    method.pcode_end = fun->cg()->pcode_end;
    method.signature = encode_signature(fun->canonical());

    if (!fun->cg()->dbgstrs)
        return;

    smx_rtti_debug_method debug;
    debug.method_index = index;
    debug.first_local = dbg_locals_->count();

    for (auto& iter : *fun->cg()->dbgstrs) {
        const char* chars = iter.c_str();
        if (chars[0] == '\0')
            continue;

        DebugString str(chars);
        if (str.kind() == 'S')
            add_debug_var(dbg_locals_, str);
        else if (str.kind() == 'L')
            add_debug_line(str);
    }

    // Only add a method table entry if we actually had locals.
    if (debug.first_local != dbg_locals_->count())
        dbg_methods_->add(debug);
}

void RttiBuilder::add_native(FunctionDecl* fun) {
    smx_rtti_native& native = natives_->add();
    native.name = names_->add(fun->name());
    native.signature = encode_signature(fun);
}

uint32_t
RttiBuilder::add_enumstruct(Type* type)
{
    TypeIdCache::Insert p = typeid_cache_.findForAdd(type);
    if (p.found())
        return p->value;

    auto es_decl = type->asEnumStruct();
    uint32_t es_index = enumstructs_->count();
    typeid_cache_.add(p, type, es_index);

    smx_rtti_enumstruct es = {};
    es.name = names_->add(*cc_.atoms(), type->declName());
    es.first_field = es_fields_->count();
    es.size = es_decl->array_size();
    enumstructs_->add(es);

    // Pre-allocate storage in case of nested types.
    const auto& enumlist = es_decl->fields();
    for (auto iter = enumlist.begin(); iter != enumlist.end(); iter++)
        es_fields_->add() = smx_rtti_es_field{};

    // Add all fields.
    size_t index = 0;
    for (auto iter = enumlist.begin(); iter != enumlist.end(); iter++) {
        auto field = (*iter);

        smx_rtti_es_field info;
        info.name = names_->add(field->name());
        info.type_id = to_typeid(field->type());
        info.offset = field->offset();
        es_fields_->at(es.first_field + index) = info;
        index++;
    }

    return es_index;
}

uint32_t
RttiBuilder::add_struct(Type* type)
{
    TypeIdCache::Insert p = typeid_cache_.findForAdd(type);
    if (p.found())
        return p->value;

    uint32_t struct_index = classdefs_->count();
    typeid_cache_.add(p, type, struct_index);

    auto ps = type->asPstruct();

    smx_rtti_classdef classdef;
    memset(&classdef, 0, sizeof(classdef));
    classdef.flags = kClassDefType_Struct;
    classdef.name = names_->add(*cc_.atoms(), ps->name());
    classdef.first_field = fields_->count();
    classdefs_->add(classdef);

    // Pre-reserve space in case we recursively add structs.
    for (size_t i = 0; i < ps->fields().size(); i++)
        fields_->add();

    for (size_t i = 0; i < ps->fields().size(); i++) {
        auto arg = ps->fields()[i];

        smx_rtti_field field;
        field.flags = 0;
        field.name = names_->add(arg->name());
        field.type_id = to_typeid(QualType(arg->type(), arg->type_info().is_const));
        fields_->at(classdef.first_field + i) = field;
    }
    return struct_index;
}

uint32_t RttiBuilder::to_typeid(QualType type) {
    std::vector<uint8_t> bytes;
    encode_type_into(bytes, type);

    if (bytes.size() <= 4) {
        uint32_t payload = 0;
        for (size_t i = 0; i < bytes.size(); i++)
            payload |= bytes[i] << (i * 8);
        if (payload <= kMaxTypeIdPayload)
            return MakeTypeId(payload, kTypeId_Inline);
    }

    uint32_t offset = type_pool_.add(bytes);
    return MakeTypeId(offset, kTypeId_Complex);
}

uint32_t RttiBuilder::encode_signature(FunctionDecl* fun) {
    assert(fun == fun->canonical());

    std::vector<uint8_t> bytes;

    uint32_t argc = fun->args().size();
    if (argc > UCHAR_MAX)
        report(45);

    bytes.push_back((uint8_t)argc);
    if (fun->IsVariadic())
        bytes.push_back(cb::kLegacyVariadic);

    encode_type_into(bytes, fun->return_type());

    for (const auto& arg : fun->args())
        encode_type_into(bytes, QualType(arg->type(), arg->type_info().is_const));

    return type_pool_.add(bytes);
}

uint32_t
RttiBuilder::add_enum(Type* type)
{
    TypeIdCache::Insert p = typeid_cache_.findForAdd(type);
    if (p.found())
        return p->value;

    uint32_t index = enums_->count();
    typeid_cache_.add(p, type, index);

    smx_rtti_enum entry;
    memset(&entry, 0, sizeof(entry));
    entry.name = names_->add(*cc_.atoms(), type->declName());
    enums_->add(entry);
    return index;
}

uint32_t
RttiBuilder::add_funcenum(Type* type, funcenum_t* fe)
{
    TypeIdCache::Insert p = typeid_cache_.findForAdd(type);
    if (p.found())
        return p->value;

    // Reserve slot beforehand in case the type is recursive.
    uint32_t index = typedefs_->count();
    typeid_cache_.add(p, type, index);
    typedefs_->add();

    std::vector<uint8_t> bytes;
    encode_signature_into(bytes, fe->entries.back());
    uint32_t signature = type_pool_.add(bytes);

    smx_rtti_typedef& def = typedefs_->at(index);
    def.name = names_->add(*cc_.atoms(), type->declName());
    def.type_id = MakeTypeId(signature, kTypeId_Complex);
    return index;
}

uint32_t
RttiBuilder::add_typeset(Type* type, funcenum_t* fe)
{
    TypeIdCache::Insert p = typeid_cache_.findForAdd(type);
    if (p.found())
        return p->value;

    // Reserve slot beforehand in case the type is recursive.
    uint32_t index = typesets_->count();
    typeid_cache_.add(p, type, index);
    typesets_->add();

    uint32_t typecount = (uint32_t)fe->entries.size();

    std::vector<uint8_t> bytes;
    CompactEncodeUint32(bytes, typecount);
    for (const auto& iter : fe->entries)
        encode_signature_into(bytes, iter);

    smx_rtti_typeset& entry = typesets_->at(index);
    entry.name = names_->add(*cc_.atoms(), type->declName());
    entry.signature = type_pool_.add(bytes);
    return index;
}

void
RttiBuilder::encode_struct_into(std::vector<uint8_t>& bytes, Type* type)
{
    bytes.push_back(cb::kClassdef);
    CompactEncodeUint32(bytes, add_struct(type));
}

void
RttiBuilder::encode_enum_into(std::vector<uint8_t>& bytes, Type* type)
{
    bytes.push_back(cb::kEnum);
    CompactEncodeUint32(bytes, add_enum(type));
}

void
RttiBuilder::encode_enumstruct_into(std::vector<uint8_t>& bytes, Type* type)
{
    bytes.push_back(cb::kEnumStruct);
    CompactEncodeUint32(bytes, add_enumstruct(type));
}

uint8_t RttiBuilder::TypeToRttiBytecode(Type* type) {
    if (type->isBool())
        return cb::kBool;
    if (type->isAny())
        return cb::kAny;
    if (type->isChar())
        return cb::kChar8;
    if (type->isFloat())
        return cb::kFloat32;
    if (type->isInt())
        return cb::kInt32;
    if (type->isVoid())
        return cb::kVoid;
    return 0;
}

void RttiBuilder::encode_type_into(std::vector<uint8_t>& bytes, Type* type) {
    encode_type_into(bytes, QualType(type));
}

void RttiBuilder::encode_type_into(std::vector<uint8_t>& bytes, QualType qt) {
    if (qt.is_const())
        bytes.emplace_back(cb::kConst);

    Type* type = *qt;
    if (auto array = type->as<ArrayType>()) {
        for (;;) {
            if (array->size()) {
                bytes.emplace_back(cb::kFixedArray);
                CompactEncodeUint32(bytes, array->size());
            } else {
                bytes.emplace_back(cb::kArray);
            }
            if (!array->inner()->isArray())
                break;
            array = array->inner()->to<ArrayType>();
        }
        type = array->inner();
    } else if (type->isReference()) {
        bytes.emplace_back(cb::kByRef);
        type = type->inner();
    }

    if (uint8_t b = TypeToRttiBytecode(type)) {
        bytes.push_back(b);
        return;
    }

    assert(!type->isObject());

    if (type->isPstruct()) {
        encode_struct_into(bytes, type);
        return;
    }

    if (type->isFunction()) {
        if (funcenum_t* fe = type->toFunction())
            encode_funcenum_into(bytes, type, fe);
        else
            bytes.push_back(cb::kTopFunction);
        return;
    }

    if (type->isEnumStruct()) {
        encode_enumstruct_into(bytes, type);
        return;
    }

    encode_enum_into(bytes, type);
}

void
RttiBuilder::encode_funcenum_into(std::vector<uint8_t>& bytes, Type* type, funcenum_t* fe)
{
    if (fe->entries.size() == 1) {
        uint32_t index = add_funcenum(type, fe);
        bytes.push_back(cb::kTypedef);
        CompactEncodeUint32(bytes, index);
    } else {
        uint32_t index = add_typeset(type, fe);
        bytes.push_back(cb::kTypeset);
        CompactEncodeUint32(bytes, index);
    }
}

void RttiBuilder::encode_signature_into(std::vector<uint8_t>& bytes, FunctionType* ft) {
    bytes.push_back(cb::kFunction);
    bytes.push_back((uint8_t)ft->nargs());

    if (ft->variadic())
        bytes.push_back(cb::kLegacyVariadic);

    encode_type_into(bytes, ft->return_type());

    for (size_t i = 0; i < ft->nargs(); i++)
        encode_type_into(bytes, ft->arg_type(i));
}

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
    RefPtr<SmxNameTable> names = new SmxNameTable(".names");

    RttiBuilder rtti(cc_, cg_, names);

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
                              SmxConsts::kCodeFeatureNullFunctions;
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
