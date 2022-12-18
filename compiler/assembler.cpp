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
#include "sc.h"
#include "sctracker.h"
#include "symbols.h"
#include "types.h"

using namespace SourcePawn;
using namespace ke;
using namespace sp;

static int
sort_by_name(const void* a1, const void* a2)
{
    symbol* s1 = *(symbol**)a1;
    symbol* s2 = *(symbol**)a2;
    return strcmp(s1->name(), s2->name());
}

struct function_entry {
    function_entry() : sym(nullptr)
    {}

    function_entry(function_entry&& other)
      : sym(other.sym),
        name(std::move(other.name))
    {}

    function_entry& operator =(function_entry&& other) {
        sym = other.sym;
        name = std::move(other.name);
        return *this;
    }

    function_entry(const function_entry& other) = delete;
    function_entry& operator =(const function_entry& other) = delete;

    symbol* sym;
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

struct variable_type_t {
    int tag;
    const int* dims;
    int dimcount;
    bool is_const;
};

class RttiBuilder
{
  public:
    RttiBuilder(CompileContext& cc, CodeGenerator& cg, SmxNameTable* names);

    void finish(SmxBuilder& builder);
    void add_method(symbol* sym);
    void add_native(symbol* sym);

  private:
    uint32_t add_enum(Type* type);
    uint32_t add_funcenum(Type* type, funcenum_t* fe);
    uint32_t add_typeset(Type* type, funcenum_t* fe);
    uint32_t add_struct(Type* type);
    uint32_t add_enumstruct(Type* type);
    uint32_t encode_signature(symbol* sym);
    void encode_signature_into(std::vector<uint8_t>& bytes, functag_t* ft);
    void encode_enum_into(std::vector<uint8_t>& bytes, Type* type);
    void encode_tag_into(std::vector<uint8_t>& bytes, int tag);
    void encode_ret_array_into(std::vector<uint8_t>& bytes, symbol* sym);
    void encode_funcenum_into(std::vector<uint8_t>& bytes, Type* type, funcenum_t* fe);
    void encode_var_type(std::vector<uint8_t>& bytes, const variable_type_t& info);
    void encode_struct_into(std::vector<uint8_t>& bytes, Type* type);
    void encode_enumstruct_into(std::vector<uint8_t>& bytes, Type* type);

    uint32_t to_typeid(const std::vector<uint8_t>& bytes);

    void add_debug_var(SmxRttiTable<smx_rtti_debug_var>* table, DebugString& str);
    void add_debug_line(DebugString& str);
    void build_debuginfo();

    uint8_t TagToRttiBytecode(int tag);

  private:
    CompileContext& cc_;
    CodeGenerator& cg_;
    TypeDictionary* types_ = nullptr;
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
    int tag = str.parse();
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

    std::vector<int> dims;
    int last_tag = 0;
    if (str.getc() == '[') {
        for (const char* ptr = str.skipspaces(); *ptr != ']'; ptr = str.skipspaces()) {
            last_tag = str.parse();
            str.skipspaces();
            str.expect(':');
            dims.emplace_back(str.parse());
        }
    }

    // Rewrite enum structs to look less like arrays.
    if (types_->find(last_tag)->asEnumStruct()) {
        dims.pop_back();
        tag = last_tag;
    }

    // Encode the type.
    uint32_t type_id;
    {
        auto dimptr = dims.empty() ? nullptr : &dims[0];
        variable_type_t type = {tag, dimptr, (int)dims.size(), is_const};
        std::vector<uint8_t> encoding;
        encode_var_type(encoding, type);

        type_id = to_typeid(encoding);
    }

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

void
RttiBuilder::add_method(symbol* sym)
{
    assert(!sym->unused());

    uint32_t index = methods_->count();
    smx_rtti_method& method = methods_->add();
    method.name = names_->add(sym->nameAtom());
    method.pcode_start = sym->addr();
    method.pcode_end = sym->codeaddr;
    method.signature = encode_signature(sym);

    if (!sym->function()->dbgstrs)
        return;

    smx_rtti_debug_method debug;
    debug.method_index = index;
    debug.first_local = dbg_locals_->count();

    for (auto& iter : *sym->function()->dbgstrs) {
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

void
RttiBuilder::add_native(symbol* sym)
{
    smx_rtti_native& native = natives_->add();
    native.name = names_->add(sym->nameAtom());
    native.signature = encode_signature(sym);
}

uint32_t
RttiBuilder::add_enumstruct(Type* type)
{
    TypeIdCache::Insert p = typeid_cache_.findForAdd(type);
    if (p.found())
        return p->value;

    symbol* sym = type->asEnumStruct();
    uint32_t es_index = enumstructs_->count();
    typeid_cache_.add(p, type, es_index);

    smx_rtti_enumstruct es = {};
    es.name = names_->add(*cc_.atoms(), type->name());
    es.first_field = es_fields_->count();
    es.size = sym->addr();
    enumstructs_->add(es);

    // Pre-allocate storage in case of nested types.
    auto& enumlist = sym->data()->asEnumStruct()->fields;
    for (auto iter = enumlist.begin(); iter != enumlist.end(); iter++)
        es_fields_->add() = smx_rtti_es_field{};

    // Add all fields.
    size_t index = 0;
    for (auto iter = enumlist.begin(); iter != enumlist.end(); iter++) {
        auto field = *iter;

        int dims[1], dimcount = 0;
        if (field->dim_count())
            dims[dimcount++] = field->dim(0);

        variable_type_t type = {field->semantic_tag, dims, dimcount, false};
        std::vector<uint8_t> encoding;
        encode_var_type(encoding, type);

        smx_rtti_es_field info;
        info.name = names_->add(field->nameAtom());
        info.type_id = to_typeid(encoding);
        info.offset = field->addr();
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

    pstruct_t* ps = type->as<pstruct_t>();

    smx_rtti_classdef classdef;
    memset(&classdef, 0, sizeof(classdef));
    classdef.flags = kClassDefType_Struct;
    classdef.name = names_->add(*cc_.atoms(), ps->name());
    classdef.first_field = fields_->count();
    classdefs_->add(classdef);

    // Pre-reserve space in case we recursively add structs.
    for (size_t i = 0; i < ps->args.size(); i++)
        fields_->add();

    for (size_t i = 0; i < ps->args.size(); i++) {
        const structarg_t* arg = ps->args[i];

        int dims[1] = {0};
        int dimcount = arg->type.ident == iREFARRAY ? 1 : 0;

        variable_type_t type = {arg->type.tag(), dims, dimcount, !!arg->type.is_const};
        std::vector<uint8_t> encoding;
        encode_var_type(encoding, type);

        smx_rtti_field field;
        field.flags = 0;
        field.name = names_->add(arg->name);
        field.type_id = to_typeid(encoding);
        fields_->at(classdef.first_field + i) = field;
    }
    return struct_index;
}

uint32_t
RttiBuilder::to_typeid(const std::vector<uint8_t>& bytes)
{
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

uint32_t
RttiBuilder::encode_signature(symbol* sym)
{
    std::vector<uint8_t> bytes;

    uint32_t argc = 0;
    bool is_variadic = false;
    for (const auto& arg : sym->function()->node->args()) {
        if (arg->type().ident == iVARARGS)
            is_variadic = true;
        argc++;
    }
    if (argc > UCHAR_MAX)
        error(45);

    bytes.push_back((uint8_t)argc);
    if (is_variadic)
        bytes.push_back(cb::kVariadic);

    symbol* child = sym->array_return();
    if (child && child->dim_count()) {
        encode_ret_array_into(bytes, child);
    } else if (sym->tag == types_->tag_void()) {
        bytes.push_back(cb::kVoid);
    } else {
        encode_tag_into(bytes, sym->tag);
    }

    for (const auto& arg : sym->function()->node->args()) {
        int tag = arg->type().tag();
        int numdim = arg->type().numdim();
        if (arg->type().numdim() && arg->type().enum_struct_tag()) {
            int last_tag = arg->type().enum_struct_tag();
            Type* last_type = types_->find(last_tag);
            if (last_type->isEnumStruct()) {
                tag = last_tag;
                numdim--;
            }
        }

        if (arg->type().ident == iREFERENCE)
            bytes.push_back(cb::kByRef);

        auto dim = numdim ? &arg->type().dim[0] : nullptr;
        variable_type_t info = {tag, dim, numdim, arg->type().is_const};
        encode_var_type(bytes, info);
    }

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
    entry.name = names_->add(*cc_.atoms(), type->name());
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
    def.name = names_->add(*cc_.atoms(), type->name());
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
    entry.name = names_->add(*cc_.atoms(), type->name());
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

void
RttiBuilder::encode_ret_array_into(std::vector<uint8_t>& bytes, symbol* sym)
{
    for (int i = 0; i < sym->dim_count(); i++) {
        bytes.push_back(cb::kFixedArray);
        CompactEncodeUint32(bytes, sym->dim(i));
    }
    encode_tag_into(bytes, sym->tag);
}

uint8_t
RttiBuilder::TagToRttiBytecode(int tag)
{
    if (tag == types_->tag_bool())
        return cb::kBool;
    if (tag == types_->tag_any())
        return cb::kAny;
    if (tag == types_->tag_string())
        return cb::kChar8;
    if (tag == types_->tag_float())
        return cb::kFloat32;
    if (tag == 0)
        return cb::kInt32;
    return 0;
}

void
RttiBuilder::encode_tag_into(std::vector<uint8_t>& bytes, int tag)
{
    if (uint8_t b = TagToRttiBytecode(tag)) {
        bytes.push_back(b);
        return;
    }

    Type* type = types_->find(tag);
    assert(!type->isObject());

    if (type->isStruct()) {
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

void
RttiBuilder::encode_signature_into(std::vector<uint8_t>& bytes, functag_t* ft)
{
    bytes.push_back(cb::kFunction);
    bytes.push_back((uint8_t)ft->args.size());
    if (!ft->args.empty() && ft->args[ft->args.size() - 1].type.ident == iVARARGS)
        bytes.push_back(cb::kVariadic);
    if (ft->ret_tag == types_->tag_void())
        bytes.push_back(cb::kVoid);
    else
        encode_tag_into(bytes, ft->ret_tag);

    for (const auto& arg : ft->args) {
        if (arg.type.ident == iREFERENCE)
            bytes.push_back(cb::kByRef);

        auto dims = arg.type.dim.empty() ? nullptr : &arg.type.dim[0];
        variable_type_t info = {arg.type.tag(), dims, arg.type.numdim(), arg.type.is_const};
        encode_var_type(bytes, info);
    }
}

void
RttiBuilder::encode_var_type(std::vector<uint8_t>& bytes, const variable_type_t& info)
{
    for (int i = 0; i < info.dimcount; i++) {
        if (info.dims[i] == 0) {
            bytes.push_back(cb::kArray);
        } else {
            bytes.push_back(cb::kFixedArray);
            CompactEncodeUint32(bytes, info.dims[i]);
        }

        if (i != info.dimcount - 1 && info.is_const)
            bytes.push_back(cb::kConst);
    }
    if (info.is_const)
        bytes.push_back(cb::kConst);
    encode_tag_into(bytes, info.tag);
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
    std::unordered_set<symbol*> symbols;

    // Sort globals.
    std::vector<symbol*> global_symbols;
    cc_.globals()->ForEachSymbol([&](symbol* sym) -> void {
        global_symbols.push_back(sym);

        // This is only to assert that we embedded pointers properly in the assembly buffer.
        symbols.emplace(sym);
    });
    for (const auto& sym : cc_.functions()) {
        if (symbols.count(sym))
            continue;
        global_symbols.push_back(sym);
        symbols.emplace(sym);
    }

    qsort(global_symbols.data(), global_symbols.size(), sizeof(symbol*), sort_by_name);

    // Build the easy symbol tables.
    for (const auto& sym : global_symbols) {
        if (sym->ident == iFUNCTN) {
            if (sym->native)
                continue;

            if (!sym->defined)
                continue;
            if (sym->unused())
                continue;

            function_entry entry;
            entry.sym = sym;
            if (sym->is_public) {
                entry.name = sym->name();
            } else {
                // Create a private name.
                entry.name = ke::StringPrintf(".%d.%s", sym->addr(), sym->name());
            }

            functions.emplace_back(std::move(entry));
        } else if (sym->ident == iVARIABLE || sym->ident == iARRAY || sym->ident == iREFARRAY) {
            if (sym->is_public || (sym->usage & (uREAD | uWRITTEN)) != 0) {
                sp_file_pubvars_t& pubvar = pubvars->add();
                pubvar.address = sym->addr();
                pubvar.name = names->add(sym->nameAtom());
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
        symbol* sym = f.sym;

        assert(sym->addr() > 0);
        assert(sym->defined);
        assert(sym->codeaddr > sym->addr());

        sp_file_publics_t& pubfunc = publics->add();
        pubfunc.address = sym->addr();
        pubfunc.name = names->add(*cc_.atoms(), f.name.c_str());

        auto id = (uint32_t(i) << 1) | 1;
        if (!Label::ValueFits(id))
            error(421);
        cg_.LinkPublicFunction(sym, id);

        rtti.add_method(sym);
    }

    // Populate the native table.
    for (size_t i = 0; i < cg_.native_list().size(); i++) {
        symbol* sym = cg_.native_list()[i];
        assert(size_t(sym->addr()) == i);

        sp_file_natives_t& entry = natives->add();

        if (auto alias = sym->function()->alias)
            entry.name = names->add(*cc_.atoms(), "@" + alias->nameAtom()->str());
        else
            entry.name = names->add(sym->nameAtom());

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
                              SmxConsts::kCodeFeatureHeapScopes;
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
