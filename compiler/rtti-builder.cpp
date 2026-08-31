// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
//  Copyright (c) AlliedModders LLC 2026
//
//  This software is provided "as-is", without any express or implied warranty.
//  In no event will the authors be held liable for any damages arising from
//  the use of this software.
//
//  Permission is granted to anyone to use this software for any purpose,
//  including commercial applications, and to alter it and redistribute it
//  freely, subject to the following restrictions:
//
//  1.  The origin of this software must not be misrepresented; you must not
//      claim that you wrote the original software. If you use this software in
//      a product, an acknowledgment in the product documentation would be
//      appreciated but is not required.
//  2.  Altered source versions must be plainly marked as such, and must not be
//      misrepresented as being the original software.
//  3.  This notice may not be removed or altered from any source distribution.
#include "rtti-builder.h"

#include "code-generator.h"
#include "utils/compact-encoding.h"

namespace sp {
namespace cc {

RttiBuilder::RttiBuilder(CompileContext& cc, SmxNameTable* names)
 : cc_(cc),
   names_(names)
{
    types_ = cc_.types();
    typeid_cache_.init(128);
    string_cache_.init(128);
    rtti_data_ = new SmxBlobSection<void>("rtti.data");
    methods_ = new SmxRttiTable<smx_rtti_method>("rtti.methods");
    enums_ = new SmxRttiTable<smx_rtti_enum>("rtti.enums");
    typesets_ = new SmxRttiTable<smx_rtti_typeset>("rtti.typesets");
    classdefs_ = new SmxRttiTable<smx_rtti_classdef>("rtti.classdefs");
    fields_ = new SmxRttiTable<smx_rtti_field>("rtti.fields");
    stringpool_ = new SmxRttiTable<smx_rtti_string>("rtti.stringpool");
    globals_ = new SmxRttiTable<smx_rtti_global>("rtti.globals");
    dbg_info_ = new SmxDebugInfoSection(".dbg.info");
    dbg_lines_ = new SmxRttiTable<smx_rtti_debug_line>(".dbg.method_lines");
    dbg_files_ = new SmxDebugFileSection(".dbg.files");
    dbg_methods_ = new SmxRttiTable<smx_rtti_debug_method>(".dbg.methods");
    dbg_globals_ = new SmxRttiTable<smx_rtti_debug_var>(".dbg.globals");
    dbg_locals_ = new SmxRttiTable<smx_rtti_debug_var>(".dbg.locals");

    // Make the type pool 1-indexed so we can use 0 as a null value.
    std::vector<uint8_t> placeholder{0xff};
    type_pool_.add(placeholder);
}

void
RttiBuilder::finish(SmxBuilder& builder)
{
    build_debuginfo();

    const ByteBuffer& buffer = type_pool_.buffer();
    rtti_data_->add(buffer.bytes(), buffer.size());

    builder.add(rtti_data_);
    builder.add(methods_);
    builder.addIfNotEmpty(enums_);
    builder.addIfNotEmpty(typesets_);
    builder.addIfNotEmpty(classdefs_);
    builder.addIfNotEmpty(fields_);
    builder.addIfNotEmpty(stringpool_);
    builder.addIfNotEmpty(globals_);
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
    // Add the last file.
    if (!last_file_name_.empty()) {
        sp_fdbg_file_t& entry = dbg_files_->add();
        entry.addr = last_file_addr_;
        entry.name = names_->add(*cc_.atoms(), last_file_name_);
    }

    // Make sure debug tables are sorted by address.
    std::sort(dbg_files_->list().begin(), dbg_files_->list().end(),
              [](const sp_fdbg_file_t& a, const sp_fdbg_file_t& b) -> bool {
                return a.addr < b.addr;
              });

    // Finish up debug header statistics.
    dbg_info_->header().num_files = dbg_files_->count();
    dbg_info_->header().num_lines = 0;
    dbg_info_->header().num_syms = 0;
    dbg_info_->header().num_arrays = 0;
}

void RttiBuilder::AddDebugFile(ucell codeidx, const char* file) {
    // We replicate the original AMXDBG behavior here which excludes duplicate
    // addresses.
    if (codeidx != last_file_addr_) {
        if (!last_file_name_.empty()) {
            sp_fdbg_file_t& entry = dbg_files_->add();
            entry.addr = last_file_addr_;
            entry.name = names_->add(*cc_.atoms(), last_file_name_);
        }
        last_file_addr_ = codeidx;
    }
    last_file_name_ = file;
}

void RttiBuilder::AddDebugLine(uint16_t addr, uint16_t line) {
    smx_rtti_debug_line& entry = dbg_lines_->add();
    entry.addr = addr;
    entry.line = line;
}

void RttiBuilder::AddDebugVar(FunctionDecl* parent, Decl* decl, uint32_t code_start, uint32_t code_end) {
    std::optional<cell> addr;
    if (auto var = decl->as<VarDeclBase>()) {
        if (var->is_shared())
            return;
        if (auto cv = var->as<ConstDecl>())
            addr.emplace(cv->const_val());
        else
            addr.emplace(var->addr());
    } else {
        assert(false);
    }

    // Encode the type.
    uint32_t type_id = to_typeid(decl->type());

    smx_rtti_debug_var* var;
    if (parent)
        var = &dbg_locals_->add();
    else
        var = &dbg_globals_->add();

    var->address = *addr;
    switch (decl->vclass()) {
        case sLOCAL:
            var->vclass = *addr >= 0 ? kVarClass_Local : kVarClass_Arg;
            break;
        case sGLOBAL:
            var->vclass = kVarClass_Global;
            break;
        case sSTATIC:
            var->vclass = kVarClass_Static;
            break;
        case sARGUMENT:
            var->vclass = kVarClass_Arg;
            break;
        default:
            var->vclass = 0;
            assert(false);
    }
    var->name = names_->add(*cc_.atoms(), decl->name());
    var->code_start = code_start;
    var->code_end = code_end;
    var->type_id = type_id;
}

smx_rtti_debug_method RttiBuilder::add_method(FunctionDecl* fun, uint32_t pcode_start) {
    assert(fun->is_live());

    uint32_t index = methods_->count();
    if (index > kMaxTableIndex)
        report(fun, 484);

    smx_rtti_method& method = methods_->add();
    if (auto mf = fun->as<MemberFunctionDecl>(); mf && mf->is_ctor())
        method.name = names_->add(*cc_.atoms(), ".constructor");
    else
        method.name = names_->add(fun->name());
    method.pcode_start = pcode_start;
    method.pcode_end = 0;
    method.signature = encode_signature(fun->canonical());

    smx_rtti_debug_method debug;
    debug.method_index = index;
    debug.first_local = dbg_locals_->count();
    debug.first_line = dbg_lines_->count();
    debug.line_start = fun->pos().line;
    return debug;
}

static inline void AppendUint16(std::vector<uint8_t>* out, uint16_t value) {
    out->push_back(static_cast<uint8_t>(value & 0xff));
    out->push_back(static_cast<uint8_t>((value >> 8) & 0xff));
}

void RttiBuilder::finish_method(FunctionDecl* fun, const smx_rtti_debug_method& entry,
                                LocalSlotSignature&& locals, uint32_t pcode_end)
{
    auto& method = methods_->at(entry.method_index);
    method.pcode_end = pcode_end;

    if (locals.count || fun->NumUpvars()) {
        std::vector<uint8_t> blob;

        // For closures, upvar slots precede local slots.
        if (fun->NumUpvars()) {
            blob.push_back(cb::kClosureSlots);

            AppendUint16(&blob, (uint16_t)fun->NumUpvars());

            for (size_t i = 0; i < fun->NumUpvars(); i++) {
                auto upvar = fun->GetUpvar(i);
                encode_type_into(blob, upvar->type()->normalize());
            }
        }

        blob.push_back(cb::kLocalSlots);
        AppendUint16(&blob, locals.count);
        blob.insert(blob.end(), locals.types.begin(), locals.types.end());

        method.locals = type_pool_.add(blob);
    } else {
        method.locals = 0;
    }

    method.flags = 0;
    if (auto mf = fun->as<MemberFunctionDecl>()) {
        if (mf->parent()->as<ClassDecl>() && mf->is_ctor())
            method.flags |= kRttiMethod_Ctor;
    }
    if (fun->is_public())
        method.flags |= kRttiMethodVisibility_Public;
    else if (fun->is_native())
        method.flags |= kRttiMethod_Native;
    if (fun->signature()->conv() == FunctionType::Closure)
        method.flags |= kRttiMethod_Closure;
    if (fun->NumUpvars())
        method.flags |= kRttiMethod_HasUpvars;

    // Only add a method table entry if we actually had locals or lines.
    if (entry.first_local != dbg_locals_->count() || entry.first_line != dbg_lines_->count())
        dbg_methods_->add(entry);
}

uint32_t
RttiBuilder::add_enumstruct(Type* type)
{
    TypeIdCache::Insert p = typeid_cache_.findForAdd(type);
    if (p.found())
        return p->value;

    auto es_decl = type->asEnumStruct();
    uint32_t es_index = classdefs_->count();
    if (es_index > kMaxTableIndex)
        report(es_decl, 484);

    typeid_cache_.add(p, type, es_index);

    smx_rtti_classdef classdef;
    memset(&classdef, 0, sizeof(classdef));
    classdef.flags = kClassType_EnumStruct;
    classdef.name = names_->add(*cc_.atoms(), type->declName());
    classdef.first_field = fields_->count();
    classdefs_->add(classdef);

    // Pre-allocate storage in case of nested types.
    const auto& enumlist = es_decl->fields();
    for (size_t i = 0; i < enumlist.size(); i++)
        fields_->add();

    // Add all fields.
    size_t index = 0;
    for (auto iter = enumlist.begin(); iter != enumlist.end(); iter++) {
        auto field = (*iter);

        smx_rtti_field info;
        info.flags = 0;
        info.name = names_->add(field->name());
        info.type_id = to_typeid(field->type());
        uint32_t field_idx = classdef.first_field + index;
        fields_->at(field_idx) = info;

        if (field_idx > kMaxTableIndex) {
            report(es_decl, 484);
            field_idx = kMaxTableIndex;
        }
        field_id_map_[field] = MakeTableId(kTableId_RttiField, field_idx);
        index++;
    }

    return es_index;
}

uint32_t RttiBuilder::add_class(Type* type) {
    assert(type->isClass());

    TypeIdCache::Insert p = typeid_cache_.findForAdd(type);
    if (p.found())
        return p->value;

    auto cls_decl = type->asClass();
    uint32_t cls_index = classdefs_->count();
    if (cls_index > kMaxTableIndex)
        report(cls_decl, 484);

    typeid_cache_.add(p, type, cls_index);

    smx_rtti_classdef classdef;
    memset(&classdef, 0, sizeof(classdef));
    classdef.flags = kClassType_Class;
    classdef.name = names_->add(*cc_.atoms(), type->declName());
    classdef.first_field = fields_->count();
    classdef.first_method = methods_->count();
    classdefs_->add(classdef);

    // Pre-allocate storage in case of nested types.
    const auto& field_list = cls_decl->fields();
    for (size_t i = 0; i < field_list.size(); i++)
        fields_->add();

    // Add all fields.
    size_t index = 0;
    for (auto iter = field_list.begin(); iter != field_list.end(); iter++) {
        auto field = (*iter);

        smx_rtti_field info;
        info.flags = 0;
        info.name = names_->add(field->name());
        info.type_id = to_typeid(field->type());
        uint32_t field_idx = classdef.first_field + index;
        fields_->at(field_idx) = info;

        if (field_idx > kMaxTableIndex) {
            report(field, 484);
            field_idx = kMaxTableIndex;
        }
        field_id_map_[field] = MakeTableId(kTableId_RttiField, field_idx);
        index++;
    }

    return cls_index;
}

uint32_t
RttiBuilder::add_struct(Type* type)
{
    TypeIdCache::Insert p = typeid_cache_.findForAdd(type);
    if (p.found())
        return p->value;

    auto ps = type->asPstruct();

    uint32_t struct_index = classdefs_->count();
    if (struct_index > kMaxTableIndex)
        report(ps, 484);

    typeid_cache_.add(p, type, struct_index);

    smx_rtti_classdef classdef;
    memset(&classdef, 0, sizeof(classdef));
    classdef.flags = kClassType_Struct;
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
        field.type_id = to_typeid(arg->type());
        uint32_t field_idx = classdef.first_field + i;
        fields_->at(field_idx) = field;

        if (field_idx > kMaxTableIndex) {
            report(arg, 484);
            field_idx = kMaxTableIndex;
        }
        field_id_map_[arg] = MakeTableId(kTableId_RttiField, field_idx);
    }
    return struct_index;
}

uint32_t RttiBuilder::AddGlobal(VarDeclBase* decl, Atom* name) {
    uint32_t index = globals_->count();
    smx_rtti_global& global = globals_->add();
    global.name = name ? names_->add(*cc_.atoms(), name) : 0;
    global.type_id = to_typeid(decl->type());
    global.flags = 0;

    if (decl->is_public())
        global.flags = kRttiGlobal_Public;

    return index;
}

uint16_t RttiBuilder::AddString(Atom* atom, DataQueue* data) {
    StringCache::Insert p = string_cache_.findForAdd(atom);
    if (p.found())
        return p->value;

    if (stringpool_->count() >= UINT16_MAX) {
        report(469);
        return 0;
    }

    uint32_t offset = data->dat_address();

    std::string blob;
    if (!EncodeCompactUint32(&blob, (uint32_t)atom->length())) {
        report(470);
        return 0;
    }
    blob.append(atom->chars(), atom->length());

    data->Add(blob.data(), blob.length());

    uint16_t index = (uint16_t)stringpool_->count();
    smx_rtti_string& entry = stringpool_->add();
    entry.offset = offset;

    string_cache_.add(p, atom, index);
    return index;
}

std::optional<uint32_t> RttiBuilder::FindStringDataOffset(Atom* atom) {
    StringCache::Result p = string_cache_.find(atom);
    if (!p.found())
        return {};

    auto index = p->value;
    return {stringpool_->at(index).offset};
}

void RttiBuilder::UpdateGlobalName(uint32_t index, Atom* name) {
    auto& global = globals_->at(index);
    global.name = names_->add(*cc_.atoms(), name);
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

    std::vector<uint8_t> bytes{cb::kFunction};

    uint32_t argc = fun->FormalArgc();
    if (argc > UCHAR_MAX)
        report(45);

    Type* hidden_arg = nullptr;
    Type* return_type = fun->return_type();
    if (fun->signature()->needs_hidden_arg()) {
        hidden_arg = return_type;
        return_type = types_->type_void();
        argc++;
    }

    bytes.push_back((uint8_t)argc);

    if (fun->IsVariadic())
        bytes.push_back(cb::kLegacyVariadic);

    encode_type_into(bytes, return_type);

    if (hidden_arg)
        encode_type_into(bytes, hidden_arg, hidden_arg->isWideInt());
    for (size_t i = 0; i < fun->FormalArgc(); i++) {
        const auto& arg = fun->args()[i];
        encode_type_into(bytes, arg->type(), arg->type()->isWideInt());
    }

    return type_pool_.add(bytes);
}

uint32_t RttiBuilder::add_enum(Type* type) {
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
    bytes.push_back(cb::kClassDef);
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

void RttiBuilder::encode_class_into(std::vector<uint8_t>& bytes, Type* type) {
    bytes.push_back(cb::kClass);
    CompactEncodeUint32(bytes, add_class(type));
}

uint8_t RttiBuilder::TypeToRttiBytecode(Type* type) {
    if (type->isBool())
        return cb::kBool;
    if (type->isAny())
        return cb::kAny;
    if (type->isChar())
        return cb::kChar8;
    if (type->isInt16())
        return cb::kInt16;
    if (type->isFloat())
        return cb::kFloat32;
    if (type->isInt())
        return cb::kInt32;
    if (type->isInt64())
        return cb::kInt64;
    if (type->isIntPtr())
        return cb::kIntPtr;
    if (type->isVoid())
        return cb::kVoid;
    return 0;
}

void RttiBuilder::encode_type_into(std::vector<uint8_t>& bytes, Type* type, bool force_by_ref) {
    encode_type_into(bytes, QualType(type), force_by_ref);
}

void RttiBuilder::encode_type_into(std::vector<uint8_t>& bytes, QualType qt, bool force_by_ref) {
    if (qt.is_const())
        bytes.emplace_back(cb::kConst);

    Type* type = *qt;
    if (auto array = type->as<ArrayType>()) {
        for (;;) {
            if (array->is_flat()) {
                bytes.emplace_back(cb::kFlatArray);
                CompactEncodeUint32(bytes, array->size());
            } else if (array->size()) {
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
    } else if (type->isReference() || force_by_ref) {
        bytes.emplace_back(cb::kByRef);
        if (type->isReference())
            type = type->inner();
    }

    if (uint8_t b = TypeToRttiBytecode(type)) {
        bytes.push_back(b);
        return;
    }

    if (type->isClass()) {
        encode_class_into(bytes, type);
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
            bytes.push_back(cb::kInt32);
        return;
    }

    if (auto ft = type->as<FunctionType>()) {
        if (ft->conv() == FunctionType::Convention::Legacy) {
            bytes.push_back(cb::kInt32);
            return;
        }

        std::vector<uint8_t> signature;
        encode_signature_into(signature, ft);
        uint32_t index = type_pool_.add(signature);

        bytes.push_back(cb::kFunctionPtr);
        CompactEncodeUint32(bytes, index);
        return;
    }

    if (type->isEnumStruct()) {
        encode_enumstruct_into(bytes, type);
        return;
    }

    assert(type->isEnum() || type->isMethodmap());

    encode_enum_into(bytes, type);
}

void
RttiBuilder::encode_funcenum_into(std::vector<uint8_t>& bytes, Type* type, funcenum_t* fe)
{
    uint32_t index = add_typeset(type, fe);
    bytes.push_back(cb::kTypeset);
    CompactEncodeUint32(bytes, index);
}

void RttiBuilder::encode_signature_into(std::vector<uint8_t>& bytes, FunctionType* ft) {
    bytes.push_back(cb::kFunction);
    bytes.push_back((uint8_t)ft->nargs());

    if (ft->variadic())
        bytes.push_back(cb::kLegacyVariadic);

    encode_type_into(bytes, ft->return_type());

    for (size_t i = 0; i < ft->nargs(); i++) {
        QualType type = ft->arg_type(i);
        encode_type_into(bytes, type, type->isWideInt());
    }
}

int32_t RttiBuilder::AddLocalSlot(LocalSlotSignature* locals, QualType type) {
    encode_type_into(locals->types, type);
    return locals->count++;
}

void RttiBuilder::ensure_type_added(Decl* decl) {
    if (auto es = decl->as<EnumStructDecl>())
        add_enumstruct(*es->type());
    else if (auto ps = decl->as<PstructDecl>())
        add_struct(*ps->type());
    else if (auto cls = decl->as<ClassDecl>())
        add_class(*cls->type());
    else
        assert(false);
}

uint32_t RttiBuilder::AddFieldRef(LayoutFieldDecl* decl) {
    auto iter = field_id_map_.find(decl);
    if (iter != field_id_map_.end())
        return iter->second;

    ensure_type_added(decl->parent());

    iter = field_id_map_.find(decl);
    assert(iter != field_id_map_.end());
    return iter->second;
}

} // namespace cc
} // namespace sp
