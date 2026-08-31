// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
//
#pragma once

#include <inttypes.h>
#include <unordered_map>

#include <amtl/am-hashmap.h>
#include <smx/smx-v1.h>
#include <smx/smx-typeinfo.h>
#include "compile-context.h"
#include "libsmx/data-pool.h"
#include "libsmx/smx-builder.h"
#include "libsmx/smx-encoding.h"
#include "parse-node.h"
#include "types.h"

namespace sp {
namespace cc {

class DataQueue;

typedef SmxBlobSection<sp_fdbg_info_t> SmxDebugInfoSection;
typedef SmxListSection<sp_fdbg_line_t> SmxDebugLineSection;
typedef SmxListSection<sp_fdbg_file_t> SmxDebugFileSection;

struct LocalSlotSignature {
  std::vector<uint8_t> types;
  uint32_t count = 0;
};

class RttiBuilder
{
  public:
    RttiBuilder(CompileContext& cc, SmxNameTable* names);

    void finish(SmxBuilder& builder);
    smx_rtti_debug_method add_method(FunctionDecl* fun, uint32_t pcode_start);
    void finish_method(FunctionDecl* fun, const smx_rtti_debug_method& entry,
                       LocalSlotSignature&& locals, uint32_t pcode_end);

    int32_t AddLocalSlot(LocalSlotSignature* locals, QualType type);
    uint32_t AddGlobal(VarDeclBase* decl, Atom* name);
    uint16_t AddString(Atom* atom, DataQueue* data);
    uint32_t AddFieldRef(LayoutFieldDecl* decl);
    std::optional<uint32_t> FindStringDataOffset(Atom* atom);

    void AddDebugFile(ucell codeidx, const char* file);
    void AddDebugLine(uint16_t addr, uint16_t line);
    void AddDebugVar(FunctionDecl* parent, Decl* decl, uint32_t code_start, uint32_t code_end);

    const smx_rtti_method& GetMethod(uint32_t method_index) const {
        return methods_->at(method_index);
    }
    void UpdateGlobalName(uint32_t index, Atom* name);

    uint32_t to_typeid(QualType type);
    uint32_t to_typeid(Type* type) {
        return to_typeid(QualType(type));
    }

    uint32_t add_class(Type* type);
    uint32_t add_enumstruct(Type* type);

  private:
    uint32_t add_enum(Type* type);
    uint32_t add_typeset(Type* type, funcenum_t* fe);
    uint32_t add_struct(Type* type);
    uint32_t encode_signature(FunctionDecl* decl);
    void encode_signature_into(std::vector<uint8_t>& bytes, FunctionType* ft);
    void encode_enum_into(std::vector<uint8_t>& bytes, Type* type);
    void encode_type_into(std::vector<uint8_t>& bytes, Type* type, bool force_by_ref = false);
    void encode_type_into(std::vector<uint8_t>& bytes, QualType qt, bool force_by_ref = false);

    void encode_funcenum_into(std::vector<uint8_t>& bytes, Type* type, funcenum_t* fe);
    void encode_struct_into(std::vector<uint8_t>& bytes, Type* type);
    void encode_enumstruct_into(std::vector<uint8_t>& bytes, Type* type);
    void encode_class_into(std::vector<uint8_t>& bytes, Type* type);

    //void add_debug_var(SmxRttiTable<smx_rtti_debug_var>* table, DebugString& str);
    void build_debuginfo();

    uint8_t TypeToRttiBytecode(Type* type);
    void ensure_type_added(Decl* decl);

  private:
    CompileContext& cc_;
    TypeManager* types_ = nullptr;
    RefPtr<SmxNameTable> names_;
    DataPool type_pool_;
    RefPtr<SmxBlobSection<void>> rtti_data_;
    RefPtr<SmxRttiTable<smx_rtti_method>> methods_;
    RefPtr<SmxRttiTable<smx_rtti_enum>> enums_;
    RefPtr<SmxRttiTable<smx_rtti_typeset>> typesets_;
    RefPtr<SmxRttiTable<smx_rtti_classdef>> classdefs_;
    RefPtr<SmxRttiTable<smx_rtti_field>> fields_;
    RefPtr<SmxRttiTable<smx_rtti_string>> stringpool_;
    RefPtr<SmxRttiTable<smx_rtti_global>> globals_;
    RefPtr<SmxDebugInfoSection> dbg_info_;
    RefPtr<SmxRttiTable<smx_rtti_debug_line>> dbg_lines_;
    RefPtr<SmxDebugFileSection> dbg_files_;
    RefPtr<SmxRttiTable<smx_rtti_debug_method>> dbg_methods_;
    RefPtr<SmxRttiTable<smx_rtti_debug_var>> dbg_globals_;
    RefPtr<SmxRttiTable<smx_rtti_debug_var>> dbg_locals_;

    typedef ke::HashMap<Type*, uint32_t, ke::PointerPolicy<Type>> TypeIdCache;
    TypeIdCache typeid_cache_;
    typedef ke::HashMap<Atom*, uint16_t, ke::PointerPolicy<Atom>> StringCache;
    StringCache string_cache_;

    std::unordered_map<LayoutFieldDecl*, uint32_t> field_id_map_;

    ucell last_file_addr_ = 0;
    std::string last_file_name_;
};

} // namespace cc
} // namespace sp
