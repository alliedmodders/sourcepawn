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
#pragma once

#include <inttypes.h>

#include <amtl/am-hashmap.h>
#include <smx/smx-v1.h>
#include "compile-context.h"
#include "libsmx/data-pool.h"
#include "libsmx/smx-builder.h"
#include "libsmx/smx-encoding.h"
#include "parse-node.h"
#include "types.h"

namespace sp {
namespace cc {

class CodeGenerator;
class DebugString;

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

} // namespace cc
} // namespace sp
