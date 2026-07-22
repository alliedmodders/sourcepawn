// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
// Copyright (C) 2006-2015 AlliedModders LLC
//
// This file is part of SourcePawn. SourcePawn is free software: you can
// redistribute it and/or modify it under the terms of the GNU General Public
// License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
//
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.
//
#pragma once

#include <string.h>

#include <algorithm>
#include <array>

#include <sp_typeutil.h>
#include <sp_vm_types.h>

/* Parameter flags */
#define SM_PARAM_COPYBACK (1 << 0) /**< Copy an array/reference back after call */

/* String parameter flags (separate from parameter flags) */
#define SM_PARAM_STRING_UTF8 (1 << 1)   /**< String should be UTF-8 handled */
#define SM_PARAM_STRING_COPY (1 << 2)   /**< String should be copied into the plugin */
#define SM_PARAM_STRING_BINARY (1 << 3) /**< String should be handled as binary data */

namespace sp {

struct CallArgs {
    CallArgs() {}

    CallArgs(const CallArgs& other)
      : api_version(other.api_version),
        argc(other.argc),
        error(other.error)
    {
        std::copy_n(other.argv.begin(), other.argc, argv.begin());
    }

    void PushCell(cell_t cell) {
        if (argc >= argv.size()) {
            error = true;
            return;
        }
        argv[argc].u.value = cell;
        argv[argc].type = ARG_CELL;
        argv[argc].flags = 0;
        argc++;
    }

    void PushCellByRef(cell_t* cell, int flags = SM_PARAM_COPYBACK) {
        if (argc >= argv.size()) {
            error = true;
            return;
        }
        argv[argc].u.addr = cell;
        argv[argc].flags = flags;
        argv[argc].type = ARG_CELL_BY_REF;
        argc++;
    }

    void PushFloat(float number) {
        if (argc >= argv.size()) {
            error = true;
            return;
        }
        argv[argc].u.value = sp_ftoc(number);
        argv[argc].type = ARG_CELL;
        argv[argc].flags = FLAG_CELL_TYPE_FLOAT;
        argc++;
    }

    void PushFloatByRef(float* number, int flags = SM_PARAM_COPYBACK) {
        if (argc >= argv.size()) {
            error = true;
            return;
        }
        argv[argc].u.addr = number;
        argv[argc].flags = flags | FLAG_CELL_TYPE_FLOAT;
        argv[argc].type = ARG_CELL_BY_REF;
        argc++;
    }

    void PushArray(cell_t* inarray, unsigned int cells, int flags = 0) {
        if (argc >= argv.size()) {
            error = true;
            return;
        }
        argv[argc].u.addr = inarray;
        argv[argc].flags = flags;
        argv[argc].type = ARG_ARRAY;
        argv[argc].array_size = cells;
        argc++;
    }

    void PushString(const char* string) {
        if (argc >= argv.size()) {
            error = true;
            return;
        }
        argv[argc].u.addr = (void*)string;
        argv[argc].flags = SM_PARAM_STRING_COPY;
        argv[argc].type = ARG_CHAR_ARRAY;
        argv[argc].array_size = strlen(string) + 1;
        argc++;
    }

    void PushString(char* buffer, size_t length, int flags) {
        if (argc >= argv.size()) {
            error = true;
            return;
        }
        argv[argc].u.addr = buffer;
        argv[argc].flags = flags;
        argv[argc].type = ARG_CHAR_ARRAY;
        argv[argc].array_size = length;
        argc++;
    }

    void PushInt64(int64_t value) {
        if (argc >= argv.size()) {
            error = true;
            return;
        }
        argv[argc].u.i64 = value;
        argv[argc].flags = 0;
        argv[argc].type = ARG_INT64;
        argc++;
    }



    void Reset() {
        argc = 0;
        error = false;
    }

    enum ArgType {
        ARG_CELL,
        ARG_CELL_BY_REF,
        ARG_ARRAY,
        ARG_CHAR_ARRAY,
        ARG_INT64,

    };

    enum Flag {
        FLAG_COPYBACK = SM_PARAM_COPYBACK,
        FLAG_STRING_UTF8 = SM_PARAM_STRING_UTF8,
        FLAG_STRING_COPY = SM_PARAM_STRING_COPY,
        FLAG_STRING_BINARY = SM_PARAM_STRING_BINARY,
        FLAG_CELL_TYPE_FLOAT = (1 << 4),
    };

    CallArgs& operator =(const CallArgs& other) {
        api_version = other.api_version;
        argc = other.argc;
        error = other.error;
        std::copy_n(other.argv.begin(), other.argc, argv.begin());
        return *this;
    }

    int api_version = kApiVersion;

    struct ArgInfo {
        union {
            cell_t value;
            float f32;
            void* addr;
            int64_t i64;
        } u;
        uint16_t flags;
        uint16_t type;
        uint32_t array_size;
    };
    std::array<ArgInfo, SP_MAX_EXEC_PARAMS> argv;
    uint32_t argc = 0;
    bool error = false;
};

} // namespace sp
