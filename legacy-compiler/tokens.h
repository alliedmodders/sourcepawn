// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
//  Copyright (c) ITB CompuPhase, 1997-2006
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

namespace sp {
namespace cc {

/*  Tokens recognized by lex()
 *  Some of these constants are assigned as well to the variable "lastst" (see SC1.C)
 */
enum TokenKind {
    /* value of first multi-character operator */
    tFIRST = 256,
    /* multi-character operators */
    taMULT = tFIRST, /* *= */
    taDIV,           /* /= */
    taMOD,           /* %= */
    taADD,           /* += */
    taSUB,           /* -= */
    taSHL,           /* <<= */
    taSHRU,          /* >>>= */
    taSHR,           /* >>= */
    taAND,           /* &= */
    taXOR,           /* ^= */
    taOR,            /* |= */
    tlOR,            /* || */
    tlAND,           /* && */
    tlEQ,            /* == */
    tlNE,            /* != */
    tlLE,            /* <= */
    tlGE,            /* >= */
    tSHL,            /* << */
    tSHRU,           /* >>> */
    tSHR,            /* >> */
    tINC,            /* ++ */
    tDEC,            /* -- */
    tELLIPS,         /* ... */
    tDBLDOT,         /* .. */
    tDBLCOLON,       /* :: */
    /* value of last multi-character operator */
    tMIDDLE = tDBLCOLON,
    /* reserved words (statements) */
    tACQUIRE,
    tAS,
    tASSERT,
    tBREAK,
    tBUILTIN,
    tCATCH,
    tCASE,
    tCAST_TO,
    tCHAR,
    tCONST,
    tCONTINUE,
    tDECL,
    tDEFAULT,
    tDEFINED,
    tDELETE,
    tDO,
    tDOUBLE,
    tELSE,
    tENUM,
    tEXIT,
    tEXPLICIT,
    tFALSE,
    tFINALLY,
    tFOR,
    tFOREACH,
    tFORWARD,
    tFUNCENUM,
    tFUNCTAG,
    tFUNCTION,
    tGOTO,
    tIF,
    tIMPLICIT,
    tIMPORT,
    tIN,
    tINT,
    tINT8,
    tINT16,
    tINT32,
    tINT64,
    tINTERFACE,
    tINTN,
    tINVALID_FUNCTION,
    tLET,
    tMETHODMAP,
    tNAMESPACE,
    tNATIVE,
    tNEW,
    tNULL,
    tNULLABLE,
    tOBJECT,
    tOPERATOR,
    tPACKAGE,
    tPRIVATE,
    tPROTECTED,
    tPUBLIC,
    tREADONLY,
    tRETURN,
    tSEALED,
    tSIZEOF,
    tSTATIC,
    tSTATIC_ASSERT,
    tSTOCK,
    tSTRUCT,
    tSWITCH,
    tTHIS,
    tTHROW,
    tTRUE,
    tTRY,
    tTYPEDEF,
    tTYPEOF,
    tTYPESET,
    tUINT8,
    tUINT16,
    tUINT32,
    tUINT64,
    tUINTN,
    tUNION,
    tUSING,
    tVAR,
    tVARIANT,
    tVIEW_AS,
    tVIRTUAL,
    tVOID,
    tVOLATILE,
    tWHILE,
    tWITH,
    /* compiler directives */
    tpASSERT, /* #assert */
    tpDEFINE,
    tpELSE,   /* #else */
    tpELSEIF, /* #elseif */
    tpENDIF,
    tpENDINPUT,
    tpENDSCRPT,
    tpERROR,
    tpWARNING,
    tpIF, /* #if */
    tINCLUDE,
    tpLINE,
    tpPRAGMA,
    tpTRYINCLUDE,
    tpUNDEF,
    tLAST = tpUNDEF, /* value of last multi-character match-able token */
    /* semicolon is a special case, because it can be optional */
    tTERM,    /* semicolon or newline */
    tENDEXPR, /* forced end of expression */
    /* other recognized tokens */
    tNUMBER,   /* integer number */
    tNUMBER64,
    tRATIONAL, /* rational number */
    tSYMBOL,
    tLABEL,
    tSTRING,
    tCHAR_LITERAL,
    tSYN_PRAGMA_UNUSED,
    tSYN_INCLUDE_PATH,
    tEOL,            /* newline, only returned by peek_new_line() */
    tNEWDECL,        /* for declloc() */
    tENTERED_MACRO,  /* internal lexer command */
    tMAYBE_LABEL,    /* internal lexer command, followed by ':' */
    // Make sure to update the token list in lexer.cpp.
    tLAST_TOKEN_ID
};

} // namespace cc
} // namespace sp
