/*  Pawn compiler - Peephole optimizer "sequences" strings (plain
 *                   and compressed formats)
 *
 *  Copyright (c) ITB CompuPhase, 2000-2006
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

#define SCPACK_TERMINATOR ,     /* end each section with a comma */

#define SCPACK_TABLE sequences_table

#define seqsize(o,p)    (opcodes(o)+opargs(p))
typedef struct {
  const char *find;
  const char *replace;
  int savesize;         /* number of bytes saved (in bytecode) */
} SEQUENCE;
static SEQUENCE sequences_cmp[] = {
  /* A very common sequence in four varieties
   *    load.s.pri n1           load.s.pri n2
   *    push.pri                load.s.alt n1
   *    load.s.pri n2           -
   *    pop.alt                 -
   *    --------------------------------------
   *    load.pri n1             load.s.pri n2
   *    push.pri                load.alt n1
   *    load.s.pri n2           -
   *    pop.alt                 -
   *    --------------------------------------
   *    load.s.pri n1           load.pri n2
   *    push.pri                load.s.alt n1
   *    load.pri n2             -
   *    pop.alt                 -
   *    --------------------------------------
   *    load.pri n1             load.pri n2
   *    push.pri                load.alt n1
   *    load.pri n2             -
   *    pop.alt                 -
   */
  {
      "load.s.pri %1!push.pri!load.s.pri %2!pop.alt!",
      "load.s.pri %2!load.s.alt %1!",
    seqsize(4,2) - seqsize(2,2)
  },
  {
      "load.pri %1!push.pri!load.s.pri %2!pop.alt!",
      "load.s.pri %2!load.alt %1!",
    seqsize(4,2) - seqsize(2,2)
  },
  {
      "load.s.pri %1!push.pri!load.pri %2!pop.alt!",
      "load.pri %2!load.s.alt %1!",
    seqsize(4,2) - seqsize(2,2)
  },
  {
      "load.pri %1!push.pri!load.pri %2!pop.alt!",
      "load.pri %2!load.alt %1!",
    seqsize(4,2) - seqsize(2,2)
  },
  /* (#1#) The above also occurs with "addr.pri" (array
   * indexing) as the first line; so that adds 2 cases.
   */
  {
      "addr.pri %1!push.pri!load.s.pri %2!pop.alt!",
      "addr.alt %1!load.s.pri %2!",
    seqsize(4,2) - seqsize(2,2)
  },
  {
      "addr.pri %1!push.pri!load.pri %2!pop.alt!",
      "addr.alt %1!load.pri %2!",
    seqsize(4,2) - seqsize(2,2)
  },
  /* And the same sequence with const.pri as either the first
   * or the second load instruction: four more cases.
   */
  {
      "const.pri %1!push.pri!load.s.pri %2!pop.alt!",
      "load.s.pri %2!const.alt %1!",
    seqsize(4,2) - seqsize(2,2)
  },
  {
      "const.pri %1!push.pri!load.pri %2!pop.alt!",
      "load.pri %2!const.alt %1!",
    seqsize(4,2) - seqsize(2,2)
  },
  {
      "load.s.pri %1!push.pri!const.pri %2!pop.alt!",
      "const.pri %2!load.s.alt %1!",
    seqsize(4,2) - seqsize(2,2)
  },
  {
      "load.pri %1!push.pri!const.pri %2!pop.alt!",
      "const.pri %2!load.alt %1!",
    seqsize(4,2) - seqsize(2,2)
  },
  /* The same as above, but now with "addr.pri" (array
   * indexing) on the first line and const.pri on
   * the second.
   */
  {
      "addr.pri %1!push.pri!const.pri %2!pop.alt!",
      "addr.alt %1!const.pri %2!",
    seqsize(4,2) - seqsize(2,2)
  },
  {
      "addr.pri %1!push.pri!zero.pri!pop.alt!",
      "addr.alt %1!zero.pri!",
    seqsize(4,1) - seqsize(2,1)
  },
  /* ??? add references */
  /* Chained relational operators can contain sequences like:
   *    move.pri                load.s.pri n1
   *    push.pri                -
   *    load.s.pri n1           -
   *    pop.alt                 -
   * The above also accurs for "load.pri" and for "const.pri",
   * so add another two cases.
   */
  {
      "move.pri!push.pri!load.s.pri %1!pop.alt!",
      "load.s.pri %1!",
    seqsize(4,1) - seqsize(1,1)
  },
  {
      "move.pri!push.pri!load.pri %1!pop.alt!",
      "load.pri %1!",
    seqsize(4,1) - seqsize(1,1)
  },
  {
      "move.pri!push.pri!const.pri %1!pop.alt!",
      "const.pri %1!",
    seqsize(4,1) - seqsize(1,1)
  },
  /* More optimizations for chained relational operators; the
   * continuation sequences can be simplified if they turn out
   * to be termination sequences:
   *    xchg                    sless       also for sless, sgeq and sleq
   *    sgrtr                   pop.alt
   *    swap.alt                and
   *    and                     ;$exp
   *    pop.alt                 -
   *    ;$exp                   -
   *    --------------------------------------
   *    xchg                    sless       also for sless, sgeq and sleq
   *    sgrtr                   pop.alt
   *    swap.alt                and
   *    and                     jzer n1
   *    pop.alt                 -
   *    jzer n1                 -
   *    --------------------------------------
   *    xchg                    jsgeq  n1   also for sless, sgeq and sleq
   *    sgrtr                   ;$exp       (occurs for non-chained comparisons)
   *    jzer n1                 -
   *    ;$exp                   -
   *    --------------------------------------
   *    xchg                    sless       also for sless, sgeq and sleq
   *    sgrtr                   ;$exp       (occurs for non-chained comparisons)
   *    ;$exp                   -
   */
  {
      "xchg!sgrtr!swap.alt!and!pop.alt!;$exp!",
      "sless!pop.alt!and!;$exp!",
    seqsize(5,0) - seqsize(3,0)
  },
  {
      "xchg!sless!swap.alt!and!pop.alt!;$exp!",
      "sgrtr!pop.alt!and!;$exp!",
    seqsize(5,0) - seqsize(3,0)
  },
  {
      "xchg!sgeq!swap.alt!and!pop.alt!;$exp!",
      "sleq!pop.alt!and!;$exp!",
    seqsize(5,0) - seqsize(3,0)
  },
  {
      "xchg!sleq!swap.alt!and!pop.alt!;$exp!",
      "sgeq!pop.alt!and!;$exp!",
    seqsize(5,0) - seqsize(3,0)
  },
  {
      "xchg!sgrtr!swap.alt!and!pop.alt!jzer %1!",
      "sless!pop.alt!and!jzer %1!",
    seqsize(5,0) - seqsize(3,0)
  },
  {
      "xchg!sless!swap.alt!and!pop.alt!jzer %1!",
      "sgrtr!pop.alt!and!jzer %1!",
    seqsize(5,0) - seqsize(3,0)
  },
  {
      "xchg!sgeq!swap.alt!and!pop.alt!jzer %1!",
      "sleq!pop.alt!and!jzer %1!",
    seqsize(5,0) - seqsize(3,0)
  },
  {
      "xchg!sleq!swap.alt!and!pop.alt!jzer %1!",
      "sgeq!pop.alt!and!jzer %1!",
    seqsize(5,0) - seqsize(3,0)
  },
  {
      "xchg!sgrtr!jzer %1!;$exp!",
      "jsgeq %1!;$exp!",
    seqsize(3,1) - seqsize(1,1)
  },
  {
      "xchg!sless!jzer %1!;$exp!",
      "jsleq %1!;$exp!",
    seqsize(3,1) - seqsize(1,1)
  },
  {
      "xchg!sgeq!jzer %1!;$exp!",
      "jsgrtr %1!;$exp!",
    seqsize(3,1) - seqsize(1,1)
  },
  {
      "xchg!sleq!jzer %1!;$exp!",
      "jsless %1!;$exp!",
    seqsize(3,1) - seqsize(1,1)
  },
  {
      "xchg!sgrtr!;$exp!",
      "sless!;$exp!",
    seqsize(2,0) - seqsize(1,0)
  },
  {
      "xchg!sless!;$exp!",
      "sgrtr!;$exp!",
    seqsize(2,0) - seqsize(1,0)
  },
  {
      "xchg!sgeq!;$exp!",
      "sleq!;$exp!",
    seqsize(2,0) - seqsize(1,0)
  },
  {
      "xchg!sleq!;$exp!",
      "sgeq!;$exp!",
    seqsize(2,0) - seqsize(1,0)
  },
  /* The entry to chained operators is also opt to optimization
   *    load.s.pri n1           load.s.pri n2
   *    load.s.alt n2           load.s.alt n1
   *    xchg                    -
   *    --------------------------------------
   *    load.s.pri n1           load.pri n2
   *    load.alt n2             load.s.alt n1
   *    xchg                    -
   *    --------------------------------------
   *    load.s.pri n1           const.pri n2
   *    const.alt n2            load.s.alt n1
   *    xchg                    -
   *    --------------------------------------
   * and all permutations...
   */
  {
      "load.s.pri %1!load.s.alt %2!xchg!",
      "load.s.pri %2!load.s.alt %1!",
    seqsize(3,2) - seqsize(2,2)
  },
  {
      "load.s.pri %1!load.alt %2!xchg!",
      "load.pri %2!load.s.alt %1!",
    seqsize(3,2) - seqsize(2,2)
  },
  {
      "load.s.pri %1!const.alt %2!xchg!",
      "const.pri %2!load.s.alt %1!",
    seqsize(3,2) - seqsize(2,2)
  },
  {
      "load.pri %1!load.s.alt %2!xchg!",
      "load.s.pri %2!load.alt %1!",
    seqsize(3,2) - seqsize(2,2)
  },
  {
      "load.pri %1!load.alt %2!xchg!",
      "load.pri %2!load.alt %1!",
    seqsize(3,2) - seqsize(2,2)
  },
  {
      "load.pri %1!const.alt %2!xchg!",
      "const.pri %2!load.alt %1!",
    seqsize(3,2) - seqsize(2,2)
  },
  {
      "const.pri %1!load.s.alt %2!xchg!",
      "load.s.pri %2!const.alt %1!",
    seqsize(3,2) - seqsize(2,2)
  },
  {
      "const.pri %1!load.alt %2!xchg!",
      "load.pri %2!const.alt %1!",
    seqsize(3,2) - seqsize(2,2)
  },
  /* some sequences where PRI is moved to ALT can be optimized
   * further when considering what follows
   *    move.alt                const.alt n1
   *    const.pri %1            -
   *    xchg                    -
   * (also for load.s.pri and load.pri)
   *    --------------------------------------
   *    lref.pri %1             lref.alt %1
   *    move.alt                [load.pri %2]
   *    [load.pri %2]           -
   * (where [load.pri %2] may also be another operatrion loading PRI)
   */
  {
      "move.alt!const.pri %1!xchg!",
      "const.alt %1!",
    seqsize(3,1) - seqsize(1,1)
  },
  {
      "move.alt!load.pri %1!xchg!",
      "load.alt %1!",
    seqsize(3,1) - seqsize(1,1)
  },
  {
      "move.alt!load.s.pri %1!xchg!",
      "load.s.alt %1!",
    seqsize(3,1) - seqsize(1,1)
  },
  /* ----- */
  {
      "lref.pri %1!move.alt!load.pri %2!",
      "lref.alt %1!load.pri %2!",
    seqsize(3,2) - seqsize(2,2)
  },
  {
      "lref.pri %1!move.alt!load.s.pri %2!",
      "lref.alt %1!load.s.pri %2!",
    seqsize(3,2) - seqsize(2,2)
  },
  {
      "lref.pri %1!move.alt!const.pri %2!",
      "lref.alt %1!const.pri %2!",
    seqsize(3,2) - seqsize(2,2)
  },
  {
      "lref.s.pri %1!move.alt!load.pri %2!",
      "lref.s.alt %1!load.pri %2!",
    seqsize(3,2) - seqsize(2,2)
  },
  {
      "lref.s.pri %1!move.alt!load.s.pri %2!",
      "lref.s.alt %1!load.s.pri %2!",
    seqsize(3,2) - seqsize(2,2)
  },
  {
      "lref.s.pri %1!move.alt!const.pri %2!",
      "lref.s.alt %1!const.pri %2!",
    seqsize(3,2) - seqsize(2,2)
  },
  /* Array indexing can merit from special instructions.
   * Simple indexed array lookup can be optimized quite
   * a bit.
   *
   * NOTE: These are documented as using idxaddr.b/lidx.b
   * variants which we no longer generate.
   *
   *    addr.pri n1             addr.alt n1
   *    push.pri                load.s.pri n2
   *    load.s.pri n2           bounds n3
   *    bounds n3               lidx.b n4
   *    shl.c.pri n4            -
   *    pop.alt                 -
   *    add                     -
   *    load.i                  -
   *
   * And to prepare for storing a value in an array
   *    addr.pri n1             addr.alt n1
   *    push.pri                load.s.pri n2
   *    load.s.pri n2           bounds n3
   *    bounds n3               idxaddr.b n4
   *    shl.c.pri n4            -
   *    pop.alt                 -
   *    add                     -
   *
   * Notes (additional cases):
   * 1. instruction addr.pri can also be const.pri (for
   *    global arrays)
   * 2. the bounds instruction can be absent
   * 3. when "n4" (the shift value) is the 2 (with 32-bit cells), use the
   *    even more optimal instructions LIDX and IDDXADDR
   *
   * If the array index is more complex, one can only optimize
   * the last four instructions:
   *    shl.c.pri n1            pop.alt
   *    pop.alt                 lidx.b n1
   *    add                     -
   *    loadi                   -
   *    --------------------------------------
   *    shl.c.pri n1            pop.alt
   *    pop.alt                 idxaddr.b n1
   *    add                     -
   */
#if !defined BIT16
  /* loading from array, "cell" shifted */
  {
      "addr.pri %1!push.pri!load.s.pri %2!bounds %3!shl.c.pri 2!pop.alt!add!load.i!",
      "addr.alt %1!load.s.pri %2!bounds %3!lidx!",
    seqsize(8,4) - seqsize(4,3)
  },
  {
      "const.pri %1!push.pri!load.s.pri %2!bounds %3!shl.c.pri 2!pop.alt!add!load.i!",
      "const.alt %1!load.s.pri %2!bounds %3!lidx!",
    seqsize(8,4) - seqsize(4,3)
  },
  {
      "addr.pri %1!push.pri!load.s.pri %2!shl.c.pri 2!pop.alt!add!load.i!",
      "addr.alt %1!load.s.pri %2!lidx!",
    seqsize(7,3) - seqsize(3,2)
  },
  {
      "const.pri %1!push.pri!load.s.pri %2!shl.c.pri 2!pop.alt!add!load.i!",
      "const.alt %1!load.s.pri %2!lidx!",
    seqsize(7,3) - seqsize(3,2)
  },
  /* array index calculation for storing a value, "cell" aligned */
  {
      "addr.pri %1!push.pri!load.s.pri %2!bounds %3!shl.c.pri 2!pop.alt!add!",
      "addr.alt %1!load.s.pri %2!bounds %3!idxaddr!",
    seqsize(7,4) - seqsize(4,3)
  },
  {
      "const.pri %1!push.pri!load.s.pri %2!bounds %3!shl.c.pri 2!pop.alt!add!",
      "const.alt %1!load.s.pri %2!bounds %3!idxaddr!",
    seqsize(7,4) - seqsize(4,3)
  },
  {
      "addr.pri %1!push.pri!load.s.pri %2!shl.c.pri 2!pop.alt!add!",
      "addr.alt %1!load.s.pri %2!idxaddr!",
    seqsize(6,3) - seqsize(3,2)
  },
  {
      "const.pri %1!push.pri!load.s.pri %2!shl.c.pri 2!pop.alt!add!",
      "const.alt %1!load.s.pri %2!idxaddr!",
    seqsize(6,3) - seqsize(3,2)
  },
  /* the shorter array indexing sequences, see above for comments */
  {
      "shl.c.pri 2!pop.alt!add!loadi!",
      "pop.alt!lidx!",
    seqsize(4,1) - seqsize(2,0)
  },
  {
      "shl.c.pri 2!pop.alt!add!",
      "pop.alt!idxaddr!",
    seqsize(3,1) - seqsize(2,0)
  },
#endif
  /* For packed arrays, there is another case (packed arrays
   * do not take advantage of the LIDX or IDXADDR instructions).
   *    addr.pri n1             addr.alt n1
   *    push.pri                load.s.pri n2
   *    load.s.pri n2           bounds n3
   *    bounds n3               -
   *    pop.alt                 -
   *
   * Notes (additional cases):
   * 1. instruction addr.pri can also be const.pri (for
   *    global arrays)
   * 2. the bounds instruction can be absent, but that
   *    case is already handled (see #1#)
   */
  {
      "addr.pri %1!push.pri!load.s.pri %2!bounds %3!pop.alt!",
      "addr.alt %1!load.s.pri %2!bounds %3!",
    seqsize(5,3) - seqsize(3,3)
  },
  {
      "const.pri %1!push.pri!load.s.pri %2!bounds %3!pop.alt!",
      "const.alt %1!load.s.pri %2!bounds %3!",
    seqsize(5,3) - seqsize(3,3)
  },
  /* Declaration of simple variables often follows the sequence:
   *    ;$lcl <name> <stk>      ;$lcl <name> <stk>
   *    stack -4                push.c <constval>
   *    const.pri <constval>    ;$exp
   *    stor.s.pri <stk>        -
   *    ;$exp                   -
   */
  {
      ";$lcl %1 %2!stack -4!const.pri %3!stor.s.pri %2!;$exp!",
      ";$lcl %1 %2!push.c %3!;$exp!",
    seqsize(3,3) - seqsize(1,1)
  },
  {
      ";$lcl %1 %2!stack -4!zero.pri!stor.s.pri %2!;$exp!",
      ";$lcl %1 %2!push.c 0!;$exp!",
    seqsize(3,2) - seqsize(1,1)
  },
  /* During a calculation, the intermediate result must sometimes
   * be moved from PRI to ALT, like in:
   *    push.pri                move.alt
   *    load.s.pri n1           load.s.pri n1
   *    pop.alt                 -
   *
   * The above also accurs for "load.pri" and for "const.pri",
   * so add another two cases.
   */
  {
      "push.pri!load.s.pri %1!pop.alt!",
      "move.alt!load.s.pri %1!",
    seqsize(3,1) - seqsize(2,1)
  },
  {
      "push.pri!load.pri %1!pop.alt!",
      "move.alt!load.pri %1!",
    seqsize(3,1) - seqsize(2,1)
  },
  {
      "push.pri!const.pri %1!pop.alt!",
      "move.alt!const.pri %1!",
    seqsize(3,1) - seqsize(2,1)
  },
  {
      "push.pri!zero.pri!pop.alt!",
      "move.alt!zero.pri!",
    seqsize(3,0) - seqsize(2,0)
  },
  /* saving PRI and then loading from its address
   * occurs when indexing a multi-dimensional array
   */
  {
      "push.pri!load.i!pop.alt!",
      "move.alt!load.i!",
    seqsize(3,0) - seqsize(2,0)
  },
  /* An even simpler PUSH/POP optimization (occurs in
   * switch statements):
   *    push.pri                move.alt
   *    pop.alt                 -
   */
  {
      "push.pri!pop.alt!",
      "move.alt!",
    seqsize(2,0) - seqsize(1,0)
  },
  /* Some simple arithmetic sequences
   */
  {
      "move.alt!load.s.pri %1!add!",
      "load.s.alt %1!add!",
    seqsize(3,1) - seqsize(2,1)
  },
  {
      "move.alt!load.pri %1!add!",
      "load.alt %1!add!",
    seqsize(3,1) - seqsize(2,1)
  },
  {
      "move.alt!const.pri %1!add!",
      "const.alt %1!add!",
    seqsize(3,1) - seqsize(2,1)
  },
  {
      "move.alt!load.s.pri %1!sub.alt!",
      "load.s.alt %1!sub!",
    seqsize(3,1) - seqsize(2,1)
  },
  {
      "move.alt!load.pri %1!sub.alt!",
      "load.alt %1!sub!",
    seqsize(3,1) - seqsize(2,1)
  },
  {
      "move.alt!const.pri %1!sub.alt!",
      "const.alt %1!sub!",
    seqsize(3,1) - seqsize(2,1)
  },
  /* User-defined operators first load the operands into registers and
   * then have them pushed onto the stack. This can give rise to sequences
   * like:
   *    const.pri n1            push.c n1
   *    const.alt n2            push.c n2
   *    push.pri                -
   *    push.alt                -
   * A similar sequence occurs with the two PUSH.pri/alt instructions inverted.
   * The first, second, or both CONST.pri/alt instructions can also be
   * LOAD.pri/alt.
   * This gives 2 x 4 cases.
   */
  {
      "const.pri %1!const.alt %2!push.pri!push.alt!",
      "push.c %1!push.c %2!",
    seqsize(4,2) - seqsize(2,2)
  },
  {
      "const.pri %1!const.alt %2!push.alt!push.pri!",
      "push.c %2!push.c %1!",
    seqsize(4,2) - seqsize(2,2)
  },
  {
      "const.pri %1!load.alt %2!push.pri!push.alt!",
      "push.c %1!push %2!",
    seqsize(4,2) - seqsize(2,2)
  },
  {
      "const.pri %1!load.alt %2!push.alt!push.pri!",
      "push %2!push.c %1!",
    seqsize(4,2) - seqsize(2,2)
  },
  {
      "load.pri %1!const.alt %2!push.pri!push.alt!",
      "push %1!push.c %2!",
    seqsize(4,2) - seqsize(2,2)
  },
  {
      "load.pri %1!const.alt %2!push.alt!push.pri!",
      "push.c %2!push %1!",
    seqsize(4,2) - seqsize(2,2)
  },
  {
      "load.pri %1!load.alt %2!push.pri!push.alt!",
      "push %1!push %2!",
    seqsize(4,2) - seqsize(2,2)
  },
  {
      "load.pri %1!load.alt %2!push.alt!push.pri!",
      "push %2!push %1!",
    seqsize(4,2) - seqsize(2,2)
  },
  /* Function calls (parameters are passed on the stack)
   *    load.s.pri n1           push.s n1
   *    push.pri                -
   *    --------------------------------------
   *    load.pri n1             push n1
   *    push.pri                -
   *    --------------------------------------
   *    const.pri n1            push.c n1
   *    push.pri                -
   *    --------------------------------------
   *    zero.pri                push.c 0
   *    push.pri                -
   *    --------------------------------------
   *    addr.pri n1             push.adr n1
   *    push.pri                -
   *
   * However, PRI must not be needed after this instruction
   * if this shortcut is used. Check for the ;$par comment.
   */
  {
      "load.s.pri %1!push.pri!;$par!",
      "push.s %1!;$par!",
    seqsize(2,1) - seqsize(1,1)
  },
  {
      "load.pri %1!push.pri!;$par!",
      "push %1!;$par!",
    seqsize(2,1) - seqsize(1,1)
  },
  {
      "const.pri %1!push.pri!;$par!",
      "push.c %1!;$par!",
    seqsize(2,1) - seqsize(1,1)
  },
  {
      "zero.pri!push.pri!;$par!",
      "push.c 0!;$par!",
    seqsize(2,0) - seqsize(1,1)
  },
  {
      "addr.pri %1!push.pri!;$par!",
      "push.adr %1!;$par!",
    seqsize(2,1) - seqsize(1,1)
  },
  /* References with a default value generate new cells on the heap
   * dynamically. That code often ends with:
   *    move.pri                push.alt
   *    push.pri                -
   */
  {
      "move.pri!push.pri!",
      "push.alt!",
    seqsize(2,0) - seqsize(1,0)
  },
  /* Simple arithmetic operations on constants. Noteworthy is the
   * subtraction of a constant, since it is converted to the addition
   * of the inverse value.
   *    const.alt n1            add.c n1
   *    add                     -
   *    --------------------------------------
   *    const.alt n1            add.c -n1
   *    sub                     -
   *    --------------------------------------
   *    const.alt n1            smul.c n1
   *    smul                    -
   *    --------------------------------------
   *    const.alt n1            eq.c.pri n1
   *    eq                      -
   */
  {
      "const.alt %1!add!",
      "add.c %1!",
    seqsize(2,1) - seqsize(1,1)
  },
  {
      "const.alt %1!sub!",
      "add.c -%1!",
    seqsize(2,1) - seqsize(1,1)
  },
  {
      "const.alt %1!smul!",
      "smul.c %1!",
    seqsize(2,1) - seqsize(1,1)
  },
  {
      "const.alt %1!eq!",
      "eq.c.pri %1!",
    seqsize(2,1) - seqsize(1,1)
  },
  /* Some operations use the alternative subtraction operation --these
   * can also be optimized.
   *    const.pri n1            load.s.pri n2
   *    load.s.alt n2           add.c -n1
   *    sub.alt                 -
   *    --------------------------------------
   *    const.pri n1            load.pri n2
   *    load.alt n2             add.c -n1
   *    sub.alt                 -
   */
  {
      "const.pri %1!load.s.alt %2!sub.alt!",
      "load.s.pri %2!add.c -%1!",
    seqsize(3,2) - seqsize(2,2)
  },
  {
      "const.pri %1!load.alt %2!sub.alt!",
      "load.pri %2!add.c -%1!",
    seqsize(3,2) - seqsize(2,2)
  },
  /* With arrays indexed with constants that come from enumerations, it happens
   * multiple add.c opcodes follow in sequence.
   *    add.c n1                add.c n1+n2
   *    add.c n2                -
   */
  {
      "add.c %1!add.c %2!",
      "add.c %1+%2!",
    seqsize(2,2) - seqsize(1,1)
  },
  /* Compare and jump
   *    eq                      jneq n1
   *    jzer n1                 -
   *    --------------------------------------
   *    eq                      jeq n1
   *    jnz n1                  -
   *    --------------------------------------
   *    neq                     jeq n1
   *    jzer n1                 -
   *    --------------------------------------
   *    neq                     jneq n1
   *    jnz n1                  -
   * Compares followed by jzer occur much more
   * often than compares followed with jnz. So we
   * take the easy route here.
   *    less                    jgeq n1
   *    jzer n1                 -
   *    --------------------------------------
   *    geq                     jless n1
   *    jzer n1                 -
   *    --------------------------------------
   *    sless                   jsgeq n1
   *    jzer n1                 -
   *    --------------------------------------
   *    sleq                    jsgrtr n1
   *    jzer n1                 -
   *    --------------------------------------
   *    sgrtr                   jsleq n1
   *    jzer n1                 -
   *    --------------------------------------
   *    sgeq                    jsless n1
   *    jzer n1                 -
   */
  {
      "eq!jzer %1!",
      "jneq %1!",
    seqsize(2,1) - seqsize(1,1)
  },
  {
      "eq!jnz %1!",
      "jeq %1!",
    seqsize(2,1) - seqsize(1,1)
  },
  {
      "neq!jzer %1!",
      "jeq %1!",
    seqsize(2,1) - seqsize(1,1)
  },
  {
      "neq!jnz %1!",
      "jneq %1!",
    seqsize(2,1) - seqsize(1,1)
  },
  {
      "sless!jzer %1!",
      "jsgeq %1!",
    seqsize(2,1) - seqsize(1,1)
  },
  {
      "sleq!jzer %1!",
      "jsgrtr %1!",
    seqsize(2,1) - seqsize(1,1)
  },
  {
      "sgrtr!jzer %1!",
      "jsleq %1!",
    seqsize(2,1) - seqsize(1,1)
  },
  {
      "sgeq!jzer %1!",
      "jsless %1!",
    seqsize(2,1) - seqsize(1,1)
  },
  /* Test for zero (common case, especially for strings)
   * E.g. the test expression of: "for (i=0; str{i}!=0; ++i)"
   *
   *    zero.alt                jzer n1
   *    jeq n1                  -
   *    --------------------------------------
   *    zero.alt                jnz n1
   *    jneq n1                 -
   */
  {
      "zero.alt!jeq %1!",
      "jzer %1!",
    seqsize(2,1) - seqsize(1,1)
  },
  {
      "zero.alt!jneq %1!",
      "jnz %1!",
    seqsize(2,1) - seqsize(1,1)
  },
  /* Array comparison has a NOT instruction that can sometimes be optimized away
   *    not                     jnz n1
   *    jzer n1                 -
   *    --------------------------------------
   *    not                     jzer n1
   *    jnz n1                  -
   * And especially the code below, that essentially does a double NOT before jumping
   *    eq.c.pri 0              jzer n1
   *    not                     -
   *    jzer n1                 -
   */
  {
      "eq.c.pri 0!not!jzer %1!",
      "jzer %1!",
    seqsize(3,2) - seqsize(1,1)
  },
  {
      "not!jzer %1!",
      "jnz %1!",
    seqsize(2,1) - seqsize(1,1)
  },
  {
      "not!jnz %1!",
      "jzer %1!",
    seqsize(2,1) - seqsize(1,1)
  },
  /* Incrementing and decrementing leaves a value in
   * in PRI which may not be used (for example, as the
   * third expression in a "for" loop).
   *    inc n1                  inc n1  ; ++n
   *    load.pri n1             ;$exp
   *    ;$exp                   -
   *    --------------------------------------
   *    load.pri n1             inc n1  ; n++, e.g. "for (n=0; n<10; n++)"
   *    inc n1                  ;$exp
   *    ;$exp                   -
   * Plus the varieties for stack relative increments
   * and decrements.
   */
  {
      "inc %1!load.pri %1!;$exp!",
      "inc %1!;$exp!",
    seqsize(2,2) - seqsize(1,1)
  },
  {
      "load.pri %1!inc %1!;$exp!",
      "inc %1!;$exp!",
    seqsize(2,2) - seqsize(1,1)
  },
  {
      "inc.s %1!load.s.pri %1!;$exp!",
      "inc.s %1!;$exp!",
    seqsize(2,2) - seqsize(1,1)
  },
  {
      "load.s.pri %1!inc.s %1!;$exp!",
      "inc.s %1!;$exp!",
    seqsize(2,2) - seqsize(1,1)
  },
  {
      "dec %1!load.pri %1!;$exp!",
      "dec %1!;$exp!",
    seqsize(2,2) - seqsize(1,1)
  },
  {
      "load.pri %1!dec %1!;$exp!",
      "dec %1!;$exp!",
    seqsize(2,2) - seqsize(1,1)
  },
  {
      "dec.s %1!load.s.pri %1!;$exp!",
      "dec.s %1!;$exp!",
    seqsize(2,2) - seqsize(1,1)
  },
  {
      "load.s.pri %1!dec.s %1!;$exp!",
      "dec.s %1!;$exp!",
    seqsize(2,2) - seqsize(1,1)
  },
  /* ??? the same (increments and decrements) for references */
  /* Loading the constant zero has a special opcode.
   * When storing zero in memory, the value of PRI must not be later on.
   *    const.pri 0             zero n1
   *    stor.pri n1             ;$exp
   *    ;$exp                   -
   *    --------------------------------------
   *    const.pri 0             zero.s n1
   *    stor.s.pri n1           ;$exp
   *    ;$exp                   -
   *    --------------------------------------
   *    zero.pri                zero n1
   *    stor.pri n1             ;$exp
   *    ;$exp                   -
   *    --------------------------------------
   *    zero.pri                zero.s n1
   *    stor.s.pri n1           ;$exp
   *    ;$exp                   -
   *    --------------------------------------
   *    const.pri 0             zero.pri
   *    --------------------------------------
   *    const.alt 0             zero.alt
   * The last two alternatives save more memory than they save
   * time, but anyway...
   */
  {
      "const.pri 0!stor.pri %1!;$exp!",
      "zero %1!;$exp!",
    seqsize(2,2) - seqsize(1,1)
  },
  {
      "const.pri 0!stor.s.pri %1!;$exp!",
      "zero.s %1!;$exp!",
    seqsize(2,2) - seqsize(1,1)
  },
  {
      "zero.pri!stor.pri %1!;$exp!",
      "zero %1!;$exp!",
    seqsize(2,1) - seqsize(1,1)
  },
  {
      "zero.pri!stor.s.pri %1!;$exp!",
      "zero.s %1!;$exp!",
    seqsize(2,1) - seqsize(1,1)
  },
  {
      "const.pri 0!",
      "zero.pri!",
    seqsize(1,1) - seqsize(1,0)
  },
  {
      "const.alt 0!",
      "zero.alt!",
    seqsize(1,1) - seqsize(1,0)
  },

  /* ------------------ */
  /* Macro instructions */
  /* ------------------ */

  { "", "", 0 },    /* separator, so optimizer can stop before generating macro opcodes */

  /* optimizing the calling of native functions (which always have a parameter
   * count pushed before, and the stack pointer restored afterwards
   */
  {
      "push.c %1!sysreq.c %2!stack %3!",        //note: %3 == %1 + 4
      "sysreq.n %2 %1!",
    seqsize(3,3) - seqsize(1,2)
  },
  /* ----- */
  /* Functions with many parameters with the same "type" have sequences like:
   *    push.c n1               push3.c n1 n2 n3
   *    ;$par                   ;$par
   *    push.c n2               -
   *    ;$par                   -
   *    push.c n3               -
   *    ;$par                   -
   *    etc.                    etc.
   *
   * Similar sequences occur with PUSH, PUSH.s and PUSHADDR
   */
  {
      "push.c %1!;$par!push.c %2!;$par!push.c %3!;$par!push.c %4!;$par!push.c %5!",
      "push5.c %1 %2 %3 %4 %5!",
    seqsize(5,5) - seqsize(1,5)
  },
  {
      "push.c %1!;$par!push.c %2!;$par!push.c %3!;$par!push.c %4!",
      "push4.c %1 %2 %3 %4!",
    seqsize(4,4) - seqsize(1,4)
  },
  {
      "push.c %1!;$par!push.c %2!;$par!push.c %3!",
      "push3.c %1 %2 %3!",
    seqsize(3,3) - seqsize(1,3)
  },
  {
      "push.c %1!;$par!push.c %2!",
      "push2.c %1 %2!",
    seqsize(2,2) - seqsize(1,2)
  },
  /* ----- */
  {
      "push %1!;$par!push %2!;$par!push %3!;$par!push %4!;$par!push %5!",
      "push5 %1 %2 %3 %4 %5!",
    seqsize(5,5) - seqsize(1,5)
  },
  {
      "push %1!;$par!push %2!;$par!push %3!;$par!push %4!",
      "push4 %1 %2 %3 %4!",
    seqsize(4,4) - seqsize(1,4)
  },
  {
      "push %1!;$par!push %2!;$par!push %3!",
      "push3 %1 %2 %3!",
    seqsize(3,3) - seqsize(1,3)
  },
  {
      "push %1!;$par!push %2!",
      "push2 %1 %2!",
    seqsize(2,2) - seqsize(1,2)
  },
  /* ----- */
  {
      "push.s %1!;$par!push.s %2!;$par!push.s %3!;$par!push.s %4!;$par!push.s %5!",
      "push5.s %1 %2 %3 %4 %5!",
    seqsize(5,5) - seqsize(1,5)
  },
  {
      "push.s %1!;$par!push.s %2!;$par!push.s %3!;$par!push.s %4!",
      "push4.s %1 %2 %3 %4!",
    seqsize(4,4) - seqsize(1,4)
  },
  {
      "push.s %1!;$par!push.s %2!;$par!push.s %3!",
      "push3.s %1 %2 %3!",
    seqsize(3,3) - seqsize(1,3)
  },
  {
      "push.s %1!;$par!push.s %2!",
      "push2.s %1 %2!",
    seqsize(2,2) - seqsize(1,2)
  },
  /* ----- */
  {
      "push.adr %1!;$par!push.adr %2!;$par!push.adr %3!;$par!push.adr %4!;$par!push.adr %5!",
      "push5.adr %1 %2 %3 %4 %5!",
    seqsize(5,5) - seqsize(1,5)
  },
  {
      "push.adr %1!;$par!push.adr %2!;$par!push.adr %3!;$par!push.adr %4!",
      "push4.adr %1 %2 %3 %4!",
    seqsize(4,4) - seqsize(1,4)
  },
  {
      "push.adr %1!;$par!push.adr %2!;$par!push.adr %3!",
      "push3.adr %1 %2 %3!",
    seqsize(3,3) - seqsize(1,3)
  },
  {
      "push.adr %1!;$par!push.adr %2!",
      "push2.adr %1 %2!",
    seqsize(2,2) - seqsize(1,2)
  },
  /* Loading two registers at a time
   *    load.pri n1             load.both n1 n2
   *    load.alt n2             -
   *    --------------------------------------
   *    load.alt n2             load.both n1 n2
   *    load.pri n1             -
   *    --------------------------------------
   *    load.s.pri n1           load.s.both n1 n2
   *    load.s.alt n2           -
   *    --------------------------------------
   *    load.s.alt n2           load.s.both n1 n2
   *    load.s.pri n1           -
   */
  {
      "load.pri %1!load.alt %2!",
      "load.both %1 %2!",
    seqsize(2,2) - seqsize(1,2)
  },
  {
      "load.alt %2!load.pri %1!",
      "load.both %1 %2!",
    seqsize(2,2) - seqsize(1,2)
  },
  {
      "load.s.pri %1!load.s.alt %2!",
      "load.s.both %1 %2!",
    seqsize(2,2) - seqsize(1,2)
  },
  {
      "load.s.alt %2!load.s.pri %1!",
      "load.s.both %1 %2!",
    seqsize(2,2) - seqsize(1,2)
  },
  /* Loading two registers and then pushing them occurs with user operators
   *    load.both n1 n2         push2 n1 n2
   *    push.pri                -
   *    push.alt                -
   *    --------------------------------------
   *    load.s.both n1 n2       push2.s n1 n2
   *    push.pri                -
   *    push.alt                -
   */
  {
      "load.both %1 %2!push.pri!push.alt!",
      "push2 %1 %2!",
    seqsize(3,2) - seqsize(1,2)
  },
  {
      "load.s.both %1 %2!push.pri!push.alt!",
      "push2.s %1 %2!",
    seqsize(3,2) - seqsize(1,2)
  },
  /* Load a constant in a variable
   *    const.pri n1            const n2 n1
   *    stor.pri n2             -
   *    --------------------------------------
   *    const.pri n1            const.s n2 n1
   *    stor.s.pri n2           -
   */
  {
      "const.pri %1!stor.pri %2!;$exp!",
      "const %2 %1!;$exp!",
    seqsize(2,2) - seqsize(1,2)
  },
  {
      "const.pri %1!stor.s.pri %2!;$exp!",
      "const.s %2 %1!;$exp!",
    seqsize(2,2) - seqsize(1,2)
  },
  /* ----- */
  { NULL, NULL, 0 }
};
