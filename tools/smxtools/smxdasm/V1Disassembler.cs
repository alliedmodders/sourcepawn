using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace smxdasm
{
    public enum V1Param
    {
        Constant,
        Stack,
        Jump,
        Function,
        Native,
        Address
    }

    public class V1OpcodeInfo
    {
        public V1Opcode Opcode;
        public string Name;
        public V1Param[] Params;
    }

    public class V1Instruction
    {
        public int Address;
        public V1OpcodeInfo Info;
        public int[] Params;
    }

    public class V1Disassembler
    {
        static private V1OpcodeInfo[] opcode_list_ = new V1OpcodeInfo[(int)V1Opcode.TOTAL_OPCODES];

        private static void Prep(V1Opcode op, params V1Param[] parms)
        {
            string name = op.ToString().Replace("_", ".");
            opcode_list_[(int)op] = new V1OpcodeInfo
            {
                Opcode = op,
                Name = name.ToLower(),
                Params = parms,
            };
        }

        static V1Disassembler()
        {
            Prep(V1Opcode.ADD);
            Prep(V1Opcode.ADD_C, V1Param.Constant);
            Prep(V1Opcode.ADDR_ALT, V1Param.Stack);
            Prep(V1Opcode.ADDR_PRI, V1Param.Stack);
            Prep(V1Opcode.AND);
            Prep(V1Opcode.BOUNDS, V1Param.Constant);
            Prep(V1Opcode.BREAK);
            Prep(V1Opcode.CALL, V1Param.Function);
            Prep(V1Opcode.CASETBL, V1Param.Constant, V1Param.Address);
            Prep(V1Opcode.CONST, V1Param.Address, V1Param.Constant);
            Prep(V1Opcode.CONST_ALT, V1Param.Constant);
            Prep(V1Opcode.CONST_PRI, V1Param.Constant);
            Prep(V1Opcode.CONST_S, V1Param.Stack, V1Param.Constant);
            Prep(V1Opcode.DEC, V1Param.Address);
            Prep(V1Opcode.DEC_ALT);
            Prep(V1Opcode.DEC_I);
            Prep(V1Opcode.DEC_PRI);
            Prep(V1Opcode.DEC_S, V1Param.Stack);
            Prep(V1Opcode.EQ);
            Prep(V1Opcode.EQ_C_ALT, V1Param.Constant);
            Prep(V1Opcode.EQ_C_PRI, V1Param.Constant);
            Prep(V1Opcode.FILL, V1Param.Constant);
            Prep(V1Opcode.GENARRAY, V1Param.Constant);
            Prep(V1Opcode.GENARRAY_Z, V1Param.Constant);
            Prep(V1Opcode.HALT, V1Param.Constant);
            Prep(V1Opcode.HEAP, V1Param.Constant);
            Prep(V1Opcode.IDXADDR);
            Prep(V1Opcode.IDXADDR_B, V1Param.Constant);
            Prep(V1Opcode.INC, V1Param.Address);
            Prep(V1Opcode.INC_ALT);
            Prep(V1Opcode.INC_I);
            Prep(V1Opcode.INC_PRI);
            Prep(V1Opcode.INC_S, V1Param.Stack);
            Prep(V1Opcode.INVERT);
            Prep(V1Opcode.JEQ, V1Param.Jump);
            Prep(V1Opcode.JNEQ, V1Param.Jump);
            Prep(V1Opcode.JNZ, V1Param.Jump);
            Prep(V1Opcode.JSGEQ, V1Param.Jump);
            Prep(V1Opcode.JSGRTR, V1Param.Jump);
            Prep(V1Opcode.JSLEQ, V1Param.Jump);
            Prep(V1Opcode.JSLESS, V1Param.Jump);
            Prep(V1Opcode.JUMP, V1Param.Jump);
            Prep(V1Opcode.JZER, V1Param.Jump);
            Prep(V1Opcode.LIDX);
            Prep(V1Opcode.LIDX_B, V1Param.Constant);
            Prep(V1Opcode.LOAD_ALT, V1Param.Constant);
            Prep(V1Opcode.LOAD_BOTH, V1Param.Constant, V1Param.Constant);
            Prep(V1Opcode.LOAD_I);
            Prep(V1Opcode.LOAD_PRI, V1Param.Constant);
            Prep(V1Opcode.LOAD_S_ALT, V1Param.Stack);
            Prep(V1Opcode.LOAD_S_BOTH, V1Param.Stack, V1Param.Stack);
            Prep(V1Opcode.LOAD_S_PRI, V1Param.Stack);
            Prep(V1Opcode.LODB_I, V1Param.Constant);
            Prep(V1Opcode.LREF_S_ALT, V1Param.Stack);
            Prep(V1Opcode.LREF_S_PRI, V1Param.Stack);
            Prep(V1Opcode.MOVE_ALT);
            Prep(V1Opcode.MOVE_PRI);
            Prep(V1Opcode.MOVS, V1Param.Constant);
            Prep(V1Opcode.NEG);
            Prep(V1Opcode.NEQ);
            Prep(V1Opcode.NOP);
            Prep(V1Opcode.NOT);
            Prep(V1Opcode.OR);
            Prep(V1Opcode.POP_ALT);
            Prep(V1Opcode.POP_PRI);
            Prep(V1Opcode.PROC);
            Prep(V1Opcode.PUSH_ALT);
            Prep(V1Opcode.PUSH_PRI);
            Prep(V1Opcode.PUSH, V1Param.Address);
            Prep(V1Opcode.PUSH2, V1Param.Address, V1Param.Address);
            Prep(V1Opcode.PUSH3, V1Param.Address, V1Param.Address, V1Param.Address);
            Prep(V1Opcode.PUSH4, V1Param.Address, V1Param.Address, V1Param.Address, V1Param.Address);
            Prep(V1Opcode.PUSH5, V1Param.Address, V1Param.Address, V1Param.Address, V1Param.Address, V1Param.Address);
            Prep(V1Opcode.PUSH_C, V1Param.Constant);
            Prep(V1Opcode.PUSH2_C, V1Param.Constant, V1Param.Constant);
            Prep(V1Opcode.PUSH3_C, V1Param.Constant, V1Param.Constant, V1Param.Constant);
            Prep(V1Opcode.PUSH4_C, V1Param.Constant, V1Param.Constant, V1Param.Constant, V1Param.Constant);
            Prep(V1Opcode.PUSH5_C, V1Param.Constant, V1Param.Constant, V1Param.Constant, V1Param.Constant, V1Param.Constant);
            Prep(V1Opcode.PUSH_S, V1Param.Stack);
            Prep(V1Opcode.PUSH2_S, V1Param.Stack, V1Param.Stack);
            Prep(V1Opcode.PUSH3_S, V1Param.Stack, V1Param.Stack, V1Param.Stack);
            Prep(V1Opcode.PUSH4_S, V1Param.Stack, V1Param.Stack, V1Param.Stack, V1Param.Stack);
            Prep(V1Opcode.PUSH5_S, V1Param.Stack, V1Param.Stack, V1Param.Stack, V1Param.Stack, V1Param.Stack);
            Prep(V1Opcode.PUSH_ADR, V1Param.Stack);
            Prep(V1Opcode.PUSH2_ADR, V1Param.Stack, V1Param.Stack);
            Prep(V1Opcode.PUSH3_ADR, V1Param.Stack, V1Param.Stack, V1Param.Stack);
            Prep(V1Opcode.PUSH4_ADR, V1Param.Stack, V1Param.Stack, V1Param.Stack, V1Param.Stack);
            Prep(V1Opcode.PUSH5_ADR, V1Param.Stack, V1Param.Stack, V1Param.Stack, V1Param.Stack, V1Param.Stack);
            Prep(V1Opcode.RETN);
            Prep(V1Opcode.SDIV);
            Prep(V1Opcode.SDIV_ALT);
            Prep(V1Opcode.SGEQ);
            Prep(V1Opcode.SGRTR);
            Prep(V1Opcode.SHL);
            Prep(V1Opcode.SHL_C_ALT, V1Param.Constant);
            Prep(V1Opcode.SHL_C_PRI, V1Param.Constant);
            Prep(V1Opcode.SHR);
            Prep(V1Opcode.SHR_C_ALT, V1Param.Constant);
            Prep(V1Opcode.SHR_C_PRI, V1Param.Constant);
            Prep(V1Opcode.SLEQ);
            Prep(V1Opcode.SLESS);
            Prep(V1Opcode.SMUL);
            Prep(V1Opcode.SMUL_C, V1Param.Constant);
            Prep(V1Opcode.SREF_S_ALT, V1Param.Stack);
            Prep(V1Opcode.SREF_S_PRI, V1Param.Stack);
            Prep(V1Opcode.SSHR);
            Prep(V1Opcode.STACK, V1Param.Constant);
            Prep(V1Opcode.STOR_ALT, V1Param.Constant);
            Prep(V1Opcode.STOR_I);
            Prep(V1Opcode.STOR_PRI, V1Param.Constant);
            Prep(V1Opcode.STOR_S_ALT, V1Param.Stack);
            Prep(V1Opcode.STOR_S_PRI, V1Param.Stack);
            Prep(V1Opcode.STRADJUST_PRI);
            Prep(V1Opcode.STRB_I, V1Param.Constant);
            Prep(V1Opcode.SUB);
            Prep(V1Opcode.SUB_ALT);
            Prep(V1Opcode.SWAP_ALT);
            Prep(V1Opcode.SWAP_PRI);
            Prep(V1Opcode.SWITCH, V1Param.Address);
            Prep(V1Opcode.SYSREQ_C, V1Param.Native);
            Prep(V1Opcode.SYSREQ_N, V1Param.Native, V1Param.Constant);
            Prep(V1Opcode.TRACKER_POP_SETHEAP);
            Prep(V1Opcode.TRACKER_PUSH_C, V1Param.Constant);
            Prep(V1Opcode.XCHG);
            Prep(V1Opcode.XOR);
            Prep(V1Opcode.ZERO, V1Param.Address);
            Prep(V1Opcode.ZERO_ALT);
            Prep(V1Opcode.ZERO_PRI);
            Prep(V1Opcode.ZERO_S, V1Param.Stack);
            Prep(V1Opcode.REBASE, V1Param.Address, V1Param.Constant, V1Param.Constant);
        }

        private SmxFile file_;
        private byte[] data_;
        private int code_start_;
        private int proc_offset_;
        private int cursor_;
        private int cursor_limit_;

        private int readAt(int offset)
        {
            return BitConverter.ToInt32(data_, code_start_ + offset);
        }

        private int readNext()
        {
            int value = readAt(cursor_);
            cursor_ += 4;
            return value;
        }

        private V1Opcode readNextOp()
        {
            return (V1Opcode)readNext();
        }

        private V1Disassembler(SmxFile file, SmxCodeV1Section code, int proc_offset)
        {
            file_ = file;
            data_ = file.Header.Data;
            code_start_ = code.CodeStart;
            proc_offset_ = proc_offset;
            cursor_ = proc_offset;
            cursor_limit_ = code.Header.CodeSize;
        }

        private V1Instruction[] disassemble()
        {
            if (readNextOp() != V1Opcode.PROC)
                throw new Exception("Function does not start with PROC");
            var insns = new List<V1Instruction>();
            while (cursor_ < cursor_limit_)
            {
                int address = cursor_;
                int op = readNext();
                if (op == (int)V1Opcode.PROC || op == (int)V1Opcode.ENDPROC)
                    break;

                var insn = new V1Instruction();
                insn.Address = address;
                insn.Info = opcode_list_[op];
                insns.Add(insn);

                // CASETBL is special in that it is variable-length.
                if (op == (int)V1Opcode.CASETBL)
                {
                    var ncases = readNext();
                    insn.Params = new int[(ncases + 1) * 2];
                    insn.Params[0] = ncases;
                    insn.Params[1] = readNext();
                    for (var i = 0; i < ncases; i++)
                    {
                        insn.Params[2 + i * 2] = readNext();
                        insn.Params[2 + i * 2 + 1] = readNext();
                    }
                    continue;
                }

                insn.Params = new int[insn.Info.Params.Length];
                for (var i = 0; i < insn.Info.Params.Length; i++)
                    insn.Params[i] = readNext();

                // Catch calls to unknown functions so they can be disassembled easily too.
                if (op == (int)V1Opcode.CALL)
                {
                    var addr = insn.Params[0];
                    if (!file_.IsFunctionAtAddress(addr))
                        file_.CalledFunctions.AddFunction((uint)addr);
                }
            }
            return insns.ToArray();
        }

        public static V1Instruction[] Disassemble(SmxFile file, SmxCodeV1Section code, int proc_offset)
        {
            var disassembler = new V1Disassembler(file, code, proc_offset);
            return disassembler.disassemble();
        }

        public static V1Instruction[] TryDisassemble(SmxFile file, SmxCodeV1Section code, int proc_offset)
        {
            try
            {
                return Disassemble(file, code, proc_offset);
            }
            catch
            {
                return null;
            }
        }
    }
}
