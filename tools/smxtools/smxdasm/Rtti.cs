using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace smxdasm
{
    public class SmxRttiListTable : SmxSection
    {
        protected uint header_size_;
        protected uint row_size_;
        protected uint row_count_;

        public SmxRttiListTable(FileHeader file, SectionEntry header, SmxNameTable names)
            : base(file, header)
        {
            var reader = file.SectionReader(header);
        }

        protected void init(BinaryReader reader) {
            header_size_ = reader.ReadUInt32();
            row_size_ = reader.ReadUInt32();
            row_count_ = reader.ReadUInt32();
        }
    }

    public static class cb
    {
        public const byte kBool = 0x01;
        public const byte kInt32 = 0x06;
        public const byte kFloat32 = 0x0c;
        public const byte kChar8 = 0x0e;
        public const byte kAny = 0x10;
        public const byte kTopFunction = 0x11;

        public const byte kFixedArray = 0x30;
        public const byte kArray = 0x31;
        public const byte kFunction = 0x32;

        public const byte kEnum = 0x42;
        public const byte kTypedef = 0x43;
        public const byte kTypeset = 0x44;
        public const byte kStruct = 0x45;
        public const byte kEnumStruct = 0x46;

        public const byte kVoid = 0x70;
        public const byte kVariadic = 0x71;
        public const byte kByRef = 0x72;
        public const byte kConst = 0x73;

        public const byte kTypeId_Inline = 0x0;
        public const byte kTypeId_Complex = 0x1;

        public static int DecodeUint32(byte[] bytes, ref int offset) {
            uint value = 0;
            int shift = 0;
            for (; ;) {
                byte b = bytes[offset++];
                value |= (uint)(b & 0x7f) << shift;
                if ((b & 0x80) == 0)
                    break;
                shift += 7;
            }
            return (int)value;
        }
    }

    public class SmxRttiData : SmxSection
    {
        private SmxFile smx_file_;
        private byte[] bytes_;

        public SmxRttiData(SmxFile file, FileHeader file_header, SectionEntry header)
            : base(file_header, header)
        {
            smx_file_ = file;
            var reader = file_header.SectionReader(header);
            bytes_ = reader.ReadBytes(header.Size);
        }

        public string TypeFromTypeId(int type_id)
        {
            int kind = (type_id) & 0xf;
            int payload = ((type_id) >> 4) & 0xfffffff;

            if (kind == cb.kTypeId_Inline)
            {
                byte[] temp = new byte[4];
                temp[0] = (byte)(payload & 0xff);
                temp[1] = (byte)((payload >> 8) & 0xff);
                temp[2] = (byte)((payload >> 16) & 0xff);
                temp[3] = (byte)((payload >> 24) & 0xff);

                TypeBuilder tb = new TypeBuilder(smx_file_, temp, 0);
                return tb.DecodeNew();
            }
            if (kind != cb.kTypeId_Complex)
                throw new Exception("Unknown type_id kind: " + kind);
            return BuildTypename(ref payload);
        }

        public string FunctionTypeFromOffset(int offset)
        {
            TypeBuilder b = new TypeBuilder(smx_file_, bytes_, offset);
            return b.DecodeFunction();
        }

        public string[] TypesetTypesFromOffset(int offset)
        {
            int count = cb.DecodeUint32(bytes_, ref offset);
            string[] types = new string[count];

            TypeBuilder tb = new TypeBuilder(smx_file_, bytes_, offset);
            for (int i = 0; i < count; i++)
                types[i] = tb.DecodeNew();
            return types;
        }

        private string BuildTypename(ref int offset)
        {
            TypeBuilder b = new TypeBuilder(smx_file_, bytes_, offset);
            string text = b.DecodeNew();
            offset = b.offset;
            return text;
        }

        private class TypeBuilder
        {
            private SmxFile file_;
            private byte[] bytes_;
            private int offset_;
            private bool is_const_ = false;

            public TypeBuilder(SmxFile file, byte[] bytes, int offset)
            {
                file_ = file;
                bytes_ = bytes;
                offset_ = offset;
            }

            // Decode a type, but reset the |is_const| indicator for non-
            // dependent type.
            public string DecodeNew()
            {
                bool was_const = is_const_;
                is_const_ = false;

                string result = Decode();
                if (is_const_)
                    result = "const " + result;

                is_const_ = was_const;
                return result;
            }

            private string Decode()
            {
                is_const_ |= match(cb.kConst);
                byte b = bytes_[offset_++];
                switch (b)
                {
                    case cb.kBool: return "bool";
                    case cb.kInt32: return "int";
                    case cb.kFloat32: return "float";
                    case cb.kChar8: return "char";
                    case cb.kAny: return "any";
                    case cb.kTopFunction: return "Function";
                    case cb.kFixedArray:
                    {
                        var index = cb.DecodeUint32(bytes_, ref offset_);
                        string inner = Decode();
                        return inner + "[" + index + "]";
                    }
                    case cb.kArray:
                    {
                        string inner = Decode();
                        return inner + "[]";
                    }
                    case cb.kEnum:
                    {
                        var index = cb.DecodeUint32(bytes_, ref offset_);
                        return file_.RttiEnums.Enums[index];
                    }
                    case cb.kTypedef:
                    {
                        var index = cb.DecodeUint32(bytes_, ref offset_);
                        return file_.RttiTypedefs.Typedefs[index].name;
                    }
                    case cb.kTypeset:
                    {
                        var index = cb.DecodeUint32(bytes_, ref offset_);
                        return file_.RttiTypesets.Typesets[index].name;
                    }
                    case cb.kStruct:
                    {
                        var index = cb.DecodeUint32(bytes_, ref offset_);
                        return file_.RttiClassDefs.Defs[index].name;
                    }
                    case cb.kFunction:
                        return DecodeFunction();
                    case cb.kEnumStruct:
                    {
                        var index = cb.DecodeUint32(bytes_, ref offset_);
                        return file_.RttiEnumStructs.Entries[index].name;
                    }
                }
                throw new Exception("unknown type code: " + b);
            }

            public string DecodeFunction()
            {
                uint argc = (uint)bytes_[offset_++];

                bool variadic = false;
                if (bytes_[offset] == cb.kVariadic) {
                    variadic = true;
                    offset_++;
                }

                string return_type;
                if (bytes_[offset] == cb.kVoid) {
                    return_type = "void";
                    offset_++;
                } else {
                    return_type = DecodeNew();
                }

                string[] argv = new string[argc];
                for (uint i = 0; i < argc; i++)
                {
                    bool is_byref = match(cb.kByRef);
                    string text = DecodeNew();
                    if (is_byref)
                        text += "&";
                    argv[i] = text;
                }

                string signature =
                    "function " +
                    return_type + " (" +
                    String.Join(", ", argv);
                if (variadic)
                    signature += "...";
                signature += ")";
                return signature;
            }

            private bool match(byte b)
            {
                if (bytes_[offset_] != b)
                    return false;
                offset_++;
                return true;
            }

            public int offset { get { return offset_; } }
        }
    }

    public class SmxRttiEnumTable : SmxRttiListTable
    {
        public SmxRttiEnumTable(FileHeader file, SectionEntry header, SmxNameTable names)
            : base(file, header, names)
        {
            var reader = file.SectionReader(header);
            base.init(reader);

            Enums = new string[row_count_];
            for (uint i = 0; i < row_count_; i++) {
                int index = reader.ReadInt32();
                Enums[i] = names.StringAt(index);
                // reserved0-2.
                reader.ReadInt32();
                reader.ReadInt32();
                reader.ReadInt32();
            }
        }

        public string[] Enums { get; }
    }

    public struct RttiMethod
    {
        public string name;
        public int pcode_start;
        public int pcode_end;
        public int signature;
    }

    public class SmxRttiMethodTable : SmxRttiListTable
    {
        public SmxRttiMethodTable(FileHeader file, SectionEntry header, SmxNameTable names)
            : base(file, header, names)
        {
            var reader = file.SectionReader(header);
            base.init(reader);

            Methods = new RttiMethod[row_count_];
            for (uint i = 0; i < row_count_; i++) {
                int index = reader.ReadInt32();
                Methods[i].name = names.StringAt(index);
                Methods[i].pcode_start = reader.ReadInt32();
                Methods[i].pcode_end = reader.ReadInt32();
                Methods[i].signature = reader.ReadInt32();
            }
        }

        public RttiMethod[] Methods { get; }
    }

    public struct RttiNative
    {
        public string name;
        public int signature;
    }

    public class SmxRttiNativeTable : SmxRttiListTable
    {
        public SmxRttiNativeTable(FileHeader file, SectionEntry header, SmxNameTable names)
            : base(file, header, names)
        {
            var reader = file.SectionReader(header);
            base.init(reader);

            Natives = new RttiNative[row_count_];
            for (uint i = 0; i < row_count_; i++) {
                int index = reader.ReadInt32();
                Natives[i].name = names.StringAt(index);
                Natives[i].signature = reader.ReadInt32();
            }
        }

        public RttiNative[] Natives { get; }
    }

    public struct RttiTypedef
    {
        public string name;
        public int type_id;
    }
 
    public class SmxRttiTypedefTable : SmxRttiListTable
    {
        public SmxRttiTypedefTable(FileHeader file, SectionEntry header, SmxNameTable names)
            : base(file, header, names)
        {
            var reader = file.SectionReader(header);
            base.init(reader);

            Typedefs = new RttiTypedef[row_count_];
            for (uint i = 0; i < row_count_; i++) {
                int index = reader.ReadInt32();
                Typedefs[i].name = names.StringAt(index);
                Typedefs[i].type_id = reader.ReadInt32();
            }
        }

        public RttiTypedef[] Typedefs { get; }
    }

    public struct RttiTypeset
    {
        public string name;
        public int signature;
    }
 
    public class SmxRttiTypesetTable : SmxRttiListTable
    {
        public SmxRttiTypesetTable(FileHeader file, SectionEntry header, SmxNameTable names)
            : base(file, header, names)
        {
            var reader = file.SectionReader(header);
            base.init(reader);

            Typesets = new RttiTypeset[row_count_];
            for (uint i = 0; i < row_count_; i++) {
                int index = reader.ReadInt32();
                Typesets[i].name = names.StringAt(index);
                Typesets[i].signature = reader.ReadInt32();
            }
        }

        public RttiTypeset[] Typesets { get; }
    }

    public class RttiEnumStruct
    {
        public int name_offset;
        public int first_field;
        public int size;
        public string name;
    }

    public class SmxRttiEnumStructTable : SmxRttiListTable
    {
        public SmxRttiEnumStructTable(FileHeader file, SectionEntry header, SmxNameTable names)
            : base(file, header, names)
        {
            var reader = file.SectionReader(header);
            base.init(reader);

            Entries = new RttiEnumStruct[row_count_];
            for (uint i = 0; i < row_count_; i++) {
                Entries[i] = new RttiEnumStruct();
                Entries[i].name_offset = reader.ReadInt32();
                Entries[i].first_field = reader.ReadInt32();
                Entries[i].size = reader.ReadInt32();
                Entries[i].name = names.StringAt(Entries[i].name_offset);
            }
        }

        public RttiEnumStruct[] Entries { get; }
    }

    public class RttiEnumStructField
    {
        public int name_offset;
        public int type_id;
        public int offset;
        public string name;
    }

    public class SmxRttiEnumStructFieldTable : SmxRttiListTable
    {
        public SmxRttiEnumStructFieldTable(FileHeader file, SectionEntry header, SmxNameTable names)
            : base(file, header, names)
        {
            var reader = file.SectionReader(header);
            base.init(reader);

            Entries = new RttiEnumStructField[row_count_];
            for (uint i = 0; i < row_count_; i++) {
                Entries[i] = new RttiEnumStructField();
                Entries[i].name_offset = reader.ReadInt32();
                Entries[i].type_id = reader.ReadInt32();
                Entries[i].offset = reader.ReadInt32();
                Entries[i].name = names.StringAt(Entries[i].name_offset);
            }
        }

        public RttiEnumStructField[] Entries { get; }
    }

    public class RttiClassDef
    {
        public int flags;
        public int name_offset;
        public int first_field;
        public string name;
    }

    public class SmxRttiClassDefTable : SmxRttiListTable
    {
        public SmxRttiClassDefTable(FileHeader file, SectionEntry header, SmxNameTable names)
            : base(file, header, names)
        {
            var reader = file.SectionReader(header);
            base.init(reader);

            Defs = new RttiClassDef[row_count_];
            for (uint i = 0; i < row_count_; i++) {
                Defs[i] = new RttiClassDef();
                Defs[i].flags = reader.ReadInt32();
                Defs[i].name_offset = reader.ReadInt32();
                Defs[i].first_field = reader.ReadInt32();
                Defs[i].name = names.StringAt(Defs[i].name_offset);
                // reserved0-3
                reader.ReadInt32();
                reader.ReadInt32();
                reader.ReadInt32();
                reader.ReadInt32();
            }
        }

        public RttiClassDef[] Defs { get; }
    }

    public class RttiField
    {
        public short flags;
        public int name_offset;
        public int type_id;
        public string name;
    }

    public class SmxRttiFieldTable : SmxRttiListTable
    {
        public SmxRttiFieldTable(FileHeader file, SectionEntry header, SmxNameTable names)
            : base(file, header, names)
        {
            var reader = file.SectionReader(header);
            base.init(reader);

            Fields = new RttiField[row_count_];
            for (uint i = 0; i < row_count_; i++) {
                Fields[i] = new RttiField();
                Fields[i].flags = reader.ReadInt16();
                Fields[i].name_offset = reader.ReadInt32();
                Fields[i].type_id = reader.ReadInt32();
                Fields[i].name = names.StringAt(Fields[i].name_offset);
            }
        }

        public RttiField[] Fields { get; }
    }
}