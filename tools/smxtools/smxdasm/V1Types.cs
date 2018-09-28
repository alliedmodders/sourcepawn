using System;
using System.IO;

namespace smxdasm
{
    public enum CodeV1Flags : ushort
    {
        Debug = 0x00000001,
    }

    // The ".code" section.
    public class CodeV1Header
    {
        public const int Size = 16;

        public const byte VERSION_JIT1 = 9;
        public const byte VERSION_JIT2 = 10;

        // Size of the code blob.
        public int CodeSize;
        
        // Size of a cell in bytes (always 4).
        public byte CellSize;

        // Code version (see above constants).
        public byte CodeVersion;

        // Flags (see above).
        public CodeV1Flags Flags;

        // Offset within the code blob to the entry point function.
        public int main;

        // Offset to the code section.
        public int codeoffs;

        // Feature set.
        public int features;

        public static CodeV1Header From(BinaryReader rd)
        {
            var code = new CodeV1Header();
            code.CodeSize = rd.ReadInt32();
            code.CellSize = rd.ReadByte();
            code.CodeVersion = rd.ReadByte();
            code.Flags = (CodeV1Flags)rd.ReadUInt16();
            code.main = rd.ReadInt32();
            code.codeoffs = rd.ReadInt32();
            if (code.CodeVersion >= 13)
              code.features = rd.ReadInt32();
            return code;
        }
    }

    // The ".data" section.
    public class DataHeader
    {
        public const int Size = 12;

        // Size of the data blob.
        public uint DataSize;

        // Amount of memory the plugin runtime requires.
        public uint MemorySize;

        // Offset within this section to the data blob.
        public uint dataoffs;

        public static DataHeader From(BinaryReader rd)
        {
            var data = new DataHeader();
            data.DataSize = rd.ReadUInt32();
            data.MemorySize = rd.ReadUInt32();
            data.dataoffs = rd.ReadUInt32();
            return data;
        }
    }

    // The ".publics" section.
    public class PublicEntry
    {
        public const int Size = 8;

        // Offset into the code section.
        public uint Address;
        
        // Offset into the .names section.
        public int nameoffs;

        // Computed.
        public string Name;

        public static PublicEntry[] From(BinaryReader rd, SectionEntry header, SmxNameTable names)
        {
            if (header.Size % Size != 0)
                throw new Exception("invalid public table size");
            var count = header.Size / Size;
            var entries = new PublicEntry[count];
            for (var i = 0; i < count; i++)
            {
                var entry = new PublicEntry();
                entry.Address = rd.ReadUInt32();
                entry.nameoffs = rd.ReadInt32();
                entry.Name = names.StringAt(entry.nameoffs);
                entries[i] = entry;
            }
            return entries;
        }
    }

    // Called functions that weren't in the .publics or .dbg.symbols sections.
    public class CalledFunctionEntry
    {
        public uint Address;
        public string Name;
    }

    // The ".natives" section.
    public class NativeEntry
    {
        public const int Size = 4;

        // Offset into the .names section.
        public int nameoffs;

        // Computed name.
        public string Name;

        public static NativeEntry[] From(BinaryReader rd, SectionEntry header, SmxNameTable names)
        {
            if (header.Size % Size != 0)
                throw new Exception("invalid native table size");
            var count = header.Size / Size;
            var entries = new NativeEntry[count];
            for (var i = 0; i < count; i++)
            {
                var entry = new NativeEntry();
                entry.nameoffs = rd.ReadInt32();
                entry.Name = names.StringAt(entry.nameoffs);
                entries[i] = entry;
            }
            return entries;
        }
    }

    // The ".pubvars" section.
    public class PubvarEntry
    {
        public const int Size = 8;

        // Offset into the data section.
        public uint Address;

        // Offset into the .names section.
        public int nameoffs;

        // Computed.
        public string Name;

        public static PubvarEntry[] From(BinaryReader rd, SectionEntry header, SmxNameTable names)
        {
            if (header.Size % Size != 0)
                throw new Exception("invalid pubvar table size");
            var count = header.Size / Size;
            var entries = new PubvarEntry[count];
            for (var i = 0; i < count; i++)
            {
                var entry = new PubvarEntry();
                entry.Address = rd.ReadUInt32();
                entry.nameoffs = rd.ReadInt32();
                entry.Name = names.StringAt(entry.nameoffs);
                entries[i] = entry;
            }
            return entries;
        }
    }

    // The ".tags" section.
    public class TagEntry
    {
        public const int Size = 8;

        // Various tags that can be on a tag id.
        public const uint FIXED = 0x40000000;
        public const uint FUNC = 0x20000000;
        public const uint OBJECT = 0x10000000;
        public const uint ENUM = 0x08000000;
        public const uint METHODMAP = 0x04000000;
        public const uint STRUCT = 0x02000000;
        public const uint FLAGMASK = (FIXED|FUNC|OBJECT|ENUM|METHODMAP|STRUCT);

        // Tag ID from the compiler.
        public uint tag;

        // Offset into the .names.
        public int nameoffs;

        // Computed
        public string name;

        public static TagEntry[] From(BinaryReader rd, SectionEntry header, SmxNameTable names)
        {
            if (header.Size % Size != 0)
                throw new Exception("invalid tag table size");
            var count = header.Size / Size;
            var entries = new TagEntry[count];
            for (var i = 0; i < count; i++)
            {
                var entry = new TagEntry();
                entry.tag = rd.ReadUInt32();
                entry.nameoffs = rd.ReadInt32();
                entry.name = names.StringAt(entry.nameoffs);
                entries[i] = entry;
            }
            return entries;
        }
    }

    // The ".dbg.info" section.
    public class DebugInfoHeader
    {
        public int num_files;
        public int num_lines;
        public int num_syms;
        public int num_arrays;

        public static DebugInfoHeader From(BinaryReader rd)
        {
            var info = new DebugInfoHeader();
            info.num_files = rd.ReadInt32();
            info.num_lines = rd.ReadInt32();
            info.num_syms = rd.ReadInt32();
            info.num_arrays = rd.ReadInt32();
            return info;
        }
    }

    // The ".dbg.files" section.
    public class DebugFileEntry
    {
        public static int Size = 8;

        // Address into code.
        public uint Address;

        // Offset into .dbg.names.
        public int nameoffs;

        // Computed.
        public string Name;

        public static DebugFileEntry[] From(BinaryReader rd, SectionEntry header, SmxNameTable names)
        {
            if (header.Size % Size != 0)
                throw new Exception("invalid debug file table size");
            var count = header.Size / Size;
            var entries = new DebugFileEntry[count];
            for (var i = 0; i < count; i++)
            {
                var entry = new DebugFileEntry();
                entry.Address = rd.ReadUInt32();
                entry.nameoffs = rd.ReadInt32();
                entry.Name = names.StringAt(entry.nameoffs);
                entries[i] = entry;
            }
            return entries;
        }
    }

    // The ".dbg.lines" section.
    public class DebugLineEntry
    {
        public static int Size = 8;

        // Address into code.
        public uint Address;

        // Line number.
        public uint Line;

        public static DebugLineEntry[] From(BinaryReader rd, SectionEntry header)
        {
            if (header.Size % Size != 0)
                throw new Exception("invalid debug line table size");
            var count = header.Size / Size;
            var entries = new DebugLineEntry[count];
            for (var i = 0; i < count; i++)
            {
                var entry = new DebugLineEntry();
                entry.Address = rd.ReadUInt32();
                entry.Line = rd.ReadUInt32();
                entries[i] = entry;
            }
            return entries;
        }
    }

    public enum SymScope : byte
    {
        Global = 0,
        Local = 1,
        Static = 2,
        Arg = 3
    }

    // The ".dbg.methods" section.
    public class DebugMethodEntry
    {
        public int method_index;
        public int first_local;

        public static DebugMethodEntry From(BinaryReader rd)
        {
            DebugMethodEntry entry = new DebugMethodEntry();
            entry.method_index = rd.ReadInt32();
            entry.first_local = rd.ReadInt32();
            return entry;
        }
    }

    // The ".dbg.globals"  and ".dbg.locals" section.
    public class DebugVarEntry
    {
        public int address;
        public SymScope scope;
        public int name_offset;
        public int code_start;
        public int code_end;
        public int type_id;

        public static DebugVarEntry From(BinaryReader rd)
        {
            DebugVarEntry entry = new DebugVarEntry();
            entry.address = rd.ReadInt32();
            entry.scope = (SymScope)(rd.ReadByte() & 3);
            entry.name_offset = rd.ReadInt32();
            entry.code_start = rd.ReadInt32();
            entry.code_end = rd.ReadInt32();
            entry.type_id = rd.ReadInt32();
            return entry;
        }
    }
}
