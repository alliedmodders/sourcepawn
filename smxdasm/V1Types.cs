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

        public static CodeV1Header From(BinaryReader rd)
        {
            var code = new CodeV1Header();
            code.CodeSize = rd.ReadInt32();
            code.CellSize = rd.ReadByte();
            code.CodeVersion = rd.ReadByte();
            code.Flags = (CodeV1Flags)rd.ReadUInt16();
            code.main = rd.ReadInt32();
            code.codeoffs = rd.ReadInt32();
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

    public enum SymKind : byte
    {
        Variable = 1,
        Reference = 2,
        Array = 3,
        RefArray = 4,
        Function = 9,
        VarArgs = 11,
    }

    public enum SymScope : byte
    {
        Global = 0,
        Local = 1,
        Static = 2,
    }

    // The ".dbg.symbols" table.
    public class DebugSymbolEntry
    {
        public int Address;            // Address relative to DAT or STK.
        public ushort TagId;        // Tag id (unmasked).
        public uint CodeStart;      // Live in region >= codestart.
        public uint CodeEnd;        // Live in region < codeend.
        public SymKind Ident;          // An IDENT value.
        public SymScope Scope;         // A VCLASS value.
        public ushort dimcount;     // Number of dimensions (see DebugSymbolDimEntry).
        public int nameoffs;        // Name (offset into .dbg.names).
        
        // Computed
        public string Name;
        public DebugSymbolDimEntry[] Dims;

        public static DebugSymbolEntry[] From(FileHeader hdr, BinaryReader rd, SmxDebugInfoSection info, SmxNameTable names)
        {
            var entries = new DebugSymbolEntry[info.NumSymbols];
            for (var i = 0; i < info.NumSymbols; i++)
            {
                var entry = new DebugSymbolEntry();
                entry.Address = rd.ReadInt32();
                entry.TagId = rd.ReadUInt16();
                // There's a padding of 2 bytes after this short.
                if (hdr.debugUnpacked)
                    rd.ReadBytes(2);
                entry.CodeStart = rd.ReadUInt32();
                entry.CodeEnd = rd.ReadUInt32();
                entry.Ident = (SymKind)rd.ReadByte();
                entry.Scope = (SymScope)rd.ReadByte();
                entry.dimcount = rd.ReadUInt16();
                entry.nameoffs = rd.ReadInt32();
                entry.Name = names.StringAt(entry.nameoffs);
                if (entry.dimcount > 0)
                    entry.Dims = DebugSymbolDimEntry.From(hdr, rd, entry.dimcount);
                entries[i] = entry;
            }
            return entries;
        }
    }

    // Occurs after a DebugSymbolEntry for each dimcount.
    public class DebugSymbolDimEntry
    {
        public ushort tagid;        // Tag id (unmasked).
        public int Size;           // Size of the dimension.

        public static DebugSymbolDimEntry[] From(FileHeader hdr, BinaryReader rd, int count)
        {
            var entries = new DebugSymbolDimEntry[count];
            for (var i = 0; i < count; i++)
            {
                var entry = new DebugSymbolDimEntry();
                // There's a padding of 2 bytes before this short.
                if (hdr != null && hdr.debugUnpacked)
                    rd.ReadBytes(2);
                entry.tagid = rd.ReadUInt16();
                entry.Size = rd.ReadInt32();
                entries[i] = entry;
            }
            return entries;
        }
    }

    // ".dbg.natives" section header.
    public class DebugNativesHeader
    {
        public uint num_entries;

        public static DebugNativeEntry[] From(BinaryReader rd, SmxNameTable names)
        {
            var header = new DebugNativesHeader();
            header.num_entries = rd.ReadUInt32();
            return DebugNativeEntry.From(rd, header, names);
        }
    }

    public class DebugNativeEntry
    {
        public int Index;           // Native index.
        public int nameoffs;        // Offset into .dbg.natives.
        public ushort tagid;        // Tag id (unmasked).
        public ushort nargs;        // Number of formal arguments.

        // Computed.
        public string Name;
        public DebugNativeArgEntry[] Args;

        public static DebugNativeEntry[] From(BinaryReader rd, DebugNativesHeader header, SmxNameTable names)
        {
            var entries = new DebugNativeEntry[header.num_entries];
            for (var i = 0; i < header.num_entries; i++)
            {
                var entry = new DebugNativeEntry();
                entry.Index = rd.ReadInt32();
                entry.nameoffs = rd.ReadInt32();
                entry.tagid = rd.ReadUInt16();
                entry.nargs = rd.ReadUInt16();
                entry.Name = names.StringAt(entry.nameoffs);
                if (entry.nargs > 0)
                    entry.Args = DebugNativeArgEntry.From(rd, names, entry.nargs);
                else
                    entry.Args = new DebugNativeArgEntry[0];
                entries[i] = entry;
            }
            return entries;
        }
    }

    public class DebugNativeArgEntry
    {
        public SymKind Ident;          // DebugSymbolEntry IDENT value.
        public ushort tagid;        // Tag id (unmasked).
        public ushort dimcount;     // Number of dimensions (and DebugSymbolDimEntries).
        public int nameoffs;       // Offset into .dbg.names.

        // Computed.
        public string Name;
        public DebugSymbolDimEntry[] Dims;

        public static DebugNativeArgEntry[] From(BinaryReader rd, SmxNameTable names, int count)
        {
            var entries = new DebugNativeArgEntry[count];
            for (var i = 0; i < count; i++)
            {
                var entry = new DebugNativeArgEntry();
                entry.Ident = (SymKind)rd.ReadByte();
                entry.tagid = rd.ReadUInt16();
                entry.dimcount = rd.ReadUInt16();
                entry.nameoffs = rd.ReadInt32();
                if (entry.dimcount > 0)
                    entry.Dims = DebugSymbolDimEntry.From(null, rd, entry.dimcount);
                entry.Name = names.StringAt(entry.nameoffs);
                entries[i] = entry;
            }
            return entries;
        }
    }
}
