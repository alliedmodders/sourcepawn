using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace smxdasm
{
    public enum SymKind : byte
    {
        Variable = 1,
        Reference = 2,
        Array = 3,
        RefArray = 4,
        Function = 9,
        VarArgs = 11,
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

    // The .dbg.natives table.
    public class SmxDebugNativesTable : SmxSection
    {
        private DebugNativeEntry[] entries_;

        public SmxDebugNativesTable(FileHeader file, SectionEntry header, SmxNameTable names)
            : base(file, header)
        {
            entries_ = DebugNativesHeader.From(file.SectionReader(header), names);
        }

        public DebugNativeEntry[] Entries
        {
            get { return entries_; }
        }
        public int Length
        {
            get { return entries_.Length; }
        }
        public DebugNativeEntry this[int index]
        {
            get { return entries_[index]; }
        }
    }

    // The .dbg.symbols table.
    public class SmxDebugSymbolsTable : SmxSection
    {
        private DebugSymbolEntry[] entries_;
        private DebugSymbolEntry[] dat_refs_;

        public SmxDebugSymbolsTable(FileHeader file, SectionEntry header, SmxDebugInfoSection info, SmxNameTable names)
            : base(file, header)
        {
            entries_ = DebugSymbolEntry.From(file, file.SectionReader(header), info, names);
        }

        private void buildDatRefs()
        {
            if (dat_refs_ != null)
                return;

            var dat_refs = new List<DebugSymbolEntry>();
            foreach (var sym in entries_)
            {
                if (sym.Scope == SymScope.Local)
                    continue;
                if (sym.Ident == SymKind.Function)
                    continue;
                dat_refs.Add(sym);
            }

            dat_refs.Sort(delegate(DebugSymbolEntry e1, DebugSymbolEntry e2)
            {
                return e1.Address.CompareTo(e2.Address);
            });
            dat_refs_ = dat_refs.ToArray();
        }

        public DebugSymbolEntry FindFunction(int address)
        {
            foreach (var entry in entries_)
            {
                if (entry.Ident != SymKind.Function)
                    continue;
                if (address >= entry.CodeStart && address <= entry.CodeEnd)
                    return entry;
            }
            return null;
        }

        public DebugSymbolEntry FindStackRef(int codeaddr, int stackaddr)
        {
            // Find symbols belonging to this address range.
            var symbols = new List<DebugSymbolEntry>();
            foreach (var sym in entries_)
            {
                if (sym.Scope != SymScope.Local)
                    continue;
                if (sym.Ident == SymKind.Function || sym.Ident == SymKind.VarArgs)
                    continue;
                if (codeaddr >= sym.CodeStart && codeaddr <= sym.CodeEnd)
                    symbols.Add(sym);
            }

            // We sort locals in reverse order, since the stack grows down.
            symbols.Sort(delegate(DebugSymbolEntry e1, DebugSymbolEntry e2)
            {
                return e2.Address.CompareTo(e1.Address);
            });

            for (var i = 0; i < symbols.Count; i++)
            {
                var sym = symbols[i];
                if (sym.Address == stackaddr)
                    return sym;

                // Ignore parameters if the offset isn't identical.
                if (sym.Address > 0)
                    continue;

                // No next symbol... just bail.
                if (i == symbols.Count - 1)
                    break;

                var next_sym = symbols[i + 1];

                // Only arrays can be accessed out of their starting address.
                if (sym.Ident != SymKind.Array)
                    continue;
                if (stackaddr > sym.Address && stackaddr < next_sym.Address)
                    return sym;
            }
            return null;
        }

        public DebugSymbolEntry FindDataRef(int address)
        {
            buildDatRefs();
            for (var i = 0; i < dat_refs_.Length; i++)
            {
                var sym = dat_refs_[i];
                if (sym.Address == address)
                    return sym;
                if (address < sym.Address)
                    break;

                // No next symbol... just bail.
                if (i == dat_refs_.Length - 1)
                    break;

                // Only arrays can be accessed out of their starting address.
                if (sym.Ident != SymKind.Array)
                    continue;

                var next_sym = dat_refs_[i + 1];
                if (address > sym.Address && address < next_sym.Address)
                    return sym;
            }
            return null;
        }

        public DebugSymbolEntry[] Entries
        {
            get { return entries_; }
        }
        public int Length
        {
            get { return entries_.Length; }
        }
        public DebugSymbolEntry this[int index]
        {
            get { return entries_[index]; }
        }
    }
}
