using System;
using System.Collections.Generic;
using System.Linq;
using System.IO;
using System.Text;
using System.Threading.Tasks;

namespace smxdasm
{
    public class SmxSection
    {
        public SmxSection(FileHeader file, SectionEntry header)
        {
            file_ = file;
            header_ = header;
        }

        public SectionEntry SectionHeader
        {
            get { return header_; }
        }

        public MemoryStream GetReader()
        {
            return new MemoryStream(file_.Data, header_.dataoffs, header_.Size);
        }

        protected FileHeader file_;
        protected SectionEntry header_;
    }

    // The following tables conform to a nametable:
    //   .names
    //   .dbg.names
    public class SmxNameTable : SmxSection
    {
        private Dictionary<int, string> names_;
        private int[] extents_;

        public SmxNameTable(FileHeader header, SectionEntry section)
            : base(header, section)
        {
            names_ = new Dictionary<int, string>();
        }

        private void compute_extents()
        {
            var extents = new List<int>();
            int last_index = 0;
            for (int i = 0; i < header_.Size; i++)
            {
                if (file_.Data[header_.dataoffs + i] == 0)
                {
                    extents.Add(last_index);
                    last_index = i + 1;
                }
            }

            extents_ = extents.ToArray();
        }

        // Returns a list of all root indexes that map to strings.
        public int[] Extents
        {
            get
            {
                if (extents_ == null)
                    compute_extents();
                return extents_;
            }
        }

        // Returns a string at a given index.
        public string StringAt(int index)
        {
            if (names_.ContainsKey(index))
                return names_[index];

            if (index >= header_.Size)
                throw new Exception("invalid string index");
            int length = 0;
            for (int i = index; i < header_.Size; i++)
            {
                if (file_.Data[header_.dataoffs + i] == 0)
                    break;
                length++;
            }

            var value = Encoding.UTF8.GetString(file_.Data, header_.dataoffs + index, length);
            names_.Add(index, value);
            return value;
        }
    }

    // The .natives table.
    public class SmxNativeTable : SmxSection
    {
        private NativeEntry[] natives_;

        public SmxNativeTable(FileHeader file, SectionEntry header, SmxNameTable names)
            : base(file, header)
        {
            natives_ = NativeEntry.From(file.SectionReader(header), header, names);
        }

        public NativeEntry[] Entries
        {
            get { return natives_; }
        }
        public int Length
        {
            get { return natives_.Length; }
        }
        public NativeEntry this[int index]
        {
            get { return natives_[index]; }
        }
    }

    // The .publics table.
    public class SmxPublicTable : SmxSection
    {
        private PublicEntry[] publics_;

        public SmxPublicTable(FileHeader file, SectionEntry header, SmxNameTable names)
            : base(file, header)
        {
            publics_ = PublicEntry.From(file.SectionReader(header), header, names);
        }

        public PublicEntry[] Entries
        {
            get { return publics_; }
        }
        public int Length
        {
            get { return publics_.Length; }
        }
        public PublicEntry this[int index]
        {
            get { return publics_[index]; }
        }
    }

    // The .pubvars table.
    public class SmxPubvarTable : SmxSection
    {
        private PubvarEntry[] pubvars_;

        public SmxPubvarTable(FileHeader file, SectionEntry header, SmxNameTable names)
            : base(file, header)
        {
            pubvars_ = PubvarEntry.From(file.SectionReader(header), header, names);
        }

        public PubvarEntry[] Entries
        {
            get { return pubvars_; }
        }
        public int Length
        {
            get { return pubvars_.Length; }
        }
        public PubvarEntry this[int index]
        {
            get { return pubvars_[index]; }
        }
    }

    [Flags]
    public enum TagFlags
    {
        Fixed     = 0x40000000,
        Func      = 0x20000000,
        Object    = 0x10000000,
        Enum      = 0x08000000,
        Methodmap = 0x04000000,
        Struct    = 0x02000000,
    }

    public class Tag
    {
        private TagEntry entry_;

        public Tag(TagEntry entry)
        {
            entry_ = entry;
        }

        public uint Id
        {
            get { return entry_.tag & ~TagEntry.FLAGMASK; }
        }
        public uint Value
        {
            get { return entry_.tag; }
        }
        public TagFlags Flags
        {
            get { return (TagFlags)(entry_.tag & TagEntry.FLAGMASK); }
        }
        public string Name
        {
            get { return entry_.name; }
        }
        public TagEntry entry
        {
            get { return entry_; }
        }
    }

    // The .tags table.
    public class SmxTagTable : SmxSection
    {
        private Tag[] tags_;
        private Dictionary<ushort, Tag> cache_ = new Dictionary<ushort, Tag>();

        public SmxTagTable(FileHeader file, SectionEntry header, SmxNameTable names)
            : base(file, header)
        {
            var tags = TagEntry.From(file.SectionReader(header), header, names);
            tags_ = new Tag[tags.Length];
            for (var i = 0; i < tags.Length; i++)
                tags_[i] = new Tag(tags[i]);
        }

        public Tag FindTag(ushort tag)
        {
            if (cache_.ContainsKey(tag))
                return cache_[tag];

            Tag found = null;
            for (var i = 0; i < tags_.Length; i++)
            {
                if (tags_[i].Id == tag)
                {
                    found = tags_[i];
                    break;
                }
            }

            cache_[tag] = found;
            return found;
        }

        public Tag[] Entries
        {
            get { return tags_; }
        }
        public int Length
        {
            get { return tags_.Length; }
        }
        public Tag this[int index]
        {
            get { return tags_[index]; }
        }
    }

    // The .data section.
    public class SmxDataSection : SmxSection
    {
        private DataHeader dh_;

        public SmxDataSection(FileHeader file, SectionEntry header)
            : base(file, header)
        {
            dh_ = DataHeader.From(file.SectionReader(header));
        }

        // Creates a new memory stream for the data buffer.
        public MemoryStream Memory()
        {
            return new MemoryStream(
                file_.Data,
                (int)(header_.dataoffs + dh_.dataoffs),
                (int)dh_.DataSize
            );
        }
        
        // Creates a new binary reader for the data buffer. The data buffer
        // should be read as a series of cells (32-bit signed integers).
        public BinaryReader Reader()
        {
            return new BinaryReader(Memory());
        }

        public DataHeader Header
        {
            get { return dh_; }
        }
    }

    // The .code section.
    public class SmxCodeV1Section : SmxSection
    {
        private CodeV1Header ch_;

        public SmxCodeV1Section(FileHeader file, SectionEntry header)
            : base(file, header)
        {
            ch_ = CodeV1Header.From(file.SectionReader(header));
        }

        // Creates a new memory stream for the data buffer.
        public MemoryStream Memory()
        {
            return new MemoryStream(
                file_.Data,
                (int)(header_.dataoffs + ch_.codeoffs),
                (int)ch_.CodeSize
            );
        }

        // Compute an absolute offset to where the code stream begins.
        public int CodeStart
        {
            get { return SectionHeader.dataoffs + Header.codeoffs; }
        }

        // Creates a new binary reader for the data buffer. The data buffer
        // should be read as a series of cells (32-bit signed integers).
        public BinaryReader Reader()
        {
            return new BinaryReader(Memory());
        }

        public CodeV1Header Header
        {
            get { return ch_; }
        }
    }

    // The .dbg.info section.
    public class SmxDebugInfoSection : SmxSection
    {
        private DebugInfoHeader info_;

        public SmxDebugInfoSection(FileHeader file, SectionEntry header)
            : base(file, header)
        {
            info_ = DebugInfoHeader.From(file.SectionReader(header));
        }

        public int NumFiles
        {
            get { return info_.num_files; }
        }
        public int NumLines
        {
            get { return info_.num_lines; }
        }
        public int NumSymbols
        {
            get { return info_.num_syms; }
        }
        public int NumArrays
        {
            get { return info_.num_arrays; }
        }
    }

    // The .dbg.files table.
    public class SmxDebugFilesTable : SmxSection
    {
        private DebugFileEntry[] entries_;

        public SmxDebugFilesTable(FileHeader file, SectionEntry header, SmxNameTable names)
            : base(file, header)
        {
            entries_ = DebugFileEntry.From(file.SectionReader(header), header, names);
        }


        public string FindFile(uint addr)
        {
            int high = entries_.Length;
            int low = -1;

            while (high - low > 1)
            {
                int mid = (low + high) / 2;
                if (entries_[mid].Address <= addr)
                    low = mid;
                else
                    high = mid;
            }

            if (low == -1)
                return null;
            return entries_[low].Name;
        }

        public DebugFileEntry[] Entries
        {
            get { return entries_; }
        }
        public int Length
        {
            get { return entries_.Length; }
        }
        public DebugFileEntry this[int index]
        {
            get { return entries_[index]; }
        }
    }

    // The .dbg.lines table.
    public class SmxDebugLinesTable : SmxSection
    {
        private DebugLineEntry[] entries_;

        public SmxDebugLinesTable(FileHeader file, SectionEntry header)
            : base(file, header)
        {
            entries_ = DebugLineEntry.From(file.SectionReader(header), header);
        }

        public uint? FindLine(uint addr)
        {
            int high = entries_.Length;
            int low = -1;

            while (high - low > 1)
            {
                int mid = (low + high) / 2;
                if (entries_[mid].Address <= addr)
                    low = mid;
                else
                    high = mid;
            }

            if (low == -1)
                return null;

            // "Since the CIP occurs BEFORE the line, we have to add one"
            return entries_[low].Line + 1;
        }

        public DebugLineEntry[] Entries
        {
            get { return entries_; }
        }
        public int Length
        {
            get { return entries_.Length; }
        }
        public DebugLineEntry this[int index]
        {
            get { return entries_[index]; }
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
