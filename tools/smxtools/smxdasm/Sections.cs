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

    public class SmxCalledFunctionsTable
    {
        private List<CalledFunctionEntry> functions_;
        
        public SmxCalledFunctionsTable()
        {
            functions_ = new List<CalledFunctionEntry>();
        }

        public CalledFunctionEntry[] Entries
        {
            get { return functions_.ToArray(); }
        }

        public int Length
        {
            get { return functions_.Count; }
        }
        public CalledFunctionEntry this[int index]
        {
            get { return functions_[index]; }
        }
        public void AddFunction(uint addr)
        {
            var entry = new CalledFunctionEntry();
            entry.Address = addr;
            entry.Name = String.Format("sub_{0:x}", addr);
            functions_.Add(entry);
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

    public class SmxDebugMethods : SmxRttiListTable
    {
        public SmxDebugMethods(FileHeader file, SectionEntry header, SmxNameTable names)
            : base(file, header, names)
        {
            var reader = file.SectionReader(header);
            base.init(reader);

            Entries = new DebugMethodEntry[row_count_];
            for (uint i = 0; i < row_count_; i++)
                Entries[i] = DebugMethodEntry.From(reader);
        }

        public DebugMethodEntry[] Entries { get; }
    }

    public class SmxDebugSymbols : SmxRttiListTable
    {
        public SmxDebugSymbols(FileHeader file, SectionEntry header, SmxNameTable names)
            : base(file, header, names)
        {
            var reader = file.SectionReader(header);
            base.init(reader);

            Entries = new DebugVarEntry[row_count_];
            for (uint i = 0; i < row_count_; i++)
                Entries[i] = DebugVarEntry.From(reader);
        }

        protected void EnsureSortedAddresses()
        {
            if (address_sorted_ != null)
                return;
            address_sorted_ = new List<DebugVarEntry>(Entries);
            address_sorted_.Sort(delegate (DebugVarEntry a, DebugVarEntry b)
            {
                return a.address.CompareTo(b.address);
            });
        }

        public DebugVarEntry[] Entries { get; }
        protected List<DebugVarEntry> address_sorted_;
    }

    public class SmxDebugGlobals : SmxDebugSymbols
    {
        public SmxDebugGlobals(FileHeader file, SectionEntry header, SmxNameTable names)
            : base(file, header, names)
        {
        }

        public DebugVarEntry FindGlobal(int address)
        {
            EnsureSortedAddresses();
            for (int i = 0; i < address_sorted_.Count; i++)
            {
                var sym = address_sorted_[i];
                if (sym.address == address)
                    return sym;
                if (address < sym.address)
                    break;
                if (i == address_sorted_.Count - 1)
                    break;
                var next_sym = address_sorted_[i + 1];
                if (address > sym.address && address < next_sym.address)
                    return sym;
            }
            return null;
        }
    }

    public class SmxDebugLocals : SmxDebugSymbols
    {
        private SmxFile smx_file_;

        public SmxDebugLocals(SmxFile smx_file, FileHeader file, SectionEntry header, SmxNameTable names)
            : base(file, header, names)
        {
             smx_file_ = smx_file;
        }

        public DebugVarEntry FindLocal(int codeaddr, int address)
        {
            int start_at = 0;
            int stop_at = Entries.Length;
            if (smx_file_.DebugMethods != null && smx_file_.RttiMethods != null)
            {
                int? index = null;
                for (int i = 0; i < smx_file_.DebugMethods.Entries.Length; i++)
                {
                    int method_index = smx_file_.DebugMethods.Entries[i].method_index;
                    var method = smx_file_.RttiMethods.Methods[method_index];
                    if (codeaddr >= method.pcode_start && codeaddr < method.pcode_end)
                    {
                        index = i;
                        break;
                    }
                }

                if (index != null)
                {
                    var i = (int)index;
                    start_at = smx_file_.DebugMethods.Entries[i].first_local;
                    if (i != smx_file_.DebugMethods.Entries.Length - 1)
                        stop_at = smx_file_.DebugMethods.Entries[i + 1].first_local;
                }
            }

            for (int i = start_at; i < stop_at; i++)
            {
                var sym = Entries[i];
                if (codeaddr < sym.code_start || codeaddr >= sym.code_end)
                    continue;
                if (sym.address == address)
                    return sym;
                if (i == stop_at - 1)
                    break;
                var next_sym = Entries[i + 1];
                if (address > sym.address && address < next_sym.address)
                    return sym;
            }
            return null;
        }
    }
}
