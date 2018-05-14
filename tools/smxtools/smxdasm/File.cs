using System.IO;
using System.Collections.Generic;

namespace smxdasm
{
    public class SmxFile
    {
        public FileHeader Header;
        public SectionEntry[] UnknownSections;

        public SmxNameTable Names;
        public SmxNameTable DebugNames;
        public SmxNativeTable Natives;
        public SmxPublicTable Publics;
        public SmxPubvarTable Pubvars;
        public SmxTagTable Tags;
        public SmxDataSection Data;
        public SmxCodeV1Section CodeV1;
        public SmxDebugInfoSection DebugInfo;
        public SmxDebugFilesTable DebugFiles;
        public SmxDebugLinesTable DebugLines;
        public SmxDebugNativesTable DebugNatives;
        public SmxDebugSymbolsTable DebugSymbols;
        public SmxRttiData RttiData;
        public SmxRttiEnumTable RttiEnums;
        public SmxRttiClassDefTable RttiClassDefs;
        public SmxRttiFieldTable RttiFields;
        public SmxRttiMethodTable RttiMethods;
        public SmxRttiNativeTable RttiNatives;
        public SmxRttiTypedefTable RttiTypedefs;
        public SmxRttiTypesetTable RttiTypesets;
        public SmxDebugMethods DebugMethods;
        public SmxDebugGlobals DebugGlobals;
        public SmxDebugLocals DebugLocals;

        public SmxFile(BinaryReader br)
        {
            Header = FileHeader.From(br);

            // Parse precursor sections.
            foreach (var section in Header.Sections)
            {
                if (section.Name == ".names")
                    Names = new SmxNameTable(Header, section);
                else if (section.Name == ".dbg.strings")
                    DebugNames = new SmxNameTable(Header, section);
                else if (section.Name == ".dbg.info")
                    DebugInfo = new SmxDebugInfoSection(Header, section);
            }

            // .dbg.names was removed when RTTI was added.
            if (DebugNames == null)
                DebugNames = Names;
            
            // Parse out other sections.
            var unknown = new List<SectionEntry>();
            foreach (var section in Header.Sections)
            {
                switch (section.Name)
                {
                case ".names":
                case ".dbg.strings":
                case ".dbg.info":
                    break;
                case ".natives":
                    Natives = new SmxNativeTable(Header, section, Names);
                    break;
                case ".publics":
                    Publics = new SmxPublicTable(Header, section, Names);
                    break;
                case ".pubvars":
                    Pubvars = new SmxPubvarTable(Header, section, Names);
                    break;
                case ".tags":
                    Tags = new SmxTagTable(Header, section, Names);
                    break;
                case ".data":
                    Data = new SmxDataSection(Header, section);
                    break;
                case ".code":
                    CodeV1 = new SmxCodeV1Section(Header, section);
                    break;
                case ".dbg.files":
                    DebugFiles = new SmxDebugFilesTable(Header, section, DebugNames);
                    break;
                case ".dbg.lines":
                    DebugLines = new SmxDebugLinesTable(Header, section);
                    break;
                case ".dbg.natives":
                    DebugNatives = new SmxDebugNativesTable(Header, section, DebugNames);
                    break;
                case ".dbg.symbols":
                    DebugSymbols = new SmxDebugSymbolsTable(Header, section, DebugInfo, DebugNames);
                    break;
                case ".dbg.methods":
                    DebugMethods = new SmxDebugMethods(Header, section, Names);
                    break;
                case ".dbg.globals":
                    DebugGlobals = new SmxDebugGlobals(Header, section, Names);
                    break;
                case ".dbg.locals":
                    DebugLocals = new SmxDebugLocals(Header, section, Names);
                    break;
                case "rtti.data":
                    RttiData = new SmxRttiData(this, Header, section);
                    break;
                case "rtti.classdefs":
                    RttiClassDefs = new SmxRttiClassDefTable(Header, section, Names);
                    break;
                case "rtti.fields":
                    RttiFields = new SmxRttiFieldTable(Header, section, Names);
                    break;
                case "rtti.methods":
                    RttiMethods = new SmxRttiMethodTable(Header, section, Names);
                    break;
                case "rtti.natives":
                    RttiNatives = new SmxRttiNativeTable(Header, section, Names);
                    break;
                case "rtti.enums":
                    RttiEnums = new SmxRttiEnumTable(Header, section, Names);
                    break;
                case "rtti.typedefs":
                    RttiTypedefs = new SmxRttiTypedefTable(Header, section, Names);
                    break;
                case "rtti.typesets":
                    RttiTypesets = new SmxRttiTypesetTable(Header, section, Names);
                    break;
                default:
                    unknown.Add(section);
                    break;
                }
            }
            UnknownSections = unknown.ToArray();
        }

        public string FindFunctionName(int address)
        {
            if (DebugSymbols != null)
            {
                var entry = DebugSymbols.FindFunction(address);
                if (entry != null)
                    return entry.Name;
            }
            if (Publics != null)
            {
                foreach (var pubfun in Publics.Entries)
                {
                    if (pubfun.Address == address)
                        return pubfun.Name;
                }
            }
            return "(unknown)";
        }
    }
}
