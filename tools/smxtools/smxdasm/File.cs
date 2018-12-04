using System.IO;
using System.Collections.Generic;
using System;

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
        public SmxCalledFunctionsTable CalledFunctions;
        public SmxDebugInfoSection DebugInfo;
        public SmxDebugFilesTable DebugFiles;
        public SmxDebugLinesTable DebugLines;
        public SmxDebugNativesTable DebugNatives;
        public SmxDebugSymbolsTable DebugSymbols;
        public SmxRttiData RttiData;
        public SmxRttiEnumTable RttiEnums;
        public SmxRttiEnumStructTable RttiEnumStructs;
        public SmxRttiEnumStructFieldTable RttiEnumStructFields;
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

            CalledFunctions = new SmxCalledFunctionsTable();

            // .dbg.names was removed when RTTI was added.
            if (DebugNames == null)
                DebugNames = Names;
            
            // Parse out other sections.
            var unknown = new List<SectionEntry>();
            foreach (var section in Header.Sections)
            {
                try
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
                            DebugLocals = new SmxDebugLocals(this, Header, section, Names);
                            break;
                        case "rtti.data":
                            RttiData = new SmxRttiData(this, Header, section);
                            break;
                        case "rtti.classdefs":
                            RttiClassDefs = new SmxRttiClassDefTable(Header, section, Names);
                            break;
                        case "rtti.enumstructs":
                            RttiEnumStructs = new SmxRttiEnumStructTable(Header, section, Names);
                            break;
                        case "rtti.enumstruct_fields":
                            RttiEnumStructFields = new SmxRttiEnumStructFieldTable(Header, section, Names);
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
                catch
                {
                    // Set a breakpoint here to see why the section failed to be parsed.
                    unknown.Add(section);
                }
            }
            UnknownSections = unknown.ToArray();

            // Disassemble all functions right away to find all called functions.
            if (DebugSymbols != null)
            {
                foreach (var entry in DebugSymbols.Entries)
                {
                    if (entry.Ident != SymKind.Function)
                        continue;
                    V1Disassembler.TryDisassemble(this, CodeV1, entry.Address);
                }
            }
            if (Publics != null)
            {
                foreach (var pubfun in Publics.Entries)
                {
                    V1Disassembler.TryDisassemble(this, CodeV1, (int)pubfun.Address);
                }
            }
            if (CalledFunctions != null)
            {
                foreach (var fun in CalledFunctions.Entries)
                {
                    V1Disassembler.TryDisassemble(this, CodeV1, (int)fun.Address);
                }
            }
        }

        public string FindGlobalName(int address)
        {
            if (DebugGlobals != null)
            {
                var sym = DebugGlobals.FindGlobal(address);
                if (sym != null)
                    return Names.StringAt(sym.name_offset);
            }
            if (DebugSymbols != null)
            {
                var sym = DebugSymbols.FindDataRef(address);
                if (sym != null)
                    return sym.Name;
            }
            return null;
        }

        public string FindLocalName(int codeaddr, int address)
        {
            if (DebugLocals != null)
            {
                var entry = DebugLocals.FindLocal(codeaddr, address);
                if (entry != null)
                    return Names.StringAt(entry.name_offset);
            }
            if (DebugSymbols != null)
            {
                var entry = DebugSymbols.FindStackRef(codeaddr, address);
                if (entry != null)
                    return entry.Name;
            }
            return null;
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
            if (CalledFunctions != null)
            {
                foreach (var fun in CalledFunctions.Entries)
                {
                    if (fun.Address == address)
                        return fun.Name;
                }
            }
            return "(unknown)";
        }

        public bool IsFunctionAtAddress(int address)
        {
            if (DebugSymbols != null)
            {
                if (DebugSymbols.FindFunction(address) != null)
                    return true;
            }
            if (Publics != null)
            {
                foreach (var pubfun in Publics.Entries)
                {
                    if (pubfun.Address == address)
                        return true;
                }
            }
            if (CalledFunctions != null)
            {
                foreach (var fun in CalledFunctions.Entries)
                {
                    if (fun.Address == address)
                        return true;
                }
            }
            return false;
        }
    }
}
