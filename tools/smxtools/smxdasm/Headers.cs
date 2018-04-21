using System;
using System.Text;
using System.IO;
using System.IO.Compression;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace smxdasm
{
    public enum CompressionType : byte
    {
        None = 0,
        Gz = 1,
    }

    // Note: C#'s coercion rules make it very annoying to use the uint type.
    // Instead, we use |int| everywhere and check bounds where it makes sense.
    public class FileHeader
    {
        // SourcePawn File Format magic number.
        public const uint FILE_MAGIC = 0x53504646;

        // File format version number.
        //
        // The major version bits (8-15) indicate a product number. Consumers
        // should reject any version for a different product.
        //
        // The minor version bits (0-7) indicate a compatibility revision. Any
        // version higher than the current version should be rejected.
        public const ushort SP1_VERSION_1_0 = 0x0101;
        public const ushort SP1_VERSION_1_1 = 0x0102;
        public const ushort SP1_VERSION_MIN = SP1_VERSION_1_0;
        public const ushort SP1_VERSION_MAX = SP1_VERSION_1_1;

        public const byte COMPRESSION_NONE = 0;
        public const byte COMPRESSION_GZ = 1;

        // Size of the header.
        public const int Size = 24;

        // Magic number and version number.
        public uint Magic;
        public ushort Version;

        // Compression algorithm. If the file is not compressed, then imagesize and
        // disksize are the same value, and dataoffs is 0.
        //
        // The start of the compressed region is indicated by dataoffs. The length
        // of the compressed region is (disksize - dataoffs). The amount of memory
        // required to hold the decompressed bytes is (imagesize - dataoffs). The
        // compressed region should be expanded in-place. That is, bytes before
        // "dataoffs" should be retained, and the decompressed region should be
        // appended.
        //
        // |imagesize| is the amount of memory required to hold the entire container
        // in memory.
        //
        // Note: This scheme may seem odd. It's a combination of historical debt and
        // previously unspecified behavior. The original .amx file format contained
        // an on-disk structure that supported an endian-agnostic variable-length 
        // encoding of its data section, and this structure was loaded directly into
        // memory and used as the VM context. AMX Mod X later developed a container
        // format called ".amxx" as a "universal binary" for 32-bit and 64-bit
        // plugins. This format dropped compact encoding, but supported gzip. The
        // disksize/imagesize oddness made its way to this file format. When .smx
        // was created for SourceMod, it persisted even though AMX was dropped
        // entirely. So it goes.
        public CompressionType Compression;
        public int DiskSize;
        public int ImageSize;

        // Number of named file secctions.
        public uint num_sections;

        // Offset to the string table. Each string is null-terminated. The string
        // table is only used for strings related to parsing the container itself.
        // For SourcePawn, a separate ".names" section exists for Pawn-specific data.
        public int stringtab;

        // Offset to where compression begins (explained above).
        public int dataoffs;

        // The computed data buffer (which contains the header).
        public byte[] Data;
        public SectionEntry[] Sections;

        // Version 0x0101 has the debug structures padded.
        public bool debugUnpacked;

        private string string_at(int index)
        {
            int count = 0;
            for (int i = (int)stringtab + index; i < Data.Length; i++)
            {
                if (Data[i] == 0)
                    break;
                count++;
            }
            return Encoding.UTF8.GetString(Data, (int)stringtab + index, count);
        }

        public BinaryReader SectionReader(SectionEntry section)
        {
            var stream = new MemoryStream(Data, section.dataoffs, section.Size);
            return new BinaryReader(stream);
        }

        public static FileHeader From(BinaryReader rd)
        {
            var header = new FileHeader();
            var header_bytes = rd.ReadBytes(Size);
            using (var stream = new MemoryStream(header_bytes))
            using (var header_reader = new BinaryReader(stream))
            {
                header.Magic = header_reader.ReadUInt32();
                if (header.Magic != FILE_MAGIC)
                    throw new Exception("invalid file magic value");
                header.Version = header_reader.ReadUInt16();
                header.Compression = (CompressionType)header_reader.ReadByte();
                header.DiskSize = header_reader.ReadInt32();
                if (header.DiskSize < Size)
                    throw new Exception("invalid disksize");
                header.ImageSize = header_reader.ReadInt32();
                if (header.ImageSize < header.DiskSize)
                    throw new Exception("invalid imagesize");
                header.num_sections = header_reader.ReadByte();
                header.stringtab = header_reader.ReadInt32();
                if (header.stringtab < Size)
                    throw new Exception("invalid string table value");
                header.dataoffs = header_reader.ReadInt32();
                if (header.dataoffs < Size)
                    throw new Exception("invalid data offset value");
            }

            // Set up the memory buffer we'll read from.
            header.Data = new byte[header.ImageSize];
            Array.Copy(header_bytes, header.Data, Size);

            switch (header.Compression)
            {
            case CompressionType.None:
            {
                // This case is easy... we can just read the rest of the file.
                rd.Read(header.Data, Size, header.ImageSize - Size);
                break;
            }

            case CompressionType.Gz:
            {
                // Read the delta stuff in between dataoffs and here.
                rd.Read(header.Data, Size, header.dataoffs - Size);

                // Read the compressed buffer. Note when constructing the deflate
                // stream, we elide the first two header bytes since C# barfs on
                // them.
                var gzbytes = rd.ReadBytes(header.DiskSize - header.dataoffs);
                using (var gzstream = new MemoryStream(gzbytes, 2, gzbytes.Length - 2))
                using (var deflate = new DeflateStream(gzstream, CompressionMode.Decompress))
                using (var gz_reader = new BinaryReader(deflate))
                {
                    gz_reader.Read(header.Data, header.dataoffs, header.ImageSize - header.dataoffs);
                }
                break;
            }

            default:
                throw new Exception("unknown compression type");
            }

            // Swap out the reader.
            var new_stream = new MemoryStream(header.Data, Size, header.ImageSize - Size);
            rd = new BinaryReader(new_stream);

            // Read section information.
            header.Sections = new SectionEntry[header.num_sections];
            bool foundDbgNativesSection = false;
            for (var i = 0; i < header.num_sections; i++)
            {
                var entry = new SectionEntry();
                entry.nameoffs = rd.ReadInt32();
                if (entry.nameoffs < 0)
                    throw new Exception("section name offset overflow");
                entry.dataoffs = rd.ReadInt32();
                if (entry.dataoffs < Size)
                    throw new Exception("section data offset overflow");
                entry.Size = rd.ReadInt32();
                if (entry.Size < 0)
                    throw new Exception("section size overflow");
                entry.Name = header.string_at(entry.nameoffs);

                // Remember that there's a .dbg.natives section in the file. 
                if (entry.Name == ".dbg.natives")
                    foundDbgNativesSection = true;

                header.Sections[i] = entry;
            }

            // There was a brief period of incompatibility, where version == 0x0101
            // and the packing changed, at the same time .dbg.natives was introduced.
            // Once the incompatibility was noted, version was bumped to 0x0102.
            header.debugUnpacked = (header.Version == SP1_VERSION_1_0) && !foundDbgNativesSection;

            return header;
        }
    }

    public class SectionEntry
    {
        // Offset into the string table.
        public int nameoffs;

        // Offset into the file for section contents.
        public int dataoffs;
        
        // Size of this section's contents.
        public int Size;

        // Computed (not present on disk).
        public string Name;
    };
}
