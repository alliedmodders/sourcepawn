// vim: set sts=4 ts=4 sw=4 tw=99 et:
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.IO;
using smxdasm;

namespace smxviewer
{
    public partial class MainWindow : Form
    {
        private SmxFile file_;
        private StringBuilder detail_buffer_ = new StringBuilder();

        public MainWindow()
        {
            InitializeComponent();

            // Setup file drag and drop event listeners
            this.AllowDrop = true;
            this.DragEnter += new DragEventHandler(MainWindow_DragEnter);
            this.DragDrop += new DragEventHandler(MainWindow_DragDrop);

            string[] args = Environment.GetCommandLineArgs();
            List<string> files = args.ToList();

            // First argument is the executable path itself
            if (files.Count <= 1)
                return;

            if (files.Count > 2)
            {
                MessageBox.Show("Can't open multiple files at once.");
                return;
            }

            Stream stream = null;
            try
            {
                stream = File.OpenRead(files[1]);
            }
            catch (Exception ex)
            {
                MessageBox.Show("Could not open file: " + ex.Message, "File error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                return;
            }

            openFile(stream);
        }

        private void MainWindow_Load(object sender, EventArgs e)
        {
        }

        private void exitToolStripMenuItem_Click(object sender, EventArgs e)
        {
            Application.Exit();
        }

        private void openToolStripMenuItem_Click(object sender, EventArgs e)
        {
            var dialog = new OpenFileDialog();
            dialog.Filter = "smx files (*.smx)|*.smx|All files (*.*)|*.*";
            dialog.FilterIndex = 1;
            dialog.RestoreDirectory = true;

            if (dialog.ShowDialog() != DialogResult.OK)
                return;

            Stream stream = null;
            try
            {
                stream = dialog.OpenFile();
            }
            catch (Exception ex)
            {
                MessageBox.Show("Could not open file: " + ex.Message, "File error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                return;
            }

            openFile(stream);
        }

        private void openFile(Stream stream)
        {
            try
            {
                using (stream)
                using (var reader = new BinaryReader(stream))
                {
                    file_ = new SmxFile(reader);
                }
            }
            catch (Exception ex)
            {
                MessageBox.Show("Could not parse file: " + ex.Message);
                return;
            }

            renderFile();
        }

        private void MainWindow_DragEnter(object sender, DragEventArgs e)
        {
            try
            {
                if (e.Data.GetDataPresent(DataFormats.FileDrop))
                {
                    e.Effect = DragDropEffects.Link;
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine(ex.ToString());
            }
        }

        private void MainWindow_DragDrop(object sender, DragEventArgs e)
        {
            try
            {
                string[] dropped = (string[])e.Data.GetData(DataFormats.FileDrop);
                List<string> files = dropped.ToList();

                if (!files.Any())
                    return;

                if (files.Count > 1)
                {
                    MessageBox.Show("Can't open multiple files at once.");
                    return;
                }

                
                Stream stream = null;
                try
                {
                    stream = File.OpenRead(files[0]);
                }
                catch (Exception ex)
                {
                    MessageBox.Show("Could not open file: " + ex.Message, "File error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                    return;
                }

                openFile(stream);
            }
            catch (Exception ex)
            {
                Console.WriteLine(ex.ToString());
            }
        }

        private void renderFile()
        {
            var roots = new Dictionary<string, TreeNode>();
            treeview_.BeginUpdate();
            treeview_.Nodes.Clear();
            var toproot = treeview_.Nodes.Add("(header)");
            toproot.Tag = new NodeData(renderFileDetail, null);

            // Add section headers.
            foreach (var section_ in file_.Header.Sections)
            {
                var section = section_;
                var root = new TreeNode(section.Name);
                root.Tag = new NodeData(delegate() {
                    renderSectionHeaderDetail(section);
                    endDetailUpdate();
                }, section);

                roots[section.Name] = root;
                treeview_.Nodes.Add(root);
            }

            // Add specific sections.
            if (roots.ContainsKey(".natives"))
                renderNativeList(roots[".natives"], file_.Natives);
            if (roots.ContainsKey(".tags"))
                renderTagList(roots[".tags"], file_.Tags);
            if (roots.ContainsKey(".pubvars"))
                renderPubvarList(roots[".pubvars"], file_.Pubvars);
            if (roots.ContainsKey(".publics"))
                renderPublicsList(roots[".publics"], file_.Publics);
            if (roots.ContainsKey(".code"))
                renderCodeSection(roots[".code"], file_.CodeV1);
            if (roots.ContainsKey(".data"))
                renderDataList(roots[".data"], file_.Data);
            if (roots.ContainsKey(".names"))
                renderNamesList(roots[".names"], file_.Names);
            if (roots.ContainsKey(".dbg.files"))
                renderDebugFiles(roots[".dbg.files"], file_.DebugFiles);
            if (roots.ContainsKey(".dbg.lines"))
                renderDebugLines(roots[".dbg.lines"], file_.DebugLines);
            if (roots.ContainsKey(".dbg.info"))
                renderDebugInfo(roots[".dbg.info"], file_.DebugInfo);
            if (roots.ContainsKey(".dbg.strings"))
                renderNamesList(roots[".dbg.strings"], file_.DebugNames);
            if (roots.ContainsKey(".dbg.symbols"))
                renderDebugSymbols(roots[".dbg.symbols"], file_.DebugSymbols);
            if (roots.ContainsKey(".dbg.natives"))
                renderDebugNatives(roots[".dbg.natives"], file_.DebugNatives);

            treeview_.EndUpdate();

            renderFileDetail();
        }

        private void startDetailUpdate()
        {
            detail_buffer_.Clear();
        }

        private void startDetail(string fmt, params object[] args)
        {
            startDetailUpdate();
            addDetailLine(fmt, args);
        }

        private void addDetailLine(string fmt, params object[] args)
        {
            detail_buffer_.Append(String.Format(fmt, args) + "\r\n");
        }

        private void endDetailUpdate()
        {
            detailbox_.Text = detail_buffer_.ToString();
        }

        private void renderFileDetail()
        {
            startDetailUpdate();
            addDetailLine("magic = 0x{0:x}", file_.Header.Magic);
            addDetailLine("version = 0x{0:x}", file_.Header.Version);
            addDetailLine("compression = {0} (0x{1:x})", file_.Header.Compression.ToString(), file_.Header.Compression);
            addDetailLine("disksize = {0} bytes", file_.Header.DiskSize);
            addDetailLine("imagesize = {0} bytes", file_.Header.ImageSize);
            addDetailLine("sections = {0}", file_.Header.num_sections);
            addDetailLine("stringtab = @{0}", file_.Header.stringtab);
            addDetailLine("dataoffs = @{0}", file_.Header.dataoffs);
            endDetailUpdate();
        }

        private void renderSectionHeaderDetail(SectionEntry header)
        {
            startDetailUpdate();
            addDetailLine(".nameoffs = 0x{0:x} ; \"{1}\"", header.nameoffs, header.Name);
            addDetailLine(".dataoffs = 0x{0:x}", header.dataoffs);
            addDetailLine(".size = {0} bytes", header.Size);
        }

        private void renderByteView(BinaryReader reader, int size)
        {
            var ndigits = string.Format("{0:x}", size).Length;
            var addrfmt = "0x{0:x" + ndigits + "}: ";

            var chars = new StringBuilder();

            startDetailUpdate();
            for (int i = 0; i < size; i++)
            {
                if (i % 16 == 0)
                {
                    if (i != 0)
                    {
                        detail_buffer_.Append("  ");
                        detail_buffer_.Append(chars);
                        detail_buffer_.Append("\r\n");
                        chars.Clear();
                    }
                    detail_buffer_.Append(string.Format(addrfmt, i));
                }
                else if (i % 8 == 0)
                {
                    detail_buffer_.Append(" ");
                    chars.Append(" ");
                }

                var value = reader.ReadByte();
                detail_buffer_.Append(string.Format("{0:x2} ", value));

                if (value >= 0x21 && value <= 0x7f)
                    chars.Append(Convert.ToChar(value));
                else
                    chars.Append(".");
            }
            detail_buffer_.Append("  ");
            detail_buffer_.Append(chars);
            detail_buffer_.Append("\r\n");

            endDetailUpdate();
        }

        private void renderHexView(BinaryReader reader, int size)
        {
            var ndigits = string.Format("{0:x}", size).Length;
            var addrfmt = "0x{0:x" + ndigits + "}: ";

            startDetailUpdate();
            for (int i = 0; i < size; i += 4)
            {
                if (i % 32 == 0)
                {
                    if (i != 0)
                    {
                        detail_buffer_.Append("  ");
                        detail_buffer_.Append("\r\n");
                    }
                    detail_buffer_.Append(string.Format(addrfmt, i));
                }
                else if (i % 16 == 0)
                {
                    detail_buffer_.Append(" ");
                }

                var value = reader.ReadInt32();
                detail_buffer_.Append(string.Format("{0:x8} ", value));
            }
            endDetailUpdate();
        }

        private void renderStringAnalysis(MemoryStream stream, BinaryReader reader, int size)
        {
            startDetailUpdate();

            var current = new StringBuilder();
            for (var i = 0; i < size; i++)
            {
                byte b = reader.ReadByte();
                if (b == 0 && current.Length > 0)
                {
                    addDetailLine("0x{0:x6}: {1}", i, current.ToString());
                    current.Clear();
                }

                if (b < 0x20 || b > 0x7f)
                {
                    current.Clear();
                    continue;
                }

                current.Append(Convert.ToChar(b));
            }
            endDetailUpdate();
        }

        private void renderCodeView(SmxCodeV1Section code, string name, int address)
        {
            startDetailUpdate();

            V1Instruction[] insns;
            try
            {
                insns = V1Disassembler.Disassemble(file_, code, address);
            }
            catch (Exception e)
            {
               addDetailLine("Could not disassemble method {0}: {1}", name, e.Message);
               endDetailUpdate();
               return;
            }

            addDetailLine("; {0}", name);
            addDetailLine("; {0} instruction(s)", insns.Length);
            addDetailLine("; starts at code address 0x{0:x}", address);
            addDetailLine("---");

            if (insns.Length == 0)
            {
                endDetailUpdate();
                return;
            }


            // Find the largest address so we can get consistent column length.
            int last_address = insns[insns.Length - 1].Address;
            var ndigits = string.Format("{0:x}", last_address).Length;
            var addrfmt = "0x{0:x" + ndigits + "}: ";

            var buffer = new StringBuilder();
            var comment = new StringBuilder();
            foreach (var insn in insns)
            {
                buffer.Clear();
                comment.Clear();
                buffer.Append(insn.Info.Name);

                for (var i = 0; i < insn.Params.Length; i++)
                {
                    if (i >= insn.Info.Params.Length)
                        break;
                    var kind = insn.Info.Params[i];
                    var value = insn.Params[i];

                    switch (kind)
                    {
                        case V1Param.Constant:
                        case V1Param.CaseTable:
                            buffer.Append(string.Format(" 0x{0:x}", value));
                            comment.Append(string.Format(" {0}", value));
                            break;
                        case V1Param.Native:
                            buffer.Append(string.Format(" {0}", value));
                            if (file_.Natives != null && value < file_.Natives.Length)
                                comment.Append(string.Format(" {0}", file_.Natives[value].Name));
                            break;
                        case V1Param.Jump:
                            int delta = value - insn.Address;
                            buffer.Append(string.Format(" 0x{0:x}", value));
                            if (delta >= 0)
                                comment.Append(string.Format(" +0x{0:x}", delta));
                            else
                                comment.Append(string.Format(" -0x{0:x}", -delta));
                            break;
                        case V1Param.Address:
                        {
                            DebugSymbolEntry sym = null;
                            if (file_.DebugSymbols != null)
                                sym = file_.DebugSymbols.FindDataRef(value);
                            buffer.Append(string.Format(" 0x{0:x}", value));
                            if (sym != null)
                                comment.Append(string.Format(" {0}", sym.Name));
                            else
                                comment.Append(string.Format(" {0}", value));
                            break;
                        }
                        case V1Param.Stack:
                        {
                            DebugSymbolEntry sym = null;
                            if (file_.DebugSymbols != null)
                                sym = file_.DebugSymbols.FindStackRef(insn.Address, value);
                            buffer.Append(string.Format(" 0x{0:x}", value));
                            if (sym != null)
                                comment.Append(string.Format(" {0}", sym.Name));
                            else
                                comment.Append(string.Format(" {0}", value));
                            break;
                        }
                        case V1Param.Function:
                            string fun = file_.FindFunctionName(value);
                            buffer.Append(string.Format(" 0x{0:x}", value));
                            comment.Append(string.Format(" {0}", fun));
                            break;
                    }
                }

                detail_buffer_.Append(string.Format(addrfmt, insn.Address));
                detail_buffer_.Append(string.Format("{0,-32}", buffer));
                if (comment.Length > 0)
                {
                    detail_buffer_.Append(string.Format(" ;{1}", buffer, comment));
                }
                detail_buffer_.Append("\r\n");
            }

            endDetailUpdate();
        }

        private void renderCodeSection(TreeNode root, SmxCodeV1Section code)
        {
            root.Tag = new NodeData(delegate()
            {
                renderSectionHeaderDetail(code.SectionHeader);
                addDetailLine("codesize = {0} bytes", code.Header.CodeSize);
                addDetailLine("cellsize = {0} bytes", code.Header.CellSize);
                addDetailLine("codeversion = 0x{0:x}", code.Header.CodeVersion);
                addDetailLine("flags = 0x{0:x} ; {0}", code.Header.Flags, code.Header.Flags.ToString());
                addDetailLine("main = 0x{0:x}", code.Header.main);
                addDetailLine("codeoffs = 0x{0:x}", code.Header.codeoffs);
                endDetailUpdate();
            }, code);

            root.Nodes.Add("cell view").Tag = new NodeData(delegate()
            {
                renderHexView(code.Reader(), (int)code.Header.CodeSize);
            }, null);

            var functionMap = new Dictionary<string, uint>();

            if (file_.Publics != null)
            {
                foreach (var pubfun in file_.Publics.Entries)
                {
                    functionMap[pubfun.Name] = pubfun.Address;
                }
            }
            if (file_.DebugSymbols != null)
            {
                foreach (var sym in file_.DebugSymbols.Entries)
                {
                    if (sym.Ident != SymKind.Function)
                        continue;
                    functionMap[sym.Name] = sym.CodeStart;
                }
            }

            foreach (var pair in functionMap)
            {
                var name = pair.Key;
                var address = functionMap[pair.Key];
                root.Nodes.Add(pair.Key).Tag = new NodeData(delegate()
                {
                    renderCodeView(code, name, (int)address);
                }, null);
            }
        }

        private void renderDataList(TreeNode root, SmxDataSection data)
        {
            root.Tag = new NodeData(delegate()
            {
                renderSectionHeaderDetail(data.SectionHeader);
                addDetailLine("datasize = {0} bytes", data.Header.DataSize);
                addDetailLine("memory = {0} bytes", data.Header.MemorySize);
                addDetailLine("dataoffs = 0x{0:x}", data.Header.dataoffs);
                endDetailUpdate();
            }, data);

            root.Nodes.Add("byte view").Tag = new NodeData(delegate()
            {
                renderByteView(data.Reader(), (int)data.Header.DataSize);
            }, null);
            root.Nodes.Add("cell view").Tag = new NodeData(delegate()
            {
                renderHexView(data.Reader(), (int)data.Header.DataSize);
            }, null);
            root.Nodes.Add("string analysis").Tag = new NodeData(delegate()
            {
                renderStringAnalysis(data.Memory(), data.Reader(), (int)data.Header.DataSize);
            }, null);
        }

        private void renderPublicsList(TreeNode root, SmxPublicTable publics)
        {
            for (var i = 0; i < publics.Length; i++)
            {
                var index = i;
                var pubfun = publics[i];
                var node = root.Nodes.Add(i + ": " + pubfun.Name);
                node.Tag = new NodeData(delegate()
                {
                    startDetail("; public entry {0}", index);
                    addDetailLine("nameoffs = 0x{0:x} ; {1}", pubfun.nameoffs, pubfun.Name);
                    addDetailLine("address = 0x{0:x}", pubfun.Address);
                    endDetailUpdate();
                }, null);
            }
        }

        private void renderPubvarList(TreeNode root, SmxPubvarTable pubvars)
        {
            for (var i = 0; i < pubvars.Length; i++)
            {
                var index = i;
                var pubvar = pubvars[i];
                var node = root.Nodes.Add(i + ": " + pubvar.Name);
                node.Tag = new NodeData(delegate()
                {
                    startDetail("; pubvar entry {0}", index);
                    addDetailLine("nameoffs = 0x{0:x} ; {1}", pubvar.nameoffs, pubvar.Name);
                    addDetailLine("address = 0x{0:x}", pubvar.Address);
                    endDetailUpdate();
                }, null);
            }
        }

        private void renderTagList(TreeNode root, SmxTagTable tags)
        {
            for (var i = 0; i < tags.Length; i++)
            {
                var tag = tags[i];
                var text = tag.Id + ": " + tag.Name;
                if ((tag.Flags & ~(TagFlags.Fixed)) != 0)
                    text += " (" + (tag.Flags & ~(TagFlags.Fixed)) + ")";
                var node = root.Nodes.Add(text);
                node.Tag = new NodeData(delegate()
                {
                    startDetail("tag: 0x{0:x} ; flags = {1}", tag.Value, tag.Flags.ToString());
                    addDetailLine("nameoffs: 0x{0:x} ; {1}", tag.entry.nameoffs, tag.Name);
                    addDetailLine("id: 0x{0:x}", tag.Id);
                    endDetailUpdate();
                }, null);
            }
        }

        private void renderDebugLines(TreeNode root, SmxDebugLinesTable lines)
        {
            root.Tag = new NodeData(delegate()
            {
                renderSectionHeaderDetail(lines.SectionHeader);
                foreach (var line in lines.Entries)
                {
                    addDetailLine("line {0} @ address 0x{1:x}", line.Line, line.Address);
                }
                endDetailUpdate();
            }, null);
        }

        private void renderNativeList(TreeNode root, SmxNativeTable natives)
        {
            for (var i = 0; i < natives.Length; i++)
            {
                var index = i;
                var native = natives[i];
                var node = root.Nodes.Add("[" + i + "] " + native.Name);
                node.Tag = new NodeData(delegate()
                {
                    startDetail("index = {0}", index);
                    addDetailLine("nameoffs: 0x{0:x} ; {1}", native.nameoffs, native.Name);
                    endDetailUpdate();
                }, null);
            }
        }

        private void renderNamesList(TreeNode root, SmxNameTable names)
        {
            root.Tag = new NodeData(delegate()
            {
                renderSectionHeaderDetail(names.SectionHeader);
                foreach (var offset in names.Extents)
                {
                    addDetailLine("0x{0:x}: {1}", offset, names.StringAt(offset));
                }
                endDetailUpdate();
            }, null);
        }

        private void renderDebugFiles(TreeNode root, SmxDebugFilesTable files)
        {
            root.Tag = new NodeData(delegate()
            {
                renderSectionHeaderDetail(files.SectionHeader);
                addDetailLine("--");
                foreach (var file in files.Entries)
                {
                    addDetailLine("\"{0}\"", file.Name);
                    addDetailLine(" nameoffs = 0x{0:x}", file.nameoffs);
                    addDetailLine(" address = 0x{0:x}", file.Address);
                }
                endDetailUpdate();
            }, null);
        }

        private void renderDebugInfo(TreeNode root, SmxDebugInfoSection info)
        {
            root.Tag = new NodeData(delegate()
            {
                renderSectionHeaderDetail(info.SectionHeader);
                addDetailLine("num_files = {0}", info.NumFiles);
                addDetailLine("num_lines = {0}", info.NumLines);
                addDetailLine("num_symbols = {0}", info.NumSymbols);
                addDetailLine("num_arrays = {0}", info.NumArrays);
                endDetailUpdate();
            }, null);
        }

        private string dimsToString(Tag tag, DebugSymbolDimEntry[] dims)
        {
            string str = "";
            for (var i = 0; i < dims.Length; i++)
            {
                int size;
                if (i == dims.Length - 1 && tag != null && tag.Name == "String")
                    size = dims[i].Size * 4;
                else
                    size = dims[i].Size;
                if (size == 0)
                    str += "[]";
                else
                    str += string.Format("[{0}]", size);
            }
            return str;
        }

        private void renderSymbolDetail(DebugSymbolEntry entry)
        {
            Tag tag = null;
            if (file_.Tags != null)
                tag = file_.Tags.FindTag(entry.TagId);

            startDetail("; {0}", entry.Name);
            if (entry.Address < 0)
                addDetailLine("address = -0x{0:x}", -entry.Address);
            else
                addDetailLine("address = 0x{0:x}", entry.Address);
            if (tag == null)
                addDetailLine("tagid = 0x{0:x}", entry.TagId);
            else
                addDetailLine("tagid = 0x{0:x} ; {1}", entry.TagId, tag.Name);
            addDetailLine("codestart = 0x{0:x}", entry.CodeStart);
            addDetailLine("codeend = 0x{0:x}", entry.CodeEnd);
            addDetailLine("nameoffs = 0x{0:x} ; {1}", entry.nameoffs, entry.Name);
            addDetailLine("kind = {0:d} ; {1}", entry.Ident, entry.Ident.ToString());
            addDetailLine("scope = {0:d} ; {1}", entry.Scope, entry.Scope.ToString());

            if (entry.Dims != null)
            {
                addDetailLine("dims = {0}", dimsToString(tag, entry.Dims));
            }

            string file = null;
            if (file_.DebugFiles != null)
                file = file_.DebugFiles.FindFile(entry.CodeStart);
            if (file != null)
                addDetailLine("file: \"{0}\"", (string)file);

            uint? line = null;
            if (file_.DebugLines != null)
                line = file_.DebugLines.FindLine(entry.CodeStart);
            if (line != null)
                addDetailLine("line: \"{0}\"", (uint)line);
            endDetailUpdate();
        }

        private void renderDebugFunction(SmxDebugSymbolsTable syms, TreeNode root, DebugSymbolEntry fun)
        {
            root.Tag = new NodeData(delegate()
            {
                renderSymbolDetail(fun);
            }, null);

            var args = new List<DebugSymbolEntry>();
            var locals = new List<DebugSymbolEntry>();
            foreach (var sym_ in syms.Entries)
            {
                var sym = sym_;
                if (sym.Scope == SymScope.Global)
                    continue;
                if (sym.CodeStart < fun.CodeStart || sym.CodeEnd > fun.CodeEnd)
                    continue;
                if (sym.Address < 0)
                    locals.Add(sym);
                else
                    args.Add(sym);
            }

            args.Sort(delegate (DebugSymbolEntry e1, DebugSymbolEntry e2)
            {
                return e1.Address.CompareTo(e2.Address);
            });
            foreach (var sym_ in args)
            {
                var sym = sym_;
                var node = root.Nodes.Add(sym.Name);
                node.Tag = new NodeData(delegate()
                {
                    renderSymbolDetail(sym);
                }, null);
            }

            locals.Sort(delegate (DebugSymbolEntry e1, DebugSymbolEntry e2)
            {
                return e1.CodeStart.CompareTo(e2.CodeStart);
            });
            foreach (var sym_ in locals)
            {
                var sym = sym_;
                var node = root.Nodes.Add(sym.Name);
                node.Tag = new NodeData(delegate()
                {
                    renderSymbolDetail(sym);
                }, null);
            }
        }

        private void renderDebugSymbols(TreeNode root, SmxDebugSymbolsTable syms)
        {
            var globals = root.Nodes.Add("globals");
            foreach (var sym_ in syms.Entries)
            {
                var sym = sym_;
                if (sym.Scope != SymScope.Global)
                    continue;
                if (sym.Ident == SymKind.Function)
                    continue;
                var node = globals.Nodes.Add(sym.Name);
                node.Tag = new NodeData(delegate()
                {
                    renderSymbolDetail(sym);
                }, null);
            }

            var functions = root.Nodes.Add("functions");
            foreach (var sym_ in syms.Entries)
            {
                var sym = sym_;
                if (sym.Scope != SymScope.Global)
                    continue;
                if (sym.Ident != SymKind.Function)
                    continue;
                var node = functions.Nodes.Add(sym.Name);
                renderDebugFunction(syms, node, sym);
            }
        }

        private void renderDebugNative(DebugNativeEntry entry)
        {
            Tag tag = null;
            if (file_.Tags != null)
                tag = file_.Tags.FindTag(entry.tagid);

            startDetailUpdate();
            addDetailLine("nameoffs = 0x{0:x}", entry.nameoffs, entry.Name);
            if (tag == null)
                addDetailLine("tagid = 0x{0:x}", entry.tagid);
            else
                addDetailLine("tagid = 0x{0:x} ; {1}", entry.tagid, tag.Name);
            addDetailLine("index = {0}", entry.Index);
            addDetailLine("nargs = {0}", entry.nargs);
            for (var i = 0; i < entry.Args.Length; i++)
            {
                var arg = entry.Args[i];
                addDetailLine("arg {0}", i);
                addDetailLine("  nameoffs = 0x{0:x} ; {1}", arg.nameoffs, arg.Name);
                addDetailLine("  kind = {0:d} ; {1}", arg.Ident, arg.Ident.ToString());
                if (file_.Tags != null)
                    tag = file_.Tags.FindTag(arg.tagid);
                if (tag == null)
                    addDetailLine("  tagid = 0x{0:x}", arg.tagid);
                else
                    addDetailLine("  tagid = 0x{0:x} ; {1}", arg.tagid, tag.Name);

                if (arg.Dims != null)
                {
                    addDetailLine("  dims = {0}", dimsToString(tag, arg.Dims));
                }
            }
            endDetailUpdate();
        }

        private void renderDebugNatives(TreeNode root, SmxDebugNativesTable natives)
        {
            foreach (var native_ in natives.Entries)
            {
                var native = native_;
                var node = root.Nodes.Add(native.Name);
                node.Tag = new NodeData(delegate()
                {
                    renderDebugNative(native);
                }, null);
            }
        }

        private void treeview_AfterSelect(object sender, TreeViewEventArgs e)
        {
            var node = e.Node;
            if (node.Tag == null)
                return;
            var data = (NodeData)node.Tag;
            if (data.callback == null)
                return;
            data.callback();
        }
    }
}
