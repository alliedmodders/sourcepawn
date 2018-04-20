namespace smxviewer
{
    partial class MainWindow
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(MainWindow));
            this.menuStrip_ = new System.Windows.Forms.MenuStrip();
            this.fileToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.openToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem1 = new System.Windows.Forms.ToolStripSeparator();
            this.exitToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.container_ = new System.Windows.Forms.SplitContainer();
            this.treeview_ = new System.Windows.Forms.TreeView();
            this.detailbox_ = new System.Windows.Forms.TextBox();
            this.menuStrip_.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.container_)).BeginInit();
            this.container_.Panel1.SuspendLayout();
            this.container_.Panel2.SuspendLayout();
            this.container_.SuspendLayout();
            this.SuspendLayout();
            // 
            // menuStrip_
            // 
            this.menuStrip_.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.fileToolStripMenuItem});
            this.menuStrip_.Location = new System.Drawing.Point(0, 0);
            this.menuStrip_.Name = "menuStrip_";
            this.menuStrip_.Size = new System.Drawing.Size(838, 24);
            this.menuStrip_.TabIndex = 0;
            this.menuStrip_.Text = "menuStrip1";
            // 
            // fileToolStripMenuItem
            // 
            this.fileToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.openToolStripMenuItem,
            this.toolStripMenuItem1,
            this.exitToolStripMenuItem});
            this.fileToolStripMenuItem.Name = "fileToolStripMenuItem";
            this.fileToolStripMenuItem.Size = new System.Drawing.Size(37, 20);
            this.fileToolStripMenuItem.Text = "&File";
            // 
            // openToolStripMenuItem
            // 
            this.openToolStripMenuItem.Name = "openToolStripMenuItem";
            this.openToolStripMenuItem.Size = new System.Drawing.Size(103, 22);
            this.openToolStripMenuItem.Text = "&Open";
            this.openToolStripMenuItem.Click += new System.EventHandler(this.openToolStripMenuItem_Click);
            // 
            // toolStripMenuItem1
            // 
            this.toolStripMenuItem1.Name = "toolStripMenuItem1";
            this.toolStripMenuItem1.Size = new System.Drawing.Size(100, 6);
            // 
            // exitToolStripMenuItem
            // 
            this.exitToolStripMenuItem.Name = "exitToolStripMenuItem";
            this.exitToolStripMenuItem.Size = new System.Drawing.Size(103, 22);
            this.exitToolStripMenuItem.Text = "E&xit";
            this.exitToolStripMenuItem.Click += new System.EventHandler(this.exitToolStripMenuItem_Click);
            // 
            // container_
            // 
            this.container_.Dock = System.Windows.Forms.DockStyle.Fill;
            this.container_.Location = new System.Drawing.Point(0, 24);
            this.container_.Name = "container_";
            this.container_.Orientation = System.Windows.Forms.Orientation.Horizontal;
            // 
            // container_.Panel1
            // 
            this.container_.Panel1.Controls.Add(this.treeview_);
            // 
            // container_.Panel2
            // 
            this.container_.Panel2.Controls.Add(this.detailbox_);
            this.container_.Size = new System.Drawing.Size(838, 562);
            this.container_.SplitterDistance = 383;
            this.container_.TabIndex = 1;
            // 
            // treeview_
            // 
            this.treeview_.Dock = System.Windows.Forms.DockStyle.Fill;
            this.treeview_.Location = new System.Drawing.Point(0, 0);
            this.treeview_.Name = "treeview_";
            this.treeview_.Size = new System.Drawing.Size(838, 383);
            this.treeview_.TabIndex = 0;
            this.treeview_.AfterSelect += new System.Windows.Forms.TreeViewEventHandler(this.treeview_AfterSelect);
            // 
            // detailbox_
            // 
            this.detailbox_.Dock = System.Windows.Forms.DockStyle.Fill;
            this.detailbox_.Font = new System.Drawing.Font("Consolas", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.detailbox_.ForeColor = System.Drawing.Color.Black;
            this.detailbox_.Location = new System.Drawing.Point(0, 0);
            this.detailbox_.Multiline = true;
            this.detailbox_.Name = "detailbox_";
            this.detailbox_.ReadOnly = true;
            this.detailbox_.ScrollBars = System.Windows.Forms.ScrollBars.Both;
            this.detailbox_.Size = new System.Drawing.Size(838, 175);
            this.detailbox_.TabIndex = 0;
            // 
            // MainWindow
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(838, 586);
            this.Controls.Add(this.container_);
            this.Controls.Add(this.menuStrip_);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.MainMenuStrip = this.menuStrip_;
            this.Name = "MainWindow";
            this.Text = "SMX Viewer";
            this.Load += new System.EventHandler(this.MainWindow_Load);
            this.menuStrip_.ResumeLayout(false);
            this.menuStrip_.PerformLayout();
            this.container_.Panel1.ResumeLayout(false);
            this.container_.Panel2.ResumeLayout(false);
            this.container_.Panel2.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.container_)).EndInit();
            this.container_.ResumeLayout(false);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.MenuStrip menuStrip_;
        private System.Windows.Forms.ToolStripMenuItem fileToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem openToolStripMenuItem;
        private System.Windows.Forms.ToolStripSeparator toolStripMenuItem1;
        private System.Windows.Forms.ToolStripMenuItem exitToolStripMenuItem;
        private System.Windows.Forms.SplitContainer container_;
        private System.Windows.Forms.TreeView treeview_;
        private System.Windows.Forms.TextBox detailbox_;
    }
}

