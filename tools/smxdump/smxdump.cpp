// vim: set sts=4 ts=8 sw=4 tw=99 et:
// 
// Copyright (C) 2026 AlliedModders LLC
// 
// This file is part of SourcePawn. SourcePawn is free software: you can
// redistribute it and/or modify it under the terms of the GNU General Public
// License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
//
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.
//
#include <sp_vm_api.h>
#include <amtl/experimental/am-argparser.h>
#include "vm/environment.h"
#include "vm/smx-image.h"

using namespace ke;
using namespace ke::args;
using namespace sp;
using namespace SourcePawn;

Environment* sEnv;

StringOption filename("file", "SMX file");
ToggleOption show_name_offsets(nullptr, "--show-name-offsets", Some(false),
                               "Show all name offsets");

class ShellDebugListener : public IDebugListener
{
public:
  void ReportError(const IErrorReport& report, IFrameIterator& iter) override {
    fprintf(stdout, "Exception thrown: %s\n", report.Message());
  }

  void OnDebugSpew(const char* msg, ...) override {
#if !defined(NDEBUG) && defined(DEBUG)
    va_list ap;
    va_start(ap, msg);
    vfprintf(stderr, msg, ap);
    va_end(ap);
#endif
  }
};

class DumpTool final {
  public:
    explicit DumpTool(const char* file, std::unique_ptr<SmxImage> smx)
      : smx_(std::move(smx)),
        file_(file)
    {}

    void Dump() {
        DumpHeaders();

        DumpPubvars();
    }

    void DumpHeaders() {
        fprintf(stdout, "; %s\n", file_.c_str());
        fprintf(stdout, ";   magic = 0x%x\n", smx_->hdr()->magic);
        fprintf(stdout, ";   version = 0x%x\n", smx_->hdr()->version);
        fprintf(stdout, ";   compression = 0x%x\n", smx_->hdr()->compression);
        fprintf(stdout, ";   disksize = 0x%x\n", smx_->hdr()->disksize);
        fprintf(stdout, ";   imagesize = 0x%x\n", smx_->hdr()->imagesize);
        fprintf(stdout, ";   sections = 0x%x\n", smx_->hdr()->imagesize);
        fprintf(stdout, ";   stringtab = 0x%x\n", smx_->hdr()->imagesize);
        fprintf(stdout, ";   dataoffs = 0x%x\n", smx_->hdr()->imagesize);
    }

    void DumpPubvars() {
        auto pubvars = smx_->pubvars();
        if (pubvars.length() == 0)
            return;

        fprintf(stdout, ".pubvars\n");
        for (uint32_t i = 0; i < pubvars.length(); i++) {
            const char* name = smx_->names() + pubvars[i].name;
            fprintf(stdout, "    ; index %u\n", i);
            fprintf(stdout, "    %s = 0x%x", name, pubvars[i].address);
            if (show_name_offsets.value())
                fprintf(stdout, " ; name_offset = %u", pubvars[i].name);
            fprintf(stdout, "\n");
        }
    }

  private:
    std::unique_ptr<SmxImage> smx_;
    std::string file_;
};

static int Dump(const char* file) {
    std::unique_ptr<FILE, decltype(&::fclose)> fp(fopen(file, "rb"), ::fclose);
    auto smx = std::make_unique<SmxImage>(fp.get());
    if (!smx->validate()) {
        fprintf(stderr, "Could not parse %s: %s\n", file, smx->errorMessage());
        return 1;
    }

    DumpTool tool(file, std::move(smx));
    tool.Dump();
    return 0;
}

int main(int argc, char** argv)
{
  Parser parser("SourcePawn SMX disassembly");

  if (!parser.parse(argc, argv)) {
    parser.usage(stderr, argc, argv);
    return 1;
  }

  if ((sEnv = Environment::New()) == nullptr) {
    fprintf(stderr, "Could not initialize ISourcePawnEngine2\n");
    return 1;
  }

  ShellDebugListener debug;
  sEnv->SetDebugger(&debug);

  int errcode = Dump(filename.value().c_str());

  sEnv->Shutdown();
  delete sEnv;
  return errcode;
}
