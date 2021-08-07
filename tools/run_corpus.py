# vim: set ts=4 sw=4 tw=99 et:
import argparse
import os
import progressbar
import queue
import re
import shutil
import subprocess
import sys
import tempfile
import threading

# Tool for interacting with a .sp corpus to find behavorial differences between
# compiler versions.
def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('spcomp', type = str, help = 'Path to spcomp')
    parser.add_argument('corpus', type = str, help = 'Path to plugin corpus')
    parser.add_argument('-i', '--include', type = str, action = 'append', default = [],
                        help = 'Extra include paths')
    parser.add_argument('--remove-bad', action = 'store_true', default = False,
                        help = 'Remove bad .sp files on failure')
    parser.add_argument('--commit', action = 'store_true', default = False,
                        help = 'Run destructive commands instead of printing them')
    parser.add_argument('--diagnose', action = 'store_true', default = False,
                        help = 'Interactive diagnose script')
    parser.add_argument("--remove-good", action = 'store_true', default = False,
                        help = 'Remove good .sp failes on success')
    parser.add_argument("-j", type = int, default = 1,
                        help = "Number of compile jobs; does not work with --diagnose")
    parser.add_argument("--verifier", type = str, default = None,
                        help = "Optional verification tool for .smx files")
    parser.add_argument("--collect-smx", type = str, default = None,
                        help = "Copy .smx files to the given folder")

    args = parser.parse_args()

    if args.j > 1 and args.diagnose:
        print("Cannot use both -j and --diagnose.")
        return 1

    files = []
    get_all_files(args.corpus, ['.sp', '.smx'], files)

    with tempfile.TemporaryDirectory() as temp_dir:
        runner = Runner(args, files, temp_dir)
        runner.run()

class Runner(object):
    def __init__(self, args, files, temp_dir):
        self.args_ = args
        self.files_ = files
        self.temp_dir_ = temp_dir
        self.threads_ = []
        self.work_ = queue.LifoQueue()
        self.completed_ = queue.Queue()
        self.progress_ = 0

    def run(self):
        with progressbar.ProgressBar(max_value = len(self.files_), redirect_stdout = True) as bar:
            if self.args_.j <= 1:
                self.run_st(bar)
            else:
                self.run_mt(bar)

    def run_st(self, bar):
        for i in range(len(self.files_)):
            result_tuple = self.compile(self.files_[i])
            self.handle_result(result_tuple)

            bar.update(i)

    def run_mt(self, bar):
        for file in self.files_:
            self.work_.put(file)

        for i in range(self.args_.j):
            thread = threading.Thread(None, self.consumer)
            self.threads_.append(thread)
            thread.start()

        while self.progress_ < len(self.files_):
            result_tuple = self.completed_.get()
            self.handle_result(result_tuple)
            bar.update(self.progress_)
            self.progress_ += 1

        for thread in self.threads_:
            thread.join()

    def consumer(self):
        while True:
            try:
                item = self.work_.get_nowait()
            except queue.Empty:
                item = None
            if item is None:
                if self.work_.empty():
                    break
                continue
            result_tuple = self.compile(item)
            self.completed_.put(result_tuple)

    def compile(self, path):
        if path.endswith('.sp'):
            argv = [
                self.args_.spcomp,
                path,
            ]
            for include_path in self.args_.include:
                argv += ['-i', include_path]

            output_file = os.path.join(self.temp_dir_, os.path.basename(path))
            if output_file.endswith('.sp'):
                output_file = output_file[:-3]
                output_file += '.smx'

            argv += ['-o', output_file]

            ok = False
            output = None
            try:
                subprocess.check_output(argv, stderr = subprocess.STDOUT)
                ok = True
            except KeyboardInterrupt:
                raise
            except subprocess.CalledProcessError as e:
                output = e.output
            except:
                pass
        else:
            ok = True
            output = ''
            output_file = path

        if ok and self.args_.verifier:
            argv = [self.args_.verifier, output_file]
            ok = False
            try:
                subprocess.check_output(argv, stderr = subprocess.STDOUT)
                ok = True
            except KeyboardInterrupt:
                raise
            except subprocess.CalledProcessError as e:
                output = e.output
            except:
                pass

        if ok and path.endswith('.sp') and self.args_.collect_smx:
            shutil.move(output_file,
                        os.path.join(self.args_.collect_smx, os.path.basename(output_file)))

        return (ok, path, output)

    def handle_result(self, result_tuple):
        ok, path, output = result_tuple

        if not ok:
            if self.args_.diagnose:
                diagnose_error(path, output.decode('utf8'))
            elif self.args_.remove_bad:
                print("rm \"{}\"".format(path))
                if self.args_.commit:
                    os.unlink(path)
        else:
            if self.args_.remove_good:
                print("rm \"{}\"".format(path))
                if self.args_.commit:
                    os.unlink(path)

def diagnose_error(path, output):
    print("Error compiling {}:".format(path))
    print("")

    lines = [line.strip() for line in output.split('\n')]
    for line in lines:
        m = re.search(r"\((\d+)\) : error", line)
        if m is None:
            continue

        message = line[m.start():]
        print("  " + message)
        print("")

        diag = extract_line(path, int(m.group(1)))
        if diag is None:
            print("   >> Unknown line <<")
        else:
            print("  " + diag.strip())
        print("")

    while True:
        sys.stdout.write("(D)elete or (S)kip? ")
        progressbar.streams.flush()
        line = sys.stdin.readline()
        line = line.strip()
        if line == 'D' or line == 'd':
            os.unlink(path)
            return
        elif line == 'S' or line == 's':
            return

def extract_line(path, number):
    with open(path, 'rb') as fp:
        data = fp.read()
    text = data.decode('utf8', 'ignore')
    lines = text.split('\n')
    if number - 1 >= len(lines):
        return None
    return lines[number - 1]

def get_all_files(path, exts, out):
    for file in os.listdir(path):
        child = os.path.join(path, file)
        if os.path.isdir(child):
            get_all_files(child, exts, out)
            continue
        _, ext = os.path.splitext(child)
        if ext in exts:
            out.append(child)

if __name__ == '__main__':
    main()
