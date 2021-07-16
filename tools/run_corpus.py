# vim: set ts=4 sw=4 tw=99 et:
import argparse
import os
import progressbar
import re
import subprocess
import sys
import tempfile

# Tool for interacting with a .sp corpus to find behavorial differences between
# compiler versions.
def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('spcomp', type = str, help = 'Path to spcomp')
    parser.add_argument('corpus', type = str, help = 'Path to plugin corpus')
    parser.add_argument('-i', '--include', type = str, action = 'append',
                        help = 'Extra include paths')
    parser.add_argument('--remove-bad', action = 'store_true', default = False,
                        help = 'Remove bad .sp files on failure')
    parser.add_argument('--commit', action = 'store_true', default = False,
                        help = 'Run destructive commands instead of printing them')
    parser.add_argument('--diagnose', action = 'store_true', default = False,
                        help = 'Interactive diagnose script')
    parser.add_argument("--remove-good", action = 'store_true', default = False,
                        help = 'Remove good .sp failes on success')

    args = parser.parse_args()

    files = []
    get_all_files(args.corpus, '.sp', files)

    with tempfile.TemporaryDirectory() as temp_dir:
        runner = Runner(args, files, temp_dir)
        runner.run()

class Runner(object):
    def __init__(self, args, files, temp_dir):
        self.args_ = args
        self.files_ = files
        self.temp_dir_ = temp_dir

    def run(self):
        for i in progressbar.progressbar(range(len(self.files_)), redirect_stdout = True):
            self.compile(self.files_[i])

    def compile(self, path):
        argv = [
            self.args_.spcomp,
            path,
        ]
        for include_path in self.args_.include:
            argv += ['-i', include_path]
        argv += [
            '-o',
            os.path.join(self.temp_dir_, os.path.basename(path)),
        ]

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

        if not ok:
            if self.args_.remove_bad:
                print("rm \"{}\"".format(path))
                if self.args_.commit:
                    os.unlink(path)
            elif self.args_.diagnose:
                diagnose_error(path, output.decode('utf8'))
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

def get_all_files(path, ext, out):
    for file in os.listdir(path):
        child = os.path.join(path, file)
        if os.path.isdir(child):
            get_all_files(child, ext, out)
        elif child.endswith(ext):
            out.append(child)

if __name__ == '__main__':
    main()
