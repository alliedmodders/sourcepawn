# vim: set ts=2 sw=2 tw=99 et:
import re
import os, sys
import argparse
import subprocess
import tempfile
import shutil

def main():
  parser = argparse.ArgumentParser()
  parser.add_argument('test', type=str, nargs='?', default=None,
                      help="Optional test folder or test file")
  parser.add_argument('--disable-phopt', default=False, action='store_true',
                      help="Disable the peephole optimizer when compiling")
  parser.add_argument('--spcomp', type=str, help="spcomp invocation string", required=True)
  parser.add_argument('--shell', type=str, help="Path to shell", required=True)
  args = parser.parse_args()

  with TempFolder() as tempFolder:
    runner = TestRunner(args, tempFolder)
    runner.run()

class TempFolder(object):
  def __enter__(self):
    self.name = tempfile.mkdtemp()
    return self.name

  def __exit__(self, exc_Type, exc_value, traceback):
    assert self.name
    shutil.rmtree(self.name)

class ChangeFolder(object):
  def __init__(self, path):
    super(ChangeFolder, self).__init__()
    self.path = path
    self.cwd = None

  def __enter__(self):
    self.cwd = os.getcwd()
    os.chdir(self.path)

  def __exit__(self, exc_Type, exc_value, traceback):
    if self.cwd is not None:
      os.chdir(self.cwd)

class Test(object):
  ManifestKeys = set([
    'returnCode',
  ])

  def __init__(self, name, path):
    super(Test, self).__init__()
    self.name = name
    self.path = os.path.abspath(path)
    self.manifest = {}
    self.stdout_file = None

    base, _ = os.path.splitext(self.path)
    stdout_file = base + '.out'
    if os.path.exists(stdout_file):
      self.stdout_file = stdout_file
    
    self.smx_name =  os.path.basename(base) + '.smx'

  def load_test(self):
    with open(self.path, 'r') as fp:
      for line in fp:
        m = re.match("// ([^:]+): (.+)\s*$", line)
        if m is None:
          break
        key = m.group(1)
        value = m.group(2)
        if key not in Test.ManifestKeys:
          raise Exception("Test {0} contains unsupported manifest key {1}".format(
            self.name, key))
        self.manifest[key] = value

  @property
  def expectedReturnCode(self):
    if 'returnCode' in self.manifest:
      return int(self.manifest['returnCode'])
    return 0

class TestRunner(object):
  def __init__(self, args, tempFolder):
    super(TestRunner, self).__init__()
    self.args = args
    self.spcomp = args.spcomp.split(' ')
    self.spcomp[0] = os.path.abspath(self.spcomp[0])
    self.shell = os.path.abspath(args.shell)
    self.tmp_folder = tempFolder
    self.inc_folder = os.path.dirname(os.path.abspath(__file__))
    if args.test is not None:
      self.testpath = args.test
    else:
      self.testpath = self.inc_folder
    self.tests = []
    self.numCompileFailures = 0
    self.numRunFailures = 0
    self.failures = []

    # Walk up the test path looking for an 'include' folder.
    search_path, _ = os.path.split(self.testpath)
    while True:
      include_path = os.path.join(search_path, 'include')
      if os.path.exists(include_path):
        break
      search_path, tail = os.path.split(search_path)
      if not tail:
        break
    self.include_path = include_path

  def run(self):
    self.collect_tests()

    with ChangeFolder(self.tmp_folder):
      for test in self.tests:
        if not self.run_test(test):
          self.failures.append(test)

    if len(self.failures):
      self.out("--- Failure Summary ---\n")
      self.out("Compiler failures: {0}\n".format(self.numCompileFailures))
      self.out("Test failures: {0}\n".format(self.numRunFailures))
      for test in self.failures:
        self.out("  {0}\n".format(test.name))
      sys.exit(1)
      return

    self.out("--- All tests passed! ---\n")

  def collect_tests(self):
    def add_path(root):
      path = os.path.join(self.testpath, root)
      if os.path.isdir(path):
        for name in os.listdir(path):
          add_path(os.path.join(root, name))
      elif path.endswith('.sp'):
        self.add_test(root, path)
      elif not os.path.exists(path):
        raise Exception("Path not found: {0}".format(path))

    add_path('')

    if not len(self.tests):
      raise Exception("No tests found at {0}".format(self.testpath))

  def add_test(self, name, path):
    self.tests.append(Test(name, path))

  def fix_path(self, path):
    if os.path.isabs(path) and os.path.splitext(self.spcomp[0])[1] == '.js':
      return '/fakeroot' + path
    return path

  def run_test(self, test):
    self.out("{0}:\n".format(test.name))

    test.load_test()

    smx_path = self.run_compiler(test)
    if not smx_path:
      self.numCompileFailures += 1
      self.out("\n")
      return False

    if not self.run_shell(test, smx_path):
      self.numRunFailures += 1
      self.out("\n")
      return False

    return True

  def run_compiler(self, test):
    self.out("  [SMX] ")

    test_path = self.fix_path(test.path)
    inc_folder = self.fix_path(self.inc_folder)

    argv = self.spcomp + [
      '-i' + inc_folder,
      '-i' + self.fix_path(os.path.abspath(self.include_path)),
    ]
    if os.path.splitext(self.spcomp[0])[1] == '.js':
      argv = ['node'] + argv
    if self.args.disable_phopt:
      argv += ['-O0']
    argv += [
      test_path,
    ]

    p = subprocess.Popen(argv, stdout = subprocess.PIPE, stderr = subprocess.PIPE)
    stdout, stderr = p.communicate()
    stdout = stdout.decode('utf-8')
    stderr = stderr.decode('utf-8')
    if p.returncode != 0:
      self.out("FAIL\n")
      self.out("\nDumping compiler output...\n")
      self.out("##############\n")
      self.out("### STDOUT ###\n")
      self.out_with_nl(sys.stdout, stdout)
      if len(stderr.strip()) != 0:
        self.out("### STDERR ###\n")
        self.out_with_nl(sys.stderr, stderr)
      self.out("##############\n")
      return None
    self.out("PASS\n")

    assert os.path.exists(test.smx_name)
    return test.smx_name

  def run_shell(self, test, smx_path):
    self.out("  [RUN] ")
    if os.path.isabs(smx_path) and os.path.splitext(self.shell)[1] == '.js':
      smx_path = '/fakeroot' + smx_path
    argv = [
      self.shell,
      smx_path,
    ]
    if os.path.splitext(self.shell)[1] == '.js':
      argv = ['node'] + argv
    p = subprocess.Popen(argv, stdout = subprocess.PIPE, stderr = subprocess.PIPE)
    stdout, stderr = p.communicate()
    stdout = stdout.decode('utf-8')
    stderr = stderr.decode('utf-8')
    if test.expectedReturnCode != p.returncode:
      self.out("FAIL\n")
      self.out("\nDumping shell output...\n")
      self.out("##############\n")
      self.out("### STDOUT ###\n")
      self.out_with_nl(sys.stdout, stdout)
      if len(stderr.strip()) != 0:
        self.out("### STDERR ###\n")
        self.out_with_nl(sys.stderr, stderr)
      self.out("##############\n")
      return False

    if test.stdout_file is not None:
      if not self.compare_output(test, 'stdout', stdout, test.stdout_file):
        return False

    self.out("PASS\n")
    return True

  def compare_output(self, test, pipeName, actual, path):
    lineNumber = 1

    with open(path, 'r') as fp:
      # By default we normalize \r\n to \n in the expected output.
      expectedLines = [line.replace("\r\n", "\n") for line in fp]

    # Append "\n" to everything but the last line.
    actual = actual.replace("\r\n", "\n")
    actualLines = [line + "\n" for line in actual.split("\n")]
    actualLines[-1] = actualLines[-1].replace("\n", "")

    # Remove the last line from actual if it's empty and the expected is one
    # line short.
    if len(actualLines) - 1 == len(expectedLines):
      if not len(actualLines[-1]):
        actualLines.pop()

    while True:
      if len(actualLines) and not len(expectedLines):
        self.out("FAIL\n")
        self.out("\n{0} line {1}: actual is longer than expected.\n".format(pipeName, lineNumber))
        return False
      if not len(actualLines) and len(expectedLines):
        self.out("FAIL\n")
        self.out("\n{0} line {1}: actual is missing lines from expected.\n".format(pipeName, lineNumber))
        return False

      actual = actualLines.pop(0)
      expected = expectedLines.pop(0)
      if actual != expected:
        self.out("FAIL\n")
        self.out("\n{0} line {1}: actual is different from expected.\n".format(pipeName, lineNumber))
        self.out("### ACTUAL ###\n")
        self.out_with_nl(sys.stdout, actual)
        self.out("### EXPECTED ###\n")
        self.out_with_nl(sys.stdout, expected)
        self.out("################\n")
        return False

      lineNumber += 1
      if not len(actualLines) and not len(expectedLines):
        break

    return True

  def out(self, text):
    sys.stdout.write(text)
    sys.stdout.flush()

  def out_with_nl(self, pipe, output):
    pipe.write(output)
    if not output.endswith("\n"):
      pipe.write("\n")

if __name__ == '__main__':
  main()
