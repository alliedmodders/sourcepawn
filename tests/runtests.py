# vim: set ts=2 sw=2 tw=99 et:
import re
import os, sys
import argparse
import subprocess
import testutil
import datetime

def main():
  parser = argparse.ArgumentParser()
  parser.add_argument('objdir', type=str, help='Build folder to test.')
  parser.add_argument('test', type=str, nargs='?', default=None,
                      help="Optional test folder or test file")
  parser.add_argument('--arch', type=str, default=None,
                      help="Force testing a specific arch on dual-arch builds.")
  parser.add_argument('--show-cli', default=False, action='store_true',
                      help='Show the command-line invocation of each test.')
  args = parser.parse_args()

  plan = TestPlan(args)
  plan.find_compilers()
  plan.find_shells()
  plan.find_tests()

  if not len(plan.modes):
    raise Exception('No compiler binaries were found in {0}'.format(args.objdir))
  if not len(plan.tests):
    raise Exception('No matching tests were found.')
  if not len(plan.shells):
    raise Exception('No spshell binaries were found in {0}'.format(args.objdir))

  with testutil.TempFolder() as tempFolder:
    runner = TestRunner(plan)
    if not runner.run():
      sys.exit(1)

  # Done.
  sys.exit(0)

class TestPlan(object):
  def __init__(self, args):
    self.args = args
    self.shells = []
    self.tests = []
    self.modes = []
    self.tests_path = os.path.split(__file__)[0]

  @property
  def show_cli(self):
    return self.args.show_cli

  arch_suffixes = [
    '',
    '.x64',
  ]
  def match_arch(self, arch):
    if self.args.arch is None:
      return True
    if self.args.arch == 'x86':
      return arch == ''
    if self.args.arch == 'x64' or self.args.arch == 'x86_64':
      return arch == '.x64'
    return False

  def find_executable(self, path):
    if os.path.exists(path):
      pass
    elif os.path.exists(path + '.js'):
      path += '.js'
    elif os.path.exists(path + '.exe'):
      path += '.exe'
    else:
      return None
    return path

  def find_shells(self):
    for arch in self.arch_suffixes:
      if not self.match_arch(arch):
        continue

      path = os.path.join(self.args.objdir, 'vm', 'spshell' + arch, 'spshell')
      path = self.find_executable(path)

      if not path:
        continue

      path = os.path.abspath(path)

      rc, stdout, stderr = testutil.exec_argv([path, '--version'])
      if rc == 0 and 'JIT' in stdout:
        self.shells.append({
          'path': path,
          'args': [],
          'name': 'default' + arch,
          })

      self.shells.append({
        'path': path,
        'args': ['--disable-jit'],
        'name': 'interpreter' + arch,
      })

  def find_compilers(self):
    self.find_spcomp()

  def find_spcomp(self):
    for arch in self.arch_suffixes:
      if not self.match_arch(arch):
        continue

      path = os.path.join(self.args.objdir, 'compiler', 'spcomp' + arch, 'spcomp')
      path = self.find_executable(path)

      if not path:
        continue

      spcomp = {
        'path': os.path.abspath(path),
        'version': 1,
        'arch': arch,
        'name': 'spcomp',
      }

      self.modes.append({
        'name': 'default',
        'spcomp': spcomp,
        'args': [],
      })
      if path.endswith('.js'):
        # Emscripten takes a long time to run, so we only test the default
        # configuration.
        pass

      self.modes.append({
        'name': 'no_phopt',
        'spcomp': spcomp,
        'args': ['-O0'],
      })
      self.modes.append({
        'name': 'pcode12',
        'spcomp': spcomp,
        'args': ['-x12'],
      })

  def find_tests(self):
    for folder in os.listdir(self.tests_path):
      sub_folder = os.path.join(self.tests_path, folder)
      if not os.path.isdir(sub_folder):
        continue
      self.find_tests_impl(folder, {})

  def find_tests_impl(self, local_folder, manifest):
    folder = os.path.join(self.tests_path, local_folder)
    manifest_path = os.path.join(folder, 'manifest.ini')
    if os.path.exists(manifest_path):
      manifest = manifest.copy()
      manifest = testutil.parse_manifest(manifest_path, manifest)
      if manifest.get('skip', None) == 'true':
        return

    for name in os.listdir(folder):
      local_path = os.path.join(local_folder, name)
      path = os.path.join(self.tests_path, local_path)
      if os.path.isdir(path):
        self.find_tests_impl(local_path, manifest)
      elif path.endswith('.sp'):
        if self.args.test is not None and not local_path.startswith(self.args.test):
          continue
        self.tests.append(Test(**{
          'path': os.path.abspath(path),
          'manifest': manifest,
        }))

###
# This is a helper class instantiated for each test file in the tree.
###
class Test(object):
  ManifestKeys = set([
    'returnCode',
    'warnings_are_errors',
  ])

  def __init__(self, **kwargs):
    self.path = kwargs.pop('path')
    self.manifest_ = kwargs.pop('manifest')
    self.local_manifest_ = None
    self.smx_path = None
    self.stdout_file = None
    self.stderr_file = None

  def prepare(self):
    if self.local_manifest_ is not None:
      return

    self.read_local_manifest()

    smx_path = os.path.basename(self.path)
    smx_base_path, ext = os.path.splitext(smx_path)
    if ext == '.sp':
      self.smx_path = smx_base_path + '.smx'
    else:
      self.smx_path += '.smx'

    base_path, _ = os.path.splitext(self.path)
    if os.path.exists(base_path + '.out'):
      self.stdout_file = base_path + '.out'
    if os.path.exists(base_path + '.err'):
      self.stderr_file = base_path + '.err'
    if os.path.exists(base_path + '.txt'):
      self.txtout_file = base_path + '.txt'

  def get_expected_output(self, pipe_name):
    if pipe_name == 'stdout':
      pipe_file = self.stdout_file
    elif pipe_name == 'stderr':
      pipe_file = self.stderr_file
    elif pipe_name == 'txt':
      pipe_file = self.txtout_file

    with open(pipe_file, 'r') as fp:
      # By default we normalize \r\n to \n in the expected output.
      expected_lines = [line.replace("\r\n", "\n") for line in fp]
    return expected_lines

  @property
  def name(self):
    return os.path.split(self.path)[1]

  @property
  def unique_name(self):
    base_path = os.path.dirname(os.path.abspath(__file__))
    return os.path.relpath(self.path, base_path)

  @property
  def type(self):
    if 'type' in self.manifest_:
      return self.manifest_['type']
    return 'runtime'

  @property
  def warnings_are_errors(self):
    return self.local_manifest_.get('warnings_are_errors', None) == 'true'

  @property
  def expectedReturnCode(self):
    if 'returnCode' in self.local_manifest_:
      return int(self.local_manifest_['returnCode'])
    return 0

  def read_local_manifest(self):
    self.local_manifest_ = {}
    with open(self.path, 'r') as fp:
      for line in fp:
        if not self.process_manifest_line(line):
          break

  def process_manifest_line(self, line):
    if not line.startswith('//'):
      return False

    m = re.match("// ([^:]+): (.+)\s*$", line)
    if m is None:
      return False

    key = m.group(1)
    value = m.group(2)
    if key not in Test.ManifestKeys:
      raise Exception("Test {0} contains unsupported manifest key {1}".format(
        self.name, key))
    self.local_manifest_[key] = value
    return True

###
# The actual test harness.
###
class TestRunner(object):
  def __init__(self, plan):
    self.plan = plan
    self.include_path = os.path.dirname(os.path.abspath(__file__))
    self.start_time_ = datetime.datetime.now()
    self.failures_ = set()

    # Walk up the test path looking for an 'include' folder.
    search_path, _ = os.path.split(self.include_path)
    while True:
      include_path = os.path.join(search_path, 'include')
      if os.path.exists(include_path):
        break
      search_path, tail = os.path.split(search_path)
      if not tail:
        break
    self.core_include_path = include_path

  def run(self):
    with testutil.TempFolder() as temp_folder:
      with testutil.ChangeFolder(temp_folder):
        self.run_impl()

    if len(self.failures_):
      self.print_failures()
      return False
    return True

  def run_impl(self):
    for mode in self.plan.modes:
      self.run_mode(mode)

  def run_mode(self, mode):
    spcomp = mode['spcomp']

    self.out(" >> Testing mode '{0}' with compiler {1}{2} << ".format(
      mode['name'],
      spcomp['name'],
      spcomp['arch']))

    for test in self.plan.tests:
      test.prepare()
      if not self.run_test(mode, test):
        self.failures_.add(test)

  def run_test(self, mode, test):
    self.out('Begin test {0}'.format(test.path))

    # First run the compiler.
    rc, stdout, stderr = self.run_compiler(mode, test)
    if test.type == 'compile-only':
      if not self.compile_ok(mode, test, rc, stdout, stderr):
        self.out_io(stderr, stdout)
        return False
      self.out("PASS")
      return True

    # If this is a runtime test, the compiler must pass to continue.
    if rc != 0:
      self.out("Compile failed, return code {0} (expected 0)".format(rc))
      self.out_io(stderr, stdout)
      return False

    # Run all shells we found.
    return self.run_shells(mode, test)

  def run_compiler(self, mode, test):
    # Make sure any previous output has been deleted.
    try:
      os.unlink(test.smx_path)
    except:
      pass

    # Build |argv| for the compiler.
    spcomp_path = mode['spcomp']['path']
    argv = [
      spcomp_path,
      '-i', self.fix_path(spcomp_path, self.core_include_path),
      '-i', self.fix_path(spcomp_path, self.include_path),
    ]
    argv += mode['args']
    if test.warnings_are_errors:
      argv += ['-E']
    argv += [self.fix_path(spcomp_path, test.path)]

    # Run and return output.
    return self.do_exec(argv)

  def run_shells(self, mode, test):
    for shell in self.plan.shells:
      if not self.run_shell(mode, shell, test):
        return False
    return True

  def run_shell(self, mode, shell, test):
    self.out("Running with shell ({0})".format(shell['name']))
    argv = [shell['path']] + shell['args']
    argv += [self.fix_path(shell['path'], test.smx_path)]

    rc, stdout, stderr = self.do_exec(argv)
    if test.expectedReturnCode != rc:
      self.out("FAIL: Shell '{0}' returned {1}, expected {2}.".format(
        shell['name'], rc, test.expectedReturnCode))
      self.out_io(stdout, stderr)
      return False

    if test.stdout_file is not None:
      if not self.compare_output(test, 'stdout', stdout):
        return False
    if test.stderr_file is not None:
      if not self.compare_output(test, 'stderr', stderr):
        return False
        
    self.out("PASS")
    return True

  def compile_ok(self, mode, test, rc, stdout, stderr):
    assert test.type == 'compile-only'
    test_prefix = test.name.split('-')[0]
    if test_prefix == 'ok' or test_prefix == 'warn':
      if rc != 0:
        self.out("FAIL: Compile failed, return code {0} (expected 0).".format(rc))
        return False
      if not os.path.exists(test.smx_path):
        self.out("FAIL: Compile failed, binary '{0}' not found.".format(test.smx_path))
    elif test_prefix == 'fail':
      if rc == 0:
        self.out("FAIL: Compile unexpectedly succeeded, expected non-zero return code.")
        return False
      if os.path.exists(test.smx_path):
        self.out("FAIL: Compile unexpectedly succeeded, expected no .smx file.")
        return False

    if test_prefix == 'ok':
      return True
    return self.compare_spcomp_output(test, stdout)

  def do_exec(self, argv):
    if argv[0].endswith('.js'):
      argv = ['node'] + argv

    if self.plan.show_cli:
      self.out(' '.join(argv))

    return testutil.exec_argv(argv)

  def compare_output(self, test, pipe_name, actual):
    expected_lines = test.get_expected_output(pipe_name)

    # Append "\n" to everything but the last line.
    actual = actual.replace("\r\n", "\n")
    actual_lines = [line + "\n" for line in actual.split("\n")]
    actual_lines[-1] = actual_lines[-1].replace("\n", "")

    # Remove the last line from actual if it's empty and the expected is one
    # line short.
    if len(actual_lines) - 1 == len(expected_lines):
      if not len(actual_lines[-1]):
        actual_lines.pop()

    line_number = 0
    while True:
      if line_number >= len(actual_lines) and line_number < len(expected_lines):
        self.out("FAIL: Output from {0} contains unexpected data.".format(pipe_name))
        break
      if line_number < len(actual_lines) and line_number >= len(expected_lines):
        self.out("FAIL: Output from {0} is missing expected lines.".format(pipe_name))
        break
      if line_number >= len(actual_lines) and line_number >= len(expected_lines):
        break

      if expected_lines[line_number] != actual_lines[line_number]:
        self.out("FAIL: Line {0} from {1} does not match the expected output.".format(
          line_number + 1, pipe_name))
        break
      line_number += 1

    if line_number >= len(actual_lines) and line_number >= len(expected_lines):
      return True

    self.out("Expected {0}:".format(pipe_name))
    for index, line in enumerate(expected_lines):
      self.out(" Line {0:2}: {1}".format(index + 1, line.rstrip()))
    self.out("Actual {0}:".format(pipe_name))
    for index, line in enumerate(actual_lines):
      self.out(" Line {0:2}: {1}".format(index + 1, line.rstrip()))
    return False

  def compare_spcomp_output(self, test, actual_stdout):
    expected_lines = []
    with open(test.txtout_file, 'r') as fp:
      for line in fp:
        expected_lines.append(line.strip())

    for expected_line in expected_lines:
      if expected_line not in actual_stdout:
        self.out("FAIL: Expected to find the following line in stdout:")
        self.out(expected_line)
        return False
    return True

  def fix_path(self, binary, path):
    if os.path.isabs(path) and binary.endswith('.js'):
      return '/fakeroot' + path
    return path

  def print_failures(self):
    self.out("Failures were detected in the following tests:")
    for test in self.failures_:
      self.out("  {0}".format(test.unique_name))

  def out(self, text):
    when = (datetime.datetime.now() - self.start_time_).total_seconds()
    sys.stdout.write('[{0:.4f}] {1}\n'.format(when, text))
    sys.stdout.flush()

  def out_io(self, stderr, stdout):
    self.out("------------- stdout -------------")
    self.out_with_nl(stdout)
    self.out("------------- stderr -------------")
    if len(stderr.strip()) != 0:
      self.out_with_nl(stderr)
    self.out("----------------------------------")

  def out_with_nl(self, text):
    lines = text.split('\n')
    for line in lines:
      self.out(line)

if __name__ == '__main__':
  main()
