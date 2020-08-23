# vim: set ts=2 sw=2 tw=99 et:
import argparse
import datetime
import os
import platform
import re
import subprocess
import sys
import testutil
from testutil import manifest_get

def main():
  parser = argparse.ArgumentParser()
  parser.add_argument('objdir', type=str, help='Build folder to test.')
  parser.add_argument('test', type=str, nargs='?', default=None,
                      help="Optional test folder or test file")
  parser.add_argument('--arch', type=str, default=None,
                      help="Force testing a specific arch on dual-arch builds.")
  parser.add_argument('--show-cli', default=False, action='store_true',
                      help='Show the command-line invocation of each test.')
  parser.add_argument('--spcomp2', default=False, action='store_true',
                      help="Only test using spcomp2.")
  parser.add_argument('--compile-only', default=False, action='store_true',
                      help="Skip execution on tests with runtime components.")
  parser.add_argument('--runtime-only', default=False, action='store_true',
                      help="Skip tests that are marked as compile-only.")
  parser.add_argument('--coverage', default=None, type=str,
                      help="Path to store code coverage data.")
  parser.add_argument('--spcomp-arg', default=None, type=str, action='append',
                      dest='spcomp_args',
                      help="Add an extra argument to all spcomp invocations.")
  args = parser.parse_args()

  if args.test and args.test.startswith('tests/'):
    args.test = args.test[6:]

  plan = TestPlan(args)
  plan.find_compilers()
  if not args.compile_only:
    plan.find_shells()
  plan.find_tests()

  if not len(plan.modes):
    raise Exception('No compiler binaries were found in {0}'.format(args.objdir))
  if not len(plan.tests):
    raise Exception('No matching tests were found.')
  if not len(plan.shells) and not args.compile_only:
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
    self.env_ = None

    if self.args.coverage:
      self.env_ = os.environ.copy()

  @property
  def show_cli(self):
    return self.args.show_cli

  def match_arch(self, arch):
    if self.args.arch is None:
      return True
    if self.args.arch == 'x64' or self.args.arch == 'x86_64':
      return arch == 'x86_64'
    return self.args.arch == arch

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

  def find_executables_in(self, path, name):
    kPlatformNames = {
        'Linux': 'linux',
        'Darwin': 'mac',
        'Windows': 'windows',
    }

    found = []
    for subdir in os.listdir(path):
      parts = subdir.split('-')
      if len(parts) < 2:
        continue
      our_platform = kPlatformNames.get(platform.system(), platform.system())
      if parts[0] != our_platform:
        continue
      if not self.match_arch(parts[1]):
        continue

      prefix = os.path.join(path, subdir, name)
      full_path = self.find_executable(prefix)
      if not full_path:
        continue
      found.append((parts[1], os.path.abspath(full_path)))
    return found

  def find_shells(self):
    search_in = os.path.join(self.args.objdir, 'vm', 'spshell')
    found = self.find_executables_in(search_in, 'spshell')

    for arch, path in found:
      env = None
      if self.args.coverage:
        env = self.env_.copy()
        env['LLVM_PROFILE_FILE'] = '{0}/spshell-%9m'.format(self.args.coverage)

      rc, stdout, stderr = testutil.exec_argv([path, '--version'])
      if rc == 0 and 'JIT' in stdout:
        self.shells.append({
          'path': path,
          'args': [],
          'name': 'default-' + arch,
          'env': env,
          })

      self.shells.append({
        'path': path,
        'args': ['--disable-jit'],
        'name': 'interpreter-' + arch,
        'env': env,
      })

  def find_compilers(self):
    if self.args.spcomp2:
      self.find_spcomp2()
    else:
      self.find_spcomp()

  def find_spcomp(self):
    search_in = os.path.join(self.args.objdir, 'compiler', 'spcomp')
    found = self.find_executables_in(search_in, 'spcomp')

    for arch, path in found:
      env = None
      if self.args.coverage:
        env = self.env_.copy()
        env['LLVM_PROFILE_FILE'] = '{0}/spcomp-%9m'.format(self.args.coverage)

      spcomp = {
        'path': os.path.abspath(path),
        'arch': arch,
        'name': 'spcomp',
        'args': [],
        'env': env,
      }

      if self.args.spcomp_args:
        spcomp['args'].extend(self.args.spcomp_args)

      self.modes.append({
        'name': 'default',
        'spcomp': spcomp,
        'args': [],
      })

      if path.endswith('.js'):
        # Emscripten takes a long time to run, so we only test the default
        # configuration.
        continue

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

  def find_spcomp2(self):
    for arch in self.arch_suffixes:
      if not self.match_arch(arch):
        continue

      path = os.path.join(self.args.objdir, 'exp', 'compiler', 'spcomp2' + arch, 'spcomp2')

      if not os.path.exists(path):
        if not os.path.exists(path + '.js'):
          continue
        path += '.js'

      spcomp2 = {
        'path': os.path.abspath(path),
        'arch': arch,
        'name': 'spcomp2',
        'args': [
          '--show-ast=false',
          '--show-sema=false',
          '--pool-stats=false',
        ],
      }

      self.modes.append({
        'name': 'default',
        'spcomp': spcomp2,
        'args': [],
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
      manifest = testutil.parse_manifest(manifest_path, local_folder, manifest)
      if manifest_get(manifest, 'folder', 'skip') == 'true':
        return

    for name in os.listdir(folder):
      local_path = os.path.join(local_folder, name)
      path = os.path.join(self.tests_path, local_path)
      if os.path.isdir(path):
        self.find_tests_impl(local_path, manifest)
      elif path.endswith('.sp'):
        if self.args.test is not None and not local_path.startswith(self.args.test):
          continue

        test = Test(**{
          'path': os.path.abspath(path),
          'manifest': manifest,
        })
        if manifest_get(manifest, test.name, 'skip') == 'true':
          continue
        self.tests.append(test)

###
# This is a helper class instantiated for each test file in the tree.
###
class Test(object):
  ManifestKeys = set([
    'returnCode',
    'warnings_are_errors',
    'compiler',
    'force_old_parser',
    'force_new_parser',
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
    return manifest_get(self.manifest_, self.name, 'type', 'runtime')

  @property
  def includes(self):
    return manifest_get(self.manifest_, self.name, 'includes', [])

  @property
  def warnings_are_errors(self):
    return self.local_manifest_.get('warnings_are_errors', None) == 'true'

  @property
  def force_old_parser(self):
    return self.local_manifest_.get('force_old_parser', None) == 'true'

  @property
  def force_new_parser(self):
    return self.local_manifest_.get('force_new_parser', None) == 'true'

  @property
  def expectedReturnCode(self):
    if 'returnCode' in self.local_manifest_:
      return int(self.local_manifest_['returnCode'])
    return 0

  def should_run(self, mode):
    compiler = self.local_manifest_.get('compiler', None)
    if compiler is None:
      compiler = manifest_get(self.manifest_, self.name, 'compiler')
    if compiler is None:
      return True
    return mode['spcomp']['name'] == compiler

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
    value = m.group(2).strip()
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
      if not test.should_run(mode):
        continue
      if not self.run_test(mode, test):
        self.failures_.add(test)

  def should_compile_only(self, test):
    if test.type == 'compiler-output' or test.type == 'compile-only':
      return True
    return self.plan.args.compile_only

  def run_test(self, mode, test):
    compile_only = self.should_compile_only(test)
    if compile_only and self.plan.args.runtime_only:
      return True

    self.out('Begin test {0}'.format(test.path))

    # First run the compiler.
    rc, stdout, stderr = self.run_compiler(mode, test)
    if compile_only:
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
    argv = [spcomp_path]

    # Add test-specific includes.
    for include in test.includes:
      include_path = self.fix_path(spcomp_path, os.path.join(self.include_path, include))
      argv += ['-i', include_path]

    # Add harness includes.
    argv += [
      '-i', self.fix_path(spcomp_path, self.core_include_path),
      '-i', self.fix_path(spcomp_path, self.include_path),
    ]

    argv += mode['spcomp']['args']
    argv += mode['args']
    argv += ['-z', '1'] # Fast compilation for tests.
    if test.warnings_are_errors:
      argv += ['-E']
    if mode['spcomp']['name'] == 'spcomp2':
      argv += ['-o', test.smx_path]
    argv += [self.fix_path(spcomp_path, test.path)]

    # Run and return output.
    return self.do_exec(argv, env = mode['spcomp']['env'])

  def run_shells(self, mode, test):
    for shell in self.plan.shells:
      if not self.run_shell(mode, shell, test):
        return False
    return True

  def run_shell(self, mode, shell, test):
    self.out("Running with shell ({0})".format(shell['name']))
    argv = [shell['path']] + shell['args']
    argv += [self.fix_path(shell['path'], test.smx_path)]

    rc, stdout, stderr = self.do_exec(argv, shell['env'])
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
    if test.type != 'compiler-output':
      return rc == 0

    assert test.type == 'compiler-output'
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

  def do_exec(self, argv, env = None):
    if self.plan.show_cli:
      self.out(' '.join(argv))

    if argv[0].endswith('.js'):
      timeout = 60
    else:
      timeout = 5

    return testutil.exec_argv(argv, timeout, logger = self, env = env)

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
    failures = sorted([test.unique_name for test in self.failures_])
    for test in failures:
      test_path = os.path.join(os.path.split(__file__)[0], test)
      self.out("  {0}".format(os.path.relpath(test_path)))

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
