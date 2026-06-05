# vim: set sts=2 sw=2 tw=99 et:
import argparse
import csv
import os
import sys
import time

import texttable

import testutil


def get_supported_architectures_and_shells(objdir):
  spshell_dir = os.path.join(objdir, 'spshell')
  found = testutil.find_executables_in(spshell_dir, 'spshell')

  architectures = []
  spshells = {}
  for arch, path in found:
    architectures.append(arch)
    spshells[arch] = path
  return sorted(architectures), spshells


def find_any_spcomp(objdir):
  spcomp_dir = os.path.join(objdir, 'spcomp')
  found = testutil.find_executables_in(spcomp_dir, 'spcomp')
  if found:
    return found[0][1]
  return None


def find_benchmark_tests(tests_path, filter_str=None):
  tests = []
  for folder in os.listdir(tests_path):
    sub_folder = os.path.join(tests_path, folder)
    if not os.path.isdir(sub_folder):
      continue
    manifest_path = os.path.join(sub_folder, 'manifest.ini')
    if not os.path.exists(manifest_path):
      continue

    manifest = testutil.parse_manifest(manifest_path, folder, {})
    if testutil.manifest_get(manifest, 'folder', 'type') != 'benchmark':
      continue

    if 'folder' in manifest and 'skip' in manifest['folder']:
      manifest['folder']['skip'] = 'false'
    for root, dirs, files in os.walk(sub_folder):
      current_manifest_path = os.path.join(root, 'manifest.ini')
      if root == sub_folder:
        current_manifest = manifest
      elif os.path.exists(current_manifest_path):
        current_manifest = testutil.parse_manifest(
            current_manifest_path, os.path.relpath(root, tests_path), manifest
        )
      else:
        current_manifest = manifest

      if testutil.manifest_get(current_manifest, 'folder', 'skip') == 'true':
        continue

      if testutil.manifest_get(current_manifest, 'folder', 'type') != 'benchmark':
        continue

      for file in files:
        if file.endswith('.sp') or file.endswith('.smx'):
          path = os.path.join(root, file)
          if testutil.manifest_get(current_manifest, file, 'skip') == 'true':
            continue
          if filter_str and filter_str not in path:
            continue
          tests.append({
              'path': os.path.abspath(path),
              'name': file,
              'manifest': current_manifest,
          })

  tests.sort(key=lambda t: t['path'])
  return tests


def map_arch_name(arch):
  if arch == 'x86_64':
    return 'x64'
  return arch


def run_benchmark_test(
    test, spcomp, architectures, spshell_configs, spshells, tests_path, iterations
):
  test_path = test['path']
  test_name = test['name']
  test_basename = os.path.splitext(test_name)[0]

  print(f'Running benchmark {test_basename}...', file=sys.stderr)

  smx_name = os.path.splitext(test_name)[0] + '.smx'
  if os.path.exists(smx_name):
    try:
      os.unlink(smx_name)
    except Exception:
      pass

  core_include_path = os.path.abspath(
      os.path.join(tests_path, '../include')
  )
  comp_argv = [spcomp]
  includes = testutil.manifest_get(
      test['manifest'], test_name, 'includes', []
  )
  if isinstance(includes, str):
    includes = [includes]
  for inc in includes:
    comp_argv += ['-i', inc]

  comp_argv += [
      '-i',
      core_include_path,
      '-i',
      tests_path,
      '-z',
      '1',
  ]
  comp_argv += [test_path]

  rc, stdout, stderr = testutil.exec_argv(comp_argv)
  if rc != 0:
    print(
        f"Compilation failed for {test_name} with command: {' '.join(comp_argv)}",
        file=sys.stderr,
    )
    print(stdout, file=sys.stderr)
    print(stderr, file=sys.stderr)

    row_results = {'Test': test_basename}
    for arch in architectures:
      for config in spshell_configs[arch]:
        col_name = f'{map_arch_name(arch)}-{config} (ms)'
        row_results[col_name] = 'N/A'
    return row_results

  smx_path = os.path.abspath(smx_name)

  row_results = {'Test': test_basename}

  for arch in architectures:
    shell_path = spshells[arch]
    configs = spshell_configs[arch]
    for config in configs:
      run_argv = [shell_path]
      run_env = os.environ.copy()

      if config == 'interp':
        run_env['DISABLE_JIT'] = '1'
        run_argv += ['--disable-watchdog']

      run_argv += [smx_path]

      total_time_ms = 0.0
      failed = False
      for _ in range(iterations):
        t0 = time.monotonic()
        run_rc, run_stdout, run_stderr = testutil.exec_argv(
            run_argv, env=run_env
        )
        t1 = time.monotonic()

        if run_rc != 0:
          print(
              f'Error: {test_basename} failed to run on {arch}-{config} (exit code {run_rc})',
              file=sys.stderr,
          )
          print(run_stdout, file=sys.stderr)
          print(run_stderr, file=sys.stderr)
          failed = True
          break

        total_time_ms += (t1 - t0) * 1000.0

      col_name = f'{map_arch_name(arch)}-{config} (ms)'
      if failed:
        row_results[col_name] = 'N/A'
      else:
        avg_time_ms = total_time_ms / iterations
        if avg_time_ms >= 0:
          elapsed_val = int(round(avg_time_ms))
        else:
          elapsed_val = avg_time_ms
        row_results[col_name] = elapsed_val

  return row_results


def main():
  parser = argparse.ArgumentParser()
  parser.add_argument('objdir', type=str, help='Build folder to benchmark.')
  parser.add_argument(
      '--filter',
      default=None,
      type=str,
      help='Filter for tests with a particular name.',
  )
  parser.add_argument(
      '--iterations',
      default=5,
      type=int,
      help='Number of times to run each benchmark test.',
  )

  group = parser.add_mutually_exclusive_group()
  group.add_argument(
      '--pretty', action='store_true', help='Output as ASCII table.'
  )
  group.add_argument('--csv', action='store_true', help='Output as CSV.')

  args = parser.parse_args()

  if not args.csv:
    args.pretty = True

  tests_path = os.path.dirname(os.path.abspath(__file__))
  tests = find_benchmark_tests(tests_path, args.filter)
  if not tests:
    print('No benchmark tests found.', file=sys.stderr)
    sys.exit(1)

  spcomp = find_any_spcomp(args.objdir)
  if not spcomp:
    print(f'Error: No spcomp binary found in {args.objdir}', file=sys.stderr)
    sys.exit(1)

  architectures, spshells = get_supported_architectures_and_shells(args.objdir)
  if not architectures:
    print(
        f'Error: No supported architectures or spshell binaries found in {args.objdir}',
        file=sys.stderr,
    )
    sys.exit(1)

  spshell_configs = {}
  for arch in architectures:
    shell_path = spshells[arch]
    rc, stdout, stderr = testutil.exec_argv([shell_path, '--version'])
    output_str = stdout + stderr
    if 'JIT' in output_str:
      spshell_configs[arch] = ['interp', 'jit']
    else:
      spshell_configs[arch] = ['interp']

  results = []
  with testutil.TempFolder() as temp_folder:
    with testutil.ChangeFolder(temp_folder):
      for test in tests:
        results.append(
            run_benchmark_test(
                test,
                spcomp,
                architectures,
                spshell_configs,
                spshells,
                tests_path,
                args.iterations,
            )
        )

  headers = ['Test']
  for arch in architectures:
    for config in spshell_configs[arch]:
      headers.append(f'{map_arch_name(arch)}-{config} (ms)')

  if args.csv:
    writer = csv.writer(sys.stdout)
    writer.writerow(headers)
    for row in results:
      writer.writerow([row.get(col, '') for col in headers])
  else:
    table = texttable.Texttable(max_width=0)
    table.set_cols_align(['l'] + ['r'] * (len(headers) - 1))
    rows = [headers]
    for row in results:
      rows.append([row.get(col, '') for col in headers])
    table.add_rows(rows)
    print(table.draw())


if __name__ == '__main__':
  main()
