# vim: set ts=2 sw=2 tw=99 et:
import os, sys
import argparse
import subprocess

def main():
  parser = argparse.ArgumentParser()
  parser.add_argument('-v', '--verbose', action='store_true', default=False,
                      help="Enable verbose mode")
  parser.add_argument('-d', '--validate_debug_sections', action='store_true', default=False,
                      help="Validate debug sections as well")
  parser.add_argument('verifier', type=str, help="Verifier path")
  parser.add_argument('folder', type=str, help="Folder with files to verify")
  args = parser.parse_args()

  env = None
  if args.verbose:
    env = os.environ.copy()
    env['VERBOSE'] = '1'
  if args.validate_debug_sections:
    if env == None:
      env = os.environ.copy()
    env['VALIDATE_DEBUG_SECTIONS'] = '1'

  verified_files = 0
  failed_files = 0
  for file in os.listdir(args.folder):
    path = os.path.join(args.folder, file)
    argv = [
      args.verifier,
      path,
    ]
    if args.verbose:
      print('####################')
      print('{0}'.format(' '.join(argv)))
    p = subprocess.Popen(
      argv,
      stdout = subprocess.PIPE,
      stderr = subprocess.PIPE,
      env = env)
    stdout, stderr = p.communicate()
    stdout = DecodeConsoleText(sys.stdout, stdout)
    stderr = DecodeConsoleText(sys.stderr, stderr)
    sys.stdout.write(stdout)
    sys.stderr.write(stderr)

    verified_files += 1

    if p.returncode != 0:
      print('failed to verify {}'.format(file))
      failed_files += 1
  print('Done. {}/{} plugins passed verification. {} ({:.02f}%) failed.'.format(verified_files-failed_files, verified_files, failed_files, failed_files/verified_files*100.0))

def DecodeConsoleText(origin, text):
  try:
    if origin.encoding:
      return text.decode(origin.encoding, 'replace')
  except:
    pass
  try:
    return text.decode(locale.getpreferredencoding(), 'replace')
  except:
    pass
  return text.decode('utf8', 'replace')

if __name__ == '__main__':
  main()
