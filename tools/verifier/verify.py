# vim: set ts=2 sw=2 tw=99 et:
import os, sys
import argparse
import subprocess

def main():
  parser = argparse.ArgumentParser()
  parser.add_argument('-v', '--verbose', action='store_true', default=False,
                      help="Enable verbose mode")
  parser.add_argument('verifier', type=str, help="Verifier path")
  parser.add_argument('folder', type=str, help="Folder with files to verify")
  args = parser.parse_args()

  env = None
  if args.verbose:
    env = os.environ.copy()
    env['VERBOSE'] = '1'

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

    if p.returncode != 0:
      raise Exception('failed to verify')

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
