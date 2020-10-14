# vim: set ts=2 sw=2 tw=99 et:
import os
import shutil
import tempfile
import subprocess
from threading import Timer
try:
  import configparser
except:
  import ConfigParser as configparser

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

def exec_argv(argv, timeout = None, logger = None, env = None):
  if argv[0].endswith('.js'):
    argv = ['node'] + argv

  p = subprocess.Popen(argv, stdout = subprocess.PIPE, stderr = subprocess.PIPE,
                       env = env)

  def on_timeout():
    logger.out("Killing process due to timeout")
    p.kill()

  timer = None
  if timeout is not None:
    timer = Timer(timeout, on_timeout)
    timer.start()

  try:
    stdout, stderr = p.communicate()
    stdout = stdout.decode('utf-8')
    stderr = stderr.decode('utf-8')
    return p.returncode, stdout, stderr
  finally:
    if timer is not None:
      timer.cancel()
  return -9, '', 'process killed due to timeout'

def parse_manifest(path, local_folder, source = {}):
  manifest = {}
  manifest['folder'] = source.get('folder', {}).copy()

  with open(path, 'r') as fp:
    cfg = configparser.ConfigParser()
    cfg.readfp(fp)

  for section in cfg.sections():
    if section not in manifest:
      manifest[section] = {}
    for key, val in cfg.items(section):
      manifest[section][key] = val

  for entry in manifest:
    includes = manifest[entry].get('includes', None)
    if includes and isinstance(includes, str):
      includes = [include.strip() for include in includes.split(',')]
      includes = [os.path.join(local_folder, include) for include in includes]
      manifest[entry]['includes'] = includes

  return manifest

def manifest_get(manifest, filename, key, default_value = None):
  if filename in manifest:
    if key in manifest[filename]:
      return manifest[filename][key]
  if 'folder' in manifest:
    if key in manifest['folder']:
      return manifest['folder'][key]
  return default_value
