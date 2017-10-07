# vim: set ts=2 sw=2 tw=99 et ft=python:
# 
# Copyright (C) 2004-2015 AlliedModders LLC
# 
# This file is part of SourcePawn.
# 
# SourcePawn is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free
# Software Foundation, either version 3 of the License, or (at your option)
# any later version.
# 
# SourcePawn is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along with
# SourcePawn. If not, see http://www.gnu.org/licenses/.
#
import os
import sys
import argparse

def main():
  parser = argparse.ArgumentParser()
  parser.add_argument('source', type=str, help='Source tree')
  parser.add_argument('objdir', type=str, help='Object folder')
  parser.add_argument('file', type=str, help='Source file')
  parser.add_argument('out', type=str, help='Output file')
  parser.add_argument('spcomp', type=str, help='Path to spcomp')
  parser.add_argument('spshell', type=str, help='Path to spshell')
  parser.add_argument('arch', type=str, help='Binary architecture')
  args = parser.parse_args()

  with open(args.file, 'r') as infp:
    with open(args.out, 'w') as outfp:
      text = infp.read()
      outfp.write(text.format(
        source = args.source,
        spcomp = args.spcomp,
        spshell = args.spshell,
        objdir = args.objdir,
        arch = args.arch,
      ))
  os.chmod(args.out, 0o755)

if __name__ == '__main__':
  main()
