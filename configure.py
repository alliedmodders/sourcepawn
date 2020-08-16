# vim: set ts=2 sw=2 tw=99 noet ft=python: 
# 
# Copyright (C) 2004-2012 David Anderson
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
import sys
try:
	from ambuild2 import run, util
except:
	try:
		import ambuild
		sys.stderr.write('It looks like you have AMBuild 1 installed, but this project uses AMBuild 2.\n')
		sys.stderr.write('Upgrade to the latest version of AMBuild to continue.\n')
	except:
		sys.stderr.write('AMBuild must be installed to build this project.\n')
		sys.stderr.write('http://www.alliedmods.net/ambuild\n')
	sys.exit(1)

# Hack to show a decent upgrade message, which wasn't done until 2.2.
ambuild_version = getattr(run, 'CURRENT_API', '2.1')
if ambuild_version.startswith('2.1'):
	sys.stderr.write("AMBuild 2.2 or higher is required; please update\n")
	sys.exit(1)

parser = run.BuildParser(sourcePath=sys.path[0], api='2.2')
parser.options.add_argument('--enable-debug', action='store_const', const='1', dest='debug',
                            help='Enable debugging symbols')
parser.options.add_argument('--enable-optimize', action='store_const', const='1', dest='opt',
                            help='Enable optimization')
parser.options.add_argument('--amtl', type=str, dest='amtl', default=None, help='Custom AMTL path')
parser.options.add_argument('--build', type=str, dest='build', default='all', 
                            help='Build which components (all, spcomp, vm, exp, test, core)')
parser.options.add_argument('--enable-spew', action='store_true', default=False, dest='enable_spew',
                            help='Enable debug spew')
parser.options.add_argument("--enable-coverage", action='store_true', default=False,
                            dest='enable_coverage', help='Enable code coverage support.')
parser.options.add_argument("--targets", type=str, default=None,
                            help='Specify target architecture (use commas to include more than one)')
parser.Configure()
