#################################################################################
#                                                                               #
# Copyright (c) 2007 Luca Bruno                                                 #
#                                                                               #
# This file is part of Smalltalk YX.                                            #
#                                                                               #
# Permission is hereby granted, free of charge, to any person obtaining a copy  #
# of this software and associated documentation files (the "Software"), to deal #
# in the Software without restriction, including without limitation the rights  #
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell     #
# copies of the Software, and to permit persons to whom the Software is         #
# furnished to do so, subject to the following conditions:                      #
#                                                                               #
# The above copyright notice and this permission notice shall be included in    #
# all copies or substantial portions of the Software.                           #
#                                                                               #
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR    #
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,      #
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE   #
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER        #
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING       #
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER           #
# DEALINGS IN THE SOFTWARE.                                                     #
#                                                                               #
#################################################################################

import os, glob

env = Environment ()

opts = Options()

if env['PLATFORM'] == 'win32':
   opts.AddOptions (PathOption('prefix',
                              'Installation prefix',
                              'C:\\\\Syx', []))
   env['bindir'] = env['datadir'] = env['libdir'] = '$prefix'
   env['imagepath'] = '$prefix\\\\default.sim'
   env['includedir'] = '$prefix\\\\include'
else:
   opts.AddOptions (
      PathOption('prefix', 
                 'Installation prefix', 
                 '/usr/local', []),
      PathOption('exec_prefix',
                 'Installation prefix for executables and object code libraries',
                 '$prefix', []),
      PathOption('bindir', 
                 'Installation prefix for user executables', 
                 '$exec_prefix/bin', []),
      PathOption('datadir',
                 'Installation prefix for packages',
                 '$prefix/share/syx', []),
      PathOption('imagepath',
                 'Installation path for the default binary image',
                 '$datadir/default.sim', []),
      PathOption('libdir',
                 'Installation prefix for object code libraries', 
                 '$exec_prefix/lib', []),
      PathOption('includedir',
                 'Installation prefix for C header files', 
                 '$prefix/include', []))

opts.AddOptions (
   BoolOption ('plugins', """Build with plugins support""", True),
   BoolOption ('bignum', """Build with infinite-precision numbers (needs gmp library)""", True),
   BoolOption ('attach', """Attach a debugger for test failures""", False),              
   EnumOption ('debug', """Debug output and symbols""", 'normal',
               allowed_values=('no', 'normal', 'info', 'full'),
               ignorecase=True),

   BoolOption ('GTK', """Build the syx-gtk plugin to support graphical user interfaces""", True),
   BoolOption ('READLINE', """Build the syx-readline plugin to add more console features""", True))

opts.Update (env)

# My 64bit LFS keep its toolchain in /tools64 and set LD_LIBRARY_PATH for tests
if env['PLATFORM'] == 'posix':
   env['ENV']['PATH'] = '/tools64/bin:' + env['ENV']['PATH']

# Specify the alternative toolchain for Windows
env['tools'] = ['default', 'mingw']
   
# Custimize the help message
env.Help (opts.GenerateHelpText (env) + """
     Type: 'scons'               to build Syx.
           'scons plugins=no'    build without plugins support
           'scons debug=no'      release build with high optimization.
           'scons debug=normal'  add debug symbols (default).
           'scons debug=info' 	 display more messages.
           'scons debug=full'    trace the entire execution stack of Smalltalk.
           'scons test'          to test Syx.
           'scons test attach=yes'
                                 to test Syx and attach a debugger if a
                                 test fails

           'scons doc'           to create reference documentation (requires Doxygen).
           'scons install'       to install Syx.
           'scons sdist'         to create a directory with source distribution.
	   'scons bdist'	 to create a directory with binary distribution
                                 (implies 'scons install', doesn't work under Win32).
     """)

# Configuration
conf = Configure (env, config_h="syx/syx-config.h")

print 'Mandatory headers...'

for h in ['string.h', 'stdint.h', 'unistd.h', 'sys/stat.h', 'time.h', 'stdio.h', 'assert.h', 'fcntl.h',
          'sys/types.h', 'errno.h', 'getopt.h']:
   if not conf.CheckCHeader (h):
      print "Can't build Syx without %s header!" % h
      env.Exit (1)

print
print 'Optional headers...'

for h in ['stdarg.h']:
   conf.CheckCHeader (h)
for t in ['int64_t']:
   conf.CheckType (t, '#include <stdint.h>', 'c')

if env['PLATFORM'] == 'win32':
   have_windows_h = conf.CheckCHeader ('windows.h')

print
print 'Mandatory functions...'

for f in ['strtol', 'strtof', 'strtod', 'gettimeofday', 'getopt']:
   if not conf.CheckFunc (f):
      print "Can't build Syx without %s function!" % f
      env.Exit (1)

if env['PLATFORM'] == 'win32':
   if not conf.CheckLibWithHeader ('wsock32', 'winsock2.h', 'c', 'select(0, NULL, NULL, NULL, NULL);'):
      print "Can't build Syx without select function!"
      env.Exit (1)
else:
   if not conf.CheckFunc ('select'):
      print "Can't build Syx without select function!"
      env.Exit (1)

print
print 'Optional functions...'

for f in ['strndup']:
   conf.CheckFunc (f)

if env['bignum']:
   if not conf.CheckLibWithHeader ('gmp', 'gmp.h', 'c', 'mpz_t t; mpz_init (t); mpz_clear (t);'):
      print "WARNING: gmp library not found. Multiple-precision numbers won't be supported"

if env['plugins']:
    if (env['PLATFORM'] == 'win32' and have_windows_h) or conf.CheckLibWithHeader ('dl', 'dlfcn.h', 'c', 'dlopen(0, 0);'):
        env.MergeFlags ('-DWITH_PLUGINS')
    else:
        print 'WARNING: building without plugins support'
        env['plugins'] = False

conf.Finish ()

# Flags
env.MergeFlags ('-Wall -Wno-strict-aliasing -std=c99 -U__STRICT_ANSI__ -I#.')
if env['PLATFORM'] == 'darwin':
   env.MergeFlags ('-fno-common')

if env['PLATFORM'] == 'win32':
   env.MergeFlags ('-DROOT_PATH="." -DIMAGE_PATH="default.sim"')
else:
   env.MergeFlags ('-DROOT_PATH="$datadir" -DIMAGE_PATH="$imagepath"')
if env['debug'] == 'no':
   env.MergeFlags ('-O3')
elif env['debug'] == 'normal':
   env.MergeFlags ('-g -O2')
elif env['debug'] == 'info':
   env.MergeFlags ('-g -O -DSYX_DEBUG_INFO')
elif env['debug'] == 'full':
   env.MergeFlags ('-g -O -DSYX_DEBUG_INFO -DSYX_DEBUG_FULL')

if env['PLATFORM'] == 'win32':
   env.MergeFlags ('-DWINDOWS')

# Doc builder

env.Alias ('doc', env.Command ('build/docs', 'Doxyfile',
                               'doxygen $SOURCES'))
env.Clean ('doc', 'build/doc')

distdir = '#syx-0.1.2'

# Installation

def builder_syxinstall (target, sources):
   t1 = env.Install (target, sources)
   env.Alias ('install', t1)
   if env['PLATFORM'] != 'win32':
      t = env.Install (os.path.join (distdir, target), sources)
      env.Alias ('bdist', t)
   return t1

setattr (env, 'SyxInstall', builder_syxinstall)

# Build

env.MergeFlags ('-L#build/lib')
env.BuildDir ('build/lib', 'syx', False)
env.SConscript (dirs=['build/lib'], exports=['env', 'distdir'])

env.BuildDir ('build/bin', 'src', False)
env.SConscript (dirs=['build/bin'], exports=['env', 'distdir'])

env.BuildDir ('build/plugins', 'plugins', False)
env.SConscript (dirs=['build/plugins'], exports=['env', 'distdir'])

env.SConscript (dirs=['tests'], exports=['env', 'distdir'])

# Install data

sources = glob.glob ('st/kernel/*.st')
path = os.path.join (env['datadir'], 'st', 'kernel')
env.SyxInstall (path, sources)

# Source distribution

path = os.path.join (distdir, 'st', 'kernel')
target = env.Install (path, sources)
env.Alias ('sdist', target)

sources = ['INSTALL', 'README', 'AUTHORS', 'ChangeLog', 'COPYING', 'SConstruct', 'TODO', 'NEWS', 'Doxyfile']
target = env.Install (distdir, sources)
env.Alias ('sdist', target)

target = env.Install (os.path.join (distdir, 'doc', 'html', 'extras'), '#doc/html/extras/footer.html')
env.Alias ('sdist', target)