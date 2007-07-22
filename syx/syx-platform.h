/* 
   Copyright (c) 2007 Luca Bruno

   This file is part of Smalltalk YX.

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell   
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:
   
   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.
   
   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER    
   DEALINGS IN THE SOFTWARE.
*/

#ifndef SYX_PLATFORM_H
#define SYX_PLATFORM_H

#include "syx-config.h"

#define _SYX_XSTRINGIFY(s) #s
#define SYX_STRINGIFY(s) _SYX_XSTRINGIFY(s)

#define SYX_IMAGE_PATH SYX_STRINGIFY(IMAGE_PATH)
#define SYX_ROOT_PATH SYX_STRINGIFY(ROOT_PATH)

/* Version */

#define SYX_VERSION "0.1.3"

/* Byte swapping */

#ifdef HAVE_BYTESWAP_H
#include <byteswap.h>
#define syx_bswap_16(x) bswap_16(x)
#define syx_bswap_32(x) bswap_32(x)
#else
#define syx_bswap_16(x)					     \
  ((((x) & 0xff00) >> 8) | (((x) & 0x00ff) << 8))
#define syx_bswap_32(x)					     \
  ((((x) & 0xff000000) >> 24) | (((x) & 0x00ff0000) >> 8) |  \
   (((x) & 0x0000ff00) << 8) | (((x) & 0x000000ff) << 24))
#endif /* HAVE_BYTESWAP_H */

/* Swap for big endian machines */

#ifdef HAVE_BIG_ENDIANNESS
#define SYX_COMPAT_SWAP_16(x) syx_bswap_16(x)
#define SYX_COMPAT_SWAP_32(x) syx_bswap_32(x)
#else
#define SYX_COMPAT_SWAP_16(x) (x)
#define SYX_COMPAT_SWAP_32(x) (x)
#endif

/* Some platform specific informations */

#ifdef WINDOWS

 #define SYX_PATH_SEPARATOR '\\'

 #ifdef BUILD_DLL

 // the dll exports
  #define _SYX_EXPORT __declspec(dllexport)

 #else

 // the exe imports
  #define _SYX_EXPORT __declspec(dllimport)

 #endif /* BUILD_DLL */

#else

 #define SYX_PATH_SEPARATOR '/'

 #define _SYX_EXPORT

#endif /* WINDOWS */

#endif /* SYX_PLATFORM_H */