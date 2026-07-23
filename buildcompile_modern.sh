#!/bin/bash
set -e

cd "$(dirname "$0")"

C_OBJDIR=/tmp/out/FreeArc/c-objects
HS_OBJDIR=/tmp/out/FreeArc/hs-objects

rm -rf "$C_OBJDIR" "$HS_OBJDIR"
mkdir -p "$C_OBJDIR" "$HS_OBJDIR"

echo "=== Compiling Lua C sources ==="
for f in HsLua/src/*.c; do
  gcc -c "$f" -o "$C_OBJDIR/$(basename "$f" .c).o" \
    -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -I HsLua/src
done

echo "=== Compiling FreeArc compression C++ library ==="
cp unix-common.mak common.mak

# Compression core
g++ -c -fno-exceptions -fno-rtti -Os -fomit-frame-pointer -ffast-math -fstrict-aliasing \
  -g0 -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER \
  Compression/Common.cpp -o "$C_OBJDIR/Common.o"
g++ -c -fno-exceptions -fno-rtti -Os -fomit-frame-pointer -ffast-math -fstrict-aliasing \
  -g0 -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER \
  Compression/CompressionLibrary.cpp -o "$C_OBJDIR/CompressionLibrary.o"
g++ -c -fno-exceptions -fno-rtti -Os -fomit-frame-pointer -ffast-math -fstrict-aliasing \
  -g0 -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -fexceptions \
  Compression/CELS.cpp -o "$C_OBJDIR/CELS.o"

# Tornado
g++ -c -fno-exceptions -fno-rtti -O3 -fomit-frame-pointer -ffast-math -fstrict-aliasing \
  -g0 -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
  Compression/Tornado/C_Tornado.cpp -o "$C_OBJDIR/C_Tornado.o"

# LZP
g++ -c -fno-exceptions -fno-rtti -O3 -fomit-frame-pointer -fstrict-aliasing \
  -g0 -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER \
  Compression/LZP/C_LZP.cpp -o "$C_OBJDIR/C_LZP.o"

# REP
g++ -c -fno-exceptions -fno-rtti -O3 -fomit-frame-pointer -fstrict-aliasing \
  -g0 -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER \
  Compression/REP/C_REP.cpp -o "$C_OBJDIR/C_REP.o"
g++ -c -fno-exceptions -fno-rtti -O3 -fomit-frame-pointer -fstrict-aliasing \
  -g0 -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -fexceptions \
  Compression/REP/cels-rep.cpp -o "$C_OBJDIR/cels-rep.o"

# Delta
g++ -c -fno-exceptions -fno-rtti -O3 -fomit-frame-pointer -fstrict-aliasing \
  -g0 -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER \
  Compression/Delta/C_Delta.cpp -o "$C_OBJDIR/C_Delta.o"

# External
g++ -c -fno-exceptions -fno-rtti -Os -fomit-frame-pointer -fstrict-aliasing \
  -g0 -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER \
  Compression/External/C_External.cpp -o "$C_OBJDIR/C_External.o"

# Encryption
g++ -c -fno-exceptions -fno-rtti -O3 -fomit-frame-pointer -fstrict-aliasing \
  -g0 -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -I Compression/_Encryption/headers \
  Compression/_Encryption/C_Encryption.cpp -o "$C_OBJDIR/C_Encryption.o"

# GRZip
g++ -c -fno-exceptions -fno-rtti -O2 -fomit-frame-pointer -ffast-math -fstrict-aliasing \
  -g0 -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER \
  Compression/GRZip/C_GRZip.cpp -o "$C_OBJDIR/C_GRZip.o"

# LZMA (7zip-based) - needs exceptions enabled for 7zip code
g++ -c -fexceptions -fno-rtti -O2 -fomit-frame-pointer -ffast-math -fstrict-aliasing \
  -g0 -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER \
  Compression/LZMA/C_LZMA.cpp -o "$C_OBJDIR/C_LZMA.o"
g++ -c -fno-exceptions -fno-rtti -O2 -fomit-frame-pointer -ffast-math -fstrict-aliasing \
  -g0 -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER \
  Compression/LZMA/C_BCJ.cpp -o "$C_OBJDIR/C_BCJ.o"

# PPMD
g++ -c -fno-exceptions -fno-rtti -O1 -fomit-frame-pointer -ffast-math -fstrict-aliasing \
  -g0 -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER \
  Compression/PPMD/C_PPMD.cpp -o "$C_OBJDIR/C_PPMD.o"

# MM/TTA
g++ -c -fno-exceptions -fno-rtti -O3 -fomit-frame-pointer -fstrict-aliasing \
  -g0 -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER \
  Compression/MM/C_MM.cpp -o "$C_OBJDIR/C_MM.o"
g++ -c -fno-exceptions -fno-rtti -O3 -fomit-frame-pointer -fstrict-aliasing \
  -g0 -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER \
  Compression/MM/C_TTA.cpp -o "$C_OBJDIR/C_TTA.o"

# Dict
g++ -c -fno-exceptions -fno-rtti -O3 -fomit-frame-pointer -fstrict-aliasing \
  -g0 -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER \
  Compression/Dict/C_Dict.cpp -o "$C_OBJDIR/C_Dict.o"

# Root-level C++
g++ -c -fno-exceptions -fno-rtti -O3 -fomit-frame-pointer -ffast-math -fstrict-aliasing \
  -g0 -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER \
  Environment.cpp -o "$C_OBJDIR/Environment.o"
g++ -c -fno-exceptions -fno-rtti -O3 -fomit-frame-pointer -ffast-math -fstrict-aliasing \
  -g0 -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER \
  URL.cpp -o "$C_OBJDIR/URL.o"

echo "=== Compiling Haskell ==="
hsc2hs Compression/_TABI/tabi.hsc

ghc --make Arc.hs \
  -i. -iCompression -iCompression/_TABI -iHsLua/src -threaded \
  -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER \
  -optc-DFREEARC_UNIX -optc-DFREEARC_INTEL_BYTE_ORDER \
  -odir "$HS_OBJDIR" -hidir "$HS_OBJDIR" \
  -o Tests/arc \
  "$C_OBJDIR"/Common.o \
  "$C_OBJDIR"/CompressionLibrary.o \
  "$C_OBJDIR"/Environment.o \
  "$C_OBJDIR"/URL.o \
  "$C_OBJDIR"/C_PPMD.o \
  "$C_OBJDIR"/C_LZP.o \
  "$C_OBJDIR"/C_LZMA.o \
  "$C_OBJDIR"/C_BCJ.o \
  "$C_OBJDIR"/C_GRZip.o \
  "$C_OBJDIR"/C_Dict.o \
  "$C_OBJDIR"/C_REP.o \
  "$C_OBJDIR"/C_MM.o \
  "$C_OBJDIR"/C_TTA.o \
  "$C_OBJDIR"/C_Tornado.o \
  "$C_OBJDIR"/C_Delta.o \
  "$C_OBJDIR"/C_External.o \
  "$C_OBJDIR"/C_Encryption.o \
  "$C_OBJDIR"/CELS.o \
  "$C_OBJDIR"/cels-rep.o \
  "$C_OBJDIR"/ntrljmp.o \
  "$C_OBJDIR"/print.o \
  "$C_OBJDIR"/lapi.o "$C_OBJDIR"/lauxlib.o "$C_OBJDIR"/lbaselib.o \
  "$C_OBJDIR"/lcode.o "$C_OBJDIR"/ldblib.o "$C_OBJDIR"/ldebug.o \
  "$C_OBJDIR"/ldo.o "$C_OBJDIR"/ldump.o "$C_OBJDIR"/lfunc.o \
  "$C_OBJDIR"/lgc.o "$C_OBJDIR"/linit.o "$C_OBJDIR"/liolib.o \
  "$C_OBJDIR"/llex.o "$C_OBJDIR"/lmathlib.o "$C_OBJDIR"/lmem.o \
  "$C_OBJDIR"/loadlib.o "$C_OBJDIR"/lobject.o "$C_OBJDIR"/lopcodes.o \
  "$C_OBJDIR"/loslib.o "$C_OBJDIR"/lparser.o "$C_OBJDIR"/lstate.o \
  "$C_OBJDIR"/lstring.o "$C_OBJDIR"/lstrlib.o "$C_OBJDIR"/ltable.o \
  "$C_OBJDIR"/ltablib.o "$C_OBJDIR"/ltm.o "$C_OBJDIR"/lundump.o \
  "$C_OBJDIR"/lvm.o "$C_OBJDIR"/lzio.o \
  -optl-s -lstdc++ -lncurses -lcurl \
  +RTS -A2m

rm -f Compression/CompressionLib_stub.?

echo "=== Build complete ==="
ls -la Tests/arc
