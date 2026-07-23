# FreeArc — GHC 9.14 / arm64 macOS Port

A port of [FreeArc 0.60 RC](http://freearc.org) (2009) to compile and run on modern **arm64 macOS** with **GHC 9.14.1** via Homebrew.

FreeArc is a high-performance open-source file archiver with compression, encryption, and recovery support. The original code targeted GHC 6.6/6.8 on 32-bit Linux/Windows. This port brings it up to date for 64-bit macOS arm64 (Apple Silicon).

## Building

### Prerequisites

```bash
brew install ghc curl ncurses
```

### Build

```bash
bash buildcompile_modern.sh
```

This compiles all C/C++ compression libraries, Lua 5.1, and the Haskell sources into `Tests/arc`. The script uses separate output directories (`c-objects/` for C/C++, `hs-objects/` for Haskell) and compiles everything directly without relying on the original makefiles (which assume x86 Linux).

### Usage

```bash
./Tests/arc --help
./Tests/arc a archive.arc files...      # create archive
./Tests/arc l archive.arc               # list contents
./Tests/arc e archive.arc               # extract files
./Tests/arc x archive.arc               # extract with paths
./Tests/arc t archive.arc               # test integrity
```

## What Was Fixed

### 64-bit ABI Fixes

- **`LuaReader` type** (`HsLua/src/Scripting/Lua.hs`): Changed `Ptr CInt` to `Ptr CSize` for the `lua_Reader` callback's size parameter. Lua 5.1's `lua_Reader` uses `size_t*` (8 bytes on arm64), but the Haskell binding used `CInt` (4 bytes), causing memory corruption in `lua_load` that made every Lua code load fail with syntax errors.

- **arm64 C/C++ compilation**: Removed x86-specific `-march=i486` flags from all makefiles. Added macOS `sysctl` compat in `Environment.cpp`. Fixed 64-bit integer handling in Tornado compression.

### GHC 9 Language & API Changes

- **`NondecreasingIndentation`** removed from default extensions; added explicitly where needed.
- **Ambiguous type variables** in `catch`/`handle`/`try`/`mapM` resolved with `SomeException` type annotations and `ScopedTypeVariables`.
- **`FunctionalDependencies`** extension added for typeclass resolution.
- **`NoMonomorphismRestriction`** added to modules with ambiguous type variables from the `Compression` typeclass.
- **`@` pattern syntax** fixed (no surrounding whitespace in GHC 9.14).
- **`mdo`** requires explicit `RecursiveDo` extension.
- **Operator sections** require parentheses: `(== 'e')` not `=='e'`.
- **`Data.HashTable`** replaced (module removed in GHC 9).
- **`GHC.PArr`** replaced (module removed).
- **`setUncaughtExceptionHandler`** removed from `Control.Exception`; replaced with `return ()`.
- **`Deadlock`** is now a separate type from `ErrorCall`; pattern matching updated.
- **`CalendarTime`** field names changed: `ctMin`/`ctSec` to `ctMinute`/`ctSecond`.
- **`noTimeDiff`** removed from `System.Time`.

### Filesystem Encoding

- **`filesystem2str`/`str2filesystem`** left as identity functions. GHC 9's `getDirectoryContents` and `peekCString` already return proper Unicode strings, so FreeArc's charset conversion layer was double-encoding non-ASCII filenames, causing "illegal UTF-8 character" crashes.

### Compatibility Shims

- `System/Time.hs` — wraps `Data.Time` to provide the old `System.Time` API (`CalendarTime`, `clockTime`, etc.)
- `System/Locale.hs` — stub for `System.Locale` (functionality moved to `Data.Time.Format`)

## Project Structure

```
├── Arc.hs                  Main entry point
├── ArcCreate.hs            Archive creation
├── ArcExtract.hs           Archive extraction
├── ArcRecover.hs           Recovery record support
├── ArhiveDirectory.hs      Archive directory encoding
├── ArhiveFileList.hs       File list processing
├── ArhiveStructure.hs      Archive format structures
├── ByteStream.hs           Binary I/O streaming
├── Charsets.hs             Character encoding conversion
├── Cmdline.hs              Command-line parsing
├── Compression.hs          Compression method management
├── Compression/            C/C++ compression libraries
│   ├── CLS/               CLS codec
│   ├── Encryption/        Encryption support
│   ├── GRZip/             GRZip codec
│   ├── LZMA/              LZMA/7zip codec
│   ├── LZP/               LZP codec
│   ├── MM/                Multimedia-optimized codec
│   ├── PPMD/              PPMd codec
│   ├── REP/               Repetition finder
│   ├── Tornado/           Tornado codec
│   └── Delta/             Delta filter
├── Encryption.hs           Encryption interface
├── Errors.hs               Error handling
├── Files.hs                Filesystem operations
├── FileInfo.hs             File metadata
├── HsLua/src/             Lua 5.1 interpreter (Haskell + C)
├── Options.hs              Configuration and Lua scripting
├── System/
│   ├── Time.hs            System.Time compatibility shim
│   └── Locale.hs          System.Locale compatibility stub
├── Utils.hs                Utility functions
├── UI.hs / UIBase.hs / CUI.hs  User interface
├── buildcompile_modern.sh  Modern build script (macOS arm64 / GHC 9)
└── Tests/arc               Built binary (after compilation)
```

## Status

| Feature | Status |
|---------|--------|
| Create archives (`a`) | Working |
| List archives (`l`, `v`) | Working |
| Extract archives (`e`, `x`) | Working |
| Test archives (`t`) | Working |
| Compression (LZMA, Tornado, etc.) | Working |
| Encryption | Compiled, untested |
| Recovery records | Compiled, untested |
| SFX archives | Compiled, untested |
| GUI | Not ported (requires GTK) |

## License

FreeArc is released under the GPL v2. See source files for individual copyright notices.
