# FreeArc — GHC 9.14 / arm64 macOS Port

A port of [FreeArc 0.60 RC](http://freearc.org) (2009) to compile and run on modern **arm64 macOS** with **GHC 9.14.1** via Homebrew.

FreeArc is a high-performance open-source file archiver with compression, encryption, and recovery support. The original code targeted GHC 6.6/6.8 on 32-bit Linux/Windows. This port brings it up to date for 64-bit macOS arm64 (Apple Silicon), including a working GTK+3 GUI.

## Building

### Prerequisites

```bash
brew install ghc curl ncurses gtk+3
```

### Build CLI only

```bash
bash buildcompile_modern.sh
```

This compiles all C/C++ compression libraries, Lua 5.1, and the Haskell sources into `Tests/arc`. The script uses separate output directories (`c-objects/` for C/C++, `hs-objects/` for Haskell) and compiles everything directly without relying on the original makefiles (which assume x86 Linux).

### Build GUI

Building the GUI requires patched gtk2hs packages for GHC 9.14. Follow the steps in the [GUI build notes](#gui-build-notes) below, then:

```bash
# After building gtk2hs packages into /tmp/gtk3-packages/
bash buildcompile_gui.sh
```

This produces `Tests/arc-gui` — a native GTK+3 file manager for FreeArc.

### Usage

```bash
./Tests/arc --help
./Tests/arc a archive.arc files...      # create archive
./Tests/arc l archive.arc               # list contents
./Tests/arc e archive.arc               # extract files
./Tests/arc x archive.arc               # extract with paths
./Tests/arc t archive.arc               # test integrity
```

## Compression Codecs

All of the following compression methods have been tested and verified with MD5-confirmed round-trip integrity (compress, decompress, compare checksums) on arm64 macOS.

### Single-Method Codecs

| Method | Description | Status |
|--------|-------------|--------|
| `storing` | No compression (passthrough) | Verified |
| `lzma` | LZMA/7zip compression (dictionary-based) | Verified |
| `delta` + `lzma` | Delta filter followed by LZMA | Verified |
| `grzip` | GRZip (bwt+mtf+ Huffman) | Verified |
| `rep` | Repetition finder (preprocessing filter) | Verified |
| `dict` | Dictionary-based preprocessor | Verified |
| `tta` | True Audio lossless codec | Verified |
| `ppmd` | Prediction by Partial Matching (PPMd) | Verified |
| `tor` | Tornado (fast byte-oriented codec) | Verified |
| `lzp` | LZP (LZ with predictions) | Verified |
| `mm` | Multimedia-optimized codec | Verified |

### Multi-Method Pipelines

Methods can be chained with `+` to create compression pipelines. Each method runs in its own GHC thread, passing data through Haskell MVar/Chan pipes.

| Pipeline | Description | Status |
|----------|-------------|--------|
| `lzma+lzma` | Double LZMA | Verified |
| `lzma+lzp` | LZMA followed by LZP | Verified |
| `lzp+lzma` | LZP followed by LZMA | Verified |
| `lzma+lzma+lzma` | Triple LZMA | Verified |
| `delta+lzma+lzp` | Delta + LZMA + LZP | Verified |
| `lzma+grzip` | LZMA followed by GRZip | Verified |
| `ppmd+lzma` | PPMd followed by LZMA | Verified |
| `rep+lzma` | Repetition finder + LZMA | Verified |
| `dict+lzma` | Dictionary filter + LZMA | Verified |
| `tta+lzma` | TTA + LZMA | Verified |
| `lzp+lzma` | LZP followed by LZMA | Verified |
| `mm+lzma` | Multimedia filter + LZMA | Verified |
| `lzma+delta+lzma` | LZMA + Delta + LZMA | Verified |
| `delta+lzma` | Delta filter + LZMA | Verified |

### Branch Filters

| Method | Description | Status |
|--------|-------------|--------|
| `bcj` / `exe` | x86 branch filter (E8/E9 CALL/JMP conversion) | Available (x86 code only) |
| `arm64` / `bcj_arm64` | AArch64 branch filter (B/BL imm26 conversion) | Verified |

The `arm64` filter converts relative branch offsets in ARM64 B/BL instructions to absolute addresses, improving LZ compression of ARM64 executables. Use `-marm64+lzma` for best compression of ARM64 binaries (~4% improvement over LZMA alone).

### Not Working

| Method | Reason |
|--------|--------|
| `cls` | Requires external CLS library (not compiled) |
| `ppmonstr` | Requires external PPMONSTR binary (not included) |

## Decompression

All codecs above support decompression. Archives created with any supported method can be read, tested, and extracted:

```bash
./Tests/arc t archive.arc    # verify integrity
./Tests/arc x archive.arc    # extract with paths
```

## What Was Fixed

### 64-bit ABI Fixes

- **`LuaReader` type** (`HsLua/src/Scripting/Lua.hs`): Changed `Ptr CInt` to `Ptr CSize` for the `lua_Reader` callback's size parameter. Lua 5.1's `lua_Reader` uses `size_t*` (8 bytes on arm64), but the Haskell binding used `CInt` (4 bytes), causing memory corruption in `lua_load` that made every Lua code load fail with syntax errors.

- **arm64 C/C++ compilation**: Removed x86-specific `-march=i486` flags from all makefiles. Added macOS `sysctl` compat in `Environment.cpp`. Fixed 64-bit integer handling in Tornado compression.

### FFI Return Types

- **All compression FFI declarations** (`CompressionLib.hs`, `HsCELS.hs`): Changed foreign imports from `IO Int` to `IO CInt`. On arm64, `Int` is 8 bytes but C `int` is 4 bytes. Calling convention mismatches caused return values to be truncated or misread, breaking every codec. All call sites wrapped with `fromIntegral`.

### PPMD Codec Fixes (64-bit)

The PPMD range coder requires exactly 32-bit arithmetic for correct normalization. On 64-bit systems, several types were silently promoted to 64-bit, breaking the algorithm.

- **`PPMdType.h`**: Changed `DWORD` from `unsigned long` (8 bytes on arm64) to `uint32_t` (4 bytes). Changed `UINT` to `unsigned int` (4 bytes). The range coder's `low`, `code`, and `range` fields must be exactly 32-bit for the normalization condition `(low ^ (low+range)) < TOP` to work correctly.

- **`Model.cpp`**: In `CreateSuccessors`, the code copied `PPM_CONTEXT` structs using assignment of individual fields. On 32-bit, this copied 8 bytes for the `Stats` pointer + 4 bytes for `Suffix` = 12 bytes. On 64-bit, `Stats` is 8 bytes and `Suffix` is 4 bytes, but only 8 bytes were copied (the `Stats` pointer was truncated). Fixed with `memcpy(pc1, &ct, sizeof(PPM_CONTEXT))`.

- **`SubAlloc.hpp`**: `UNIT_SIZE` was computed as `sizeof(WORD) + sizeof(WORD*) + sizeof(uint)` which was 18 bytes on 32-bit but 20 bytes on 64-bit. The `MEM_BLK` struct was padded to 20 bytes. Fixed `U2B` and `UnitsCpy` to use `sizeof(MEM_BLK)` consistently. Changed `~0UL` sentinel values to `~(DWORD)0` (always 32-bit).

### LZP Codec Fixes

- **`Common.h` lb()**: The `lb()` function (find highest set bit) used `__builtin_clz(0)` which is undefined behavior. On arm64 it returned 63 instead of a safe default, causing off-by-one errors in hash table sizing. Added `n ? (...) : 0` guard.

- **`GRZip/LZP.c`**: Hash table allocation used `sizeof(uint32)` per entry but stored `uint8*` pointers. On 64-bit, pointers are 8 bytes, causing the hash table to overflow its allocation. Fixed to `sizeof(uint8*)`.

### MM Codec Fixes

- **`mmdet.cpp`**: Changed `long*` pointers to `int32_t*` for `_32bit_run` and `_32bit_diff_run`. On arm64, `long` is 8 bytes but the MM codec expects 4-byte samples.

### Multi-Method Pipeline Fix

The most critical fix. Multi-method pipelines (`lzma+lzma`, `lzma+lzp`, etc.) crashed with `EXC_BAD_ACCESS` in `___chkstk_darwin`.

**Root cause**: The `CHECK` error-handling macro in `Common.h` allocated `char s[MY_FILENAME_MAX*4]` (262,144 bytes = 256KB) on the stack. `MY_FILENAME_MAX` is 65,536. Although this allocation was in the `default:` switch branch that is never reached, GCC allocates the full stack frame in the function prologue. Through aggressive inlining, this 256KB frame propagated into compression functions. Combined with the existing call chain (~250KB), this exceeded the 512KB default pthread stack on macOS.

Single-method `lzma` worked because it ran on the main thread (8MB stack). In multi-method pipelines, intermediate stages ran on GHC worker threads with only 512KB pthread stacks.

**Fixes applied**:

1. **`Common.h`**: Changed the Unix `CHECK` macro from stack allocation to heap allocation (`malloc`/`free`), matching the Windows versions.

2. **`Process.hs`**: Changed `forkOS` to `forkIO` for pipeline threads. GHC's `safe` FFI calls automatically use bound threads when needed, so dedicated OS threads were unnecessary. `forkIO` threads run on GHC's scheduler, which uses larger worker thread stacks.

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

### Build System

- **`buildcompile_modern.sh`**: Modern build script compiling all C/C++ sources individually into `c-objects/`, Haskell into `hs-objects/`, then linking with GHC. Added `-rtsopts +RTS -A2m -K8m` for RTS flag support (2MB allocation area, 8MB max stack per Haskell thread).

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
├── GUI.hs                  GTK+3 GUI core (window, menus, dialogs)
├── FileManager.hs          File manager main window and logic
├── FileManPanel.hs         File list panel with selection
├── FileManDialogs.hs       Add/Extract/Settings dialogs
├── FileManDialogAdd.hs     Add files dialog
├── FileManUtils.hs         FM state, config, and utilities
├── HsLua/src/             Lua 5.1 interpreter (Haskell + C)
├── Options.hs              Configuration and Lua scripting
├── System/
│   ├── Time.hs            System.Time compatibility shim
│   └── Locale.hs          System.Locale compatibility stub
├── Utils.hs                Utility functions
├── UI.hs / UIBase.hs / CUI.hs  User interface
├── buildcompile_modern.sh  Modern build script (macOS arm64 / GHC 9)
└── Tests/arc               Built CLI binary (after compilation)
    Tests/arc-gui           Built GUI binary (after GUI compilation)
```

## Status

| Feature | Status |
|---------|--------|
| Create archives (`a`) | Working |
| List archives (`l`, `v`) | Working |
| Extract archives (`e`, `x`) | Working |
| Test archives (`t`) | Working |
| 11 single-method codecs | All verified |
| Multi-method pipelines | All verified |
| Encryption | Compiled, untested |
| Recovery records | Compiled, untested |
| SFX archives | Compiled, untested |
| GUI (GTK+3) | **Working** — file navigation, archive browsing, menus, toolbar |

## GUI (GTK+3)

The graphical file manager has been ported to GTK+3. The GUI provides a dual-pane archive manager with menus, toolbar, directory navigation, and archive browsing.

### What Works

- **Window and layout**: Menu bar, toolbar, file list with columns (Name, Size, Packed, Ratio, Date, Attrs), status bar
- **Directory navigation**: Enter directories, go up (Backspace/Up button), navigate into `.arc` archives
- **Archive listing**: Browse archive contents with file sizes and compression ratios
- **Menu system**: File, Commands, Tools, Options, Help menus with keyboard accelerators
- **Toolbar**: Open, Add, Extract, Test, Info, Delete, Lock, Join, Refresh
- **History**: Directory/archive path history with combo dropdowns
- **Dialogs**: Add files dialog with compression options, Extract dialog, Settings dialog

### GTK+3 Port Details

Building the GUI required porting the gtk2hs bindings from GTK+2 to GTK+3:

- **Patched glib-0.13.12.0**: Fixed `hsgclosure.c` for GHC 9.14 RTS API (`runIO_closure` → `ghc_hs_iface->runIO_closure`)
- **Patched gtk3-0.15.10**: Added `GTK_DISABLE_DEPRECATION_WARNINGS` to handle deprecated GTK+3 APIs
- **Signal API changes**: Converted all signal connections from GTK+2 (`widget \`onXxx\` callback`) to GTK+3 (`GtkSigs.on widget xxxSignal $ callback`)
- **Event callbacks**: Changed from function-taking-event to monadic `EventM` style with `liftIO`
- **ComboBoxText rewrite**: GTK+3 `ComboBoxText` no longer has an internal Entry child. Rewrote `fmEntryWithHistory` to use standalone Entry + ComboBox in a VBox
- **Threading fix**: Removed `bg $ do` wrapper that ran the entire GUI in a background thread — GTK+3 requires the main thread on macOS (Cocoa backend). Restructured `doMain` with separate GUI and CLI code paths
- **Safe file selection**: Bounds-checked row indices in `getSelectionFileInfo` to prevent crashes when selection is stale after directory changes

### GUI Build Notes

The gtk2hs packages need to be built with local patches:

1. Clone and patch glib's `hsgclosure.c` for GHC 9.14 RTS
2. Build into a local package database at `/tmp/gtk3-packages/`
3. Use `-package-db /tmp/gtk3-packages/ -package gtk3` when compiling
4. The GUI build uses `-DFREEARC_GUI` to enable GTK+ code paths

## Releases

- [v0.62-macos-arm64](https://github.com/mattwaltbriggs/freearc/releases/tag/v0.62) — AArch64 BCJ filter for ARM64 executables, version 0.62
- [v0.61-macos-arm64](https://github.com/mattwaltbriggs/freearc/releases/tag/v0.61-macos-arm64) — Working GTK+3 GUI, version 0.61
- [v0.60-rc2-macos-arm64](https://github.com/mattwaltbriggs/freearc/releases/tag/v0.60-rc2-macos-arm64) — Pipeline crash fix, CHECK macro heap allocation, version RC2
- [v0.60-rc-macos-arm64](https://github.com/mattwaltbriggs/freearc/releases/tag/v0.60-rc-macos-arm64) — Initial port: all codecs, GHC 9 compatibility, 64-bit fixes

## License

FreeArc is released under the GPL v2. See source files for individual copyright notices.
