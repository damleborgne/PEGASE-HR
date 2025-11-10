# Migration Guide: Modernizing PEGASE-HR Build System

This document explains the changes made to modernize the PEGASE-HR build system and how to migrate from the old autoconf-only system.

## What Changed?

### Before (Old System - circa 2010)
- Required bundled cfitsio in a `cfitsio/` subdirectory
- Only autoconf-based build system
- Complex platform-specific binary directories
- Manual compilation of cfitsio during configure

### After (Modern System - 2025)
- **Primary**: CMake build system (recommended)
- **Alternative**: Updated autoconf with optional system cfitsio support
- Uses system-installed cfitsio (Homebrew, apt, yum)
- Simpler, cross-platform builds
- Out-of-source builds supported

## Benefits of the New System

### CMake Build (Recommended)
✅ Modern, industry-standard build system  
✅ Automatic dependency detection  
✅ Out-of-source builds (keeps source tree clean)  
✅ Better IDE integration (VS Code, CLion, etc.)  
✅ Cross-platform (macOS, Linux, Windows with MinGW)  
✅ Parallel builds by default  
✅ Integrated testing with CTest  
✅ Easy installation with `cmake --install`  

### Updated Autoconf Build (Legacy Support)
✅ Backward compatible with old workflow  
✅ Now supports system cfitsio via `--with-system-cfitsio`  
✅ No need to bundle cfitsio sources  

## Step-by-Step Migration

### Option A: Migrate to CMake (Recommended)

#### 1. Install Dependencies
```bash
# macOS
brew install cmake gcc cfitsio

# Ubuntu/Debian
sudo apt-get install cmake gfortran libcfitsio-dev

# CentOS/RHEL
sudo yum install cmake gcc-gfortran cfitsio-devel
```

#### 2. Clean Old Build Artifacts
```bash
# If you have old autoconf builds
make veryclean  # or rm -rf bin/* autom4te.cache config.* make.sys make.rules
```

#### 3. Build with CMake
```bash
mkdir build
cd build
cmake ..
cmake --build .
# For parallel build:
make -j4
```

#### 4. Test
```bash
ctest
# Or manually:
cd ../data/tests/
./do_test.tcsh
```

#### 5. Install (Optional)
```bash
# System-wide (requires sudo)
sudo cmake --install .

# Or to custom location
cmake --install . --prefix $HOME/pegase-hr
```

### Option B: Use Updated Autoconf

If you prefer to stick with autoconf but want to use system cfitsio:

#### 1. Install cfitsio
```bash
# macOS
brew install cfitsio pkg-config

# Ubuntu/Debian
sudo apt-get install libcfitsio-dev pkg-config

# CentOS/RHEL
sudo yum install cfitsio-devel pkgconfig
```

#### 2. Build with System CFITSIO
```bash
autoconf
./configure --with-system-cfitsio
make
```

Or to use bundled cfitsio (old behavior):
```bash
autoconf
./configure  # without --with-system-cfitsio
make
```

## Removing Bundled cfitsio

If you no longer need the bundled cfitsio sources:

```bash
# Save disk space and simplify your repository
rm -rf cfitsio/

# Update .gitignore if needed (already done in new .gitignore)
```

**Important**: If you remove the bundled cfitsio directory:
- CMake builds will automatically use system cfitsio
- Autoconf builds MUST use `--with-system-cfitsio`

## Configuration Comparison

### Old Autoconf Build
```bash
cd cfitsio
autoconf
./configure
make
cd ..

autoconf
./configure
make
```

### New CMake Build
```bash
mkdir build
cd build
cmake ..
cmake --build .
```

### New Autoconf Build (with system cfitsio)
```bash
autoconf
./configure --with-system-cfitsio
make
```

## Environment Setup Changes

### Old Setup
Executables in `bin/${ARCHBIN}/` (e.g., `bin/arm-apple-darwin24.6.0/`)

Add to `.bashrc`:
```bash
export PATH=/path/to/PEGASE-HR/bin/${ARCHBIN}:$PATH
```

### New Setup (CMake)
Executables in `build/` directory or `/usr/local/bin` after install

Add to `.bashrc`:
```bash
# If using build directory directly
export PATH=/path/to/PEGASE-HR/build:$PATH

# Or if installed system-wide, nothing to add (already in PATH)
```

## Troubleshooting Migration Issues

### "CFITSIO not found" Error

**With CMake:**
```bash
# Find where cfitsio is installed
pkg-config --libs cfitsio

# If not found, install it
brew install cfitsio  # macOS
sudo apt-get install libcfitsio-dev  # Linux

# Then rebuild
cd build
cmake ..
```

**With autoconf:**
```bash
# Use system cfitsio
./configure --with-system-cfitsio

# Or specify library path manually
./configure LDFLAGS="-L/usr/local/lib" CPPFLAGS="-I/usr/local/include"
```

### "Module not found" Errors

CMake automatically handles module dependencies. If you see module errors:
```bash
# Clean and rebuild
cd build
rm -rf *
cmake ..
cmake --build .
```

### Old Binary Directory Issues

If you have scripts referencing `bin/${ARCHBIN}/`:
```bash
# Update your scripts to use:
bin/  # if using build directory directly
# or
/usr/local/bin/  # if installed system-wide
```

## Testing Both Build Systems

You can test both build systems side-by-side:

```bash
# CMake build (in build/ directory)
mkdir build && cd build
cmake ..
cmake --build .
cd ..

# Autoconf build (in source tree)
autoconf
./configure --with-system-cfitsio
make

# Both will work independently
```

## Recommended Workflow for Developers

1. **Daily development**: Use CMake with out-of-source build
   ```bash
   cd build
   make -j4
   ```

2. **Testing changes**: Quick rebuild with CMake
   ```bash
   make SSPs_HR  # rebuild specific target
   # or: cmake --build . --target SSPs_HR
   ```

3. **Release**: Create clean build
   ```bash
   rm -rf build
   mkdir build && cd build
   cmake -DCMAKE_BUILD_TYPE=Release ..
   make -j$(nproc)
   ctest
   ```

## CI/CD Integration

### GitHub Actions Example
```yaml
name: Build and Test
on: [push, pull_request]
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Install dependencies
        run: sudo apt-get install -y gfortran libcfitsio-dev cmake
      - name: Build
        run: |
          mkdir build && cd build
          cmake ..
          make -j2
      - name: Test
        run: cd build && ctest
```

## Questions?

- **Which build system should I use?** → CMake for new projects, autoconf if you have specific compatibility needs
- **Can I delete autoconf files?** → Not yet, but CMake is now the primary build method
- **What about Windows?** → CMake supports MinGW/MSYS2 on Windows
- **Performance difference?** → Both produce identical optimized binaries

## Summary

| Feature | Old Autoconf | New Autoconf | CMake |
|---------|-------------|--------------|-------|
| System cfitsio | ❌ | ✅ | ✅ |
| Out-of-source build | ❌ | ❌ | ✅ |
| Parallel build | Manual | Manual | Automatic |
| Dependency detection | Manual | Semi-auto | Automatic |
| IDE support | Limited | Limited | Excellent |
| Cross-platform | Good | Good | Excellent |
| Modern practices | ❌ | ⚠️ | ✅ |

**Recommendation**: Use CMake for all new development. The autoconf build is maintained for backward compatibility but is no longer the primary build method.
