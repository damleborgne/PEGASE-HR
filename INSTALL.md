# PEGASE-HR Installation Guide

PEGASE-HR is a high-resolution spectral synthesis code written in Fortran 90.
This guide describes the modern installation procedure using CMake.

## ⚠️ IMPORTANT: Large Data Files (Git LFS)

**CRITICAL**: The stellar library files (`data/stellibs/*.fits`) are stored using Git LFS (Large File Storage). 

After cloning the repository, these files will appear as small text files (~134 bytes) instead of the actual large FITS files (~20 MB). **The programs will fail with an error like:**

```
Failed opening file /path/to/PEGASE-HR/data/stellibs/stellibLCBcor.fits
```

### To download the actual data files:

**Option 1: Using Git LFS (recommended)**
```bash
# Install Git LFS (requires root access)
sudo apt-get install git-lfs  # Ubuntu/Debian
# or
brew install git-lfs          # macOS

# Then fetch the large files
cd /path/to/PEGASE-HR
git lfs install
git lfs pull
```

**Option 2: Manual download (no root required)**

Download the files directly from GitHub:
```bash
cd /path/to/PEGASE-HR/data/stellibs

# Download stellibLCBcor.fits
wget https://media.githubusercontent.com/media/damleborgne/PEGASE-HR/master/data/stellibs/stellibLCBcor.fits -O stellibLCBcor.fits

# Download stellibELODIE_3.0.fits
wget https://media.githubusercontent.com/media/damleborgne/PEGASE-HR/master/data/stellibs/stellibELODIE_3.0.fits -O stellibELODIE_3.0.fits

# Download stellibELODIE_3.1.fits
wget https://media.githubusercontent.com/media/damleborgne/PEGASE-HR/master/data/stellibs/stellibELODIE_3.1.fits -O stellibELODIE_3.1.fits
```

**Verify the files are correctly downloaded:**
```bash
ls -lh data/stellibs/*.fits
# Should show files of ~20 MB, NOT 134 bytes
```

---

## Requirements

### System Requirements
- **Fortran 90 compiler**: `gfortran` (recommended), Intel Fortran, or other F90-compatible compiler
- **CMake**: Version 3.15 or later
- **CFITSIO library**: For FITS file I/O

### Installing Dependencies

#### macOS (using Homebrew)
```bash
brew install cmake gcc cfitsio
```

#### Ubuntu/Debian Linux
```bash
sudo apt-get update
sudo apt-get install cmake gfortran libcfitsio-dev pkg-config
```

#### CentOS/RHEL/Fedora
```bash
sudo yum install cmake gcc-gfortran cfitsio-devel pkgconfig
# or on newer systems:
sudo dnf install cmake gcc-gfortran cfitsio-devel pkgconfig
```

## Installation Procedure

### Method 1: CMake Build (Recommended)

This is the modern, cross-platform build method:

```bash
# 1. Create a build directory (out-of-source build)
mkdir build
cd build

# 2. Configure the project
cmake ..

# 3. Build the executables
cmake --build .
# Or for parallel build (CMake 3.12+):
# cmake --build . --parallel 4
# Or older CMake versions:
# cmake --build . -- -j4

# 4. (Optional) Run tests
ctest

# 5. Install (default: /usr/local/bin, or specify with -DCMAKE_INSTALL_PREFIX)
sudo cmake --install .
# Or install to a custom location:
# cmake --install . --prefix $HOME/pegase-hr
```

#### CMake Build Options

You can customize the build with these options:

```bash
# Debug build (includes debugging symbols, bounds checking)
cmake -DCMAKE_BUILD_TYPE=Debug ..

# Specify a custom installation directory
cmake -DCMAKE_INSTALL_PREFIX=$HOME/software/pegase-hr ..

# Use a specific Fortran compiler
cmake -DCMAKE_Fortran_COMPILER=ifort ..

# If CFITSIO is installed in a non-standard location
cmake -DCFITSIO_ROOT=/path/to/cfitsio ..
```

### Method 2: Legacy autoconf Build

If you prefer the traditional autoconf-based build (deprecated but still supported):

```bash
# 1. Generate configure script
autoconf

# 2. Run configure
./configure

# 3. Build
make

# The executables will be in bin/${ARCHBIN}/
```

**Note**: The legacy build expects cfitsio to be in a `cfitsio/` subdirectory, which is no longer recommended.

## Post-Installation Setup

### Setting Up Your Environment

To easily run PEGASE-HR programs from any directory, add the following to your shell configuration:

#### For bash users (~/.bashrc or ~/.bash_profile):
```bash
# PEGASE-HR setup
export PEGASE_HR_ROOT=/path/to/PEGASE-HR3
export PATH=$PEGASE_HR_ROOT/build:$PATH
# Or if you installed system-wide: (no changes needed, /usr/local/bin is in PATH)
```

#### For csh/tcsh users (~/.cshrc):
```csh
# PEGASE-HR setup
setenv PEGASE_HR_ROOT /path/to/PEGASE-HR3
setenv PATH $PEGASE_HR_ROOT/build:$PATH
```

Alternatively, if you installed to the default location (`/usr/local/bin`), the binaries are already in your PATH.

## Testing the Installation

**BEFORE RUNNING TESTS**: Make sure you have downloaded the large data files (see the warning at the top of this document)!

Verify the stellar library files are correctly downloaded:
```bash
ls -lh data/stellibs/*.fits
# Should show files of ~20 MB each, NOT 134 bytes
```

Run the test suite:

```bash
# From the build directory
ctest

# Or manually
cd data/tests/
./do_test.tcsh
```

This will verify that the installation is working correctly.

## Available Programs

After installation, the following executables are available:

- `SSPs_HR` - Generate Simple Stellar Populations
- `spectra_HR` - Compute spectra from scenarios
- `scenarios_HR` - Manage evolution scenarios
- `colors_HR` - Compute colors from spectra
- `lick` - Compute Lick indices
- `calib_HR` - Calibration utilities
- `compare_fits` - Compare FITS files
- `fitstodat` - Convert FITS to ASCII data

## Troubleshooting

### "Failed opening file" error for stellib*.fits files

**Symptom**: The program crashes with:
```
Failed opening file /path/to/PEGASE-HR/data/stellibs/stellibLCBcor.fits
```

**Cause**: The stellar library files were not properly downloaded. If you see files of ~134 bytes in `data/stellibs/`, these are Git LFS pointers, not the actual data files.

**Solution**: See the [Large Data Files section](#️-important-large-data-files-git-lfs) at the top of this document for instructions to download the actual files using Git LFS or wget.

### CFITSIO not found

If CMake cannot find CFITSIO, you have two options:

#### Option 1: System-wide installation (requires root)
```bash
# Ubuntu/Debian
sudo apt-get install libcfitsio-dev pkg-config

# macOS with Homebrew
brew install cfitsio
```

#### Option 2: Local installation (no root required)

If you don't have root access, install CFITSIO locally:

```bash
# Download and extract CFITSIO
cd ~
wget https://heasarc.gsfc.nasa.gov/FTP/software/fitsio/c/cfitsio-4.6.3.tar.gz
tar -xzf cfitsio-4.6.3.tar.gz
cd cfitsio-4.6.3

# Configure, compile, and install to ~/local
./configure --prefix=$HOME/local
make
make install

# Add to your shell configuration (~/.bashrc or ~/.tcshrc)
# For bash:
export LD_LIBRARY_PATH=$HOME/local/lib:$LD_LIBRARY_PATH
export PKG_CONFIG_PATH=$HOME/local/lib/pkgconfig:$PKG_CONFIG_PATH

# For tcsh:
setenv LD_LIBRARY_PATH $HOME/local/lib
setenv PKG_CONFIG_PATH $HOME/local/lib/pkgconfig

# Reload your shell configuration
source ~/.bashrc  # or source ~/.tcshrc

# Then configure PEGASE-HR
cd /path/to/PEGASE-HR/build
cmake ..
```

### Memory Issues (Segmentation Fault)

**IMPORTANT**: PEGASE-HR uses large static arrays that require increased stack size.

If you encounter segmentation faults when running the programs, you **MUST** increase the stack limit:

```bash
# On macOS - Add to your ~/.zshrc or ~/.bash_profile:
ulimit -s 65520

# On Linux - Add to your ~/.bashrc:
ulimit -s unlimited
```

After adding this line, either:
- Restart your terminal, or  
- Run `source ~/.zshrc` (macOS) or `source ~/.bashrc` (Linux)

**This step is mandatory** - the programs will crash without it.

### Compiler Issues
If you get Fortran compiler errors, ensure you have a modern gfortran version:
```bash
gfortran --version  # Should be 8.0 or later
```

For GCC 10+, the `-fallow-argument-mismatch` flag is automatically added.

## Migration from Old Build System

If you previously used the autoconf-based build:

1. **Remove old build artifacts**:
   ```bash
   make veryclean  # if you still have the old Makefile
   # or manually:
   rm -rf bin/* autom4te.cache config.* make.sys make.rules
   ```

2. **No longer need cfitsio subdirectory**: You can remove the bundled cfitsio if it exists
   ```bash
   rm -rf cfitsio/
   ```

3. **Update your build scripts** to use CMake commands instead

## Advanced Configuration

### Custom Compiler Flags

Edit `CMakeLists.txt` to customize compiler flags, or pass them via command line:

```bash
cmake -DCMAKE_Fortran_FLAGS="-O3 -march=native" ..
```

### Parallel Builds

**⚠️ WARNING**: Fortran module dependencies can cause race conditions with parallel builds.
If you encounter errors like `Cannot delete temporary module file`, use sequential build (`-j1`).

Sequential build (recommended, safe):
```bash
cmake --build .
# Or: make
```

Parallel build (faster but may fail):
```bash
# CMake 3.12 and later
cmake --build . --parallel 4

# Older CMake versions (pass flags to underlying build system)
cmake --build . -- -j4

# Or use make directly
make -j4
```

## Getting Help

For issues with PEGASE-HR, please contact: leborgne@iap.fr

For CFITSIO documentation: https://heasarc.gsfc.nasa.gov/fitsio/

---

**Copyright 2004-2025, D. Le Borgne et al.**
