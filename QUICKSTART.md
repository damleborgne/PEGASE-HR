# PEGASE-HR Quick Reference

## Quick Build Commands

### First Time Setup
```bash
# Install dependencies
brew install cmake gcc cfitsio  # macOS
# or
sudo apt-get install cmake gfortran libcfitsio-dev  # Linux

# Increase stack size (IMPORTANT - prevents segfaults)
ulimit -s 65520  # macOS - add to ~/.zshrc
# or
ulimit -s unlimited  # Linux - add to ~/.bashrc

# Build
mkdir build && cd build
cmake ..
cmake --build .
# Or parallel: make -j4
```

### Common Tasks

```bash
# Full rebuild
cd build
cmake --build . --clean-first

# Rebuild specific program
cmake --build . --target SSPs_HR
# Or: make SSPs_HR

# Run tests
ctest
# or verbose
ctest -V

# Install
sudo cmake --install .

# Uninstall
sudo rm /usr/local/bin/{SSPs_HR,spectra_HR,scenarios_HR,fitstodat,colors_HR,lick,compare_fits,calib_HR}
```

### Build Variants

```bash
# Debug build (with bounds checking)
cmake -DCMAKE_BUILD_TYPE=Debug ..

# Release build (optimized)
cmake -DCMAKE_BUILD_TYPE=Release ..

# Custom install location
cmake -DCMAKE_INSTALL_PREFIX=$HOME/pegase ..

# Use specific compiler
cmake -DCMAKE_Fortran_COMPILER=ifort ..
```

### Development Workflow

```bash
# 1. Edit source files in src/
vim src/SSPs_HR.f90

# 2. Rebuild (from build directory)
cmake --build .

# 3. Test your changes
./SSPs_HR

# 4. Run full test suite
ctest
```

## Available Programs

- `SSPs_HR` - Simple Stellar Populations
- `spectra_HR` - Spectral synthesis
- `scenarios_HR` - Scenario management
- `colors_HR` - Color calculations
- `lick` - Lick indices
- `calib_HR` - Calibration
- `compare_fits` - FITS comparison
- `fitstodat` - FITS to ASCII conversion

## Troubleshooting

```bash
# CFITSIO not found
brew install cfitsio  # then rebuild

# Clean everything
cd build && rm -rf * && cmake .. && cmake --build .

# Check what would be installed
cmake --install . --prefix /tmp/test-install

# Verbose build to see compiler commands
cmake --build . --verbose

# See detailed error output
cmake .. 2>&1 | less
```

## File Locations

- Source code: `src/*.f90`
- Data files: `data/`
- Documentation: `doc/`
- Build files: `build/` (CMake) or `bin/${ARCHBIN}/` (autoconf)
- Configuration template: `src/peg_config.f90.in`

## Environment Variables

```bash
# Optional: Set PEGASE-HR root
export PEGASE_HR_ROOT=/path/to/PEGASE-HR3

# Add to PATH (if not installed system-wide)
export PATH=$PEGASE_HR_ROOT/build:$PATH
```
