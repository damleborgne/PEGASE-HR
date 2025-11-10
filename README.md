# PEGASE-HR

High resolution (and .fits) version of the PEGASE.2 code

## Quick Start

### Modern Build (CMake - Recommended)

```bash
# Install dependencies (macOS)
brew install cmake gcc cfitsio

# Or on Linux (Ubuntu/Debian)
sudo apt-get install cmake gfortran libcfitsio-dev

# Build
mkdir build && cd build
cmake ..
cmake --build .
# Or for faster parallel build: make -j4

# Important: Increase stack size to avoid segfaults
ulimit -s 65520  # macOS
# or: ulimit -s unlimited  # Linux

# Test
ctest

# Install (optional)
sudo cmake --install .
```

### Legacy Build (autoconf)

```bash
autoconf
./configure
make
```

For detailed installation instructions, see [INSTALL.md](INSTALL.md).

## About

PEGASE-HR is a stellar population synthesis code that computes high-resolution spectra 
for simple stellar populations (SSPs) and composite stellar populations following 
arbitrary star formation histories.

### Features
- High-resolution spectral synthesis
- FITS file support via CFITSIO
- Multiple stellar libraries (ELODIE, etc.)
- Photometric colors and Lick indices
- OpenMP parallelization support

## Documentation

See the `doc/` directory for detailed documentation.

## License

Copyright 2004-2025, D. Le Borgne et al.
