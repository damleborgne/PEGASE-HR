cmake_minimum_required(VERSION 3.15)
project(PEGASE-HR Fortran)

# Read version from VERSION file
file(STRINGS "${CMAKE_SOURCE_DIR}/VERSION" PEGASE_VERSION)
string(STRIP "${PEGASE_VERSION}" PEGASE_VERSION)

# Set default build type
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)
endif()

# Project version
message(STATUS "Building PEGASE-HR version ${PEGASE_VERSION}")

# Option to enable/disable OpenMP
option(USE_OPENMP "Enable OpenMP parallelization (recommended only for long calculations)" OFF)

# Set Fortran compiler flags
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    # Enable preprocessor (-cpp) for #ifdef directives in Fortran
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fallow-argument-mismatch -Wall -cpp")
    if(USE_OPENMP)
        # -fopenmp automatically defines _OPENMP, no need for add_definitions()
        set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fopenmp")
        message(STATUS "OpenMP: ENABLED")
    else()
        message(STATUS "OpenMP: DISABLED (use -DUSE_OPENMP=ON to enable)")
    endif()
    set(CMAKE_Fortran_FLAGS_RELEASE "-O4")
    set(CMAKE_Fortran_FLAGS_DEBUG "-g -O0 -fbounds-check")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    # Enable preprocessor (-fpp) for #ifdef directives in Fortran
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -warn all -fpp")
    if(USE_OPENMP)
        # -qopenmp automatically defines _OPENMP, no need for add_definitions()
        set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -qopenmp")
        message(STATUS "OpenMP: ENABLED")
    else()
        message(STATUS "OpenMP: DISABLED (use -DUSE_OPENMP=ON to enable)")
    endif()
    set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
    set(CMAKE_Fortran_FLAGS_DEBUG "-g -O0 -check all")
endif()

# Find required dependencies
find_package(PkgConfig REQUIRED)

# Try to find CFITSIO using pkg-config first
pkg_check_modules(CFITSIO cfitsio)

if(NOT CFITSIO_FOUND)
    # Fallback: try to find it manually
    find_library(CFITSIO_LIBRARIES NAMES cfitsio
        HINTS
            /usr/local/lib
            /opt/homebrew/lib
            /usr/lib
    )
    find_path(CFITSIO_INCLUDE_DIRS fitsio.h
        HINTS
            /usr/local/include
            /opt/homebrew/include
            /usr/include
    )
    if(CFITSIO_LIBRARIES AND CFITSIO_INCLUDE_DIRS)
        set(CFITSIO_FOUND TRUE)
        message(STATUS "Found CFITSIO: ${CFITSIO_LIBRARIES}")
    else()
        message(FATAL_ERROR "CFITSIO not found. Please install it:\n"
            "  macOS: brew install cfitsio\n"
            "  Ubuntu/Debian: sudo apt-get install libcfitsio-dev\n"
            "  CentOS/RHEL: sudo yum install cfitsio-devel")
    endif()
else()
    # pkg-config found it, use the link flags it provides
    message(STATUS "Found CFITSIO via pkg-config: ${CFITSIO_LINK_LIBRARIES}")
endif()

# Set PEG_ROOT for the Fortran source
set(PEG_ROOT "${CMAKE_SOURCE_DIR}" CACHE PATH "PEGASE-HR root directory")
string(LENGTH "${PEG_ROOT}" PEG_ROOT_LEN)

# Configure peg_config.f90 (this is an include file, not a standalone module)
configure_file(
    "${CMAKE_SOURCE_DIR}/src/peg_config.f90.in"
    "${CMAKE_BINARY_DIR}/peg_config.f90"
    @ONLY
)

# Include directories (peg_config.f90 will be found via include statement)
include_directories(
    ${CMAKE_BINARY_DIR}
    ${CMAKE_SOURCE_DIR}/src
    ${CFITSIO_INCLUDE_DIRS}
)

# Source files
# Note: peg_config.f90 is NOT listed here - it's an include file used by constants.f90
set(PEGASE_COMMON_SOURCES
    src/nrtype.f90
    src/constants.f90
    src/types.f90
    src/types_SSPs.f90
    src/util.f90
    src/fits_spec_io.f90
    src/data_io.f90
    src/stellib_io.f90
    src/scenario_io.f90
    src/ssp_io.f90
    src/pegase_func.f90
    src/SSPsubr.f90
    src/compute_scenario.f90
)

# Link libraries
set(PEGASE_LINK_LIBRARIES
    ${CFITSIO_LINK_LIBRARIES}
    m
)

# Executables
add_executable(SSPs_HR src/SSPs_HR.f90 ${PEGASE_COMMON_SOURCES})
target_link_libraries(SSPs_HR ${PEGASE_LINK_LIBRARIES})

add_executable(spectra_HR src/spectra_HR.f90 ${PEGASE_COMMON_SOURCES})
target_link_libraries(spectra_HR ${PEGASE_LINK_LIBRARIES})

add_executable(scenarios_HR src/scenarios_HR.f90 ${PEGASE_COMMON_SOURCES})
target_link_libraries(scenarios_HR ${PEGASE_LINK_LIBRARIES})

add_executable(fitstodat src/fitstodat.f90 ${PEGASE_COMMON_SOURCES})
target_link_libraries(fitstodat ${PEGASE_LINK_LIBRARIES})

add_executable(colors_HR src/colors_HR.f90 ${PEGASE_COMMON_SOURCES})
target_link_libraries(colors_HR ${PEGASE_LINK_LIBRARIES})

add_executable(lick src/lick.f90 ${PEGASE_COMMON_SOURCES})
target_link_libraries(lick ${PEGASE_LINK_LIBRARIES})

add_executable(compare_fits src/compare_fits.f90 ${PEGASE_COMMON_SOURCES})
target_link_libraries(compare_fits ${PEGASE_LINK_LIBRARIES})

add_executable(calib_HR src/calib_HR.f90 ${PEGASE_COMMON_SOURCES})
target_link_libraries(calib_HR ${PEGASE_LINK_LIBRARIES})

# Installation - install locally in bin/ subdirectory
install(TARGETS SSPs_HR spectra_HR scenarios_HR fitstodat colors_HR lick compare_fits calib_HR
    RUNTIME DESTINATION ${CMAKE_SOURCE_DIR}/bin
)

# Configure shell scripts for runtime
configure_file(
    "${CMAKE_SOURCE_DIR}/peg_bashrc.in"
    "${CMAKE_BINARY_DIR}/peg_bashrc"
    @ONLY
)
configure_file(
    "${CMAKE_SOURCE_DIR}/peg_cshrc.in"
    "${CMAKE_BINARY_DIR}/peg_cshrc"
    @ONLY
)

install(FILES
    ${CMAKE_BINARY_DIR}/peg_bashrc
    ${CMAKE_BINARY_DIR}/peg_cshrc
    DESTINATION ${CMAKE_SOURCE_DIR}/bin
)

message(STATUS "")
message(STATUS "PEGASE-HR Configuration Summary:")
message(STATUS "  Version: ${PEGASE_VERSION}")
message(STATUS "  Build type: ${CMAKE_BUILD_TYPE}")
message(STATUS "  Fortran compiler: ${CMAKE_Fortran_COMPILER}")
message(STATUS "  CFITSIO libraries: ${CFITSIO_LIBRARIES}")
message(STATUS "  CFITSIO include: ${CFITSIO_INCLUDE_DIRS}")
message(STATUS "  Install prefix: ${CMAKE_INSTALL_PREFIX}")
message(STATUS "")
