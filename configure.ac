AC_PREREQ(2.5)
AC_INIT([PEGASE.HR], - , [leborgne@iap.fr]) 
AC_COPYRIGHT([Copyright 2013 PEGASE development team])

# store variables from the environment (may be set or not)
arch=$ARCH
cc=$CC
cpp=$CPP
cflags=$CFLAGS
cppflags=$CPPFLAGS
dflags=$DFLAGS
f90=$F90
fflags=$FFLAGS
f90flags=$F90FLAGS
ld=$LD
ldflags=$LDFLAGS
libs=$LIBS
libdirs=$LIBDIRS
ar=$AR
arflags=$ARFLAGS

#archbin=`uname -m`
ARCHBIN=`config.guess`
echo ARCHBIN is now $ARCHBIN

# Define version by reading the VERSION	file
version=`cat VERSION`
AC_DEFINE_UNQUOTED(PL_VERSION, "$version")
AC_MSG_RESULT([Starting configuration for PEGASE.HR package])
AC_SUBST(version)


#----------------------------------------------------------------------------
# Define the root directory of PEGASE.HR in Makefile and peg_config.f
PEG_ROOT=`pwd`
AC_SUBST(PEG_ROOT)

#----------------------------------------------------------------------------
# Can we install here?
if test -f configure; then 
  test -w configure || AC_MSG_ERROR([
*******************************************************************
PEGASE.HR configuration failed : 
Permission to write files in the current directory is not granted.
Therefore, it will not be possible to install the compiled files...
*******************************************************************
])
fi



cd cfitsio && ./configure && make && cd ..

AC_SEARCH_LIBS(fabs, m)

#libs="$LIBS -L../cfitsio -lcfitsio -lm -fopenmp"
#LIBS="$LIBS -Lcfitsio -lcfitsio -lm -fopenmp"
libs="$LIBS -L../cfitsio -lcfitsio -lm"
LIBS="$LIBS -Lcfitsio -lcfitsio -lm"

AC_CHECK_LIB(cfitsio,ffukye, [], [
        echo "cfitsio not found. You have two options :"
	echo "1) Compile it from source by copy-pasting the following line:"
        echo "   cd cfitsio ; ./configure ; make distclean ; make; cd .."
        echo "   Then run ./configure again."
	echo "2) If libcfitsio.a exists on your host, specify its path with "
	echo "   the option './configure --with-libraries=mypath'"
        exit -1
        ])


#----------------------------------------------------------------------------
# Fortran 90 compiler

ranlib="ranlib"
try_ar="ar"
try_arflags="ruv"

try_f90="f95 gfortran xlf95 fort ifort ifc efc f95 g95 pgf95 lf95 ftn xlf90 f90 pgf90 pghpf epcf90"
if test "$f90" = "" ; then f90="$try_f90" ; fi

AC_PROG_FC($f90, [90])
f90=$FC

try_f90flags="\$(FFLAGS)"

# fflags : O4 is perfect. O2 is slower. stack_size adjustemnt is needed with openmp (or use allocatable arrays in the code...)

#myopts="-fopenmp -Wl,-stack_size,0x10000000000 -ffree-form -std=f95 -pedantic-errors -Wall -Wconversion -Wimplicit-interface -Wunderflow -Wextra -Wunreachable-code -ffpe-trap=invalid,zero,overflow,underflow -g3 -fbounds-check -O0 -fstack-protector-all -fall-intrinsics"

#myopts="-fopenmp -Wl,-stack_size,0x10000000000 -ffree-form  -pedantic-errors -Wconversion -Wunderflow -Wextra -Wunreachable-code -ffpe-trap=invalid,zero,overflow,underflow -g3 -fbounds-check -O0 -fstack-protector-all -fall-intrinsics"
#myopts="-fopenmp -Wl,-stack_size,0x10000000000"
#try_fflags="$myopts"
#ldflags="$myopts"

##################################################
#-> ld: O2=perfect, O4 is slower !
myopts="-O4 -fopenmp -Wl,-stack_size,0x10000000000"
try_fflags="$myopts"
ldflags="-O4 -fopenmp -Wl,-stack_size,0x10000000000"
##################################################

if test "$fflags" = ""   ; then fflags=$try_fflags     ; fi
if test "$f90flags" = "" ; then f90flags=$try_f90flags ; fi
if test "$fflags_noopt" = ""   ; then fflags_noopt=$try_fflags_noopt     ; fi
if test "$f90flags_noopt" = "" ; then f90flags_noopt=$try_f90flags_noopt ; fi

echo setting FFLAGS... $fflags
echo setting F90FLAGS... $f90flags
echo setting FFLAGS_NOOPT... $fflags_noopt
echo setting F90FLAGS_NOOPT... $f90flags_noopt

# linker and archiver (no tests)
if test "$ld" = ""       ; then ld="$f90"              ; fi
if test "$ldflags" = ""  ; then ldflags="$try_ldflags" ; fi
if test "$ar" = ""       ; then ar="$try_ar"           ; fi
if test "$arflags" = ""  ; then arflags="$try_arflags" ; fi

echo setting LD... $ld
echo setting LDFLAGS... $ldflags
echo setting AR... $ar
echo setting ARFLAGS... $arflags
echo setting LIBS... $libs


# compilation rules
AC_PROG_MAKE_SET

f90rule="\$(F90) \$(F90FLAGS) \$(MODULEFLAG) -c \$<"

# export settings to generated files
AC_SUBST(dflags)
AC_SUBST(fdflags)
AC_SUBST(cpp)
AC_SUBST(cppflags)
AC_SUBST(f90)
AC_SUBST(fflags)
AC_SUBST(f90flags)
AC_SUBST(fflags_noopt)
AC_SUBST(f90flags_noopt)
AC_SUBST(pre_fdflags)
AC_SUBST(ld)
AC_SUBST(ldflags)
AC_SUBST(libs)
AC_SUBST(ar)
AC_SUBST(arflags)
AC_SUBST(ranlib)
AC_SUBST(f90rule)
AC_SUBST(ARCHBIN)

AC_MSG_NOTICE([
*********************************************************************
SUGGESTED COMPILATION OPTION IF YOU'RE ON A MAC WITH SNOW LEOPARD:
You can set the F90FLAGS environament variable to "-O3 -m64" 
to enable 64 bit mode, and run ./configure again.
*********************************************************************
])

AC_CONFIG_FILES([make.sys make.rules peg_cshrc peg_bashrc])
rm -rf config.status autom4te.cache && AC_MSG_RESULT(configure: removing ./config.status and autom4te.cache/)


AC_OUTPUT

chmod u+x peg_bashrc peg_cshrc

