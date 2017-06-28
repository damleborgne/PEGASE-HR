# Makefile for PEGASE.HR package; version 2004/07/05
## Ph. Prugniel, prugniel@obs.univ-lyon1.fr
## This Makefile is automatically generated by the "configure" command,
## fron the file Makefile.in
##
## If you need to modify the Makefile, for example if you like to add
## an application of a routine, you must do it in Makefile.in and
## run again configure (the make command will do it for you if Makefile.in
## is newer than Makefile).
## 
## How to use this file:
##    make
##      will install or update the library (lib/libpegase.a) and all
##      the executable programs listed in the variable SRCAPP (see below).
##      The executables are put in the directory ./bin, to use them you have
##      to add this directory to the your PATH variable, or give explicitely
##      the full path to the application.
##    make lib/libpegase.a
##      will only install or update the library.
##    make <myprogram>
##      compile your program: <myprogram>.f with the pegase library
##    make tar
##      prepare an archive for distribution 
##      note: this target uses "tar -czf" which is a GNU extension and may not
##      be suported by all versions of "tar"
############################################################################
PEGASE_ROOT = /Users/leborgne/central/astro/models/pegase/PEGASE-HR

CONFIGOPTIONS = 

BINDIR = ${PEGASE_ROOT}/bin
LIBDIR = ${PEGASE_ROOT}/lib
SRC = src

F77 = gfortran
FFLAGS = -g -O4 -fno-backslash -fdollar-ok -ffixed-line-length-none -fno-automatic
LIBRARIES = -L${LIBDIR} -lpegase -lcfitsio  -Lcfitsio -lcfitsio -lm

## If you add or remove an application program, edit the two variables:
## APPLIC ad SRCAPP below...

APPLIC = ${BINDIR}/SSPs_HR  ${BINDIR}/spectra_HR ${BINDIR}/fitstodat\
	${BINDIR}/scenarios_HR ${BINDIR}/lick ${BINDIR}/colors_HR\
	${BINDIR}/calib_HR ${BINDIR}/compare_fits

SRCAPP = ${SRC}/SSPs_HR.f  ${SRC}/spectra_HR.f ${SRC}/fitstodat.f\
	${SRC}/scenarios_HR.f ${SRC}/lick.f ${SRC}/colors_HR.f\
	${SRC}/calib_HR.f ${SRC}/compare_fits.f

### If you add/remove files from the library, edit the variable SRCSUB

SRCSUB =  ${SRC}/SSPsubr.f ${SRC}/fits_spec_io.f ${SRC}/scenario_io.f \
	${SRC}/util.f ${SRC}/pegase_func.f ${SRC}/data_io.f \
	${SRC}/stellib_io.f ${SRC}/ssp_io.f

LIBPEGASE=${LIBDIR}/libpegase.a


############################################################################
## Do not edit below
all:  Makefile ${BINDIR} ${LIBDIR} ${LIBPEGASE} ${APPLIC} calib

calib: data/user_defined/filters.dat
	@ cd data/user_defined/; ../../bin/calib_HR

${APPLIC}: ${SRCAPP} ${LIBPEGASE} 
	@ ${RM} -f $@;\
	${F77} ${FFLAGS} ${SRC}/${@F}.f ${LIBRARIES} \
	${INCL} -o ${BINDIR}/${@F} ||\
	echo "PEGASE.HR: failed to install: ${EXEC}${@F}"

${LIBPEGASE}:  ${SRCSUB} ${SRC}/peg_config.f ${SRC}/peg_include.f 
	 @ ${F77} ${FFLAGS} -c ${SRCSUB} ${INCL} && ar -csr ${LIBPEGASE} *.o &&\
	 ${RM} -f *.o 


.f:$<
	@ ${F77} ${FFLAGS} $<  ${LIBRARIES} ${INCL} -o  $@ ||\
	echo "PEGASE: failed to install: ${EXEC}${@F}"

# The file peg_config defines the variable PEG_ROOT
#   It is computed from peg_config.f.in, using sh to expand variables
${SRC}/peg_config.f: ${SRC}/peg_config.f.in Makefile
	 @ PEG_ROOT=${PWD}/; PEG_ROOT_LEN=`echo ${PWD}|wc -c`; \
	   export PEG_ROOT; export PEG_ROOT_LEN;\
	   (echo "cat <<EOF"; cat ${SRC}/peg_config.f.in; echo EOF) | sh > $@

${BINDIR}:
	mkdir -p $@
${LIBDIR}:
	mkdir -p $@


Makefile: Makefile.in configure
	@ ./configure ${CONFIGOPTIONS}
	@echo
	@echo "----------------------------------------------------------------"
	@echo "Configuration had to be redone... please type your command again"
	@echo "----------------------------------------------------------------"
	exit 1;

configure: configure.in
	autoconf

tar: pegasehr.tar.gz
devel : pegasehr_devel.tar.gz

pegasehr.tar.gz: INSTALL README VERSION  configure* *.in src/ data/ doc/
	cd ../; tar -czf $@ PEGASE-HR/INSTALL PEGASE-HR/README PEGASE-HR/VERSION  PEGASE-HR/configure* PEGASE-HR/*.in PEGASE-HR/src/ PEGASE-HR/data/ PEGASE-HR/doc/ PEGASE-HR/make_test

pegasehr_devel.tar.gz: INSTALL README VERSION TODO configure* *.in src/ data/ doc/
	cd ../; tar -czf $@ PEGASE-HR/INSTALL PEGASE-HR/README PEGASE-HR/VERSION PEGASE-HR/TODO PEGASE-HR/configure* PEGASE-HR/*.in PEGASE-HR/src/ PEGASE-HR/data/ PEGASE-HR/doc/ PEGASE-HR/make_test --exclude PEGASE-HR/data/stellibs/stellibLCBcor.fits --exclude PEGASE-HR/data/stellibs/stellibELODIE.fits 

clean:
	@ find . -name \*~ -exec rm -f "{}" \;
	@ find . -name core -exec rm -f "{}" \;
	@ rm -f bin/*
	@ rm -f lib/*
	@ rm -rf test_tmp

dsitclean:
	clean
	@ rm -rf autom4te*
	@ rm -f configure.scan
	@ rm -f Makefile
	@ cd cfitsio; make distclean


test:   all
	@ tcsh -f ${PWD}/data/tests/do_test.tcsh

#############################################################################
