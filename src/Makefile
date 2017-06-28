# Makefile for PEGASE_HR binary

include ../make.rules
include ../make.sys

PEGSRC= \
SSPs_HR.f90 \
SSPsubr.f90 \
calib_HR.f90 \
colors_HR.f90 \
compare_fits.f90 \
constants.f90 \
data_io.f90 \
fits_spec_io.f90 \
fitstodat.f90 \
lick.f90 \
nrtype.f90 \
peg_config.f90 \
pegase_func.f90 \
scenario_io.f90 \
scenarios_HR.f90 \
spectra_HR.f90 \
ssp_io.f90 \
stellib_io.f90 \
types.f90 \
types_SSPs.f90 \
util.f90 \
compute_scenario.f90


PEGOBJS= \
pegase_func.o \
ssp_io.o \
SSPsubr.o \
constants.o \
scenario_io.o \
stellib_io.o \
data_io.o \
nrtype.o \
types.o \
types_SSPs.o \
fits_spec_io.o  \
util.o \
compute_scenario.o


default : all

all : peg_config.f90 Makefile SSPs_HR spectra_HR scenarios_HR fitstodat colors_HR lick compare_fits calib_HR

peg_config.f90 : peg_config.f90.in ../make.sys 
	  @ PEG_ROOT=`echo ${PEG_ROOT}`; \
	    PEG_ROOT_LEN=`echo ${PEG_ROOT}|wc -c`; \
	    export PEG_ROOT; export PEG_ROOT_LEN;\
	    (echo "cat <<EOF"; cat peg_config.f90.in; echo EOF) | sh > $@

../make.sys: ../make.sys.in ../configure
	 (cd ..; ./configure ${CONFIGOPTIONS} )
	@ echo '----------------------------------------------------------------'
	@ echo 'Configuration had to be redone... please type your command again'
	@ echo '----------------------------------------------------------------'
	exit 1;

../configure: ../configure.ac
	(cd ..; autoconf)


SSPs_HR : ../make.sys peg_config.f90 SSPs_HR.f90 SSPs_HR.o 
	$(LD) -o $@ SSPs_HR.o SSPsubr.o constants.o types_SSPs.o util.o $(MODULES) $(LDFLAGS)
	- ( cp $@ ../bin/${ARCHBIN}/ )
scenarios_HR : ../make.sys peg_config.f90 scenarios_HR.f90 scenarios_HR.o 
	$(LD) -o $@ scenarios_HR.o constants.o util.o types_SSPs.o $(MODULES) $(LDFLAGS)
	- ( cp $@ ../bin/${ARCHBIN}/ )
spectra_HR : ../make.sys peg_config.f90 spectra_HR.f90 spectra_HR.o 
	$(LD) -o $@ spectra_HR.o compute_scenario.o pegase_func.o fits_spec_io.o constants.o data_io.o nrtype.o scenario_io.o ssp_io.o stellib_io.o types.o util.o $(MODULES) $(LDFLAGS)
	- ( cp $@ ../bin/${ARCHBIN}/ )
lick : ../make.sys peg_config.f90  lick.f90 lick.o 
	$(LD) -o $@ lick.o fits_spec_io.o constants.o types.o util.o $(MODULES) $(LDFLAGS)
	- ( cp $@ ../bin/${ARCHBIN}/ )
fitstodat : ../make.sys peg_config.f90  fitstodat.f90 fitstodat.o
	$(LD) -o $@ fitstodat.o constants.o nrtype.o util.o $(MODULES) $(LDFLAGS)
	- ( cp $@ ../bin/${ARCHBIN}/ )
calib_HR : ../make.sys peg_config.f90  calib_HR.f90 calib_HR.o 
	$(LD) -o $@ calib_HR.o constants.o pegase_func.o types.o util.o $(MODULES) $(LDFLAGS)
	- ( cp $@ ../bin/${ARCHBIN}/ )
colors_HR : ../make.sys peg_config.f90  colors_HR.f90 colors_HR.o 
	$(LD) -o $@ colors_HR.o constants.o fits_spec_io.o types.o util.o $(MODULES) $(LDFLAGS)
	- ( cp $@ ../bin/${ARCHBIN}/ )
compare_fits : ../make.sys peg_config.f90 compare_fits.f90 compare_fits.o 
	$(LD) -o $@ compare_fits.o constants.o fits_spec_io.o types.o util.o $(MODULES) $(LDFLAGS)
	- ( cp $@ ../bin/${ARCHBIN}/ )


clean :
	-rm -f *.o *.mod core ../bin/${ARCHBIN}/* SSPs_HR spectra_HR scenarios_HR fitstodat colors_HR lick compare_fits calib_HR

cleanmod :
	-rm -f *.o *.mod core


include .dependencies
