
-include make.sys # to get ${ARCHBIN}

default : peg

bindir :
	@ test -d bin || mkdir bin
	@ test -d bin/${ARCHBIN} || mkdir bin/${ARCHBIN}
	@ cp peg_cshrc peg_bashrc bin/${ARCHBIN}

#all : make.sys peg

#make.sys : configure
#	./configure ${CONFIGOPTIONS}

configure : configure.ac
	#make veryclean
	autoconf
	rm -rf autom4te.cache

peg :  bindir
	@ echo Making PEGASE.HR
	@ if test -d src ; then (cd src; make all) ; fi

clean :
	-rm -f src/*.o src/*.mod src/SSPs_HR src/spectra_HR src/scenarios_HR src/fitstodat src/colors_HR src/lick src/compare_fits src/calib_HR
	-rm -rf bin/*

veryclean : 
	-rm -f src/*.o src/*.mod src/SSPs_HR src/spectra_HR src/scenarios_HR src/fitstodat src/colors_HR src/lick src/compare_fits src/calib_HR
	-rm -rf bin/*
	-rm -f src/peg_config.f90 make.rules make.sys config.log config.status peg_cshrc peg_bashrc configure
	-rm -rf bin/*
	-rm -rf data/tests/test_tmp/*
	-find . -name "*~" -exec rm "{}" \;
	-find . -name "#*" -exec rm "{}" \;
	-cd cfitsio; make clean
	

