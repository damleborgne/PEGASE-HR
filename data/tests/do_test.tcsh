#!/bin/tcsh -f
set noexit = 0
if ($# == 1) then
    set noexit = 1
endif

set diffopts='--brief'

source peg_cshrc

mkdir -p test_tmp

cd test_tmp

cp ../data/tests/ages.dat ./


rm -f all_tests.log
touch all_tests.log
#--------------------------------------------------
# Builds output spectra with spectra_HR and tests them
#--------------------------------------------------
rm -f test_1.fits
rm -f test_2.fits
echo '----------------------------------------------------------------------'
echo 'Test 1/4 :'
echo 'Testing spectra_HR : computing a scenario with the BaSeL library....'
../bin/spectra_HR  ../data/tests/tests_LCB.scn >>& all_tests.log
echo '----------------------------------------------------------------------' >> all_tests.log
echo 'Test 1/4 :'>> all_tests.log
echo 'Testing spectra_HR : computing a scenario with the BaSeL library....' >> all_tests.log
../bin/compare_fits test_1.fits ../data/tests/test_1.fits >& test.log
cat test.log >> all_tests.log

if ( -z test.log) then
 echo Successful !
else
  echo ERROR : FILES SEEM TO DIFFER !
  echo 'Diff file :'
  cat test.log  
  if ($noexit != 1) then
     exit
  endif
endif
rm -f test.log

echo '----------------------------------------------------------------------'
echo 'Test 2/4 :'
echo 'Testing spectra_HR : computing a scenario with the BaSeL library....'
echo '----------------------------------------------------------------------' >> all_tests.log
echo 'Test 2/4 :' >> all_tests.log
echo 'Testing spectra_HR : computing a scenario with the BaSeL library....' >> all_tests.log

../bin/compare_fits test_2.fits ../data/tests/test_2.fits >& test.log
cat test.log >> all_tests.log
if ( -z test.log) then
 echo Successful !
else
  echo ERROR : FILES SEEM TO DIFFER !
  echo 'Diff file :'
  cat test.log  
  if ($noexit != 1) then
     exit
  endif
endif
rm -f test.log



echo '----------------------------------------------------------------------'
echo 'Test 3/4 :'
echo 'Testing spectra_HR : computing a scenario with the ELODIE library....'
echo '----------------------------------------------------------------------' >> all_tests.log
echo 'Test 3/4 :' >> all_tests.log
echo 'Testing spectra_HR : computing a scenario with the ELODIE library....' >> all_tests.log
rm -f test_ELO.fits
../bin/spectra_HR  ../data/tests/tests_ELODIE.scn >>& all_tests.log
../bin/compare_fits test_ELO.fits ../data/tests/test_ELO.fits >& test.log
cat test.log >> all_tests.log

if ( -z test.log) then
 echo Successful !
else
  echo ERROR : FILES SEEM TO DIFFER !
  echo 'Diff file :'
  cat test.log  
  echo --------------
  echo Please make sure that you have downloaded the file stellibELODIE.fits
  echo and that it is now in the data/stellibs/ directory.
  echo Otherwise, download it from http://www2.iap.fr/pegase/pegasehr/package/stellibELODIE_3.1.fits 
  echo and put it in the data/stellibs/ directory.   
  if ($noexit != 1) then
     exit
  endif

endif
rm -f test.log


#--------------------------------------------------
# Measures Lick indices on an 'ELODIE' spectrum and tests
#--------------------------------------------------

echo '----------------------------------------------------------------------'
echo 'Test 4/4 :'
echo 'Testing lick.f : measuring Lick indices on a spectrum'
echo '----------------------------------------------------------------------' >> all_tests.log
echo 'Test 4/4 :' >> all_tests.log
echo 'Testing lick.f : measuring Lick indices on a spectrum' >> all_tests.log
../bin/lick test_ELO.fits 5. >>& all_tests.log
../bin/compare_fits test_ELO.fits ../data/tests/test_ELO.fits >& test.log
cat test.log >> all_tests.log

if ( -z test.log) then
 echo Successful !
else
  echo ERROR : FILES SEEM TO DIFFER !
  echo 'Diff file :'
  cat test.log  
  if ($noexit != 1) then
     exit
  endif

endif
rm -f test.log
echo "Done."
echo "----------------"
echo "Done." >> all_tests.log
echo "----------------" >> all_tests.log
