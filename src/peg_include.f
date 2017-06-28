cccccccccccccccccccccccccccccccccccccccccccc
cc INPUT CONSTANTS
cccccccccccccccccccccccccccccccccccccccccccccccccc
cc STELLIB CONSTANTS
      integer nmaxstellibs
      parameter(nmaxstellibs = 10)
    
c nmaxZl: Maximum number of Z planes in the "stellib"
c  (used in the programs "SSPs" "spectra" and "stellib_io")
      integer nmaxZl  
      parameter (nmaxZl=10)
c nmaxsl: Maximum number of spectra per Z plane in the "stellib"
c  (used in the programs "SSPs" and "stellib_io")
      integer nmaxsl
      parameter (nmaxsl=500)

c nmaxlambda : Maximum number of wavelengths in the output
c  (and input stellar) spectra
      integer nmaxlambda
      parameter (nmaxlambda=15000)

c nmaxCM : Maximum number of CM spectra
      integer nmaxCM
      parameter(nmaxCM=50)

c nmaxlambdaLCB
      integer nmaxlambdaLCB
      parameter (nmaxlambdaLCB=1221)

c nmaxlambdaCM
      integer nmaxlambdaCM
      parameter(nmaxlambdaCM=1300)

c stellar HR diagram
      integer nmaxspecLCB,nmaxspec
      parameter(nmaxspecLCB=4500,nmaxspec=4500)

cccccccccccccccccccccccccccccccccccccccccccccccccc
cc TRACKS CONSTANTS
      integer nmaxZtracks
      parameter(nmaxZtracks=7)

c nmaxMS : Maximum number of ZAMS tracks
      integer nmaxMS
      parameter(nmaxMS=200)

cccccccccccccccccccccccccccccccccccccccccccccccccc
cc isochrones CONSTANTS (output of SSPs_HR.f)
      integer nmaxtimesSSP
      parameter(nmaxtimesSSP=600) ! >= 516, actually


cccccccccccccccccccccccccccccccccccccccccccccccccc
cc IMF CONSTANTS
      integer nmaxIMFbins,nmaxIMFfiles
      parameter(nmaxIMFbins=100)
      parameter(nmaxIMFfiles=100)

cccccccccccccccccccccccccccccccccccccccccccccccccc
cc Dust CONSTANTS
      integer nmaxlambdaext
      parameter(nmaxlambdaext=300)     

cccccccccccccccccccccccccccccccccccccccccccc
cc OUTPUT CONSTANTS
cccccccccccccccccccccccccccccccccccccccccccc

cc output spectra CONSTANTS

      integer nmaxtimes
      parameter(nmaxtimes=20002)

      integer nmaxotimes !! max number of output times
      parameter(nmaxotimes=2000)

      integer nmaxparams
      parameter(nmaxparams=100)
      
      integer nmaxlines ! emission lines
      parameter(nmaxlines=100)
      
cc colors CONSTANTS
      integer nmaxfilters
      parameter(nmaxfilters=500)

      integer nmaxlambdatrans
      parameter(nmaxlambdatrans=5000)

cc Lick
      integer nmaxlick
      parameter(nmaxlick=100)
