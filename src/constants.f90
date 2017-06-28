MODULE constants
  use nrtype

  integer, PARAMETER :: nmaxstellibs   = 10
  integer, PARAMETER :: nmaxZtracks    = 7    ! number of stellar tracks
  integer, PARAMETER :: nmaxMS         = 200  ! Maximum number of ZAMS tracks
  integer, PARAMETER :: nmaxZl         = 10   ! Maximum number of Z planes in the "stellib"
  integer, PARAMETER :: nmaxsl         = 500  ! Maximum number of spectra per Z plane in the "stellib"
  integer, PARAMETER :: nmaxotimes     = 2000 ! max number of output times
  integer, PARAMETER :: nmaxtimes      = 20002! simulation timesteps
  integer, PARAMETER :: nmaxtimesSSP   = 600  ! >= 516, actually
  integer, PARAMETER :: nmaxparams     = 100  ! Output parameters
  integer, PARAMETER :: nmaxlines      = 100  ! Emission lines per spectrum
  integer, PARAMETER :: nmaxfilters    = 500  ! Number of filters (colors)
  integer, PARAMETER :: nmaxlambdatrans= 5000 ! Transmission curves (colors)
  integer, PARAMETER :: nmaxlick       = 100  ! Max number of Lick indices


  integer, PARAMETER :: nmaxCM         = 50   ! Maximum number of CM spectra
  integer, PARAMETER :: nmaxspecLCB    = 4500 ! stellar HR diagram
  integer, PARAMETER :: nmaxspec       = 4500 ! number of spectra per library
  integer, PARAMETER :: nmaxlambda     = 15000! default ELODIE spectra
  integer, PARAMETER :: nmaxlambdaLCB  = 1221 ! Lejeune spectra
  integer, PARAMETER :: nmaxlambdaCM   = 1300 ! Clegg&Middlemass spectra
  integer, PARAMETER :: nmaxlambdaext  = 300  ! Dust attenuation
  integer, PARAMETER :: nmaxIMFbins    = 100  ! IMF sampling mass bins
  integer, PARAMETER :: nmaxIMFfiles   = 100  ! IMF files

  integer, PARAMETER :: nmaxscenarios  = 10000  !

  integer, PARAMETER :: strmax         = 280  ! length of filenames strings

  integer, PARAMETER :: verbose        = 2

  real, PARAMETER :: Zsol = 0.02
  REAL, PARAMETER :: c = 2.99792458e18 ! in angstrom/s

  integer,parameter :: nslab1=17,nslab2=9,nslab3=6,nslab4=10
  integer,parameter :: nK1=12,nK2=9,nK3=11

  integer, parameter :: nCMspectra = 31

  include 'peg_config.f90'

END MODULE constants
