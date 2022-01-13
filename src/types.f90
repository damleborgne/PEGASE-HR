MODULE types

  USE constants
  USE nrtype

  implicit none

  !##################################################
  TYPE t_scenario
     integer                          :: number 
     character(len=strmax)            :: filespectra
     REAL                             :: Zgasinit
     REAL                             :: tinfall
     REAL                             :: Zinfall
     REAL,DIMENSION(nmaxparams+2)     :: SFRparam
     INTEGER                          :: infall
     character(len=strmax)            :: fileSFR
     INTEGER                          :: codeZ
     REAL                             :: ZSFRinit
     REAL                             :: fsub
     REAL                             :: twind
     character(len=72)                :: answerneb
     character(len=72)                :: answerinfall
     character(len=72)                :: answerz
     character(len=72)                :: answerwind
     character(len=72)                :: answerext
     INTEGER                          :: codeext
     REAL                             :: inclin     
     REAL                             :: fSNIa
  end TYPE t_scenario

  TYPE(t_scenario), dimension(nmaxscenarios), SAVE :: scenarios
  type(t_scenario), SAVE    :: myscen

  !##################################################
  TYPE t_gal_properties
     real,dimension(nmaxtimes)       :: Zgas, Zstars, ZSFR, SFR, SFRlum, agestars, Mgal
     real,dimension(nmaxtimes)       :: sigmaBHNS, sigmaWD, sigmastars, sigmagas, sigmasub
     integer,dimension(nmaxtimes)    :: iZinf,iZsup ! min max max Ztrack used at each timestep
     integer                         :: excessSFR  ! Time when SFR exceeded max, or 0

     real,dimension(nmaxlambda,nmaxotimes) :: continuum_spectra
     real,dimension(nmaxlines,nmaxotimes)  :: lines_spectra
     real,dimension(nmaxotimes)     :: Zbol,fluxbol,nSNIatot,nSNIItot,agebol
     real(DP),dimension(nmaxotimes) :: nLymtot
     real,dimension(nmaxotimes)     :: fluxext, tauV
     integer                        :: nlines
     real,dimension(nmaxlines)      :: lambdaline

  end TYPE t_gal_properties

  type(t_gal_properties), SAVE :: galprop

  !##################################################
  TYPE t_stellibinfo
     character(len=strmax)               :: filename,filename_short
     integer                             :: Lfits  ! Unit number for accessing FITS file
     character(len=30)                   :: grid_type
     integer                             :: nz  ! number of Z bins 
     integer                             :: nspectot ! number of spectra in library
     integer,dimension(nmaxZl)           :: nspecZ
     integer, dimension(nmaxZl+1)        :: firstspec ! Index of 1st spec in each Z bin
     real(dp), dimension(nmaxZl)         :: Z
     integer                             :: nlambda
     real, dimension(:),allocatable      :: lambda
     real, dimension(:,:),allocatable    :: spectra
     real(DP),dimension(nmaxZl,-nmaxCM:nmaxsl)  :: Tspec,gspec,NHI,NHeI,NHeII ! includes CM lib.

  end type t_stellibinfo

  type(t_stellibinfo), SAVE :: stellibinfo, stellibCM
  !##################################################
  TYPE t_SSP_SP ! single precision in spectra_HR is enough
     character(len=strmax)                        :: fileSSPs !pack of SSPs
     character(len=strmax)                        :: fileSSPs_prefix
     character(len=strmax),dimension(nmaxZtracks) :: fileSSPZ ! filenames
     character(len=100),dimension(4,nmaxZtracks)  :: header

     real,dimension(nmaxZtracks)                  :: ZSSP ! metallicities
     integer                                      :: nZ

     real                                         :: fSNIa ! fraction of close binaries, used to compute nSNIa
     real,dimension(nmaxZtracks,nmaxtimes)       :: massalive, massBHNS, massWD,nSNIa, nSNII
     real,dimension(nmaxZtracks,nmaxtimes)       ::  NLym

     integer,dimension(nmaxtimes)                         :: invtime
     integer                                              :: ntimes ! used by ncountused
     integer,dimension(nmaxtimesSSP,nmaxZtracks)          :: nused  ! used by ncountused
     integer,dimension(nmaxspec,nmaxtimesSSP,nmaxZtracks) :: iused  ! used by ncountused

     real,dimension(nmaxtimes)                :: alpha, beta ! interpolation factors in Z and time
     real,dimension(nmaxZtracks,nmaxtimes)    :: ejecta, ejectaZ
     integer,dimension(nmaxZtracks)           :: iZ1, iZ2

     real,dimension(nmaxZtracks,nmaxtimesSSP)                     :: fluxbolSSP ! Bolometric flux of an SSP
     real,dimension(-nmaxCM:nmaxspecLCB,nmaxtimesSSP,nmaxZtracks) :: Lspec_SSP  ! Bolometric flux contribution of individual stars to an SSP flux     

  end type t_SSP_SP

  type(t_SSP_SP), SAVE :: SSP

  !##################################################
  type t_timeinfo
     integer                       :: ntimesimpr 
     integer,dimension(nmaxotimes) :: timeimpr
  end type t_timeinfo

  type(t_timeinfo), SAVE :: timeinfo

  !##################################################
  type t_dust
     real :: tauslab(nslab1),albslab(nslab2)
     real :: asymslab(nslab3),inclinslab(nslab4)
     real :: emergslab(nslab1,nslab2,nslab3)
     real :: emerginclinslab(nslab1,nslab2,nslab3,nslab4)
     real ::  tauKing(nK1),albKing(nK2),asymKing(nK3)
     real ::  emergKing(nK1,nK2,nK3)
     real,dimension(5)               ::  Zext, frac
     integer                         ::  nlambdaext
     real,dimension(nmaxlambdaext)   ::  lambdaext
     real,dimension(nmaxlambdaext,2) ::  tauext,albedoext,asymext
  end type t_dust

  type(t_dust), save :: dust

END MODULE types
