 MODULE types_ssps

  USE constants
  USE nrtype

  implicit none
 !##################################################
  TYPE t_SSP_DP ! double precision in SSPs_HR for AGB stars in particular

     character(len=strmax)                        :: fileSSPs !pack of SSPs
     character(len=strmax)                        :: fileSSPs_prefix
     character(len=strmax),dimension(nmaxZtracks) :: fileSSPZ ! filenames
     character(len=100),dimension(4,nmaxZtracks)  :: header

     real(DP),dimension(nmaxZtracks)                  :: ZSSP ! metallicities
     integer                                      :: nZ

     real(DP)                                         :: fSNIa ! fraction of close binaries, used to compute nSNIa
     real(DP),dimension(nmaxZtracks,nmaxtimes)       :: massalive, massBHNS, massWD,nSNIa, nSNII
     real(DP),dimension(nmaxZtracks,nmaxtimes)       ::  NLym

     integer,dimension(nmaxtimes)                         :: invtime
     integer                                              :: ntimes ! used by ncountused
     integer,dimension(nmaxtimesSSP,nmaxZtracks)          :: nused  ! used by ncountused
     integer,dimension(nmaxspec,nmaxtimesSSP,nmaxZtracks) :: iused  ! used by ncountused

     real(DP),dimension(nmaxtimes)                :: alpha, beta ! interpolation factors in Z and time
     real(DP),dimension(nmaxZtracks,nmaxtimes)    :: ejecta, ejectaZ
     integer,dimension(nmaxZtracks)               :: iZ1, iZ2

     real(DP),dimension(nmaxZtracks,nmaxtimesSSP)                     :: fluxbolSSP ! Bolometric flux of an SSP
     real(DP),dimension(-nmaxCM:nmaxspecLCB,nmaxtimesSSP,nmaxZtracks) :: Lspec_SSP  ! Bolometric flux contribution of individual stars to an SSP flux     

  end type t_SSP_DP
  
  type(t_SSP_DP), SAVE :: SSP_DP
  

  !##################################################
  TYPE t_track ! Read in SSPs_HR.f90
     integer                          :: nMS, nHeF, nHB, nVLM,  nHBsupp
     integer, dimension(nmaxMS)       :: nsteps, nstepssupp, nWD, nWDsupp
     real(dp)                         :: metal
     REAL(DP), dimension(nmaxMS)      :: mass, massHB, massfinal, lifetime, msupp, Mc, MassWD
     integer                          :: nr
     REAL(DP), dimension(nmaxMS)      :: mr,r,rmet, rZ
     real(DP), dimension(nmaxMS,1000) :: age,Lum,Teff,grav
     real(DP), dimension(nmaxMS,1000) :: agesupp,lumsupp,Teffsupp,gravsupp
     real(DP), dimension(nmaxMS,30)   :: ageWD, LumWD, TeffWD, gravWD

     real(DP), dimension(1000)        :: m, Tisoch, Lisoch, gisoch

     
  end type t_track
  !##################################################
  TYPE t_stell
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

  end type t_stell

  type(t_stell), SAVE :: stell

end MODULE types_ssps
