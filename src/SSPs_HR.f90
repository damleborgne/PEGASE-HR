program SSPs_HR

  ! Converts stellar tracks to isochrones assuming an IMF.

  use types_ssps
  use constants
  use SSPsubr
  use util

  implicit none

  character(len=1) ::    answerwinds,answerejecta,useCM
  character(len=72) ::  char_massmin,char_massmax
  character(len=strmax) ::   fileSSP,filetracks(nmaxZtracks)
  character(len=strmax) ::   fileIMF(nmaxIMFfiles),prefix
  character(len=strmax) ::   whole_filename
  character(len=256) ::  line
!  character(len=30) ::  grid_type

  integer :: i,j,k,j1,j2,p,nbins, isupp
  integer :: l,iWD,error,iIMF,itracks
  integer :: nIMFbins,time,timeprev,q,nIMFfiles,ntracksfiles
  integer :: kend,j3,j4
  integer :: nZ_WW,nmass_WW
  integer :: j5,j6,j7,j8

  integer :: nused,iz,nz
  integer :: iz1,iz2,iz3,iz4,iz5,iz6,iz7,iz8
  integer :: iused(nmaxZl*nmaxsl),nr,nmu
  integer :: lun               ! Used for logical unit numbers

  !     Double precision is required for post-AGB stars.
!  real(DP),dimension(nmaxZl)                :: z
!  real(DP),dimension(nmaxZl,-nmaxCM:nmaxsl) :: Tspec, gspec, NHI, NHeI, NHeII

  integer :: nCM
  real(DP),dimension(nmaxCM)                :: TCM,gCM,NHICM,NHeICM,NHeIICM

  real(DP) ::  alpha1,alpha2,alpha3,alpha4
  real(DP) ::  alpha5,alpha6,alpha7,alpha8
  real(DP) ::  alpha,norm
  real(DP) ::  dm,mp,Tp,Lp, dn
  real(DP) ::  mend,yield,dtime
  real(DP) ::  nSNIa,m2,mSNIa,mSNII,mu,mCh,m1, masssup, deltat


  real(DP), dimension(-nmaxCM:nmaxZl*nmaxsl):: fluxSSP

  real(DP), dimension(nmaxIMFbins) ::  coeffcont, massinf, slope

  real(DP) ::  gp
  real(DP) ::  fz
  real(DP) ::  NHItot,NHeItot,NHeIItot,Lbol
  real(DP) ::  ej,ejZ,Cbin,mbin,massmin,massmax,m2max
  real(DP) ::  dmu,muinf,musup,dn12,mWD1,deltam,r2,gammabin
  real(DP) ::  ejecta_IS,ejectaZ_IS,ejecta_CB,ejectaZ_CB
  real(DP) ::  massBHNS_IS,massWD_IS,massBHNS_CB,massWD_CB
  real(DP) ::  nSNII_IS,nSNII_CB,ejectatot,ejectaZtot,mmin,mmax

  real(DP) ::  mass_WW(10),ejectaX_WW(10,3,5),ejectaY_WW(10,3,5)
  real(DP) ::  ejectaZ_WW(10,3,5)
  real(DP) ::  ejectatot_WW(10,3,5),Z_WW(5)
  real(DP) ::  logZinf,logZsup

  character(len=strmax) :: stellib_name

  !  integer      str_length
  integer      izseekinf,izseeksup
  integer, DIMENSION(8) :: vi,ve,vi1,ve1,vi2,ve2
  real :: ti,te


  type(t_track) :: tr

  !     Minimal mass of a binary system to produce a SNIa 
  !     (Matteucci & Greggio 86).
  mSNIa=3.                  
  !     Chandrasekhar mass.
  mCh=1.4                   
  !     Exponent of the mass-ratio function of binary stars (MC86).
  gammabin=2.               


  !     Read the list of IMF
  lun=60
  call file_open(trim(PEG_ROOT)//'/data/user_defined/list_IMFs.dat',&
       lun,error)
  if (error.ne.0) then
     write(0,*) 'SSPs_HR: Could not open file ',&
          trim(PEG_ROOT)//'/data/user_defined/list_IMFs.dat'
     !     call exit(1)
     stop 1
  endif
  nIMFfiles=0
  do while(error.eq.0)
     read(lun,'(a)',iostat=error) line
     if (error.eq.0.and.line.gt.' '.and.line(1:1).ne.'!') then
        if (nIMFfiles.lt.nmaxIMFfiles) then
           nIMFfiles = nIMFfiles+1
           fileIMF(nIMFfiles) = line
        else
           write(0,*) 'SSPs_HR: too many IMFs'
           !           call exit(1)
           stop 1
        endif
     endif
  enddo
  close(lun)
  fileIMF(nIMFfiles+1)='ln'
  fileIMF(nIMFfiles+2)='RB'
  fileIMF(nIMFfiles+3)='Fe' 
  fileIMF(nIMFfiles+4)='Ch2003'
  fileIMF(nIMFfiles+5)='Ch2005'     

  write(*,*) ' '
  write(*,*) 'Initial mass function?'
  do i=1,nIMFfiles
     write(*,'(1x,i3,a,a)') i,': ',trim(fileIMF(i))
  enddo
  write(*,'(1x,i3,a)') nIMFfiles+1,': lognormal IMF'
  write(*,'(1x,i3,a)') nIMFfiles+2,': Rana & Basu (1992)'
  !     Ferrini F. 1991, Chemical and Dynamical Evolution of Galaxies,
  !     F. Ferrini, F. Matteucci, J. Franco (eds.), p. 520
  write(*,'(1x,i3,a)') nIMFfiles+3,': Ferrini (1991)'      
  write(*,'(1x,i3,a)') nIMFfiles+4,': Chabrier (2003)'
  write(*,'(1x,i3,a)') nIMFfiles+5,': Chabrier (2005)'

  !     Choose an IMF and read the file
  read(*,*) iIMF      
  if (iIMF.le.0.or.iIMF.gt.nIMFfiles+5) then
     write(0,*) 'SSPs_HR: IMF number is invalid', iIMF
     !     call exit(1)
     stop 1
  endif
     if ((fileIMF(iIMF).ne.'ln').and.(fileIMF(iIMF).ne.'RB')&
      .and.(fileIMF(iIMF).ne.'Fe')&
      .and.(fileIMF(iIMF).ne.'Ch2003')&
      .and.(fileIMF(iIMF).ne.'Ch2005')) then

     lun=30
     call file_open(trim(PEG_ROOT)//'/data/user_defined/'//fileIMF(iIMF),&
          lun,error)
     if (error.ne.0) then
        write(0,*) 'SSPs_HR: Could not open file ',&
             trim(PEG_ROOT)//'/data/user_defined/'//fileIMF(iIMF)
        !        call exit(1)
        stop 1
     endif
     read(lun,*) nIMFbins
     do i=1,nIMFbins
        read(lun,*) massinf(i),slope(i)
     end do
     read(lun,*) masssup
     coeffcont(1)=1.
     do i=2,nIMFbins
        coeffcont(i)=coeffcont(i-1)*massinf(i)**&
             (slope(i-1)-slope(i))
     end do
     massmin=massinf(1)
     massmax=masssup
     close(lun)
  else
     masssup=120.
     massmin=0.09
     massmax=masssup
  end if
  error=1
  do while (error.ne.0)
     write(*,'(1x,a,e8.3,a)') 'Lower mass (default=',massmin,&
          ' Msol)?'
     read(*,'(a)') char_massmin
     if (char_massmin.ne.' ') then
        read(char_massmin,*,iostat=error) massmin
     else
        error=0
     end if
     if (error.ne.0) write(*,*) 'Invalid input!'
  end do
  error=1
  do while (error.ne.0)
     write(*,'(1x,a,e8.3,a)') 'Upper mass (default=',massmax,&
          ' Msol)?'
     read(*,'(a)') char_massmax
     if (char_massmax.ne.' ') then
        read(char_massmax,*,iostat=error) massmax
     else
        error=0
     end if
     if (error.ne.0) write(*,*) 'Invalid input!'
  end do

  write(*,*) ' '
  write(*,*) 'SNII ejecta: model A, B or C ', &
       'of Woosley & Weaver (A/B [default]/C)?'
  read(*,'(a)') answerejecta 
  if (answerejecta.eq.'a') answerejecta='A'
  if (answerejecta.eq.'b') answerejecta='B'
  if (answerejecta.eq.'c') answerejecta='C'
  if (answerejecta.ne.'A'.and.answerejecta.ne.'C') &
       answerejecta='B'


  !     Read stellar yields of Woosley and Weather (1995)
  lun=50
  call file_open(trim(PEG_ROOT)//'/data/external/WW.dat',&
       lun,error)
  nmass_WW=10
  read(lun,*) nZ_WW
  read(lun,*) (mass_WW(i),i=1,7),(mass_WW(8),j=1,2),&
       ((mass_WW(i),j=1,3),i=9,nmass_WW)
  do k=1,nZ_WW
     read(lun,*) Z_WW(k)
     read(lun,*)(ejectaX_WW(i,1,k),i=1,7),&
          (ejectaX_WW(8,j,k),j=1,2),&
          ((ejectaX_WW(i,j,k),j=1,3),i=9,nmass_WW) 
     read(lun,*)(ejectaY_WW(i,1,k),i=1,7),&
          (ejectaY_WW(8,j,k),j=1,2),&
          ((ejectaY_WW(i,j,k),j=1,3),i=9,nmass_WW)
     read(lun,*)(ejectatot_WW(i,1,k),i=1,7),&
          (ejectatot_WW(8,j,k),j=1,2),&
          ((ejectatot_WW(i,j,k),j=1,3),i=9,nmass_WW)
  end do
  do i=1,7
     do j=2,3
        do k=1,nZ_WW
           ejectaX_WW(i,j,k)=ejectaX_WW(i,1,k)
           ejectaY_WW(i,j,k)=ejectaY_WW(i,1,k)
           ejectatot_WW(i,j,k)=ejectatot_WW(i,1,k)
        end do
     end do
  end do
  do k=1,nZ_WW
     ejectaX_WW(8,3,k)=ejectaX_WW(8,2,k)
     ejectaY_WW(8,3,k)=ejectaY_WW(8,2,k)
     ejectatot_WW(8,3,k)=ejectatot_WW(8,2,k)
  end do

  !**** Computation of the net yield of metals for the supernovae

  do i=1,nmass_WW
     do j=1,3
        do k=1,nZ_WW
           ejectaZ_WW(i,j,k)=ejectatot_WW(i,j,k)*(1.-Z_WW(k))&
                -ejectaX_WW(i,j,k)-ejectaY_WW(i,j,k)
        enddo
     enddo
  enddo
  close(lun)


  write(*,*) ' '
  write(*,*) 'Stellar winds (y [default]/n)?'
  read(*,'(a)') answerwinds 
  if (answerwinds.eq.'N') answerwinds='n'
  if (answerwinds.ne.'n') answerwinds='y'


  lun=60
  call file_open(trim(PEG_ROOT)//'/data/user_defined/list_tracks.dat',&
       lun,error)
  if (error.ne.0) then
     write(0,*) 'SSPs_HR: Could not open file ',&
          trim(PEG_ROOT)//'/data/user_defined/list_tracks.dat'
     !     call exit(1)
     stop 1
  endif
  do i=1,nmaxZtracks
     read(lun,'(a)',end=1002) filetracks(i)
     if (filetracks(i).ne.' ') ntracksfiles=i
  enddo
1002 close(lun)


  !**** Read the stellar libraries
  !     Use the Clegg & Middlemass library of hot stars anyway:
  useCM='y'

  if (useCM.eq.'n') then
     nCM = -1
  else
     open(90,file=trim(PEG_ROOT)//'/data/external/stellibCM.dat',&
          status='old')
     read(90,*) nCM
     do i=1,nCM
        read(90,*) TCM(i),gCM(i),NHICM(i),NHeICM(i),NHeIICM(i)
     enddo
     close(90)
  endif

  call choose_stellib(stellib_name, error)
  if (error.ne.0) then
     write(0,*)'SSPs_HR: Failed to select a stellar library'
     !     call exit(1)
     stop 1
  endif

  call read_para_stellib(stellib_name)
  !     stell%nspecZ (prev nspec) now contains the array of number of spectra used 
  !     in LCB or ELO library, for each metallicity

  do iz = 1, stell%nz
     do i=1,nCM
        stell%Tspec(iz,-i)=TCM(i)
        stell%gspec(iz,-i)=gCM(i)
        stell%NHI(iz,-i)=NHICM(i)
        stell%NHeI(iz,-i)=NHeICM(i)
        stell%NHeII(iz,-i)=NHeIICM(i)
     enddo
  enddo

  !**** Create the SSPs.dat file
  !     This file contains a short header. Its contains is the list of SSP
  !     which are contained in separate files.
  !     the SSPs.dat file is read by the routine ssp_files_read (in ssp_io.f)
  write(*,*) 'Prefix?'
  read(*,'(a)') prefix
  lun=80
  call file_unit(lun)
  open(lun,file=prefix(1:index(prefix,' ')-1)//'_'//'SSPs.dat',&
       status='new')
  write(lun, '(a)') 'format: PEGASE-HR/SSPs   '
  write(lun, '(a)') 'version: 1               '
  write(lun, '(a)') 'grid type: ',stell%grid_type

  write(lun, '(a)') 'lib: '//stellib_name(:str_length(stellib_name))
  write(lun, '(a)') 'head_end:'

  !################################################################################
  !################################################################################
  !**** Loop on the SSP files
  do itracks=1,ntracksfiles

     call date_and_time(values=vi)
     call date_and_time(values=vi1)

     write(*,*) ' Computing isochones # ',itracks,'/',ntracksfiles
     whole_filename=trim(PEG_ROOT)//'/data/tracks/'//filetracks(itracks)
     open(10,status='old',file=whole_filename)
     read(10,*) tr%nMS,tr%nHeF,tr%nHB,tr%nVLM,tr%metal,mSNII
     !     tr%nVLM: number of tracks of very low mass stars (~<=0.5 Msol) 
     !     which do not evolve.
     !     tr%nHeF: number of tracks (ZAMS->tip of the RGB) 
     !     of low mass stars which undergo the helium flash
     !     and have to be connected to ZAHB tracks.
     !     tr%nMS: number of ZAMS tracks.
     !     tr%nHB: number of ZAHB tracks.
     do i=1,tr%nMS
        read(10,*) tr%mass(i),tr%nsteps(i),tr%Mc(i),tr%massfinal(i),tr%nWD(i),tr%rZ(i)
        do j=1,tr%nsteps(i)+tr%nWD(i)*21
           read(10,*) tr%Lum(i,j),tr%Teff(i,j),tr%grav(i,j),tr%age(i,j)
        end do
        tr%m(i)=log10(tr%mass(i))     
     end do
     do i=tr%nMS+1,tr%nMS+tr%nHB-1
        read(10,*) tr%massHB(i),tr%nsteps(i),tr%Mc(i),tr%massfinal(i),&
             tr%nWD(i),tr%rZ(i)
        j=tr%nVLM
        do while ((tr%massfinal(j)-tr%massHB(i))&
             *(tr%massfinal(j+1)-tr%massHB(i)).gt.0.)
           j=j+1
        end do
        tr%mass(i)=tr%mass(j)+(tr%mass(j+1)-tr%mass(j))&
             *(tr%massHB(i)-tr%massfinal(j))&
             /(tr%massfinal(j+1)-tr%massfinal(j)) 
        tr%m(i)=log10(tr%mass(i))
        tr%lifetime(i)=log10(tr%age(j,tr%nsteps(j)))&
             +(tr%m(i)-tr%m(j))&
             *(log10(tr%age(j+1,tr%nsteps(j+1)))-log10(tr%age(j,tr%nsteps(j))))&
             /(tr%m(j+1)-tr%m(j))
        tr%lifetime(i)=10.**tr%lifetime(i)
        do j=1,tr%nsteps(i)+tr%nWD(i)*21
           read(10,*) tr%Lum(i,j),tr%Teff(i,j),tr%grav(i,j),tr%age(i,j)
        end do
     end do

     i=tr%nMS+tr%nHB
     read(10,*) tr%massHB(i),tr%nsteps(i),tr%Mc(i),tr%massfinal(i),tr%nWD(i),tr%rZ(i)
     do j=1,tr%nsteps(i)+tr%nWD(i)*21
        read(10,*) tr%Lum(i,j),tr%Teff(i,j),tr%grav(i,j),tr%age(i,j)
        tr%lifetime(i)=tr%age(tr%nVLM+tr%nHeF,tr%nsteps(tr%nVLM+tr%nHeF))
     end do
     tr%mass(i)=tr%mass(tr%nVLM+tr%nHeF)
     tr%m(i)=log10(tr%mass(i))

     i=tr%nVLM+tr%nHeF+1
     deltat=tr%age(tr%nVLM+tr%nHeF,tr%nsteps(tr%nVLM+tr%nHeF))&
          +tr%age(tr%nMS+tr%nHB,tr%nsteps(tr%nMS+tr%nHB))&
          -tr%age(i,tr%nsteps(i))
     do j=tr%nsteps(i),tr%nsteps(i)+tr%nWD(i)*21
        tr%age(i,j)=tr%age(i,j)+deltat
     end do
     close(10)  

     tr%nr=0
     do i=tr%nMS+1,tr%nMS+tr%nHB-1,2
        tr%nr=tr%nr+1
        tr%mr(tr%nr)=tr%mass(i)
        tr%r(tr%nr)=tr%mass(i)-tr%massfinal(i)
        tr%rmet(tr%nr)=tr%rZ(i)
        if (tr%rmet(tr%nr).lt.1.e-4) tr%rmet(tr%nr)=0.
     end do

     i=tr%nMS+tr%nHB
     tr%nr=tr%nr+1
     tr%mr(tr%nr)=tr%mass(i)
     tr%r(tr%nr)=tr%mass(i)-tr%massfinal(i)
     tr%rmet(tr%nr)=tr%rZ(i)
     if (tr%rmet(tr%nr).lt.1.e-4) tr%rmet(tr%nr)=0.
     do i=tr%nVLM+tr%nHeF+2,tr%nMS,2
        tr%nr=tr%nr+1
        tr%mr(tr%nr)=tr%mass(i)
        if (tr%mass(i).lt.mSNII) then
           tr%r(tr%nr)=tr%mass(i)-tr%massfinal(i)
           tr%rmet(tr%nr)=tr%rZ(i)
           if (tr%rmet(tr%nr).lt.1.e-4) tr%rmet(tr%nr)=0.
        else
           call Woosley(tr%mass(i),tr%metal,tr%massfinal(i),tr%rZ(i),nZ_WW,&
                Z_WW,nmass_WW,mass_WW,ejectaZ_WW,&
                ejectatot_WW,answerwinds,answerejecta,ej,ejZ)
           tr%r(tr%nr)=ej
           tr%rmet(tr%nr)=ejZ
        end if
     end do

     j=tr%nMS+1
     i=tr%nVLM+1
     isupp=0
     tr%nHBsupp=0

     !     Extension of the ZAMS tracks by ZAHB tracks for stars undergoing
     !     the Helium flash AND "extension" of the ZAHB tracks by ZAMS tracks.

     call date_and_time(values=ve1)
     print*, '                                        init step = ',&
          ((ve1(8)+1000.*(ve1(7)+60.*ve1(6)))-&
          (vi1(8)+1000.*(vi1(7)+60.*vi1(6))))/1000.
     vi1=ve1

     do while (i.lt.tr%nVLM+tr%nHeF)
        if (tr%massfinal(i).lt.tr%massHB(tr%nMS+1)) then
           if (tr%mass(i).ge.tr%massHB(tr%nMS+1)) then 

              !     Stars with a ZAMS mass higher than the lowest ZAHB mass [massHB(tr%nMS+1)],
              !     but which after the RGB have a lower mass than massHB(tr%nMS+1), are
              !     assigned the evolution corresponding to massHB(tr%nMS+1) after the RGB.

              tr%nHBsupp=tr%nHBsupp+1
              isupp=isupp+1
              tr%msupp(isupp)=tr%m(i)
              tr%nstepssupp(isupp)=tr%nsteps(tr%nMS+1)
              tr%nWDsupp(isupp)=tr%nWD(tr%nMS+1)
              do k=1,tr%nsteps(tr%nMS+1)+tr%nWD(tr%nMS+1)*21
                 tr%agesupp(isupp,k)=tr%age(i,tr%nsteps(i))+tr%age(tr%nMS+1,k)
                 tr%Lumsupp(isupp,k)=tr%Lum(tr%nMS+1,k)
                 tr%Teffsupp(isupp,k)=tr%Teff(tr%nMS+1,k)
                 tr%gravsupp(isupp,k)=tr%grav(tr%nMS+1,k)
              end do
              i=i+1
           end if
        else
           if (tr%m(j).lt.tr%m(i)) then

              !     "Extension" of the ZAHB tracks by ZAMS tracks.

              isupp=isupp+1
              tr%msupp(isupp)=tr%m(j)
              tr%nstepssupp(isupp)=tr%nsteps(j)
              tr%nWDsupp(isupp)=tr%nWD(j)
              do k=1,tr%nsteps(j)+tr%nWD(j)*21
                 tr%agesupp(isupp,k)=tr%lifetime(j)+tr%age(j,k)
                 tr%Lumsupp(isupp,k)=tr%Lum(j,k)
                 tr%Teffsupp(isupp,k)=tr%Teff(j,k)
                 tr%gravsupp(isupp,k)=tr%grav(j,k)
              end do
              j=j+1
           else

              !     Extension of the ZAMS tracks by ZAHB tracks.

              isupp=isupp+1
              tr%nHBsupp=tr%nHBsupp+1
              alpha=(tr%m(j)-tr%m(i))/(tr%m(j)-tr%m(j-1))
              tr%msupp(isupp)=tr%m(i)
              tr%nstepssupp(isupp)=tr%nsteps(j-1)
              tr%nWDsupp(isupp)=tr%nWD(j-1)
              tr%agesupp(isupp,1)=tr%age(i,tr%nsteps(i))
              tr%Lumsupp(isupp,1)=alpha*tr%Lum(j-1,1)+(1.-alpha)*tr%Lum(j,1)
              tr%Teffsupp(isupp,1)=alpha*tr%Teff(j-1,1)&
                   +(1.-alpha)*tr%Teff(j,1)
              tr%gravsupp(isupp,1)=alpha*tr%grav(j-1,1)&
                   +(1.-alpha)*tr%grav(j,1)
              do k=2,tr%nsteps(j-1)+tr%nWD(j-1)*21
                 tr%agesupp(isupp,k)=tr%age(i,tr%nsteps(i))&
                      +10.**(alpha*log10(tr%age(j-1,k))&
                      +(1.-alpha)*log10(tr%age(j,k)))
                 tr%Lumsupp(isupp,k)=alpha*tr%Lum(j-1,k)&
                      +(1.-alpha)*tr%Lum(j,k)
                 tr%Teffsupp(isupp,k)=alpha*tr%Teff(j-1,k)&
                      +(1.-alpha)*tr%Teff(j,k)
                 tr%gravsupp(isupp,k)=alpha*tr%grav(j-1,k)&
                      +(1.-alpha)*tr%grav(j,k)
              end do
              i=i+1
           end if
        end if
     end do

     call date_and_time(values=ve1)
     print*, '                                        next step = ',&
          ((ve1(8)+1000.*(ve1(7)+60.*ve1(6)))-&
          (vi1(8)+1000.*(vi1(7)+60.*vi1(6))))/1000.
     vi1=ve1

     !     "Extension" of the remaining ZAHB tracks by ZAMS tracks.

     do i=j,tr%nMS+tr%nHB
        isupp=isupp+1
        tr%msupp(isupp)=tr%m(i)
        tr%nstepssupp(isupp)=tr%nsteps(i)
        tr%nWDsupp(isupp)=tr%nWD(i)
        do k=1,tr%nsteps(i)+tr%nWD(i)*21
           tr%agesupp(isupp,k)=tr%lifetime(i)+tr%age(i,k)
           tr%Lumsupp(isupp,k)=tr%Lum(i,k)
           tr%Teffsupp(isupp,k)=tr%Teff(i,k)
           tr%gravsupp(isupp,k)=tr%grav(i,k)
        end do
     end do
     tr%nHB=tr%nHB+tr%nHBsupp
     do i=tr%nMS+1,tr%nMS+tr%nHB
        isupp=i-tr%nMS
        tr%m(i)=tr%msupp(isupp)         
        tr%nsteps(i)=tr%nstepssupp(isupp)
        tr%nWD(i)=tr%nWDsupp(isupp)
        tr%mass(i)=10.**tr%m(i)
        do j=1,tr%nsteps(i)+tr%nWD(i)*21
           tr%age(i,j)=tr%agesupp(isupp,j)
           tr%Lum(i,j)=tr%Lumsupp(isupp,j)
           tr%Teff(i,j)=tr%Teffsupp(isupp,j)
           tr%grav(i,j)=tr%gravsupp(isupp,j)
        end do
     end do

     iWD=0
     do i=tr%nMS+1,tr%nMS+tr%nHB-1
        if (tr%nWD(i).gt.0) then
           iWD=iWD+1
           tr%massWD(iWD)=tr%mass(i)
           do j=1,21
              tr%LumWD(iWD,j)=tr%Lum(i,j+tr%nsteps(i))
              tr%TeffWD(iWD,j)=tr%Teff(i,j+tr%nsteps(i))
              tr%gravWD(iWD,j)=tr%grav(i,j+tr%nsteps(i))
              tr%ageWD(iWD,j)=tr%age(i,j+tr%nsteps(i))
           end do
        end if
     end do
     do i=tr%nVLM+tr%nHeF+1,tr%nMS
        if (tr%nWD(i).gt.0) then
           iWD=iWD+1
           tr%massWD(iWD)=tr%mass(i)
           do j=1,21
              tr%LumWD(iWD,j)=tr%Lum(i,j+tr%nsteps(i))
              tr%TeffWD(iWD,j)=tr%Teff(i,j+tr%nsteps(i))
              tr%gravWD(iWD,j)=tr%grav(i,j+tr%nsteps(i))
              tr%ageWD(iWD,j)=tr%age(i,j+tr%nsteps(i))
           end do
        end if
     end do

     iz=1
     do while ((tr%metal-stell%z(iz))*(tr%metal-stell%z(iz+1)).gt.0.)
        iz=iz+1
     end do
     fz=(stell%z(iz+1)-tr%metal)/(stell%z(iz+1)-stell%z(iz))

     ejectatot=0.
     ejectaZtot=0.
     mmin=log10(massmin)
     mmax=log10(massmax)
     nbins=int((mmax-mmin)/0.001+1)
     dm=(mmax-mmin)/nbins
     norm=0.
     do p=0,nbins-1
        mp=mmin+(p+0.5)*dm
        dn=nstars(mp,dm,fileIMF(iIMF),massinf,nIMFbins,&
             coeffcont,slope,1.d0,massmin,massmax)
        norm=norm+dn*10.**mp
        ejectatot=ejectatot+dn*interpol(10.**mp,tr%mr,tr%r,tr%nr)
        ejectaZtot=ejectaZtot+dn*interpol(10.**mp,tr%mr,tr%rmet,tr%nr)
     end do
     yield=ejectaZtot/norm/(1.-ejectatot/norm)                  

     fileSSP=prefix(1:index(prefix,' ')-1)//'_'//&
          filetracks(itracks)
     open(40,file=fileSSP,status='new')
     write(lun,'(a)') fileSSP(1:index(fileSSP,' '))
     write(40,'(a,a)') 'Evolutionary tracks: ',filetracks(itracks)
     write(40,'(a,a)')    'grid type                  : ',stell%grid_type
     write(40,'(a,f5.4)') 'Metallicity (mass fraction): ',tr%metal
     if (answerwinds.eq.'y') then
        write(40,'(a,a,a1,a)') 'Ejecta of massive stars: ',&
             'SNII (model ',answerejecta,&
             ' of Woosley & Weaver [1995]) + stellar winds'
     else
        write(40,'(a,a,a1,a)') 'Ejecta of massive stars: ',&
             'SNII (model ',answerejecta,&
             ' of Woosley & Weaver [1995]) only'
     end if
     i=index(fileIMF(iIMF),' ')
     write(40,'(a,a,a,f8.4,a,f8.4,a)') 'Initial mass function: ',&
          fileIMF(iIMF)(1:i-1),' from ',massmin,' to ',massmax,&
          ' solar masses'
     write(40,'(6(i5,1x),e10.4)') 516,stell%nspectot,stell%firstspec(iz),&
          stell%firstspec(iz+1)-1,iz, iz+1, yield

     call date_and_time(values=ve1)
     print*, '                                        end init step = ',&
          ((ve1(8)+1000.*(ve1(7)+60.*ve1(6)))-&
          (vi1(8)+1000.*(vi1(7)+60.*vi1(6))))/1000.
     vi1=ve1
     !##################################################
     ! MOST EXPENSIVE COMPUTING BIG LOOP (515 iter = 5 sec : 10-2 sec / iter)
     !     Computation of the isochrones.
     !     time in Myr
     mend=log10(tr%mass(tr%nMS))
     timeprev=0
     do l=0,515
        if (l.le.29) then
           time=l             
        else
           if (l.le.64) then
              time=30+2*(l-30)
           else
              if (l.le.104) then
                 time=100+5*(l-65)
              else
                 if (l.le.174) then
                    time=300+10*(l-105)
                 else
                    if (l.le.274) then
                       time=1000+20*(l-175)
                    else
                       if (l.le.414) then
                          time=3000+50*(l-275)
                       else
                          time=10000+100*(l-415)
                       end if
                    end if
                 end if
              end if
           end if
        end if

        !     k: number of points on the isochrone.
        !     m: log10(mass) of the point on the isochrone.
        !     Tisoch: effective temperature""" (log10(.[Kelvin])).
        !     Lisoch: bolometric luminosity""" (log10(./Lsol))
        !     gisoch: surface tr%gravity """.


        call date_and_time(values=vi2)

        k=0
        if (time.gt.0) then

           !     Time strictly > 0.

           !     Very low mass stars.

           do i=1,tr%nVLM
              k=k+1
              tr%m(k)=log10(tr%mass(i))
              j=1
              do while ((tr%age(i,j)-time)*(tr%age(i,j+1)-time).gt.0.)
                 j=j+1
              end do
              alpha=(time-tr%age(i,j))/(tr%age(i,j+1)-tr%age(i,j))
              tr%Tisoch(k)=tr%Teff(i,j)+alpha*(tr%Teff(i,j+1)-tr%Teff(i,j))
              tr%Lisoch(k)=tr%Lum(i,j)+alpha*(tr%Lum(i,j+1)-tr%Lum(i,j))      
              tr%gisoch(k)=tr%grav(i,j)+alpha*(tr%grav(i,j+1)-tr%grav(i,j))
           end do
           do i=tr%nVLM+1,tr%nVLM+tr%nHeF-1
              if ((tr%nsteps(i).eq.tr%nsteps(i+1)).and.&
                   (tr%mass(i).lt.tr%mass(i+1)-1.e-6)) then
                 if (((tr%age(i,1)-time)*(tr%age(i+1,tr%nsteps(i+1))-time)&
                      .le.0.).or.((tr%age(i,tr%nsteps(i))-time)&
                      *(tr%age(i+1,1)-time).le.0.)) then
                    j1=1
                    j2=1
                    if (tr%age(i,1).gt.time) j1=0
                    if (tr%age(i,tr%nsteps(i)).lt.time) j1=tr%nsteps(i)
                    if (tr%age(i+1,1).gt.time) j2=0
                    if (tr%age(i+1,tr%nsteps(i+1)).lt.time) j2=tr%nsteps(i+1)
                    if (j1*(j1-tr%nsteps(i)).ne.0) then
                       do while ((tr%age(i,j1)-time)&
                            *(tr%age(i,j1+1)-time).gt.0.)
                          j1=j1+1
                       end do
                    end if
                    if (j2*(j2-tr%nsteps(i+1)).ne.0) then
                       do while ((tr%age(i+1,j2)-time)&
                            *(tr%age(i+1,j2+1)-time).gt.0.)
                          j2=j2+1
                       end do
                    end if
                    if (j1*(j1-tr%nsteps(i)).ne.0) then
                       k=k+1
                       tr%m(k)=log10(tr%mass(i))
                       alpha=(time-tr%age(i,j1))/&
                            (tr%age(i,j1+1)-tr%age(i,j1))
                       tr%Tisoch(k)=tr%Teff(i,j1)&
                            +alpha*(tr%Teff(i,j1+1)-tr%Teff(i,j1))
                       tr%Lisoch(k)=tr%Lum(i,j1)&
                            +alpha*(tr%Lum(i,j1+1)-tr%Lum(i,j1))
                       tr%gisoch(k)=tr%grav(i,j1)&
                            +alpha*(tr%grav(i,j1+1)-tr%grav(i,j1))
                    end if
                    if (j1.gt.j2) then
                       do j=j1,j2+1,-1
                          alpha=log10(time/tr%age(i+1,j))/&
                               log10(tr%age(i,j)/tr%age(i+1,j))
                          k=k+1
                          tr%m(k)=alpha*log10(tr%mass(i))&
                               +(1.-alpha)*log10(tr%mass(i+1))
                          tr%Tisoch(k)=alpha*tr%Teff(i,j)&
                               +(1.-alpha)*tr%Teff(i+1,j)
                          tr%Lisoch(k)=alpha*tr%Lum(i,j)&
                               +(1.-alpha)*tr%Lum(i+1,j)
                          tr%gisoch(k)=alpha*tr%grav(i,j)&
                               +(1.-alpha)*tr%grav(i+1,j)
                       end do
                    else
                       do j=j1+1,j2
                          alpha=log10(time/tr%age(i+1,j))/&
                               log10(tr%age(i,j)/tr%age(i+1,j))
                          k=k+1
                          tr%m(k)=alpha*log10(tr%mass(i))&
                               +(1.-alpha)*log10(tr%mass(i+1))
                          tr%Tisoch(k)=alpha*tr%Teff(i,j)&
                               +(1.-alpha)*tr%Teff(i+1,j)
                          tr%Lisoch(k)=alpha*tr%Lum(i,j)&
                               +(1.-alpha)*tr%Lum(i+1,j)
                          tr%gisoch(k)=alpha*tr%grav(i,j)&
                               +(1.-alpha)*tr%grav(i+1,j)
                       end do
                    end if
                    if (j2*(j2-tr%nsteps(i+1)).ne.0) then
                       k=k+1
                       tr%m(k)=log10(tr%mass(i+1))
                       alpha=(time-tr%age(i+1,j2))/&
                            (tr%age(i+1,j2+1)-tr%age(i+1,j2))
                       tr%Tisoch(k)=tr%Teff(i+1,j2)&
                            +alpha*(tr%Teff(i+1,j2+1)-tr%Teff(i+1,j2))
                       tr%Lisoch(k)=tr%Lum(i+1,j2)&
                            +alpha*(tr%Lum(i+1,j2+1)-tr%Lum(i+1,j2))
                       tr%gisoch(k)=tr%grav(i+1,j2)&
                            +alpha*(tr%grav(i+1,j2+1)-tr%grav(i+1,j2))
                    end if
                 end if
              end if
           end do
           i=tr%nVLM+tr%nHeF
           do j=1,tr%nsteps(i)-1
              if ((tr%age(i,j)-time)*(tr%age(i,j+1)-time).le.0.) then
                 k=k+1
                 tr%m(k)=log10(tr%mass(i))
                 alpha=(time-tr%age(i,j))/(tr%age(i,j+1)-tr%age(i,j))
                 tr%Tisoch(k)=tr%Teff(i,j)+alpha*(tr%Teff(i,j+1)-tr%Teff(i,j))
                 tr%Lisoch(k)=tr%Lum(i,j)+alpha*(tr%Lum(i,j+1)-tr%Lum(i,j))
                 tr%gisoch(k)=tr%grav(i,j)+alpha*(tr%grav(i,j+1)-tr%grav(i,j))
              end if
           end do
           do i=tr%nMS+1,tr%nMS+tr%nHB-1
              if ((tr%nsteps(i).eq.tr%nsteps(i+1)).and.&
                   (tr%mass(i).lt.tr%mass(i+1)-1.e-6)) then
                 if (((tr%age(i,1)-time)*(tr%age(i+1,tr%nsteps(i+1))-time)&
                      .le.0.).or.((tr%age(i,tr%nsteps(i))-time)&
                      *(tr%age(i+1,1)-time).le.0.)) then
                    j1=1
                    j2=1
                    if (tr%age(i,1).gt.time) j1=0
                    if (tr%age(i,tr%nsteps(i)).lt.time) j1=tr%nsteps(i)
                    if (tr%age(i+1,1).gt.time) j2=0
                    if (tr%age(i+1,tr%nsteps(i+1)).lt.time) j2=tr%nsteps(i+1)
                    if (j1*(j1-tr%nsteps(i)).ne.0) then
                       do while ((tr%age(i,j1)-time)&
                            *(tr%age(i,j1+1)-time).gt.0.)
                          j1=j1+1
                       end do
                    end if
                    if (j2*(j2-tr%nsteps(i+1)).ne.0) then
                       do while ((tr%age(i+1,j2)-time)&
                            *(tr%age(i+1,j2+1)-time).gt.0.)
                          j2=j2+1
                       end do
                    end if
                    if (j1*(j1-tr%nsteps(i)).ne.0) then
                       k=k+1
                       tr%m(k)=log10(tr%mass(i))
                       alpha=(time-tr%age(i,j1))/&
                            (tr%age(i,j1+1)-tr%age(i,j1))
                       tr%Tisoch(k)=tr%Teff(i,j1)&
                            +alpha*(tr%Teff(i,j1+1)-tr%Teff(i,j1))
                       tr%Lisoch(k)=tr%Lum(i,j1)&
                            +alpha*(tr%Lum(i,j1+1)-tr%Lum(i,j1))
                       tr%gisoch(k)=tr%grav(i,j1)&
                            +alpha*(tr%grav(i,j1+1)-tr%grav(i,j1))
                    end if
                    if (j1.gt.j2) then
                       do j=j1,j2+1,-1
                          alpha=log10(time/tr%age(i+1,j))/&
                               log10(tr%age(i,j)/tr%age(i+1,j))
                          k=k+1
                          tr%m(k)=alpha*log10(tr%mass(i))&
                               +(1.-alpha)*log10(tr%mass(i+1))
                          tr%Tisoch(k)=alpha*tr%Teff(i,j)&
                               +(1.-alpha)*tr%Teff(i+1,j)
                          tr%Lisoch(k)=alpha*tr%Lum(i,j)&
                               +(1.-alpha)*tr%Lum(i+1,j)
                          tr%gisoch(k)=alpha*tr%grav(i,j)&
                               +(1.-alpha)*tr%grav(i+1,j)
                       end do
                    else
                       do j=j1+1,j2
                          alpha=log10(time/tr%age(i+1,j))/&
                               log10(tr%age(i,j)/tr%age(i+1,j))
                          k=k+1
                          tr%m(k)=alpha*log10(tr%mass(i))&
                               +(1.-alpha)*log10(tr%mass(i+1))
                          tr%Tisoch(k)=alpha*tr%Teff(i,j)&
                               +(1.-alpha)*tr%Teff(i+1,j)
                          tr%Lisoch(k)=alpha*tr%Lum(i,j)&
                               +(1.-alpha)*tr%Lum(i+1,j)
                          tr%gisoch(k)=alpha*tr%grav(i,j)&
                               +(1.-alpha)*tr%grav(i+1,j)
                       end do
                    end if
                    if (j2*(j2-tr%nsteps(i+1)).ne.0) then
                       k=k+1
                       tr%m(k)=log10(tr%mass(i+1))
                       alpha=(time-tr%age(i+1,j2))/&
                            (tr%age(i+1,j2+1)-tr%age(i+1,j2))
                       tr%Tisoch(k)=tr%Teff(i+1,j2)&
                            +alpha*(tr%Teff(i+1,j2+1)-tr%Teff(i+1,j2))
                       tr%Lisoch(k)=tr%Lum(i+1,j2)&
                            +alpha*(tr%Lum(i+1,j2+1)-tr%Lum(i+1,j2))
                       tr%gisoch(k)=tr%grav(i+1,j2)&
                            +alpha*(tr%grav(i+1,j2+1)-tr%grav(i+1,j2))
                    end if
                 end if
              end if
           end do
           i=tr%nMS+tr%nHB
           do j=1,tr%nsteps(i)-1
              if ((tr%age(i,j)-time)*(tr%age(i,j+1)-time).le.0.) then
                 k=k+1
                 tr%m(k)=log10(tr%mass(i))
                 alpha=(time-tr%age(i,j))/(tr%age(i,j+1)-tr%age(i,j))
                 tr%Tisoch(k)=tr%Teff(i,j)+alpha*(tr%Teff(i,j+1)-tr%Teff(i,j))
                 tr%Lisoch(k)=tr%Lum(i,j)+alpha*(tr%Lum(i,j+1)-tr%Lum(i,j))
                 tr%gisoch(k)=tr%grav(i,j)+alpha*(tr%grav(i,j+1)-tr%grav(i,j))
              end if
           end do
           do i=tr%nVLM+tr%nHeF+1,tr%nMS-1
              if ((tr%nsteps(i).eq.tr%nsteps(i+1)).and.&
                   (tr%mass(i).lt.tr%mass(i+1)-1.e-6)) then
                 if (((tr%age(i,1)-time)*(tr%age(i+1,tr%nsteps(i+1))-time)&
                      .le.0.).or.((tr%age(i,tr%nsteps(i))-time)&
                      *(tr%age(i+1,1)-time).le.0.)) then
                    j1=1
                    j2=1
                    if (tr%age(i,1).gt.time) j1=0
                    if (tr%age(i,tr%nsteps(i)).lt.time) j1=tr%nsteps(i)
                    if (tr%age(i+1,1).gt.time) j2=0
                    if (tr%age(i+1,tr%nsteps(i+1)).lt.time) j2=tr%nsteps(i+1)
                    if (j1*(j1-tr%nsteps(i)).ne.0) then
                       do while ((tr%age(i,j1)-time)&
                            *(tr%age(i,j1+1)-time).gt.0.)
                          j1=j1+1
                       end do
                    end if
                    if (j2*(j2-tr%nsteps(i+1)).ne.0) then
                       do while ((tr%age(i+1,j2)-time)&
                            *(tr%age(i+1,j2+1)-time).gt.0.)
                          j2=j2+1
                       end do
                    end if
                    if (j1*(j1-tr%nsteps(i)).ne.0) then
                       k=k+1
                       tr%m(k)=log10(tr%mass(i))
                       alpha=(time-tr%age(i,j1))/&
                            (tr%age(i,j1+1)-tr%age(i,j1))
                       tr%Tisoch(k)=tr%Teff(i,j1)&
                            +alpha*(tr%Teff(i,j1+1)-tr%Teff(i,j1))
                       tr%Lisoch(k)=tr%Lum(i,j1)&
                            +alpha*(tr%Lum(i,j1+1)-tr%Lum(i,j1))
                       tr%gisoch(k)=tr%grav(i,j1)&
                            +alpha*(tr%grav(i,j1+1)-tr%grav(i,j1))
                    end if
                    if (j1.gt.j2) then
                       do j=j1,j2+1,-1
                          alpha=log10(time/tr%age(i+1,j))/&
                               log10(tr%age(i,j)/tr%age(i+1,j))
                          k=k+1
                          tr%m(k)=alpha*log10(tr%mass(i))&
                               +(1.-alpha)*log10(tr%mass(i+1))
                          tr%Tisoch(k)=alpha*tr%Teff(i,j)&
                               +(1.-alpha)*tr%Teff(i+1,j)
                          tr%Lisoch(k)=alpha*tr%Lum(i,j)&
                               +(1.-alpha)*tr%Lum(i+1,j)
                          tr%gisoch(k)=alpha*tr%grav(i,j)&
                               +(1.-alpha)*tr%grav(i+1,j)
                       end do
                    else
                       do j=j1+1,j2
                          alpha=log10(time/tr%age(i+1,j))/&
                               log10(tr%age(i,j)/tr%age(i+1,j))
                          k=k+1
                          tr%m(k)=alpha*log10(tr%mass(i))&
                               +(1.-alpha)*log10(tr%mass(i+1))
                          tr%Tisoch(k)=alpha*tr%Teff(i,j)&
                               +(1.-alpha)*tr%Teff(i+1,j)
                          tr%Lisoch(k)=alpha*tr%Lum(i,j)&
                               +(1.-alpha)*tr%Lum(i+1,j)
                          tr%gisoch(k)=alpha*tr%grav(i,j)&
                               +(1.-alpha)*tr%grav(i+1,j)
                       end do
                    end if
                    if (j2*(j2-tr%nsteps(i+1)).ne.0) then
                       k=k+1
                       tr%m(k)=log10(tr%mass(i+1))
                       alpha=(time-tr%age(i+1,j2))/&
                            (tr%age(i+1,j2+1)-tr%age(i+1,j2))
                       tr%Tisoch(k)=tr%Teff(i+1,j2)&
                            +alpha*(tr%Teff(i+1,j2+1)-tr%Teff(i+1,j2))
                       tr%Lisoch(k)=tr%Lum(i+1,j2)&
                            +alpha*(tr%Lum(i+1,j2+1)-tr%Lum(i+1,j2))
                       tr%gisoch(k)=tr%grav(i+1,j2)&
                            +alpha*(tr%grav(i+1,j2+1)-tr%grav(i+1,j2))
                    end if
                 end if
              end if
           end do
           i=tr%nMS
           do j=1,tr%nsteps(i)-1
              if ((tr%age(i,j)-time)*(tr%age(i,j+1)-time).le.0.) then
                 k=k+1
                 tr%m(k)=log10(tr%mass(i))
                 alpha=(time-tr%age(i,j))/(tr%age(i,j+1)-tr%age(i,j))
                 tr%Tisoch(k)=tr%Teff(i,j)+alpha*(tr%Teff(i,j+1)-tr%Teff(i,j))
                 tr%Lisoch(k)=tr%Lum(i,j)+alpha*(tr%Lum(i,j+1)-tr%Lum(i,j))
                 tr%gisoch(k)=tr%grav(i,j)+alpha*(tr%grav(i,j+1)-tr%grav(i,j))
              end if
           end do
        else

           !     Time = 0.

           do i=1,tr%nMS
              k=k+1
              tr%m(k)=log10(tr%mass(i))
              tr%Tisoch(k)=tr%Teff(i,1)
              tr%Lisoch(k)=tr%Lum(i,1)
              tr%gisoch(k)=tr%grav(i,1)
           end do
        end if

!        call date_and_time(values=ve2)
!        print*, ' cas time step = ',&
!             ((ve2(8)+1000.*(ve2(7)+60.*ve2(6)))-&
!             (vi2(8)+1000.*(vi2(7)+60.*vi2(6))))/1000.
!        vi2=ve2

        ! ----------------------------------------

        kend=k
        if (10.**tr%m(kend).gt.tr%massWD(1)) then
           do i=1,iWD-1
              if (((tr%ageWD(i,1)-time)*(tr%ageWD(i+1,21)-time)&
                   .le.0.).or.((tr%ageWD(i,21)-time)&
                   *(tr%ageWD(i+1,1)-time).le.0.)) then
                 j1=1
                 j2=1
                 if (tr%ageWD(i,1).gt.time) j1=0
                 if (tr%ageWD(i,21).lt.time) j1=21
                 if (tr%ageWD(i+1,1).gt.time) j2=0
                 if (tr%ageWD(i+1,21).lt.time) j2=21
                 if (j1*(j1-21).ne.0) then
                    do while ((tr%ageWD(i,j1)-time)&
                         *(tr%ageWD(i,j1+1)-time).gt.0.)
                       j1=j1+1
                    end do
                 end if
                 if (j2*(j2-21).ne.0) then
                    do while ((tr%ageWD(i+1,j2)-time)&
                         *(tr%ageWD(i+1,j2+1)-time).gt.0.)
                       j2=j2+1
                    end do
                 end if
                 if (j1*(j1-21).ne.0) then
                    k=k+1
                    tr%m(k)=log10(tr%massWD(i))
                    alpha=(time-tr%ageWD(i,j1))/&
                         (tr%ageWD(i,j1+1)-tr%ageWD(i,j1))
                    tr%Tisoch(k)=tr%TeffWD(i,j1)&
                         +alpha*(tr%TeffWD(i,j1+1)-tr%TeffWD(i,j1))
                    tr%Lisoch(k)=tr%LumWD(i,j1)&
                         +alpha*(tr%LumWD(i,j1+1)-tr%LumWD(i,j1))
                    tr%gisoch(k)=tr%gravWD(i,j1)&
                         +alpha*(tr%gravWD(i,j1+1)-tr%gravWD(i,j1))
                 end if
                 if (j1.gt.j2) then
                    do j=j1,j2+1,-1
                       alpha=log10(time/tr%ageWD(i+1,j))/&
                            log10(tr%ageWD(i,j)/tr%ageWD(i+1,j))
                       k=k+1
                       tr%m(k)=alpha*log10(tr%massWD(i))&
                            +(1.-alpha)*log10(tr%massWD(i+1))
                       tr%Tisoch(k)=alpha*tr%TeffWD(i,j)&
                            +(1.-alpha)*tr%TeffWD(i+1,j)
                       tr%Lisoch(k)=alpha*tr%LumWD(i,j)&
                            +(1.-alpha)*tr%LumWD(i+1,j)
                       tr%gisoch(k)=alpha*tr%gravWD(i,j)&
                            +(1.-alpha)*tr%gravWD(i+1,j)
                    end do
                 else
                    do j=j1+1,j2
                       alpha=log10(time/tr%ageWD(i+1,j))/&
                            log10(tr%ageWD(i,j)/tr%ageWD(i+1,j))
                       k=k+1
                       tr%m(k)=alpha*log10(tr%massWD(i))&
                            +(1.-alpha)*log10(tr%massWD(i+1))
                       tr%Tisoch(k)=alpha*tr%TeffWD(i,j)&
                            +(1.-alpha)*tr%TeffWD(i+1,j)
                       tr%Lisoch(k)=alpha*tr%LumWD(i,j)&
                            +(1.-alpha)*tr%LumWD(i+1,j)
                       tr%gisoch(k)=alpha*tr%gravWD(i,j)&
                            +(1.-alpha)*tr%gravWD(i+1,j)
                    end do
                 end if
                 if (j2*(j2-21).ne.0) then
                    k=k+1
                    tr%m(k)=log10(tr%massWD(i+1))
                    alpha=(time-tr%ageWD(i+1,j2))/&
                         (tr%ageWD(i+1,j2+1)-tr%ageWD(i+1,j2))
                    tr%Tisoch(k)=tr%TeffWD(i+1,j2)&
                         +alpha*(tr%TeffWD(i+1,j2+1)-tr%TeffWD(i+1,j2))
                    tr%Lisoch(k)=tr%LumWD(i+1,j2)&
                         +alpha*(tr%LumWD(i+1,j2+1)-tr%LumWD(i+1,j2))
                    tr%gisoch(k)=tr%gravWD(i+1,j2)&
                         +alpha*(tr%gravWD(i+1,j2+1)-tr%gravWD(i+1,j2))
                 end if
              end if
           end do
        end if

!        call date_and_time(values=ve2)
!        print*, ' next sub step = ',&
!             ((ve2(8)+1000.*(ve2(7)+60.*ve2(6)))-&
!             (vi2(8)+1000.*(vi2(7)+60.*vi2(6))))/1000.
!        vi2=ve2

        ! ----------------------------------------

        do j=-nCM,stell%nspectot
           fluxSSP(j)=0.
        end do
        !     Bolometric luminosity.
        Lbol=0.                
        !     Number of photons ionizing H.
        NHItot=0.              
        !     Number of photons ionizing He once.
        NHeItot=0.            
        !     Number of photons ionizing He twice.
        NHeIItot=0.            


        !--------------------------------------------------
        ! This is the double loop taking 10-2 sec.
        do i=1,k-1 ! i is the index along the isochrone at time "l"

           !     Division of the isochrone in smaller bins.

           nbins=int((tr%m(i+1)-tr%m(i))/0.01+1)&
                +int(abs(tr%Tisoch(i+1)-tr%Tisoch(i))/0.01+1)&
                +int(abs(tr%Lisoch(i+1)-tr%Lisoch(i))/0.01+1)
           dm=(tr%m(i+1)-tr%m(i))/nbins
           do p=0,nbins-1
              !°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
              call cpu_time(ti)
              mp=tr%m(i)+(p+0.5)*dm
              dn=nstars(mp,dm,fileIMF(iIMF),massinf,nIMFbins,&
                   coeffcont,slope,norm,massmin,massmax)
              Tp=tr%Tisoch(i)+(p+0.5)*(tr%Tisoch(i+1)-tr%Tisoch(i))/nbins
              Lp=tr%Lisoch(i)+(p+0.5)*(tr%Lisoch(i+1)-tr%Lisoch(i))/nbins
              gp=tr%gisoch(i)+(p+0.5)*(tr%gisoch(i+1)-tr%gisoch(i))/nbins
              Lbol=Lbol+dn*10.**Lp

!              call cpu_time(te)
!              print*, '1 = ',te-ti
!              ti=te
              ! time=1d-6
              !°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
              !     bin number p: centered on mp (log10(mass)) and of width dm
              !     (log10(mass)); contains dn stars.


              !     Computation of the fluxes emitted in the stellar spectra 
              !     with metallicity BELOW that of the tracks (idem for the number
              !     of photons).

              if (stell%grid_type(:str_length(stell%grid_type)).eq.'standard') &
                   then
                 !     For the LCB library, we have plenty of spectra at every metallicity, 
                 !     so we just stick to the metallicities *immediately* below and above the 
                 !     current metallicity.
                 izseekinf=iz
                 izseeksup=iz
              else
                 !     For the other libraries, we have fewer spectra, and possibly holes in the (T,g) diagram
                 !     around the current metallicity. So, we allow stars with more different 
                 !     metallicities (but with always delta_logT<0.1 and delta_logg<0.5)
                 !     to contribute to the final spectrum:
                 izseekinf=1
                 izseeksup=iz
              endif
              call interpstellib(Tp,gp,tr%metal,&
                   nCM,j1,j2,j3,j4,&
                   alpha1,alpha2,alpha3,alpha4,logZinf,&
                   izseekinf,izseeksup,&
                   iz1,iz2,iz3,iz4)

!              call cpu_time(te)
              ! time=8d-6
              !°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
!              print*, '2 = ',te-ti
!              ti=te

              !     Computation of the fluxes emitted in the stellar spectra 
              !     with metallicity ABOVE that of the tracks (idem for the number
              !     of photons).

              if (stell%grid_type(:str_length(stell%grid_type)).eq.'standard') &
                   then
                 !     For the LCB library, we have plenty of spectra at every metallicity, 
                 !     so we just stick to the metallicities *immediately* below and above the 
                 !     current metallicity.
                 izseekinf=iz+1
                 izseeksup=iz+1
              else
                 !     For the other libraries, we have fewer spectra, and possibly holes in the (T,g) diagram
                 !     around the current metallicity. So, we allow stars with more different 
                 !     metallicities (but with always delta_logT<0.1 and delta_logg<0.5)
                 !     to contribute to the final spectrum:
                 izseekinf=iz+1
                 izseeksup=stell%nz
              endif

              call interpstellib(Tp,gp,tr%metal,&
                   nCM,j5,j6,j7,j8,&
                   alpha5,alpha6,alpha7,alpha8,logZsup,&
                   izseekinf,izseeksup,&
                   iz5,iz6,iz7,iz8)


 
 !             call cpu_time(te)
              ! time=7d-6
              !°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
 !             print*, '3 = ',te-ti
 !             ti=te

              fz=(logZsup-log10(tr%metal))/(logZsup-logZinf)

              if (alpha1+alpha2+alpha3+alpha4.eq.0.) fz=0.d0
              if (alpha5+alpha6+alpha7+alpha8.eq.0.) fz=1.d0


              alpha1=alpha1*fz
              alpha2=alpha2*fz
              alpha3=alpha3*fz
              alpha4=alpha4*fz
              NHItot=NHItot+10.**Lp*dn*(&
                   alpha1*stell%NHI(iz1,j1)+&
                   alpha2*stell%NHI(iz2,j2)+&
                   alpha3*stell%NHI(iz3,j3)+&
                   alpha4*stell%NHI(iz4,j4))
              NHeItot=NHeItot+10.**Lp*dn*(&
                   alpha1*stell%NHeI(iz1,j1)+&
                   alpha2*stell%NHeI(iz2,j2)+&
                   alpha3*stell%NHeI(iz3,j3)+&
                   alpha4*stell%NHeI(iz4,j4))
              NHeIItot=NHeIItot+10.**Lp*dn*(&
                   alpha1*stell%NHeII(iz1,j1)+&
                   alpha2*stell%NHeII(iz2,j2)+&
                   alpha3*stell%NHeII(iz3,j3)+&
                   alpha4*stell%NHeII(iz4,j4))
              if (j1.gt.0) j1=j1+stell%firstspec(iz1)-1
              if (j2.gt.0) j2=j2+stell%firstspec(iz2)-1
              if (j3.gt.0) j3=j3+stell%firstspec(iz3)-1
              if (j4.gt.0) j4=j4+stell%firstspec(iz4)-1
              fluxSSP(j1)=fluxSSP(j1)+10.**Lp*alpha1*dn
              fluxSSP(j2)=fluxSSP(j2)+10.**Lp*alpha2*dn
              fluxSSP(j3)=fluxSSP(j3)+10.**Lp*alpha3*dn
              fluxSSP(j4)=fluxSSP(j4)+10.**Lp*alpha4*dn


              alpha5=alpha5*(1.d0-fz)
              alpha6=alpha6*(1.d0-fz)
              alpha7=alpha7*(1.d0-fz)
              alpha8=alpha8*(1.d0-fz)
              NHItot=NHItot+10.**Lp*dn*(alpha5*stell%NHI(iz5,j5)+&
                   alpha6*stell%NHI(iz6,j6)+alpha7*stell%NHI(iz7,j7)&
                   +alpha8*stell%NHI(iz8,j8))
              NHeItot=NHeItot+10.**Lp*dn*(alpha5*stell%NHeI(iz5,j5)+&
                   alpha6*stell%NHeI(iz6,j6)+alpha7*stell%NHeI(iz7,j7)&
                   +alpha8*stell%NHeI(iz8,j8))
              NHeIItot=NHeIItot+10.**Lp*dn*(alpha5*stell%NHeII(iz5,j5)+&
                   alpha6*stell%NHeII(iz6,j6)+alpha7*stell%NHeII(iz7,j7)&
                   +alpha8*stell%NHeII(iz8,j8))
              if (j5.gt.0) j5=j5+stell%firstspec(iz5)-1
              if (j6.gt.0) j6=j6+stell%firstspec(iz6)-1
              if (j7.gt.0) j7=j7+stell%firstspec(iz7)-1
              if (j8.gt.0) j8=j8+stell%firstspec(iz8)-1
              fluxSSP(j5)=fluxSSP(j5)+10.**Lp*alpha5*dn
              fluxSSP(j6)=fluxSSP(j6)+10.**Lp*alpha6*dn
              fluxSSP(j7)=fluxSSP(j7)+10.**Lp*alpha7*dn
              fluxSSP(j8)=fluxSSP(j8)+10.**Lp*alpha8*dn


              ! if (l.eq.3.and.(j1.eq.3350.or.j2.eq.3350.or.j3.eq.3350.or.j4.eq.3350.or.&
              !      j5.eq.3350.or.j6.eq.3350.or.j7.eq.3350.or.j8.eq.3350)) then
              !    print*, 'AH!'
              !    print*, tr%metal,i,p,mp,Tp,Lp,gp,Lbol,iz,iz+1,nCM,logZinf,logZsup
              !    print*, j1,j2,j3,j4,j5,j6,j7,j8
              !    print*, iz1,iz2,iz3,iz4,iz5,iz6,iz7,iz8
              !    print*, alpha1,alpha2,alpha3,alpha4,alpha5,alpha6,alpha7,alpha8

             !!      print*, stell%z
              !    print*, log10(stell%z)
              !    print*, log10(tr%metal)
              ! endif

!              call cpu_time(te)
              ! time=7d-6
              !°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
!              print*, '4 = ',te-ti
!              ti=te
           end do
        end do


!        call date_and_time(values=ve2)
!        print*, ' do i=0,k-1 loop = ',&
!             ((ve2(8)+1000.*(ve2(7)+60.*ve2(6)))-&
!             (vi2(8)+1000.*(vi2(7)+60.*vi2(6))))/1000.
!        vi2=ve2

        ! ----------------------------------------



        !     Only the fluxes in the spectra really used will be written.

        nused=0
        do j=-nCM,stell%nspectot
           if (fluxSSP(j).gt.0.) then
              nused=nused+1
              iused(nused)=j
           end if
        end do

        !     Computation of the mass of gas and metals ejected since the
        !     previous timestep. The apportionment between "isolated" stars [IS]
        !     and stars in close binary systems [CB] is made in spectra.f ("fSNIa").

        nSNII_IS=0.
        nSNII_CB=0.
        nSNIa=0.
        ejecta_IS=0.
        ejectaZ_IS=0.
        ejecta_CB=0.
        ejectaZ_CB=0.
        massBHNS_IS=0.
        massBHNS_CB=0.
        massWD_IS=0.
        massWD_CB=0.
        nbins=int((mend-tr%m(kend))/0.01+1)
        dm=(mend-tr%m(kend))/nbins
        do p=0,nbins-1
           mp=tr%m(kend)+(p+0.5)*dm

           !     "Isolated" star (single star or member of a loose binary system).

           dn=nstars(mp,dm,fileIMF(iIMF),massinf,nIMFbins,&
                coeffcont,slope,norm,massmin,massmax)
           ejecta_IS=ejecta_IS+dn*interpol(10.**mp,tr%mr,tr%r,tr%nr)
           ejectaZ_IS=ejectaZ_IS+dn*interpol(10.**mp,tr%mr,tr%rmet,tr%nr)
           if (10.**mp.ge.mSNII) then 
              !     SNII.
              nSNII_IS=nSNII_IS+dn
              massBHNS_IS=massBHNS_IS+dn*(10.**mp &
                                !     Mass locked in black holes or neutron stars.
                   -interpol(10.**mp,tr%mr,tr%r,tr%nr))
           else
              massWD_IS=massWD_IS+dn*(10.**mp &
                                !     Mass locked in white dwarfs.
                   -interpol(10.**mp,tr%mr,tr%r,tr%nr))
           end if

           !     Close binary system.
           !     massmin<=m2<=m1<=massmax.
           !     mu=m2/(m1+m2).
           !     f(mu)=Cbin*mu**gammabin: mass-ratio function.
           !     integral_{muinf}^{musup}f(mu).dmu=1.
           !     mbin=m1+m2 is assumed to be distributed according the IMF
           !     for "isolated" stars.

           !     Primary star of a close binary system.     

           m1=10.**mp
           muinf=massmin/(massmin+m1)
           m2max=min(m1,massmax-m1)
           musup=m2max/(m2max+m1)
           !     (mu is an increasing function of m2)
           if (musup.gt.muinf) then
              Cbin=(gammabin+1.)/&
                   (musup**(gammabin+1.)-muinf**(gammabin+1.))
              nmu=int((musup-muinf)/0.001+1)
              dmu=(musup-muinf)/nmu
              do q=0,nmu-1
                 mu=muinf+(q+0.5)*dmu
                 mbin=m1/(1.-mu)
                 dn=nstars(log10(mbin),dm,fileIMF(iIMF),massinf,&
                      nIMFbins,coeffcont,slope,norm,&
                      massmin,massmax)
                 dn12=dn*Cbin*mu**gammabin*dmu
                 ejecta_CB=ejecta_CB+dn12*interpol(m1,tr%mr,tr%r,tr%nr)
                 ejectaZ_CB=ejectaZ_CB+dn12*interpol(m1,tr%mr,tr%rmet,tr%nr)
                 if (m1.ge.mSNII) then
                    nSNII_CB=nSNII_CB+dn12
                    massBHNS_CB=massBHNS_CB+dn12*(m1&
                         -interpol(m1,tr%mr,tr%r,tr%nr))
                 else
                    massWD_CB=massWD_CB+dn12*(m1&
                         -interpol(m1,tr%mr,tr%r,tr%nr))
                 end if
              end do
           end if

           !     Secondary star of a close binary system.

           m2=10.**mp
           muinf=m2/massmax
           musup=0.5
           if (musup.gt.muinf) then
              Cbin=(gammabin+1.)/&
                   (musup**(gammabin+1.)-muinf**(gammabin+1.))
              nmu=int((musup-muinf)/0.001+1)
              dmu=(musup-muinf)/nmu
              do q=0,nmu-1
                 mu=muinf+(q+0.5)*dmu
                 m1=m2*(1.-mu)/mu
                 mbin=m1+m2
                 dn=nstars(log10(mbin),dm,fileIMF(iIMF),massinf,&
                      nIMFbins,coeffcont,slope,norm,&
                      massmin,massmax)
                 dn12=dn*Cbin*mu**2*dmu
                 if (mbin.lt.mSNIa.or.m1.gt.mSNII) then 
                    !     Mass too small or the remnant of the primary is not a white dwarf. 
                    ejecta_CB=ejecta_CB+dn12*interpol(m2,tr%mr,tr%r,tr%nr)
                    ejectaZ_CB=ejectaZ_CB&
                         +dn12*interpol(m2,tr%mr,tr%rmet,tr%nr)
                    if (m2.ge.mSNII) then
                       nSNII_CB=nSNII_CB+dn12
                       massBHNS_CB=massBHNS_CB+dn12*(m2&
                            -interpol(m2,tr%mr,tr%r,tr%nr))
                    else
                       massWD_CB=massWD_CB+dn12*(m2&
                            -interpol(m2,tr%mr,tr%r,tr%nr))
                    end if
                 else          
                    !     SNIa.
                    nSNIa=nSNIa+dn12
                    mWD1=m1-interpol(m1,tr%mr,tr%r,tr%nr) 
                    !     Mass of the white dwarf remnant of the primary.
                    massWD_CB=massWD_CB-dn12*mWD1
                    !     Destroyed.
                    deltam=mCh-mWD1 
                    !     Mass ejected by the secondary used by the primary to reach 
                    !     the Chandrasekhar mass.
                    r2=interpol(m2,tr%mr,tr%r,tr%nr)
                    ejecta_CB=ejecta_CB+dn12*&
                         (r2-deltam &
                                !     Ejecta of the secondary not processed by the primary.
                         +mCh) 
                    !     Ejecta of the primary (totally destroyed).
                    ejectaZ_CB=ejectaZ_CB+dn12*&
                         (interpol(m2,tr%mr,tr%rmet,tr%nr)*(r2-deltam)/r2 &
                                !     Secundary.
                         +mCh) 
                    !     Primary (no H or He remains).
                 end if
              end do
           end if
        end do

        call date_and_time(values=ve2)
!        print*, ' SNe treatment = ',&
!             ((ve2(8)+1000.*(ve2(7)+60.*ve2(6)))-&
!             (vi2(8)+1000.*(vi2(7)+60.*vi2(6))))/1000.
!        vi2=ve2

        ! ----------------------------------------

        mend=tr%m(kend)
        dtime=max(time-timeprev+0.d0,1.d0)

        write(40,'(i5,2x,i4,1x,5(1x,e11.5))') time,&
             nused,10.**mend,Lbol,NHItot,NHeItot,NHeIItot
        write(40,'(7(1x,e11.5))') nSNII_IS/dtime,nSNII_CB/dtime,&
             nSNIa/dtime,massBHNS_IS/dtime,massBHNS_CB/dtime,&
             massWD_IS/dtime,massWD_CB/dtime
        write(40,'(4(1x,e11.5))') &
             ejecta_IS/dtime,ejecta_CB/dtime,ejectaZ_IS/dtime,&
             ejectaZ_CB/dtime
        write(40,'(5(i4,1x,e11.6,1x))') (iused(k),&
             fluxSSP(iused(k)),k=1,nused)
        timeprev=time

     end do
     call date_and_time(values=ve1)
     print*, '                                        isocrhone step = ',&
          ((ve1(8)+1000.*(ve1(7)+60.*ve1(6)))-&
          (vi1(8)+1000.*(vi1(7)+60.*vi1(6))))/1000.
     vi1=ve1

     close(40)

     call date_and_time(values=ve)
     print*, '                                        last Z track = ',&
          ((ve(8)+1000.*(ve(7)+60.*ve(6)))-&
          (vi(8)+1000.*(vi(7)+60.*vi(6))))/1000.
     vi=ve

  enddo
  close(lun)

end program SSPs_HR

!********************************

