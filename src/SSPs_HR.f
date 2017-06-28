      program SSPs_HR


      implicit none

      include 'peg_config.f'
      include 'peg_include.f'

      character*1   answerwinds,answerejecta,useCM
      character*72  char_massmin,char_massmax
      character*280  fileSSP,filetracks(nmaxZtracks)
      character*280  fileIMF(nmaxIMFfiles),prefix
      character*280  whole_filename
      character*256 line
      character*30  grid_type
      integer i,j,k,nMS,nsteps(nmaxMS),j1,j2,p,nbins
      integer nspec(nmaxZl),l,iWD,error,iIMF,itracks
      integer nIMFbins,time,timeprev,q,nVLM,nIMFfiles,ntracksfiles
      integer nHB,nHeF,isupp,nstepssupp(nmaxMS)
      integer nHBsupp,kend,j3,j4,nZ_WW,nmass_WW
      integer j5,j6,j7,j8
      integer nWD(nmaxMS),nWDsupp(nmaxMS)
      integer nused,nprec(nmaxZl),iz,nz,nspectot
      integer iz1,iz2,iz3,iz4,iz5,iz6,iz7,iz8
      integer iused(nmaxZl*nmaxsl),nr,nmu,nCM
      integer lun,luntrack               ! Used for logical unit numbers
*     Double precision is required for post-AGB stars.
      double precision TCM(nmaxCM),gCM(nmaxCM),NHICM(nmaxCM)
      double precision NHeICM(nmaxCM),NHeIICM(nmaxCM)
      double precision Tspec(nmaxZl,-nmaxCM:nmaxsl)
      double precision alpha1,alpha2,alpha3,alpha4
      double precision alpha5,alpha6,alpha7,alpha8
      double precision alpha,mass(nmaxMS),Lum(nmaxMS,1000),m(1000)
      double precision age(nmaxMS,1000),Teff(nmaxMS,1000)
      double precision Tisoch(1000),Lisoch(1000),norm,grav(nmaxMS,1000)
      double precision dm,mp,Tp,Lp
      double precision fluxSSP(-nmaxCM:nmaxZl*nmaxsl),dn
      double precision coeffcont(nmaxIMFbins),massinf(nmaxIMFbins)
      double precision slope(nmaxIMFbins),masssup,lifetime(nmaxMS)
      double precision mend,nstars,yield,dtime
      double precision nSNIa,m2,mSNIa,mu,mCh,m1
      double precision massHB(nmaxMS),massfinal(nmaxMS)
      double precision msupp(nmaxMS),agesupp(nmaxMS,1000)
      double precision Teffsupp(nmaxMS,1000),gravsupp(nmaxMS,1000)
      double precision Lumsupp(nmaxMS,1000)
      double precision Mc(nmaxMS),massWD(nmaxMS),deltat
      double precision TeffWD(nmaxMS,30),LumWD(nmaxMS,30)
      double precision gravWD(nmaxMS,30),ageWD(nmaxMS,30)
      double precision gp,gisoch(1000)
      double precision gspec(nmaxZl,-nmaxCM:nmaxsl)
      double precision metal,z(nmaxZl),fz
      double precision NHI(nmaxZl,-nmaxCM:nmaxsl)
      double precision NHeI(nmaxZl,-nmaxCM:nmaxsl)
      double precision NHeII(nmaxZl,-nmaxCM:nmaxsl)
      double precision NHItot,NHeItot,NHeIItot,rZ(nmaxMS),mSNII,Lbol
      double precision mr(nmaxMS),r(nmaxMS),rmet(nmaxMS)
      double precision interpol,ej,ejZ,Cbin,mbin,massmin,massmax,m2max
      double precision dmu,muinf,musup,dn12,mWD1,deltam,r2,gammabin
      double precision ejecta_IS,ejectaZ_IS,ejecta_CB,ejectaZ_CB
      double precision massBHNS_IS,massWD_IS,massBHNS_CB,massWD_CB
      double precision nSNII_IS,nSNII_CB,ejectatot,ejectaZtot,mmin,mmax
      double precision mass_WW(10),ejectaX_WW(10,3,5),ejectaY_WW(10,3,5)
      double precision ejectaZ_WW(10,3,5)
      double precision ejectatot_WW(10,3,5),Z_WW(5)
      double precision logZinf,logZsup

      character*280 stellib
      integer      str_length
      integer      izseekinf,izseeksup

*     Minimal mass of a binary system to produce a SNIa 
*     (Matteucci & Greggio 86).
      mSNIa=3.                  
*     Chandrasekhar mass.
      mCh=1.4                   
*     Exponent of the mass-ratio function of binary stars (MC86).
      gammabin=2.               


*     Read the list of IMF
      lun=60
      call file_open(PEG_ROOT//'data/user_defined/list_IMFs.dat',
     s     lun,error)
      if (error.ne.0) then
         write(0,*) 'SSPs_HR: Could not open file ',
     s        PEG_ROOT//'data/user_defined/list_IMFs.dat'
         call exit(1)
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
               call exit(1)
            endif
         endif
      enddo
      close(lun)
      fileIMF(nIMFfiles+1)='ln'
      fileIMF(nIMFfiles+2)='RB'
      fileIMF(nIMFfiles+3)='Fe'      

      write(*,*) ' '
      write(*,*) 'Initial mass function?'
      do i=1,nIMFfiles
         write(*,'(1x,i3,a,a)') i,': ',trim(fileIMF(i))
      enddo
      write(*,'(1x,i3,a)') nIMFfiles+1,': lognormal IMF'
      write(*,'(1x,i3,a)') nIMFfiles+2,': Rana & Basu (1992)'
*     Ferrini F. 1991, Chemical and Dynamical Evolution of Galaxies,
*     F. Ferrini, F. Matteucci, J. Franco (eds.), p. 520
      write(*,'(1x,i3,a)') nIMFfiles+3,': Ferrini (1991)'      


*     Choose an IMF and read the file
      read(*,*) iIMF      
      if (iIMF.le.0.or.iIMF.gt.nIMFfiles+3) then
         write(0,*) 'SSPs_HR: IMF number is invalid', iIMF
         call exit(1)
      endif
      if ((fileIMF(iIMF).ne.'ln').and.(fileIMF(iIMF).ne.'RB')
     $     .and.(fileIMF(iIMF).ne.'Fe')) then
         lun=30
         call file_open(PEG_ROOT//'data/user_defined/'//fileIMF(iIMF),
     s        lun,error)
         if (error.ne.0) then
            write(0,*) 'SSPs_HR: Could not open file ',
     s           PEG_ROOT//'data/user_defined/'//fileIMF(iIMF)
            call exit(1)
         endif
         read(lun,*) nIMFbins
         do i=1,nIMFbins
            read(lun,*) massinf(i),slope(i)
         end do
         read(lun,*) masssup
         coeffcont(1)=1.
         do i=2,nIMFbins
            coeffcont(i)=coeffcont(i-1)*massinf(i)**
     $           (slope(i-1)-slope(i))
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
         write(*,'(1x,a,e8.3,a)') 'Lower mass (default=',massmin,
     $        ' Msol)?'
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
         write(*,'(1x,a,e8.3,a)') 'Upper mass (default=',massmax,
     $        ' Msol)?'
         read(*,'(a)') char_massmax
         if (char_massmax.ne.' ') then
            read(char_massmax,*,iostat=error) massmax
         else
            error=0
         end if
         if (error.ne.0) write(*,*) 'Invalid input!'
      end do

      write(*,*) ' '
      write(*,*) 'SNII ejecta: model A, B or C ', 
     $     'of Woosley & Weaver (A/B [default]/C)?'
      read(*,'(a)') answerejecta 
      if (answerejecta.eq.'a') answerejecta='A'
      if (answerejecta.eq.'b') answerejecta='B'
      if (answerejecta.eq.'c') answerejecta='C'
      if (answerejecta.ne.'A'.and.answerejecta.ne.'C') 
     $     answerejecta='B'


*     Read stellar yields of Woosley and Weather (1995)
      lun=50
      call file_open(PEG_ROOT//'data/external/WW.dat',
     s     lun,error)
      nmass_WW=10
      read(lun,*) nZ_WW
      read(lun,*) (mass_WW(i),i=1,7),(mass_WW(8),j=1,2),
     $     ((mass_WW(i),j=1,3),i=9,nmass_WW)
      do k=1,nZ_WW
         read(lun,*) Z_WW(k)
         read(lun,*)(ejectaX_WW(i,1,k),i=1,7),
     $        (ejectaX_WW(8,j,k),j=1,2),
     $        ((ejectaX_WW(i,j,k),j=1,3),i=9,nmass_WW) 
         read(lun,*)(ejectaY_WW(i,1,k),i=1,7),
     $        (ejectaY_WW(8,j,k),j=1,2),
     $        ((ejectaY_WW(i,j,k),j=1,3),i=9,nmass_WW)
         read(lun,*)(ejectatot_WW(i,1,k),i=1,7),
     $        (ejectatot_WW(8,j,k),j=1,2),
     $        ((ejectatot_WW(i,j,k),j=1,3),i=9,nmass_WW)
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

***** Computation of the net yield of metals for the supernovae

      do i=1,nmass_WW
         do j=1,3
            do k=1,nZ_WW
               ejectaZ_WW(i,j,k)=ejectatot_WW(i,j,k)*(1.-Z_WW(k))
     $              -ejectaX_WW(i,j,k)-ejectaY_WW(i,j,k)
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
      call file_open(PEG_ROOT//'data/user_defined/list_tracks.dat',
     s     lun,error)
      if (error.ne.0) then
         write(0,*) 'SSPs_HR: Could not open file ',
     s        PEG_ROOT//'data/user_defined/list_tracks.dat'
         call exit(1)
      endif
      do i=1,nmaxZtracks
         read(lun,'(a)',end=1002) filetracks(i)
         if (filetracks(i).ne.' ') ntracksfiles=i
      enddo
 1002 close(lun)


***** Read the stellar libraries
*     Use the Clegg & Middlemass library of hot stars anyway:
      useCM='y'

      if (useCM.eq.'n') then
         nCM = -1
      else
         open(90,file=PEG_ROOT//'data/external/stellibCM.dat',
     $        status='old')
         read(90,*) nCM
         do i=1,nCM
            read(90,*) TCM(i),gCM(i),NHICM(i),NHeICM(i),NHeIICM(i)
         enddo
         close(90)
      endif
      
      call choose_stellib(stellib, error)
      if (error.ne.0) then
         write(0,*)'SSPs_HR: Failed to select a stellar library'
         call exit(1)
      endif

      call read_para_stellib(stellib, grid_type, 
     $     nz, nspec, z, Tspec, gspec, NHI, NHeI, NHeII)
*     nspec now contains the array of number of spectra used 
*     in LCB or ELO library, for each metallicity

      nspectot=0
      do iz = 1, nz
         nspectot=nspectot+nspec(iz)
         nprec(iz)=0
         do i=1,nCM
            Tspec(iz,-i)=TCM(i)
            gspec(iz,-i)=gCM(i)
            NHI(iz,-i)=NHICM(i)
            NHeI(iz,-i)=NHeICM(i)
            NHeII(iz,-i)=NHeIICM(i)
         enddo
      enddo

      do iz=2,nz
         nprec(iz)=nprec(iz-1)+nspec(iz-1)
      end do


***** Create the SSPs.dat file
*     This file contains a short header. Its contains is the list of SSP
*     which are contained in separate files.
*     the SSPs.dat file is read by the routine ssp_files_read (in ssp_io.f)
      write(*,*) 'Prefix?'
      read(*,'(a)') prefix
      lun=80
      call file_unit(lun)
      open(lun,file=prefix(1:index(prefix,' ')-1)//'_'//'SSPs.dat',
     $     status='new')
      write(lun, '(a)') 'format: PEGASE-HR/SSPs   '
      write(lun, '(a)') 'version: 1               '
      write(lun, '(a)') 'grid type: ',grid_type

      write(lun, '(a)') 'lib: '//stellib(:str_length(stellib))
      write(lun, '(a)') 'head_end:'

***** Loop on the SSP files
      do itracks=1,ntracksfiles         
         write(*,*) ' Computing isochones # ',itracks,'/',ntracksfiles
         whole_filename=PEG_ROOT//'data/tracks/'//filetracks(itracks)
         call file_unit(luntrack)
         open(luntrack,status='old',file=whole_filename)
         read(luntrack,*) nMS,nHeF,nHB,nVLM,metal,mSNII
*     nVLM: number of tracks of very low mass stars (~<=0.5 Msol) 
*     which do not evolve.
*     nHeF: number of tracks (ZAMS->tip of the RGB) 
*     of low mass stars which undergo the helium flash
*     and have to be connected to ZAHB tracks.
*     nMS: number of ZAMS tracks.
*     nHB: number of ZAHB tracks.
         do i=1,nMS
            read(luntrack,*) mass(i),nsteps(i),Mc(i),massfinal(i),nWD(i),rZ(i)
            do j=1,nsteps(i)+nWD(i)*21
               read(luntrack,*) Lum(i,j),Teff(i,j),grav(i,j),age(i,j)
            end do
            m(i)=log10(mass(i))     
         end do
         do i=nMS+1,nMS+nHB-1
            read(luntrack,*) massHB(i),nsteps(i),Mc(i),massfinal(i),
     $           nWD(i),rZ(i)
            j=nVLM
            do while ((massfinal(j)-massHB(i))
     $           *(massfinal(j+1)-massHB(i)).gt.0.)
               j=j+1
            end do
            mass(i)=mass(j)+(mass(j+1)-mass(j))
     $           *(massHB(i)-massfinal(j))
     $           /(massfinal(j+1)-massfinal(j)) 
            m(i)=log10(mass(i))
            lifetime(i)=log10(age(j,nsteps(j)))
     $           +(m(i)-m(j))
     $           *(log10(age(j+1,nsteps(j+1)))-log10(age(j,nsteps(j))))
     $           /(m(j+1)-m(j))
            lifetime(i)=10.**lifetime(i)
            do j=1,nsteps(i)+nWD(i)*21
               read(luntrack,*) Lum(i,j),Teff(i,j),grav(i,j),age(i,j)
            end do
         end do
         
         i=nMS+nHB
         read(luntrack,*) massHB(i),nsteps(i),Mc(i),massfinal(i),nWD(i),rZ(i)
         do j=1,nsteps(i)+nWD(i)*21
            read(luntrack,*) Lum(i,j),Teff(i,j),grav(i,j),age(i,j)
            lifetime(i)=age(nVLM+nHeF,nsteps(nVLM+nHeF))
         end do
         mass(i)=mass(nVLM+nHeF)
         m(i)=log10(mass(i))
         
         i=nVLM+nHeF+1
         deltat=age(nVLM+nHeF,nsteps(nVLM+nHeF))
     $        +age(nMS+nHB,nsteps(nMS+nHB))
     $        -age(i,nsteps(i))
         do j=nsteps(i),nsteps(i)+nWD(i)*21
            age(i,j)=age(i,j)+deltat
         end do
         close(10)  
         
         nr=0
         do i=nMS+1,nMS+nHB-1,2
            nr=nr+1
            mr(nr)=mass(i)
            r(nr)=mass(i)-massfinal(i)
            rmet(nr)=rZ(i)
            if (rmet(nr).lt.1.e-4) rmet(nr)=0.
         end do
         
         i=nMS+nHB
         nr=nr+1
         mr(nr)=mass(i)
         r(nr)=mass(i)-massfinal(i)
         rmet(nr)=rZ(i)
         if (rmet(nr).lt.1.e-4) rmet(nr)=0.
         do i=nVLM+nHeF+2,nMS,2
            nr=nr+1
            mr(nr)=mass(i)
            if (mass(i).lt.mSNII) then
               r(nr)=mass(i)-massfinal(i)
               rmet(nr)=rZ(i)
               if (rmet(nr).lt.1.e-4) rmet(nr)=0.
            else
               call Woosley(mass(i),metal,massfinal(i),rZ(i),nZ_WW,
     $              Z_WW,nmass_WW,mass_WW,ejectaZ_WW,
     $              ejectatot_WW,answerwinds,answerejecta,ej,ejZ)
               r(nr)=ej
               rmet(nr)=ejZ
            end if
         end do
         
         j=nMS+1
         i=nVLM+1
         isupp=0
         nHBsupp=0

*     Extension of the ZAMS tracks by ZAHB tracks for stars undergoing
*     the Helium flash AND "extension" of the ZAHB tracks by ZAMS tracks.
         
         do while (i.lt.nVLM+nHeF)
            if (massfinal(i).lt.massHB(nMS+1)) then
               if (mass(i).ge.massHB(nMS+1)) then 
                  
*     Stars with a ZAMS mass higher than the lowest ZAHB mass [massHB(nMS+1)],
*     but which after the RGB have a lower mass than massHB(nMS+1), are
*     assigned the evolution corresponding to massHB(nMS+1) after the RGB.

                  nHBsupp=nHBsupp+1
                  isupp=isupp+1
                  msupp(isupp)=m(i)
                  nstepssupp(isupp)=nsteps(nMS+1)
                  nWDsupp(isupp)=nWD(nMS+1)
                  do k=1,nsteps(nMS+1)+nWD(nMS+1)*21
                     agesupp(isupp,k)=age(i,nsteps(i))+age(nMS+1,k)
                     Lumsupp(isupp,k)=Lum(nMS+1,k)
                     Teffsupp(isupp,k)=Teff(nMS+1,k)
                     gravsupp(isupp,k)=grav(nMS+1,k)
                  end do
                  i=i+1
               end if
            else
               if (m(j).lt.m(i)) then
                  
*     "Extension" of the ZAHB tracks by ZAMS tracks.
                  
                  isupp=isupp+1
                  msupp(isupp)=m(j)
                  nstepssupp(isupp)=nsteps(j)
                  nWDsupp(isupp)=nWD(j)
                  do k=1,nsteps(j)+nWD(j)*21
                     agesupp(isupp,k)=lifetime(j)+age(j,k)
                     Lumsupp(isupp,k)=Lum(j,k)
                     Teffsupp(isupp,k)=Teff(j,k)
                     gravsupp(isupp,k)=grav(j,k)
                  end do
                  j=j+1
               else
                  
*     Extension of the ZAMS tracks by ZAHB tracks.

                  isupp=isupp+1
                  nHBsupp=nHBsupp+1
                  alpha=(m(j)-m(i))/(m(j)-m(j-1))
                  msupp(isupp)=m(i)
                  nstepssupp(isupp)=nsteps(j-1)
                  nWDsupp(isupp)=nWD(j-1)
                  agesupp(isupp,1)=age(i,nsteps(i))
                  Lumsupp(isupp,1)=alpha*Lum(j-1,1)+(1.-alpha)*Lum(j,1)
                  Teffsupp(isupp,1)=alpha*Teff(j-1,1)
     $                 +(1.-alpha)*Teff(j,1)
                  gravsupp(isupp,1)=alpha*grav(j-1,1)
     $                 +(1.-alpha)*grav(j,1)
                  do k=2,nsteps(j-1)+nWD(j-1)*21
                     agesupp(isupp,k)=age(i,nsteps(i))
     $                    +10.**(alpha*log10(age(j-1,k))
     $                    +(1.-alpha)*log10(age(j,k)))
                     Lumsupp(isupp,k)=alpha*Lum(j-1,k)
     $                    +(1.-alpha)*Lum(j,k)
                     Teffsupp(isupp,k)=alpha*Teff(j-1,k)
     $                    +(1.-alpha)*Teff(j,k)
                     gravsupp(isupp,k)=alpha*grav(j-1,k)
     $                    +(1.-alpha)*grav(j,k)
                  end do
                  i=i+1
               end if
            end if
         end do

*     "Extension" of the remaining ZAHB tracks by ZAMS tracks.
         
         do i=j,nMS+nHB
            isupp=isupp+1
            msupp(isupp)=m(i)
            nstepssupp(isupp)=nsteps(i)
            nWDsupp(isupp)=nWD(i)
            do k=1,nsteps(i)+nWD(i)*21
               agesupp(isupp,k)=lifetime(i)+age(i,k)
               Lumsupp(isupp,k)=Lum(i,k)
               Teffsupp(isupp,k)=Teff(i,k)
               gravsupp(isupp,k)=grav(i,k)
            end do
         end do
         nHB=nHB+nHBsupp
         do i=nMS+1,nMS+nHB
            isupp=i-nMS
            m(i)=msupp(isupp)         
            nsteps(i)=nstepssupp(isupp)
            nWD(i)=nWDsupp(isupp)
            mass(i)=10.**m(i)
            do j=1,nsteps(i)+nWD(i)*21
               age(i,j)=agesupp(isupp,j)
               Lum(i,j)=Lumsupp(isupp,j)
               Teff(i,j)=Teffsupp(isupp,j)
               grav(i,j)=gravsupp(isupp,j)
            end do         
         end do

         iWD=0
         do i=nMS+1,nMS+nHB-1
            if (nWD(i).gt.0) then
               iWD=iWD+1
               massWD(iWD)=mass(i)
               do j=1,21
                  LumWD(iWD,j)=Lum(i,j+nsteps(i))
                  TeffWD(iWD,j)=Teff(i,j+nsteps(i))
                  gravWD(iWD,j)=grav(i,j+nsteps(i))
                  ageWD(iWD,j)=age(i,j+nsteps(i))
               end do
            end if
         end do
         do i=nVLM+nHeF+1,nMS
            if (nWD(i).gt.0) then
               iWD=iWD+1
               massWD(iWD)=mass(i)
               do j=1,21
                  LumWD(iWD,j)=Lum(i,j+nsteps(i))
                  TeffWD(iWD,j)=Teff(i,j+nsteps(i))
                  gravWD(iWD,j)=grav(i,j+nsteps(i))
                  ageWD(iWD,j)=age(i,j+nsteps(i))
               end do
            end if         
         end do

         iz=1
         do while ((metal-z(iz))*(metal-z(iz+1)).gt.0.)
            iz=iz+1
         end do
         fz=(z(iz+1)-metal)/(z(iz+1)-z(iz))

         ejectatot=0.
         ejectaZtot=0.
         mmin=log10(massmin)
         mmax=log10(massmax)
         nbins=int((mmax-mmin)/0.001+1)
         dm=(mmax-mmin)/nbins
         norm=0.
         do p=0,nbins-1
            mp=mmin+(p+0.5)*dm
            dn=nstars(mp,dm,fileIMF(iIMF),massinf,nIMFbins,
     $           coeffcont,slope,1.d0,massmin,massmax)
            norm=norm+dn*10.**mp
            ejectatot=ejectatot+dn*interpol(10.**mp,mr,r,nr)
            ejectaZtot=ejectaZtot+dn*interpol(10.**mp,mr,rmet,nr)
         end do
         yield=ejectaZtot/norm/(1.-ejectatot/norm)                  

         fileSSP=prefix(1:index(prefix,' ')-1)//'_'//
     $        filetracks(itracks)
         open(40,file=fileSSP,status='new')
         write(lun,'(a)') fileSSP(1:index(fileSSP,' '))
         write(40,'(a,a)') 'Evolutionary tracks: ',filetracks(itracks)
         write(40,'(a,a)')    'grid type                  : ',grid_type
         write(40,'(a,f5.4)') 'Metallicity (mass fraction): ',metal
         if (answerwinds.eq.'y') then
            write(40,'(a,a,a1,a)') 'Ejecta of massive stars: ',
     $           'SNII (model ',answerejecta,
     $           ' of Woosley & Weaver [1995]) + stellar winds'
         else
            write(40,'(a,a,a1,a)') 'Ejecta of massive stars: ',
     $           'SNII (model ',answerejecta,
     $           ' of Woosley & Weaver [1995]) only'
         end if
         i=index(fileIMF(iIMF),' ')
         write(40,'(a,a,a,f8.4,a,f8.4,a)') 'Initial mass function: ',
     $        fileIMF(iIMF)(1:i-1),' from ',massmin,' to ',massmax,
     $        ' solar masses'
         write(40,'(6(i5,1x),e10.4)') 516,nspectot,nprec(iz)+1,
     $        nprec(iz+1)+nspec(iz+1),iz, iz+1, yield
         
*     Computation of the isochrones.
*     time in Myr
         mend=log10(mass(nMS))
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

*     k: number of points on the isochrone.
*     m: log10(mass) of the point on the isochrone.
*     Tisoch: effective temperature""" (log10(.[Kelvin])).
*     Lisoch: bolometric luminosity""" (log10(./Lsol))
*     gisoch: surface gravity """.
            
            k=0
            if (time.gt.0) then
               
*     Time strictly > 0.

*     Very low mass stars.

               do i=1,nVLM
                  k=k+1
                  m(k)=log10(mass(i))
                  j=1
                  do while ((age(i,j)-time)*(age(i,j+1)-time).gt.0.)
                     j=j+1
                  end do
                  alpha=(time-age(i,j))/(age(i,j+1)-age(i,j))
                  Tisoch(k)=Teff(i,j)+alpha*(Teff(i,j+1)-Teff(i,j))
                  Lisoch(k)=Lum(i,j)+alpha*(Lum(i,j+1)-Lum(i,j))      
                  gisoch(k)=grav(i,j)+alpha*(grav(i,j+1)-grav(i,j))
               end do
               do i=nVLM+1,nVLM+nHeF-1
                  if ((nsteps(i).eq.nsteps(i+1)).and.
     $                 (mass(i).lt.mass(i+1)-1.e-6)) then
                     if (((age(i,1)-time)*(age(i+1,nsteps(i+1))-time)
     $                    .le.0.).or.((age(i,nsteps(i))-time)
     $                    *(age(i+1,1)-time).le.0.)) then
                        j1=1
                        j2=1
                        if (age(i,1).gt.time) j1=0
                        if (age(i,nsteps(i)).lt.time) j1=nsteps(i)
                        if (age(i+1,1).gt.time) j2=0
                        if (age(i+1,nsteps(i+1)).lt.time) j2=nsteps(i+1)
                        if (j1*(j1-nsteps(i)).ne.0) then
                           do while ((age(i,j1)-time)
     $                          *(age(i,j1+1)-time).gt.0.)
                              j1=j1+1
                           end do
                        end if
                        if (j2*(j2-nsteps(i+1)).ne.0) then
                           do while ((age(i+1,j2)-time)
     $                          *(age(i+1,j2+1)-time).gt.0.)
                              j2=j2+1
                           end do
                        end if
                        if (j1*(j1-nsteps(i)).ne.0) then
                           k=k+1
                           m(k)=log10(mass(i))
                           alpha=(time-age(i,j1))/
     $                          (age(i,j1+1)-age(i,j1))
                           Tisoch(k)=Teff(i,j1)
     $                          +alpha*(Teff(i,j1+1)-Teff(i,j1))
                           Lisoch(k)=Lum(i,j1)
     $                          +alpha*(Lum(i,j1+1)-Lum(i,j1))
                           gisoch(k)=grav(i,j1)
     $                          +alpha*(grav(i,j1+1)-grav(i,j1))
                        end if
                        if (j1.gt.j2) then
                           do j=j1,j2+1,-1
                              alpha=log10(time/age(i+1,j))/
     $                             log10(age(i,j)/age(i+1,j))
                              k=k+1
                              m(k)=alpha*log10(mass(i))
     $                             +(1.-alpha)*log10(mass(i+1))
                              Tisoch(k)=alpha*Teff(i,j)
     $                             +(1.-alpha)*Teff(i+1,j)
                              Lisoch(k)=alpha*Lum(i,j)
     $                             +(1.-alpha)*Lum(i+1,j)
                              gisoch(k)=alpha*grav(i,j)
     $                             +(1.-alpha)*grav(i+1,j)
                           end do    
                        else
                           do j=j1+1,j2
                              alpha=log10(time/age(i+1,j))/
     $                             log10(age(i,j)/age(i+1,j))
                              k=k+1
                              m(k)=alpha*log10(mass(i))
     $                             +(1.-alpha)*log10(mass(i+1))
                              Tisoch(k)=alpha*Teff(i,j)
     $                             +(1.-alpha)*Teff(i+1,j)
                              Lisoch(k)=alpha*Lum(i,j)
     $                             +(1.-alpha)*Lum(i+1,j)
                              gisoch(k)=alpha*grav(i,j)
     $                             +(1.-alpha)*grav(i+1,j)
                           end do    
                        end if
                        if (j2*(j2-nsteps(i+1)).ne.0) then
                           k=k+1
                           m(k)=log10(mass(i+1))
                           alpha=(time-age(i+1,j2))/
     $                          (age(i+1,j2+1)-age(i+1,j2))
                           Tisoch(k)=Teff(i+1,j2)
     $                          +alpha*(Teff(i+1,j2+1)-Teff(i+1,j2))
                           Lisoch(k)=Lum(i+1,j2)
     $                          +alpha*(Lum(i+1,j2+1)-Lum(i+1,j2))
                           gisoch(k)=grav(i+1,j2)
     $                          +alpha*(grav(i+1,j2+1)-grav(i+1,j2))
                        end if
                     end if
                  end if
               end do
               i=nVLM+nHeF
               do j=1,nsteps(i)-1
                  if ((age(i,j)-time)*(age(i,j+1)-time).le.0.) then
                     k=k+1
                     m(k)=log10(mass(i))
                     alpha=(time-age(i,j))/(age(i,j+1)-age(i,j))
                     Tisoch(k)=Teff(i,j)+alpha*(Teff(i,j+1)-Teff(i,j))
                     Lisoch(k)=Lum(i,j)+alpha*(Lum(i,j+1)-Lum(i,j))
                     gisoch(k)=grav(i,j)+alpha*(grav(i,j+1)-grav(i,j))
                  end if
               end do
               do i=nMS+1,nMS+nHB-1
                  if ((nsteps(i).eq.nsteps(i+1)).and.
     $                 (mass(i).lt.mass(i+1)-1.e-6)) then
                     if (((age(i,1)-time)*(age(i+1,nsteps(i+1))-time)
     $                    .le.0.).or.((age(i,nsteps(i))-time)
     $                    *(age(i+1,1)-time).le.0.)) then
                        j1=1
                        j2=1
                        if (age(i,1).gt.time) j1=0
                        if (age(i,nsteps(i)).lt.time) j1=nsteps(i)
                        if (age(i+1,1).gt.time) j2=0
                        if (age(i+1,nsteps(i+1)).lt.time) j2=nsteps(i+1)
                        if (j1*(j1-nsteps(i)).ne.0) then
                           do while ((age(i,j1)-time)
     $                          *(age(i,j1+1)-time).gt.0.)
                              j1=j1+1
                           end do
                        end if
                        if (j2*(j2-nsteps(i+1)).ne.0) then
                           do while ((age(i+1,j2)-time)
     $                          *(age(i+1,j2+1)-time).gt.0.)
                              j2=j2+1
                           end do
                        end if
                        if (j1*(j1-nsteps(i)).ne.0) then
                           k=k+1
                           m(k)=log10(mass(i))
                           alpha=(time-age(i,j1))/
     $                          (age(i,j1+1)-age(i,j1))
                           Tisoch(k)=Teff(i,j1)
     $                          +alpha*(Teff(i,j1+1)-Teff(i,j1))
                           Lisoch(k)=Lum(i,j1)
     $                          +alpha*(Lum(i,j1+1)-Lum(i,j1))
                           gisoch(k)=grav(i,j1)
     $                          +alpha*(grav(i,j1+1)-grav(i,j1))
                        end if
                        if (j1.gt.j2) then
                           do j=j1,j2+1,-1
                              alpha=log10(time/age(i+1,j))/
     $                             log10(age(i,j)/age(i+1,j))
                              k=k+1
                              m(k)=alpha*log10(mass(i))
     $                             +(1.-alpha)*log10(mass(i+1))
                              Tisoch(k)=alpha*Teff(i,j)
     $                             +(1.-alpha)*Teff(i+1,j)
                              Lisoch(k)=alpha*Lum(i,j)
     $                             +(1.-alpha)*Lum(i+1,j)
                              gisoch(k)=alpha*grav(i,j)
     $                             +(1.-alpha)*grav(i+1,j)
                           end do    
                        else
                           do j=j1+1,j2
                              alpha=log10(time/age(i+1,j))/
     $                             log10(age(i,j)/age(i+1,j))
                              k=k+1
                              m(k)=alpha*log10(mass(i))
     $                             +(1.-alpha)*log10(mass(i+1))
                              Tisoch(k)=alpha*Teff(i,j)
     $                             +(1.-alpha)*Teff(i+1,j)
                              Lisoch(k)=alpha*Lum(i,j)
     $                             +(1.-alpha)*Lum(i+1,j)
                              gisoch(k)=alpha*grav(i,j)
     $                             +(1.-alpha)*grav(i+1,j)
                           end do    
                        end if
                        if (j2*(j2-nsteps(i+1)).ne.0) then
                           k=k+1
                           m(k)=log10(mass(i+1))
                           alpha=(time-age(i+1,j2))/
     $                          (age(i+1,j2+1)-age(i+1,j2))
                           Tisoch(k)=Teff(i+1,j2)
     $                          +alpha*(Teff(i+1,j2+1)-Teff(i+1,j2))
                           Lisoch(k)=Lum(i+1,j2)
     $                          +alpha*(Lum(i+1,j2+1)-Lum(i+1,j2))
                           gisoch(k)=grav(i+1,j2)
     $                          +alpha*(grav(i+1,j2+1)-grav(i+1,j2))
                        end if
                     end if
                  end if
               end do
               i=nMS+nHB
               do j=1,nsteps(i)-1
                  if ((age(i,j)-time)*(age(i,j+1)-time).le.0.) then
                     k=k+1
                     m(k)=log10(mass(i))
                     alpha=(time-age(i,j))/(age(i,j+1)-age(i,j))
                     Tisoch(k)=Teff(i,j)+alpha*(Teff(i,j+1)-Teff(i,j))
                     Lisoch(k)=Lum(i,j)+alpha*(Lum(i,j+1)-Lum(i,j))
                     gisoch(k)=grav(i,j)+alpha*(grav(i,j+1)-grav(i,j))
                  end if
               end do
               do i=nVLM+nHeF+1,nMS-1
                  if ((nsteps(i).eq.nsteps(i+1)).and.
     $                 (mass(i).lt.mass(i+1)-1.e-6)) then
                     if (((age(i,1)-time)*(age(i+1,nsteps(i+1))-time)
     $                    .le.0.).or.((age(i,nsteps(i))-time)
     $                    *(age(i+1,1)-time).le.0.)) then
                        j1=1
                        j2=1
                        if (age(i,1).gt.time) j1=0
                        if (age(i,nsteps(i)).lt.time) j1=nsteps(i)
                        if (age(i+1,1).gt.time) j2=0
                        if (age(i+1,nsteps(i+1)).lt.time) j2=nsteps(i+1)
                        if (j1*(j1-nsteps(i)).ne.0) then
                           do while ((age(i,j1)-time)
     $                          *(age(i,j1+1)-time).gt.0.)
                              j1=j1+1
                           end do
                        end if
                        if (j2*(j2-nsteps(i+1)).ne.0) then
                           do while ((age(i+1,j2)-time)
     $                          *(age(i+1,j2+1)-time).gt.0.)
                              j2=j2+1
                           end do
                        end if
                        if (j1*(j1-nsteps(i)).ne.0) then
                           k=k+1
                           m(k)=log10(mass(i))
                           alpha=(time-age(i,j1))/
     $                          (age(i,j1+1)-age(i,j1))
                           Tisoch(k)=Teff(i,j1)
     $                          +alpha*(Teff(i,j1+1)-Teff(i,j1))
                           Lisoch(k)=Lum(i,j1)
     $                          +alpha*(Lum(i,j1+1)-Lum(i,j1))
                           gisoch(k)=grav(i,j1)
     $                          +alpha*(grav(i,j1+1)-grav(i,j1))
                        end if
                        if (j1.gt.j2) then
                           do j=j1,j2+1,-1
                              alpha=log10(time/age(i+1,j))/
     $                             log10(age(i,j)/age(i+1,j))
                              k=k+1
                              m(k)=alpha*log10(mass(i))
     $                             +(1.-alpha)*log10(mass(i+1))
                              Tisoch(k)=alpha*Teff(i,j)
     $                             +(1.-alpha)*Teff(i+1,j)
                              Lisoch(k)=alpha*Lum(i,j)
     $                             +(1.-alpha)*Lum(i+1,j)
                              gisoch(k)=alpha*grav(i,j)
     $                             +(1.-alpha)*grav(i+1,j)
                           end do    
                        else
                           do j=j1+1,j2
                              alpha=log10(time/age(i+1,j))/
     $                             log10(age(i,j)/age(i+1,j))
                              k=k+1
                              m(k)=alpha*log10(mass(i))
     $                             +(1.-alpha)*log10(mass(i+1))
                              Tisoch(k)=alpha*Teff(i,j)
     $                             +(1.-alpha)*Teff(i+1,j)
                              Lisoch(k)=alpha*Lum(i,j)
     $                             +(1.-alpha)*Lum(i+1,j)
                              gisoch(k)=alpha*grav(i,j)
     $                             +(1.-alpha)*grav(i+1,j)
                           end do    
                        end if
                        if (j2*(j2-nsteps(i+1)).ne.0) then
                           k=k+1
                           m(k)=log10(mass(i+1))
                           alpha=(time-age(i+1,j2))/
     $                          (age(i+1,j2+1)-age(i+1,j2))
                           Tisoch(k)=Teff(i+1,j2)
     $                          +alpha*(Teff(i+1,j2+1)-Teff(i+1,j2))
                           Lisoch(k)=Lum(i+1,j2)
     $                          +alpha*(Lum(i+1,j2+1)-Lum(i+1,j2))
                           gisoch(k)=grav(i+1,j2)
     $                          +alpha*(grav(i+1,j2+1)-grav(i+1,j2))
                        end if
                     end if
                  end if
               end do
               i=nMS
               do j=1,nsteps(i)-1
                  if ((age(i,j)-time)*(age(i,j+1)-time).le.0.) then
                     k=k+1
                     m(k)=log10(mass(i))
                     alpha=(time-age(i,j))/(age(i,j+1)-age(i,j))
                     Tisoch(k)=Teff(i,j)+alpha*(Teff(i,j+1)-Teff(i,j))
                     Lisoch(k)=Lum(i,j)+alpha*(Lum(i,j+1)-Lum(i,j))
                     gisoch(k)=grav(i,j)+alpha*(grav(i,j+1)-grav(i,j))
                  end if
               end do
            else

*     Time = 0.

               do i=1,nMS
                  k=k+1
                  m(k)=log10(mass(i))
                  Tisoch(k)=Teff(i,1)
                  Lisoch(k)=Lum(i,1)
                  gisoch(k)=grav(i,1)
               end do
            end if
            
            kend=k
            if (10.**m(kend).gt.massWD(1)) then
               do i=1,iWD-1
                  if (((ageWD(i,1)-time)*(ageWD(i+1,21)-time)
     $                 .le.0.).or.((ageWD(i,21)-time)
     $                 *(ageWD(i+1,1)-time).le.0.)) then
                     j1=1
                     j2=1
                     if (ageWD(i,1).gt.time) j1=0
                     if (ageWD(i,21).lt.time) j1=21
                     if (ageWD(i+1,1).gt.time) j2=0
                     if (ageWD(i+1,21).lt.time) j2=21
                     if (j1*(j1-21).ne.0) then
                        do while ((ageWD(i,j1)-time)
     $                       *(ageWD(i,j1+1)-time).gt.0.)
                           j1=j1+1
                        end do
                     end if
                     if (j2*(j2-21).ne.0) then
                        do while ((ageWD(i+1,j2)-time)
     $                       *(ageWD(i+1,j2+1)-time).gt.0.)
                           j2=j2+1
                        end do
                     end if
                     if (j1*(j1-21).ne.0) then
                        k=k+1
                        m(k)=log10(massWD(i))
                        alpha=(time-ageWD(i,j1))/
     $                       (ageWD(i,j1+1)-ageWD(i,j1))
                        Tisoch(k)=TeffWD(i,j1)
     $                       +alpha*(TeffWD(i,j1+1)-TeffWD(i,j1))
                        Lisoch(k)=LumWD(i,j1)
     $                       +alpha*(LumWD(i,j1+1)-LumWD(i,j1))
                        gisoch(k)=gravWD(i,j1)
     $                       +alpha*(gravWD(i,j1+1)-gravWD(i,j1))
                     end if
                     if (j1.gt.j2) then
                        do j=j1,j2+1,-1
                           alpha=log10(time/ageWD(i+1,j))/
     $                          log10(ageWD(i,j)/ageWD(i+1,j))
                           k=k+1
                           m(k)=alpha*log10(massWD(i))
     $                          +(1.-alpha)*log10(massWD(i+1))
                           Tisoch(k)=alpha*TeffWD(i,j)
     $                          +(1.-alpha)*TeffWD(i+1,j)
                           Lisoch(k)=alpha*LumWD(i,j)
     $                          +(1.-alpha)*LumWD(i+1,j)
                           gisoch(k)=alpha*gravWD(i,j)
     $                          +(1.-alpha)*gravWD(i+1,j)
                        end do    
                     else
                        do j=j1+1,j2
                           alpha=log10(time/ageWD(i+1,j))/
     $                          log10(ageWD(i,j)/ageWD(i+1,j))
                           k=k+1
                           m(k)=alpha*log10(massWD(i))
     $                          +(1.-alpha)*log10(massWD(i+1))
                           Tisoch(k)=alpha*TeffWD(i,j)
     $                          +(1.-alpha)*TeffWD(i+1,j)
                           Lisoch(k)=alpha*LumWD(i,j)
     $                          +(1.-alpha)*LumWD(i+1,j)
                           gisoch(k)=alpha*gravWD(i,j)
     $                          +(1.-alpha)*gravWD(i+1,j)
                        end do    
                     end if
                     if (j2*(j2-21).ne.0) then
                        k=k+1
                        m(k)=log10(massWD(i+1))
                        alpha=(time-ageWD(i+1,j2))/
     $                       (ageWD(i+1,j2+1)-ageWD(i+1,j2))
                        Tisoch(k)=TeffWD(i+1,j2)
     $                       +alpha*(TeffWD(i+1,j2+1)-TeffWD(i+1,j2))
                        Lisoch(k)=LumWD(i+1,j2)
     $                       +alpha*(LumWD(i+1,j2+1)-LumWD(i+1,j2))
                        gisoch(k)=gravWD(i+1,j2)
     $                       +alpha*(gravWD(i+1,j2+1)-gravWD(i+1,j2))
                     end if
                  end if
               end do
            end if
            
            do j=-nCM,nspectot
               fluxSSP(j)=0.
            end do
*     Bolometric luminosity.
            Lbol=0.                
*     Number of photons ionizing H.
            NHItot=0.              
*     Number of photons ionizing He once.
            NHeItot=0.            
*     Number of photons ionizing He twice.
            NHeIItot=0.            
            
            do i=1,k-1
               
*     Division of the isochrone in smaller bins.

               nbins=int((m(i+1)-m(i))/0.01+1)
     $              +int(abs(Tisoch(i+1)-Tisoch(i))/0.01+1)
     $              +int(abs(Lisoch(i+1)-Lisoch(i))/0.01+1)
               dm=(m(i+1)-m(i))/nbins
               do p=0,nbins-1
                  mp=m(i)+(p+0.5)*dm
                  dn=nstars(mp,dm,fileIMF(iIMF),massinf,nIMFbins,
     $                 coeffcont,slope,norm,massmin,massmax)
                  Tp=Tisoch(i)+(p+0.5)*(Tisoch(i+1)-Tisoch(i))/nbins
                  Lp=Lisoch(i)+(p+0.5)*(Lisoch(i+1)-Lisoch(i))/nbins
                  gp=gisoch(i)+(p+0.5)*(gisoch(i+1)-gisoch(i))/nbins
                  Lbol=Lbol+dn*10.**Lp
*     bin number p: centered on mp (log10(mass)) and of width dm
*     (log10(mass)); contains dn stars.


*     Computation of the fluxes emitted in the stellar spectra 
*     with metallicity BELOW that of the tracks (idem for the number
*     of photons).

                  if (grid_type(:str_length(grid_type)).eq.'standard') 
     $                 then
*     For the LCB library, we have plenty of spectra at every metallicity, 
*     so we just stick to the metallicities *immediately* below and above the 
*     current metallicity.
                     izseekinf=iz
                     izseeksup=iz
                  else
*     For the other libraries, we have fewer spectra, and possibly holes in the (T,g) diagram
*     around the current metallicity. So, we allow stars with more different 
*     metallicities (but with always delta_logT<0.1 and delta_logg<0.5)
*     to contribute to the final spectrum:
                     izseekinf=1
                     izseeksup=iz
                  endif
                  
                  call interpstellib(Tp,gp,metal,Tspec,gspec,z,nz,nspec,
     $                 nCM,j1,j2,j3,j4,
     $                 alpha1,alpha2,alpha3,alpha4,logZinf,
     $                 izseekinf,izseeksup,
     $                 iz1,iz2,iz3,iz4)

                  
*     Computation of the fluxes emitted in the stellar spectra 
*     with metallicity ABOVE that of the tracks (idem for the number
*     of photons).

                  if (grid_type(:str_length(grid_type)).eq.'standard') 
     $                 then
*     For the LCB library, we have plenty of spectra at every metallicity, 
*     so we just stick to the metallicities *immediately* below and above the 
*     current metallicity.
                     izseekinf=iz+1
                     izseeksup=iz+1
                  else
*     For the other libraries, we have fewer spectra, and possibly holes in the (T,g) diagram
*     around the current metallicity. So, we allow stars with more different 
*     metallicities (but with always delta_logT<0.1 and delta_logg<0.5)
*     to contribute to the final spectrum:
                     izseekinf=iz+1
                     izseeksup=nz
                  endif

                  call interpstellib(Tp,gp,metal,Tspec,gspec,z,nz,nspec,
     $                 nCM,j5,j6,j7,j8,
     $                 alpha5,alpha6,alpha7,alpha8,logZsup,
     $                 izseekinf,izseeksup,
     $                 iz5,iz6,iz7,iz8)


                  fz=(logZsup-log10(metal))/(logZsup-logZinf)

                  if (alpha1+alpha2+alpha3+alpha4.eq.0.) fz=0.d0
                  if (alpha5+alpha6+alpha7+alpha8.eq.0.) fz=1.d0


                  alpha1=alpha1*fz
                  alpha2=alpha2*fz
                  alpha3=alpha3*fz
                  alpha4=alpha4*fz
                  NHItot=NHItot+10.**Lp*dn*(alpha1*NHI(iz1,j1)+
     $                 alpha2*NHI(iz2,j2)+alpha3*NHI(iz3,j3)
     $                 +alpha4*NHI(iz4,j4))
                  NHeItot=NHeItot+10.**Lp*dn*(alpha1*NHeI(iz1,j1)+
     $                 alpha2*NHeI(iz2,j2)+alpha3*NHeI(iz3,j3)
     $                 +alpha4*NHeI(iz4,j4))
                  NHeIItot=NHeIItot+10.**Lp*dn*(alpha1*NHeII(iz1,j1)+
     $                 alpha2*NHeII(iz2,j2)+alpha3*NHeII(iz3,j3)
     $                 +alpha4*NHeII(iz4,j4))
                  if (j1.gt.0) j1=j1+nprec(iz1)
                  if (j2.gt.0) j2=j2+nprec(iz2)
                  if (j3.gt.0) j3=j3+nprec(iz3)
                  if (j4.gt.0) j4=j4+nprec(iz4)
                  fluxSSP(j1)=fluxSSP(j1)+10.**Lp*alpha1*dn
                  fluxSSP(j2)=fluxSSP(j2)+10.**Lp*alpha2*dn
                  fluxSSP(j3)=fluxSSP(j3)+10.**Lp*alpha3*dn
                  fluxSSP(j4)=fluxSSP(j4)+10.**Lp*alpha4*dn


                  alpha5=alpha5*(1.d0-fz)
                  alpha6=alpha6*(1.d0-fz)
                  alpha7=alpha7*(1.d0-fz)
                  alpha8=alpha8*(1.d0-fz)
                  NHItot=NHItot+10.**Lp*dn*(alpha5*NHI(iz5,j5)+
     $                 alpha6*NHI(iz6,j6)+alpha7*NHI(iz7,j7)
     $                 +alpha8*NHI(iz8,j8))
                  NHeItot=NHeItot+10.**Lp*dn*(alpha5*NHeI(iz5,j5)+
     $                 alpha6*NHeI(iz6,j6)+alpha7*NHeI(iz7,j7)
     $                 +alpha8*NHeI(iz8,j8))
                  NHeIItot=NHeIItot+10.**Lp*dn*(alpha5*NHeII(iz5,j5)+
     $                 alpha6*NHeII(iz6,j6)+alpha7*NHeII(iz7,j7)
     $                 +alpha8*NHeII(iz8,j8))
                  if (j5.gt.0) j5=j5+nprec(iz5)
                  if (j6.gt.0) j6=j6+nprec(iz6)
                  if (j7.gt.0) j7=j7+nprec(iz7)
                  if (j8.gt.0) j8=j8+nprec(iz8)
                  fluxSSP(j5)=fluxSSP(j5)+10.**Lp*alpha5*dn
                  fluxSSP(j6)=fluxSSP(j6)+10.**Lp*alpha6*dn
                  fluxSSP(j7)=fluxSSP(j7)+10.**Lp*alpha7*dn
                  fluxSSP(j8)=fluxSSP(j8)+10.**Lp*alpha8*dn

               end do
            end do


*     Only the fluxes in the spectra really used will be written.

            nused=0
            do j=-nCM,nspectot
               if (fluxSSP(j).gt.0.) then
                  nused=nused+1
                  iused(nused)=j
               end if
            end do

*     Computation of the mass of gas and metals ejected since the
*     previous timestep. The apportionment between "isolated" stars [IS]
*     and stars in close binary systems [CB] is made in spectra.f ("fSNIa").
            
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
            nbins=int((mend-m(kend))/0.01+1)
            dm=(mend-m(kend))/nbins
            do p=0,nbins-1
               mp=m(kend)+(p+0.5)*dm

*     "Isolated" star (single star or member of a loose binary system).

               dn=nstars(mp,dm,fileIMF(iIMF),massinf,nIMFbins,
     $              coeffcont,slope,norm,massmin,massmax)
               ejecta_IS=ejecta_IS+dn*interpol(10.**mp,mr,r,nr)
               ejectaZ_IS=ejectaZ_IS+dn*interpol(10.**mp,mr,rmet,nr)
               if (10.**mp.ge.mSNII) then 
*     SNII.
                  nSNII_IS=nSNII_IS+dn
                  massBHNS_IS=massBHNS_IS+dn*(10.**mp 
*     Mass locked in black holes or neutron stars.
     $                 -interpol(10.**mp,mr,r,nr))
               else
                  massWD_IS=massWD_IS+dn*(10.**mp 
*     Mass locked in white dwarfs.
     $                 -interpol(10.**mp,mr,r,nr))
               end if

*     Close binary system.
*     massmin<=m2<=m1<=massmax.
*     mu=m2/(m1+m2).
*     f(mu)=Cbin*mu**gammabin: mass-ratio function.
*     integral_{muinf}^{musup}f(mu).dmu=1.
*     mbin=m1+m2 is assumed to be distributed according the IMF
*     for "isolated" stars.

*     Primary star of a close binary system.     

               m1=10.**mp
               muinf=massmin/(massmin+m1)
               m2max=min(m1,massmax-m1)
               musup=m2max/(m2max+m1)
*     (mu is an increasing function of m2)
               if (musup.gt.muinf) then
                  Cbin=(gammabin+1.)/
     $                 (musup**(gammabin+1.)-muinf**(gammabin+1.))
                  nmu=int((musup-muinf)/0.001+1)
                  dmu=(musup-muinf)/nmu
                  do q=0,nmu-1
                     mu=muinf+(q+0.5)*dmu
                     mbin=m1/(1.-mu)
                     dn=nstars(log10(mbin),dm,fileIMF(iIMF),massinf,
     $                    nIMFbins,coeffcont,slope,norm,
     $                    massmin,massmax)
                     dn12=dn*Cbin*mu**gammabin*dmu
                     ejecta_CB=ejecta_CB+dn12*interpol(m1,mr,r,nr)
                     ejectaZ_CB=ejectaZ_CB+dn12*interpol(m1,mr,rmet,nr)
                     if (m1.ge.mSNII) then
                        nSNII_CB=nSNII_CB+dn12
                        massBHNS_CB=massBHNS_CB+dn12*(m1
     $                       -interpol(m1,mr,r,nr))
                     else
                        massWD_CB=massWD_CB+dn12*(m1 
     $                       -interpol(m1,mr,r,nr))
                     end if
                  end do
               end if

*     Secondary star of a close binary system.

               m2=10.**mp
               muinf=m2/massmax
               musup=0.5
               if (musup.gt.muinf) then
                  Cbin=(gammabin+1.)/
     $                 (musup**(gammabin+1.)-muinf**(gammabin+1.))
                  nmu=int((musup-muinf)/0.001+1)
                  dmu=(musup-muinf)/nmu
                  do q=0,nmu-1
                     mu=muinf+(q+0.5)*dmu
                     m1=m2*(1.-mu)/mu
                     mbin=m1+m2
                     dn=nstars(log10(mbin),dm,fileIMF(iIMF),massinf,
     $                    nIMFbins,coeffcont,slope,norm,
     $                    massmin,massmax)
                     dn12=dn*Cbin*mu**2*dmu
                     if (mbin.lt.mSNIa.or.m1.gt.mSNII) then 
*     Mass too small or the remnant of the primary is not a white dwarf. 
                        ejecta_CB=ejecta_CB+dn12*interpol(m2,mr,r,nr)
                        ejectaZ_CB=ejectaZ_CB
     $                       +dn12*interpol(m2,mr,rmet,nr)
                        if (m2.ge.mSNII) then
                           nSNII_CB=nSNII_CB+dn12
                           massBHNS_CB=massBHNS_CB+dn12*(m2
     $                          -interpol(m2,mr,r,nr))
                        else
                           massWD_CB=massWD_CB+dn12*(m2 
     $                          -interpol(m2,mr,r,nr))
                        end if
                     else          
*     SNIa.
                        nSNIa=nSNIa+dn12
                        mWD1=m1-interpol(m1,mr,r,nr) 
*     Mass of the white dwarf remnant of the primary.
                        massWD_CB=massWD_CB-dn12*mWD1
*     Destroyed.
                        deltam=mCh-mWD1 
*     Mass ejected by the secondary used by the primary to reach 
*     the Chandrasekhar mass.
                        r2=interpol(m2,mr,r,nr)
                        ejecta_CB=ejecta_CB+dn12*
     $                       (r2-deltam 
*     Ejecta of the secondary not processed by the primary.
     $                       +mCh) 
*     Ejecta of the primary (totally destroyed).
                        ejectaZ_CB=ejectaZ_CB+dn12*
     $                       (interpol(m2,mr,rmet,nr)*(r2-deltam)/r2 
*     Secundary.
     $                       +mCh) 
*     Primary (no H or He remains).
                     end if
                  end do
               end if
            end do    
            
            mend=m(kend)
            dtime=max(time-timeprev+0.d0,1.d0)
            
            write(40,'(i5,2x,i4,1x,5(1x,e11.5))') time,
     $           nused,10.**mend,Lbol,NHItot,NHeItot,NHeIItot
            write(40,'(7(1x,e11.5))') nSNII_IS/dtime,nSNII_CB/dtime,
     $           nSNIa/dtime,massBHNS_IS/dtime,massBHNS_CB/dtime,
     $           massWD_IS/dtime,massWD_CB/dtime
            write(40,'(4(1x,e11.5))') 
     $           ejecta_IS/dtime,ejecta_CB/dtime,ejectaZ_IS/dtime,
     $           ejectaZ_CB/dtime
            write(40,'(5(i4,1x,e11.6,1x))') (iused(k),
     $           fluxSSP(iused(k)),k=1,nused)
            timeprev=time

         end do        
         close(40)
      enddo
      close(lun)
      
      end

*********************************

