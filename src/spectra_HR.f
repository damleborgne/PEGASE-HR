      program spectra_HR

      implicit none
      include 'peg_include.f'
      include 'peg_config.f'

      integer i912, nspecLCB
      character*280 fileSSPZ(nmaxZtracks),filespectra,a
      character*280 fileSSPs,filescenarios
      character*280 filestellib,filestellib_short
      character*100 header(4,nmaxZtracks)
      character*280 fileSFR
      character*72 answerinfall
      character*72 answerneb,answerZ,rien
      character*30 grid_type
      integer p,infall,i0
      integer i,j,k,ntimes,nlines,nZstellib
      integer firstspecLCB(nmaxZl+1) ! Index of 1st spec in each Z bin
      integer iZstellib1,iZstellib2
      integer nlambdaCM, idum
      integer nspec,nlambda
      integer ntimesimpr,jimpr
      integer timeimpr(nmaxotimes)
      integer invtime(nmaxtimes),q
      integer nused(nmaxtimesSSP,nmaxZtracks)
      integer iused(nmaxspec,nmaxtimesSSP,nmaxZtracks),nZ
      integer itotused(nmaxspec), ntotused 
      integer iZinf(nmaxtimes),iZsup(nmaxtimes)
      integer nparam
      real time(nmaxtimes),twind,trwind,rdum
      real ejecta(nmaxZtracks,nmaxtimes),ZSSP(nmaxZtracks)
      real zLCB(nmaxZl)
      real ejectaZ(nmaxZtracks,nmaxtimes)
      real Lspec_SSP(-nmaxCM:nmaxspecLCB,nmaxtimesSSP,nmaxZtracks)
      real Lspec_gal(-nmaxCM:nmaxspecLCB,nmaxotimes)
      real inclin,fluxbolZ,Zbol
      real SFR(nmaxtimes),SFRlum(nmaxtimes)
      real sigmaBHNS(nmaxtimes),agestars(nmaxtimes)
      real massBHNS(nmaxZtracks,nmaxtimes)
      real sigmaWD(nmaxtimes),agebol,SFRmax,fluxbolage
      real massWD(nmaxZtracks,nmaxtimes),fsub
c     real fluxstel(nmaxlambda,nmaxspec)
      real flux_gal(nmaxlambda, nmaxotimes)
      real fluxgal(nmaxlambda)
      real sigmastars(nmaxtimes)
      real lambda(nmaxlambda),lambdaCM(nmaxlambdaCM)
      double precision NLymtot,NLym(nmaxZtracks,nmaxtimes)
      real lambdaline(nmaxlines)
      real fline(nmaxlines),fneb(nmaxlambda)
      real sigmagas(nmaxtimes),sigmasub(nmaxtimes)
      real Zgas(nmaxtimes),ZSFR(nmaxtimes), ZSFRlim(2)
      real flinetot(nmaxlines),f
      real fluxbol,nSNIItot,fluxext(nmaxotimes)
      real nSNII(nmaxZtracks,nmaxtimes)
      real sigmaZ
      real fSNIa,Zinfall
      real tinfall,nSNIa(nmaxZtracks,nmaxtimes),Zstars(nmaxtimes)
      real massalive(nmaxZtracks,nmaxtimes)
      real nSNIatot
      real Mgal(nmaxtimes)
      real beta(nmaxtimes),alpha(nmaxtimes)
      real fluxbolSSP(nmaxZtracks,nmaxtimesSSP)
      real SFRparam(2+nmaxparams) ! Type of SFR and parameters
      integer excessSFR         ! Time when SFR exceeded max, or 0

      real Zsol, c
      parameter(Zsol=0.02,c=2.99792458e18) ! c: 0.1nm/s

      integer nSpitzer
      real taudustSpitzer(20),ySpitzer(20),y,taudust
      real NH,tauV(nmaxotimes)

      integer codeext,codeZ
      integer iscenario 
      real contribneb

      integer istat             ! Return code
      integer Lfits             ! Unit number for accessing FITS file
      integer LuSpec            ! Unit Number for FITS output spectra

      integer LfitsCM 
      
      integer iZ1(nmaxZtracks), iZ2(nmaxZtracks)
      integer deca_spec
      integer str_length
      

***** Opening of the file containing the scenarios.

      
      filescenarios=' '

      if (iargc().gt.0) then 
         call getarg(1,filescenarios)
         filescenarios=filescenarios(:str_length(filescenarios))
      endif

      call scenario_open(filescenarios,istat)
      if(istat.eq.0) then
         write(*,*) ' Found file containing scenarios'
      else
         write(*,*) ' Did not find file containing scenarios'
         stop
      end if


***** Reading of the names of the SSPs files, fraction of binaries, name of
*     library.
      call scenario_header_r(fileSSPs,fSNIa,filestellib,istat)
      if(istat.eq.0) then
         write(*,*) ' Read filenames of input SSP files '
      else
         write(*,*) ' Read filenames of input SSP files... failed '
         stop
      end if
      filestellib_short=filestellib
      filestellib=PEG_ROOT//'data/stellibs/'//filestellib


***** Reading of the times for which the spectra are to be computed,
c     "ages.dat"
      call data_time_r(ntimesimpr,nmaxotimes,timeimpr,istat)
      if(istat.ne.0) then
         write(*,*) ' Read output times failed ...'
         stop
      else
         write(*,*) ' Read output times '
      end if

***** Reading of the header of the stellar library.

      call stell_open(filestellib,Lfits,grid_type,nlambda,nZstellib,
     $     nspecLCB,istat)

      if(istat.eq.0) then
         write(*,*) ' Finished reading needed library spectra '
         write(*,*) ' Total number of spectra loaded: ',nspecLCB
      else
         write(*,*)'Failed to open "stellib" file',
     s        filestellib(:str_length(filestellib)),istat
         stop
      endif

      iZstellib1=0
      iZstellib2=0
      call stell_nz_r(Lfits,firstspecLCB,zLCB,istat)
      write(*,*) ' Finished reading header of stellar library'

      call stell_wave_r(Lfits,nlambda,lambda,istat)

***** Read the SSP files: files, header, and finally data
***** 1) Read SSP files
      nZ=nmaxZtracks            ! initialize nZ to max possi number
      call ssp_files_read(fileSSPs,grid_type,nZ,fileSSPZ,ZSSP,istat)
      if (istat.ne.0) then
         write(*,*) ' Could not read SSP files ',nZ,fileSSPs
         stop
      else
         write(*,*) ' Read filenames of input SSP files '
      endif

***** 2) Read header of SSP files
      call ssp_head_read(nZ,fileSSPZ,header,istat)
      if(istat.ne.0) then
         write(*,*) ' Read headers of SSP files .. failed'
         stop
      else
         write(*,*) ' Read headers of SSP files '
      end if
      
***** 3) Reading of the flux emitted for an instantaneous burst in
***** the stellar spectra.
      call ssp_data_read(nZ,fileSSPZ,fSNIa, ! <- Input
     $     ntimes,              ! -> output
     $     massalive,
     $     nused,
     $     iused,
     $     fluxbolSSP,
     $     Lspec_SSP,
     $     invtime,
     $     beta,
     $     NLym,
     $     nSNII,
     $     nSNIa,
     $     ejecta,
     $     ejectaZ,
     $     massBHNS,
     $     massWD,
     $     iZ1,
     $     iZ2,
     $     istat
     $     )

***** Reading data for extinction
      call redden_r(istat)
      if(istat.eq.0) then
         write(*,*) ' Finished reading extinction ppties'
      else
         write(*,*) ' Reading extinction ppties... failed'
         stop
      end if

      
***** Loop on star formation scenarios.
      iscenario=0
      do while (istat.eq.0)
         
c     Read scenario from file opened with "scenario_open" earlier.
         iscenario=iscenario+1
         call scenario_sfr_r(iscenario,
     $        filespectra,Zgas,tinfall,Zinfall,infall,SFRparam,fileSFR,
     $        codeZ,ZSFR,fsub,trwind,
     $        answerneb,codeext,inclin,
     $        istat)
         if(istat.ne.0) then
            write(*,*) ' No more scenarios to compute.... Exiting.'
            exit                ! Exit loop on scenarii
         else
            write(*,*) ' -------------------------------------------'
            write(*,*) ' Finished reading scenario number ',iscenario
         end if
         if(trwind.lt.0) then   ! No galactic wind
            twind =nmaxtimes+1.
         else
            twind=trwind
         end if
         
c     Create the output spectra file 
         call fits_spec_creat(filespectra,nlambda,ntimesimpr,
     $        nZ,header,fSNIa,
     $        Zgas,tinfall,Zinfall,infall,SFRparam,fileSFR,
     $        codeZ,ZSFR,fsub,trwind,
     $        answerneb,codeext,inclin,filestellib_short,LuSpec,
     $        istat)       
! write(*,*) 'File unit number for spectra output file:',LuSpec
         if (istat.ne.0) then
            write(*,*) 'ERROR : Cannot create "spectra" file'
            write(*,*) 'ERROR',istat
            write(*,*) 'Please check that the file doesn''t exist'
     $           //' already :'
            write(*,*) filespectra(:str_length(filespectra))
            write(*,*) 'Exiting now...'
            stop
         endif

         call fits_spec_wave_w(LuSpec,nlambda,lambda,istat)

         if(istat.ne.0) then
            write(*,*)'Error writing wavelength to "spectra" file'
            write(*,*)'Exiting now!'
            stop
         end if
         

******Initialization.
         Mgal(1)=(1.-infall)
         sigmagas(1)=Mgal(1)       
         sigmaZ=sigmagas(1)*Zgas(1)
         sigmaBHNS(1)=0.
         sigmaWD(1)=0.
         excessSFR= 0           ! init time when SFR exceeded max to 0 (ie. did not)
         sigmasub(1)=0.
         ZSFRlim(1)=1.          ! AL- will be min and max useful ZSFR
         ZSFRlim(2)=0.

         do i=1,nmaxotimes
            do k=-nmaxCM,nmaxspecLCB
               Lspec_gal(k,i)=0.
            enddo
         enddo         
         

***** Reading of the SFR (and possibly Z also) in fileSFR, 
*     interpolate to the sampling times. 
         if (SFRparam(1).lt.0) then
            call sfr_time(SFRparam,fileSFR,SFR,ZSFR,istat)
            if (istat.ne.0) then
               write(*,*)'Could not read SFR file',istat
               stop
            end if
         end if
         
***** Convolution of the SSPs with the star formation rate.
         

         
!     print *,'begin convol SSP with SFR'
         do i=1, timeimpr(ntimesimpr)+1

            time(i)=i-1.
            
c     Find position of ZSFR(i) among available ZSSP, 
c     and determine interpolation weight alpha.
            call weightZ(ZSFR(i),ZSSP,nZ,alpha(i),iZinf(i),iZsup(i))
            

c     Compute ZSFR(i) (if not read in file previously).
            call increm_SFR(i, time(i), SFRparam,
     $           sigmagas(i), twind, SFR(i))

            
            
c     Test derived SFR(i) agains maximal SFR consistent with sigmagas(i)
            SFRmax=sigmagas(i)
            if (SFR(i).gt.SFRmax) then
               SFR(i)=SFRmax
               if (excessSFR.eq.0) excessSFR=i
            end if


c     - AL       Compute sigmastars(i), Zstars(i), agestars(i) by
c     convolution, update sigmaZ,  and compute sigmasub(i+1), Mgal(i+1), 
c     sigmaBHNS(i+1), sigmaWD(i+1), sigmagas(i+1), Zgas(i+1)
            call increm_ppties(i, SFR(i), time(i), SFRlum, iZinf, iZsup,
     $           alpha,fsub, infall, tinfall, Zinfall, twind, 
     $           ejecta, ejectaZ, massBHNS, massWD, massalive,
     $           ZSFR,sigmaZ, sigmasub, Mgal, sigmaBHNS, sigmaWD,
     $           sigmagas, Zgas,sigmastars(i), Zstars(i), agestars(i))
            

c     Compute ZSFR(i+1) if not read in input file previously.
            if (codeZ.eq.1) then
               ZSFR(i+1)=Zgas(i+1)
            elseif (codeZ.eq.0) then
               ZSFR(i+1)=ZSFR(1)
            end if
         end do


c     - PhP, Log "SFR_EXCESS" warning in the output spect.
         if(excessSFR.gt.0) then
            write(a,'(a,a,i5,a)') 
     $           'WARNING: the SFR has exceeded the maximal ',
     $           'possible SFR at ',excessSFR-1,' Myr!'
            call fits_spec_cmt_w(LuSpec,a,istat)
            if (istat.ne.0) then 
               write(*,*) 'Error when writing to Fits file !'
               stop
            endif
            write(*,'(a)') a
         endif
         

         iZstellib1 = nmaxZtracks
         iZstellib2 = 1
         do i = 1, timeimpr(ntimesimpr)
            if (iZinf(i) .lt. iZstellib1) iZstellib1 = iZinf(i)
            if (iZsup(i) .gt. iZstellib2) iZstellib2 = iZsup(i)
         enddo
         iZstellib1 = iZ1(iZstellib1)
         iZstellib2 = iZ2(iZstellib2)



c     - AL    Total number of spectra actually used
c     After this, ntotused contains the number of spectra actually used,
c     and itotused contains their index (on the INITIAL LCB scale).
!     write(*,*) ' Counting spectra actually used'

         call count_used(nused,iused, filestellib, ntimes, nZ,
     $        nZstellib, iZstellib1, iZstellib2, firstspecLCB, 
     $        ntotused, itotused)

         write(*,*) ' Spectra actually used: ',ntotused

***** AL- Various computations related to the continuum, moved here
c     because
*     the wavelengths must be known.

*     Position of the Lyman break: lambda(i912) < 912 A <= lambda(i912+1)

         i912=0
*     i912 was mistakenly set to 1 before version 1.3
         do while (lambda(i912+1).lt.912.)
            i912=i912+1
         end do
*     Calculations for the nebular continuum and nebular emission
         call data_neb_r(nlambda, lambda, fneb, nlines, lambdaline,
     $        fline, istat)
         if(istat.eq.0)
     $        call data_spitz_r(nSpitzer,taudustSpitzer,ySpitzer,istat)
         if(istat.ne.0) then
            write(*,*) 'Compute nebular continuum & emission ... failed'
            stop
         else
            write(*,*) ' Finished computing nebular continuum & '
     $           //'emission'
         end if
         
         do jimpr=1,ntimesimpr
            i=timeimpr(jimpr)+1
            call convol_SFR_SSPs(
     $           jimpr,
     $           i,   
     $           ntotused,  
     $           itotused,  
     $           SFRlum,     
     $           iZinf, iZsup, 
     $           invtime,     
     $           beta,         
     $           alpha,        
     $           Lspec_SSP,
     $           Lspec_gal)
         enddo         
         
         
         call comput_continuum(Lfits,  
     $        nspecLCB, nlambda, ntimesimpr,
     $        ntotused,itotused,Lspec_gal, flux_gal)

***** Loop on output times.
         i0=0
         do jimpr=1,ntimesimpr
            i=timeimpr(jimpr)+1
            do j = 1, nlambda
               fluxgal(j) = flux_gal(j, jimpr)
            enddo

c     Add nebular emission continuum if requested
            if (answerneb.eq.'Nebular emission') then
               if (codeext.ne.0) then
                  taudust=0.5*Zgas(i)/Zsol
                  call bracket(nSpitzer,taudustSpitzer,taudust,i0)
                  call Steffen(nSpitzer,taudustSpitzer,ySpitzer,.false.,
     $                 taudust,y,i0)
                  f=y**3
               else
                  f=1.
               end if
               
               contribneb=0.
               do k=max(i-50,1),i
                  if (SFRlum(k).gt.1.e-20) then
                     q=i+1-k
                     contribneb=contribneb+ SFRlum(k)*
     $                    (beta(q)*
     $                    (alpha(k)*NLym(iZinf(k),invtime(q))
     $                    +(1.-alpha(k))*NLym(iZsup(k),invtime(q)))
     $                    +(1.-beta(q))*
     $                    (alpha(k)*NLym(iZinf(k),invtime(q)+1)
     $                    +(1.-alpha(k))*NLym(iZsup(k),invtime(q)+1)))
                  end if
               end do
               contribneb=contribneb*f
               do k=1,nlambda
                  fluxgal(k)=fluxgal(k) + contribneb *fneb(k)
               end do
            else
               f=0.
            end if
            
*     The Lyman continuum photons used to produce the nebular emission
*     are removed from the spectrum.
            do j=1,i912
               fluxgal(j)=(1.-f)*fluxgal(j)
            end do

            call redden_cont(codeext, i912, lambda, nlambda, inclin,
     $           Zgas(i), sigmagas(i),
     $           fluxgal,
     $           fluxext(jimpr), tauV(jimpr))
            call fits_spec_cont_w(LuSpec,jimpr,nlambda,fluxgal,istat)
         enddo

         write(*,*) ' Creating the output '//
     $        'evolutive spectrum in FITS format'

***** Loop on output times.
         i0=0
         do jimpr=1,ntimesimpr
            i=timeimpr(jimpr)+1

            do k=1,nlines
               flinetot(k)=0.
            end do
c     Add nebular emission lines if requested
            if (answerneb.eq.'Nebular emission') then
               if (codeext.ne.0) then
                  taudust=0.5*Zgas(i)/Zsol
                  call bracket(nSpitzer,taudustSpitzer,taudust,i0)
                  call Steffen(nSpitzer,taudustSpitzer,ySpitzer,.false.,
     $                 taudust,y,i0)
                  f=y**3
               else
                  f=1.
               end if
               
               contribneb=0.
               do k=max(i-50,1),i
                  if (SFRlum(k).gt.1.e-20) then
                     q=i+1-k
                     contribneb=contribneb+ SFRlum(k)*
     $                    (beta(q)*
     $                    (alpha(k)*NLym(iZinf(k),invtime(q))
     $                    +(1.-alpha(k))*NLym(iZsup(k),invtime(q)))
     $                    +(1.-beta(q))*
     $                    (alpha(k)*NLym(iZinf(k),invtime(q)+1)
     $                    +(1.-alpha(k))*NLym(iZsup(k),invtime(q)+1)))
                  end if
               end do 
               contribneb=contribneb*f
               do k=1,nlines
                  flinetot(k)=flinetot(k) + contribneb *fline(k)
               end do
            else
               f=0.
            end if
            
*     Compute luminosity weighted quantities.
            fluxbol=0.         
            fluxbolZ=0.
            fluxbolage=0.
            do k=1,i
               q=i+1-k
               fluxbol=fluxbol+SFRlum(k)*(beta(q)
     $              *(alpha(k)*fluxbolSSP(iZinf(k),invtime(q))
     $              +(1.-alpha(k))*fluxbolSSP(iZsup(k),invtime(q)))
     $              +(1.-beta(q))
     $              *(alpha(k)*fluxbolSSP(iZinf(k),invtime(q)+1)
     $              +(1.-alpha(k))*fluxbolSSP(iZsup(k),invtime(q)+1)))
               
               fluxbolZ=fluxbolZ+SFRlum(k)*ZSFR(k)*(beta(q)
     $              *(alpha(k)*fluxbolSSP(iZinf(k),invtime(q))
     $              +(1.-alpha(k))*fluxbolSSP(iZsup(k),invtime(q)))
     $              +(1.-beta(q))
     $              *(alpha(k)*fluxbolSSP(iZinf(k),invtime(q)+1)
     $              +(1.-alpha(k))*fluxbolSSP(iZsup(k),invtime(q)+1)))
               
               fluxbolage=fluxbolage+SFRlum(k)*(i-k)*(beta(q)
     $              *(alpha(k)*fluxbolSSP(iZinf(k),invtime(q))
     $              +(1.-alpha(k))*fluxbolSSP(iZsup(k),invtime(q)))
     $              +(1.-beta(q))
     $              *(alpha(k)*fluxbolSSP(iZinf(k),invtime(q)+1)
     $              +(1.-alpha(k))*fluxbolSSP(iZsup(k),invtime(q)+1)))
            end do

*     Calculation of the number of Lyman continuum photons. 
            NLymtot=0.
            do k=max(1,i-50),i  
               NLymtot=NLymtot
     $              +SFRlum(k)*(alpha(k)*NLym(iZinf(k),i+1-k)
     $              +(1.-alpha(k))*NLym(iZsup(k),i+1-k))
            end do
*     Calculation of the number of SNII.
            nSNIItot=0.
            do k=max(1,i-100),i 
               nSNIItot=nSNIItot
     $              +SFRlum(k)*(alpha(k)*nSNII(iZinf(k),i+1-k)
     $              +(1.-alpha(k))*nSNII(iZsup(k),i+1-k))
            end do
*     Calculation of the number of SNIa.
            nSNIatot=0.
            do k=1,i            
               nSNIatot=nSNIatot
     $              +SFRlum(k)*(alpha(k)*nSNIa(iZinf(k),i+1-k)
     $              +(1.-alpha(k))*nSNIa(iZsup(k),i+1-k))
            end do
            
            call redden_lines(codeext, inclin,
     $           lambdaline, nlines, Zgas(i), sigmagas(i),
     $           flinetot,
     $           fluxext(jimpr))
            
            if (fluxbol.gt.0.) then
               fluxext(jimpr)=fluxext(jimpr)/fluxbol
               Zbol=fluxbolZ/fluxbol
               agebol=fluxbolage/fluxbol
            else
               fluxext(jimpr)=0.
               Zbol=0.
               agebol=0.
            end if

            call fits_spec_para_w(LuSpec,i,
     $           Mgal,sigmastars,sigmaWD,sigmaBHNS,sigmasub,sigmagas,
     $           Zgas,Zstars,Zbol,fluxbol,tauV(jimpr),fluxext(jimpr),
     $           SFR, NLymtot,nSNIItot,nSNIatot,agestars,agebol,istat)
            call fits_spec_line_w(LuSpec, jimpr, nlines, lambdaline,
     $           flinetot, istat)

         end do

         call fits_spec_close(LuSpec,istat)

      end do                    ! End of loop on scenarii
      call scenario_close(istat)

      end
