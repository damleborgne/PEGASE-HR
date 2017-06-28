MODULE compute_scenario


  implicit none


CONTAINS

  !################################################################################

  subroutine compute_scen()

    use types
    use nrtype
    use constants
    use pegase_func

    implicit none


    !##################################################
    ! Variables
    !##################################################

!    INTEGER,INTENT(IN)    :: iscenario
!    type(t_scenario)    :: myscen

    ! evolutive physical properties
     real,dimension(:,:),allocatable :: flux_gal    ! (stellibinfo%nlambda, timeinfo%ntimesimpr)
     real,dimension(:,:),allocatable :: Lspec_gal   !(-nmaxCM:nmaxspecLCB,timeinfo%ntimesimpr)

    ! within a timestep:
    real,dimension(stellibinfo%nlambda)        :: fluxgal
    REAL(DP)                          :: NLymtot
    REAL                              :: nSNIItot, nSNIatot,  agebol, Zbol
    real                              :: fluxbolZ, fluxbolage, fluxbol

    ! spectral features
    real,dimension(nmaxlines)  :: fline ! flux in lines
    real,dimension(nmaxlines)  :: flinetot

    ! Dust
    integer            :: nSpitzer
    real,dimension(20) :: taudustSpitzer,ySpitzer
    real               :: taudust,       y
    integer            :: i0

    ! Nebular emission
    real,dimension(stellibinfo%nlambda) :: fneb ! nebular continuum interpolated at stellibinfo%lambda
    real                       :: contribneb ! fraction of fneb added to galaxy specturm
    real                       :: f ! fraction of light below 912 A absorbed by dust or neb emission


    ! other local variables
    integer,dimension(nmaxspec)  :: itotused
    integer                      :: ntotused 
    integer                      :: i,k,q,jimpr
    integer                      :: iZstellib1,iZstellib2
    integer                      :: i912
    integer                      :: istat
!    real                         :: ti1,te1 ! timing
    integer, DIMENSION(8)        :: vi,ve ! timing

    !##################################################
    ! Main body
    !##################################################

    call date_and_time(values=vi)

    if (verbose.ge.1) write(*,*) ' ----------------------------------------'
    if (verbose.ge.1) write(*,*) '  Scenario number',myscen%number


    allocate(flux_gal(stellibinfo%nlambda, timeinfo%ntimesimpr))
    allocate(Lspec_gal(-nmaxCM:nmaxspecLCB,timeinfo%ntimesimpr))

    !     Read scenario from file opened with "scenario_open" earlier.
    galprop%ZSFR(1)=myscen%ZSFRinit
    galprop%Zgas(1)=myscen%Zgasinit


    ! Initialization.
    Lspec_gal=0.d0

    !     Reading of the SFR (and possibly Z also) in fileSFR, 
    !     interpolate to the sampling times. 
    if (myscen%SFRparam(1).lt.0) then
       call sfr_time(myscen%SFRparam,myscen%fileSFR,galprop%SFR,galprop%ZSFR,istat)
       if (istat.ne.0) then
          write(*,*)'Could not read SFR file',istat
          stop
       end if
    end if


    !################################################################################
    !     Convolution of the SSPs with the star formation rate. 
    !     This loop takes 2.1 seconds. TBO ?
    !################################################################################
    
    if (verbose.ge.2) write(*,*) ' Incrementing properties....'
    ! Here is the CONVOLUTION 20000x20000. About 3 seconds
    call increm_ppties()
!    if (verbose.ge.2) write(*,*) '..... Done.'
      
    call date_and_time(values=ve)
    print*, '                                        last step = ',&
         ((ve(8)+1000.*(ve(7)+60.*ve(6)))-&
         (vi(8)+1000.*(vi(7)+60.*vi(6))))/1000.
    vi=ve
    
    !################################################################################
    ! Min anx Max Z for SSPs
    iZstellib1 = nmaxZtracks
    iZstellib2 = 1

    do i = 1, timeinfo%timeimpr(timeinfo%ntimesimpr)
       if (galprop%iZinf(i) .lt. iZstellib1) iZstellib1 = galprop%iZinf(i)
       if (galprop%iZsup(i) .gt. iZstellib2) iZstellib2 = galprop%iZsup(i)
    enddo
    iZstellib1 = SSP%iZ1(iZstellib1)
    iZstellib2 = SSP%iZ2(iZstellib2)

    !##################################################
    ! Total number of spectra actually used
    ! After this, ntotused contains the number of spectra actually used,
    !    and itotused contains their index (on the INITIAL LCB scale).
    call count_used(iZstellib1, iZstellib2, ntotused, itotused)
    if (verbose.ge.2) write(*,*) ' Spectra actually used: ',ntotused

    !##################################################
    ! Various computations related to the continuum, moved here
    !   because the wavelengths must be known.

    ! Position of the Lyman break: lambda(i912) < 912 A <= lambda(i912+1)
    i912=0  ! i912 was mistakenly set to 1 before version 1.3
    do while (stellibinfo%lambda(i912+1).lt.912.d0)
       i912=i912+1
    end do

    if (verbose.ge.2) write(*,*) ' Computing nebular continuum emission'
    ! Calculations for the nebular continuum and nebular emission
    call data_neb_r(fneb, galprop%nlines, galprop%lambdaline, fline, istat)

    if(istat.eq.0) call data_spitz_r(nSpitzer,taudustSpitzer,ySpitzer,istat)

    if(istat.ne.0) then
       write(*,*) 'Compute nebular continuum & emission ... failed'
       stop
    else
!       if (verbose.ge.2) write(*,*) '..... Done.'
    end if
    call date_and_time(values=ve)
    print*, '                                        last step = ',&
         ((ve(8)+1000.*(ve(7)+60.*ve(6)))-&
         (vi(8)+1000.*(vi(7)+60.*vi(6))))/1000.
    vi=ve

    !################################################################################
    !     Convolution of the SSPs spectra with the star formation rate at timeinfo%timeimpr
    !     This loop takes 3.3 seconds. TBO ?
    !################################################################################

    !     Convolution of SSP spectra with SFR. Takes 3 seconds. TBO with fft ?
    if (verbose.ge.2) print*, "Convolving SFR and SSPs"

    call convol_SFR_SSPs(&  ! in pegase_func.f
         ntotused,  &
         itotused,  &
         Lspec_gal)

!    if (verbose.ge.2) print*, "..... Done."

    call date_and_time(values=ve)
    print*, '                                        last step = ',&
         ((ve(8)+1000.*(ve(7)+60.*ve(6)))-&
         (vi(8)+1000.*(vi(7)+60.*vi(6))))/1000.
    vi=ve

    !################################################################################
    !     Continuum. Takes 0.6 seconds
    !################################################################################

    
    if (verbose.ge.2) write(*,*) ' Computing stellar continuum...'
    call comput_continuum(ntotused,itotused, Lspec_gal, flux_gal)
!    if (verbose.ge.2) write(*,*) '..... Done.'

    call date_and_time(values=ve)
    print*, '                                        last step = ',&
         ((ve(8)+1000.*(ve(7)+60.*ve(6)))-&
         (vi(8)+1000.*(vi(7)+60.*vi(6))))/1000.
    vi=ve

    !--------------------------------------------------
    if (verbose.ge.2) write(*,*) ' First loop :final continuum spectra (+neb cont. + ext.)'
    !**** First loop on output times : 
    i0=0
    do jimpr=1,timeinfo%ntimesimpr
       i=timeinfo%timeimpr(jimpr)+1

       fluxgal = flux_gal(:, jimpr)

       ! Add nebular emission continuum if requested
       if (myscen%answerneb.eq.'Nebular emission') then
          if (myscen%codeext.ne.0) then
             taudust=0.5d0*galprop%Zgas(i)/Zsol
             call bracket(nSpitzer,taudustSpitzer,taudust,i0)
             call Steffen(nSpitzer,taudustSpitzer,ySpitzer,.false.,taudust,y,i0)
             f=y**3
          else
             f=1.d0
          end if

          contribneb=0.d0
          do k=max(i-50,1),i
             if (galprop%SFRlum(k).gt.1.d-20) then
                q=i+1-k
                contribneb=contribneb+ galprop%SFRlum(k)*&
                     (SSP%beta(q)*&
                     (SSP%alpha(k)*SSP%NLym(galprop%iZinf(k),SSP%invtime(q))&
                     +(1.d0-SSP%alpha(k))*SSP%NLym(galprop%iZsup(k),SSP%invtime(q)))&
                     +(1.d0-SSP%beta(q))*&
                     (SSP%alpha(k)*SSP%NLym(galprop%iZinf(k),SSP%invtime(q)+1)&
                     +(1.d0-SSP%alpha(k))*SSP%NLym(galprop%iZsup(k),SSP%invtime(q)+1)))
             end if
          end do
          contribneb=contribneb*f

          fluxgal = fluxgal + contribneb * fneb

       else
          f=0.d0
       end if

       ! The Lyman continuum photons used to produce the nebular emission are removed from the spectrum.
       fluxgal(1:i912)=(1.d0-f)*fluxgal(1:i912)

       call redden_cont(myscen%codeext, i912, stellibinfo%lambda, stellibinfo%nlambda, myscen%inclin,&
            galprop%Zgas(i), galprop%sigmagas(i),&
            fluxgal,&
            galprop%fluxext(jimpr), galprop%tauV(jimpr))

       galprop%continuum_spectra(1:stellibinfo%nlambda,jimpr)=fluxgal

    enddo

    call date_and_time(values=ve)
    print*, '                                        last step = ',&
         ((ve(8)+1000.*(ve(7)+60.*ve(6)))-&
         (vi(8)+1000.*(vi(7)+60.*vi(6))))/1000.
    vi=ve
    !--------------------------------------------------
    if (verbose.ge.2) write(*,*) ' Second loop on output times : emission lines, lum average quantities...'
    !**** Second loop on output times : 
    i0=0
    do jimpr=1,timeinfo%ntimesimpr
       i=timeinfo%timeimpr(jimpr)+1

       flinetot=0.d0

       !  Add nebular emission lines if requested
       if (myscen%answerneb.eq.'Nebular emission') then
          if (myscen%codeext.ne.0) then
             taudust=0.5d0*galprop%Zgas(i)/Zsol
             call bracket(nSpitzer,taudustSpitzer,taudust,i0)
             call Steffen(nSpitzer,taudustSpitzer,ySpitzer,.false.,taudust,y,i0)
             f=y**3d0
          else
             f=1.d0
          end if

          contribneb=0.d0
          do k=max(i-50,1),i
             if (galprop%SFRlum(k).gt.1.d-20) then
                q=i+1-k
                contribneb=contribneb + galprop%SFRlum(k)*&
                     (SSP%beta(q)*&
                     (SSP%alpha(k)*SSP%NLym(galprop%iZinf(k),SSP%invtime(q))&
                     +(1.d0-SSP%alpha(k))*SSP%NLym(galprop%iZsup(k),SSP%invtime(q)))&
                     +(1.d0-SSP%beta(q))*&
                     (SSP%alpha(k)*SSP%NLym(galprop%iZinf(k),SSP%invtime(q)+1)&
                     +(1.d0-SSP%alpha(k))*SSP%NLym(galprop%iZsup(k),SSP%invtime(q)+1)))
             end if
          end do
          contribneb=contribneb*f

          flinetot(1:galprop%nlines) = flinetot(1:galprop%nlines) + contribneb *fline(1:galprop%nlines)
       else
          f=0.d0
       end if

       !     Compute luminosity weighted quantities.
       fluxbol=0.d0         
       fluxbolZ=0.d0
       fluxbolage=0.d0
       do k=1,i
          q=i+1-k
          fluxbol=fluxbol+galprop%SFRlum(k)*(SSP%beta(q)&
               *(SSP%alpha(k)*SSP%fluxbolSSP(galprop%iZinf(k),SSP%invtime(q))&
               +(1.d0-SSP%alpha(k))*SSP%fluxbolSSP(galprop%iZsup(k),SSP%invtime(q)))&
               +(1.d0-SSP%beta(q))&
               *(SSP%alpha(k)*SSP%fluxbolSSP(galprop%iZinf(k),SSP%invtime(q)+1)&
               +(1.d0-SSP%alpha(k))*SSP%fluxbolSSP(galprop%iZsup(k),SSP%invtime(q)+1)))

          fluxbolZ=fluxbolZ+galprop%SFRlum(k)*galprop%ZSFR(k)*(SSP%beta(q)&
               *(SSP%alpha(k)*SSP%fluxbolSSP(galprop%iZinf(k),SSP%invtime(q))&
               +(1.d0-SSP%alpha(k))*SSP%fluxbolSSP(galprop%iZsup(k),SSP%invtime(q)))&
               +(1.d0-SSP%beta(q))&
               *(SSP%alpha(k)*SSP%fluxbolSSP(galprop%iZinf(k),SSP%invtime(q)+1)&
               +(1.d0-SSP%alpha(k))*SSP%fluxbolSSP(galprop%iZsup(k),SSP%invtime(q)+1)))

          fluxbolage=fluxbolage+galprop%SFRlum(k)*(i-k)*(SSP%beta(q)&
               *(SSP%alpha(k)*SSP%fluxbolSSP(galprop%iZinf(k),SSP%invtime(q))&
               +(1.d0-SSP%alpha(k))*SSP%fluxbolSSP(galprop%iZsup(k),SSP%invtime(q)))&
               +(1.d0-SSP%beta(q))&
               *(SSP%alpha(k)*SSP%fluxbolSSP(galprop%iZinf(k),SSP%invtime(q)+1)&
               +(1.d0-SSP%alpha(k))*SSP%fluxbolSSP(galprop%iZsup(k),SSP%invtime(q)+1)))
       end do

       !     Calculation of the number of Lyman continuum photons. 
       NLymtot=0.d0
       do k=max(1,i-50),i  
          NLymtot=NLymtot&
               +galprop%SFRlum(k)*(SSP%alpha(k)*SSP%NLym(galprop%iZinf(k),i+1-k)&
               +(1.d0-SSP%alpha(k))*SSP%NLym(galprop%iZsup(k),i+1-k))
       end do

       !     Calculation of the number of SNII.
       nSNIItot=0.d0
       do k=max(1,i-100),i 
          nSNIItot=nSNIItot&
               +galprop%SFRlum(k)*(SSP%alpha(k)*SSP%nSNII(galprop%iZinf(k),i+1-k)&
               +(1.d0-SSP%alpha(k))*SSP%nSNII(galprop%iZsup(k),i+1-k))
       end do
       !     Calculation of the number of SNIa.
       nSNIatot=0.d0
       do k=1,i            
          nSNIatot=nSNIatot&
               +galprop%SFRlum(k)*(SSP%alpha(k)*SSP%nSNIa(galprop%iZinf(k),i+1-k)&
               +(1.d0-SSP%alpha(k))*SSP%nSNIa(galprop%iZsup(k),i+1-k))
       end do

       call redden_lines(myscen%codeext, myscen%inclin,&
            galprop%lambdaline, galprop%nlines, galprop%Zgas(i), galprop%sigmagas(i),&
            flinetot,&
            galprop%fluxext(jimpr))


       if (fluxbol.gt.0.d0) then
          galprop%fluxext(jimpr)=galprop%fluxext(jimpr)/fluxbol
          Zbol=fluxbolZ/fluxbol
          agebol=fluxbolage/fluxbol
       else
          galprop%fluxext(jimpr)=0.d0
          Zbol=0.d0
          agebol=0.d0
       end if


       ! save Zbol,fluxbol,NLymtot,nSNIItot,nSNIatot,agebol to arrays
       galprop%Zbol(jimpr)=Zbol
       galprop%fluxbol(jimpr)=fluxbol
       galprop%NLymtot(jimpr)=NLymtot
       galprop%nSNIItot(jimpr)=nSNIItot
       galprop%nSNIatot(jimpr)=nSNIatot
       galprop%agebol(jimpr)=agebol

       galprop%lines_spectra(1:galprop%nlines,jimpr)=flinetot
       
    end do

    call date_and_time(values=ve)
    print*, '                                        last step = ',&
         ((ve(8)+1000.*(ve(7)+60.*ve(6)))-&
         (vi(8)+1000.*(vi(7)+60.*vi(6))))/1000.
    vi=ve

    deallocate(flux_gal)
    deallocate(Lspec_gal)

  end subroutine compute_scen

  !##################################################
  subroutine write_spectrum()

    use fits_spec_io

    implicit none

    integer :: LuSpec, istat, jimpr
    character(len=strmax)        :: a ! string used for conversion
    real,dimension(stellibinfo%nlambda)        :: fluxgal
    real,dimension(nmaxlines)  :: flinetot 

    ! Write header
    call fits_spec_creat(myscen,stellibinfo%nlambda,timeinfo%ntimesimpr,&
         SSP%nZ,SSP%header,SSP%fSNIa,&
         stellibinfo%filename_short,LuSpec,&
         istat)

    if (istat.ne.0) then
       write(*,*) 'ERROR : Cannot create "spectra" file'
       write(*,*) 'ERROR',istat
       write(*,*) 'Please check that the file doesn''t exist already :'
       write(*,*) trim(myscen%filespectra)
       write(*,*) 'Exiting now...'
       stop
    endif
    
    ! Write wavelength array
    call fits_spec_wave_w(LuSpec,stellibinfo%nlambda,stellibinfo%lambda,istat)
    if(istat.ne.0) then
       write(*,*)'Error writing wavelength to "spectra" file'
       write(*,*)'Exiting now!'
       stop
    end if
    
    !     Log a "SFR_EXCESS" warning in the output fits spectrum
    if(galprop%excessSFR.gt.0) then
       write(a,'(a,i5,a)') &
            'WARNING: the SFR has exceeded the maximal possible SFR at ',galprop%excessSFR-1,' Myr!'

       call fits_spec_cmt_w(LuSpec,a,istat)
       if (istat.ne.0) then 
          write(*,*) 'Error when writing to Fits file !'
          stop
       endif
       write(*,'(a)') a
    endif


    do jimpr=1,timeinfo%ntimesimpr
       fluxgal = galprop%continuum_spectra(1:stellibinfo%nlambda,jimpr)
       call fits_spec_cont_w(LuSpec,jimpr,stellibinfo%nlambda,fluxgal,istat)
       
    enddo

    do jimpr=1,timeinfo%ntimesimpr

       call fits_spec_para_w(LuSpec, timeinfo%timeimpr(jimpr)+1,&
            galprop%Zbol(jimpr), galprop%fluxbol(jimpr),&
            galprop%tauV(jimpr), galprop%fluxext(jimpr),&
            galprop%NLymtot(jimpr), galprop%nSNIItot(jimpr),&
            galprop%nSNIatot(jimpr),galprop%agebol(jimpr), istat)

       flinetot=galprop%lines_spectra(1:galprop%nlines,jimpr)
       call fits_spec_line_w(LuSpec, jimpr, galprop%nlines, galprop%lambdaline,&
            flinetot, istat)

    enddo

    call fits_spec_close(LuSpec,istat)


    
  end subroutine write_spectrum

END MODULE compute_scenario
