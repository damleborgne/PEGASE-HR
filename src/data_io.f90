MODULE data_io


  use types
  use constants
  use util

!  use pegase_func

  ! data_io
  ! collection of subroutine used to access data used by the program "spectra"
  !
  !------------------------------------------------------------------------------
  ! subroutine data_time_r (ntimesimpr,timeimpr,istat) 
  ! subroutine data_extin_r(tauslab,albslab,asymslab,emergslab,
  !    s     inclinslab,emerginclinslab,istat)
  ! subroutine data_king_r(tauKing,albKing,asymKing,emergKing,istat)
  ! subroutine data_dust_r(Zext,frac,nlambdaext,lambdaext,tauext,albedoext,
  !    $        asymext,istat)
  ! subroutine data_spitz_r(nSpitzer,taudustSpitzer,ySpitzer,istat)
  ! subroutine data_sfr_r(typeSFR,fileSFR,ntimesfile,timefile,SFRfile,
  !    s     Zfile,istat)
  ! subroutine sfr_time(SFRparam,fileSFR,SFR,ZSFR,istat)
  ! subroutine data_neb_r(nlambda,lambda,
  !    s     >fneb,nlines,lambdaline,fline,istat)
  !--------------------------------------------------------------------

CONTAINS
  !**** Reading of the times for which the spectra are to be computed.
  !****  file: "ages.dat"
  subroutine data_time_r (timeinfo,istat)     

    use constants

    implicit none

    TYPE(t_timeinfo),INTENT(INOUT) :: timeinfo
    integer istat

    character(len=80) :: line
    integer Lun,error

    !     First, try to read the *local* ages.dat file
    call file_open('ages.dat',lun,istat)
    if(istat.eq.0) then       
       write(*,'(a)') 'Warning: a file "ages.dat" was found in the'&
            //' current directory.'
       write(*,'(a)') '         This file will be used, overriding'&
            //' the default one.'
    else  ! If absent, try to use the "default" ages
       write(*,'(a)') 'Reading '//trim(PEG_ROOT)//'/data/user_defined/ages.dat'
       call file_open(trim(PEG_ROOT)//'/data/user_defined/ages.dat',lun,istat)
    endif

    if(istat.ne.0) return

    timeinfo%ntimesimpr=0
    error=0
    do while (error.eq.0)
       read(Lun,*,iostat=error) line
       if (error.eq.0.and.line.gt.' '.and.line(1:1).ne.'!') then
          read(line,*,iostat=error) timeinfo%timeimpr(timeinfo%ntimesimpr+1)
          if (error.eq.0) then
             ! No age larger than 20 Gyr.
             if (timeinfo%timeimpr(timeinfo%ntimesimpr+1).le.20000.and.&
                  timeinfo%timeimpr(timeinfo%ntimesimpr+1).ge.0.and.&
                  timeinfo%ntimesimpr+1.le.nmaxotimes) then
                timeinfo%ntimesimpr=timeinfo%ntimesimpr+1
             endif
             if (timeinfo%ntimesimpr+1.gt.nmaxotimes) then
                write(*,*) ' WARNING : the number of output timesteps is larger than the maximum number authorized.'
                write(*,*) ' Please adjust the nmaxotimes variables in constants.f90'
             endif
          endif
       endif
    end do
    close(Lun)

    return
  end subroutine data_time_r


  !--------------------------------------------------------------------
  subroutine data_extin_r(istat)

    use types

    implicit none

    
    integer istat

    integer lun,i1,i2,i3,i4

    call file_open(trim(PEG_ROOT)//'/data/external/slab.dat',lun,istat)
    if(istat.ne.0) return

    do i1=1,nslab1
       do i2=1,nslab2

          read(lun,*) dust%tauslab(i1),dust%albslab(i2),&
               dust%asymslab(1:nslab3)

          read(lun,*) (dust%emergslab(i1,i2,i3),i3=1,nslab3)
          dust%emergslab(i1,i2,1:nslab3)=log(dust%emergslab(i1,i2,1:nslab3))

          do i4=1,nslab4
             read(lun,*) dust%inclinslab(i4),(dust%emerginclinslab(i1,i2,i3,i4),&
                  i3=1,nslab3)
             do i3=1,nslab3
                dust%emerginclinslab(i1,i2,i3,i4)=&
                     log(dust%emerginclinslab(i1,i2,i3,i4))
             end do
          end do
       end do
    end do
    close(lun)

    return
  end subroutine data_extin_r


  !--------------------------------------------------------------------
  subroutine data_king_r(istat)
    implicit none

    integer istat

    integer Lun,i1,i2,i3

    call file_open(trim(PEG_ROOT)//'/data/external/King.dat',lun,istat)
    if(istat.ne.0) return
    do i1=1,nK1
       do i2=1,nK2
          do i3=1,nK3
             read(lun,*) dust%tauKing(i1),dust%albKing(i2),dust%asymKing(i3),&
                  dust%emergKing(i1,i2,i3)
             dust%emergKing(i1,i2,i3)=log(dust%emergKing(i1,i2,i3))
          end do
       end do
    end do
    close(lun)

    return
  end subroutine data_king_r


  !--------------------------------------------------------------------
  subroutine data_dust_r(istat)
    implicit none

    integer istat

    integer      lun,i,j
    real :: coeffext(5,2)
    character(len=72) :: a

    call file_open(trim(PEG_ROOT)//'/data/external/dust.dat',lun,istat)
    if(istat.ne.0) return

    read(lun,'(a)') a
    read(lun,'(a)') a
    do i=2,4
       read(lun,*) dust%Zext(i),(coeffext(i,j),j=1,2)
    end do
    dust%Zext(1)=0.
    coeffext(1,1)=coeffext(2,1)
    coeffext(1,2)=coeffext(2,2)
    dust%Zext(5)=1.
    coeffext(5,1)=coeffext(4,1)
    coeffext(5,2)=coeffext(4,2)
    do i=1,5
       dust%frac(i)=coeffext(i,1)/(coeffext(i,1)+coeffext(i,2))
    enddo
    read(lun,'(a)') a
    read(lun,*) dust%nlambdaext
    do i=1,dust%nlambdaext
       read(lun,*) dust%lambdaext(i),(dust%tauext(i,j),dust%albedoext(i,j),&
            dust%asymext(i,j),j=1,2)
    end do
    close(lun)

    return
  end subroutine data_dust_r


  !--------------------------------------------------------------------
  subroutine data_spitz_r(nSpitzer,taudustSpitzer,ySpitzer,istat)

    implicit none


    integer nSpitzer
    real    taudustSpitzer(20),ySpitzer(20)
    integer istat

    integer Lun,i

    call file_open(trim(PEG_ROOT)//'/data/external/Spitzer.dat',lun,istat)
    read(lun,*) nSpitzer
    do i=1,nSpitzer
       read(lun,*) taudustSpitzer(i),ySpitzer(i)
    enddo
    close(lun)

    return
  end subroutine data_spitz_r


  !--------------------------------------------------------------------
  subroutine data_sfr_r(SFRparam, fileSFR, ntimesfile, timefile,&
       SFRfile, Zfile, istat)
    implicit none

    real      SFRparam(*)
    character(len=*) :: fileSFR
    integer       ntimesfile
    real          timefile(*),SFRfile(*),Zfile(*)
    integer       istat

    integer       Lun,error

    istat=0

    if (SFRparam(1).eq.-1) then
       call file_open(fileSFR,Lun,istat)
       if(istat.ne.0) return
       ntimesfile=0
       error=0
       do while (error.eq.0)
          read(Lun,*,iostat=error) timefile(ntimesfile+1),&
               SFRfile(ntimesfile+1)
          if (error.eq.0) ntimesfile=ntimesfile+1
       end do
       close(Lun)
    else if (SFRparam(1).eq.-2) then
       call file_open(fileSFR,Lun,istat)
       if(istat.ne.0) return
       ntimesfile=0
       error=0
       do while (error.eq.0)
          read(Lun,*,iostat=error) timefile(ntimesfile+1),&
               SFRfile(ntimesfile+1),Zfile(ntimesfile+1)
          if (error.eq.0) ntimesfile=ntimesfile+1
       end do
       close(Lun)
    endif

    return
  end subroutine data_sfr_r

  !------------------------------------------------------------------------------
  ! For SFRparam(1) lt 0, sfr_time reads the file "fileSFR" and resamples the
  ! SFR and  ZSFR (typeSFR=-2) at each time step (from t=0 to nmaxtimes-1)
  ! [note: there is no need to compute up to nmaxtimes, we should stop at
  !  timeimpr(ntimesimpr), to pass as input arg ...]

  subroutine sfr_time(SFRparam,fileSFR,SFR,ZSFR,istat)

    implicit none


    real,dimension(:) :: SFRparam ! input (do nothing if SFRparam(1) ge 0)
    character(len=*) :: fileSFR     ! input

    real,dimension(:) :: SFR(*), ZSFR(*)     ! output
    integer       :: istat

    integer       ntimesfile
    real,dimension(nmaxtimes) :: timefile,SFRfile,Zfile
    real          :: time
    integer       :: i,i0

    !**** Reading of the SFR (and possibly Z also) in fileSFR.         
    !     Does nothing is typeSFR.ge.0.
    call data_sfr_r(SFRparam,fileSFR,ntimesfile,timefile,SFRfile,&
         Zfile,istat)
    if(istat.ne.0) return

    !**** Interpolate SFR, and possibly ZSFR at the time sampling points
    if (SFRparam(1).le.-1) then

       i0=0
       do i=1,nmaxtimes-1            
          time=i-1.
          !     
          call bracket(ntimesfile,timefile,time,i0)
          call Steffen(ntimesfile,timefile,SFRfile,.false.,&
               time,SFR(i),i0)
          if (SFRparam(1).eq.-2) then
             call Steffen(ntimesfile,timefile,Zfile,.false.,&
                  time,ZSFR(i),i0)
          endif
          !
       enddo
    endif

    return
  end subroutine sfr_time


  !--------------------------------------------------------------------
  !**** Calculations for the nebular continuum.
  ! Read the file "HII.dat". Compute and interpolate the nebular continuum "fneb"
  ! at the grid points "lambda". Read the fluxes in lines.

  subroutine data_neb_r(fneb,nlines,lambdaline,fline,istat)

    use types

    implicit none
    


    real,dimension(:)    :: fneb   ! out: neb continuum interpolated at lambda
    real,dimension(:)    :: lambdaline     ! out: wavelengths of the lines
    real,dimension(:)    :: fline          ! out: flux in lines
    integer     :: nlines            ! out: number of emission lines
    integer     :: istat             ! out: return status

    integer     :: lun
    real    :: g1,g2,g3,g4,gam1(100),gam2(100),gam3(100),gam4(100)
    integer     :: nlambdacont
    real    :: lambdacont(100)
    real    :: HeI,HeII,alphaTe,gline
    integer     :: i,j

    ! Initialize variables to avoid uninitialized warnings
    g1 = 0.0
    g2 = 0.0
    g3 = 0.0
    g4 = 0.0

    call file_open(trim(PEG_ROOT)//'/data/external/HII.dat',lun,istat)
    if(istat.ne.0) return

    read(lun,*) HeI,HeII,alphaTe
    read(lun,*) nlambdacont   
    do i=1,nlambdacont
       read(lun,*) lambdacont(i),gam1(i),gam2(i),gam3(i),gam4(i)
    end do

    j=1
    do i=1,stellibinfo%nlambda
       do while (((lambdacont(j)-stellibinfo%lambda(i))&
            *(lambdacont(j+1)-stellibinfo%lambda(i)).gt.0.)&
            .and.(j.lt.nlambdacont))
          j=j+1
       end do
       if ((lambdacont(j)-stellibinfo%lambda(i))&
            *(lambdacont(j+1)-stellibinfo%lambda(i)).le.0.) then
          g1=interplinlog(1./lambdacont(j),1./lambdacont(j+1),&
               gam1(j),gam1(j+1),1./stellibinfo%lambda(i))
          g2=interplinlog(1./lambdacont(j),1./lambdacont(j+1),&
               gam2(j),gam2(j+1),1./stellibinfo%lambda(i))
          g3=interplinlog(1./lambdacont(j),1./lambdacont(j+1),&
               gam3(j),gam3(j+1),1./stellibinfo%lambda(i))
          g4=interplinlog(1./lambdacont(j),1./lambdacont(j+1),&
               gam4(j),gam4(j+1),1./stellibinfo%lambda(i))
       endif
       !     -1.9: gaunt factor ~ nu**(-0.1) at long wavelength
       if (stellibinfo%lambda(i).ge.lambdacont(nlambdacont)) then
          g1=gam1(nlambdacont)&
               *(stellibinfo%lambda(i)/lambdacont(nlambdacont))**0.1
          g2=gam2(nlambdacont)&
               *(stellibinfo%lambda(i)/lambdacont(nlambdacont))**0.1
          g3=gam3(nlambdacont)&
               *(stellibinfo%lambda(i)/lambdacont(nlambdacont))**0.1
          g4=gam4(nlambdacont)&
               *(stellibinfo%lambda(i)/lambdacont(nlambdacont))**0.1
       endif
       fneb(i)=1.d-40*(g1+g2+HeI*g3+HeII*g4)*c/alphaTe&
            /stellibinfo%lambda(i)**2
    enddo

    !**** Calculations for the nebular lines.

    read(lun,*) nlines
    do i=1,nlines
       !     gline: line strength relative to Hbeta (first line).
       read(lun,*) lambdaline(i),gline
       fline(i)=1.244d-25*gline/alphaTe
    end do
    close(lun)


    return
  end subroutine data_neb_r

  !----------------------------------------------------------------------
  subroutine redden_r(istat)

    implicit none

    integer istat


    !****** Read extinction data from files.

    ! Extinction parameters for the slab model
    call data_extin_r(istat)

    ! Extinction parameters for the King model
    if(istat.eq.0) call data_king_r(istat)

    ! Dust properties (absorption, albedo, asymmetry)
    ! for graphites and silicates.  C/Si ratios depend on the metallicity.
    if (istat.eq.0) call data_dust_r(istat)

    return
  end subroutine redden_r

end MODULE data_io
