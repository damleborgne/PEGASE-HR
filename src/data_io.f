c data_io
c Part of the Pegase package; version PhP, 2001/09/24
c collection of subroutine used to access data used by the program "spectra"
c
c------------------------------------------------------------------------------
c subroutine data_time_r (ntimesimpr,timeimpr,istat) 
c subroutine data_extin_r(tauslab,albslab,asymslab,emergslab,
c    s     inclinslab,emerginclinslab,istat)
c subroutine data_king_r(tauKing,albKing,asymKing,emergKing,istat)
c subroutine data_dust_r(Zext,frac,nlambdaext,lambdaext,tauext,albedoext,
c    $        asymext,istat)
c subroutine data_spitz_r(nSpitzer,taudustSpitzer,ySpitzer,istat)
c subroutine data_sfr_r(typeSFR,fileSFR,ntimesfile,timefile,SFRfile,
c    s     Zfile,istat)
c subroutine sfr_time(SFRparam,fileSFR,SFR,ZSFR,istat)
c subroutine data_neb_r(nlambda,lambda,
c    s     >fneb,nlines,lambdaline,fline,istat)
c--------------------------------------------------------------------
***** Reading of the times for which the spectra are to be computed.
*****  file: "ages.dat"
      subroutine data_time_r (ntimesimpr,nmaxtimesimpr,timeimpr,istat)     

      implicit none
      include 'peg_config.f'

      integer ntimesimpr
      integer nmaxtimesimpr
      integer timeimpr(*)
      integer istat

      character*80 line
      integer Lun,error

*     First, try to read the *local* ages.dat file
      call file_open('ages.dat',lun,istat)
      if(istat.eq.0) then       
         write(*,'(a)') 'Warning: a file "ages.dat" was found in the'
     &        //' current directory.'
         write(*,'(a)') '         This file will be used, overriding'
     &        //' the default one.'
      else  ! If absent, try to use the "default" ages
         call file_open(PEG_ROOT//'data/user_defined/ages.dat',lun,istat)
      endif

      if(istat.ne.0) return

      ntimesimpr=0
      error=0
      do while (error.eq.0)
         read(Lun,*,iostat=error) line
         if (error.eq.0.and.line.gt.' '.and.line(1:1).ne.'!') then
            read(line,*,iostat=error) timeimpr(ntimesimpr+1)
            if (error.eq.0) then
               ! No age larger than 20 Gyr.
               if (timeimpr(ntimesimpr+1).le.20000.and.
     $              timeimpr(ntimesimpr+1).ge.0.and.
     $              ntimesimpr+1.le.nmaxtimesimpr) then
                  ntimesimpr=ntimesimpr+1
               endif
            endif
         endif
      end do
      close(Lun)

      return
      end


c--------------------------------------------------------------------
      subroutine data_extin_r(tauslab,albslab,asymslab,emergslab,
     s     inclinslab,emerginclinslab,istat)
      implicit none
      include 'peg_config.f'
      integer nslab1,nslab2,nslab3,nslab4
      parameter(nslab1=17,nslab2=9,nslab3=6,nslab4=10)
      real    tauslab(nslab1),albslab(nslab2)
      real    asymslab(nslab3),inclinslab(nslab4)
      real    emergslab(nslab1,nslab2,nslab3)
      real    emerginclinslab(nslab1,nslab2,nslab3,nslab4)
      integer istat

      integer lun,i1,i2,i3,i4

      call file_open(PEG_ROOT//'data/external/slab.dat',lun,istat)
      if(istat.ne.0) return

      do i1=1,nslab1
         do i2=1,nslab2
            read(lun,*) tauslab(i1),albslab(i2),
     $           (asymslab(i3),i3=1,nslab3)
            read(lun,*) (emergslab(i1,i2,i3),i3=1,nslab3)
            do i3=1,nslab3
               emergslab(i1,i2,i3)=log(emergslab(i1,i2,i3))
            end do
            do i4=1,nslab4
               read(lun,*) inclinslab(i4),(emerginclinslab(i1,i2,i3,i4),
     $              i3=1,nslab3)
               do i3=1,nslab3
                  emerginclinslab(i1,i2,i3,i4)=
     $                 log(emerginclinslab(i1,i2,i3,i4))
               end do
            end do
         end do
      end do
      close(lun)

      return
      end


c--------------------------------------------------------------------
      subroutine data_king_r(tauKing,albKing,asymKing,emergKing,istat)
      implicit none
      include 'peg_config.f'
      integer nK1,nK2,nK3
      parameter(nK1=12,nK2=9,nK3=11)
      real    tauKing(nK1),albKing(nK2),asymKing(nK3)
      real    emergKing(nK1,nK2,nK3)
      integer istat

      integer Lun,i1,i2,i3

      call file_open(PEG_ROOT//'data/external/King.dat',lun,istat)
      if(istat.ne.0) return
      do i1=1,nK1
         do i2=1,nK2
            do i3=1,nK3
               read(lun,*) tauKing(i1),albKing(i2),asymKing(i3),
     $              emergKing(i1,i2,i3)
               emergKing(i1,i2,i3)=log(emergKing(i1,i2,i3))
            end do
         end do
      end do
      close(lun)

      return
      end


c--------------------------------------------------------------------
      subroutine data_dust_r(Zext, frac, nlambdaext, lambdaext, tauext,
     $     albedoext, asymext, istat)
      implicit none
      include 'peg_config.f'
      integer nmaxlambdaext
      parameter (nmaxlambdaext=300)
      real    Zext(5),frac(5)
      integer nlambdaext
      real    lambdaext(nmaxlambdaext),tauext(nmaxlambdaext,2)
      real    albedoext(nmaxlambdaext,2),asymext(nmaxlambdaext,2)
      integer istat

      real         coeffext(5,2)
      integer      lun,i,j
      character*72 a

      call file_open(PEG_ROOT//'data/external/dust.dat',lun,istat)
      if(istat.ne.0) return

      read(lun,'(a)') a
      read(lun,'(a)') a
      do i=2,4
         read(lun,*) Zext(i),(coeffext(i,j),j=1,2)
      end do
      Zext(1)=0.
      coeffext(1,1)=coeffext(2,1)
      coeffext(1,2)=coeffext(2,2)
      Zext(5)=1.
      coeffext(5,1)=coeffext(4,1)
      coeffext(5,2)=coeffext(4,2)
      do i=1,5
         frac(i)=coeffext(i,1)/(coeffext(i,1)+coeffext(i,2))
      enddo
      read(lun,'(a)') a
      read(lun,*) nlambdaext
      do i=1,nlambdaext
         read(lun,*) lambdaext(i),(tauext(i,j),albedoext(i,j),
     $        asymext(i,j),j=1,2)
      end do
      close(lun)

      return
      end


c--------------------------------------------------------------------
      subroutine data_spitz_r(nSpitzer,taudustSpitzer,ySpitzer,istat)

      implicit none
      include 'peg_config.f'

      integer nSpitzer
      real    taudustSpitzer(20),ySpitzer(20)
      integer istat

      integer Lun,i

      call file_open(PEG_ROOT//'data/external/Spitzer.dat',lun,istat)
      read(lun,*) nSpitzer
      do i=1,nSpitzer
         read(lun,*) taudustSpitzer(i),ySpitzer(i)
      enddo
      close(lun)

      return
      end


c--------------------------------------------------------------------
      subroutine data_sfr_r(SFRparam, fileSFR, ntimesfile, timefile,
     $     SFRfile, Zfile, istat)
      implicit none

      real          SFRparam(*)
      character*(*) fileSFR
      integer       ntimesfile
      real          timefile(*),SFRfile(*),Zfile(*)
      integer       istat

      integer       Lun,error
      integer       str_length
      
      istat=0

      if (SFRparam(1).eq.-1) then
         call file_open(fileSFR,Lun,istat)
         if(istat.ne.0) return
         ntimesfile=0
         error=0
         do while (error.eq.0)
            read(Lun,*,iostat=error) timefile(ntimesfile+1),
     $           SFRfile(ntimesfile+1)
            if (error.eq.0) ntimesfile=ntimesfile+1
         end do
         close(Lun)
      else if (SFRparam(1).eq.-2) then
         call file_open(fileSFR,Lun,istat)
         if(istat.ne.0) return
         ntimesfile=0
         error=0
         do while (error.eq.0)
            read(Lun,*,iostat=error) timefile(ntimesfile+1),
     $           SFRfile(ntimesfile+1),Zfile(ntimesfile+1)
            if (error.eq.0) ntimesfile=ntimesfile+1
         end do
         close(Lun)
      endif

      return
      end

c------------------------------------------------------------------------------
c For SFRparam(1) lt 0, sfr_time reads the file "fileSFR" and resamples the
c SFR and  ZSFR (typeSFR=-2) at each time step (from t=0 to nmaxtimes-1)
c [note: there is no need to compute up to nmaxtimes, we should stop at
c  timeimpr(ntimesimpr), to pass as input arg ...]

      subroutine sfr_time(SFRparam,fileSFR,SFR,ZSFR,istat)

      implicit none
      include 'peg_include.f'

      real          SFRparam(*) ! input (do nothing if SFRparam(1) ge 0)
      character*(*) fileSFR     ! input

      real          SFR(*)      ! output
      real          ZSFR(*)     ! output
      integer       istat

      integer       ntimesfile
      real          timefile(nmaxtimes)
      real          SFRfile(nmaxtimes)
      real          Zfile(nmaxtimes)
      real          time
      integer       i,i0

***** Reading of the SFR (and possibly Z also) in fileSFR.         
c     Does nothing is typeSFR.ge.0.
      call data_sfr_r(SFRparam,fileSFR,ntimesfile,timefile,SFRfile,
     s     Zfile,istat)
      if(istat.ne.0) return

***** Interpolate SFR, and possibly ZSFR at the time sampling points
      if (SFRparam(1).le.-1) then

         i0=0
         do i=1,nmaxtimes-1            
            time=i-1.
*     
            call bracket(ntimesfile,timefile,time,i0)
            call Steffen(ntimesfile,timefile,SFRfile,.false.,
     $           time,SFR(i),i0)
            if (SFRparam(1).eq.-2) then
               call Steffen(ntimesfile,timefile,Zfile,.false.,
     $              time,ZSFR(i),i0)
            endif
*
         enddo
      endif

      return
      end


c--------------------------------------------------------------------
***** Calculations for the nebular continuum.
c Read the file "HII.dat". Compute and interpolate the nebular continuum "fneb"
c at the grid points "lambda". Read the fluxes in lines.

      subroutine data_neb_r(nlambda,lambda,fneb,
     s     nlines,lambdaline,fline,istat)

      implicit none
      include 'peg_config.f'

      integer nlambda           ! input: number of wave in output spec
      real    lambda(*)         ! input: wavelengths in output spec
      real    fneb(*)           ! out: neb continuum interpolated at lambda
      integer nlines            ! out: number of emission lines
      real    lambdaline(*)     ! out: wavelengths of the lines
      real    fline(*)          ! out: flux in lines

      integer istat             ! out: return status

      integer lun
      real    g1,g2,g3,g4,gam1(100),gam2(100),gam3(100),gam4(100)
      integer nlambdacont
      real    lambdacont(100)
      real    HeI,HeII,alphaTe,gline,c
      integer i,j
      parameter(c=2.99792458e18)

      real interplinlog         ! function in "pegase_func.f"

      call file_open(PEG_ROOT//'data/external/HII.dat',lun,istat)
      if(istat.ne.0) return

      read(lun,*) HeI,HeII,alphaTe
      read(lun,*) nlambdacont   
      do i=1,nlambdacont
         read(lun,*) lambdacont(i),gam1(i),gam2(i),gam3(i),gam4(i)
      end do      
      j=1
      do i=1,nlambda
         do while (((lambdacont(j)-lambda(i))
     $        *(lambdacont(j+1)-lambda(i)).gt.0.)
     $        .and.(j.lt.nlambdacont))
            j=j+1
         end do
         if ((lambdacont(j)-lambda(i))
     $        *(lambdacont(j+1)-lambda(i)).le.0.) then
            g1=interplinlog(1./lambdacont(j),1./lambdacont(j+1),
     $           gam1(j),gam1(j+1),1./lambda(i))
            g2=interplinlog(1./lambdacont(j),1./lambdacont(j+1),
     $           gam2(j),gam2(j+1),1./lambda(i))
            g3=interplinlog(1./lambdacont(j),1./lambdacont(j+1),
     $           gam3(j),gam3(j+1),1./lambda(i))
            g4=interplinlog(1./lambdacont(j),1./lambdacont(j+1),
     $           gam4(j),gam4(j+1),1./lambda(i))
         endif
*     -1.9: gaunt factor ~ nu**(-0.1) at long wavelength
         if (lambda(i).ge.lambdacont(nlambdacont)) then
            g1=gam1(nlambdacont)
     $           *(lambda(i)/lambdacont(nlambdacont))**0.1
            g2=gam2(nlambdacont)
     $           *(lambda(i)/lambdacont(nlambdacont))**0.1
            g3=gam3(nlambdacont)
     $           *(lambda(i)/lambdacont(nlambdacont))**0.1
            g4=gam4(nlambdacont)
     $           *(lambda(i)/lambdacont(nlambdacont))**0.1
         endif
         fneb(i)=1.d-40*(g1+g2+HeI*g3+HeII*g4)*c/alphaTe
     $        /lambda(i)**2
      enddo

***** Calculations for the nebular lines.

      read(lun,*) nlines
      do i=1,nlines
*     gline: line strength relative to Hbeta (first line).
         read(lun,*) lambdaline(i),gline
         fline(i)=1.244d-25*gline/alphaTe
      end do
      close(lun)


      return
      end
