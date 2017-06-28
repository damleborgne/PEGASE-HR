!----------------------------------------------------------------------
!     Written by D. Le Borgne, 2004
!     Measures the relative diffence between 2 fits files 
!     (outputs of spectra_HR)
!     If a relative difference > 1e-5 occurs once, this is mentionned on stdout
!     Otherwise, nothing is done nor shown.
!----------------------------------------------------------------------
      program compare_fits
      implicit none

      include 'peg_config.f'
      include 'peg_include.f'
      
      
      integer i,error,il1,nlambda1,nlambda2,il2,j,age,k
      integer ntimes1,ntimes2,Lfits1,Lfits2,istat
      integer nlines1,nlines2,nlick1,nlick2
      integer hdutype

      double precision Lambda1(nmaxlambda),Flux1(nmaxlambda)
      double precision Lambda2(nmaxlambda),Flux2(nmaxlambda)

      double precision Lambdalines1(nmaxlambda),Fluxlines1(nmaxlambda)
      double precision Lambdalines2(nmaxlambda),Fluxlines2(nmaxlambda)

      double precision lick1(nmaxlick),lick2(nmaxlick)
      character*200     nom_spec1,nom_spec2
      integer           nargs


      nargs=iargc()
      if (nargs.ne.2) then 
         write(*,*) 'Error : too few or too many arguments.'
         write(*,*) 'Usage : >compare_fits spec1.fits spec2.fits'
         stop
      endif
      
      if (nargs.eq.2) then 
         call getarg(1,nom_spec1)
         call getarg(2,nom_spec2)
      endif


      
      
*     open and reads nlambda,ntimes
      call fits_spec_open_rw(nom_spec1,
     &     Lfits1,nlambda1,nlines1,ntimes1,istat)
      if (istat.ne.0) then
         write(*,*) 'ERROR reading input file : istat=',istat
         stop
      endif
      call fits_spec_open_rw(nom_spec2,
     &     Lfits2,nlambda2,nlines2,ntimes2,istat)
      if (istat.ne.0) then
         write(*,*) 'ERROR reading input file : istat=',istat
         stop
      endif

      if (ntimes1.ne.ntimes2.or.
     $     nlines1.ne.nlines2.or.
     $     nlambda1.ne.nlambda2) then
         write (*,*) 'ntimes, or nlambda, or nlines differ.'
         stop
      endif
      
      do j=1,ntimes1
         call read_spectra_fits(Lfits1,j,Lambda1,nlambda1,Flux1,
     &        Lambdalines1,nlines1,Fluxlines1,nlick1,lick1)
         call read_spectra_fits(Lfits2,j,Lambda2,nlambda2,Flux2,
     &        Lambdalines2,nlines2,Fluxlines2,nlick2,lick2)

         do k=1,nlambda1
            if (Flux1(k).ne.0) then 
               if (abs((Flux1(k)-Flux2(k))/Flux1(k)).gt.1e-5) then 
                  write(*,*) 'relative diff = ',
     &                 (Flux1(k)-Flux2(k))/Flux1(k),' > 1e-5 !'
                  write(*,*) 'lambda=',Lambda1(k)
                  write(*,*) 'timestep #',j
                  write(*,*) 'f1,f2=',Flux1(k),Flux2(k)
                  call fits_spec_close(Lfits1,istat)
                  call fits_spec_close(Lfits2,istat)
                  stop
               endif
            else
               if (Flux2(k).ne.0) then
                  write(*,*) 'Flux1=0, Flux2=',Flux2(k),' at'
                  write(*,*) 'lambda=',Lambda1(k)
                  write(*,*) 'timestep #',j
                  write(*,*) 'f1,f2=',Flux1(k),Flux2(k)
                  call fits_spec_close(Lfits1,istat)
                  call fits_spec_close(Lfits2,istat)
                  stop                  
               endif
            endif
            
         enddo

                                ! compares lick indices if present in both files
         if (nlick1*nlick2.ne.0) then
            if (nlick1.ne.nlick2) then
               write(*,*) 'lick tables don''t have same size !'
                  call fits_spec_close(Lfits1,istat)
                  call fits_spec_close(Lfits2,istat)
                  stop                  
            endif
            do k=1,nlick1
               if (lick1(k).ne.0) then 
                  if (abs((lick1(k)-lick2(k))/lick1(k)).gt.1e-3) then 
                     write(*,*) 'relative diff = ',
     &                    (lick1(k)-lick2(k))/lick1(k),' > 1e-3 !'
                     write(*,*) 'ilick=',k
                     write(*,*) 'timestep #',j
                     write(*,*) 'lick1,lick2=',lick1(k),lick2(k)
                     call fits_spec_close(Lfits1,istat)
                     call fits_spec_close(Lfits2,istat)
                     stop
                  endif
               else
                  if (lick2(k).ne.0) then
                     write(*,*) 'lick1=0, lick2=',lick2(k),' at'
                     write(*,*) 'lambda=',Lambda1(k)
                     write(*,*) 'timestep #',j
                     call fits_spec_close(Lfits1,istat)
                     call fits_spec_close(Lfits2,istat)
                     stop                  
                  endif
               endif

            enddo
         endif
         
      enddo
      
      call fits_spec_close(Lfits1,istat)
      call fits_spec_close(Lfits2,istat)
      
      

      end

      
C     **************************************************
      subroutine fits_spec_open_rw(filename,Lfits,nw,nwl,nt,istat)
      implicit none
      character*(*) filename
      integer       Lfits
      integer       nw,nwl,nt
      integer       istat
      integer       blocksize,naxes(2),nfound

      istat=0
      call ftgiou(Lfits,istat)
      call ftopen(Lfits,filename,1,blocksize,istat) !rwmode=1?
      call ftgknj(Lfits,'NAXIS',1,2,naxes,nfound,istat)     !size of the image.
      if (nfound .ne. 2)then
         istat=1
         return
      endif
      nw=naxes(1)
      nt=naxes(2)

      istat=0
      call ftmnhd(Lfits, 2, 'ETS_LINES', 0, istat)
      if (istat.ne.0) return
      call ftgknj(Lfits,'NAXIS',1,2,naxes,nfound,istat)     !size of the image.
      if (nfound .ne. 2)then
         istat=1
         return
      endif
      nwl=naxes(2)



      end
C     **************************************************
      subroutine read_spectra_fits(Lfits,iage,Lambda,nlambda,
     &     Flux,Lambdalines,nlines,Fluxlines,nlick,licktab)
C     Reads a pegase FITS spectrum      

      implicit none
      include 'peg_include.f'

      character*200        header
      character*80         comment
      integer             iage,nlambda,istat,nlines
      double precision    Lambda(nmaxlambda),Lambdalines(nmaxlambda),
     &     Flux(nmaxlambda),Fluxlines(nmaxlambda)
      integer             Lfits,nlick,i,k,nt
      double precision    licktab(nmaxlick)
      double precision    lickcol(nmaxotimes)
      logical       anynulls
      logical       flagvals(nmaxotimes)

      nlick=0
     
      call fits_spec_wave_read(Lfits,nlambda,Lambda,istat) 
      if (istat.ne.0) then
         write(*,*)'Lick: could not get cnt wavelengths in ETS file'
         stop
      endif
      call fits_spec_cont_r(Lfits,nlambda,iage,Flux,istat)
      if (istat.ne.0) then
         write(*,*)'Lick: err2'
         stop
      endif

c     read lines 
      call fits_spec_wavl_read(Lfits,nlines,Lambdalines,istat)
      if (istat.ne.0) then
         write(*,*)'Lick: could not get line wavelengths in ETS file'
         write(*,*) 'istat=',istat
         stop
      endif
      call fits_spec_line_r(Lfits,nlines,iage,Fluxlines,istat)
      if (istat.ne.0) then
         write(*,*)'Lick: err3'
         stop
      endif


      call ftmnhd(Lfits, 2, 'ETS_LICK', 0, istat)
      if (istat.eq.0) then
      call FTGNCL(Lfits, nlick, istat)
      call FTGNRW(Lfits, nt, istat)
         if (istat.ne.0) then
            write(*,*) 'nbadd!',istat
            return
         endif
         do i=1,nlick
            do k=1,nmaxotimes
               lickcol(k)=0.
            enddo
            call FTGCFD(Lfits,i,1,1,nt,lickcol,flagvals,anynulls,istat)
            licktab(i)=lickcol(iage)
         enddo
      else 
         istat=0
      endif


      end
