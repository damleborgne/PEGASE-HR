!----------------------------------------------------------------------
!     Written by D. Le Borgne, 2004
!     Measures the relative diffence between 2 fits files 
!     (outputs of spectra_HR)
!     If a relative difference > 1e-5 occurs once, this is mentionned on stdout
!     Otherwise, nothing is done nor shown.
!----------------------------------------------------------------------
program compare_fits

  use types
  use constants
  use fits_spec_io

  implicit none

  integer nlambda1,nlambda2,j,k
  integer ntimes1,ntimes2,Lfits1,Lfits2,istat
  integer nlines1,nlines2,nlick1,nlick2

  double precision Lambda1(nmaxlambda),Flux1(nmaxlambda)
  double precision Lambda2(nmaxlambda),Flux2(nmaxlambda)

  double precision Lambdalines1(nmaxlambda),Fluxlines1(nmaxlambda)
  double precision Lambdalines2(nmaxlambda),Fluxlines2(nmaxlambda)

  double precision lick1(nmaxlick),lick2(nmaxlick)
  character(len=200) ::     nom_spec1,nom_spec2
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




  !     open and reads nlambda,ntimes
  call fits_spec_open_rw(nom_spec1,&
       Lfits1,nlambda1,nlines1,ntimes1,istat)
  if (istat.ne.0) then
     write(*,*) 'ERROR reading input file : istat=',istat
     stop
  endif
  call fits_spec_open_rw(nom_spec2,&
       Lfits2,nlambda2,nlines2,ntimes2,istat)
  if (istat.ne.0) then
     write(*,*) 'ERROR reading input file : istat=',istat
     stop
  endif

  if (ntimes1.ne.ntimes2.or.&
       nlines1.ne.nlines2.or.&
       nlambda1.ne.nlambda2) then
     write (*,*) 'ntimes, or nlambda, or nlines differ.'
     stop
  endif

  do j=1,ntimes1
     call read_spectra_fits(Lfits1,j,Lambda1,nlambda1,Flux1,&
          Lambdalines1,nlines1,Fluxlines1,nlick1,lick1)
     call read_spectra_fits(Lfits2,j,Lambda2,nlambda2,Flux2,&
          Lambdalines2,nlines2,Fluxlines2,nlick2,lick2)

     do k=1,nlambda1
        if (Flux1(k).ne.0) then 
           if (abs((Flux1(k)-Flux2(k))/Flux1(k)).gt.1e-5) then 
              write(*,*) 'relative diff = ',&
                   (Flux1(k)-Flux2(k))/Flux1(k),' > 1e-5 !'
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
                 write(*,*) 'relative diff = ',&
                      (lick1(k)-lick2(k))/lick1(k),' > 1e-3 !'
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



end program compare_fits


