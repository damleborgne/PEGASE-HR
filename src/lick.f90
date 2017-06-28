!----------------------------------------------------------------------
!     Written by D. Le Borgne, 2002
!     computes Lick indices (described in the file index_table.dat)
!     in a fits PEGASE-HR spectrum, and write them in the HDU 'ETS_LICK'
!     (or replace it if the HDU already exists)
!----------------------------------------------------------------------
program lick

  use types
  use constants
  use fits_spec_io
  use util

  implicit none

  integer error,nlambda,j,ntimes,Lfits,istat,k,nindex
  integer nlines
  character(len=10) :: iindices(nmaxlick),uniindice
  double precision Lambda(nmaxlambda),Flux(nmaxlambda)
  double precision Lambdalines(nmaxlambda),Fluxlines(nmaxlambda)
  double precision unindex,Flux_res(nmaxlambda),lesindex(nmaxlick)
  character(len=280) ::     nom_spectra_in,nom_ind
  integer           nargs,unitindex,indicesunits(nmaxlick)
  character(len=80) ::      chFWHM
  double precision  FWHM,delta
  double precision Fluxout(nmaxlambda)
  double precision resol

  nom_spectra_in=' '

  nargs=iargc()
  if (nargs.ge.3.or.nargs.eq.1) then 
     write(*,*) 'Error : too few or too many arguments.'
     write(*,*) 'Usage : >lick spectrum.fits FWMH '//&
             '(command line mode)'
     write(*,*) '     or >lick                    '//&
             '(interactive mode)'
     write(*,*) 'FHWM is the width of the emission lines '//&
             'to include in the spectrum. Enter 0. not to '//&
             'use the emission lines in the measure of the '//&
             'lick indices.'
     stop
  endif
  if (nargs.eq.2) then 
     call getarg(1,nom_spectra_in)
     call getarg(2,chFWHM)
     read(chFWHM,*) FWHM
  endif

  do while (nom_spectra_in.ne.'end')

     !     if interactive mode, ask for file(s)
     if (nargs.eq.0) then 
        write (*,'(a)') 'input fits file (end to stop): '
        read (*,'(a)') nom_spectra_in
     endif

     if (nom_spectra_in.ne.'end') then

        if (nargs.eq.0) then 
           write (*,'(a)') 'FWHM (in Angstrom) of the emission '&
                         //'lines if you want to include them in the measure'&
                         //' of the indices (0. for no emission line) :'
           read (*,*) FWHM
        endif

        nom_ind=trim(PEG_ROOT)//'/data/user_defined/index_table.dat'
        open (30,file=nom_ind,status='old')
        read (30,'(i5)') nindex
        close (30)

        !     open and reads nlambda,ntimes
        call fits_spec_open_rw_lick(nom_spectra_in,&
                   Lfits,nlambda,nlines,ntimes,resol,istat)
        if (istat.ne.0) then
           write(*,*) 'ERROR reading input file : istat=',istat
        endif

        !     check that it is a high resolution spectrum
        if (abs(resol-0.55).ge.1e-3) then
           write(*,*) 'Error : the resolution of the spectra'&
                         //' is too low to measure Lick indices'
           istat=1
        endif
        if (istat.eq.0) then
           write(*,*) ' Now computing indices'
           write(*,*) ' New HDU created (ETS_LICK)'

           do j=1,ntimes
              print*, 'timestep = ',j,'/',ntimes
              call read_spectra_fits_lick(Lfits,j,Lambda,nlambda,Flux,&
                               Lambdalines,nlines,Fluxlines)

              delta=Lambda(int(nlambda/2)+1)-Lambda(int(nlambda/2))

              do k=1,nlambda
                 Fluxout(k)=Flux(k)
              enddo
              if (FWHM.gt.3.*delta) then
                 call addlines(nlambda,Lambda,Flux,nlines,&
                                     Lambdalines,&
                                     Fluxlines,FWHM,Fluxout)
              elseif (j.eq.1) then

                 write (*,*) 'Warning : the FWHM of the emission '&
                                     //'lines is too '&
                                     //'narrow compared to the sampling of '&
                                     //'the spectrum.'&
                                     //' Lines are not included.'&
                                     //' FWHM is set to zero.'
                 FWHM=0.
              endif

              call reduce_res_lick(Lambda,nlambda,Fluxout,Flux_res)

              do k=1,nindex
                 error=0
                 call Lickindex(Lambda,nlambda,Flux_res,&
                                     nom_ind,k,uniindice,unitindex,unindex,error)
                 iindices(k)=uniindice
                 indicesunits(k)=unitindex
                 if (error.ne.0) then
                    write(*,*) 'index',k,&
                                           ' : error computing index.'
                    write(*,*) 'It could be because index'//&
                                           '  definitions are out of the spectrum,'&
                                           //' or because the flux is zero.'
                    write(*,*) 'error=',error
                    lesindex(k)=-99.
                 else 
                    lesindex(k)=unindex
                 endif
              enddo
              call fits_lick_w(Lfits,j,ntimes,lesindex,&
                               iindices,indicesunits,nindex,FWHM)
           enddo
           call fits_spec_close(Lfits,istat)
        endif

        !     ending if command line argument
        if (nargs.eq.2) then 
           nom_spectra_in='end'
        endif

     endif
  enddo
end program lick
!     ##################################################################
subroutine addlines(nlambda,Lambda,Flux,nlines,Lambdalines,&
       Fluxlines,FWHM,Fluxout)

  use constants

  implicit none  


  integer                nlambda,nlines
  double precision       Flux(nmaxlambda),Fluxlines(nmaxlambda)
  double precision       Lambda(nmaxlambda),Lambdalines(nmaxlambda)
  double precision       FWHM,sigma,delta
  double precision       Fluxout(nmaxlambda),gaussi
  integer                i,j,k,ileft,icenter,iright

  do i=1,nlambda
     Fluxout(i)=Flux(i)
  enddo

  delta=Lambda(int(nlambda/2)+1)-Lambda(int(nlambda/2))

  if (FWHM.gt.3.*delta) then
     sigma=FWHM/2.3548
     do j=1,nlines
        if (Lambdalines(j).gt.Lambda(1).and.&
                      Lambdalines(j).lt.Lambda(nlambda)) then
           !     find first point in Lambda just after Lambdalines(j)
           icenter=1
           do while (Lambda(icenter).lt.Lambdalines(j)) 
              icenter=icenter+1
           enddo
           !     find first point for the sum of the gaussian
           ileft=1
           do while (Lambda(ileft).lt.Lambdalines(j)-3.*FWHM) 
              ileft=ileft+1
           enddo
           !     find last point for the sum of the gaussian
           iright=1
           do while (Lambda(iright).lt.Lambdalines(j)+3.*FWHM) 
              iright=iright+1
           enddo
           !     do the sum            
           if (ileft.le.0) ileft=1
           if (iright.gt.nlambda) iright=nlambda
           do k=ileft,iright-1
              delta=Lambda(k+1)-Lambda(k)
              gaussi=Fluxlines(j)*&
                              exp(-(((Lambda(k)-Lambda(icenter))/sigma)**2.)/2.)&
                               /sqrt(2*3.145926)/sigma
              Fluxout(k)=Flux(k)+gaussi
           enddo
        endif
     enddo
  endif
end subroutine addlines

!     ##################################################################



!     **************************************************
subroutine fits_spec_open_rw_lick(filename,Lfits,nw,nwl,nt,resol,istat)

  use util
  use fits_spec_io

  implicit none

  character(len=*) :: filename
  integer       Lfits
  integer       nw,nwl,nt
  integer       istat
  integer       blocksize,naxes(2),nfound
  double precision resol
  character(len=80) ::  comment

  istat=0
  !call ftgiou(Lfits,istat)
  call file_unit(Lfits)
  call ftopen(Lfits,filename,1,blocksize,istat) !rwmode=1?
  call ftgknj(Lfits,'NAXIS',1,2,naxes,nfound,istat)     !size of the image.
  if (nfound .ne. 2)then
     istat=1
     return
  endif
  nw=naxes(1)
  nt=naxes(2)

  if (istat.eq.0) then
     call ftgkyd(Lfits,'H_WRESOL',resol,comment,istat)
     if (istat.ne.0) then 
        resol=20.
        istat=0
     endif
  endif

  if (istat.eq.0) then
     call ftmnhd(Lfits, 2, 'ETS_LINES', 0, istat)
     if (istat.ne.0) return
     call ftgknj(Lfits,'NAXIS',1,2,naxes,nfound,istat) !size of the image.
     if (nfound .ne. 2)then
        istat=1
        return
     endif
     nwl=naxes(2)
  endif


end subroutine fits_spec_open_rw_lick
!     **************************************************
subroutine fits_lick_w(Lfits,itime,ntimes,indices,iindices,&
     indicesunits,nindices,FWHM)

  use types
  use constants
  use util
  use fits_spec_io

  implicit none


  integer       itime,ntimes,nindices,istat,Lfits,hdutype,i
  character(len=16) ::   ttype(nmaxlick),untype
  character(len=16) ::   tform(nmaxlick)
  character(len=16) ::   tunit(nmaxlick)
  double precision  ::  indices(nmaxlick)
  real(DP) ::          unindice
  character(len=*) :: iindices(nmaxlick)
  integer       indicesunits(nmaxlick)
  double precision FWHM

  istat=0
  !     move to the HDU called ETS_LICK
  call ftmnhd(Lfits, 2, 'ETS_LICK', 0, istat) 

  !     first call : destroy LICK HDU if present     
  !                   create LICK HDU               
  if (itime.eq.1) then      
     if (istat.eq.0) then   
        call FTDHDU(Lfits,hdutype,istat)
        if (istat.ne.0) then
           write(*,*) 'Error in lick.f : couldn''t delete Lick HDU'
        endif
     endif

     istat=0
     do i=1,nindices
        untype=iindices(i)
        ttype(i)=untype     !nom du champ
        tform(i)='1E'       !format
        if (indicesunits(i).eq.0) then
           tunit(i)='Angstrom' !unite
        else
           tunit(i)='mag'   !unite
        endif
     enddo
     call ftmahd(Lfits,3,hdutype,istat) ! move to  HDU #3
     call ftibin(Lfits,ntimes,nindices,ttype,tform,tunit,&
          'ETS_LICK',0, istat) !create binary extension
     call ftpkyf(Lfits,'FWHM',FWHM,2,'FWHM (in 0.1nm)'&
          //' used for the emission lines',istat)
     if (istat.ne.0) write(*,*) 'ERREUR : istat=',istat
  endif

  !     write indices for step itime
  do i=1,nindices
     unindice=indices(i)
     call ftpcne(Lfits, i,itime, 1, 1,unindice,-999., istat)
     if (istat.ne.0) write(*,*) 'istat!',istat
  enddo


end subroutine fits_lick_w

!     **************************************************
subroutine reduce_res_lick(Lambda,nlambda,Flux,Flux_red)

  use types
  use constants

  implicit none


  integer             nlambda,jres,nres,i,j
  double precision    Lambda(nmaxlambda),Flux(nmaxlambda)
  double precision    Flux_red(nmaxlambda)
  double precision    maxsigma,delta,lambda0,fluxj
  double precision    lick_res(5),lambda_lick_res(5)
  double precision    ma_resolution(nmaxlambda),sigma
  integer             jmin,jmax

  lambda_lick_res(1)=4000.
  lambda_lick_res(2)=4400.
  lambda_lick_res(3)=4900.
  lambda_lick_res(4)=5400.
  lambda_lick_res(5)=6000.
  lick_res(1)=11.5          !=FWHM (A)
  lick_res(2)=9.2
  lick_res(3)=8.4
  lick_res(4)=8.4
  lick_res(5)=9.8
  nres=5

  do i=1,nlambda
     jres=1
     if (lambda_lick_res(nres).le.Lambda(i)) then
        jres=nres-1
     else
        if (lambda_lick_res(1).ge.Lambda(i)) then
           jres=1
        else
           do while(lambda_lick_res(jres).le.Lambda(i))
              jres=jres+1
           enddo
           jres=jres-1
        endif
     endif

     ma_resolution(i)=lick_res(jres)+&
             (lick_res(jres+1)-lick_res(jres))*&
             (Lambda(i)-lambda_lick_res(jres))/&
             (lambda_lick_res(jres+1)-lambda_lick_res(jres))
  enddo
  ! ---------------------------------------------------------------------
  !     Convolution by g=1/sqrt(2*pi*sigma^2) * exp(-r^2/(2*sigma^2))
  !      we have int(g) = 1.0
  do i=1,nlambda
     Flux_red(i)=Flux(i)
  enddo
  do i=1,nlambda
     sigma=sqrt(ma_resolution(i)**2.-0.55**2.)/2.35482 ! 2.35=2*sqrt(2*Ln(2))
     Flux_red(i)=0d0
     maxsigma=3d0*sigma
     if (sigma/10.d0.le.0.2d0) then
        delta=sigma/10.d0   ! echantillonnage = min (0.2 A, sigma/10)
        lambda0=Lambda(i)-maxsigma
        if (i-100*(i/100).eq.0) write(*,*) 'L,nadd',i,&
                   int((maxsigma/delta))
        do j=-int((maxsigma/delta)),int((maxsigma/delta))
           call interp_lambda_flux(Lambda,nlambda,&
                         Flux,Lambda(i)+delta*j,fluxj)
           Flux_red(i)=Flux_red(i)+fluxj*delta&
                         *exp(-((delta*j/sigma)**2.)/2d0)
        enddo
     else !cas de la librairie ELODIE : on degrade la resolution
        delta=0.2d0
        lambda0=Lambda(i)-maxsigma
        jmin=max(1,i-int((maxsigma/delta)))
        jmax=min(nlambda,i+int((maxsigma/delta)))
        do j=jmin,jmax
           Flux_red(i)=Flux_red(i)+Flux(j)*delta&
                *exp(-((delta*(i-j)/sigma)**2.)/2d0)
        enddo
     endif

     Flux_red(i)=Flux_red(i)/sigma/sqrt(2.d0*3.1415926d0)
  enddo
end subroutine reduce_res_lick

!     **************************************************
! function min(a,b)
!   integer          a,b,min
! 
!   if (a.lt.b) then 
!      min=a
!   else
!      min=b
!   endif
!   return
! end function min
! 
! !     **************************************************
! function max(a,b)
!   integer          a,b,max
! 
!   if (a.gt.b) then 
!      max=a
!   else
!      max=b
!   endif
!   return
! end function max
! 
!     **************************************************
subroutine interp_lambda_flux(Lambda,nlambda,&
       Flux,lambda0,fluxout)
  !     computes the flux interpolated at lambda0

  use types
  use constants

  implicit none


  integer        nlambda,i
  double precision    Lambda(nmaxlambda),Flux(nmaxlambda),fluxout
  double precision    lambda0

  i=1
  if (Lambda(nlambda).le.lambda0) then
     i=nlambda-1
  else
     if (Lambda(1).ge.lambda0) then
        i=1
     else
        do while(Lambda(i).le.lambda0)
           i=i+1
        enddo
        i=i-1
     endif
  endif
  fluxout=Flux(i)+(lambda0-Lambda(i))*&
       (Flux(i+1)-Flux(i))/(Lambda(i+1)-Lambda(i))
end subroutine interp_lambda_flux

!     **************************************************
subroutine read_spectra_fits_lick(Lfits,iage,Lambda,nlambda,&
     Flux,Lambdalines,nlines,Fluxlines)
  !     Reads a pegase FITS spectrum      

  use types
  use constants
  use fits_spec_io

  implicit none


  integer             iage,nlambda,istat,nlines
  double precision    Lambda(nmaxlambda),Lambdalines(nmaxlambda),&
       Flux(nmaxlambda),Fluxlines(nmaxlambda)
  integer             Lfits

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

  !     read lines 
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
end subroutine read_spectra_fits_lick
!     **************************************************
subroutine read_spectra_ascii(fichier,iage,Lambda,nlambda,Flux)
  !     Reads a pegase ASCII spectrum      

  use types
  use constants

  implicit none


  character(len=280) ::        fichier,header
  integer             iage,nlambda,ntimes,nlines,i,j
  double precision    Lambda(nmaxlambda),Lambdalines(nmaxlambda),&
       Flux(nmaxlambda),Fluxlines(nmaxlambda)


  open(20,status='old',file=fichier)     
  !     Description of the scenario
  read(20,'(a)') header 
  write(50,'(a)') header
  do while (header(1:10).ne.'**********')
     read(20,'(a)') header 
     write(50,'(a)') header
  end do
  read(20,*) ntimes,nlambda,nlines      
  read(20,*) (Lambda(i),i=1,nlambda)
  read(20,*) (Lambdalines(i),i=1,nlines)
  do j=1,iage
     read(20,'(a)') header
     read(20,'(a)') header
     read(20,*) (Flux(i),i=1,nlambda)
     read(20,*) (Fluxlines(i),i=1,nlines)
  enddo
  close(20)
end subroutine read_spectra_ascii


!     **************************************************
subroutine Lickindex(Lambda,nlambda,Flux,fichier_ind,number,&
       iindex,unit,unindex,error)
  !     Computes the Lick index of line # 'number' described in the file
  !     'fichier_ind'.

  use types
  use constants

  implicit none


  double precision Lambda(nmaxlambda),Flux(nmaxlambda),unindex
  integer          number,nlambda,numindex,iread,error,unit
  character(len=280) ::     fichier_ind,header
  character(len=*) ::     iindex
  double precision lb1,lb2,lr1,lr2,lp1,lp2,lbm,lrm,fbm,frm

  !     Reading the index file ----------------------------------------
  open(10,file=fichier_ind,status='old')
  read(10,*) numindex

  read(10,'(a)') header
  if (number.gt.numindex) then 
     error=3
     write(*,*) 'index not found in',fichier_ind
  else
     do iread=1,number 
        read(10,'(6x,f8.3,1x,f8.3,2x,f8.3,1x,'//&
                   'f8.3,2x,f8.3,1x,f8.3,1x,i1,2x,a10)')&
                   lp1,lp2,lb1,lb2,lr1,lr2,unit,iindex
     enddo
     close(10)
  endif


  !     computes the mean blue flux-------------------------------
  if (error.eq.0) then
     call fluxmean(Lambda,nlambda,Flux,lb1,lb2,lbm,fbm,error)
  endif
  !     computes the mean red flux--------------------------------
  if (error.eq.0) then
     call fluxmean(Lambda,nlambda,Flux,lr1,lr2,lrm,frm,error)
  endif
  !     computes the flux through passband------------------------
  if (error.eq.0) then
     call flux_passband(Lambda,nlambda,Flux,lbm,lrm,fbm,frm,&
             lp1,lp2,unit,unindex,error)
  endif
end subroutine Lickindex
!     **************************************************
subroutine flux_passband(Lambda,nlambda,Flux,lbm,lrm,fbm,frm,&
       lp1,lp2,unit,fp,error)
  !     computes flux through passband defined by wavelengths (lp1,lp2)
  !     continuum is defined by mean blue and red fluxes (fbm,frm)
  !     and by mean blue and red wavelengths (lbm,lrm)
  !     Output is fp (flux through pass band) = \int_lp1^lp2 {1-F/Fc}


  use types
  use constants

  implicit none

  double precision Lambda(nmaxlambda),Flux(nmaxlambda)
  double precision lbm,lrm,fbm,frm,lp1,lp2,fp,slope
  integer          nlambda,error,ilp1,ilp2,i,unit

  double precision Flux_red(nmaxlambda),Lambda_red(nmaxlambda)
  double precision continuum_red(nmaxlambda),Flux_norm(nmaxlambda)
  integer          nlambda_red

  fp=0.d0
  if (error.eq.0) then
     call indice_inf(Lambda,nlambda,lp1,ilp1,error)
  endif
  if (error.eq.0) then
     call indice_sup(Lambda,nlambda,lp2,ilp2,error)
  endif

  if (error.eq.0) then
     nlambda_red=ilp2-ilp1+1
     slope=(frm-fbm)/(lrm-lbm)
     do i=ilp1,ilp2
        Lambda_red(i-ilp1+1)=Lambda(i)
        Flux_red(i-ilp1+1)=Flux(i)
        continuum_red(i-ilp1+1)=fbm+slope*(Lambda(i)-lbm)
        if (continuum_red(i-ilp1+1).eq.0.) then
           error=4
           !               write(*,*) 'error : continuum=0!'
           !               write(*,*)  'slope=',slope
           !               write(*,*)  'fbm,frm=',fbm,frm
        endif
     enddo
  endif

  if (error.eq.0) then
     do i=1,nlambda_red
        if (unit.eq.0) then 
           !     if unit=Angstrom : EW
           Flux_norm(i)=1.-Flux_red(i)/continuum_red(i)
        else
           !     else : magnitude
           Flux_norm(i)=Flux_red(i)/continuum_red(i)
        endif
     enddo

     call myinteg(Lambda_red,nlambda_red,Flux_norm,lp1,lp2,fp,error)
     if (unit.eq.1) then
        fp=-2.5*log10(fp/(lp2-lp1))
     endif
  endif
end subroutine flux_passband

!     **************************************************
subroutine fluxmean(Lambda,nlambda,Flux,l1,l2,lm,fm,error)

  !     computes mean flux between l1 (lambda1) and l2 (lambda2). 
  !     Output is fm (fluxmean) and lm (lambda mean)



  use types
  use constants

  implicit none

  double precision Lambda(nmaxlambda),Flux(nmaxlambda),fm,lm
  integer          nlambda,error
  double precision l1,l2

  fm=0.
  lm=0.
  call myinteg(Lambda,nlambda,Flux,l1,l2,fm,error)

  if (error.eq.0) then
     fm=fm/(l2-l1)
     lm=(l1+l2)/2.d0
  endif
end subroutine fluxmean
!     ##################################################
subroutine myinteg(Lambda,nlambda,Flux,l1,l2,Fint,error)
  !     integrates Flux(Lambda) between l1 and l2, including 
  !     fractions of pixels


  use types
  use constants

  implicit none

  double precision Lambda(nmaxlambda),Flux(nmaxlambda),Fint
  integer          nlambda,error,i
  double precision l1,l2,Fl,f1,f2,ratio
  integer          il1,il2

  Fint=0d0
  if (error.eq.0) then
     call indice_inf(Lambda,nlambda,l1,il1,error)
  endif
  if (error.eq.0) then
     call indice_sup(Lambda,nlambda,l2,il2,error)
  endif
  !     now, Lambda(il1)<= l1 and l2<=Lambda(il2)  

  if (il1.ge.il2)  then 
     write (*,*) 'WARNING in myinteg: il1 >= il2 !!!!',il1,il2
     error=2
  endif
  if (error.eq.0) then

     if (il1.eq.il2-1) then
        !     integration inside a pixel only
        ratio=(l1-Lambda(il1))/(Lambda(il2)-Lambda(il1))
        f1=ratio*Flux(il2)+(1.-ratio)*Flux(il1)
        ratio=(l2-Lambda(il1))/(Lambda(il2)-Lambda(il1))
        f2=ratio*Flux(il2)+(1.-ratio)*Flux(il1)
        Fint=Fint+(f1+f2)/2.*(l2-l1)
     else
        !     integration over more than one pixel
        if (Lambda(il1).ne.l1) then
           !     there is a non-entire pixel on the blue part
           ratio=(Lambda(il1+1)-l1)/(Lambda(il1+1)-Lambda(il1))
           Fl=ratio*Flux(il1)+(1.-ratio)*Flux(il1+1)
           Fint=Fint+(Fl+Flux(il1+1))/2.*(Lambda(il1+1)-l1)
           il1=il1+1
        endif
        if (Lambda(il2).ne.l2) then
           !     there is a non-entire pixel on the red part
           ratio=(l2-Lambda(il2-1))/(Lambda(il2)-Lambda(il2-1))
           Fl=ratio*Flux(il2)+(1.-ratio)*Flux(il2-1)
           Fint=Fint+(Fl+Flux(il2-1))/2.*(l2-Lambda(il2-1))
           il2=il2-1
        endif
        !     now there are only entire pixels
        do i=il1,il2-1
           Fint=Fint+(Flux(i)+Flux(i+1))/2.*(Lambda(i+1)-Lambda(i))
        enddo
     endif


  endif

end subroutine myinteg

! ##################################################
subroutine indice_inf(Lambda,nlambda,l1,il1,error)


  use types
  use constants

  implicit none

  double precision Lambda(nmaxlambda)
  integer          nlambda,error
  double precision l1  !input
  integer          il1 !output : Lambda(il1) le l1

  il1=1
  if ((Lambda(1).gt.l1).or.(Lambda(nlambda).lt.l1)) then 
     error=1
     il1=0
  endif
  if (error.eq.0) then
     do while ((il1.lt.nlambda).and.(Lambda(il1).le.l1)) 
        il1=il1+1
     enddo
     if (Lambda(nlambda).ne.l1)  il1=il1-1
  endif
end subroutine indice_inf

! ##################################################
subroutine indice_sup(Lambda,nlambda,l2,il2,error)


  use types
  use constants

  implicit none
  double precision Lambda(nmaxlambda)
  integer          nlambda,error
  double precision l2  !input
  integer          il2 !output : l2 le Lambda(il2)

  il2=1
  if ((Lambda(1).gt.l2).or.(Lambda(nlambda).lt.l2)) then 
     error=1
     il2=0
  endif
  if (error.eq.0) then
     do while (Lambda(il2).lt.l2)
        il2=il2+1
     enddo
  endif
end subroutine indice_sup

