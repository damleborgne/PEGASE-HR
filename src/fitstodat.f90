program fitstodat

  use nrtype

  implicit none

  integer       nt
  character(280)  filein,fileout
  integer        unitout,blocksize
  integer        i,istat,Lfits,ii

  !      filein='test.fits'
  !      fileout='test.dat'

  unitout=11
  call getarg(1, filein)
  call getarg(2, fileout)
  write(*,*) 'input:',filein
  write(*,*) 'output:',fileout
  open(unitout,file=fileout,status='replace',iostat=istat)

  ii=0

  ii=ii+1

  call ftgiou(Lfits,istat)  ! Get A free unit
  
  call ftopen(Lfits,filein,0,blocksize,istat)

  call rw_spectra_fitsheader(Lfits,unitout,nt)

  call rw_lambdadef(Lfits,unitout)
  call rw_linesdef(Lfits,unitout)

  do i=1,nt
     call rw_spectra_fitsparams(Lfits,i,unitout)
     call rw_spectra_flux(Lfits,i,unitout)
     call rw_spectra_lines(Lfits,i,unitout)
  enddo

  close(unitout)
  call ftclos(Lfits,istat)
  call ftfiou(Lfits,istat)  ! Free the unit

end program fitstodat

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subroutine rw_spectra_fitsheader(Lfits,unitout,nt)
  implicit none

  integer       nw,nt,nl,unitout
  character(len=80) :: header(200)
  character(len=280) :: comment
  integer       nlh,nlhtot
  integer       istat,Lfits
  integer       nfound,naxes(2)
  integer       istart,iend,i
  integer        hdutype

  istat=0


  call FTMAHD(Lfits, 1,hdutype, istat)
  call ftgknj(Lfits,'NAXIS',1,2,naxes,nfound,istat)


  if (nfound .ne. 2) then
     istat=1
     return
  endif
  nw=naxes(1)
  nt=naxes(2)

  call FTGREC(Lfits,0, comment,istat)


  istart=0
  iend=0
  nlhtot=400
  nlh=0
  do i=1,nlhtot
     call ftgcrd(Lfits,'COMMEN?',comment,istat)
     if (index(comment,'FILE STRUCTURE').ne.0) then 
        iend=1
     endif
     if (istat.eq.0.and.istart.eq.1.and.iend.eq.0) then
        nlh=nlh+1
        header(nlh)=comment(9:len(comment))
     endif
     if (index(comment,'DESCRIPTION OF THE SCENARIO').ne.0) &
          then
        istart=1
     endif
  enddo

  istat=0

  call FTMNHD(Lfits, 2,'ETS_LINES', 0, istat)
  !      call FTGREC(Lfits,0,comment,istat)
  call ftgkyj(Lfits,'NAXIS2',nl,comment,istat)


  do i=1,nlh
     write(unitout,'(a80)') header(i)
  enddo
  write(unitout,'(60a1)')  ('*',i=1,60)

  write(unitout,*) nt,nw,nl

end subroutine rw_spectra_fitsheader

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

subroutine rw_lambdadef(Lfits,unitout)

  use constants

  implicit none

  integer       istat,Lfits,i,nl
  character(80) comm
  integer       unitout
  double precision dlambda
  double precision lambdaz
  integer       nlambda,hdutype
  logical       anyf
  real(DP) ::        lambdaLCB(nmaxlambdaLCB)

  istat=0

  call FTMAHD(Lfits, 1,hdutype, istat)
  call FTGREC(Lfits,0,comm,istat)
  call ftgkyd(Lfits,'CRVAL1',lambdaz,comm,istat)
  if (istat.eq.0) then 
     !     elodie type
     call FTGREC(Lfits,0,comm,istat)
     call ftgkyd(Lfits,'CDELT1',dlambda,comm,istat)
     call FTGREC(Lfits,0,comm,istat)
     call ftgkyj(Lfits,'NAXIS1',nlambda,comm,istat)
     write(unitout,'(5f10.1)') (lambdaz+dlambda*i,i=0,nlambda-1)
  else
     istat=0
     call FTMNHD(Lfits, 2,'ETS_CONT_WCA', 0, istat)
     call ftgkyj(Lfits,'NAXIS2',nl,comm,istat)
     anyf=.true.
     call FTGCVd(Lfits,1,1,1,nl,0,lambdaLCB,anyf,istat)   
     write(unitout,'(5f10.1)') (lambdaLCB(i),i=1,nl)         
  endif
end subroutine rw_lambdadef
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

subroutine rw_linesdef(Lfits,unitout)

  use constants


  implicit none

  integer       istat,Lfits,i
  character(80) comm
  integer       unitout
  real(DP) ::        lambda(nmaxlines)
  integer       nlambda
  logical       anyf

  istat=0
  call FTMNHD(Lfits, 2,'ETS_LINES', 0, istat)

  call FTGREC(Lfits,0,comm,istat)
  call ftgkyj(Lfits,'NAXIS2',nlambda,comm,istat)

  call FTGCVd(Lfits,1,1,1,nlambda,0, lambda,anyf,istat)   

  write(unitout,'(5f10.1)') (lambda(i),i=1,nlambda)

end subroutine rw_linesdef
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subroutine rw_spectra_lines(Lfits,it,unitout)

  use constants

  implicit none

  integer        Lfits,it,i
  integer        istat,unitout,nlambda,ncol
  character(80)  comm
  real           Lsol
  parameter(Lsol=3.826e33)
  real           flux(nmaxlines)
  logical        anyf

  istat=0
  call FTMNHD(Lfits, 2,'ETS_LINES', 0, istat)

  call FTGREC(Lfits,0,comm,istat)
  call ftgkyj(Lfits,'NAXIS2',nlambda,comm,istat)

  call ftgcno(Lfits,0,'FLUXLINE',ncol,istat)

  !     colnum,frow,felem,nelements
  do i=1,nlambda
     call FTGCVe(Lfits,ncol,i,it,1,1e-25, flux(i),anyf,istat)   
  enddo

  write(unitout,'(5(e9.4,1x))') (Lsol*flux(i),i=1,nlambda)

end subroutine rw_spectra_lines


!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subroutine rw_spectra_fitsparams(Lfits,it,unitout)

  use constants
  
  implicit none

  integer        nt
  integer        istat,unitout,Lfits,i,it,npara
  character(80)  comm
  real(DP) ::         params(nmaxparams),column(nmaxotimes)
  logical        anyf

  istat=0

  call FTMNHD(Lfits, 2,'ETS_PARA', 0, istat)

  call FTGREC(Lfits,0,comm,istat)
  call ftgkyj(Lfits,'TFIELDS',npara,comm,istat)
  call FTGREC(Lfits,0,comm,istat)
  call ftgkyj(Lfits,'NAXIS2',nt,comm,istat)

  anyf=.true.
  do i=1,npara
     call FTGCVd(Lfits,i,1,1,nt,0,column,anyf,istat)   
     params(i)=column(it)
  enddo


  write(unitout,'(i6,9(1x,e9.3))') int(params(1)),(params(i),i=2,10)
  write(unitout,'(6x,9(1x,e9.3))') (params(i),i=11,npara)


end subroutine rw_spectra_fitsparams



!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subroutine rw_spectra_flux(Lfits,it,unitout)

  use constants

  implicit none

  integer        hdutype
  integer        istat,unitout,it,nl,Lfits,i
  character(80)  comm
  logical        anyf
  real           Lsol
  real           flux(nmaxlambda)
  parameter(Lsol=3.826e33)

  istat=0

  call FTMAHD(Lfits, 1,hdutype, istat)
  call ftgkyj(Lfits,'NAXIS1',nl,comm,istat)

  call ftgpve(Lfits,1,(it-1)*nl+1,nl,1.e-25,flux,anyf,istat)


  do i=1,nl
     flux(i)=flux(i)*Lsol
  enddo

  write(unitout,'(5(e9.4,1x))') (flux(i),i=1,nl)

end subroutine rw_spectra_flux
