MODULE fits_spec_io

  use constants
  use types
  use util

  !  fits_spec_io, IO to scenario file 
  !  Part of the Pegase package
  !------------------------------------------------------------------------------
  !
  !   The output spectra are written into a FITS file. Its format, ETS,
  !   Evolutive Template Spectra consists in a series of HDUs. The primary
  !   HDU contains the spectra (1st axis is wavelength direction, second is 
  !   time). If the wavelengths are evenly sampled, the World Coordinate System
  !   is set to the new FITS standard, otherwyse the wavelength of each pixel
  !   is stored in a separate HDU (table named ETS_WCA_CONT). Two other HDUs
  !   respectively contains the wavelength and fluxes in emission lines and
  !   the properties at different times (ETS_LINES and ETS_PARA).
  !
  !   Some dedicated keywords are written to help Pleinpot to interpret this
  !   format (for example, to allow a transparent resampling to wavelegnths).
  !   The format shoud be correctly understood by any FITS reader. 
  !
  !   The access to FITS files is made with the CFITSIO Library (Bill Pence, 
  !   GSFC) http://heasarc.gsfc.nasa.gov/docs/software/fitsio/fitsio.html
  !
  !------------------------------------------------------------------------------
  ! routine: spec_creat
  !      subroutine fits_spec_creat(< filename,nw,nt,
  !     s     nZ,header,fSNIa,
  !     s     Zgas,tinfall,Zinfall,infall,SFRparam,fileSFR,
  !     s     codeZ,ZSFR,fsub,twind,
  !     s     answerneb,codeext,inclin,
  !     s     > Lfits,istat)
  !
  !   filename C* input Name of the file
  !   nw       I  input Number of wavelength points
  !   nt       I  input Number of spectra (output ages)
  !
  !------------------------------------------------------------------------------
  ! routine fits_spec_cmt_w: wirte a comment into the header of the FITS file
  !      subroutine fits_spec_cmt_w(< Lfits,comment,> istat)
  !
  !
  !------------------------------------------------------------------------------
  ! routine fits_spec_wave_w Write the wavelength of the continuum spectra 
  !        subroutine fits_spec_wave_w(< Lfits,nlambda,lambda,> istat)
  !
  !------------------------------------------------------------------------------
  ! routine fits_spec_cont_w: write one spectrum (ie a line in primary HDU)
  !       subroutine fits_spec_cont_w(Lfits,itime,nlambda,flux,istat)
  !
  !------------------------------------------------------------------------------
  ! routine fits_spec_line_w: write emission lines at a given time
  !       subroutine fits_spec_line_w( <Lfits,itime,nlines,lambdaline,fline,
  !                                    > istat)
  !
  !------------------------------------------------------------------------------
  ! routine fits_spec_para_w: write the various properties at a given time
  !       subroutine fits_spec_para_w(< Lfits,time,
  !      s           Mgal,sigmastars,sigmaWD,sigmaBHNS,sigmasub,sigmagas,
  !      s           Zgas,Zstars,Zbol,fluxbol,tauV,fluxext,SFR,NLymtot,
  !      s           nSNIItot,nSNIatot,agestars,agebol,
  !      s           > istat)
  !
  !
  !------------------------------------------------------------------------------
  ! routine fits_spec_close: close the ETS file
  !        subroutine fits_spec_close(Lfits,istat)
  !------------------------------------------------------------------------------
  ! routines: fits_spec_open fits_spec_wave_read fits_spec_wavl_read
  !           fits_spec_cont_r fits_spec_line_r fits_spec_para_r
  !  WARNING: The read routines, used by the program "colors" are not exactly
  !    symetric to the write routines. The main difference is that in the latter
  !    the arguments are in double precision.
  !------------------------------------------------------------------------------
CONTAINS
  subroutine fits_spec_creat(myscen,nw,nt,&
       nZ,header,fSNIa,&
       filestellib_short,Lfits,&
       istat)

    implicit none


    TYPE(t_scenario), INTENT(IN) :: myscen
    integer       :: nw,nt
    integer       :: nZ
    character(len=*),dimension(:,:) :: header ! character*(*) header(4,nZ)
    real              :: fSNIa
    character(len=*)  :: filestellib_short
    integer       :: Lfits
    integer       :: istat

    integer       :: i,p
    character(len=120) :: comment
    character(len=40)  :: version
    integer       :: naxes(2)
    integer       :: nparam
    integer       :: lun

    !
    ! Create a FITS image, first HDU to host the spectra: 
    ! 1st direction is wavelength; 2nd direction is age.
    istat=0
    call ftgiou(Lfits,istat)  ! Get A free unit. Remember to free it with ftfiou !
    !Lfits=myscen%number
    !print*, myscen%filespectra, Lfits
    call ftinit(Lfits,trim(myscen%filespectra),1,istat) ! Create an empty file

    if (istat.ne.0) return
    naxes(1)=nw
    naxes(2)=nt
    call ftphpr(Lfits,.true.,-32,2,naxes,0,1,.true.,istat) ! FITS header
    call ftpkys(Lfits,'EXTNAME','ETS_CONTINUUM',' ',istat)
    call ftpkys(Lfits,'CTYPE1','WAVE','1st axis is wavelength',istat)
    call ftpkys(Lfits,'CTYPE2','AGE','2nd axis is age',istat)
    call ftpkys(Lfits,'BUNIT','LSUN/0.1nm','Unit of data',istat)
    call ftpkys(Lfits, 'H_CONTXT', 'ETS',     &
         'Evolutive template spectra', istat)


    comment='              PEGASE-HR'
    call ftpcom(Lfits,comment, istat)
    ! read version in VERSION file
    call file_open(trim(PEG_ROOT)//'/VERSION',lun,istat)
    if(istat.eq.0) then
       read(lun,'(a)') version
       call ftpkys(Lfits, 'VERSION', version,' ', istat)
       close(lun)
    endif

    call ftpkys(Lfits,'AUTHOR', 'PEGASEHR Team',' ', istat)
    call ftpkys(Lfits,'REFERENC','2004, A&A xxx, xxx',' ',istat)
    call ftpkys(Lfits,'REFCODE','2004A&A...xxx..xxx',' ',istat)
    call ftpkys(Lfits,'EMAIL','leborgne@iap.fr',' ',istat)


    !
    ! Copy the scenario information into COMMENT keywords
    comment =&
         '--------- DESCRIPTION OF THE SCENARIO --------------'
    call ftpcom(Lfits, comment, istat)
    istat=0

    comment ='Stellar library: '//trim(filestellib_short)
    call ftpcom(Lfits, comment, istat)
    istat=0

    call ftpcom(Lfits,header(3,1), istat)
    call ftpcom(Lfits,header(4,1), istat)

    do p=1,nZ
       do i=1,2
          call ftpcom(Lfits,header(i,p), istat)
       end do
    end do

    write(comment,'(a,e11.5)') 'Fraction of close binary systems: ',&
         fSNIa
    call ftpcom(Lfits, comment, istat)
    write(comment,'(a,e11.5)')'Initial metallicity: ',myscen%Zgasinit
    call ftpcom(Lfits, comment, istat)

    if(myscen%infall.eq.1) then
       write(comment,'(a)')'Infall'
       call ftpcom(Lfits, comment, istat)
       write(comment,'(a,e11.5)')'Infall timescale (Myr): ',myscen%tinfall
       call ftpcom(Lfits, comment, istat)
       write(comment,'(2a,e11.5)')'Metallicity of the ',&
            'infalling gas: ',myscen%Zinfall
       call ftpcom(Lfits, comment, istat)
    elseif(myscen%infall.eq.0) then
       write(comment,'(a)')'No infall '
       call ftpcom(Lfits, comment, istat)
    endif

    write(Comment,'(a,i3)')'Type of star formation: ',int(myscen%SFRparam(1))
    call ftpcom(Lfits, comment, istat)
    if (myscen%SFRparam(1).ge.1.and.myscen%SFRparam(1).le.3) then
       write(Comment,'(a,e11.5)')'p1: ',myscen%SFRparam(3)
       call ftpcom(Lfits, comment, istat)
       write(Comment,'(a,e11.5)')'p2: ',myscen%SFRparam(4)
       call ftpcom(Lfits, comment, istat)
    endif

    if (myscen%SFRparam(1).ge.10) then
       nparam=int(myscen%SFRparam(2))
       write(Comment,'(a,i2)')'Number of parameters:  ',nparam
       call ftpcom(Lfits, comment, istat)
       do i=1,nparam
          write(Comment,'(a,i2,a,e11.5)')' ',i,': ',myscen%SFRparam(2+i)
          call ftpcom(Lfits, comment, istat)
       end do
    end if
    if (myscen%SFRparam(1).le.-1) then
       write(Comment,'(2a)')'SFR file:',myscen%fileSFR(:str_length(myscen%fileSFR))
       call ftpcom(Lfits, comment, istat)
    end if

    if (myscen%SFRparam(1).ge.-1) then
       if(myscen%codeZ.eq.0) then
          write(Comment,'(a)')&
               'No evolution of the stellar metallicity'
          call ftpcom(Lfits, comment, istat)
          write(Comment,'(a,e11.5)')'Stellar metallicity: ', myscen%ZSFRinit
          call ftpcom(Lfits, comment, istat)
       else
          write(Comment,'(2a)')'Consistent evolution of the ',&
               'stellar metallicity'
          call ftpcom(Lfits, comment, istat)
       endif

    endif

    write(Comment,'(2a,e11.5)')'Mass fraction of substellar ',&
         'objects: ',myscen%fsub
    call ftpcom(Lfits, comment, istat)

    if(myscen%twind .lt.0.or.myscen%twind.gt.nmaxtimes) then
       write(Comment,'(a)')'No galactic winds'
       call ftpcom(Lfits, comment, istat)
    else
       write(Comment,'(a)')'Galactic winds'
       call ftpcom(Lfits, comment, istat)
       write(Comment,'(a,e11.5)')'Age of the galactic winds: ',&
            myscen%twind
       call ftpcom(Lfits, comment, istat)
    endif

    write(Comment,'(a)') myscen%answerneb
    call ftpcom(Lfits, comment, istat)
    if (myscen%codeext.eq.0) then
       write(Comment,'(a)')'No extinction '
       call ftpcom(Lfits, comment, istat)
    elseif(myscen%codeext.eq.1) then
       write(Comment,'(a)')'Extinction for a spheroidal geometry '
       call ftpcom(Lfits, comment, istat)
    elseif(myscen%codeext.eq.2) then
       write(Comment,'(a,a)')'Extinction for a disk geometry:  ',&
            'inclination-averaged'
       call ftpcom(Lfits, comment, istat)
    elseif(myscen%codeext.eq.3) then
       write(Comment,'(a,a)')'Extinction for a disk geometry: ',&
            'specific inclination'
       call ftpcom(Lfits, comment, istat)
       write(Comment,'(a,e11.5)')'Inclination: ',myscen%inclin
       call ftpcom(Lfits, comment, istat)
    endif

    comment =&
         '---------- DESCRIPTION OF THE FILE STRUCTURE  -------------'
    call ftpcom(Lfits, comment, istat)

    comment='This file consists in 3 or 4 Header Data Units'
    call ftpcom(Lfits, comment, istat)
    comment =&
         'The primary HDU contains the spectra, '//&
         'first axis is the wavelength'
    call ftpcom(Lfits, comment, istat)
    comment='direction and the second axis is the Age'
    call ftpcom(Lfits, comment, istat)
    comment = &
         'The HDU named ETS_LINES lists the emission lines and their'
    call ftpcom(Lfits, comment, istat)
    comment='equivalent width'
    call ftpcom(Lfits, comment, istat)
    comment =&
         'The HDU named ETS_PARA gives '//&
         'the characteristics of each spectrum'
    call ftpcom(Lfits, comment, istat)
    comment='Age in Myr, total mas in stars...'
    call ftpcom(Lfits, comment, istat)
    comment = &
         'If the wavelength sampling is not linear, '// &
         'a fourth HDU, named '
    call ftpcom(Lfits, comment, istat)
    comment =  &
         'If present, ETS_CONT_WCA contains the '//&
         '"dispersion relation", '
    call ftpcom(Lfits, comment, istat)
    comment='which is the list of '//&
         'wavelengths of each pixels'
    call ftpcom(Lfits, comment, istat)
    comment =&
         '--------------------------------------------------'
    call ftpcom(Lfits, comment, istat)


    return
  end subroutine fits_spec_creat

  !------------------------------------------------------------------------------
  subroutine fits_spec_cmt_w(Lfits,comment,istat)
    implicit none
    integer       Lfits
    character(len=*) :: comment
    integer       istat

    istat=0
    call ftpcom(Lfits, comment, istat)
    return
  end subroutine fits_spec_cmt_w

  !------------------------------------------------------------------------------
  ! Write the wavelength of the continuum spectra 
  subroutine fits_spec_wave_w(Lfits,nlambda,lambda,istat)

    implicit none
    integer Lfits
    integer nlambda
    real,dimension(:) ::    lambda
    integer istat

    real(dp) ::  step
    integer flag
    integer k
    integer hdutype

    ! First we search if the wavenlength points are linearly spaced
    step=(lambda(nlambda)-lambda(1))/(nlambda-1)

    k=2
    flag=0
    do while(k.le.nlambda.and.flag.eq.0)
       if(abs(lambda(k)-lambda(k-1)-step).gt.1e-3) then
          flag=1
       endif
       k=k+1
    enddo

    istat=0
    call ftmahd(Lfits,1,hdutype,istat)

    if(flag.eq.0) then
       !         print *,'linear sampling'
       istat=0
       call ftukys(Lfits, 'CTYPE1', 'WAVE-WAV',&
            '1st axis is wavelength', istat)
       call ftukys(Lfits,'CUNIT1','0.1 nm','Unit for axis 1',istat)
       call ftukye(Lfits,'CRPIX1',1.,-9,'Reference pixel',istat)
       call ftukye(Lfits,'CRVAL1',lambda(1),-9,&
            '[0.1nm] Value at reference',istat)
       call ftukyd(Lfits,'CDELT1',step,-9,'Step',istat)
       call ftukyd(Lfits,'CD1_1',step,-9,'Step',istat)
       call ftpkye(Lfits, 'H_WRESOL', 0.55, -9,&
            '[0.1nm] FWHM wavelength resolution', istat)
    else
       !         print *,'must write the WCA'
       istat=0
       call ftukys(Lfits, 'H_WCALIB', '[ETS_CONT_WCA]',&
            'Pointer to WCA', istat)

       call ftmnhd(Lfits, 2, 'ETS_CONT_WCA', 0, istat)
       if(istat.eq.0) then    ! HDU exists, we delete it
          call ftdhdu(Lfits, hdutype, istat)
       endif

       istat=0
       call ftibin(Lfits,nlambda,1,&
            'BFIT','1D','0.1nm','ETS_CONT_WCA',0, istat)

       call ftpcne(Lfits, 1, 1, 1, nlambda, lambda, -999., istat)
       call ftpcom(Lfits, &
            'List of wavelengths corresponding '//    &
            'to 1st axis of 1st HDU, in 0.1nm',istat)
       call ftukyj(Lfits,'H_WCATYP',102,'Type of WCA', istat)
       call ftukyj(Lfits,'BFIT5',1,'1D WCA relation', istat)
       call ftukyj(Lfits,'BFIT6',nlambda,&
            'Number of wavelength points', istat)

    endif

    return
  end subroutine fits_spec_wave_w

  !------------------------------------------------------------------------------
  subroutine fits_spec_cont_w(Lfits,itime,nlambda,flux,istat)
    implicit none

    integer,intent(in) :: Lfits
    integer,intent(in) :: itime
    integer,intent(in) :: nlambda
    real,dimension(:), intent(in) :: flux
    integer  :: istat


    real,dimension(stellibinfo%nlambda) :: flux_real
    real, parameter  :: PLPrnoval=-1.e36
    integer :: hdutype
    integer :: felem

    call ftmahd(Lfits,1,hdutype,istat)

    felem=1+(itime-1)*nlambda

    flux_real=real(flux)
    call ftppne(Lfits,1, felem, nlambda, flux_real, PLPrnoval, istat)

    return
  end subroutine fits_spec_cont_w

  !------------------------------------------------------------------------------
  subroutine fits_spec_line_w(Lfits, itime, nlines, lambdaline, fline, istat)

    implicit none

    integer,intent(in) :: Lfits,itime,nlines
    real,dimension(:),intent(in) ::    fline,lambdaline
    integer  :: istat

    character(len=16) ::  ttype(2)
    character(len=16) ::  tform(2)
    character(len=16) ::  tunit(2)
    integer           :: hdutype
    integer           :: ntimes
    character(len=80) :: comm
    integer           :: i

    istat=0
    call ftmnhd(Lfits, 2, 'ETS_LINES', 0, istat)
    if(istat.ne.0) then       ! HDU does not exists, we create
       istat=0
       call ftmahd(Lfits,1,hdutype,istat)
       call ftgkyj(Lfits,'NAXIS2',ntimes,comm, istat)

       ttype(1)='WAVE'
       tform(1)='1D'
       tunit(1)='0.1nm'
       ttype(2)='FLUXLINE'
       write(tform(2),'(i4,a)') ntimes,'E'
       tunit(2)='??'
       call ftibin(Lfits,nlines,2,ttype,tform,tunit,'ETS_LINES',0, istat)
       call ftpcne(Lfits, 1, 1, 1, nlines, 1.*lambdaline, -999., istat)
    endif

    do i=1,nlines
       call ftpcle(Lfits,2,i,itime,1,1.*fline(i), istat)
    enddo

    return
  end subroutine fits_spec_line_w

  !------------------------------------------------------------------------------
  subroutine fits_spec_para_w(Lfits,time,&
       Zbol,fluxbol,tauV,fluxext,NLymtot,&
       nSNIItot,nSNIatot,agebol,&
       istat)

    implicit none

    integer,intent(in) :: Lfits,time
!    real,dimension(:),intent(in) ::    Mgal,sigmastars,sigmaWD,&
!         sigmaBHNS,sigmagas,sigmasub,Zgas,Zstars,SFR,agestars
    real(DP), intent(in) :: NLymtot
    real,intent(in)      :: Zbol,fluxbol,nSNIItot,fluxext,tauV,nSNIatot,agebol
    integer istat

    character(len=16) :: ttype(19)
    character(len=16) :: tform(19)
    character(len=16) :: tunit(19)
    integer           :: hdutype
    integer           :: ntimes
    character(len=80) :: comm
    integer, SAVE     :: frow


    istat=0
    call ftmnhd(Lfits, 2, 'ETS_PARA', 0, istat)

    if(istat.ne.0) then       ! HDU does not exists, we create
       istat=0
       call ftmahd(Lfits,1,hdutype,istat)
       call ftgkyj(Lfits,'NAXIS2',ntimes,comm, istat)

       ttype(1)='AGE'
       tform(1)='1E'
       tunit(1)='Myr'

       ttype(2)='MGAL'
       tform(2)='1E'
       tunit(2)='SUN '

       ttype(3)='Mstars'
       tform(3)='1E'
       tunit(3)='SUN'

       ttype(4)='MWD'
       tform(4)='1E'
       tunit(4)='SUN'

       ttype(5)='MBHNS'
       tform(5)='1E'
       tunit(5)='SUN'

       ttype(6)='Msub'
       tform(6)='1E'
       tunit(6)='SUN'

       ttype(7)='sigmagas'
       tform(7)='1E'
       tunit(7)=' '

       ttype(8)='Zgas'
       tform(8)='1E'
       tunit(8)='(mass fraction)'

       ttype(9)='Zstars'
       tform(9)='1E'
       tunit(9)='(mass fraction)'

       ttype(10)='Zbol'
       tform(10)='1E'
       tunit(10)='(mass fraction)'

       ttype(11)='fluxbol'
       tform(11)='1E'
       tunit(11)='SUN'
       ! Note: flux bol is in Solar unit, not erg/s, to avoid overflow

       ttype(12)='tauV'
       tform(12)='1E'
       tunit(12)=' '

       ttype(13)='fluxext'
       tform(13)='1E'
       tunit(13)=' '

       ttype(14)='SFR'
       tform(14)='1E'
       tunit(14)='SUN/Myr '

       ttype(15)='NLymtot'
       tform(15)='1E'
       tunit(15)='erg**-1'
       ! Note: do not multiply NLymtot by Lsol, to avoid overflow

       ttype(16)='nSNIItot'
       tform(16)='1E'
       tunit(16)='Myr**-1'

       ttype(17)='nSNIatot'
       tform(17)='1E'
       tunit(17)='Myr**-1'

       ttype(18)='agestars'
       tform(18)='1E'
       tunit(18)='Myr'

       ttype(19)='agebol'
       tform(19)='1E'
       tunit(19)='Myr'

       call ftibin(Lfits,ntimes,19,ttype,tform,tunit,'ETS_PARA',0, istat)

       frow=1
    endif

    call ftpcnj(Lfits, 1, frow, 1, 1, time-1, -999, istat)
    call ftpcne(Lfits, 2, frow, 1, 1, real(galprop%Mgal(time)), -999., istat)
    call ftpcne(Lfits, 3, frow, 1, 1, real(galprop%sigmastars(time)), -999., istat)
    call ftpcne(Lfits, 4, frow, 1, 1, real(galprop%sigmaWD(time)), -999., istat)
    call ftpcne(Lfits, 5, frow, 1, 1, real(galprop%sigmaBHNS(time)), -999., istat)
    call ftpcne(Lfits, 6, frow, 1, 1, real(galprop%sigmasub(time)), -999., istat)
    call ftpcne(Lfits, 7, frow, 1, 1, real(galprop%sigmagas(time)), -999., istat)
    call ftpcne(Lfits, 8, frow, 1, 1, real(galprop%Zgas(time)), -999., istat)
    call ftpcne(Lfits, 9, frow, 1, 1, real(galprop%Zstars(time)), -999., istat)
    call ftpcne(Lfits,10, frow, 1, 1, real(Zbol), -999., istat)
                                      
    call ftpcne(Lfits,11, frow, 1, 1, real(fluxbol), -999., istat)
    call ftpcne(Lfits,12, frow, 1, 1, real(tauV), -999., istat)
    call ftpcne(Lfits,13, frow, 1, 1, real(fluxext), -999., istat)
    call ftpcne(Lfits,14, frow, 1, 1, real(galprop%SFR(time)), -999., istat)
    call ftpcnd(Lfits,15, frow, 1, 1, NLymtot, -999.d0, istat)
    call ftpcne(Lfits,16, frow, 1, 1, real(nSNIItot), -999., istat)
    call ftpcne(Lfits,17, frow, 1, 1, real(nSNIatot), -999., istat)
    call ftpcne(Lfits,18, frow, 1, 1, real(galprop%agestars(time)), -999., istat)
    call ftpcne(Lfits,19, frow, 1, 1, real(agebol), -999., istat)

    frow=frow+1              

    return
  end subroutine fits_spec_para_w

  !------------------------------------------------------------------------------
  subroutine fits_spec_close(Lfits,istat)
    implicit none
    integer Lfits,istat

    istat=0
    call ftclos(Lfits,istat)
    call ftfiou(Lfits,istat)
    if(istat.eq.0) Lfits=-1

    return
  end subroutine fits_spec_close
  !------------------------------------------------------------------------------
  subroutine fits_spec_open(filename,Lfits,nw,nt,istat)
    implicit none
    character(len=*) :: filename
    integer       Lfits
    integer       nw,nt
    integer       istat

    integer       blocksize,naxes(2),nfound

    istat=0
    call ftgiou(Lfits,istat)
    !call file_unit(Lfits)
    call ftopen(Lfits,filename,0,blocksize,istat)

    call ftgknj(Lfits,'NAXIS',1,2,naxes,nfound,istat)     !size of the image.
    if (nfound .ne. 2)then
       istat=1
       return
    endif

    nw=naxes(1)
    nt=naxes(2)

    return
  end subroutine fits_spec_open

  !------------------------------------------------------------------------------
  subroutine fits_spec_wave_read(Lfits,nw,lambda,istat)
    implicit none
    integer Lfits
    integer nw
    real(dp),dimension(:),intent(out) ::  lambda
    !real,dimension(nmaxlambda) ::  lambda_real
    integer istat

    integer       hdutype
    character(len=120) ::  keyval,comment
    logical  :: anynulls
    real(dp)     :: cr,cv,cd
    integer  :: k

    istat=0
    keyval = ' '
    comment = ' '
    call ftmahd(Lfits,1,hdutype,istat)
    !print*, 'istat=',istat

    call ftgkys(Lfits,'CTYPE1',keyval,comment,istat)
    !print*, 'istat=',istat
    if (keyval(:4).ne.'WAVE') then
       istat=123
       return
    endif

    if (keyval(:8).eq.'WAVE-WAV') then ! Linear sampling of Wavelength
      !print*, 'here1'
       call ftgkyd(Lfits,'CRPIX1',cr,comment,istat)
       !    print*, 'istat=',istat, cr
       call ftgkyd(Lfits,'CRVAL1',cv,comment,istat)
       !    print*, 'istat=',istat, cv
       call ftgkyd(Lfits,'CD1_1',cd,comment,istat)
       !    print*, 'istat=',istat, cd, comment
       if(istat.ne.0) return

       do k=1,nw
          !lambda_real(k)=cv + (k-cr) * cd
          lambda(k)=cv + (k-cr) * cd
       enddo

    else
      !print*, 'here2'
       call ftgkys(Lfits,'H_WCALIB',keyval,comment,istat)
      ! print*, 'istat=',istat
       if (istat.ne.0) return
       if(keyval(1:1).eq.'[')keyval=keyval(2:)
       k=2
       do while(istat.eq.0)
          if(k.gt.len(keyval)) then
             istat=1
          else
             if (keyval(k:k).eq.']') then
                keyval=keyval(:k-1)
                istat=2
             endif
          endif
          k=k+1
       enddo
       istat=0
       call ftmnhd(Lfits, 2, keyval, 0, istat)

       if(istat.ne.0) return
       call ftgcno(Lfits,0,'BFIT',k,istat)
       call ftgcvd(Lfits,k,1,1,nw,-999.d0,lambda,anynulls,istat)
    endif

    return
  end subroutine fits_spec_wave_read
  !------------------------------------------------------------------------------
  subroutine fits_spec_wavl_read(Lfits,nlines,lambdaline,istat)

    implicit none

    integer,intent(in)  :: Lfits
    integer,intent(out)  :: nlines
    integer :: nlines_read
    real(dp),DIMENSION(:), INTENT(out) ::  lambdaline
    integer istat,a
    character(len=240) :: comment
    character(len=10) :: keyword

    real,DIMENSION(nmaxlines) ::  lambdaline_real

    integer :: ncol
    logical :: anynulls

    istat=0
    keyword = 'ETS_LINES'
    call ftmnhd(Lfits, 2, keyword, 0, istat)
    if (istat.ne.0) return

    comment = ' '
    keyword = 'NAXIS2'
    call ftgkyj(Lfits, keyword, nlines_read, comment, istat)   ! BUGS HERE !!!!!
    nlines = nlines_read
    if (istat.ne.0) return

    keyword = 'WAVE'
    call ftgcno(Lfits,0,keyword,ncol,istat)

    call ftgcvd(Lfits, ncol, 1, 1, nlines, -999.d0, lambdaline, anynulls, istat)

    !lambdaline=1d0*lambdaline_real

    return
  end subroutine fits_spec_wavl_read
  !------------------------------------------------------------------------------

  subroutine fits_spec_cont_r(Lfits,nlambda,itime,flux,istat)
    implicit none

    integer,intent(in) :: Lfits
    integer nlambda
    integer itime
    real(dp),dimension(:) ::  flux
    integer istat

    integer hdutype
    integer felem
    logical anynull

    istat=0

    call ftmahd(Lfits,1,hdutype,istat)
    felem=(itime-1)*nlambda+1
    call ftgpvd(Lfits,1,felem,nlambda,1.d-25,flux,anynull,istat)

    return
  end subroutine fits_spec_cont_r
  !------------------------------------------------------------------------------

  subroutine fits_spec_line_r(Lfits,nlines,itime,fluxline,istat)

    implicit none

    integer, intent(in) ::  Lfits, nlines, itime
    real(dp),DIMENSION(:)  :: fluxline
    real(dp) :: one_real 
 
    integer istat

    integer ncol,ncoltrue
    logical anynulls
    integer k

    istat=0
    call ftmnhd(Lfits, 2, 'ETS_LINES', 0, istat)
    if (istat.ne.0) then
      print*, 'error',istat
      return
    endif

    call ftgcno(Lfits,0,'FLUXLINE',ncol,istat)
    if (istat.ne.0) then
      print*, 'error2',istat
      return
    endif
    !ncol=ncoltrue
    do k=1,nlines
       call ftgcvd(Lfits, ncol, k, itime, 1, -999.d0, one_real, anynulls, istat)
       if (istat.ne.0) then
        print*, 'error3',Lfits, ncol, k,itime, 1, -999., one_real, anynulls, istat
        return
       endif

       fluxline(k)=one_real
    enddo

    return
  end subroutine fits_spec_line_r
  !------------------------------------------------------------------------------

  subroutine fits_spec_para_r(Lfits,itime,&
       time,&
       Mgal,sigmastars,sigmaWD,sigmaBHNS,sigmasub,sigmagas,&
       Zgas,Zstars,Zbol,fluxbol,tauV,fluxext,SFR,NLymtot,&
       nSNIItot,nSNIatot,agestars,agebol,&
       istat)

    implicit none
    integer :: Lfits
    integer :: itime
    integer :: time
    real(dp) ::  Mgal
    real(dp) ::  sigmastars
    real(dp) ::  sigmaWD
    real(dp) ::  sigmaBHNS
    real(dp) ::  sigmagas,sigmasub
    real(dp) ::  Zgas,Zstars,Zbol
    real(dp) ::  fluxbol,nSNIItot,fluxext
    real(dp) ::  tauV
    real(dp) ::  SFR
    real(dp) ::  NLymtot
    real(dp) ::  nSNIatot,agestars,agebol

    integer istat

    integer ncol
    logical anynulls

    istat=0
    call ftmnhd(Lfits, 2, 'ETS_PARA', 0, istat)
    if (istat.ne.0) return

    call ftgcno(Lfits,0,'AGE',ncol,istat)
    call ftgcvj(Lfits,ncol,itime,1,1,-999,time,anynulls,istat)

    call ftgcno(Lfits,0,'MGAL',ncol,istat)
    call ftgcvd(Lfits,ncol,itime,1,1,-999.d0,Mgal,anynulls,istat)

    call ftgcno(Lfits,0,'Mstars',ncol,istat)
    call ftgcvd(Lfits, ncol, itime, 1, 1, -999.d0, sigmastars,&
         anynulls, istat)

    call ftgcno(Lfits,0,'MWD',ncol,istat)
    call ftgcvd(Lfits,ncol,itime,1,1,-999.d0,sigmaWD,anynulls,istat)

    call ftgcno(Lfits,0,'MBHNS',ncol,istat)
    call ftgcvd(Lfits,ncol,itime,1,1,-999.d0,sigmaBHNS,anynulls,istat)

    call ftgcno(Lfits,0,'Msub',ncol,istat)
    call ftgcvd(Lfits,ncol,itime,1,1,-999.d0,sigmasub,anynulls,istat)

    call ftgcno(Lfits,0,'sigmagas',ncol,istat)
    call ftgcvd(Lfits,ncol,itime,1,1,-999.d0,sigmagas,anynulls,istat)

    call ftgcno(Lfits,0,'Zgas',ncol,istat)
    call ftgcvd(Lfits,ncol,itime,1,1,-999.d0,Zgas,anynulls,istat)

    call ftgcno(Lfits,0,'Zstars',ncol,istat)
    call ftgcvd(Lfits,ncol,itime,1,1,-999.d0,Zstars,anynulls,istat)

    call ftgcno(Lfits,0,'Zbol',ncol,istat)
    call ftgcvd(Lfits,ncol,itime,1,1,-999.d0,Zbol,anynulls,istat)

    call ftgcno(Lfits,0,'fluxbol',ncol,istat)
    call ftgcvd(Lfits,ncol,itime,1,1,-999.d0,fluxbol,anynulls,istat)

    call ftgcno(Lfits,0,'tauV',ncol,istat)
    call ftgcvd(Lfits,ncol,itime,1,1,-999.d0,tauV,anynulls,istat)

    call ftgcno(Lfits,0,'fluxext',ncol,istat)
    call ftgcvd(Lfits,ncol,itime,1,1,-999.d0,fluxext,anynulls,istat)

    call ftgcno(Lfits,0,'SFR',ncol,istat)
    call ftgcvd(Lfits,ncol,itime,1,1,-999.d0,SFR,anynulls,istat)

    call ftgcno(Lfits,0,'NLymtot',ncol,istat)
    call ftgcvd(Lfits,ncol,itime,1,1,-999.d0,NLymtot,anynulls,istat)

    call ftgcno(Lfits,0,'nSNIItot',ncol,istat)
    call ftgcvd(Lfits,ncol,itime,1,1,-999.d0,nSNIItot,anynulls,istat)

    call ftgcno(Lfits,0,'nSNIatot',ncol,istat)
    call ftgcvd(Lfits,ncol,itime,1,1,-999.d0,nSNIatot,anynulls,istat)

    call ftgcno(Lfits,0,'agestars',ncol,istat)
    call ftgcvd(Lfits,ncol,itime,1,1,-999.d0,agestars,anynulls,istat)

    call ftgcno(Lfits,0,'agebol',ncol,istat)
    call ftgcvd(Lfits,ncol,itime,1,1,-999.d0,agebol,anynulls,istat)

    return
  end subroutine fits_spec_para_r
  !------------------------------------------------------------------------------

  !     **************************************************

  subroutine fits_spec_open_rw(filename,Lfits,nw,nwl,nt,istat)

    implicit none

    character(len=*) :: filename
    integer       Lfits
    integer       nw,nwl,nt
    integer       istat
    integer       blocksize,naxes(2),nfound

    istat=0
    call ftgiou(Lfits,istat)
    !call file_unit(Lfits)
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



  end subroutine fits_spec_open_rw

  !     **************************************************

  subroutine read_spectra_fits(Lfits,iage,Lambda,nlambda,&
       Flux,Lambdalines,nlines,Fluxlines,nlick,licktab)
    !     Reads a pegase FITS spectrum      

    use types
    use constants

    implicit none

    integer                :: iage,nlambda,istat,nlines
    real(dp), dimension(:) :: Lambda,Lambdalines,Flux,Fluxlines
    integer                :: Lfits,nlick,i,nt
    real(dp),dimension(:)  :: licktab
    logical                :: anynulls
    logical,dimension(nmaxotimes) :: flagvals
    real(dp),dimension(nmaxotimes)    :: lickcol

    nlick=0

    call fits_spec_wave_read(Lfits,nlambda,Lambda,istat) 
    if (istat.ne.0) then
       write(*,*)'Lick: could not get cnt wavelengths in ETS file : issue in fits_spec_wave_read', istat, Lfits
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
       write(*,*)'Lick: could not get line wavelengths in ETS file : issue in fits_spec_wavl_read this time...'
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
          lickcol=0.
          call FTGCFD(Lfits,i,1,1,nt,lickcol,flagvals,anynulls,istat)
          licktab(i)=lickcol(iage)
       enddo
    else 
       istat=0
    endif

  end subroutine read_spectra_fits

  !******************************************************************************************

  subroutine read_spectra_fits_lick(Lfits,iage,Lambda,nlambda,&
       Flux,Lambdalines,nlines,Fluxlines)
    !     Reads a pegase FITS spectrum      

    use types
    use constants

    implicit none

    integer             iage,nlambda,istat,nlines
    REAL(DP), dimension(:), intent(out) ::   Lambda, Flux
    REAL(DP), dimension(:), intent(out) :: Lambdalines,Fluxlines
    integer, intent(in)            :: Lfits

    !write(*,*) 'LFITS=',Lfits
    call fits_spec_wave_read(Lfits,nlambda,Lambda,istat) 
    !write(*,*) 'LFITS=',Lfits
    if (istat.ne.0) then
       write(*,*)'Lick: could not get cnt wavelengths in ETS file : issue in fits_spec_wave_read for a change'
       stop
    endif
    call fits_spec_cont_r(Lfits,nlambda,iage,Flux,istat)
    !write(*,*) 'LFITS=',Lfits
    if (istat.ne.0) then
       write(*,*)'Lick: err2'
       stop
    endif

    !     read lines 
    !write(*,*) 'GO', nlines, Lambdalines
    call fits_spec_wavl_read(Lfits,nlines,Lambdalines,istat)
    ! write(*,*) 'LFITS=',Lfits
    if (istat.ne.0) then
       write(*,*)'Lick: could not get line wavelengths in ETS file'
       write(*,*) 'istat=',istat
       stop
    endif
    call fits_spec_line_r(Lfits,nlines,iage,Fluxlines,istat)
    if (istat.ne.0) then
       write(*,*)'Lick: err3 bis', istat
       stop
    endif
  end subroutine read_spectra_fits_lick

END MODULE fits_spec_io
