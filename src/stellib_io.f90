MODULE stellib_io


  use types
  use constants
  use util

CONTAINS
  !DOC ...................................................................
  !    Nom du programme  stellib_io
  !    
  !    Applicration       Access to the stellar libraries
  !
  !    Author            Philippe Prugniel  2001/07
  !
  !    Purpose
  !      This collection of routines read and write information from/to
  !      "stellib" FITS files and can also read the PEGASE2 ascii format
  !      for these files.
  !
  !    Description
  !      subroutine stell_open(filename, > Lfits,grid_type,grid_type,nw,nz,nspectot,istat)
  !          Open a "stellib" FITS file, return a pointer "Lfits" to be used for
  !          any further transaction
  !      subroutine stell_close(Lfits,istat)
  !          Disconnect the FITS file, reset Lfits to -1
  !      subroutine stell_para_r(Lfits, > Teff,Gspec,P1,P2,P3,istat)
  !          Read the "parameters" of all spectra in the file
  !      subroutine stell_nz_r(< Lfits, > nspec,Z,istat)
  !          Read the metallicity for each metallicity bin and the index of the
  !          first spectra in the corresponding bins.
  !      subroutine stell_wave_r(Lfits,nw,wave,istat)
  !          Read the wavelengths
  !      subroutine stell_creat(filename,grid_type,nw,nz,nspectot, > Lfits,istat)
  !          Create a "stellib" FITS file
  !      subroutine stell_wcl_w(Lfits,wstart,wstep, > istat)
  !          Write the wavelength sampling, when it is linear
  !      subroutine stell_wave_w(Lfits,nw,wave, > istat)
  !          Write the table of wavelength points
  !      subroutine stell_para_w(Lfits,nz,nspec,zspec,tspec,gspec,p1,p2,p3, 
  !                              > istat)
  !          Write the parameters of each spectra
  !      subroutine stell_flux_w(Lfits,nw,iz,nspec,flux, > istat)
  !          Write the series of spectra for metallicity iz
  !      subroutine stell_ascii_open(filename, > Lun,istat)
  !          Open an ASCII format "stellib" file 
  !      subroutine stell_ascii_hr(Lun, > nz,nw,nspectot,nspec,zspec,Tspec, 
  !                                gspec,p1,p2,p3,wave,istat)
  !          Read the first part (header) of ascii "stellib"
  !      subroutine stell_ascii_dr(Lun,nw,nspectot, > flux,istat)
  !          Read fluxes in ascii "stellib"
  !
  !      The access to FITS files is made with the CFITSIO Library (Bill Pence, 
  !      GSFC) http://heasarc.gsfc.nasa.gov/docs/software/fitsio/fitsio.html
  !
  !    Arguments
  !        General conventions: Input arguments first, I/O, outpu last
  !                             Last argument, istat, is 0 if no error occured
  !     filename  C*                 Name of a "stellib file" 
  !     Lfits     I                  Pointer to a FITS stream
  !     nw        I                  Number of wavelength points
  !     nz        I                  Number of metallicity bins
  !     nspectot  I                  Total number of spectra in "stellib" file
  !     istat     I                  Return code (0=normal)
  !     Teff      R(nmaxZl,nmaxsl)     log(Teff)
  !     gspec     R(nmaxZl,nmaxsl)     log(g)
  !     P1        R(nmaxZl,nmaxsl)     N(HI)
  !     P2        R(nmaxZl,nmaxsl)     N(HeI)
  !     P3        R(nmaxZl,nmaxsl)     N(HeII)
  !     nspec     I(nmaxZl+1)         Index of the first spectrum for each z bin
  !     Z         R(nmaxZl)           Z value of each bin
  !     Num       I                  Absolute numner (1-nspectot) of a spectrum
  !     flux      R(nmaxlambda,nmaxsl) Flux array
  !     wave      R(nmaxlambda)      [0.1nm] Wavelengths
  !     iz        I                  Index in Z
  !     wstart    R                  First wavelength point
  !     wstep     R                  Wavelength step (if linear bining)
  !     Lun       I                  Logical unit number of connected ASCII file
  !
  !    Structure of the FITS "stellib" file:
  !        The stellib file is made of 3 HDU. The first HDU is an "image"
  !        whose first axis is the wavelength direction (CTYPE1="WAVE"), but
  !        with in general unevenly sampled points.
  !        The second extension is a binary table giving the wavelength, in
  !        0.1 nm of each sampled point, it is writen in a format that allows
  !        Pleinpot to make the wavelength resampling on the fly, but can
  !        easily be handld by any program (a couple of keywords are written
  !        in each HDU to allow Pleinpot to interpret the data, they do not
  !        interfer with any other FITS interpreter).
  !        The third  HDU is a table of 6 columns giving: z, log(Teff), log(g)
  !        and the number of ionising photons NH1, NHeI and NheII.
  !
  !    Entrees
  !
  !    Sorties
  !
  !    Sous_Programmes  This program depends on fitsio library
  !
  !DOC ...................................................................
  subroutine stell_open(istat)

    implicit none

    integer             :: istat

    integer       blocksize,naxes(2),nfound
    character(len=80) ::  comment

    istat=0
    call ftgiou(stellibinfo%Lfits,istat)
    call ftopen(stellibinfo%Lfits,stellibinfo%filename,0,blocksize,istat)

    call ftgknj(stellibinfo%Lfits,'NAXIS',1,2,naxes,nfound,istat)     !size of the image.
    if (nfound .ne. 2)then
       istat=1
       return
    endif

    stellibinfo%nlambda=naxes(1)
    stellibinfo%nspectot=naxes(2)

    allocate(stellibinfo%lambda(naxes(1)))
    allocate(stellibinfo%spectra(naxes(1),naxes(2)))

    call ftgkyj(stellibinfo%Lfits,'NZBIN',stellibinfo%nz,comment,istat)     ! number of z bins

    if(istat.eq.0) call ftgkys(stellibinfo%Lfits,'GRID_TYP', stellibinfo%grid_type, &
         comment, istat)

    return
  end subroutine stell_open

  !-----------------------------------------------------------------------
  subroutine stell_close(Lfits,istat)

    implicit none

    integer       Lfits
    integer       istat

    istat=0

    call ftclos(Lfits, istat)
    call ftfiou(Lfits, istat)
    Lfits=-1

    return
  end subroutine stell_close

  !-----------------------------------------------------------------------
  subroutine stell_nz_r(istat)

    implicit none

    integer       istat


    character(len=80)::  comment
    logical       anynulls
    integer       ncol,iz,nfound
    real(dp) :: dummyreal

    if(stellibinfo%Lfits.le.0) then
       istat=1
       return
    endif
    istat=0

    call ftmnhd(stellibinfo%Lfits, 2, 'TGZ', 0, istat)

    call ftgkyj(stellibinfo%Lfits,'NZBIN',stellibinfo%nz,comment,istat) ! number of Z bins
    call ftgknj(stellibinfo%Lfits,'NZS',1,stellibinfo%nz,stellibinfo%firstspec,nfound,istat) ! get num of spec in each bin
    call ftgnrw(stellibinfo%Lfits, stellibinfo%nspectot, istat)

    !* Convert firstspec(i) into the index of the first spectrum in the corresponding
    !* Z bin
    stellibinfo%firstspec(stellibinfo%nZ+1)=stellibinfo%nspectot+1
    do iz=stellibinfo%nZ,1,-1                          
       stellibinfo%firstspec(iz)=stellibinfo%firstspec(iz+1)-stellibinfo%firstspec(iz)
    enddo

    call ftgcno(stellibinfo%Lfits,0,'Z',ncol,istat)
    do iz=1,stellibinfo%nz
       call ftgcvd(stellibinfo%Lfits,ncol,stellibinfo%firstspec(iz),1,1,-99d0,dummyreal,anynulls,istat)
       stellibinfo%Z(iz)=dummyreal
    enddo

    return
  end subroutine stell_nz_r

  !-----------------------------------------------------------------------
  subroutine stell_wave_r(istat)

    implicit none

    integer       istat  ! Removed unused: i

    logical       anynulls

    if(stellibinfo%Lfits.le.0) then
       istat=1
       return
    endif
    istat=0

    call ftmnhd(stellibinfo%Lfits, 2, 'WCA', 0, istat)
    call ftgcve(stellibinfo%Lfits,1,1,1,stellibinfo%nlambda,-99.,stellibinfo%lambda,anynulls,istat) ! lambda in 1st col

    return
  end subroutine stell_wave_r

  !-----------------------------------------------------------------------
  subroutine stell_wcl_w(Lfits,wstart,wstep,istat)
    ! When the wavelength sampling is linear...

    implicit none

    integer       Lfits
    real          wstart,wstep
    integer       istat

    integer       hdutype

    istat=0
    call ftmahd(Lfits,1,hdutype,istat)

    call ftukys(Lfits, 'CTYPE1', 'WAVE-WAV', '1st axis is wavelength',&
         istat)
    call ftukye(Lfits,'CRPIX1',1.,-9,'Reference pixel',istat)    
    call ftukye(Lfits, 'CRVAL1', wstart, -9,&
         '[0.1nm] Wavelength at reference pixel', istat) 
    call ftukys(Lfits,'CUNIT1','0.1nm','Unit for wavelength',istat)
    call ftukye(Lfits, 'CDELT1', wstep, -9, '[0.1nm] Wavelength step',&
         istat) 
    call ftukye(Lfits, 'CD1_1', wstep, -9, '[0.1nm] Wavelength step',&
         istat) 

    return
  end subroutine stell_wcl_w

  !-----------------------------------------------------------------------
  subroutine stell_wave_w(Lfits,nw,wave,istat)

    implicit none

    integer Lfits
    integer nw
    real    wave(*)
    integer istat

    character(len=16) :: wttype(1),wtform(1),wtunit(1)
    data wttype/'BFIT'/
    data wtform/'1D'/
    data wtunit/'0.1nm'/

    istat=0
    call ftmnhd(Lfits, 2, 'WCA', 0, istat)
    if(istat.ne.0) then
       istat=0
       call ftcrhd(Lfits,istat) ! create empty HDU
       call ftphbn(Lfits,nw,1,wttype,wtform,wtunit,'WCA',0,istat)

    endif

    call ftukyj(Lfits, 'H_WCATYP', 102,&
         'W calibration relat = list of wavelengths', istat)
    call ftukyj(Lfits,'BFIT5',1,'1D WCA rel',istat)
    call ftukyj(Lfits,'BFIT6',nw,'Number of wavelength points',istat)

    call ftpcle(Lfits,1,1,1,nw,wave,istat)  

    return
  end subroutine stell_wave_w

  !-----------------------------------------------------------------------
  subroutine stell_para_w(Lfits, nz, nspec, zspec, tspec, gspec, p1,&
       p2, p3, istat)

    implicit none

    integer Lfits
    integer nz

    integer nspec(*)
    real    zspec(*)
    real    tspec(nmaxZl,nmaxsl),gspec(nmaxZl,nmaxsl)
    real    p1(nmaxZl,nmaxsl),p2(nmaxZl,nmaxsl),p3(nmaxZl,nmaxsl)
    integer istat

    character(len=16) :: ttype(6),tform(6),tunit(6)
    data ttype/'Z','Teff','logG','NHI','NHeI','NHeII'/
    data tform/'1E','1E','1E','1E','1E','1E'/
    data tunit/' ',' log(K)','log(g/s**2)',' ',' ',' '/
    integer nsz(nmaxZl+1)

    integer nspectot
    real    buffer(nmaxZl*nmaxsl)
    integer iz,j,n

    istat=0

       call ftmnhd(Lfits, 2, 'TGZ', 0, istat)
       if(istat.ne.0)then
          istat=0
          call ftcrhd(Lfits,istat) ! create empty HDU
       endif

    nspectot=nspec(nz+1)-1
    call ftphbn(Lfits,nspectot,6,ttype,tform,tunit,'TGZ',0,istat)

    call ftukyj(Lfits,'NZBIN',nz,'Number of Z bins',istat)     
    do iz=1,nz
       nsz(iz)=nspec(iz+1)-nspec(iz)
    enddo
    call ftpknj(Lfits,'NZS',1,nz,nsz,'&',istat)     

    j=0
    do iz=1,nz
       do n=1,nsz(iz)
          j=j+1
          buffer(j)=zspec(iz)
       enddo
    enddo
    call ftpcle(Lfits,1,1,1,nspectot,buffer,istat)  
    j=0
    do iz=1,nz
       do n=1,nsz(iz)
          j=j+1
          buffer(j)=Tspec(iz,n)
       enddo
    enddo
    call ftpcle(Lfits,2,1,1,nspectot,buffer,istat)  
    j=0
    do iz=1,nz
       do n=1,nsz(iz)
          j=j+1
          buffer(j)=gspec(iz,n)
       enddo
    enddo
    call ftpcle(Lfits,3,1,1,nspectot,buffer,istat)  
    j=0
    do iz=1,nz
       do n=1,nsz(iz)
          j=j+1
          buffer(j)=p1(iz,n)
       enddo
    enddo
    call ftpcle(Lfits,4,1,1,nspectot,buffer,istat)  
    j=0
    do iz=1,nz
       do n=1,nsz(iz)
          j=j+1
          buffer(j)=p2(iz,n)
       enddo
    enddo
    call ftpcle(Lfits,5,1,1,nspectot,buffer,istat)  
    j=0
    do iz=1,nz
       do n=1,nsz(iz)
          j=j+1
          buffer(j)=p3(iz,n)
       enddo
    enddo
    call ftpcle(Lfits,6,1,1,nspectot,buffer,istat)  

    return
  end subroutine stell_para_w

  !-----------------------------------------------------------------------
  subroutine stell_flux_w(Lfits,nw,iz,nspec,flux,istat)

    implicit none

    real PLPrnoval ! undefined value in Pleinpot.
    parameter (PLPrnoval=-1.e36)
    integer Lfits
    integer nw
    integer iz

    integer nspec(*)
    real    flux(*)
    integer istat

    integer hdutype,fpix,nt

    istat=0
       call ftmahd(Lfits,1,hdutype,istat)

    fpix=(nspec(iz)-1)*nw+1
    nt=nw*(nspec(iz+1)-nspec(iz))
    call ftppne(Lfits,1,fpix,nt,flux,PLPrnoval,istat) ! spectrum

    return
  end subroutine stell_flux_w
  !
  !------------------------------------------------------------------------------
  subroutine stell_ascii_open(filename,Lun,istat)

    implicit none

    character(len=*) :: filename
    integer istat
    integer Lun

    istat=0
    call file_open(filename,Lun,istat)

    return
  end subroutine stell_ascii_open
  !
  !------------------------------------------------------------------------------
  subroutine read_CM()
    implicit none
    
    integer :: LunCM, i, k, istat  ! Removed unused: nCM
    character(len=72) :: a

    call file_open(trim(PEG_ROOT)//'/data/external/stellibCM.dat',&
         LunCM,istat)

    read(LunCM,*) stellibCM%nspectot,stellibCM%nlambda

    allocate(stellibCM%lambda(stellibCM%nlambda))
    allocate(stellibCM%spectra(stellibCM%nlambda,stellibCM%nspectot))
    
    do i=1,stellibCM%nspectot
       read(LunCM,'(a)') a         
    enddo

    read(LunCM,*) (stellibCM%lambda(k),k=1,stellibCM%nlambda)

    do i=1,stellibCM%nspectot !-ispec
       read(LunCM,*) (stellibCM%spectra(k,i),k=1,stellibCM%nlambda)
    end do
    close(LunCM)
    
  end subroutine read_CM

  !--------------------------------------------------
  subroutine read_stellib_spectra()
    implicit none

    integer :: nhdu, hdutype, status
    integer :: group, fpixel, ispec
    real    :: nullval
    parameter(nullval = 1.e-37)
    logical  :: anyf

    nhdu = 1
    status=0
    call ftmahd(stellibinfo%Lfits, nhdu, hdutype, status)
    
    do ispec=1,stellibinfo%nspectot
       fpixel = (ispec-1)*stellibinfo%nlambda+1
       status=0
       group=1
       call ftgpve(stellibinfo%Lfits, group, fpixel, stellibinfo%nlambda, nullval, &
            stellibinfo%spectra(:,ispec),anyf, status)
    enddo

  end subroutine read_stellib_spectra
end MODULE stellib_io
