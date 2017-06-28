CDOC ...................................................................
C    Nom du programme  stellib_io
C    
C    Applicration       Access to the stellar libraries
C
C    Author            Philippe Prugniel  2001/07
C
C    Purpose
C      This collection of routines read and write information from/to
C      "stellib" FITS files and can also read the PEGASE2 ascii format
C      for these files.
C
C    Description
c      subroutine stell_open(filename, > Lfits,grid_type,grid_type,nw,nz,nspectot,istat)
c          Open a "stellib" FITS file, return a pointer "Lfits" to be used for
c          any further transaction
c      subroutine stell_close(Lfits,istat)
c          Disconnect the FITS file, reset Lfits to -1
c      subroutine stell_para_r(Lfits, > Teff,Gspec,P1,P2,P3,istat)
c          Read the "parameters" of all spectra in the file
c      subroutine stell_nz_r(< Lfits, > nspec,Z,istat)
c          Read the metallicity for each metallicity bin and the index of the
c          first spectra in the corresponding bins.
c      subroutine stell_wave_r(Lfits,nw,wave,istat)
c          Read the wavelengths
c      subroutine stell_creat(filename,grid_type,nw,nz,nspectot, > Lfits,istat)
c          Create a "stellib" FITS file
c      subroutine stell_wcl_w(Lfits,wstart,wstep, > istat)
c          Write the wavelength sampling, when it is linear
c      subroutine stell_wave_w(Lfits,nw,wave, > istat)
c          Write the table of wavelength points
c      subroutine stell_para_w(Lfits,nz,nspec,zspec,tspec,gspec,p1,p2,p3, 
c                              > istat)
c          Write the parameters of each spectra
c      subroutine stell_flux_w(Lfits,nw,iz,nspec,flux, > istat)
c          Write the series of spectra for metallicity iz
c      subroutine stell_ascii_open(filename, > Lun,istat)
c          Open an ASCII format "stellib" file 
c      subroutine stell_ascii_hr(Lun, > nz,nw,nspectot,nspec,zspec,Tspec, 
c                                gspec,p1,p2,p3,wave,istat)
c          Read the first part (header) of ascii "stellib"
c      subroutine stell_ascii_dr(Lun,nw,nspectot, > flux,istat)
c          Read fluxes in ascii "stellib"
c
c      The access to FITS files is made with the CFITSIO Library (Bill Pence, 
C      GSFC) http://heasarc.gsfc.nasa.gov/docs/software/fitsio/fitsio.html
c
c    Arguments
C        General conventions: Input arguments first, I/O, outpu last
c                             Last argument, istat, is 0 if no error occured
c     filename  C*                 Name of a "stellib file" 
c     Lfits     I                  Pointer to a FITS stream
c     nw        I                  Number of wavelength points
c     nz        I                  Number of metallicity bins
c     nspectot  I                  Total number of spectra in "stellib" file
c     istat     I                  Return code (0=normal)
c     Teff      R(nmaxZl,nmaxsl)     log(Teff)
c     gspec     R(nmaxZl,nmaxsl)     log(g)
c     P1        R(nmaxZl,nmaxsl)     N(HI)
c     P2        R(nmaxZl,nmaxsl)     N(HeI)
c     P3        R(nmaxZl,nmaxsl)     N(HeII)
c     nspec     I(nmaxZl+1)         Index of the first spectrum for each z bin
c     Z         R(nmaxZl)           Z value of each bin
c     Num       I                  Absolute numner (1-nspectot) of a spectrum
c     flux      R(nmaxlambda,nmaxsl) Flux array
c     wave      R(nmaxlambda)      [0.1nm] Wavelengths
c     iz        I                  Index in Z
c     wstart    R                  First wavelength point
c     wstep     R                  Wavelength step (if linear bining)
c     Lun       I                  Logical unit number of connected ASCII file
C
C    Structure of the FITS "stellib" file:
C        The stellib file is made of 3 HDU. The first HDU is an "image"
C        whose first axis is the wavelength direction (CTYPE1="WAVE"), but
C        with in general unevenly sampled points.
C        The second extension is a binary table giving the wavelength, in
C        0.1 nm of each sampled point, it is writen in a format that allows
C        Pleinpot to make the wavelength resampling on the fly, but can
C        easily be handld by any program (a couple of keywords are written
C        in each HDU to allow Pleinpot to interpret the data, they do not
C        interfer with any other FITS interpreter).
C        The third  HDU is a table of 6 columns giving: z, log(Teff), log(g)
C        and the number of ionising photons NH1, NHeI and NheII.
C
C    Entrees
C
C    Sorties
C
C    Sous_Programmes  This program depends on fitsio library
C
CDOC ...................................................................
      subroutine stell_open(filename,Lfits,grid_type,
     $     nw,nz,nspectot,istat)
      implicit none
      character*(*) filename
      integer       Lfits
      character*(*) grid_type
      integer       nw,nz,nspectot
      integer       istat

      integer funit,fcurr,fflux,fwave,fpara
      common/stellfio/funit,fcurr,fflux,fwave,fpara

      integer       blocksize,naxes(2),nfound
      character*80  comment

      istat=0
      call ftgiou(Lfits,istat)
      call ftopen(Lfits,filename,0,blocksize,istat)

      call ftgknj(Lfits,'NAXIS',1,2,naxes,nfound,istat)     !size of the image.
      if (nfound .ne. 2)then
         istat=1
         return
      endif

      nw=naxes(1)
      nspectot=naxes(2)

      call ftgkyj(Lfits,'NZBIN',nz,comment,istat)     ! number of z bins

      if(istat.eq.0) then
         funit=Lfits
         fcurr=1
         fflux=1
         fwave=0
         fpara=0         
      endif

      if(istat.eq.0) call ftgkys(Lfits,'GRID_TYP', grid_type, 
     $     comment, istat)

      return
      end

c-----------------------------------------------------------------------
      subroutine stell_close(Lfits,istat)
      implicit none
      integer       Lfits
      integer       istat

      integer funit,fcurr,fflux,fwave,fpara
      common/stellfio/funit,fcurr,fflux,fwave,fpara

      istat=0

      call ftclos(Lfits, istat)
      call ftfiou(Lfits, istat)
      Lfits=-1
      
      funit=-1
      fcurr=0
      fflux=0
      fwave=0
      fpara=0

      return
      end

c-----------------------------------------------------------------------
      subroutine stell_para_r(Lfits,Teff,G,P1,P2,P3,istat)
      implicit none
      include 'peg_include.f'
      integer       Lfits
      real          Teff(nmaxZl,nmaxsl)
      real          G(nmaxZl,nmaxsl)
      real          P1(nmaxZl,nmaxsl)
      real          P2(nmaxZl,nmaxsl)
      real          P3(nmaxZl,nmaxsl)
      real          buffer(nmaxZl*nmaxsl)
      integer       istat

      integer funit,fcurr,fflux,fwave,fpara
      common/stellfio/funit,fcurr,fflux,fwave,fpara

      integer       nsz(nmaxZl),nz
      integer       hdutype,nfound,ncol
      integer       nspectot
      character*80  comment
      logical       anynulls
      integer       j,n,iz

      if(Lfits.le.0) then
         istat=1
         return
      endif
      istat=0

      if(Lfits.ne.funit)then
         funit=Lfits
         fcurr=-1
         fflux=0
         fwave=0
         fpara=0
      endif

      if(fcurr.ne.fpara) then
         call ftmnhd(Lfits, 2, 'TGZ', 0, istat)
         if(istat.eq.0) then
            call ftghdn(Lfits,fpara)
            fcurr=fpara
         endif
      endif
      call ftgkyj(Lfits,'NZBIN',nz,comment,istat)     ! number of z bins
      call ftgknj(Lfits,'NZS',1,nz,nsz,nfound,istat)  ! num of spec in each bin

      call ftgnrw(Lfits, nspectot, istat)             ! total number of spectra

      call ftgcno(Lfits,0,'Teff',ncol,istat)
      call ftgcve(Lfits,ncol,1,1,nspectot,-999.,buffer,anynulls,istat)
      j=0
      do iz=1,nz
         do n=1,nsz(iz)
            j=j+1
            Teff(iz,n)=buffer(j)
         enddo
      enddo

      call ftgcno(Lfits,0,'logG',ncol,istat)
      call ftgcve(Lfits,ncol,1,1,nspectot,-999.,buffer,anynulls,istat)
      j=0
      do iz=1,nz
         do n=1,nsz(iz)
            j=j+1
            g(iz,n)=buffer(j)
         enddo
      enddo

      call ftgcno(Lfits,0,'NHI',ncol,istat)
      call ftgcve(Lfits,ncol,1,1,nspectot,-999.,buffer,anynulls,istat)
      j=0
      do iz=1,nz
         do n=1,nsz(iz)
            j=j+1
            p1(iz,n)=buffer(j)
         enddo
      enddo

      call ftgcno(Lfits,0,'NHeI',ncol,istat)
      call ftgcve(Lfits,ncol,1,1,nspectot,-999.,buffer,anynulls,istat)
      j=0
      do iz=1,nz
         do n=1,nsz(iz)
            j=j+1
            p2(iz,n)=buffer(j)
         enddo
      enddo

      call ftgcno(Lfits,0,'NHeII',ncol,istat)
      call ftgcve(Lfits,ncol,1,1,nspectot,-999.,buffer,anynulls,istat)
      j=0
      do iz=1,nz
         do n=1,nsz(iz)
            j=j+1
            p3(iz,n)=buffer(j)
         enddo
      enddo

      return
      end

c-----------------------------------------------------------------------
      subroutine stell_nz_r(Lfits,nspec,Z,istat)
      implicit none
      integer       Lfits
      integer       nspec(*)    ! (out) index of the 1st spectrum in each Z bin
      real          Z(*)        ! (out) metallicity of each bin
      integer       istat

      integer funit,fcurr,fflux,fwave,fpara
      common/stellfio/funit,fcurr,fflux,fwave,fpara

      integer       hdutype
      character*80  comment
      integer       nspectot
      logical       anynulls
      integer       nz,ncol,ni,iz,nfound

      if(Lfits.le.0) then
         istat=1
         return
      endif
      istat=0

      if(Lfits.ne.funit)then
         funit=Lfits
         fcurr=-1
         fflux=0
         fwave=0
         fpara=0
      endif
      if(fcurr.ne.fpara) then
         call ftmnhd(Lfits, 2, 'TGZ', 0, istat)
         if(istat.eq.0) then
            call ftghdn(Lfits,fpara)
            fcurr=fpara
         endif
      endif

      call ftgkyj(Lfits,'NZBIN',nz,comment,istat) ! number of Z bins
      call ftgknj(Lfits,'NZS',1,nz,nspec,nfound,istat) ! get num of spec in each bin
      call ftgnrw(Lfits, nspectot, istat)

** Convert nspec(i) into the index of the first spectrum in the corresponding
** Z bin
      nspec(nZ+1)=nspectot+1
      do iz=nZ,1,-1                          
         nspec(iz)=nspec(iz+1)-nspec(iz)
      enddo

      call ftgcno(Lfits,0,'Z',ncol,istat)
      do iz=1,nz
         call ftgcve(Lfits,ncol,nspec(iz),1,1,-99.,Z(iz),anynulls,istat)
      enddo

      return
      end

c-----------------------------------------------------------------------
      subroutine stell_wave_r(Lfits,nw,wave,istat)
      implicit none
      integer       Lfits
      integer       nw
      real          wave(*)
      integer       istat

      integer funit,fcurr,fflux,fwave,fpara
      common/stellfio/funit,fcurr,fflux,fwave,fpara

      include   'peg_include.f'
      integer       hdutype
      integer       nspectot
      character*80  comment
      logical       anynulls

      if(Lfits.le.0) then
         istat=1
         return
      endif
      istat=0

      if(Lfits.ne.funit)then
         funit=Lfits
         fcurr=-1
         fflux=0
         fwave=0
         fpara=0
      endif
      if(fcurr.ne.fwave) then
         call ftmnhd(Lfits, 2, 'WCA', 0, istat)
         if(istat.eq.0) then
            call ftghdn(Lfits,fwave)
            fcurr=fwave
         endif
      endif
      call ftgcve(Lfits,1,1,1,nw,-99.,wave,anynulls,istat) ! wave in 1st col
      
      return
      end

c-----------------------------------------------------------------------
      subroutine stell_creat(filename,grid_type,
     $     nw,nz,nspectot,Lfits,istat)
      implicit none
      character*(*) filename
      integer       Lfits
      character*(*) grid_type
      integer       nw,nz,nspectot
      integer       istat

      integer funit,fcurr,fflux,fwave,fpara
      common/stellfio/funit,fcurr,fflux,fwave,fpara
      integer       naxes(2)

c
c Create a FITS image, first HDU to host the spectra
      istat=0
      call ftgiou(Lfits,istat)  ! Get A free unit
      call ftinit(Lfits,filename,1,istat) ! Create an empty file
      naxes(1)=nw
      naxes(2)=nspectot
      call ftphpr(Lfits,.true.,-32,2,naxes,0,1,.true.,istat) ! FITS header
      call ftpkys(Lfits,'CTYPE1','WAVE','1st axis is wavelength',istat)
      call ftpkys(Lfits,'H_CONTXT','SPE','Multi-spectra',istat)
      call ftpkys(Lfits,'H_WCALIB','[WCA]','Pointer to WCA',istat)
      call ftpkys(Lfits,'GRID_TYP',grid_type,'Type of grid',istat)
      call ftpkyj(Lfits,'NZBIN',nz,'Number of Z bins',istat)     

      funit=Lfits
      fwave=0
      fpara=0
      if(istat.eq.0) then
         fcurr=1
         fflux=1
      else
         fcurr=0
         fflux=0
      endif

      return
      end

c-----------------------------------------------------------------------
      subroutine stell_wcl_w(Lfits,wstart,wstep,istat)
c When the wavelength sampling is linear...
      implicit none
      integer       Lfits
      real          wstart,wstep
      integer       istat

      integer funit,fcurr,fflux,fwave,fpara
      common/stellfio/funit,fcurr,fflux,fwave,fpara
      integer       hdutype

      istat=0
      if(Lfits.ne.funit)then
         funit=Lfits
         fcurr=-1
         fflux=0
         fwave=0
         fpara=0
      endif
      if(fcurr.ne.1) then
         call ftmahd(Lfits,1,hdutype,istat)
         if(istat.eq.0)fcurr=1
      endif
      call ftukys(Lfits, 'CTYPE1', 'WAVE-WAV', '1st axis is wavelength',
     $     istat)
      call ftukye(Lfits,'CRPIX1',1.,-9,'Reference pixel',istat)    
      call ftukye(Lfits, 'CRVAL1', wstart, -9,
     $     '[0.1nm] Wavelength at reference pixel', istat) 
      call ftukys(Lfits,'CUNIT1','0.1nm','Unit for wavelength',istat)
      call ftukye(Lfits, 'CDELT1', wstep, -9, '[0.1nm] Wavelength step',
     $     istat) 
      call ftukye(Lfits, 'CD1_1', wstep, -9, '[0.1nm] Wavelength step',
     $     istat) 
 
      return
      end

c-----------------------------------------------------------------------
      subroutine stell_wave_w(Lfits,nw,wave,istat)
      implicit none
      integer Lfits
      integer nw
      real    wave(*)
      integer istat

      integer funit,fcurr,fflux,fwave,fpara
      common/stellfio/funit,fcurr,fflux,fwave,fpara
      character*16 wttype(1),wtform(1),wtunit(1)
      data wttype/'BFIT'/
      data wtform/'1D'/
      data wtunit/'0.1nm'/

      istat=0
      if(Lfits.ne.funit)then
         funit=Lfits
         fcurr=-1
         fflux=0
         fwave=0
         fpara=0
      endif
      if(fcurr.ne.fwave) then
         call ftmnhd(Lfits, 2, 'WCA', 0, istat)
         if(istat.ne.0) then
            istat=0
            call ftcrhd(Lfits,istat) ! create empty HDU
            call ftphbn(Lfits,nw,1,wttype,wtform,wtunit,'WCA',0,istat)
            if(istat.eq.0) call ftghdn(Lfits,fwave)
         endif
         if(istat.eq.0) fcurr=fwave 
      endif

      call ftukyj(Lfits, 'H_WCATYP', 102,
     $     'W calibration relat = list of wavelengths', istat)
      call ftukyj(Lfits,'BFIT5',1,'1D WCA rel',istat)
      call ftukyj(Lfits,'BFIT6',nw,'Number of wavelength points',istat)

      call ftpcle(Lfits,1,1,1,nw,wave,istat)  

      return
      end

c-----------------------------------------------------------------------
      subroutine stell_para_w(Lfits, nz, nspec, zspec, tspec, gspec, p1,
     $     p2, p3, istat)
      implicit none
      include 'peg_include.f'
      integer Lfits
      integer nz

      integer nspec(*)
      real    zspec(*)
      real    tspec(nmaxZl,nmaxsl),gspec(nmaxZl,nmaxsl)
      real    p1(nmaxZl,nmaxsl),p2(nmaxZl,nmaxsl),p3(nmaxZl,nmaxsl)
      integer istat

      character*16 ttype(6),tform(6),tunit(6)
      data ttype/'Z','Teff','logG','NHI','NHeI','NHeII'/
      data tform/'1E','1E','1E','1E','1E','1E'/
      data tunit/' ',' log(K)','log(g/s**2)',' ',' ',' '/
      integer nsz(nmaxZl+1)

      integer funit,fcurr,fflux,fwave,fpara
      common/stellfio/funit,fcurr,fflux,fwave,fpara

      integer nspectot
      real    buffer(nmaxZl*nmaxsl)
      integer iz,j,n

      istat=0

      if(Lfits.ne.funit)then
         funit=Lfits
         fcurr=-1
         fflux=0
         fwave=0
         fpara=0
      endif
      if(fcurr.ne.fpara) then
         call ftmnhd(Lfits, 2, 'TGZ', 0, istat)
         if(istat.ne.0)then
            istat=0
            call ftcrhd(Lfits,istat) ! create empty HDU
            if(istat.eq.0) call ftghdn(Lfits,fpara)
         endif
         if(istat.eq.0) fcurr=fpara 
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
      end

c-----------------------------------------------------------------------
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
      integer funit,fcurr,fflux,fwave,fpara
      common/stellfio/funit,fcurr,fflux,fwave,fpara

      integer hdutype,fpix,nt,i

      istat=0
      if(Lfits.ne.funit)then
         funit=Lfits
         fcurr=-1
         fflux=0
         fwave=0
         fpara=0
      endif
      if(fcurr.ne.1) then
         call ftmahd(Lfits,1,hdutype,istat)
         if(istat.eq.0)fcurr=1
      endif

      fpix=(nspec(iz)-1)*nw+1
      nt=nw*(nspec(iz+1)-nspec(iz))
      call ftppne(Lfits,1,fpix,nt,flux,PLPrnoval,istat) ! spectrum

      return
      end
c
c------------------------------------------------------------------------------
      subroutine stell_ascii_open(filename,Lun,istat)
      implicit none
      character*(*) filename
      integer istat
      integer Lun

      istat=0
      call file_open(filename,Lun,istat)

      return
      end
c
c------------------------------------------------------------------------------
      subroutine stell_ascii_hr(Lun,nz,nw,nspectot,nspec,zspec,Tspec,
     s     gspec,p1,p2,p3,wave,istat)
      implicit none

      include 'peg_include.f'

      integer Lun
      integer nz,nw,nspectot
      integer nspec(nmaxZl)
      real    zspec(nmaxZl)
      real    Tspec(nmaxZl,nmaxsl),gspec(nmaxZl,nmaxsl)
      real    p1(nmaxZl,nmaxsl),p2(nmaxZl,nmaxsl),p3(nmaxZl,nmaxsl)
      real    wave(*)
      integer istat
      
      integer Luerror,i,j,k,iz,ief
      
      istat=0
      luerror=0

c Read the first part of the file which describes the grid.
c and copy to the new library files
      read(Lun,*,iostat=ief) nz, nw
      if(ief.ne.0.or.nz.gt.nmaxZl) then
         write(luerror,'(a)')
     $        'STELL_READ_ASCII: error in Lejeune library ??'
         istat=1
         return
      endif
      nspectot=0
      do iz=1,nz
         read(Lun,*) nspec(iz),zspec(iz)
         do i=1,nspec(iz)
            j=i+nspectot
            read(Lun,*) Tspec(iz,i), gspec(iz,i), p1(iz,i), p2(iz,i),
     $           p3(iz,i)
         end do
         nspectot=nspectot+nspec(iz)
      end do         

      nspec(nz+1)=nspectot+1
      do iz=nz,1,-1
         nspec(iz)=nspec(iz+1)-nspec(iz)
      end do         
      

c For each grid point:
c Read LeJeune spectrum and write a line in fits
c Compute interpolated ELODIE spectrum, normalize, write
      read(Lun,*) (wave(k),k=1,nw)

      return
      end

c-----------------------------------------------------------------------
      subroutine stell_ascii_dr(Lun,nw,nspectot,flux,istat)
      implicit none
      include 'peg_include.f'
      integer Lun,nw,nspectot


      real flux(nmaxlambda,nmaxsl)
      integer istat

      integer i,k

      istat=0
      do i=1,nspectot
         read(Lun,*)(flux(k,i),k=1,nw)
      enddo

      return
      end
