c  fits_spec_io, IO to scenario file 
c  PhP, 2001/09/24
c  Part of the Pegase package
c------------------------------------------------------------------------------
c
c   The output spectra are written into a FITS file. Its format, ETS,
C   Evolutive Template Spectra consists in a series of HDUs. The primary
C   HDU contains the spectra (1st axis is wavelength direction, second is 
c   time). If the wavelengths are evenly sampled, the World Coordinate System
c   is set to the new FITS standard, otherwyse the wavelength of each pixel
c   is stored in a separate HDU (table named ETS_WCA_CONT). Two other HDUs
c   respectively contains the wavelength and fluxes in emission lines and
c   the properties at different times (ETS_LINES and ETS_PARA).
c
c   Some dedicated keywords are written to help Pleinpot to interpret this
C   format (for example, to allow a transparent resampling to wavelegnths).
c   The format shoud be correctly understood by any FITS reader. 
c
c   The access to FITS files is made with the CFITSIO Library (Bill Pence, 
C   GSFC) http://heasarc.gsfc.nasa.gov/docs/software/fitsio/fitsio.html
c
c------------------------------------------------------------------------------
C routine: spec_creat
c      subroutine fits_spec_creat(< filename,nw,nt,
c     s     nZ,header,fSNIa,
c     s     Zgas,tinfall,Zinfall,infall,SFRparam,fileSFR,
c     s     codeZ,ZSFR,fsub,twind,
c     s     answerneb,codeext,inclin,
c     s     > Lfits,istat)
c
c   filename C* input Name of the file
c   nw       I  input Number of wavelength points
c   nt       I  input Number of spectra (output ages)
c
c------------------------------------------------------------------------------
C routine fits_spec_cmt_w: wirte a comment into the header of the FITS file
c      subroutine fits_spec_cmt_w(< Lfits,comment,> istat)
c
c
c------------------------------------------------------------------------------
c routine fits_spec_wave_w Write the wavelength of the continuum spectra 
c        subroutine fits_spec_wave_w(< Lfits,nlambda,lambda,> istat)
c
c------------------------------------------------------------------------------
c routine fits_spec_cont_w: write one spectrum (ie a line in primary HDU)
c       subroutine fits_spec_cont_w(Lfits,itime,nlambda,flux,istat)
c
c------------------------------------------------------------------------------
c routine fits_spec_line_w: write emission lines at a given time
c       subroutine fits_spec_line_w( <Lfits,itime,nlines,lambdaline,fline,
c                                    > istat)
c
c------------------------------------------------------------------------------
c routine fits_spec_para_w: write the various properties at a given time
c       subroutine fits_spec_para_w(< Lfits,time,
c      s           Mgal,sigmastars,sigmaWD,sigmaBHNS,sigmasub,sigmagas,
c      s           Zgas,Zstars,Zbol,fluxbol,tauV,fluxext,SFR,NLymtot,
c      s           nSNIItot,nSNIatot,agestars,agebol,
c      s           > istat)
c
c
c------------------------------------------------------------------------------
c routine fits_spec_close: close the ETS file
c        subroutine fits_spec_close(Lfits,istat)
c------------------------------------------------------------------------------
c routines: fits_spec_open fits_spec_wave_read fits_spec_wavl_read
c           fits_spec_cont_r fits_spec_line_r fits_spec_para_r
c  WARNING: The read routines, used by the program "colors" are not exactly
c    symetric to the write routines. The main difference is that in the latter
c    the arguments are in double precision.
c------------------------------------------------------------------------------
      subroutine fits_spec_creat(filename,nw,nt,
     s     nZ,header,fSNIa,
     s     Zgas,tinfall,Zinfall,infall,SFRparam,fileSFR,
     s     codeZ,ZSFR,fsub,twind,
     s     answerneb,codeext,inclin,filestellib_short,
     s     Lfits,istat)
      
      implicit none

      include 'peg_config.f'

      character*(*) filename
      integer       nw,nt
      integer       nZ
      character*(*) header(4,nZ)
      real          fSNIa
      real          Zgas(*)
      real          tinfall
      real          Zinfall
      integer       infall
      real          SFRparam(*)
      character*(*) fileSFR
      integer       codeZ,codeext
      real          ZSFR(*),fsub,twind
      character*(*) answerneb
      real          inclin
      character*(*) filestellib_short
      integer       Lfits
      integer       istat

      character*72  a,answerinfall,answerz,answerext
      integer       i,p
      character*120 comment
      character*40  version
      integer       naxes(2)
      integer       nparam
      integer       lun
      integer       str_length
c
c Create a FITS image, first HDU to host the spectra: 
c 1st direction is wavelength; 2nd direction is age.
      istat=0
      !call ftgiou(Lfits,istat)  ! Get A free unit
      call file_unit(Lfits)
      call ftinit(Lfits,filename,1,istat) ! Create an empty file
      if (istat.ne.0) return
      naxes(1)=nw
      naxes(2)=nt
      call ftphpr(Lfits,.true.,-32,2,naxes,0,1,.true.,istat) ! FITS header
      call ftpkys(Lfits,'EXTNAME','ETS_CONTINUUM',' ',istat)
      call ftpkys(Lfits,'CTYPE1','WAVE','1st axis is wavelength',istat)
      call ftpkys(Lfits,'CTYPE2','AGE','2nd axis is age',istat)
      call ftpkys(Lfits,'BUNIT','LSUN/0.1nm','Unit of data',istat)
      call ftpkys(Lfits, 'H_CONTXT', 'ETS',     
     $     'Evolutive template spectra', istat)

      comment='              PEGASE-HR'
      call ftpcom(Lfits,comment, istat)
c read version in VERSION file
      call file_open(PEG_ROOT//'VERSION',lun,istat)
      if(istat.eq.0) then
         read(lun,'(a)') version
         call ftpkys(Lfits, 'VERSION', version,' ', istat)
         close(lun)
      endif
      call ftpkys(Lfits, 'AUTHOR', 'PEGASEHR Team',
     $     ' ', istat)
      call ftpkys(Lfits,'REFERENC','2004, A&A xxx, xxx',' ',istat)
      call ftpkys(Lfits,'REFCODE','2004A&A...xxx..xxx',' ',istat)
      call ftpkys(Lfits,'EMAIL','leborgne@iap.fr',' ',istat)

c
c Copy the scenario information into COMMENT keywords
      comment =
     $     '--------- DESCRIPTION OF THE SCENARIO --------------'
      call ftpcom(Lfits, comment, istat)
      istat=0

      comment =' Stellar library: '//
     $     filestellib_short(:str_length(filestellib_short))
      call ftpcom(Lfits, comment, istat)
      istat=0

      call ftpcom(Lfits,header(3,1), istat)
      call ftpcom(Lfits,header(3,1), istat)

      call ftpcom(Lfits,header(4,1), istat)
      do p=1,nZ
         do i=1,2
            call ftpcom(Lfits,header(i,p), istat)
         end do
      end do

      write(comment,'(a,e11.5)') 'Fraction of close binary systems: ',
     $     fSNIa
      call ftpcom(Lfits, comment, istat)
      write(comment,'(a,e11.5)')'Initial metallicity: ',Zgas(1)
      call ftpcom(Lfits, comment, istat)
      
      if(infall.eq.1) then
         write(comment,'(a)')'Infall'
         call ftpcom(Lfits, comment, istat)
         write(comment,'(a,e11.5)')'Infall timescale (Myr): ',tinfall
         call ftpcom(Lfits, comment, istat)
         write(comment,'(2a,e11.5)')'Metallicity of the ',
     $                 'infalling gas: ',Zinfall
         call ftpcom(Lfits, comment, istat)
      elseif(infall.eq.0) then
         write(comment,'(a)')'No infall '
         call ftpcom(Lfits, comment, istat)
      endif
      
      write(Comment,'(a,i3)')'Type of star formation: ',int(SFRparam(1))
      call ftpcom(Lfits, comment, istat)
      if (SFRparam(1).ge.1.and.SFRparam(1).le.3) then
         write(Comment,'(a,e11.5)')'p1: ',SFRparam(3)
         call ftpcom(Lfits, comment, istat)
         write(Comment,'(a,e11.5)')'p2: ',SFRparam(4)
         call ftpcom(Lfits, comment, istat)
      endif

      if (SFRparam(1).ge.10) then
         nparam=SFRparam(2)
         write(Comment,'(a,i2)')'Number of parameters:  ',nparam
         call ftpcom(Lfits, comment, istat)
         do i=1,nparam
            write(Comment,'(a,i2,a,e11.5)')' ',i,': ',SFRparam(2+i)
            call ftpcom(Lfits, comment, istat)
         end do
      end if
      if (SFRparam(1).le.-1) then
         print *,fileSFR
         write(Comment,'(2a)')'SFR file:',fileSFR(:str_length(fileSFR))
         call ftpcom(Lfits, comment, istat)
      end if
      
      if (SFRparam(1).ge.-1) then
         if(codeZ.eq.0) then
            write(Comment,'(a)')
     $           'No evolution of the stellar metallicity'
            call ftpcom(Lfits, comment, istat)
            write(Comment,'(a,e11.5)')'Stellar metallicity: ', ZSFR(1)
            call ftpcom(Lfits, comment, istat)
         else
            write(Comment,'(2a)')'Consistent evolution of the ',
     $              'stellar metallicity'
            call ftpcom(Lfits, comment, istat)
         endif

      endif

      write(Comment,'(2a,e11.5)')'Mass fraction of substellar ',
     $              'objects: ',fsub
      call ftpcom(Lfits, comment, istat)

      if(twind .lt.0) then
         write(Comment,'(a)')'No galactic winds'
         call ftpcom(Lfits, comment, istat)
      else
         write(Comment,'(a)')'Galactic winds'
         call ftpcom(Lfits, comment, istat)
         write(Comment,'(a,e11.5)')'Age of the galactic winds: ',
     $        twind
         call ftpcom(Lfits, comment, istat)
      endif

      write(Comment,'(a)')answerneb
      call ftpcom(Lfits, comment, istat)
      if (codeext.eq.0) then
         write(Comment,'(a)')'No extinction '
         call ftpcom(Lfits, comment, istat)
      elseif(codeext.eq.1) then
         write(Comment,'(a)')'Extinction for a spheroidal geometry '
         call ftpcom(Lfits, comment, istat)
      elseif(codeext.eq.2) then
         write(Comment,'(a,a)')'Extinction for a disk geometry:  ',
     $              'inclination-averaged'
         call ftpcom(Lfits, comment, istat)
      elseif(codeext.eq.3) then
         write(Comment,'(a,a)')'Extinction for a disk geometry: ',
     $              'specific inclination'
         call ftpcom(Lfits, comment, istat)
         write(Comment,'(a,e11.5)')'Inclination: ',inclin
         call ftpcom(Lfits, comment, istat)
      endif

      comment =
     $     '---------- DESCRIPTION OF THE FILE STRUCTURE  -------------'
      call ftpcom(Lfits, comment, istat)

      comment='This file consists in 3 or 4 Header Data Units'
      call ftpcom(Lfits, comment, istat)
      comment =
     $     'The primary HDU contains the spectra, '//
     $     'first axis is the wavelength'
      call ftpcom(Lfits, comment, istat)
      comment='direction and the second axis is the Age'
      call ftpcom(Lfits, comment, istat)
      comment = 
     $     'The HDU named ETS_LINES lists the emission lines and their'
      call ftpcom(Lfits, comment, istat)
      comment='equivalent width'
      call ftpcom(Lfits, comment, istat)
      comment =
     $     'The HDU named ETS_PARA gives '//
     $     'the characteristics of each spectrum'
      call ftpcom(Lfits, comment, istat)
      comment='Age in Myr, total mas in stars...'
      call ftpcom(Lfits, comment, istat)
      comment = 
     $     'If the wavelength sampling is not linear, '// 
     $     'a fourth HDU, named '
      call ftpcom(Lfits, comment, istat)
      comment =  
     $     'If present, ETS_CONT_WCA contains the '//
     $     '"dispersion relation", '
      call ftpcom(Lfits, comment, istat)
      comment='which is the list of '//
     $     'wavelengths of each pixels'
      call ftpcom(Lfits, comment, istat)
      comment =
     $     '--------------------------------------------------'
      call ftpcom(Lfits, comment, istat)

      return
      end

c------------------------------------------------------------------------------
      subroutine fits_spec_cmt_w(Lfits,comment,istat)
      implicit none
      integer       Lfits
      character*(*) comment
      integer       istat
      
      istat=0
      call ftpcom(Lfits, comment, istat)
      return
      end

c------------------------------------------------------------------------------
c Write the wavelength of the continuum spectra 
      subroutine fits_spec_wave_w(Lfits,nlambda,lambda,istat)

      implicit none
      integer Lfits
      integer nlambda
      real    lambda(*)
      integer istat

      real*8  step
      integer flag
      integer k
      integer hdutype

c First we search if the wavenlength points are linearly spaced
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
         call ftukys(Lfits, 'CTYPE1', 'WAVE-WAV',
     $        '1st axis is wavelength', istat)
         call ftukys(Lfits,'CUNIT1','0.1 nm','Unit for axis 1',istat)
         call ftukye(Lfits,'CRPIX1',1.,-9,'Reference pixel',istat)
         call ftukye(Lfits,'CRVAL1',lambda(1),-9,
     $        '[0.1nm] Value at reference',istat)
         call ftukyd(Lfits,'CDELT1',step,-9,'Step',istat)
         call ftukyd(Lfits,'CD1_1',step,-9,'Step',istat)
         call ftpkye(Lfits, 'H_WRESOL', 0.55, -9,
     $        '[0.1nm] FWHM wavelength resolution', istat)
      else
!         print *,'must write the WCA'
         istat=0
         call ftukys(Lfits, 'H_WCALIB', '[ETS_CONT_WCA]',
     $        'Pointer to WCA', istat)

         call ftmnhd(Lfits, 2, 'ETS_CONT_WCA', 0, istat)
         if(istat.eq.0) then    ! HDU exists, we delete it
            call ftdhdu(Lfits, hdutype, istat)
         endif

         istat=0
         call ftibin(Lfits,nlambda,1,
     s        'BFIT','1D','0.1nm','ETS_CONT_WCA',0, istat)

         call ftpcne(Lfits, 1, 1, 1, nlambda, lambda, -999., istat)
         call ftpcom(Lfits, 
     $        'List of wavelengths corresponding '//    
     $        'to 1st axis of 1st HDU, in 0.1nm',istat)
         call ftukyj(Lfits,'H_WCATYP',102,'Type of WCA', istat)
         call ftukyj(Lfits,'BFIT5',1,'1D WCA relation', istat)
         call ftukyj(Lfits,'BFIT6',nlambda,
     s        'Number of wavelength points', istat)

      endif

      return
      end

c------------------------------------------------------------------------------
      subroutine fits_spec_cont_w(Lfits,itime,nlambda,flux,istat)
      implicit none
      real    PLPrnoval
      parameter (PLPrnoval=-1.e36)
      integer Lfits
      integer itime
      integer nlambda
      real    flux(*)
      integer istat

      integer hdutype
      integer felem

      call ftmahd(Lfits,1,hdutype,istat)
      felem=1+(itime-1)*nlambda
      call ftppne(Lfits,1, felem, nlambda, flux, PLPrnoval, istat)

      return
      end

c------------------------------------------------------------------------------
      subroutine fits_spec_line_w(Lfits, itime, nlines, lambdaline,
     $     fline, istat)
      integer Lfits,itime,nlines
      real    fline(*),lambdaline(*)
      integer  istat

      character*16 ttype(2)
      character*16 tform(2)
      character*16 tunit(2)
      integer      hdutype
      integer      ntimes
      character*80 comm
      integer      i

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
         write(tform(2),'(i4,a)')ntimes,'E'
         tunit(2)='??'
         call ftibin(Lfits,nlines,2,ttype,tform,tunit,
     s        'ETS_LINES',0, istat)
         call ftpcne(Lfits, 1, 1, 1, nlines, lambdaline, -999., istat)
      endif

      do i=1,nlines
         call ftpcle(Lfits,2,i,itime,1,fline(i), istat)
      enddo

      return
      end

c------------------------------------------------------------------------------
      subroutine fits_spec_para_w(Lfits,time,
     s           Mgal,sigmastars,sigmaWD,sigmaBHNS,sigmasub,sigmagas,
     s           Zgas,Zstars,Zbol,fluxbol,tauV,fluxext,SFR,NLymtot,
     s           nSNIItot,nSNIatot,agestars,agebol,
     s           istat)
      implicit none
      integer Lfits,time
      real    Mgal(*)
      real    sigmastars(*)
      real    sigmaWD(*)
      real    sigmaBHNS(*)
      real    sigmagas(*),sigmasub(*)
      real    Zgas(*),Zstars(*),Zbol
      real    fluxbol,nSNIItot,fluxext
      real    tauV
      real    SFR(*)
      double precision NLymtot
      real    nSNIatot,agestars(*),agebol
      integer istat

      integer j

      character*16 ttype(19)
      character*16 tform(19)
      character*16 tunit(19)
      integer      hdutype
      integer      ntimes
      character*80 comm
      integer      frow

      save frow

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
c Note: flux bol is in Solar unit, not erg/s, to avoid overflow

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
c Note: do not multiply NLymtot by Lsol, to avoid overflow

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

         call ftibin(Lfits,ntimes,19,ttype,tform,tunit,
     s        'ETS_PARA',0, istat)
         frow=1
      endif

      call ftpcnj(Lfits, 1, frow, 1, 1, time-1, -999., istat)
      call ftpcne(Lfits, 2, frow, 1, 1, Mgal(time), -999., istat)
      call ftpcne(Lfits, 3, frow, 1, 1, sigmastars(time), -999., istat)
      call ftpcne(Lfits, 4, frow, 1, 1, sigmaWD(time), -999., istat)
      call ftpcne(Lfits, 5, frow, 1, 1, sigmaBHNS(time), -999., istat)
      call ftpcne(Lfits, 6, frow, 1, 1, sigmasub(time), -999., istat)
      call ftpcne(Lfits, 7, frow, 1, 1, sigmagas(time), -999., istat)
      call ftpcne(Lfits, 8, frow, 1, 1, Zgas(time), -999., istat)
      call ftpcne(Lfits, 9, frow, 1, 1, Zstars(time), -999., istat)
      call ftpcne(Lfits,10, frow, 1, 1, Zbol, -999., istat)

      call ftpcne(Lfits,11, frow, 1, 1, fluxbol, -999., istat)
      call ftpcne(Lfits,12, frow, 1, 1, tauV, -999., istat)
      call ftpcne(Lfits,13, frow, 1, 1, fluxext, -999., istat)
      call ftpcne(Lfits,14, frow, 1, 1, SFR(time), -999., istat)
      call ftpcnd(Lfits,15, frow, 1, 1, NLymtot, -999.d0, istat)
      call ftpcne(Lfits,16, frow, 1, 1, nSNIItot, -999., istat)
      call ftpcne(Lfits,17, frow, 1, 1, nSNIatot, -999., istat)
      call ftpcne(Lfits,18, frow, 1, 1, agestars(time), -999., istat)
      call ftpcne(Lfits,19, frow, 1, 1, agebol, -999., istat)

      frow=frow+1              

      return
      end

c------------------------------------------------------------------------------
      subroutine fits_spec_close(Lfits,istat)
      implicit none
      integer Lfits,istat
      
      istat=0
      call ftclos(Lfits,istat)
      if(istat.eq.0) Lfits=-1

      return
      end
c------------------------------------------------------------------------------
      subroutine fits_spec_open(filename,Lfits,nw,nt,istat)
      implicit none
      character*(*) filename
      integer       Lfits
      integer       nw,nt
      integer       istat

      integer       blocksize,naxes(2),nfound

      istat=0
      !call ftgiou(Lfits,istat)
      call file_unit(Lfits)
      call ftopen(Lfits,filename,0,blocksize,istat)

      call ftgknj(Lfits,'NAXIS',1,2,naxes,nfound,istat)     !size of the image.
      if (nfound .ne. 2)then
         istat=1
         return
      endif

      nw=naxes(1)
      nt=naxes(2)

      return
      end

c------------------------------------------------------------------------------
      subroutine fits_spec_wave_read(Lfits,nw,lambda,istat)
      implicit none
      integer Lfits
      integer nw
      real*8  lambda(*)
      integer istat

      integer       hdutype
      character*68  keyval,comment
      logical       anynulls
      real*8        cr,cv,cd
      integer       k

      istat=0
      call ftmahd(Lfits,1,hdutype,istat)

      call ftgkys(Lfits,'CTYPE1',keyval,comment,istat)
      if (keyval(:4).ne.'WAVE') then
         istat=123
         return
      endif

      if (keyval(:8).eq.'WAVE-WAV') then ! Linear sampling of Wavelength
         call ftgkyd(Lfits,'CRPIX1',cr,comment,istat)
         call ftgkyd(Lfits,'CRVAL1',cv,comment,istat)
         call ftgkyd(Lfits,'CD1_1',cd,comment,istat)
         if(istat.ne.0) return

         do k=1,nw
            lambda(k)=cv + (k-cr) * cd
         enddo

      else
         call ftgkys(Lfits,'H_WCALIB',keyval,comment,istat)
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
      end
c------------------------------------------------------------------------------
      subroutine fits_spec_wavl_read(Lfits,nlines,lambdaline,istat)
      implicit none
      integer Lfits
      integer nlines
      real*8  lambdaline(*)
      integer istat
      character*68 comment
    
      integer ncol
      logical anynulls

      istat=0
      call ftmnhd(Lfits, 2, 'ETS_LINES', 0, istat)
      if (istat.ne.0) return
      call ftgkyj(Lfits, 'NAXIS2', nlines, comment, istat)
      if (istat.ne.0) return

      call ftgcno(Lfits,0,'WAVE',ncol,istat)
      call ftgcvd(Lfits, ncol, 1, 1, nlines, -999.d0, lambdaline,
     $     anynulls, istat)
      
      return
      end
c------------------------------------------------------------------------------
      subroutine fits_spec_cont_r(Lfits,nlambda,itime,flux,istat)
      implicit none
      integer Lfits
      integer nlambda
      integer itime
      real*8  flux(*)
      integer istat

      integer hdutype
      integer felem
      logical anynull

      istat=0

      call ftmahd(Lfits,1,hdutype,istat)
      felem=(itime-1)*nlambda+1
      call ftgpvd(Lfits,1,felem,nlambda,1.d-25,flux,anynull,istat)

      return
      end
c------------------------------------------------------------------------------
      subroutine fits_spec_line_r(Lfits,nlines,itime,fluxline,istat)
      implicit none
      integer Lfits
      integer nlines
      integer itime
      real*8  fluxline(*)
      integer istat

      integer ncol
      logical anynulls
      integer k

      istat=0
      call ftmnhd(Lfits, 2, 'ETS_LINES', 0, istat)
      if (istat.ne.0) return

      call ftgcno(Lfits,0,'FLUXLINE',ncol,istat)

      do k=1,nlines
         call ftgcvd(Lfits, ncol, k, itime, 1, -999.d0, fluxline(k),
     $        anynulls, istat)
      enddo

      return
      end
c------------------------------------------------------------------------------
      subroutine fits_spec_para_r(Lfits,itime,
     s           time,
     s           Mgal,sigmastars,sigmaWD,sigmaBHNS,sigmasub,sigmagas,
     s           Zgas,Zstars,Zbol,fluxbol,tauV,fluxext,SFR,NLymtot,
     s           nSNIItot,nSNIatot,agestars,agebol,
     s           istat)

      implicit none
      integer Lfits
      integer itime
      integer time
      double precision Mgal
      double precision sigmastars
      double precision sigmaWD
      double precision sigmaBHNS
      double precision sigmagas,sigmasub
      double precision Zgas,Zstars,Zbol
      double precision fluxbol,nSNIItot,fluxext
      double precision tauV
      double precision SFR
      double precision NLymtot
      double precision nSNIatot,agestars,agebol

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
      call ftgcvd(Lfits, ncol, itime, 1, 1, -999.d0, sigmastars,
     $     anynulls, istat)

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
      end
c------------------------------------------------------------------------------
