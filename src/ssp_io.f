c Pegase Package: version 2001/09/30 PhP.
c------------------------------------------------------------------------------
c       subroutine ssp_files_read(<fileSSPs, <> nZ, > fileSSPZ,ZSSP,istat)
c          In entry nZ is the maximum allowed number of SSP files, in output
C          it is the actual number of Z
c
c       subroutine ssp_head_read( <nZ,fileSSP, >header,istat)
c
c       subroutine ssp_data_read( <nZ,fileSSPZ,fSNIa, >
c      $    ntimes,
c      $    massalive,
c      $    nused,
c      $    iused,
c      $    fluxbolSSP,
c      $    fluxspec,
c      $    invtime,
c      $    beta,
c      $    NLym,
c      $    nSNII,
c      $    nSNIa,
c      $    ejecta,
c      $    ejectaZ,
c      $    massBHNS,
c      $    massWD,
c      $    istat
c      $     )
c------------------------------------------------------------------------------
***** Reading of the names of the SSPs files. 
c        In entry   fileSSPs is the name of the file containing the list of SSP
c                   grid_type is the type of grid expected for the SSPs
c                   nZ is the maximum allowed number of SSP files.
c
c        The path included in fileSSPs is added to the individual
c        SSP filesnames listed inside fileSSPs. 
c
      subroutine ssp_files_read(fileSSPs,grid_type,nZ,fileSSPZ,ZSSP,
     $     istat)
      implicit none
      character*(*) fileSSPs, grid_type
      integer       nZ
      character*(*) fileSSPZ(*)
      real          ZSSP(*)
      integer       istat

      integer       nzm
      integer       i,j,lu1,lu2
      character*280  a,name
      character*280  fileexch
      real          Zexch
      character*280 line
      integer       headend
      character*280 fileSSPs_prefix
      integer       iindex,iindextot,str_length

      istat=0

      call file_open(fileSSPs,Lu1,istat)      
      if (istat.ne.0) then
         write(*,*)'SSP_FILES_READ: Failed to open SSP file'
         write(*,*) 'istat=',istat
         return
      endif

      read(lu1,'(a)',iostat=istat) line
      if (line(:22).ne.'format: PEGASE-HR/SSPs') then
         write(*,*)'SSP_FILES_READ: SSPs file has uncorrect format'
         istat=1
         return
      endif


      headend=0
      do while (istat.eq.0)
         read(lu1,'(a)',iostat=istat) line
         if (istat.eq.0) then
            if (line(:9).eq.'head_end:') then
               headend=1
               istat=1
            else if (line(:9).eq.'version: ') then
               if (line(10:11).ne.'1 ') then
                  write(*,*)
     $                 'SSP_FILES_READ: SSPs file has uncorrect version'
                  istat=1
                  return
               endif
            else if (line(:11).eq.'grid_type: ') then
               if(line(12:).ne.grid_type) then
                  write(*,*)
     $                 'SSP_FILES_READ: SSPs grid_type is uncorrect'
                  istat=1
                  return
               endif
            endif
         endif
      enddo

      if(headend.ne.1) then
         write(*,*)
     $        'SSP_FILES_READ: SSPs file ended before end of header'
         istat=1
         return
      endif

      iindex=1
      iindextot=0
      fileSSPs_prefix=fileSSPs
      do while (iindex.gt.0) 
         iindex=index(fileSSPs_prefix(iindextot+1:
     $        str_length(fileSSPs_prefix)),'/')
         iindextot=iindextot+iindex
      enddo
      
      fileSSPs_prefix=fileSSPs(:iindextot)


      lu2=lu1+1
      nZm=nZ
      nZ=0
      do i=1,nZm
         read(lu1,'(a)',end=999) fileSSPZ(i)

         fileSSPZ(i)=fileSSPs_prefix(:str_length(fileSSPs_prefix))
     $        //fileSSPZ(i)


         call file_open(fileSSPZ(i),Lu2,istat)      
         if(istat.ne.0) then
            print *,'SSP_FILES_READ: Could not open SSP file ',
     $           fileSSPZ(i)
            return
         endif
         read(lu2,'(a)') a
         read(lu2,'(29x,a)') line
         if(line.ne.grid_type) then
            write(*,*)'SSP_FILES_READ: SSP grid_type is uncorrect'
            istat=1
            return
         endif
         read(lu2,'(29x,f5.4)') ZSSP(i)
         close(lu2)
         nZ=nZ+1
      end do
 999  close(lu1)     
* The files are ordered according to increasing metallicity.
      if (nZ.gt.1 ) then
       do i=1,nZ-1 
         do j=i+1,nZ
            if (ZSSP(j).lt.ZSSP(i)) then
               Zexch=ZSSP(i)
               ZSSP(i)=ZSSP(j)
               ZSSP(j)=Zexch
               fileexch=fileSSPZ(i)
               fileSSPZ(i)=fileSSPZ(j)
               fileSSPZ(j)=fileexch
            end if
         end do
       end do
      endif

      return
      end

c------------------------------------------------------------------------------
***** Read header of SSP files
      subroutine ssp_head_read(nZ,fileSSP,header,istat)
      implicit none

      integer       nZ
      character*(*) fileSSP(*)
      character*100 header(4,nz)
      integer istat

      integer i,p
      integer lun

      istat=0

      do p=1,nZ
         call file_open(fileSSP(p),lun,istat)
         if(istat.eq.0) then
            read(lun,'(a)') header(1,p)
            read(lun,'(a)')     ! skip the grid_type
            do i=2,4
               read(lun,'(a)') header(i,p)
            enddo
            close(lun)
         endif
      enddo

      return
      end

c------------------------------------------------------------------------------
      subroutine ssp_data_read(nZ,fileSSPZ,fSNIa,
     $     ntimes,
     $     massalive,
     $     nused,
     $     iused,
     $     fluxbolSSP,
     $     fluxspec,
     $     invtime,
     $     beta,
     $     NLym,
     $     nSNII,
     $     nSNIa,
     $     ejecta,
     $     ejectaZ,
     $     massBHNS,
     $     massWD,
     $     iZ1, 
     $     iZ2,
     $     istat
     $     )
      implicit none
      include 'peg_include.f'

c Input variables
      integer nZ
      character*280 fileSSPZ(nmaxZtracks)
      real fSNIa

c Local variables
      integer      Lun          ! Logical Unit Nuber for file reading
      character*72 a
      integer      i,k,p
      real massBHNS_IS,massBHNS_CB,massWD_IS,massWD_CB
      real ejecta_IS,ejecta_CB,ejectaZ_IS,ejectaZ_CB
      real nSNIaint,nSNII_IS,nSNII_CB
      double precision NHItot,NHItotprev
      real ejectaZint,nSNIIint
      real massBHNSint
      real massWDint
      real ejectaint
      integer timeSSP(nmaxtimesSSP)
      real x

c Output
      integer ntimes                                 ! used by ncountused
      real massalive(nmaxZtracks,nmaxtimes)
      integer nused(nmaxtimesSSP,nmaxZtracks)              ! used by ncountused
      integer iused(nmaxspec,nmaxtimesSSP,nmaxZtracks)  ! used by ncountused
      real fluxbolSSP(nmaxZtracks,nmaxtimesSSP)
      real fluxspec(-nmaxCM:nmaxspecLCB,nmaxtimesSSP,nmaxZtracks)
      integer invtime(nmaxtimes)
      real beta(nmaxtimes)
      double precision NLym(nmaxZtracks,nmaxtimes)
      real nSNII(nmaxZtracks,nmaxtimes)
      real nSNIa(nmaxZtracks,nmaxtimes)
      real ejecta(nmaxZtracks,nmaxtimes)
      real ejectaZ(nmaxZtracks,nmaxtimes)
      real massBHNS(nmaxZtracks,nmaxtimes)
      real massWD(nmaxZtracks,nmaxtimes)
      integer iZ1(nmaxZtracks), iZ2(nmaxZtracks)
      integer istat

c----------------------------------------------------------------------
c- AL-21/7/01: Pas besoin de calculer fluxSSP ici puisqu'on
c          effectuera la convolution sur les numeros de spectres.

      write(*,*) ' Reading SSP files...'

      do p=1,nZ
         call file_open(fileSSPZ(p),Lun,istat)

c        variable "header" already read by data_ssphead_r, just skip
         do i=1,5
            read(Lun,'(a)') a
         end do
         read(Lun,*) ntimes, i, i, i, iZ1(p), iZ2(p)

         massalive(p,1)=1.
	 NHItotprev=0.
         do i=0,ntimes-1

c           Read bolometric contributions of each spectrum.
            read(Lun,*) timeSSP(i+1),nused(i+1,p),
     $           x,fluxbolSSP(p,i+1),NHItot
           
            read(Lun,*) nSNII_IS,nSNII_CB,nSNIaint,
     $           massBHNS_IS,massBHNS_CB,massWD_IS,massWD_CB
            read(Lun,*) ejecta_IS,ejecta_CB,ejectaZ_IS,ejectaZ_CB
            read(Lun,*) (iused(k,i+1,p), fluxspec(iused(k,i+1,p),i+1,p),
     &            k=1,nused(i+1,p))

            nSNIIint=(1.-fSNIa)*nSNII_IS+fSNIa*nSNII_CB
            nSNIaint=fSNIa*nSNIaint
            massBHNSint=(1.-fSNIa)*massBHNS_IS+fSNIa*massBHNS_CB
            massWDint=(1.-fSNIa)*massWD_IS+fSNIa*massWD_CB
            ejectaint=(1.-fSNIa)*ejecta_IS+fSNIa*ejecta_CB
            ejectaZint=(1.-fSNIa)*ejectaZ_IS+fSNIa*ejectaZ_CB


	    if (i.eq.0) then
                 NHItotprev=NHItot
	    else
              do k=timeSSP(i)+1,timeSSP(i+1)    
                 invtime(k)=i
                 beta(k)=(timeSSP(i+1)-k+1.)/(timeSSP(i+1)-timeSSP(i))
                 NLym(p,k)=NHItotprev+(NHItot-NHItotprev)
     $                *(k-timeSSP(i)-1.)/(timeSSP(i+1)-timeSSP(i))
                 nSNII(p,k)=nSNIIint
                 nSNIa(p,k)=nSNIaint
                 ejecta(p,k)=ejectaint
                 ejectaZ(p,k)=ejectaZint
                 massBHNS(p,k)=massBHNSint
                 massWD(p,k)=massWDint
                 massalive(p,k+1)=massalive(p,k) 
     $                -ejecta(p,k)-massBHNS(p,k)-massWD(p,k)
              end do
	    end if
            NHItotprev=NHItot
         end do
         close(Lun)

      end do  ! end of loop   p=1,nZ

      invtime(timeSSP(ntimes)+1)=ntimes-1
      beta(timeSSP(ntimes)+1)=0.

      write(*,*) ' Finished reading SSP files'
      istat=0

      return
      end
