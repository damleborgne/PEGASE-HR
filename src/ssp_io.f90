MODULE ssp_io


  use types
  use constants
  use util


CONTAINS
  ! Pegase Package
  !------------------------------------------------------------------------------
  !       subroutine ssp_files_read(<fileSSPs, <> nZ, > fileSSPZ,ZSSP,istat)
  !          In entry nZ is the maximum allowed number of SSP files, in output
  !          it is the actual number of Z
  !
  !       subroutine ssp_head_read( <nZ,fileSSP, >header,istat)
  !
  !       subroutine ssp_data_read( <nZ,fileSSPZ,fSNIa, >
  !      $    ntimes,
  !      $    massalive,
  !      $    nused,
  !      $    iused,
  !      $    fluxbolSSP,
  !      $    fluxspec,
  !      $    invtime,
  !      $    beta,
  !      $    NLym,
  !      $    nSNII,
  !      $    nSNIa,
  !      $    ejecta,
  !      $    ejectaZ,
  !      $    massBHNS,
  !      $    massWD,
  !      $    istat
  !      $     )
  !------------------------------------------------------------------------------
  !**** Reading of the names of the SSPs files. 
  !        In entry   fileSSPs is the name of the file containing the list of SSP
  !                   grid_type is the type of grid expected for the SSPs
  !                   nZ is the maximum allowed number of SSP files.
  !
  !        The path included in fileSSPs is added to the individual
  !        SSP filesnames listed inside fileSSPs. 
  !
  subroutine ssp_files_read(SSP,grid_type,istat)

    implicit none

    TYPE(t_SSP_SP), INTENT(INOUT)   :: SSP
    character(len=30), intent(IN):: grid_type
    integer                      :: istat

    integer       nzm
    integer       i,j,lu1,lu2
    character(len=280) ::   a
    character(len=280) ::   fileexch
    real          Zexch
    character(len=280) ::  line
    integer       headend
    integer       iindex,iindextot

    istat=0

    call file_open(SSP%fileSSPs,Lu1,istat)      
    if (istat.ne.0) then
       write(*,*)'Error : SSP_FILES_READ: Failed to open SSP file'
       write(*,*) 'istat=',istat
       return
    endif

    read(lu1,'(a)',iostat=istat) line
    if (line(:22).ne.'format: PEGASE-HR/SSPs') then
       write(*,*)'Error: SSP_FILES_READ: SSPs file has uncorrect format'
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
                write(*,*)&
                     'Error: SSP_FILES_READ: SSPs file has uncorrect version'
                istat=1
                return
             endif
          else if (line(:11).eq.'grid_type: ') then
             if(line(12:).ne.grid_type) then
                write(*,*)&
                     'Error: SSP_FILES_READ: SSPs grid_type is uncorrect'
                istat=1
                return
             endif
          endif
       endif
    enddo

    if(headend.ne.1) then
       write(*,*)&
            'Error: SSP_FILES_READ: SSPs file ended before end of header'
       istat=1
       return
    endif

    iindex=1
    iindextot=0
    SSP%fileSSPs_prefix=SSP%fileSSPs
    do while (iindex.gt.0) 
       iindex=index(SSP%fileSSPs_prefix(iindextot+1:&
            str_length(SSP%fileSSPs_prefix)),'/')
       iindextot=iindextot+iindex
    enddo

    SSP%fileSSPs_prefix=SSP%fileSSPs(:iindextot)


    lu2=lu1+1
    nZm=SSP%nZ
    SSP%nZ=0
    do i=1,nZm
       read(lu1,'(a)',end=999) SSP%fileSSPZ(i)

       SSP%fileSSPZ(i)=SSP%fileSSPs_prefix(:str_length(SSP%fileSSPs_prefix))&
            //SSP%fileSSPZ(i)


       call file_open(SSP%fileSSPZ(i),Lu2,istat)      
       if(istat.ne.0) then
          write(*,*) 'Error: SSP_FILES_READ: Could not open SSP file ',&
               SSP%fileSSPZ(i)
          return
       endif
       read(lu2,'(a)') a
       read(lu2,'(29x,a)') line
       if(line.ne.grid_type) then
          write(*,*)'Error: SSP_FILES_READ: SSP grid_type is uncorrect'
          istat=1
          return
       endif
       read(lu2,'(29x,f5.4)') SSP%ZSSP(i)
       close(lu2)
       SSP%nZ=SSP%nZ+1
    end do
999 close(lu1)     
    ! The files are ordered according to increasing metallicity.
    if (SSP%nZ.gt.1 ) then
       do i=1,SSP%nZ-1 
          do j=i+1,SSP%nZ
             if (SSP%ZSSP(j).lt.SSP%ZSSP(i)) then
                Zexch=SSP%ZSSP(i)
                SSP%ZSSP(i)=SSP%ZSSP(j)
                SSP%ZSSP(j)=Zexch
                fileexch=SSP%fileSSPZ(i)
                SSP%fileSSPZ(i)=SSP%fileSSPZ(j)
                SSP%fileSSPZ(j)=fileexch
             end if
          end do
       end do
    endif

    return
  end subroutine ssp_files_read

  !------------------------------------------------------------------------------
  !**** Read header of SSP files
  subroutine ssp_head_read(SSP,istat)

    implicit none

    TYPE(t_SSP_SP), INTENT(INOUT) :: SSP

    integer istat

    integer i,p
    integer lun

    istat=0

    do p=1,SSP%nZ
       call file_open(SSP%fileSSPZ(p),lun,istat)
       if(istat.eq.0) then
          read(lun,'(a)') SSP%header(1,p)
          read(lun,'(a)')     ! skip the grid_type
          do i=2,4
             read(lun,'(a)') SSP%header(i,p)
          enddo
          close(lun)
       endif
    enddo

    return
  end subroutine ssp_head_read

  !------------------------------------------------------------------------------
  subroutine ssp_data_read(SSP,istat)

    implicit none

    !     Input variables
    TYPE(t_SSP_SP), INTENT(INOUT) :: SSP

    !     Local variables
    integer           :: Lun          ! Logical Unit Nuber for file reading
    character(len=72) :: a
    integer           :: i,k,p
    real              :: massBHNS_IS,massBHNS_CB,massWD_IS,massWD_CB
    real              :: ejecta_IS,ejecta_CB,ejectaZ_IS,ejectaZ_CB
    real              :: nSNIaint,nSNII_IS,nSNII_CB
    double precision  :: NHItot,NHItotprev
    real              :: ejectaZint,nSNIIint
    real              :: massBHNSint
    real              :: massWDint
    real              :: ejectaint
    integer           :: timeSSP(nmaxtimesSSP)
    real x

    !     Output
    integer istat

    !----------------------------------------------------------------------
    if (verbose.ge.2) write(*,*) ' Reading SSP files...'

    do p=1,SSP%nZ
       call file_open(SSP%fileSSPZ(p),Lun,istat)

       !     variable "header" already read by data_ssphead_r, just skip
       do i=1,5
          read(Lun,'(a)') a
       end do
       read(Lun,*) SSP%ntimes, i, i, i, SSP%iZ1(p), SSP%iZ2(p)

       SSP%massalive(p,1)=1.
       NHItotprev=0.
       do i=0,SSP%ntimes-1
          !     Read bolometric contributions of each spectrum.
          read(Lun,*) timeSSP(i+1),SSP%nused(i+1,p),&
               x,SSP%fluxbolSSP(p,i+1),NHItot

          read(Lun,*) nSNII_IS,nSNII_CB,nSNIaint,&
               massBHNS_IS,massBHNS_CB,massWD_IS,massWD_CB
          read(Lun,*) ejecta_IS,ejecta_CB,ejectaZ_IS,ejectaZ_CB
          read(Lun,*) (&
               SSP%iused(k,i+1,p), &
               SSP%Lspec_SSP(SSP%iused(k,i+1,p),i+1,p), &
               k=1,SSP%nused(i+1,p)&
               )

          nSNIIint=(1.-SSP%fSNIa)*nSNII_IS+SSP%fSNIa*nSNII_CB
          nSNIaint=SSP%fSNIa*nSNIaint
          massBHNSint=(1.-SSP%fSNIa)*massBHNS_IS+SSP%fSNIa*massBHNS_CB
          massWDint=(1.-SSP%fSNIa)*massWD_IS+SSP%fSNIa*massWD_CB
          ejectaint=(1.-SSP%fSNIa)*ejecta_IS+SSP%fSNIa*ejecta_CB
          ejectaZint=(1.-SSP%fSNIa)*ejectaZ_IS+SSP%fSNIa*ejectaZ_CB


          if (i.eq.0) then
             NHItotprev=NHItot
          else
             do k=timeSSP(i)+1,timeSSP(i+1)    
                SSP%invtime(k)=i
                SSP%beta(k)=(timeSSP(i+1)-k+1.)/(timeSSP(i+1)-timeSSP(i))
                SSP%NLym(p,k)=NHItotprev+(NHItot-NHItotprev)&
                     *(k-timeSSP(i)-1.)/(timeSSP(i+1)-timeSSP(i))
                SSP%nSNII(p,k)=nSNIIint
                SSP%nSNIa(p,k)=nSNIaint
                SSP%ejecta(p,k)=ejectaint
                SSP%ejectaZ(p,k)=ejectaZint
                SSP%massBHNS(p,k)=massBHNSint
                SSP%massWD(p,k)=massWDint
                SSP%massalive(p,k+1)=SSP%massalive(p,k) &
                     -SSP%ejecta(p,k)-SSP%massBHNS(p,k)-SSP%massWD(p,k)
             end do
          end if
          NHItotprev=NHItot
       end do
       close(Lun)

    end do                    ! end of loop   p=1,SSP%nZ

    SSP%invtime(timeSSP(SSP%ntimes)+1)=SSP%ntimes-1
    SSP%beta(timeSSP(SSP%ntimes)+1)=0.

    if (verbose.ge.2) write(*,*) ' Finished reading SSP files'
    istat=0

    return
  end subroutine ssp_data_read
END MODULE ssp_io
