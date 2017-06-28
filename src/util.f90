MODULE util

  use constants
  use types

  !DOC ...................................................................
  !      call read_write_real(lur,luw, > x)
  !      call read_write_char(lur,luw, > a)
  !      call read_write_integer(lur,luw, > i)
  !      call bracket(n,x,t,i)
  !      call file_unit ( > lun)
  !         Determine a free logical unit number
  !      call file_create( <> newfile, > Lun,istat)
  !         create a new file, if "newfile" already exists, add some "+" 
  !      call file_open( <> filename, > Lun,istat)
  !         open an existing file for read access. If filename is blank
  !         prompts the user for a name.
  !
  !      call read_char(a,adef,error)
  !      call read_real(x,xdef,error)
  !      call read_integer(i,idef,error)
  !DOC ...................................................................
CONTAINS
  subroutine read_write_real(ur,uw,x)

    implicit none
    character(len=72) a
    integer ur,uw
    real x

    read(ur,'(a)') a
    write(uw,'(a)') a
    a=a(index(a,':')+1:)
    read(a,*) x

  end subroutine read_write_real

  !*****

  subroutine read_write_char(ur,uz,a)

    implicit none
    character(len=72) a
    integer ur,uz,i

    read(ur,'(a)') a
    write(uz,'(a)') a
    i=index(a,':')
    i=i+1
    do while (a(i:i).eq.' ') 
       i=i+1
    end do
    a=a(i:)

  end subroutine read_write_char

  !*****

  subroutine read_write_integer(ur,uw,i)

    implicit none
    character(len=72) :: a
    integer ur,uw,i

    read(ur,'(a)') a
    write(uw,'(a)') a
    a=a(index(a,':')+1:)
    read(a,*) i

  end subroutine read_write_integer

  !**** 
  ! Search the largest index i in x(n) such that x(i) < t
  ! If in entry i is 0, do a dichotomic search over the whole intervalle 1..n
  ! Otherwyse search in i .. n
  subroutine bracket(n,x,t,i)

    implicit none
    integer n,iinf,isup,imed,i,di,niter
    real x(n),t,xinf,xsup

    if (i.eq.0) then
       niter=0
       iinf=1
       isup=n
       if (t.gt.x(iinf)) then
          if (t.ge.x(isup)) then
             iinf=isup-1
          else
             do while (iinf+1.lt.isup)
                niter=niter+1
                imed=(iinf+isup)/2
                if (t.le.x(imed)) then
                   isup=imed
                else
                   iinf=imed
                end if
             end do
          end if
       end if
    else
       niter=0
       iinf=1
       isup=n
       if (t.gt.x(iinf)) then
          if (t.ge.x(isup)) then
             iinf=isup-1
          else
             di=1
             if (t.ge.x(i)) then
                iinf=i
                isup=min(n,i+di)
                xsup=x(isup)
                do while(t.gt.xsup) 
                   niter=niter+1
                   di=2*di
                   isup=min(n,i+di)
                   xsup=x(isup)
                enddo
             else
                isup=i
                iinf=max(1,i-di)
                xinf=x(iinf)
                do while(t.lt.xinf) 
                   niter=niter+1
                   di=2*di
                   iinf=max(1,i-di)
                   xinf=x(iinf)
                enddo
             endif
             do while (iinf+1.lt.isup)
                niter=niter+1
                imed=(iinf+isup)/2
                if (t.le.x(imed)) then
                   isup=imed
                else
                   iinf=imed
                end if
             end do
          end if
       end if
    end if
    i=iinf

  end subroutine bracket

  !DOC ...................................................................
  !    Nom du module      subroutine file_unit (lun)
  !    
  !    Application        Return the number of a free logical unit number
  !
  !    Auteur             PhP
  !
  !    Methode      Scan the possible range of logical unit number
  !                 to find a free one.
  !                 It starts from the input value of lun and return
  !                 the first free lun encountered.
  !
  !                 If no free lun is available, lu=-1 is returned
  !
  !                 Note that the corresponding unit is not assigned,
  !                 ie. it is still free after getlun has been called.
  !
  !    Arguments          lun    I   I/O logical unit number
  !
  !    Sous_Programmes    
  !
  !DOC ...................................................................
  subroutine file_unit (lun)
    implicit none
    integer   lun
    logical   ope
    !	  
    ope=.true.
    !
    lun=lun-1
    if(lun.gt.100)lun=0
    do while(lun.lt.100.and.ope)
       lun=lun+1
       inquire(unit=lun,opened=ope)
    enddo
    if(lun.eq.100) then
       lun=-1
       return
    endif
    !     
    return
  end subroutine file_unit
  !

  !--------------------------------------------------------------------
  ! file_create creates a new file named newfile if no such file yet exists
  ! otherwise tries to create a file with some '+' appended, return with 
  ! non-zero istat if it fails...
  subroutine file_create(newfile,Lun,istat)
    implicit none
    character(len=*) :: newfile
    integer       Lun
    integer       Istat

    integer i

    lun=10
    call file_unit(lun)
    if(lun.lt.0)then
       istat=1
       return
    endif
    istat=0

    open(Lun,status='new',file=newfile,iostat=istat)
    !      write(*,*) ' newfile error = ', istat
    if (istat.ne.0) then
       write(*,*) newfile(1:index(newfile,' ')), 'already existed;'
       do while (istat.ne.0)
          i=index(newfile,' ')
          if(i.gt.0) then
             newfile=newfile(1:i-1)//'+'
             open(Lun,status='new',file=newfile,iostat=istat)
             if (istat.eq.0) then
                write(*,*) 'a new file named ',&
                     newfile(1:index(newfile,' ')),&
                     'has been created.'
                write(*,*) ' '
             endif
          else
             write(*,*) 'Failed to create a new file'
             istat=999
          endif
       end do
    endif

    return
  end subroutine file_create

  !**********

  subroutine file_open(filename,Lun,istat)
    implicit none
    character(len=*) :: filename
    integer       Lun,istat

    istat=1
    lun=10
    call file_unit(lun)
    if(lun.lt.0)then
       return
    endif

    istat=0
    if(filename.gt.' ')then
       open(lun,file=filename,status='old',iostat=istat)
    else
       istat=1
       do while(istat.ne.0)
          write(*,'(a)')'Name of the file:'
          read (*,'(a)')filename
          open(lun,file=filename,status='old',iostat=istat)
       enddo
    endif

    return
  end subroutine file_open

  !**********

  subroutine read_real(x,xdef,error)

    implicit none
    integer error
    real x,xdef
    character(len=72) a

    read(*,'(a)') a
    if (a.ne.' ') then
       read(a,*,iostat=error) x
    else
       x=xdef
       error=0
    end if

  end subroutine read_real

  !************

  subroutine read_char(a,adef,error)

    implicit none
    integer error
    character(len=72) ::  a,adef

    read(*,'(a)') a
    if (a.eq.'Y') a='y'
    if (a.eq.'N') a='n'
    if (a.ne.'y'.and.a.ne.'n'&
         .and.a.ne.' ') then
       error=1
       write(*,*) 'Invalid input!'  
    else
       if (a.eq.' ') a=adef
       error=0
    end if

  end subroutine read_char

  !**********

  subroutine read_integer(i,idef,error)

    implicit none
    integer error
    integer i,idef
    character(len=72) ::  a

    read(*,'(a)') a
    if (a.ne.' ') then
       read(a,*,iostat=error) i
    else
       i=idef
       error=0
    end if

  end subroutine read_integer
  !*************
  !DOC ...................................................................
  !    Nom du module  function str_length (string)
  !    
  !    Application    
  !        Return the useful length of a string
  !
  !    Auteur         Ph. Prugniel
  !
  !    Methode
  !        Return the length of the string (prompt), trimming the trailing 
  !        blank and non-printable characters (Null, CR, ...)
  !
  !    Arguments       
  !       prompt  C*  Input String to analyse
  !
  !    Sous_Programmes None
  !
  !DOC ...................................................................
  integer function str_length(PROMPT)
    implicit none

    character(len=*) :: prompt

    str_length=len(prompt)

    do while (prompt(str_length:str_length).LE.' '.and.str_length.gt.0)
       str_length=str_length-1
    enddo

    return
  end function str_length

  !-----------------------------------------------------------------------
  subroutine Steffen(n,x,y,condition,t,z,i) 
    !     Steffen M. (1990), A&A 443, 450                            

    implicit none

    integer n,i
    real x(n),y(n)
    real a,b,c,d,t,z
    real h_i,h_im1,h_ip1
    real s_i,s_im1,s_ip1
    real p_i,p_ip1,y1_i,y1_ip1
    real tmx_i
    logical condition

    if (i.eq.1) then
       h_i=x(i+1)-x(i)
       s_i=(y(i+1)-y(i))/h_i
       h_ip1=x(i+2)-x(i+1)
       s_ip1=(y(i+2)-y(i+1))/h_ip1
       p_ip1=(s_i*h_ip1+s_ip1*h_i)/(h_i+h_ip1)
       y1_ip1=(sign(1.,s_i)+sign(1.,s_ip1))&
            *min(abs(s_i),abs(s_ip1),0.5*abs(p_ip1))
       if (condition) then    
          !     1st derivative is null in x=0
          y1_i=(6.*s_i*x(1)*(x(1)+h_i)&
               -y1_ip1*x(1)*(3.*x(1)+2.*h_i))&
               /(3.*x(1)**2+4.*x(1)*h_i+h_i**2)
       else
          y1_i=1.5*s_i-0.5*y1_ip1
       end if
    else
       if (i.eq.n-1) then
          h_i=x(i+1)-x(i)
          s_i=(y(i+1)-y(i))/h_i
          h_im1=x(i)-x(i-1)
          s_im1=(y(i)-y(i-1))/h_im1
          p_i=(s_im1*h_i+s_i*h_im1)/(h_im1+h_i)
          y1_i=(sign(1.,s_im1)+sign(1.,s_i))&
               *min(abs(s_im1),abs(s_i),0.5*abs(p_i))
          y1_ip1=1.5*s_i-0.5*y1_i
       else
          h_i=x(i+1)-x(i)
          s_i=(y(i+1)-y(i))/h_i
          h_im1=x(i)-x(i-1)
          s_im1=(y(i)-y(i-1))/h_im1
          h_ip1=x(i+2)-x(i+1)
          s_ip1=(y(i+2)-y(i+1))/h_ip1
          p_i=(s_im1*h_i+s_i*h_im1)/(h_im1+h_i)
          p_ip1=(s_i*h_ip1+s_ip1*h_i)/(h_i+h_ip1)
          y1_i=(sign(1.,s_im1)+sign(1.,s_i))&
               *min(abs(s_im1),abs(s_i),0.5*abs(p_i))
          y1_ip1=(sign(1.,s_i)+sign(1.,s_ip1))&
               *min(abs(s_i),abs(s_ip1),0.5*abs(p_ip1))
       end if
    end if
    if (t.gt.x(n)) then
       z=y(n)+y1_ip1*(t-x(n))
    else
       tmx_i=t-x(i)
       a=(y1_i+y1_ip1-2.*s_i)/h_i**2
       b=(3.*s_i-2.*y1_i-y1_ip1)/h_i
       c=y1_i
       d=y(i)
       z=((a*tmx_i+b)*tmx_i+c)*tmx_i+d
    endif

  end subroutine Steffen

  !--------------------------------------------------------------------------
  subroutine Steffen3D(x1,x2,x3,y,n1,n2,n3,&
       t1,t2,t3,z,i01,i02,i03)

    implicit none

    integer n1,n2,n3,i1,i2,i3,n,i01,i02,i03
    parameter(n=100)
    real t1,t2,t3,z,x1(n1),x2(n2),x3(n3)
    real y(n1,n2,n3),y1(n),y2(n),y3(n)

    call bracket(n1,x1,t1,i01)
    call bracket(n2,x2,t2,i02)
    call bracket(n3,x3,t3,i03)
    do i1=max(1,i01-1),min(n1,i01+2)
       do i2=max(1,i02-1),min(n2,i02+2)
          do i3=max(1,i03-1),min(n3,i03+2)
             y3(i3)=y(i1,i2,i3)
          end do
          call Steffen(n3,x3,y3,.false.,t3,y2(i2),i03)
       end do
       call Steffen(n2,x2,y2,.false.,t2,y1(i1),i02)
    end do
    call Steffen(n1,x1,y1,.false.,t1,z,i01)

  end subroutine Steffen3D

  !--------------------------------------------------------------------------
  subroutine Steffen4D(x1,x2,x3,x4,y,n1,n2,n3,n4,&
       t1,t2,t3,t4,z,i01,i02,i03,i04)

    implicit none

    integer n1,n2,n3,n4,i1,i2,i3,i4,n,i01,i02,i03,i04
    parameter(n=100)
    real t1,t2,t3,t4,z,x1(n1),x2(n2),x3(n3),x4(n4)
    real y(n1,n2,n3,n4),y1(n),y2(n),y3(n),y4(n)

    call bracket(n1,x1,t1,i01)
    call bracket(n2,x2,t2,i02)
    call bracket(n3,x3,t3,i03)
    call bracket(n4,x4,t4,i04)
    do i1=max(1,i01-1),min(n1,i01+2)
       do i2=max(1,i02-1),min(n2,i02+2)
          do i3=max(1,i03-1),min(n3,i03+2)
             do i4=max(1,i04-1),min(n4,i04+2)
                y4(i4)=y(i1,i2,i3,i4)
             end do
             call Steffen(n4,x4,y4,.true.,t4,y3(i3),i04)
          end do
          call Steffen(n3,x3,y3,.false.,t3,y2(i2),i03)
       end do
       call Steffen(n2,x2,y2,.false.,t2,y1(i1),i02)
    end do
    call Steffen(n1,x1,y1,.false.,t1,z,i01)

  end subroutine Steffen4D

!**** 

double precision function interplinlin(x1,x2,y1,y2,t)

  implicit none
  double precision x1,x2,y1,y2,t

  interplinlin=y1+(y2-y1)*(t-x1)/(x2-x1)

end function interplinlin

!**** 

double precision function interploglog(x1,x2,y1,y2,t)

  implicit none
  double precision x1,x2,y1,y2,t,eps
  parameter(eps=1.d-37)

  if (y1.gt.eps.and.y2.gt.eps) then      
     interploglog=y1*(y2/y1)**(log(t/x1)/log(x2/x1))
  else
     interploglog=0.d0
  endif

end function interploglog


!---------------------------------------------------------------------------
  real function interplinlog(x1,x2,y1,y2,x)

    implicit none


    real x1,x2,y1,y2,x,epsilon

    epsilon=1.e-37
    if (abs(x2-x1).lt.epsilon) then
       interplinlog=y1
    else
       if (y1.le.epsilon.or.y2.le.epsilon) then
          interplinlog=0.
       else
          interplinlog=y1*(y2/y1)**((x-x1)/(x2-x1))
       endif
    endif

  end function interplinlog

!---------------------------------------------------------------------------
  subroutine choose_stellib(stellib, status)

    implicit none


    character(len=*) ::  stellib
    integer status

    integer unit, n_stellib,  i_stellib
    character(len=280) ::  stellib_i(nmaxstellibs)
    character(len=280) ::  stellibcour

    unit=10
    call file_open(trim(PEG_ROOT)//'/data/user_defined/list_stellib.dat',&
         unit,status)
    if(status.ne.0) return

    n_stellib = 0
    do while(status .eq. 0)
       read(unit, '(a)', iostat = status) stellib_i(n_stellib+1)
       if (status .eq. 0) n_stellib = n_stellib+1
    enddo
    close(unit)

    write(*,*) ' '
    write(*,*) 'Library of stellar spectra?'
    do i_stellib = 1, n_stellib
       stellibcour=stellib_i(i_stellib)
       write(*,'(1x,i3,a,a)') i_stellib, ': ', &
            stellibcour(:str_length(stellibcour))
    enddo
    read(*,*,iostat=status) i_stellib
    if (status.ne.0) return

    stellib = stellib_i(i_stellib)

  end subroutine choose_stellib


  !*************
end MODULE util
