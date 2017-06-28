CDOC ...................................................................
c      call read_write_real(lur,luw, > x)
c      call read_write_char(lur,luw, > a)
c      call read_write_integer(lur,luw, > i)
c      call bracket(n,x,t,i)
c      call file_unit ( > lun)
c         Determine a free logical unit number
C      call file_create( <> newfile, > Lun,istat)
C         create a new file, if "newfile" already exists, add some "+" 
C      call file_open( <> filename, > Lun,istat)
C         open an existing file for read access. If filename is blank
C         prompts the user for a name.
C
C      call read_char(a,adef,error)
C      call read_real(x,xdef,error)
C      call read_integer(i,idef,error)
CDOC ...................................................................
      subroutine read_write_real(ur,uw,x)

      implicit none
      character*72 a
      integer ur,uw
      real x

      read(ur,'(a)') a
      write(uw,'(a)') a
      a=a(index(a,':')+1:)
      read(a,*) x

      end

******

      subroutine read_write_char(ur,uz,a)
      
      implicit none
      character*72 a
      integer ur,uz,i

      read(ur,'(a)') a
      write(uz,'(a)') a
      i=index(a,':')
      i=i+1
      do while (a(i:i).eq.' ') 
         i=i+1
      end do
      a=a(i:)

      end

******

      subroutine read_write_integer(ur,uw,i)

      implicit none
      character*72 a
      integer ur,uw,i

      read(ur,'(a)') a
      write(uw,'(a)') a
      a=a(index(a,':')+1:)
      read(a,*) i

      end

***** 
c Search the largest index i in x(n) such that x(i) < t
c If in entry i is 0, do a dichotomic search over the whole intervalle 1..n
c Otherwyse search in i .. n
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

      end

CDOC ...................................................................
C    Nom du module      subroutine file_unit (lun)
C    
C    Application        Return the number of a free logical unit number
C
C    Auteur             PhP
C
C    Methode      Scan the possible range of logical unit number
C                 to find a free one.
C                 It starts from the input value of lun and return
C                 the first free lun encountered.
C
C                 If no free lun is available, lu=-1 is returned
C
C                 Note that the corresponding unit is not assigned,
C                 ie. it is still free after getlun has been called.
C
C    Arguments          lun    I   I/O logical unit number
C
C    Sous_Programmes    
C
CDOC ...................................................................
      subroutine file_unit (lun)
      implicit none
      integer   lun
      logical   ope
c	  
      ope=.true.
c
c      lun=lun-1
      if(lun.gt.100)lun=0
      do while(lun.lt.100.and.ope)
         lun=lun+1
         inquire(unit=lun,opened=ope)
      enddo
      if(lun.eq.100) then
         lun=-1
         return
      endif
c     
      return
      end
c

c--------------------------------------------------------------------
c file_create creates a new file named newfile if no such file yet exists
c otherwise tries to create a file with some '+' appended, return with 
c non-zero istat if it fails...
      subroutine file_create(newfile,Lun,istat)
      implicit none
      character*(*) newfile
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
                  write(*,*) 'a new file named ',
     $                 newfile(1:index(newfile,' ')),
     $                 'has been created.'
                  write(*,*) ' '
               endif
            else
               write(*,*) 'Failed to create a new file'
               istat=999
            endif
         end do
      endif

      return
      end

***********

      subroutine file_open(filename,Lun,istat)
      implicit none
      character*(*) filename
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
            write(*,'(a,$)')'Name of the file:'
            read (*,'(a)')filename
            open(lun,file=filename,status='old',iostat=istat)
         enddo
      endif

      return
      end 

***********

      subroutine read_real(x,xdef,error)

      implicit none
      integer error
      real x,xdef
      character*72 a

      read(*,'(a)') a
      if (a.ne.' ') then
         read(a,*,iostat=error) x
      else
         x=xdef
         error=0
      end if

      end

*************

      subroutine read_char(a,adef,error)
      
      implicit none
      integer error
      character*72 a,adef
      
      read(*,'(a)') a
      if (a.eq.'Y') a='y'
      if (a.eq.'N') a='n'
      if (a.ne.'y'.and.a.ne.'n'
     $     .and.a.ne.' ') then
         error=1
         write(*,*) 'Invalid input!'  
      else
         if (a.eq.' ') a=adef
         error=0
      end if

      end

***********

      subroutine read_integer(i,idef,error)

      implicit none
      integer error
      integer i,idef
      character*72 a

      read(*,'(a)') a
      if (a.ne.' ') then
         read(a,*,iostat=error) i
      else
         i=idef
         error=0
      end if

      end
**************
CDOC ...................................................................
C    Nom du module  function str_length (string)
C    
C    Application    
C        Return the useful length of a string
C
C    Auteur         Ph. Prugniel
C
C    Methode
C        Return the length of the string (prompt), trimming the trailing 
C        blank and non-printable characters (Null, CR, ...)
C
C    Arguments       
C       prompt  C*  Input String to analyse
C
C    Sous_Programmes None
C
CDOC ...................................................................
      integer function str_length(PROMPT)
      implicit none
      character*(*) prompt

      str_length=len(prompt)

      do while (prompt(str_length:str_length).LE.' '.and.str_length.gt.0)
         str_length=str_length-1.
      enddo

      return
      end


**************
