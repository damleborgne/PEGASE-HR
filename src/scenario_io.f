c  scenario_io, IO to scenario file (actually only reading)
c  PhP, 2001/07/13
c  Part of the Pegase package
c-----------------------------------------------------------------------------
c
c      call scenario_open(filescenarios, > istat)
c        Open the scenario file.
c        The filename can be given in entry. If this name is blank or if the
c        file is not found, the routine prompts the user for a validd name.
c
c        The scenario file remains connected with an internal unit number which
c        is used until scenario_close is called.
c
c      call scenario_close( > istat)
C        Disconnect the scenario file
c
c  lread -> read a card given its line in the file
c      call scenario_lread_char(bloc,line, > name,string,istat)
c      call scenario_lread_real(bloc,line, > name,rval,istat)
c      call scenario_lread_int(bloc,line, > name,ival,istat)
c  nread -> read next card
c      call scenario_nread_char(name, > string,istat)
c      call scenario_nread_real(name, > rval,istat)
c      call scenario_nread_int(name, > ival,istat)
c        Read data from the scenario file. "char" "real" and "int" indicate 
c        the type of data.
c        The scenario file is structured in blocs ended with a line starting
c        the the "*" character.
c        The first bloc contains the name of the SSP files.
C        The following blocs (at minimum 1) describe the evolution scenario.
c        Each line in the file can consists of 2 fields, first a keyword "name"
c        and second, its "value". They are separated by ":"; if there is no 
c        separator, the only field is the "value". The "read" routines return
c        the value.
c        The "lread" routines access directly to a given bloc and line.
c        The "nread" routines read the next line from the current position.
c
c        The "normal" return status is 0. A status=4444 means that we read a
c        bloc separator.
c
c      call scenario_SSPs_r( > fileSSPs,fSNIa,libchoice,istat)
c        Read the SSP files, first bloc of the scenario file
c
c      call scenario_sfr_r(Nscenario, > 
c     $     filespectra,Zgas,tinfall,Zinfall,infall,SFRparam, 
c     $     fileSFR,codeZ,ZSFR,fsub,twind,
c     $     answerneb,codeext,inclin,
c     $     istat)
c        Read the SFR from bloc "Nscenario" in scenario file.
c        If "No galactic wind", twind is returned = -99 (we do not know 
c           nmaxtimes here...) 
c------------------------------------------------------------------------------

c**** Opening of the file containing the scenarios.
      subroutine scenario_open(filescenarios,istat)
      implicit none
      character*(*) filescenarios
      integer       istat
      
      integer       Lscenario,Sbloc,Sline ! Unit number, current bloc and line
      common /pegase_scenario/Lscenario,Sbloc,Sline 
      integer error

      istat=0
      error=1
      Lscenario=50
      Sbloc=0
      Sline=0
      call file_unit(Lscenario)
      if(Lscenario.le.0) then
         istat=1
         return
      endif

      if (filescenarios.le.' ') then
         write(*,*) 'Name of the file containing the parameters', 
     $        ' of the star formation scenarios?'
         read(*,'(a)') filescenarios
      endif
      do while (error.ne.0) 
         open(Lscenario,file=filescenarios,status='old',iostat=error)
         if (error.ne.0) then
            write(*,*) 'This file does not exist!'
            write(*,*) 'Name of the file containing the',
     $           ' parameters of the star formation scenarios?'
            read(*,'(a)') filescenarios
         end if
      end do

      Sbloc=1 ! Current position in file
      Sline=1 ! Current position in file

      return
      end
c
c------------------------------------------------------------------------------
      subroutine scenario_close(istat)
      implicit none
      integer istat
      integer       Lscenario,Sbloc,Sline ! Unit number, current bloc and line
      common /pegase_scenario/Lscenario,Sbloc,Sline 

      close(Lscenario)
      Lscenario=-1
      Sbloc=0
      Sline=0

      istat=0
      return
      end

c
c------------------------------------------------------------------------------
      subroutine scenario_lread_char(bloc,line,name,string,istat)
      implicit none
      integer       bloc,line
      character*(*) name
      character*(*) string
      integer istat
      integer       Lscenario,Sbloc,Sline ! Unit number, current bloc and line
      common /pegase_scenario/Lscenario,Sbloc,Sline 
      character*80 a

      integer iek,i

      istat=0
      if (Lscenario.le.0.or.Sbloc.le.0.or.bloc.le.0.or.line.le.0) then
         istat=1
         return
      endif

      if(bloc.lt.Sbloc) then
         rewind(Lscenario)
         Sbloc=1
         Sline=1
      elseif(bloc.eq.Sbloc.and.line.lt.Sline) then
         rewind(Lscenario)
         Sbloc=1
         Sline=1        
      endif
      do while (bloc.gt.Sbloc) 
         iek=0
         do while (iek.eq.0)
            read(Lscenario,'(a)',iostat=iek)a
            if(iek.eq.0.and.a(1:1).eq.'*') iek=-1
         enddo
         if(a(1:1).eq.'*') then
            Sbloc=Sbloc+1
            Sline=1
         else
            istat=2
            return
         endif
      enddo

      do while (line.gt.Sline)
         read(Lscenario,'(a)',iostat=iek)a
         if (iek.ne.0) then
            istat=3
            return
         elseif (a(1:1).eq.'*') then
            Sline=1
            Sbloc=Sbloc+1
            istat=3
            return
         endif
         Sline=Sline+1
      enddo

      read(Lscenario,'(a)',iostat=istat)string
      if(istat.eq.0) then
         if(string(1:1).ne.'*') then
            Sline=Sline+1
            i=index(string,':')+1
            if(i.gt.2) name=string(:i-1)
            string=string(i:)
            return
         else
            Sbloc=Sbloc+1
            istat=4444
            Sline=1
         endif
      endif
            
      return
      end

c
c------------------------------------------------------------------------------
      subroutine scenario_nread_char(name,string,istat)
      implicit none
      character*(*) name
      character*(*) string
      integer istat
      integer       Lscenario,Sbloc,Sline ! Unit number, current bloc and line
      common /pegase_scenario/Lscenario,Sbloc,Sline 

      integer i

      istat=0
      if (Lscenario.le.0.or.Sbloc.le.0) then
         istat=1
         return
      endif

      read(Lscenario,'(a)',iostat=istat)string
      if(istat.eq.0) then
         if(string(1:1).ne.'*') then
            Sline=Sline+1
            i=index(string,':')+1
            if(i.gt.2) name=string(:i-1)
            string=string(i:)
            return
         else
            Sbloc=Sbloc+1
            istat=4444
            Sline=1
         endif
      endif
            
      return
      end

c
c------------------------------------------------------------------------------
      subroutine scenario_lread_real(bloc,line,name,rval,istat)
      implicit none
      integer       bloc,line
      character*(*) name
      real          rval
      integer istat
      character*80 string

      rval=-1.e36
      call scenario_lread_char(bloc,line,name,string,istat)
      if(istat.ne.0) return

      read(string,*,iostat=istat)rval

      return
      end
      
c
c------------------------------------------------------------------------------
      subroutine scenario_lread_int(bloc,line,name,ival,istat)
      implicit none
      integer       bloc,line
      character*(*) name
      integer       ival
      integer istat
      character*80 string

      ival=-999
      call scenario_lread_char(bloc,line,name,string,istat)
      if(istat.ne.0) return

      read(string,*,iostat=istat)ival

      return
      end
      
c
c------------------------------------------------------------------------------
      subroutine scenario_nread_real(name,rval,istat)
      implicit none
      character*(*) name
      real          rval
      integer istat
      character*80 string

      rval=-1.e36
      call scenario_nread_char(name,string,istat)
      if(istat.ne.0) return
      read(string,*,iostat=istat)rval

      return
      end
      
c
c------------------------------------------------------------------------------
      subroutine scenario_nread_int(name,ival,istat)
      implicit none
      character*(*) name
      integer       ival
      integer istat
      character*80 string

      ival=-999
      call scenario_nread_char(name,string,istat)
      if(istat.ne.0) return
      read(string,*,iostat=istat)ival

      return
      end
      

c
c------------------------------------------------------------------------------
***** Reading of the names of the SSPs files, fraction of close binaries,
*     and name of the stellar library. (1st bloc of scenario file)
      subroutine scenario_header_r(fileSSPs,fSNIa,stellib,istat)
      implicit none
      character*(*) fileSSPs
      real          fSNIa
      character*(*) stellib
      integer       istat
      integer       i
      character*72  name

      istat=0
      call scenario_lread_char(1,1,name,fileSSPs,istat)

      if(istat.ne.0) return
      i=1
      do while (fileSSPs(i:i).eq.' '.and.i.lt.len(fileSSPs)) 
         i=i+1
      end do
      fileSSPs=fileSSPs(i:)
c
      call scenario_nread_real(name,fSNIa,istat)
      if(istat.ne.0) return

      call scenario_nread_char(name,stellib,istat)
      if(istat.ne.0) return
      i=1
      do while (stellib(i:i).le.' '.and.i.lt.len(stellib)) 
         i=i+1
      end do
      stellib=stellib(i:)

      return
      end


c----------------------------------------------------------------------------
c PhP, 2001/07/13: replace param1,param2 by param() (and nparam=2)

c codeZ takes the values: 0: no evolution of Z, 1: consistent evolution, and
C                         2: evolution given in a user's file (typeSFR=-2)
c SFRparam(1) is the type of SFR (SFRtype) 
      subroutine scenario_sfr_r(Nscenario,
     $     filespectra,Zgas,tinfall,Zinfall,infall,SFRparam,fileSFR,
     $     codeZ,ZSFR,fsub,twind,
     $     answerneb,codeext,inclin,
     $     istat)
      implicit none
      
      integer Nscenario

      character*(*) filespectra
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
      integer       istat

      character*72  a,answerinfall,answerz,answerwind,answerext
      integer       i
      integer       typeSFR
      integer       nparam


      istat=0
      call scenario_lread_char(Nscenario+1,1,a,filespectra,istat)
      if (istat.ne.0) return
      i=1
      do while (filespectra(i:i).eq.' ')
         i=i+1
      end do
      filespectra=filespectra(i:)
 
      call scenario_nread_real(a,Zgas(1),istat)
      if(istat.ne.0) return

      call scenario_nread_char(a,answerinfall,istat)
      if(istat.ne.0) return
      if (answerinfall.eq.'Infall') then
         infall=1
         call scenario_nread_real(a,tinfall,istat)
         call scenario_nread_real(a,Zinfall,istat)
      else
         tinfall=1.
         Zinfall=0.
         infall=0
      end if

      call scenario_nread_int(a,typeSFR,istat)      
      if(istat.ne.0) return
      SFRparam(1)=typeSFR
      if (typeSFR.ge.1.and.typeSFR.le.3) then
         SFRparam(2)=2
         call scenario_nread_real(a,SFRparam(3),istat)
         call scenario_nread_real(a,SFRparam(4),istat)
      end if
      if (typeSFR.ge.10) then
         call scenario_nread_int(a,nparam,istat)
         SFRparam(2)=nparam
         do i=1,nparam
            call scenario_nread_real(a,SFRparam(2+i),istat)
         end do
      end if
      if (typeSFR.le.-1) then
         call scenario_nread_char(a,fileSFR,istat)
         i=1
         do while (fileSFR(i:i).le.' '.and.i.lt.len(fileSFR)) 
            i=i+1
         end do
         fileSFR=fileSFR(i:)
      end if
      
      if (typeSFR.ge.-1) then
         call scenario_nread_char(a,answerZ,istat)
         if (index(answerZ,'No').ne.0) then
            codeZ=0
         else
            codeZ=1
         endif
         if (codeZ.eq.0) then
            call scenario_nread_real(a,ZSFR(1),istat)
         else
            ZSFR(1)=Zgas(1)
         end if
      else
         codeZ=2                ! value added PhP 2001/07/14
      endif
      
      call scenario_nread_real(a,fsub,istat)
      if(istat.ne.0) return
      call scenario_nread_char(a,answerwind,istat)
      if(istat.ne.0) return
      if (answerwind.eq.'Galactic winds') then
         call scenario_nread_real(a,twind,istat)
      else
         twind= -99
      end if
      
      call scenario_nread_char(a,answerneb,istat)
      if(istat.ne.0) return
      call scenario_nread_char(a,answerext,istat)
      if(istat.ne.0) return
      if (index(answerext,'No').ne.0) codeext=0
      if (index(answerext,'spheroidal').ne.0) codeext=1
      if (index(answerext,'averaged').ne.0) codeext=2
      if (index(answerext,'specific').ne.0) codeext=3
      if (codeext.eq.3) call scenario_nread_real(a,inclin,istat)
      
      return
      end

