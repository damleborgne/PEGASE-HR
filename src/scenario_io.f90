MODULE scenario_io

  use constants
  use types
  use util

  !  scenario_io, IO to scenario file (actually only reading)
  !  Part of the Pegase package
  !-----------------------------------------------------------------------------
  !
  !      call scenario_open(filescenarios, > istat)
  !        Open the scenario file.
  !        The filename can be given in entry. If this name is blank or if the
  !        file is not found, the routine prompts the user for a validd name.
  !
  !        The scenario file remains connected with an internal unit number which
  !        is used until scenario_close is called.
  !
  !      call scenario_close( > istat)
  !        Disconnect the scenario file
  !
  !  lread -> read a card given its line in the file
  !      call scenario_lread_char(bloc,line, > name,string,istat)
  !      call scenario_lread_real(bloc,line, > name,rval,istat)
  !      call scenario_lread_int(bloc,line, > name,ival,istat)
  !  nread -> read next card
  !      call scenario_nread_char(name, > string,istat)
  !      call scenario_nread_real(name, > rval,istat)
  !      call scenario_nread_int(name, > ival,istat)
  !        Read data from the scenario file. "char" "real" and "int" indicate 
  !        the type of data.
  !        The scenario file is structured in blocs ended with a line starting
  !        the the "*" character.
  !        The first bloc contains the name of the SSP files.
  !        The following blocs (at minimum 1) describe the evolution scenario.
  !        Each line in the file can consists of 2 fields, first a keyword "name"
  !        and second, its "value". They are separated by ":"; if there is no 
  !        separator, the only field is the "value". The "read" routines return
  !        the value.
  !        The "lread" routines access directly to a given bloc and line.
  !        The "nread" routines read the next line from the current position.
  !
  !        The "normal" return status is 0. A status=4444 means that we read a
  !        bloc separator.
  !
  !      call scenario_SSPs_r( > fileSSPs,fSNIa,libchoice,istat)
  !        Read the SSP files, first bloc of the scenario file
  !
  !      call scenario_sfr_r(Nscenario, > 
  !     $     filespectra,Zgas,tinfall,Zinfall,infall,SFRparam, 
  !     $     fileSFR,codeZ,ZSFR,fsub,twind,
  !     $     answerneb,codeext,inclin,
  !     $     istat)
  !        Read the SFR from bloc "Nscenario" in scenario file.
  !        If "No galactic wind", twind is returned = -99 (we do not know 
  !           nmaxtimes here...) 
  !------------------------------------------------------------------------------
CONTAINS
  !**** Opening of the file containing the scenarios.
  subroutine scenario_open(filescenarios,istat)

    implicit none

    character(len=*) filescenarios
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
       write(*,*) 'Name of the file containing the parameters', &
            ' of the star formation scenarios?'
       read(*,'(a)') filescenarios
    endif
    do while (error.ne.0) 
       open(Lscenario,file=filescenarios,status='old',iostat=error)
       if (error.ne.0) then
          write(*,*) 'This file does not exist!'
          write(*,*) 'Name of the file containing the',&
               ' parameters of the star formation scenarios?'
          read(*,'(a)') filescenarios
       end if
    end do

    Sbloc=1 ! Current position in file
    Sline=1 ! Current position in file

    return
  end subroutine scenario_open
  !
  !------------------------------------------------------------------------------
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
  end subroutine scenario_close

  !
  !------------------------------------------------------------------------------
  subroutine scenario_lread_char(bloc,line,name,string,istat)

    implicit none

    integer       bloc,line
    character(len=*) ::  name
    character(len=*) ::  string
    integer istat
    integer       Lscenario,Sbloc,Sline ! Unit number, current bloc and line
    common /pegase_scenario/Lscenario,Sbloc,Sline 
    character(len=80) :: a

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
  end subroutine scenario_lread_char

  !
  !------------------------------------------------------------------------------
  subroutine scenario_nread_char(name,string,istat)

    implicit none

    character(len=*) ::  name
    character(len=*) ::  string
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
  end subroutine scenario_nread_char

  !
  !------------------------------------------------------------------------------
  subroutine scenario_lread_real(bloc,line,name,rval,istat)

    use types

    implicit none

    integer       bloc,line
    character(len=*)  ::  name
    real              ::           rval
    integer           :: istat
    character(len=80) :: string

    rval=-1.e36
    call scenario_lread_char(bloc,line,name,string,istat)
    if(istat.ne.0) return

    read(string,*,iostat=istat)rval

    return
  end subroutine scenario_lread_real

  !
  !------------------------------------------------------------------------------
  subroutine scenario_lread_int(bloc,line,name,ival,istat)

    implicit none

    integer       bloc,line
    character(len=*) ::  name
    integer       ival
    integer istat
    character(len=80) :: string

    ival=-999
    call scenario_lread_char(bloc,line,name,string,istat)
    if(istat.ne.0) return

    read(string,*,iostat=istat)ival

    return
  end subroutine scenario_lread_int

  !
  !------------------------------------------------------------------------------
  subroutine scenario_nread_real(name,rval,istat)

    use types
    implicit none

    character(len=*) ::  name
    real             ::           rval
    integer istat
    character(len=80) :: string

    rval=-1.e36
    call scenario_nread_char(name,string,istat)
    if(istat.ne.0) return
    read(string,*,iostat=istat)rval

    return
  end subroutine scenario_nread_real

  !
  !------------------------------------------------------------------------------
  subroutine scenario_nread_int(name,ival,istat)

    implicit none

    character(len=*) ::  name
    integer       ival
    integer istat
    character(len=80) :: string

    ival=-999
    call scenario_nread_char(name,string,istat)
    if(istat.ne.0) return
    read(string,*,iostat=istat)ival

    return
  end subroutine scenario_nread_int


  !
  !------------------------------------------------------------------------------
  !**** Reading of the names of the SSPs files, fraction of close binaries,
  !     and name of the stellar library. (1st bloc of scenario file)
  subroutine scenario_header_r(SSP,stellib,istat)

    implicit none

    type(t_SSP_SP) :: SSP

    character(len=*) ::  stellib
    integer       istat
    integer       i
    character(len=72) ::  name

    istat=0
    call scenario_lread_char(1,1,name,SSP%fileSSPs,istat)

    if(istat.ne.0) return
    i=1
    do while (SSP%fileSSPs(i:i).eq.' '.and.i.lt.len(SSP%fileSSPs)) 
       i=i+1
    end do
    SSP%fileSSPs=SSP%fileSSPs(i:)
    !
    call scenario_nread_real(name,SSP%fSNIa,istat)

    if(istat.ne.0) return

    call scenario_nread_char(name,stellib,istat)

    if(istat.ne.0) return
    i=1
    do while (stellib(i:i).le.' '.and.i.lt.len(stellib)) 
       i=i+1
    end do
    stellib=stellib(i:)

    return
  end subroutine scenario_header_r


  !----------------------------------------------------------------------------
  ! codeZ takes the values: 0: no evolution of Z, 1: consistent evolution, and
  !                         2: evolution given in a user's file (typeSFR=-2)
  ! SFRparam(1) is the type of SFR (SFRtype) 

  subroutine scenario_sfr_r(myscen,istat)

    implicit none

    TYPE(t_scenario), intent(INOUT) :: myscen
    integer, intent(INOUT)          :: istat

    character(len=72) ::  a
    integer       i
    integer       typeSFR
    integer       nparam


    istat=0
    call scenario_lread_char(myscen%number+1,1,a,myscen%filespectra,istat)
    if (istat.ne.0) return
    i=1
    do while (myscen%filespectra(i:i).eq.' ')
       i=i+1
    end do
    myscen%filespectra=myscen%filespectra(i:)

    call scenario_nread_real(a,myscen%Zgasinit,istat)
    if(istat.ne.0) return

    call scenario_nread_char(a,myscen%answerinfall,istat)
    if(istat.ne.0) return
    if (myscen%answerinfall.eq.'Infall') then
       myscen%infall=1
       call scenario_nread_real(a,myscen%tinfall,istat)
       call scenario_nread_real(a,myscen%Zinfall,istat)
    else
       myscen%tinfall=1.
       myscen%Zinfall=0.
       myscen%infall=0
    end if

    call scenario_nread_int(a,typeSFR,istat)      
    if(istat.ne.0) return
    myscen%SFRparam(1)=typeSFR
    if (typeSFR.ge.1.and.typeSFR.le.3) then
       myscen%SFRparam(2)=2
       call scenario_nread_real(a,myscen%SFRparam(3),istat)
       call scenario_nread_real(a,myscen%SFRparam(4),istat)
    end if
    if (typeSFR.ge.10) then
       call scenario_nread_int(a,nparam,istat)
       myscen%SFRparam(2)=nparam
       do i=1,nparam
          call scenario_nread_real(a,myscen%SFRparam(2+i),istat)
       end do
    end if
    if (typeSFR.le.-1) then
       call scenario_nread_char(a,myscen%fileSFR,istat)
       i=1
       do while (myscen%fileSFR(i:i).le.' '.and.i.lt.len(myscen%fileSFR)) 
          i=i+1
       end do
       myscen%fileSFR=myscen%fileSFR(i:)
    end if

    if (typeSFR.ge.-1) then
       call scenario_nread_char(a,myscen%answerZ,istat)
       if (index(myscen%answerZ,'No').ne.0) then
          myscen%codeZ=0
       else
          myscen%codeZ=1
       endif
       if (myscen%codeZ.eq.0) then
          call scenario_nread_real(a,myscen%ZSFRinit,istat)
       else
          myscen%ZSFRinit=myscen%Zgasinit
       end if
    else
       myscen%codeZ=2                
    endif

    call scenario_nread_real(a,myscen%fsub,istat)
    if(istat.ne.0) return
    call scenario_nread_char(a,myscen%answerwind,istat)
    if(istat.ne.0) return
    if (myscen%answerwind.eq.'Galactic winds') then
       call scenario_nread_real(a,myscen%twind,istat)
    else
       myscen%twind= -99
    end if

    call scenario_nread_char(a,myscen%answerneb,istat)
    if(istat.ne.0) return
    call scenario_nread_char(a,myscen%answerext,istat)
    if(istat.ne.0) return
    if (index(myscen%answerext,'No').ne.0) myscen%codeext=0
    if (index(myscen%answerext,'spheroidal').ne.0) myscen%codeext=1
    if (index(myscen%answerext,'averaged').ne.0) myscen%codeext=2
    if (index(myscen%answerext,'specific').ne.0) myscen%codeext=3
    if (myscen%codeext.eq.3) call scenario_nread_real(a,myscen%inclin,istat)

    if(myscen%twind.lt.0) then   ! No galactic wind
       myscen%twind = nmaxtimes+1.
    end if

    return
  end subroutine scenario_sfr_r

end MODULE scenario_io
