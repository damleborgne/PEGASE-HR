!**** This code is aimed to prepare the input file to "spectra.f".

program scenarios_HR


  use util
  use constants
!  use SSPsubr

  implicit none


  character(len=280):: filescenarios,fileSSPs,filespectra,fileSFR
  character(len=72) :: answerneb,answerZ,char_nfiles,answerZdef
  character(len=72) :: answernebdef
  character(len=72) :: infall,infalldef
  character(len=280):: filespectradef
  character(len=72) :: answerwind,answerwinddef
  integer nfiles,typeSFR,answerext,i,typeSFRdef
  integer nparam,iparam,error,answerextdef
  real param1,param2,tinfall,Zinfall,Zsteldef
  real inclin,Zstel,ZISM0,fSNIa,param,fSNIadef,ZISM0def,tinfalldef
  real Zinfalldef,inclindef,fsub,fsubdef,twind,twinddef
  real param1def(9),param2def(9)

  !     added by DLB
  integer status
  character(len=50) ::  stellib
  character(len=280) :: fileSSPsdef


  !**** Default values

  typeSFRdef=0
  param1def(1)=5.e-5
  param1def(2)=1000.
  param1def(3)=1.
  param2def(1)=20001.
  param2def(2)=1.
  param2def(3)=3000.
  fSNIadef=0.05
  infalldef='n'
  tinfalldef=1000.
  Zinfalldef=0.
  answernebdef='y'
  answerextdef=0
  inclindef=0.
  answerwinddef='n'
  twinddef=20001.
  ZISM0def=0.02
  answerZdef='y'
  Zsteldef=0.02
  fsubdef=0.
  fileSSPsdef=trim(PEG_ROOT)//'/data/isochrones/Salp_default_SSPs.dat'

  write(*,*) 'Press <RETURN> to use the default values'
  write(*,*) ' '

  !**** filescenarios

  error=1
  do while (error.ne.0) 
     write(*,*) 'Name of the input file to "spectra_HR"?'      
     read(*,'(a)') filescenarios 
     open(10,file=filescenarios,status='new',iostat=error)
     if (error.ne.0) then
        write(*,*) 'Invalid input!'
     end if
  end do
  write(*,'(60a1)') ('=',i=1,60)

  !**** fileSSPs

  write(*,*) 'Name of the file containing ', &
       'the SSPs properties?'
  write(*,*) 'Default: ',fileSSPsdef(1:index(fileSSPsdef,&
       ' '))
  read(*,'(a)') fileSSPs 
  if (fileSSPs.eq.' ') fileSSPs=fileSSPsdef

  write(10,'(2a)') 'SSPs file: ',&
       fileSSPs(1:index(fileSSPs,' ')-1)
  write(*,'(60a1)') ('=',i=1,60)

  !**** fSNIa

  error=1
  do while (error.ne.0)
     write(*,*) 'Fraction of close binary systems ',&
          '(real in [0.,1.])?'
     write(*,'(a,e11.5)') ' Default: ',fSNIadef
     call read_real(fSNIa,fSNIadef,error)
     if (fSNIa.lt.0..or.fSNIa.gt.1.) error=1
     if (error.eq.0) then
        write(10,'(a,e11.5)') 'Fraction of close binary systems: ',&
             fSNIa
     else
        write(*,*) 'Invalid input!'
     end if
  end do


  call choose_stellib(stellib, status)
  if (status.ne.0) then
     write(0,*)'scenarios_HR: Failed to select a stellar library'
     call exit(1)
  endif
  write(10,'(a,a)') 'libstell : ',stellib


  !**** filespectra      

  write(*,'(60a1)') ('*',i=1,60)
  nfiles=1
  write(char_nfiles,*) nfiles
  i=1
  do while (char_nfiles(i:i).eq.' ') 
     i=i+1
  end do
  char_nfiles=char_nfiles(i:)
  filespectradef='spectra'//char_nfiles(1:index(char_nfiles,&
       ' ')-1)//'.fits'
  write(*,*) 'Name of the output file ',&
       '(type "end" to stop)?'
  write(*,*) 'Default: ',filespectradef(1:index(filespectradef,&
       ' '))
  read(*,'(a)') filespectra
  if (filespectra.eq.'END'.or.filespectra.eq.'End') &
       filespectra='end'
  do while (filespectra.ne.'end')
     if (filespectra.eq.' ') filespectra=filespectradef
     write(10,'(60a1)') ('*',i=1,60)
     write(10,'(i3,2a)') nfiles,': ',&
          filespectra(1:index(filespectra,' ')-1)
     write(*,'(60a1)') ('=',i=1,60)

     !**** ZISM0

     error=1
     do while (error.ne.0)
        write(*,*) 'Metallicity (mass fraction) of the ',&
             'ISM at t=0 (real in [0.,1.])?' 
        write(*,'(a,e11.5)') ' Default: ',ZISM0def
        call read_real(ZISM0,ZISM0def,error)
        if (ZISM0.lt.0..or.ZISM0.gt.1.) error=1
        if (error.eq.0) then
           write(10,'(a,e11.5)') 'Initial metallicity: ',ZISM0
        else
           write(*,*) 'Invalid input!'  
        end if
     end do
     ZISM0def=ZISM0
     write(*,'(60a1)') ('=',i=1,60)

     !**** infall

     error=1
     do while (error.ne.0)
        write(*,*) 'Infall (y/n)?'
        write(*,'(2a)') ' Default: ', &
             infalldef(1:index(infalldef,' '))
        call read_char(infall,infalldef,error)
     end do
     if (infall.eq.'y') then
        write(10,'(a)') 'Infall'
     else
        write(10,'(a)') 'No infall'
     end if
     infalldef=infall

     !**** tinfall

     if (infall.eq.'y') then            
        write(*,'(60a1)') ('-',i=1,60)
        error=1
        do while (error.ne.0)
           write(*,*) 'Infall timescale (Myr, real)?'
           write(*,'(a,e11.5)') ' Default: ',tinfalldef
           call read_real(tinfall,tinfalldef,error)
           if (error.eq.0) then
              write(10,'(a,e11.5)') 'Infall timescale (Myr): ',&
                   tinfall
           else
              write(*,*) 'Invalid input!'  
           end if
        end do
        tinfalldef=tinfall
     end if


     !**** Zinfall

     if (infall.eq.'y') then
        write(*,'(60a1)') ('-',i=1,60)
        error=1
        do while (error.ne.0)
           write(*,*) 'Metallicity of the infalling gas ',&
                '(mass fraction, real in [0.,1.])?'
           write(*,'(a,e11.5)') ' Default: ',Zinfalldef
           call read_real(Zinfall,Zinfalldef,error)
           if (Zinfall.lt.0..or.Zinfall.gt.1.) error=1
           if (error.eq.0) then
              write(10,'(2a,e11.5)') 'Metallicity of the ',&
                   'infalling gas: ',Zinfall
           else
              write(*,*) 'Invalid input!'  
           end if
        end do
        Zinfalldef=Zinfall
     end if
     write(*,'(60a1)') ('=',i=1,60)

     !**** typeSFR

     error=1
     do while (error.ne.0)
        write(*,*) 'Type of star formation scenario?'
        write(*,*) '-2: file giving the SFR and the metallicity'
        write(*,*) '-1: file giving the SFR'
        write(*,*) ' 0: instantaneous burst'
        write(*,*) ' 1: SFR=p1 from t=0 to p2'
        write(*,*) ' 2: SFR=p2*exp(-t/p1)/p1'
        write(*,*) ' 3: SFR=(Mgas^p1)/p2'
        write(*,*) '10 or more: code of the SFR law you ',&
             'have implemented in "spectra.f"'
        write(*,'(a,i3)') ' Default: ',typeSFRdef
        call read_integer(typeSFR,typeSFRdef,error)
        if (typeSFR.lt.-2) error=1
        if (error.eq.0) then
           write(10,'(a,i3)') 'Type of star formation: ',typeSFR         
        else
           write(*,*) 'Invalid input!'
        end if
     end do
     typeSFRdef=typeSFR

     !**** Predefined SFR law
     !**** param1

     if (typeSFR.ge.1.and.typeSFR.le.3) then 
        write(*,'(60a1)') ('-',i=1,60)
        error=1
        do while (error.ne.0) 
           write(*,*) 'p1 (real)?'
           if (typeSFR.eq.1) then
              write(*,*) 'p1: Msol/Myr'
           end if
           if (typeSFR.eq.2) then
              write(*,*) 'p1: Myr'
           end if
           if (typeSFR.eq.3) then
              write(*,*) 'p1: no dimension'
           end if
           write(*,'(a,e11.5)') ' Default: ',param1def(typeSFR)
           call read_real(param1,param1def(typeSFR),error)
           if (error.eq.0) then
              write(10,'(a,e11.5)') 'p1: ',param1
           else
              write(*,*) 'Invalid input!'
           end if
        end do
        param1def(typeSFR)=param1

        !**** param2

        write(*,'(60a1)') ('.',i=1,60)
        error=1
        do while (error.ne.0)
           write(*,*) 'p2 (real)?'
           if (typeSFR.eq.1) then
              write(*,*) 'p2: Myr'
           end if
           if (typeSFR.eq.2) then
              write(*,*) 'p2: Msol'
           end if
           if (typeSFR.eq.3) then
              write(*,*) 'p2: Myr/Msol'
           end if
           write(*,'(a,e11.5)') ' Default: ',param2def(typeSFR)
           call read_real(param2,param2def(typeSFR),error)
           if (error.eq.0) then
              write(10,'(a,e11.5)') 'p2: ',param2
           else
              write(*,*) 'Invalid input!'  
           end if
        end do
        param2def(typeSFR)=param2
     end if

     !**** Your SFR law

     if (typeSFR.ge.10) then
        write(*,'(60a1)') ('-',i=1,60)
        error=1
        do while (error.ne.0)
           write(*,*) 'Number of parameters used in your ',&
                'star formation law (integer)?'
           read(*,*,iostat=error) nparam
           if (nparam.lt.0.or.nparam.ge.100) error=1
           if (error.eq.0) then
              write(10,'(a,i2)') 'Number of parameters: ',nparam
           else
              write(*,*) 'Invalid input!'  
           end if
        end do
        do iparam=1,nparam
           write(*,'(60a1)') ('.',i=1,60)
           error=1
           do while (error.ne.0) 
              write(*,'(a,i2,a)') 'Parameter number ',iparam,&
                   ' (real)?'
              read(*,*,iostat=error) param
              if (error.eq.0) then
                 write(10,'(a,i2,a,e11.5)') 'Parameter number ',&
                      iparam,': ',param
              else
                 write(*,*) 'Invalid input!'  
              end if
           end do
        end do
     end if

     !**** SFR file

     if (typeSFR.le.-1) then
        write(*,'(60a1)') ('-',i=1,60)
        write(*,*) 'Name of the file?'
        read(*,'(a)') fileSFR
        write(*,*) ' '
        write(10,'(3a)') 'SFR file: ',&
             fileSFR(1:index(fileSFR,' ')-1)
     end if
     write(*,'(60a1)') ('=',i=1,60)

     !**** answerZ

     if (typeSFR.ge.-1) then
        error=1
        do while (error.ne.0)
           write(*,*) 'Consistent evolution ',&
                'of the stellar metallicity (y/n)?'
           write(*,'(2a)') ' Default: ',&
                answerZdef(1:index(answerZdef,' '))
           call read_char(answerZ,answerZdef,error)
        end do
        if (answerZ.eq.'y') then
           write(10,'(2a)') 'Consistent evolution of the ',&
                'stellar metallicity'
        else
           write(10,'(a)') 'No evolution of the stellar metallicity'
        end if
        answerZdef=answerZ

        !**** Zstel

        if (answerZ.eq.'n') then
           write(*,'(60a1)') ('-',i=1,60)
           error=1
           do while (error.ne.0)
              write(*,*) 'Stellar metallicity ',&
                   '(real in [0.,1.])?'
              write(*,'(a,e11.5)') ' Default: ',Zsteldef
              call read_real(Zstel,Zsteldef,error)
              if (Zstel.lt.0..or.Zstel.gt.1.) error=1
              if (error.eq.0) then
                 write(10,'(a,e11.5)') 'Stellar metallicity: ',Zstel
              else
                 write(*,*) 'Invalid input!'  
              end if
           end do
           Zsteldef=Zstel
        end if
        write(*,'(60a1)') ('=',i=1,60)
     endif

     !**** fraction of substellar objects

     error=1
     do while (error.ne.0)
        write(*,*) 'Mass fraction of substellar objects formed ',&
             '(real in [0.,1.])?'
        write(*,'(a,e11.5)') ' Default: ',fsubdef
        call read_real(fsub,fsubdef,error)
        if (fsub.lt.0..or.fsub.gt.1.) error=1
        if (error.eq.0) then
           write(10,'(2a,e11.5)') 'Mass fraction of substellar ',&
                'objects: ',fsub
        else
           write(*,*) 'Invalid input!'
        end if
     end do
     fsubdef=fsub
     write(*,'(60a1)') ('=',i=1,60)

     !**** answerwind

     error=1
     do while (error.ne.0)
        write(*,*) 'Galactic winds (y/n)?'
        write(*,'(2a)') ' Default: ',&
             answerwinddef(1:index(answerwinddef,' '))
        call read_char(answerwind,answerwinddef,error)     
     end do
     if (answerwind.eq.'y') then
        write(10,'(a)') 'Galactic winds'
     else
        write(10,'(a)') 'No galactic winds'
     end if
     answerwinddef=answerwind

     !**** twind

     if (answerwind.eq.'y') then
        write(*,'(60a1)') ('-',i=1,60)
        error=1
        do while (error.ne.0) 
           write(*,*) 'Age of the galactic winds (Myr, real)?'
           write(*,'(a,e11.5)') ' Default: ',twinddef
           call read_real(twind,twinddef,error)
           if (twind.lt.0) error=1
           if (error.eq.0) then
              write(10,'(a,e11.5)') 'Age of the galactic winds: ',&
                   twind
           else
              write(*,*) 'Invalid input!'  
           end if
        end do
        twinddef=twind
     end if
     write(*,'(60a1)') ('=',i=1,60)

     !**** answerneb

     error=1
     do while (error.ne.0)
        write(*,*) 'Nebular emission (y/n)?'
        write(*,'(2a)') ' Default: ',&
             answernebdef(1:index(answernebdef,' '))
        call read_char(answerneb,answernebdef,error)
     end do
     if (answerneb.eq.'y') then
        write(10,'(a)') 'Nebular emission'
     else
        write(10,'(a)') 'No nebular emission'
     end if
     answernebdef=answerneb
     write(*,'(60a1)') ('=',i=1,60)

     !**** answerext

     error=1
     do while (error.ne.0)
        write(*,*) 'Global extinction?' 
        write(*,*) '0: No extinction'
        write(*,*) '1: Extinction for a ',&
             'spheroidal geometry'
        write(*,*) '2: Extinction for a disk geometry: ',&
             'inclination-averaged'
        write(*,*) '3: Extinction for a disk geometry: ',&
             'specific inclination'
        write(*,'(a,i2)') ' Default: ',answerextdef
        call read_integer(answerext,answerextdef,error)
        if (answerext.lt.0.or.answerext.gt.3) error=1
        if (error.eq.0) then
           if (answerext.eq.0) write(10,'(a)') 'No extinction'
           if (answerext.eq.1) write(10,'(a)') &
                'Extinction for a spheroidal geometry'
           if (answerext.eq.2) write(10,'(a,a)') &
                'Extinction for a disk geometry: ',&
                'inclination-averaged'
           if (answerext.eq.3) write(10,'(a,a)') &
                'Extinction for a disk geometry: ',&
                'specific inclination'
        else
           write(*,*) 'Invalid input!'  
        end if
     end do
     answerextdef=answerext

     !**** inclin

     if (answerext.eq.3) then            
        write(*,'(60a1)') ('-',i=1,60)
        error=1
        do while (error.ne.0) 
           write(*,*) 'Inclination in degrees ',&
                '(real in [0.,90.[; "0."=face-on)?'
           write(*,'(a,e11.5)') ' Default: ',inclindef
           call read_real(inclin,inclindef,error)
           if (inclin.lt.0..or.inclin.ge.90.) error=1
           if (error.eq.0) then
              write(10,'(a,e11.5)') 'Inclination: ',inclin
           else
              write(*,*) 'Invalid input!'  
           end if
        end do
        inclindef=inclin
     end if

     !**** filespectra

     write(*,'(60a1)') ('*',i=1,60)
     nfiles=nfiles+1
     write(char_nfiles,*) nfiles
     i=1
     do while (char_nfiles(i:i).eq.' ') 
        i=i+1
     end do
     char_nfiles=char_nfiles(i:)
     filespectradef='spectra'//char_nfiles(1:index(char_nfiles,&
          ' ')-1)//'.fits'
     write(*,*) 'Name of the output file ',&
          '(type "end" to stop)?'
     write(*,*) 'Default: ',filespectradef(1:index(filespectradef,' '))
     read(*,'(a)') filespectra
     if (filespectra.eq.'END'.or.filespectra.eq.'End') &
          filespectra='end'
  end do
  close(10)

end program scenarios_HR

!*************
