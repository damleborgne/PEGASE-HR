program spectra_HR

  use types
  use nrtype
  use constants
  use stellib_io
  use data_io
  use scenario_io
  use ssp_io
  use compute_scenario

  implicit none

  character(len=strmax) :: filescenarios

  integer :: nscenarios
  integer :: iscenario 
  integer :: istat             ! Return code
  integer :: i
  !  type(t_scenario)    :: myscen


  integer, DIMENSION(8) :: vi,ve

  real :: ti,te ! timing
#ifdef _OPENMP
  integer :: num_threads, omp_get_max_threads
#endif


  !################################################################################
  !     READING filenames and basic header information. Very fast.
  !################################################################################

  if (verbose.ge.2) write(*,*) 'Initialization'
  
  ! Display OpenMP thread count
#ifdef _OPENMP
  num_threads = omp_get_max_threads()
  write(*,'(A,I0,A)') ' OpenMP: Using ', num_threads, ' thread(s)'
#else
  write(*,'(A)') ' OpenMP: DISABLED (compiled without OpenMP support)'
#endif
  
  call cpu_time(ti)

  !################################################################################
  !**** Read scenarios file
  !################################################################################
  filescenarios=' '

  if (iargc().gt.0) then 
     call getarg(1,filescenarios)
     filescenarios=trim(filescenarios)
  endif

  call scenario_open(filescenarios,istat)
  if(istat.eq.0) then
     if (verbose.ge.2) write(*,*) ' Found file containing scenarios'
  else
     write(*,*) 'Error : Did not find file containing scenarios.'//filescenarios//'. Aborting.'
     stop
  end if


  !**** Reading of the names of the SSPs files, fraction of binaries, name of library.
  call scenario_header_r(SSP,stellibinfo%filename,istat)
  if(istat.eq.0) then
     if (verbose.ge.2) write(*,*) ' Read filenames of input SSP files'
  else
     write(*,*) ' Error: Read filenames of input SSP files... failed'
     stop
  end if
  stellibinfo%filename_short=stellibinfo%filename
  stellibinfo%filename=trim(PEG_ROOT)//'/data/stellibs/'//stellibinfo%filename


  !**** Reading of the times for which the spectra are to be computed,
  !     "ages.dat"
  call data_time_r(timeinfo,istat)
  if(istat.ne.0) then
     write(*,*) 'Error: Read output times failed ...'
     stop
  else
     if (verbose.ge.2) write(*,*) ' Read output times '
  end if


  i=0
  do while (istat.eq.0)
     ! Read scenario from file opened with "scenario_open" earlier.
     myscen%number = i+1
     call scenario_sfr_r(myscen,istat)
     if(istat.ne.0) then
        if (verbose.ge.2) write(*,*) ' No more scenarios to compute.... Exiting.'
        exit    ! Exit loop on scenarii
     else
        i=i+1
        scenarios(i) = myscen
     end if
  enddo
  call scenario_close(istat)

  nscenarios=i
  if (verbose.ge.2) write(*,'(a,i3,a)') 'Finished reading ',nscenarios,' scenarios'


  !################################################################################
  !**** Read basic information in stellar library file
  !################################################################################

  ! Reading of the header of the stellar library.
  call stell_open(istat)

  if(istat.eq.0) then
     if (verbose.ge.2) write(*,*) ' Finished reading needed library spectra '
     if (verbose.ge.2) write(*,*) ' Total number of spectra loaded: ',stellibinfo%nspectot
  else
     write(*,*)' Error: Failed to open "stellib" file',trim(stellibinfo%filename),istat
     stop
  endif

  call stell_nz_r(istat)
  if(istat.eq.0) then
     if (verbose.ge.2) write(*,*) ' Finished reading header of stellar library'
  else
     write(*,*) ' Error in stell_nz_r. Aborting.'
     stop
  endif

  call stell_wave_r(istat)

  call read_stellib_spectra()
  call read_CM()


  !################################################################################
  !**** Read the SSP files: files, header, and finally data :  2.5 seconds. TBO ?
  !################################################################################

  !**** 1) Read SSP files

  SSP%nZ=nmaxZtracks            ! initialize nZ to max possi number
  call ssp_files_read(SSP,stellibinfo%grid_type,istat) 
  if (istat.ne.0) then
     write(*,*) ' Error: Could not read SSP files ',SSP%nZ,SSP%fileSSPs
     stop
  else
     if (verbose.ge.2) write(*,*) ' Read filenames of input SSP files '
  endif

  !**** 2) Read header of SSP files

  call ssp_head_read(SSP,istat)
  if(istat.ne.0) then
     write(*,*) ' Error: Read headers of SSP files .. failed'
     stop
  else
     if (verbose.ge.2) write(*,*) ' Read headers of SSP files '
  end if

  !**** 3) Reading of the flux emitted for an instantaneous burst in
  !**** the stellar spectra.
  !     "ssp_files_read" takes a few seconds. TBO ?

  call ssp_data_read(SSP,istat)

  call cpu_time(te)
  if (verbose.ge.2) write(*,*) 'Initializations (seconds) = ',te-ti
  ti=te         


  !################################################################################
  !**** Reading data for extinction. Very fast.
  !################################################################################
  if (verbose.ge.2) write(*,*) ' Reading extinction ppties...'
  call redden_r(istat)
  if(istat.eq.0) then
     if (verbose.ge.2) write(*,*) '..... Done.'
  else
     write(*,*) '... failed !!!'
     stop
  end if


  !################################################################################
  !**** BIG Loop on star formation scenarios.
  !################################################################################

  do iscenario=1,nscenarios

     myscen=scenarios(iscenario)

     !--------------------------------------------------
     call date_and_time(values=vi)

     if (verbose.ge.1) write(*, fmt='(a,i5)', advance="no") '  Computing scenario number',myscen%number

     call compute_scen()

     call date_and_time(values=ve)
     if (verbose.ge.1) write(*,fmt= '(a, f10.3, a)') ', done in ',&
          ((ve(8)+1000.*(ve(7)+60.*ve(6)))-&
          (vi(8)+1000.*(vi(7)+60.*vi(6))))/1000., ' seconds'

     !if (verbose.ge.2) write(*,*) 'This scenario took (seconds to compute) = ',&
     !     ((ve(8)+1000.*(ve(7)+60.*ve(6)))-&
     !     (vi(8)+1000.*(vi(7)+60.*vi(6))))/1000.

      vi=ve


     !--------------------------------------------------
     ! write scenario

     ! Create the output spectra file 
     call write_spectrum()

     call date_and_time(values=ve)
     if (verbose.ge.2) write(*,*) 'This scenario took (seconds to write) = ',&
          ((ve(8)+1000.*(ve(7)+60.*ve(6)))-&
          (vi(8)+1000.*(vi(7)+60.*vi(6))))/1000.


  end do


  !--------------------------------------------------
  deallocate(stellibCM%lambda)
  deallocate(stellibCM%spectra)

  deallocate(stellibinfo%lambda)
  deallocate(stellibinfo%spectra)
  

end program spectra_HR
