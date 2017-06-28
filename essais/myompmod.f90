module myompmod

  implicit none

  CONTAINS
    subroutine myroutine(i,j,mybig)

      implicit none

      real,dimension(20000,20000),intent(in) :: mybig
      real,dimension(20000) :: onearr
      integer,intent(in) :: i,j
      character(len=2) :: a
      real,dimension(:,:),ALLOCATABLE :: bigarray

      allocate(bigarray(-100:1000,1000))
      print*, j
      write(a,'(i1)') i
      print*, a
      open(unit=i,file='omp_junk_'//trim(a),status='replace')
      bigarray = j
      write(i,*) bigarray
      close(unit=i)

      deallocate(bigarray)
      
    end subroutine myroutine

end module myompmod
