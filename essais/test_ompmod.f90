program test_ompmod

  use myompmod

  implicit none


  integer :: i,j
  real,dimension(20000,20000) :: mybig

  !$OMP PARALLEL DO PRIVATE(i,j)
  do i=1,5
     j=2*i
     call myroutine(i,j,mybig)
  enddo
  !$OMP END PARALLEL DO

end program test_ompmod
