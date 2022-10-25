program test
  
  integer :: a(2,2)
  integer   :: b(2)

  a=transpose(reshape ((/1,2,3,4/),(/2,2/)))

  b=(/5,6/)

  write(*,*) a(1,:)
  write(*,*) a(2,:)

  write(*,*) b


  
  write(*,*) sum(a(1:2,1:2)*b(1:2))

end program test
