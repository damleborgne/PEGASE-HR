program test_ompvariants_2

  !! compilation line : gfortran -O4 -fopenmp -Wl,-stack_size,0x40000000 -o test_ompvariants_2 test_ompvariants_2.f90

  use omp_lib

  real(8)               :: a,b,c,d
  real(8)               :: a_tt,b_tt,c_tt,d_tt
  real(8), dimension(0:3) :: a_t,b_t,c_t,d_t
  real(8), dimension(20002) :: a_tb,b_tb,c_tb,d_tb

  real(8), dimension(20002) :: bigarr1
  integer, dimension(20002) :: bigarr2
  real(8), dimension(8,20002) :: bigarr3

  integer :: i,k,q,j
  real    :: ti,te

  integer :: r,nr
  integer, DIMENSION(8) :: valuesi,valuese

  bigarr3=1
  bigarr2=2

  i=20000
  nr=10000

  do itest=0,3

     a=0.

     call cpu_time(ti)
     call date_and_time(values=valuesi)

     if (itest.eq.0) then  !                     -> 383ms
        do r=0,nr
           a=0.
           do k=1,i
              q=i+1-k
              a=a+r*bigarr3(bigarr2(k),q)
           enddo
           if (r.eq.nr) print*,a
        enddo
     endif

     if (itest.eq.1) then   !                    -> 193ms
        !$OMP PARALLEL PRIVATE(j,a)
        j = omp_get_thread_num()
        !$OMP DO PRIVATE(r,k,q,a_t)
        do r=0,nr
           a_t=0.
           do k=1,i
              q=i+1-k
              a_t(j)=a_t(j)+r*bigarr3(bigarr2(k),q)
           enddo
           a=sum(a_t)
           if (r.eq.nr) print*,a
        enddo
        !$OMP END DO 
        !$OMP END PARALLEL
     endif

     if (itest.eq.2) then  !                     -> 184ms
        !$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(r,a,k,q)
        do r=0,nr
           a=0.
           do k=1,i
              q=i+1-k
              a=a+r*bigarr3(bigarr2(k),q)
           enddo
           if (r.eq.nr) print*,a
        enddo
        !$OMP END PARALLEL DO
     endif


     if (itest.eq.3) then !                     -> 271ms
        !$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(r,k,q,a,a_tb)
        do r=0,nr
           do k=1,i
              q=i+1-k
              a_tb(k)=r*bigarr3(bigarr2(k),q)
           enddo
           a=sum(a_tb)
           if (r.eq.nr) print*,a
        enddo
        !$OMP END PARALLEL DO
     endif

     call cpu_time(te)
     call date_and_time(values=valuese)

     write(*,'(a,i3,a,f13.5,a,f13.0,a,f20.2)') &
          'test ',itest,&
          '; cpu time (ms)=',(te-ti)*1d3,&
          '; user time (ms)=',(1000.*valuese(7)+1.*valuese(8))-(1000.*valuesi(7)+1.*valuesi(8))
  enddo


end program test_ompvariants_2
