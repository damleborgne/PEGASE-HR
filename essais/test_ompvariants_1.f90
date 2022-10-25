program test_ompvariants_1

  !! compilation line : gfortran -O4 -fopenmp -Wl,-stack_size,0x40000000 -o test_ompvariants_1 test_ompvariants_1.f90

  use omp_lib

  real(8)               :: a,b,c,d
  real(8)               :: a_tt,b_tt,c_tt,d_tt
  real(8), dimension(1:4) :: a_t,b_t,c_t,d_t
  real(8), dimension(200002) :: a_tb,b_tb,c_tb,d_tb

  real(8), dimension(200002) :: bigarr1
  integer, dimension(200002) :: bigarr2
  real(8), dimension(8,200002) :: bigarr3

  integer :: i,k,q,j,l,imin,imax
  real    :: ti,te

  integer :: r,nr
  integer, DIMENSION(8) :: valuesi,valuese

  do i=1,8
     do j=1,200002
        bigarr3(i,j)=1.2*i+j*1.6
     enddo
  enddo

  bigarr2=2

  i=200000
  nr=200

  do itest=0,5

     a=0.

     call cpu_time(ti)
     call date_and_time(values=valuesi)

     if (itest.eq.0) then  !                     ->
        a=0.
        a_t=0.
        a_tt=0.
        do r=0,nr
           a=0.
           do k=1,i
              q=i+1-k
              a=a+1.*bigarr3(bigarr2(k),q)+2.*bigarr3(bigarr2(k),q)+3.*bigarr3(bigarr2(k),q)
           enddo
        enddo
     endif

     if (itest.eq.1) then   !                    ->
        a=0.
        a_t=0.
        a_tt=0.
        do r=0,nr
           a_t=0.
           !$OMP PARALLEL PRIVATE(j)
           j = omp_get_thread_num()+1
           !$OMP DO PRIVATE(k,q)
           do k=1,i
              q=i+1-k
              a_t(j)=a_t(j)+1.*bigarr3(bigarr2(k),q)+2.*bigarr3(bigarr2(k),q)+3.*bigarr3(bigarr2(k),q)
           enddo
           !$OMP END DO 
           !$OMP END PARALLEL
           a=sum(a_t)
        enddo
     endif

     if (itest.eq.2) then  !                     ->
        a=0.
        a_t=0.
        a_tt=0.
        do r=0,nr
           a=0.
           !$OMP PARALLEL DO REDUCTION(+:a) DEFAULT(SHARED) PRIVATE(k,q)
           do k=1,i
              q=i+1-k
              a=a+1.*bigarr3(bigarr2(k),q)+2.*bigarr3(bigarr2(k),q)+3.*bigarr3(bigarr2(k),q)
           enddo
           !$OMP END PARALLEL DO
        enddo
     endif

     if (itest.eq.3) then !                     ->
        a=0.
        a_t=0.
        a_tt=0.
        do r=0,nr
           a=0.
           !$OMP PARALLEL PRIVATE(a_tt)
           a_tt=0.
           !$OMP DO PRIVATE(j,k,q)
           do k=1,i
              q=i+1-k
              a_tt=a_tt+1.*bigarr3(bigarr2(k),q)+2.*bigarr3(bigarr2(k),q)+3.*bigarr3(bigarr2(k),q)
           enddo
           !$OMP END DO
           !$OMP ATOMIC
           a=a+a_tt
           !$OMP END PARALLEL
        enddo
     endif

     if (itest.eq.4) then !                     ->
        a=0.
        a_t=0.
        a_tt=0.
        do r=0,nr
           !$OMP PARALLEL DO PRIVATE(k,q)
           do k=1,i
              q=i+1-k
              a_tb(k)=1.*bigarr3(bigarr2(k),q)+2.*bigarr3(bigarr2(k),q)+3.*bigarr3(bigarr2(k),q)
           enddo
           !$OMP END PARALLEL DO
           a=sum(a_tb)
        enddo
     endif

     if (itest.eq.5) then !                     ->
        a=0.
        a_t=0.
        a_tt=0.
        !$OMP PARALLEL
        j = omp_get_num_threads()
        !$OMP END PARALLEL
        do r=0,nr
           a=0.
           !$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(k,q,a_tt,l,imin,imax,a_t)
           do k=1,j
              imin=(k-1)*i/j+1
              imax=(k)*i/j
              if (k.eq.j) imax=i
              a_tt=0.
              do l=imin,imax
                 q=i+1-l
                 a_tt=a_tt+1.*bigarr3(bigarr2(l),q)+2.*bigarr3(bigarr2(l),q)+3.*bigarr3(bigarr2(l),q)
              enddo
              a_t(k)=a_tt
!              if (r.eq.0) print*, k,imin,imax,a_t(k),a_tt
           enddo
           !$OMP END PARALLEL DO
!           if (r.eq.0) print*, a_t
           a=sum(a_t)
        enddo
     endif

     call cpu_time(te)
     call date_and_time(values=valuese)

     write(*,'(a,i3,a,f13.5,a,f13.0,a,f15.2)') &
          'test ',itest,&
          '; cpu time (ms)=',(te-ti)*1d3,&
          '; user time (ms)=',&
          (1000.*valuese(7)+1.*valuese(8))-(1000.*valuesi(7)+1.*valuesi(8)),&
          '; a=',a
  enddo


end program test_ompvariants_1
