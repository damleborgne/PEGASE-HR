!**** D. Le Borgne : this file is identical to the PEGASE.2 's calib.f file
!     Except that the path where the filters.dat fit can be found is set 
!     by the variable PEG_ROOT.

!**** Calibration of the filters

program calib_HR


  use types
  use constants
  use pegase_func !calculflux_calib

  implicit none

  character(len=72) ::  name(nmaxfilters)
  integer nlambdaVega,nfilters,nlambdafilter(nmaxfilters),i,j
  integer typetrans(nmaxfilters),nlambdaFSD,nlambdaSun
  integer typecalib(nmaxfilters)
  double precision lambdafilter(nmaxfilters,nmaxlambdatrans)
  double precision trans(nmaxfilters,nmaxlambdatrans)
  double precision lambdamed,transmed,fluxtrans
  double precision area(nmaxfilters),lambdaeff,fluxtransFSD
  double precision fluxVega(5000),lambdaVega(5000)
  double precision lambdamean(nmaxfilters)
  double precision ABVega,a,TGVega,lambdaeffFSD
  double precision areanu(nmaxfilters),lambdaFSD(5000)
  double precision fluxFSD(5000)
  double precision lambdaSun(5000),fluxSun(5000)
  double precision lambdaeffSun,fluxtransSun

  !**** Reading of the spectrum of Vega

  open(10,status='old',file=trim(PEG_ROOT)//'/data/external/VegaLCB.dat')
  read(10,*) nlambdaVega
  do i=1,nlambdaVega
     read(10,*) lambdaVega(i),fluxVega(i) 
  end do
  close(10)

  !**** Reading of the spectrum of the Sun

  open(10,status='old',file=trim(PEG_ROOT)//'/data/external/SunLCB.dat') 
  !     Intrinsic flux: erg/s/A
  read(10,*) nlambdaSun     
  do i=1,nlambdaSun
     read(10,*) lambdaSun(i),fluxSun(i) 
  end do
  close(10)

  !**** Reading of the spectrum of BD+17o4708 (F subdwarf used to calibrate the 
  !**** Thuan & Gunn system).

  open(10,status='old',file=trim(PEG_ROOT)//&
       '/data/external/BD+17o4708.dat')
  read(10,*) nlambdaFSD     
  do i=1,nlambdaFSD
     read(10,*) lambdaFSD(i),fluxFSD(i) 
  end do
  close(10)

  !**** Reading of the transmissions of the filters

  open(20,status='old',file=trim(PEG_ROOT)//&
       '/data/user_defined/filters.dat')
  read(20,*) nfilters          
  do i=1,nfilters
     read(20,*) nlambdafilter(i),typetrans(i),typecalib(i),name(i)
     do j=1,nlambdafilter(i)
        read(20,*) lambdafilter(i,j),trans(i,j)
     end do
  end do
  close(20)

  open(40,status='unknown',file=trim(PEG_ROOT)//&
       '/data/user_defined/calib.dat')

  write(40,'(a,4x,a,4x,a,3x,a,4x,a,2x,a,2x,a,2x,a,2x,a)') &
       'filter','index','Flambda(Vega)',&
       'area','mean lambda','lambda eff(Vega)','mAB(Vega)',&
       'mTG(Vega)','Flambda(Sun)'

  !**** Areas and mean wavelengths of the filters

  do i=1,nfilters
     area(i)=0.
     a=0.
     areanu(i)=0.
     fluxtrans=0.
     lambdamean(i)=0.
     do j=1,nlambdafilter(i)-1
        lambdamed=(lambdafilter(i,j)+lambdafilter(i,j+1))/2. 
        transmed=(trans(i,j)+trans(i,j+1))/2.
        lambdamean(i)=lambdamean(i)+lambdamed**(1+typetrans(i))&
             *transmed*(lambdafilter(i,j+1)-lambdafilter(i,j))
        area(i)=area(i)+transmed&
             *(lambdafilter(i,j+1)-lambdafilter(i,j))&
             *lambdamed**typetrans(i)
        areanu(i)=areanu(i)+transmed&
             *(lambdafilter(i,j+1)-lambdafilter(i,j))&
             *lambdamed**typetrans(i)/lambdamed**2*c
     end do
     lambdamean(i)=lambdamean(i)/area(i)
  end do

  !**** Computation of the effective wavelengths, AB-magnitudes of Vega
  !**** and calibration constants of the filters.

  do i=1,nfilters
     call calculflux_calib(lambdaVega,fluxVega,lambdafilter,trans,area,&
          nlambdafilter,typetrans,fluxtrans,lambdaeff,i,nlambdaVega)
     call calculflux_calib(lambdaSun,fluxSun,lambdafilter,trans,area,&
          nlambdafilter,typetrans,fluxtransSun,lambdaeffSun,i,&
          nlambdaSun)
     ABVega=-2.5*log10(fluxtrans*area(i)/areanu(i))-48.60
     call calculflux_calib(lambdaFSD,fluxFSD,lambdafilter,trans,area,&
          nlambdafilter,typetrans,fluxtransFSD,lambdaeffFSD,i,&
          nlambdaFSD)
     if (fluxtransFSD.gt.0.) then
        TGVega=-2.5*log10(fluxtrans/fluxtransFSD)+9.5
     else
        TGVega=99.999
     end if
     write(40,50) name(i),i,fluxtrans,&
          area(i),lambdamean(i),lambdaeff,&
          ABVega,TGVega,fluxtransSun
  end do
  close(40)

50 format(a10,1x,i3,7x,e9.4,3x,e9.4,3x,e9.4,5x,e9.4,5x,&
       2(f8.3,3x),2x,e9.4)

end program calib_HR

!****





