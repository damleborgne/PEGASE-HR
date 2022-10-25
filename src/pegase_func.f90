MODULE pegase_func

  use types
  use constants
  use util
  use data_io

  IMPLICIT NONE

CONTAINS
  !c-------------------------------------------------------------------------
  subroutine weightZ(Z,ZSSP,nZ,alpha,iZinf,iZsup)

    implicit none

    integer j,nZ
    real :: Z,ZSSP(nmaxZtracks)
    real :: alpha
    integer iZinf,iZsup

    if (nZ.gt.1) then
       if (Z.le.ZSSP(1)) then 
          iZinf=1
          iZsup=1
          alpha=1.
       else
          if (Z.ge.ZSSP(nZ)) then
             iZinf=nZ
             iZsup=nZ
             alpha=1.
          else
             j=1
             do while (((Z-ZSSP(j))*(Z-ZSSP(j+1)).gt.0.)&
                  .and.(j.lt.nZ))
                j=j+1
             end do
             iZinf=j
             iZsup=j+1
             alpha=log(ZSSP(j+1)/Z)/&
                  log(ZSSP(j+1)/ZSSP(j))
          end if
       end if
    else
       iZinf=1
       iZsup=1
       alpha=1.
    end if

  end subroutine weightZ

  !------------------------------------------------------------------------
  subroutine dust_composition(Z,coeffextZ)

    implicit none

    real Z,coeffextZ(2)
    integer i

    i=0
    call bracket(5,dust%Zext,Z,i)
    call Steffen(5,dust%Zext,dust%frac,.false.,Z,coeffextZ(1),i)
    coeffextZ(2)=1.-coeffextZ(1)

  end subroutine dust_composition

  !------------------------------------------------------------------------
  subroutine ext(lambda,coeffextZ,tau,albedo,asym,j)

    implicit none

    integer :: j
    real :: lambda,tau,albedo,asym
    real :: coeffextZ(2)
    real :: tau1,tau2,albedo1,albedo2,asym1,asym2,alpha

    call bracket(dust%nlambdaext,dust%lambdaext,lambda,j)

    alpha=(1./lambda-1./dust%lambdaext(j))/&
         (1./dust%lambdaext(j+1)-1./dust%lambdaext(j))

    tau1=dust%tauext(j,1)+alpha*(dust%tauext(j+1,1)-dust%tauext(j,1))
    tau2=dust%tauext(j,2)+alpha*(dust%tauext(j+1,2)-dust%tauext(j,2))
    tau=coeffextZ(1)*tau1+coeffextZ(2)*tau2
    if (tau.gt.0.) then
       albedo1=dust%albedoext(j,1)+alpha*(dust%albedoext(j+1,1)-dust%albedoext(j,1))
       albedo2=dust%albedoext(j,2)+alpha*(dust%albedoext(j+1,2)-dust%albedoext(j,2))
       albedo=(coeffextZ(1)*tau1*albedo1&
            +coeffextZ(2)*tau2*albedo2)/tau
       if (albedo.gt.1.) albedo=1.
       if (albedo.gt.0.) then
          asym1=dust%asymext(j,1)+alpha*(dust%asymext(j+1,1)-dust%asymext(j,1))
          asym2=dust%asymext(j,2)+alpha*(dust%asymext(j+1,2)-dust%asymext(j,2))
          asym=(coeffextZ(1)*tau1*albedo1*asym1&
               +coeffextZ(2)*tau2*albedo2*asym2)/tau/albedo
          if (asym.gt.1.) asym=1.
          if (asym.lt.-1.) asym=-1.
       else
          albedo=0.
          asym=0.
       endif
    else
       tau=0.
       albedo=0.
       asym=0.
    endif

  end subroutine ext


  !--------------------------------------------------------------------------
  subroutine count_used(iZstellib1,iZstellib2,ntotused, itotused)

    !        Total number of spectra actually used
    !        After this, ntotused contains the number of spectra actually used,
    !        and itotused contains their index (on the INITIAL LCB scale).
    !**      stellibinfo%firstspec is now the index of the first spec in a Z bin

    use types
    use constants

    implicit none

    ! OUTPUT
    integer              :: iZstellib1,iZstellib2
    integer              :: ntotused
    integer,DIMENSION(:) :: itotused

    ! Local
    logical,dimension(-nmaxCM:nmaxspecLCB) ::  boolused
    integer i,p,j

    ! stellibinfo%firstspec(i) is the index of the 1st spectrum in a Z bin
    !  (stellibinfo%firstspec(nZ+1) is equal to the total number of spectra + 1)

!    do i=-nmaxCM,stellibinfo%firstspec(stellibinfo%nz+1)-1
     do i=-nmaxCM,stellibinfo%nspectot
       boolused(i)=.false.
    end do

    if (index(stellibinfo%filename,'LCB') .ne. 0) then
       do p=1,SSP%nZ
          do i=1,SSP%ntimes
             do j=1,SSP%nused(i,p)
                !                AL- Don't leave out CM spectra:
                if (SSP%iused(j,i,p).lt.0 .or.&
                     (SSP%iused(j,i,p).ge.stellibinfo%firstspec(iZstellib1)  .and.&
                     SSP%iused(j,i,p).lt.stellibinfo%firstspec(iZstellib2+1)))&
                     then 
                   boolused(SSP%iused(j,i,p)) = .true.
                end if
             end do
          end do
       enddo
    else                    ! whatever other lib
       do p = 1, SSP%nZ
          do i=1,SSP%ntimes
             do j=1,SSP%nused(i,p)
                !     AL- Leave out CM spectra:
                if (SSP%iused(j,i,p).gt.0  .and.&
                     SSP%iused(j,i,p).ge.stellibinfo%firstspec(iZstellib1)  .and.&
                     SSP%iused(j,i,p).lt.stellibinfo%firstspec(iZstellib2+1) )&
                     then
                   boolused(SSP%iused(j,i,p)) = .true.
                end if
             end do
          end do
       enddo
    endif
    ntotused=0
    do j=-nmaxCM,stellibinfo%firstspec(stellibinfo%nz+1)-1
       if ( boolused(j) ) then
          ntotused=ntotused+1
          itotused(ntotused)=j
       end if
    end do

    return
  end subroutine count_used

  !-----------------------------------------------------------------------
  subroutine increm_SFR(i, timei)

    ! Compute next SFRi (SFR at time index i)

    implicit none

    integer i
    real timei

    if (myscen%SFRparam(1).eq.0) then !  Instantaneous burst
       if (i.gt.1) then
          galprop%SFR(i)=0.
       else
          galprop%SFR(i)=1.
       endif

    elseif (myscen%SFRparam(1).eq.1) then !  Constant star formation rate
       if (timei.le.myscen%SFRparam(4)) then
          galprop%SFR(i)=myscen%SFRparam(3)
       else
          galprop%SFR(i)=0.
       end if

    elseif (myscen%SFRparam(1).eq.2) then ! Exponentially decreasing or increasing
       galprop%SFR(i)=myscen%SFRparam(4)*exp(-timei/myscen%SFRparam(3))/myscen%SFRparam(3)

    else if (myscen%SFRparam(1).eq.3) then !  prop to a power of the mass of gas
       galprop%SFR(i)=galprop%sigmagas(i)**myscen%SFRparam(3)/myscen%SFRparam(4)

       !       else if (SFRparam(1).eq.n>=10) then
       !               galprop%SFR(i)=your SFR law (note that i = time in Myr + 1)

    end if

    if (timei.ge.myscen%twind) galprop%SFR(i)=0. 

    return
  end subroutine increm_SFR

  !################################################################################
  !-----------------------------------------------------------------------
  subroutine increm_ppties( )

    use types

    implicit none

    ! Local
    integer  :: i,jimpr, k, q
    real     :: timei, sigmaZ, ma
    real(dp) :: dejecta, dejectaZ, dsigmaBHNS, dsigmaWD
    real(DP) :: dsigmaZ, dsigmagas
    real, dimension(nmaxtimes) :: SFRalphak, SFR1malphak

    real :: ti1,ti2,te1,te2

    !     Requirements : compute sigmagas and SFR at all timesteps because they are interdependent,
    !     as well as ZSFR and Zgas because they are used to compute lum weighted Zstars later.
    !     Other quantities can be computed only at "timeimpr" values


    galprop%Mgal(1)=(1.-myscen%infall)
    galprop%sigmagas(1)=galprop%Mgal(1)
    sigmaZ=galprop%sigmagas(1)*galprop%Zgas(1)
    galprop%sigmaBHNS(1)=0.0
    galprop%sigmaWD(1)=0.0
    galprop%excessSFR= 0              ! init time when SFR exceeded max to 0 (ie. did not)
    galprop%sigmasub(1)=0.0



    do i=1, timeinfo%timeimpr(timeinfo%ntimesimpr)+1

       timei=i-1.             ! e.g. index=1 -> real age=0

       ! Find position of ZSFR(i) among available ZSSP, and determine interpolation weight alpha.
       call weightZ(galprop%ZSFR(i),SSP%ZSSP,SSP%nZ,SSP%alpha(i),galprop%iZinf(i),galprop%iZsup(i))

       ! Compute ZSFR(i) (if not read in file previously).
       call increm_SFR(i, timei)

       ! Test derived SFR(i) agains maximal SFR consistent with sigmagas(i)
       if (galprop%SFR(i).gt.galprop%sigmagas(i)) then
          galprop%SFR(i) = galprop%sigmagas(i)
          if (galprop%excessSFR.eq.0) galprop%excessSFR=i
       end if

       ! Properties at time index i.
       galprop%sigmagas(i)=galprop%sigmagas(i)-galprop%SFR(i)
       sigmaZ=sigmaZ-galprop%SFR(i)*galprop%Zgas(i)
       galprop%SFRlum(i)=(1.d0-myscen%fsub)*galprop%SFR(i)

       dejecta=0.d0
       dejectaZ=0.d0
       dsigmaBHNS=0.d0
       dsigmaWD=0.d0

       SFRalphak(i)=galprop%SFRlum(i)*SSP%alpha(i)
       SFR1malphak(i)=galprop%SFRlum(i)*(1.-SSP%alpha(i))

       !$OMP PARALLEL DO REDUCTION(+:dejecta,dejectaZ,dsigmaBHNS,dsigmaWD) PRIVATE(k,q)
       do k=1,i
          if (galprop%SFRlum(k).gt.0) then               
             q=i+1-k

             dejecta=dejecta+SFRalphak(k)*SSP%ejecta(galprop%iZinf(k),q)&
                  +SFR1malphak(k)*SSP%ejecta(galprop%iZsup(k),q)

             dejectaZ=dejectaZ+SFRalphak(k)*SSP%ejectaZ(galprop%iZinf(k),q)&
                  +SFR1malphak(k)*SSP%ejectaZ(galprop%iZsup(k),q)&
                  +galprop%Zgas(k)*(SFRalphak(k)*SSP%ejecta(galprop%iZinf(k),q)&
                  +SFR1malphak(k)*SSP%ejecta(galprop%iZsup(k),q))

             dsigmaBHNS=dsigmaBHNS&
                  +SFRalphak(k)*SSP%massBHNS(galprop%iZinf(k),q)&
                  +SFR1malphak(k)*SSP%massBHNS(galprop%iZsup(k),q)

             dsigmaWD=dsigmaWD&
                  +SFRalphak(k)*SSP%massWD(galprop%iZinf(k),q)&
                  +SFR1malphak(k)*SSP%massWD(galprop%iZsup(k),q)
          endif
       enddo
       !$OMP END  PARALLEL DO

       ! Properties at time index i+1.
       dsigmagas = dejecta+myscen%infall*exp(-timei/myscen%tinfall)/myscen%tinfall
       dsigmaZ   = dejectaZ&
            +myscen%infall*exp(-timei/myscen%tinfall)/myscen%tinfall*myscen%Zinfall
       galprop%sigmasub(i+1) = galprop%sigmasub(i)+myscen%fsub*galprop%SFR(i)

       if (timei.ge.myscen%twind) then
          galprop%Mgal(i+1)       = galprop%Mgal(i)-galprop%sigmagas(i)-dejecta
          galprop%sigmaBHNS(i+1)  = galprop%sigmaBHNS(i)+dsigmaBHNS
          galprop%sigmaWD(i+1)    = galprop%sigmaWD(i)+dsigmaWD
          galprop%sigmagas(i+1)   = 0.
          sigmaZ=0.
          galprop%Zgas(i+1)=0.
       else
          galprop%Mgal(i+1)=galprop%Mgal(i)+myscen%infall&
               *exp(-timei/myscen%tinfall)/myscen%tinfall
          galprop%sigmaBHNS(i+1) = galprop%sigmaBHNS(i)+dsigmaBHNS
          galprop%sigmaWD(i+1)   = galprop%sigmaWD(i)+dsigmaWD
          galprop%sigmagas(i+1)  = max(galprop%sigmagas(i)+dsigmagas,0.d0)
          sigmaZ                 = max(sigmaZ+dsigmaZ,0.d0)
          if (galprop%sigmagas(i+1).le.0.) then
             galprop%Zgas(i+1)=0.
          else
             galprop%Zgas(i+1)=sigmaZ/galprop%sigmagas(i+1)
          end if
       end if

       !     Compute ZSFR(i+1) if not read in input file previously.
       if (myscen%codeZ.eq.1) then
          galprop%ZSFR(i+1)=galprop%Zgas(i+1)
       elseif (myscen%codeZ.eq.0) then
          galprop%ZSFR(i+1)=galprop%ZSFR(1)
       end if

    end do

    !###  Second loop, for things we need only at timeimpr

    galprop%agestars=0.
    galprop%sigmastars=0.
    galprop%Zstars=0.

    do jimpr=1, timeinfo%ntimesimpr
       i=timeinfo%timeimpr(jimpr)+1    ! e.g. age=0 -> index=1

       do k=1,i
          if (galprop%SFRlum(k).gt.0) then
             q=i+1-k
             ma=SFRalphak(k)*SSP%massalive(galprop%iZinf(k),q)&
                  +SFR1malphak(k)*SSP%massalive(galprop%iZsup(k),q)

             galprop%sigmastars(i) = galprop%sigmastars(i)+ ma           
             galprop%Zstars(i)     = galprop%Zstars(i)    + ma*galprop%ZSFR(k)
             galprop%agestars(i)   = galprop%agestars(i)  + ma*(i-k)
          end if
       end do


       if (galprop%sigmastars(i).gt.0.) then
          galprop%agestars(i) = galprop%agestars(i) /galprop%sigmastars(i)
          galprop%Zstars(i)   = galprop%Zstars(i)   /galprop%sigmastars(i)
       else
          galprop%agestars(i)=0.
          galprop%Zstars(i)=0.
       end if

    end do

  end subroutine increm_ppties

  !################################################################################
  !----------------------------------------------------------------------
  subroutine redden_cont(codeext, i912, lambda, nlambda, inclin,&
       Zgasi, sigmagasi, fluxgal, fluxext, tauV)

    !  Inputs: codeext, i912, lambda, nlambda, inclin,
    !          Zgasi (=current metallicity of the gas),
    !          sigmagasi (=current amount of gas),
    !  Input/Output (modified by the subroutine) : fluxgal
    !  Outputs: fluext, tauV


    implicit none


    ! IN :
    integer codeext, i912, nlambda
    real inclin
    real,dimension(:) :: lambda
    real Zgasi, sigmagasi

    !INOUT : 
    real,dimension(:) :: fluxgal

    !OUT :  
    real fluxext, tauV


    real coeffextZ(2)

    integer  j, j0
    integer i01, i02, i03, i04
    real NH, col_dens, mH, albedo, asym, tau
    parameter(mH=1.67e-24)
    real,dimension(stellibinfo%nlambda) ::  corextcont,corextinclincont

    real lambdaV
    parameter(lambdaV = 5500.)


    !******Apply reddening.

    fluxext=0.
    tauV=0.
    if (codeext.ne.0) then
       call dust_composition(Zgasi,coeffextZ)


       !     If there is some extinction, the Lyman continuum photons
       !     not absorbed by the gas are absorbed by the dust%

       if (Zgasi.gt.0.) then
          if (i912.gt.1) then
             do j=1,i912-1
                fluxext=fluxext+(fluxgal(j)+fluxgal(j+1))&
                     *(lambda(j+1)-lambda(j))/2.
                fluxgal(j)=0.
             end do
             fluxext=fluxext+(912.-lambda(i912))*fluxgal(i912)
             fluxgal(i912)=0.
          endif
       endif


       !*******Ellipticals.

       if (codeext.eq.1) then
          j0=0
          NH=5.3e22*sigmagasi
          col_dens=0.35*Zgasi*NH*mH*1.4
          call ext(lambdaV,coeffextZ,tauV,albedo,asym,j0)
          tauV=tauV*col_dens

          !     Continuum in ellipticals:

          i01=0
          i02=0
          i03=0
          j0=0
          do j=1,nlambda
             call ext(lambda(j),coeffextZ,tau,albedo,asym,j0)
             tau=tau*col_dens
             call Steffen3D(dust%tauKing,dust%albKing,dust%asymKing,dust%emergKing,&
                  nK1,nK2,nK3,tau,albedo,asym,corextcont(j),&
                  i01,i02,i03)
             corextcont(j)=exp(corextcont(j))
          end do
          do j=1,nlambda-1
             fluxext=fluxext+((1.-corextcont(j))*fluxgal(j)&
                  +(1.-corextcont(j+1))*fluxgal(j+1))/2.&
                  *(lambda(j+1)-lambda(j))
          end do
          
          fluxgal=fluxgal*corextcont
       end if

       !*******Disk galaxies.

       if (codeext.ge.2) then
          NH=6.8e21*sigmagasi
          col_dens=0.35*Zgasi*NH*mH*1.4
          j0=0
          call ext(lambdaV,coeffextZ,tauV,albedo,asym,j0)
          tauV=tauV*col_dens

          !     Continuuum in disks:

          i01=0
          i02=0
          i03=0
          i04=0
          j0=0
          do j=1,nlambda
             call ext(lambda(j),coeffextZ,tau,albedo,asym,j0)
             tau=tau*col_dens
             call Steffen3D(dust%tauslab,dust%albslab,dust%asymslab,dust%emergslab,&
                  nslab1,nslab2,nslab3,tau,albedo,asym,&
                  corextcont(j),i01,i02,i03)
             corextcont(j)=exp(corextcont(j))
             if (codeext.eq.3) then
                call Steffen4D(dust%tauslab,dust%albslab,dust%asymslab,&
                     dust%inclinslab,dust%emerginclinslab,nslab1,nslab2,&
                     nslab3,nslab4,tau,albedo,asym,inclin,&
                     corextinclincont(j),i01,i02,i03,i04)
                corextinclincont(j)=exp(corextinclincont(j))
             else
                corextinclincont(j)=corextcont(j)
             endif
          end do
          do j=1,nlambda-1
             fluxext=fluxext+((1.-corextcont(j))*fluxgal(j)&
                  +(1.-corextcont(j+1))*fluxgal(j+1))/2.&
                  *(lambda(j+1)-lambda(j))
          end do
             fluxgal=fluxgal*corextinclincont
       end if
    end if
    !     End of if (codeext.ne.0)

    return
  end subroutine redden_cont
  !################################################################################
  !----------------------------------------------------------------------
  subroutine redden_lines(codeext, inclin,&
       lambdaline, nlines, Zgasi, sigmagasi,&
       flinetot, fluxext)

    !     Inputs: codeext, nlines, inclin,
    !     Zgasi (=current metallicity of the gas),
    !     sigmagasi (=current amount of gas),
    !     Input/Output (modified by the subroutine) : fluxext, flinetot
    !     Outputs: fluext


    implicit none


    integer codeext, nlines
    real inclin
    real lambdaline(*)
    real Zgasi, sigmagasi
    real flinetot(*)
    real fluxext


    real coeffextZ(2)

    integer j, j0
    integer i01, i02, i03, i04
    real NH, col_dens, mH, albedo, asym, tau
    parameter(mH=1.67e-24)
    real corextline(nmaxlines),corextinclinline(nmaxlines)


    !******Apply reddening.

    !      fluxext=0. ! fluxext was previoulsy partly computed frrom stellar continuum in redden_cont
    if (codeext.ne.0) then
       call dust_composition(Zgasi,coeffextZ)


       !     If there is some extinction, the Lyman continuum photons
       !     not absorbed by the gas are absorbed by the dust%

       if (Zgasi.gt.0.) then
          fluxext=fluxext+flinetot(30)
          flinetot(30)=0.
       endif


       !*******Ellipticals.

       if (codeext.eq.1) then
          j0=0
          NH=5.3e22*sigmagasi
          col_dens=0.35*Zgasi*NH*mH*1.4

          !     Lines in ellipticals:

          do j=1,nlines
             i01=0
             i02=0
             i03=0
             i04=0
             j0=0
             call ext(lambdaline(j),coeffextZ,tau,albedo,asym,j0)
             tau=tau*col_dens
             call Steffen3D(dust%tauKing,dust%albKing,dust%asymKing,dust%emergKing,&
                  nK1,nK2,nK3,tau,albedo,asym,corextline(j),&
                  i01,i02,i03)
             corextline(j)=exp(corextline(j))
             fluxext=fluxext+(1.-corextline(j))*flinetot(j)
             flinetot(j)=flinetot(j)*corextline(j)
          end do
       end if

       !*******Disk galaxies.

       if (codeext.ge.2) then
          NH=6.8e21*sigmagasi
          col_dens=0.35*Zgasi*NH*mH*1.4

          !     Lines in disks:

          do j=1,nlines
             i01=0
             i02=0
             i03=0
             i04=0
             j0=0
             call ext(lambdaline(j),coeffextZ,tau,albedo,asym,j0)
             tau=tau*col_dens
             call Steffen3D(dust%tauslab,dust%albslab,dust%asymslab,dust%emergslab,&
                  nslab1,nslab2,nslab3,tau,albedo,asym,&
                  corextline(j),i01,i02,i03)
             corextline(j)=exp(corextline(j))
             if (codeext.eq.3) then
                call Steffen4D(dust%tauslab,dust%albslab,dust%asymslab,&
                     dust%inclinslab,dust%emerginclinslab,nslab1,nslab2,&
                     nslab3,nslab4,tau,albedo,asym,inclin,&
                     corextinclinline(j),i01,i02,i03,i04)
                corextinclinline(j)=exp(corextinclinline(j))
             else
                corextinclinline(j)=corextline(j)
             endif
             fluxext=fluxext+(1.-corextline(j))*flinetot(j)
             flinetot(j)=flinetot(j)*corextinclinline(j)
          end do
       end if
    end if

    return
  end subroutine redden_lines
  !################################################################################
  !******************

  subroutine convol_SFR_SSPs(&
       ntotused,    &        !< number of used spectra
       itotused,    &        !< Indices of used spectra
       Lspec_gal)           !> integrated spectra (stars)

    use constants
    use types

    implicit none


    real, DIMENSION(-nmaxCM:nmaxspecLCB,timeinfo%ntimesimpr), intent(inout) :: Lspec_gal
    real, DIMENSION(-nmaxCM:nmaxspecLCB)                  :: one_Lspec_gal

    integer               :: ntotused       
    integer,dimension(:)  :: itotused       ! dimension nmaxspec


    ! local variables:
    integer :: k,q,jimpr
    integer :: itime             ! time in Myr

    do jimpr=1,timeinfo%ntimesimpr
       itime=timeinfo%timeimpr(jimpr)+1 ! e.g. real time=0 -> index=1
       
       one_Lspec_gal=0d0
       
       !$OMP PARALLEL DO REDUCTION(+:one_Lspec_gal) PRIVATE(k,q)
       do k=1,itime
          if (galprop%SFRlum(k).gt.1.e-20) then               
             q=itime+1-k

             one_Lspec_gal = &
                  one_Lspec_gal +&
                  galprop%SFRlum(k) *&
                  (SSP%beta(q) *&
                  (SSP%alpha(k) *& 
                  SSP%Lspec_SSP(:,SSP%invtime(q),galprop%iZinf(k))&
                  +(1.-SSP%alpha(k)) * &
                  SSP%Lspec_SSP(:,SSP%invtime(q),galprop%iZsup(k)))&
                  +(1.-SSP%beta(q)) *&
                  (SSP%alpha(k) * &
                  SSP%Lspec_SSP(:,SSP%invtime(q)+1,galprop%iZinf(k))&
                  +(1.-SSP%alpha(k)) * &
                  SSP%Lspec_SSP(:,SSP%invtime(q)+1,galprop%iZsup(k))))
          end if
       end do
       !$OMP END PARALLEL DO
       Lspec_gal(:,jimpr)=one_Lspec_gal
    end do


  end subroutine convol_SFR_SSPs

  !################################################################################

  subroutine comput_continuum(ntotused,itotused,Lspec_gal,flux_gal)

    use nrtype
    use constants
    use types

    implicit none

    !INPUT
    integer, intent(in)                                :: ntotused
    integer, DIMENSION(:), intent(in)                  :: itotused
    real,dimension(-nmaxCM:nmaxspecLCB, timeinfo%ntimesimpr), intent(in) ::  Lspec_gal

    !output
    real,dimension(:,:) :: flux_gal

    !Local
    integer :: i
    integer :: ilambda, itimeimpr, ispec
    integer  :: istat

    real,dimension(stellibinfo%nlambda,ntotused)   :: used_spectra



    do i=1,ntotused
       ispec=itotused(i)
       if (ispec.lt.0) then 
          ! use the appropriate CM spectrum
          used_spectra(:,i)=stellibCM%spectra(:,-ispec)
       else
          !  any other library : reading the spectrum
          used_spectra(:,i)=stellibinfo%spectra(:,ispec)
       endif

    enddo


    flux_gal = 0.d0

    do i=1,ntotused
       do itimeimpr = 1, timeinfo%ntimesimpr
          if (Lspec_gal(itotused(i),itimeimpr).gt.0.d0) then
             flux_gal(:, itimeimpr) = flux_gal(:, itimeimpr)+&
                  used_spectra(:,i)*Lspec_gal(itotused(i),itimeimpr)             
          endif
       enddo
    enddo

    
  end subroutine comput_continuum

  !################################################################################


  subroutine calculflux_calib(lambda,flux,lambdafilter,trans,area,&
       nlambdafilter,typetrans,fluxtrans,lambdaeff,i,nlambda)

    use constants
    use types
    use util

    implicit none

    integer nlambda
    integer i,j,k,nlambdafilter(nmaxfilters),typetrans(nmaxfilters)
    double precision fluxtrans,lambda(5000),flux(5000),transsup
    double precision trans(nmaxfilters,1000),fluxinf,transinf,fluxsup
    double precision lambdaeff,lambdainf,lambdasup
    double precision area(nmaxfilters),lambdafilter(nmaxfilters,1000)

    lambdaeff=0.
    fluxtrans=0.
    if ((lambda(1).gt.lambdafilter(i,1)).or.(lambda(nlambda).lt.&
         lambdafilter(i,nlambdafilter(i)))) then
       fluxtrans=-1.
    else
       k=1
       do while(lambda(k+1).lt.lambdafilter(i,1))
          k=k+1
       enddo
       j=1
       lambdainf=lambdafilter(i,j)
       transinf=0.
       fluxinf=0.
       lambdasup=lambdainf
       do while(j+1.le.nlambdafilter(i).and.k+1.le.nlambda)
          if (lambdafilter(i,j+1).lt.lambda(k+1)) then
             lambdasup=lambdafilter(i,j+1)
             fluxsup=interploglog(lambda(k),lambda(k+1),flux(k),&
                  flux(k+1),lambdasup)
             transsup=trans(i,j+1)*lambdasup**typetrans(i)               
             j=j+1
          else
             lambdasup=lambda(k+1)
             fluxsup=flux(k+1)
             transsup=interplinlin(lambdafilter(i,j),&
                  lambdafilter(i,j+1),trans(i,j),trans(i,j+1),&
                  lambdasup)*lambdasup**typetrans(i)  
             k=k+1
          endif
          fluxtrans=fluxtrans+(transinf*fluxinf&
               +transsup*fluxsup)*(lambdasup-lambdainf)/2
          lambdaeff=lambdaeff+(transinf*fluxinf&
               +transsup*fluxsup)*(lambdasup-lambdainf)/2&
               *(lambdasup+lambdainf)/2
          lambdainf=lambdasup
          fluxinf=fluxsup
          transinf=transsup
       enddo
       lambdaeff=lambdaeff/fluxtrans
       fluxtrans=fluxtrans/area(i)             
    endif

  end subroutine calculflux_calib


END MODULE pegase_func
