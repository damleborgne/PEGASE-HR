MODULE SSPsubr

  use types_ssps
  use constants
  use util


CONTAINS

  subroutine interpstellib(T,g,metal,&
       nCM,i1,i2,i3,i4,&
       alpha1,alpha2,alpha3,alpha4,logZmean,izinf,izsup,&
       iz1,iz2,iz3,iz4)

    implicit none

    integer  :: i,i1,i2,i3,i4,izinf,izsup,nCM,iz
    integer  :: iz1,iz2,iz3,iz4
    real(DP) :: T,g,metal,zlog(nmaxZl),metallog
    real(DP) :: DT1,DG1,DT2,DG2,DT3,DG3,DT4,DG4
    real(DP) :: DZ1,DZ2,DZ3,DZ4
    real(DP) :: alpha1,alpha2,alpha3,alpha4
    real(DP) :: alpha,beta,gamma,gprim,gsec,g1,g2,g3,g4
    real(DP) :: T1,T2,T3,T4,logZmean


!    do i=1,stell%nz
!       zlog(i)=log10(stell%z(i))
!    enddo
    zlog=log10(stell%z)
    metallog=log10(metal)


    i1=0
    i2=0
    i3=0
    i4=0
    iz1=izinf
    iz2=izinf
    iz3=izinf
    iz4=izinf

    T1=-1000.
    T2=-1000.
    T3=-1000.
    T4=-1000.

    DT1=0.1
    DG1=0.5
    DZ1=1000.
    DT2=0.1
    DG2=-0.5
    DZ2=1000.
    DT3=-0.1
    DG3=0.5
    DZ3=1000.
    DT4=-0.1
    DG4=-0.5
    DZ4=1000.

    do iz=izinf,izsup
       do i=-nCM,stell%nspecZ(iz)
          if (stell%Tspec(iz,i).ge.T) then
             if (stell%gspec(iz,i).ge.g) then
                !     closest stell%Tspec>T and stell%gspec>g : i1
                if (stell%Tspec(iz,i)-T.lt.DT1) then
                   !     stell%Tspec(iz,i) is better than the previous best one
                   i1=i
                   T1=stell%Tspec(iz,i)
                   DT1=stell%Tspec(iz,i)-T-1.e-6
                   DG1=stell%gspec(iz,i)-g-1.e-6
                   DZ1=abs(zlog(iz)-metallog)-1.e-6
                   iz1=iz
                else
                   !     stell%Tspec(iz,i) is not better than the previous best one (T1)
                   if (abs(stell%Tspec(iz,i)-T1).lt.1.e-6) then
                      !     stell%Tspec(iz,i) is equal to the previous best one : check g and Z
                      if (stell%gspec(iz,i)-g.lt.DG1) then
                         !     stell%gspec(iz,i) is better : we keep this spectrum
                         i1=i
                         T1=stell%Tspec(iz,i)
                         DT1=stell%Tspec(iz,i)-T-1.e-6
                         DG1=stell%gspec(iz,i)-g-1.e-6
                         DZ1=abs(zlog(iz)-metallog)-1.e-6
                         iz1=iz
                      else 
                         !     stell%gspec(iz,i) is not better
                         if (abs(stell%gspec(iz,i)-stell%gspec(iz1,i1)).lt.1.e-6) then
                            !     but stell%gspec(iz,i) is as good as the best until now : we check Z
                            if (abs(zlog(iz)-metallog).lt.DZ1) then
                               i1=i
                               T1=stell%Tspec(iz,i)
                               DT1=stell%Tspec(iz,i)-T-1.e-6
                               DG1=stell%gspec(iz,i)-g-1.e-6
                               DZ1=abs(zlog(iz)-metallog)-1e-6
                               iz1=iz
                            end if
                         end if
                      end if
                   endif
                endif
             else
                !     closest stell%Tspec>T and stell%gspec<g : i2
                if (stell%Tspec(iz,i)-T.lt.DT2) then
                   i2=i
                   T2=stell%Tspec(iz,i)
                   DT2=stell%Tspec(iz,i)-T-1.e-6
                   DG2=stell%gspec(iz,i)-g+1.e-6
                   DZ2=abs(zlog(iz)-metallog)-1.e-6
                   iz2=iz
                else
                   if (abs(stell%Tspec(iz,i)-T2).lt.1.e-6) then
                      if (stell%gspec(iz,i)-g.gt.DG2) then
                         i2=i
                         T2=stell%Tspec(iz,i)
                         DT2=stell%Tspec(iz,i)-T-1.e-6
                         DG2=stell%gspec(iz,i)-g+1.e-6
                         DZ2=abs(zlog(iz)-metallog)-1.e-6
                         iz2=iz
                      else 
                         if (abs(stell%gspec(iz,i)-stell%gspec(iz2,i2)).lt.1.e-6) then
                            if (abs(zlog(iz)-metallog).lt.DZ2) then
                               i2=i
                               T2=stell%Tspec(iz,i)
                               DT2=stell%Tspec(iz,i)-T-1.e-6
                               DG2=stell%gspec(iz,i)-g+1.e-6
                               DZ2=abs(zlog(iz)-metallog)-1e-6
                               iz2=iz
                            end if
                         end if
                      end if
                   end if
                end if
             end if
          else
             if (stell%gspec(iz,i).ge.g) then
                !     stell%Tspec<T and stell%gspec>g
                if (stell%Tspec(iz,i)-T.gt.DT3) then
                   i3=i
                   T3=stell%Tspec(iz,i)
                   DT3=stell%Tspec(iz,i)-T+1.e-6
                   DG3=stell%gspec(iz,i)-g-1.e-6
                   DZ3=abs(zlog(iz)-metallog)-1.e-6
                   iz3=iz
                else
                   if (abs(stell%Tspec(iz,i)-T3).lt.1.e-6) then
                      if (stell%gspec(iz,i)-g.lt.DG3) then
                         i3=i
                         T3=stell%Tspec(iz,i)
                         DT3=stell%Tspec(iz,i)-T+1.e-6
                         DG3=stell%gspec(iz,i)-g-1.e-6
                         DZ3=abs(zlog(iz)-metallog)-1.e-6
                         iz3=iz
                      else 
                         if (abs(stell%gspec(iz,i)-stell%gspec(iz3,i3)).lt.1.e-6) then
                            if (abs(zlog(iz)-metallog).lt.DZ3) then
                               i3=i
                               T3=stell%Tspec(iz,i)
                               DT3=stell%Tspec(iz,i)-T+1.e-6
                               DG3=stell%gspec(iz,i)-g-1.e-6
                               DZ3=abs(zlog(iz)-metallog)-1e-6
                               iz3=iz
                            end if
                         end if
                      end if
                   end if
                end if
             else
                !     stell%Tspec<T and stell%gspec<g
                if (stell%Tspec(iz,i)-T.gt.DT4) then
                   i4=i
                   T4=stell%Tspec(iz,i)
                   DT4=stell%Tspec(iz,i)-T+1.e-6
                   DG4=stell%gspec(iz,i)-g+1.e-6
                   DZ4=abs(zlog(iz)-metallog)-1.e-6
                   iz4=iz
                else
                   if (abs(stell%Tspec(iz,i)-T4).lt.1.e-6) then
                      if (stell%gspec(iz,i)-g.gt.DG4) then
                         i4=i
                         T4=stell%Tspec(iz,i)
                         DT4=stell%Tspec(iz,i)-T+1.e-6
                         DG4=stell%gspec(iz,i)-g+1.e-6
                         DZ4=abs(zlog(iz)-metallog)-1.e-6
                         iz4=iz
                      else 
                         if (abs(stell%gspec(iz,i)-stell%gspec(iz4,i4)).lt.1.e-6) then
                            if (abs(zlog(iz)-metallog).lt.DZ4) then
                               i4=i
                               T4=stell%Tspec(iz,i)
                               DT4=stell%Tspec(iz,i)-T+1.e-6
                               DG4=stell%gspec(iz,i)-g+1.e-6
                               DZ4=abs(zlog(iz)-metallog)-1e-6
                               iz4=iz
                            end if
                         end if
                      end if
                   end if
                end if
             end if
          end if
       end do
    enddo


    g1=stell%gspec(iz1,i1)
    g2=stell%gspec(iz2,i2)
    g3=stell%gspec(iz3,i3)
    g4=stell%gspec(iz4,i4)

    alpha1=0.
    alpha2=0.
    alpha3=0.
    alpha4=0.


    if (i1.eq.0.or.i2.eq.0.or.i3.eq.0.or.i4.eq.0) then
       if (i1.eq.0.or.i2.eq.0.or.i3.eq.0) then
          if (i1.eq.0.or.i2.eq.0.or.i4.eq.0) then
             if (i1.eq.0.or.i3.eq.0.or.i4.eq.0) then
                if (i2.eq.0.or.i3.eq.0.or.i4.eq.0) then
                   if (i1.eq.0.or.i2.eq.0) then
                      if (i1.eq.0.or.i3.eq.0) then
                         if (i1.eq.0.or.i4.eq.0) then
                            if (i2.eq.0.or.i3.eq.0) then
                               if (i2.eq.0.or.i4.eq.0) then
                                  if (i3.eq.0.or.i4.eq.0) then
                                     if (i1.eq.0) then
                                        if (i2.eq.0) then
                                           if (i3.eq.0) then
                                              alpha4=1.
                                           else
                                              alpha3=1.
                                           end if
                                        else
                                           alpha2=1.
                                        end if
                                     else
                                        alpha1=1.
                                     end if
                                  else
                                     if (abs(T3-T4).lt.1.e-6) then
                                        alpha3=(g-g4)/(g3-g4)
                                     else
                                        if (T3.gt.T4) then
                                           alpha3=1.
                                        else
                                           alpha3=0.
                                        end if
                                     end if
                                     alpha4=1-alpha3
                                  end if
                               else
                                  alpha2=(T-T4)/(T2-T4)
                                  alpha4=1-alpha2
                               end if
                            else
                               alpha2=(T-T3)/(T2-T3)
                               alpha3=1-alpha2
                            end if
                         else
                            alpha1=(T-T4)/(T1-T4)
                            alpha4=1-alpha1
                         end if
                      else
                         alpha1=(T-T3)/(T1-T3)
                         alpha3=1-alpha1
                      end if
                   else
                      if (abs(T1-T2).lt.1.e-6) then
                         alpha1=(g-g2)/(g1-g2)
                      else
                         if (T1.lt.T2) then
                            alpha1=1.
                         else
                            alpha1=0.
                         end if
                      end if
                      alpha2=1-alpha1
                   end if
                else
                   if (abs(T3-T4).lt.1.e-6) then
                      i1=i2
                      iz1=iz2
                      T1=T2
                      g1=g3
                   else
                      if (T3.gt.T4) then
                         i4=0
                         alpha2=(T-T3)/(T2-T3)
                         alpha3=1-alpha2
                      else
                         i3=0
                         alpha2=(T-T4)/(T2-T4)
                         alpha4=1-alpha2
                      end if
                   end if
                end if
             else
                if (abs(T3-T4).lt.1.e-6) then
                   i2=i1
                   iz2=iz1
                   T2=T1
                   g2=g4
                else
                   if (T3.gt.T4) then
                      i4=0
                      alpha1=(T-T3)/(T1-T3)
                      alpha3=1-alpha1
                   else
                      i3=0
                      alpha1=(T-T4)/(T1-T4)
                      alpha4=1-alpha1
                   end if
                end if
             end if
          else
             if (abs(T2-T1).lt.1.e-6) then
                i3=i4
                iz3=iz4
                T3=T4
                g3=g1
             else
                if (T1.lt.T2) then
                   i2=0        
                   alpha1=(T-T4)/(T1-T4)
                   alpha4=1-alpha1
                else
                   i1=0
                   alpha2=(T-T4)/(T2-T4)
                   alpha4=1-alpha2
                end if
             end if
          end if
       else
          if (abs(T2-T1).lt.1.e-6) then
             i4=i3
             iz4=iz3
             T4=T3
             g4=g2            
          else
             if (T1.lt.T2) then
                i2=0                  
                alpha1=(T-T3)/(T1-T3)
                alpha3=1-alpha1
             else
                i1=0
                alpha2=(T-T3)/(T2-T3)
                alpha3=1-alpha2
             end if
          end if
       end if
    end if


    if (i1.eq.0.and.i2.eq.0.and.i3.eq.0.and.i4.eq.0) then
       alpha1=0.
       alpha2=0.
       alpha3=0.
       alpha4=0.
    endif

    if (i3.ne.0.and.i4.ne.0.and.i1.ne.0.and.i2.ne.0) then
       alpha=(T-T3)/(T1-T3)
       beta=(T-T4)/(T2-T4)
       gprim=alpha*g1+(1-alpha)*g3
       gsec=beta*g2+(1-beta)*g4
       gamma=(g-gsec)/(gprim-gsec)
       alpha1=alpha*gamma
       alpha2=beta*(1-gamma)
       alpha3=(1-alpha)*gamma
       alpha4=(1-beta)*(1-gamma)
    end if

    logZmean=alpha1*zlog(iz1)+alpha2*zlog(iz2)+alpha3*zlog(iz3)+alpha4*zlog(iz4)


  end subroutine interpstellib

  !******************************

  double precision function interpol(m,mr,r,nr)

    implicit none

    integer k,nr
    double precision m,mr(nmaxMS),r(nmaxMS)

    if (m.lt.mr(1)) then
       interpol=0.
    else
       k=1
       do while ((m-mr(k))*(m-mr(k+1)).gt.0..and.(k.lt.nr))
          k=k+1            
       end do
       interpol=r(k)+(m-mr(k))*(r(k+1)-r(k))/(mr(k+1)-mr(k))
    end if

  end function interpol

  !************************************************************

  subroutine Woosley(mass,metal,masswind,windZ,nZ_WW,Z_WW,&
       nmass_WW,mass_WW,ejectaZ_WW,ejectatot_WW,&
       answerwinds,answerejecta,ej,ejZ)

    implicit none

    character(len=1) :: answerejecta,answerwinds
    integer imodel,iZ,imass,nZ_WW,nmass_WW,nmaxmass
    parameter (nmaxmass=10)
    double precision Z_WW(nZ_WW),alphaZ,metal,mass_WW(nmass_WW)
    double precision ejectatot(nmaxmass),remnant(nmaxmass)
    double precision ejectaZ(nmaxmass)
    double precision ejectatot_WW(nmass_WW,3,nZ_WW)
    double precision ejectaZ_WW(nmass_WW,3,nZ_WW)
    double precision mass,alphamass,rem,masswind,ej,ejZ,windZ

    if (answerejecta.eq.'A') imodel=1
    if (answerejecta.eq.'B') imodel=2
    if (answerejecta.eq.'C') imodel=3

    if (metal.lt.Z_WW(1)) then
       iZ=1
       alphaZ=0.
    else
       if (metal.gt.Z_WW(nZ_WW)) then
          iZ=nZ_WW-1
          alphaZ=1.
       else
          iZ=1
          do while ((metal-Z_WW(iZ))*(metal-Z_WW(iZ+1)).gt.0.) 
             iZ=iZ+1
          end do
          alphaZ=(metal-Z_WW(iZ))/(Z_WW(iZ+1)-Z_WW(iZ))
       end if
    end if
    do imass=1,nmass_WW
       ejectatot(imass)=min(ejectatot_WW(imass,imodel,iZ)+alphaZ&
            *(ejectatot_WW(imass,imodel,iZ+1)&
            -ejectatot_WW(imass,imodel,iZ)),mass_WW(imass)-1.28d0)
       remnant(imass)=mass_WW(imass)-ejectatot(imass)
       ejectaZ(imass)=ejectaZ_WW(imass,imodel,iZ)&
            +alphaZ*(ejectaZ_WW(imass,imodel,iZ+1)&
            -ejectaZ_WW(imass,imodel,iZ))
    end do

    if (answerwinds.eq.'y') then
       if (mass.gt.mass_WW(1)) then
          if (mass.gt.mass_WW(nmass_WW)) then
             imass=nmass_WW-1
             alphamass=(mass_WW(imass+1)-mass)/&
                  (mass_WW(imass+1)-mass_WW(imass))
             rem=max(masswind*(alphamass*remnant(imass)&
                  +(1.-alphamass)*remnant(imass+1))/mass,1.28d0)
             ej=masswind-rem
             ejZ=min(max(alphamass*ejectaZ(imass)&
                  +(1-alphamass)*ejectaZ(imass+1),0.d0)+ej*metal,ej)
          else
             imass=1
             do while ((mass-mass_WW(imass))&
                  *(mass-mass_WW(imass+1)).gt.0.&
                  .and.(imass.lt.nmass_WW))
                imass=imass+1
             end do
             alphamass=(mass_WW(imass+1)-mass)/&
                  (mass_WW(imass+1)-mass_WW(imass))
             rem=max(masswind*(alphamass*remnant(imass)&
                  +(1.-alphamass)*remnant(imass+1))/mass,1.28d0)
             ej=masswind-rem
             ejZ=min(max(alphamass*ejectaZ(imass)&
                  +(1-alphamass)*ejectaZ(imass+1),0.d0)+ej*metal,ej)
          end if
       else
          imass=1
          rem=max(masswind*remnant(imass)/mass_WW(imass),1.28d0)
          ej=masswind-rem
          ejZ=min(max(ejectaZ(imass)*ej/ejectatot(imass),0.d0)&
               +ej*metal,ej)
       end if
       ejZ=max(ejZ,0.d0)+windZ+(mass-masswind)*metal
       ej=ej+mass-masswind
    else
       if (mass.gt.mass_WW(1)) then
          if (mass.gt.mass_WW(nmass_WW)) then
             imass=nmass_WW-1
          else
             imass=1
             do while ((mass-mass_WW(imass))&
                  *(mass-mass_WW(imass+1)).gt.0.&
                  .and.(imass.lt.nmass_WW))
                imass=imass+1
             end do
          end if
       else
          imass=1
       end if
       alphamass=(mass_WW(imass+1)-mass)/&
            (mass_WW(imass+1)-mass_WW(imass))
       rem=max(alphamass*remnant(imass)&
            +(1.-alphamass)*remnant(imass+1),1.28d0)
       ej=mass-rem
       ejZ=max(alphamass*ejectaZ(imass)&
            +(1-alphamass)*ejectaZ(imass+1),0.d0)+ej*metal
       ejZ=min(ejZ,ej)
    end if
    ejZ=ejZ-ej*metal          
    !     raw yield -> net yield

  end subroutine Woosley

  !************************************************************

  double precision function nstars(mp,dm,fileIMF,massinf,&
       nIMFbins,coeffcont,slope,norm,massmin,massmax)

    implicit none

    ! input: dm is actually the interval in log10(m) for which we want the number of stars
    ! (so dm is in dex)
    
    integer nIMFbins,j
    character(len=280) :: fileIMF
    double precision mp,dm,dn,mass
    double precision massinf(nmaxIMFbins),coeffcont(nmaxIMFbins)
    double precision slope(nmaxIMFbins),norm,massmin,massmax

    mass=10.**mp
    if (mass.gt.massmax.or.mass.lt.massmin) then
       dn=0.
    else
       dn=10.**(dm/2)-10.**(-dm/2)
       if (fileIMF.eq.'ln') then
          dn=dn*10.**(-mp**2.&
               /2.)/mass
       end if
       if (fileIMF.eq.'RB') then
          dn=dn*10.**(1.548-1.513*mp&
               -0.395*mp**2+0.502*mp**3&
               -0.169*mp**4)            
       end if
       if (fileIMF.eq.'Fe') then
          dn=dn*2.01*mass**(-0.52)&
               *10**(-sqrt(2.07*mp**2+1.92*mp&
               +0.73))
       end if
       if (fileIMF .eq. 'Ch2003') then
          if (mass .le. 1.) then
             dn = dn*0.158*exp(-(mp+1.1024)**2/0.9522)
          else
             dn = dn*0.0443*mass**(-1.3)*0.9953
          end if
       end if
       if (fileIMF .eq. 'Ch2005') then
          if (mass .le. 1.) then
             dn = dn*0.093*exp(-(mp+0.69897)**2/0.605)
          else
             dn = dn*0.04147*mass**(-1.35)
          end if
       end if

       if ((fileIMF.ne.'ln').and.(fileIMF.ne.'RB')&
            .and.(fileIMF.ne.'Fe')&
            .and.(fileIMF.ne.'Ch2003')&
            .and.(fileIMF.ne.'Ch2005')) then
          j=1
          if (mass.gt.massinf(1)) then
             if (mass.ge.massinf(nIMFbins)) then
                j=nIMFbins
             else
                do while ((mass-massinf(j))&
                     *(mass-massinf(j+1)).gt.0.)
                   j=j+1
                end do
             end if
          end if
          dn=coeffcont(j)*dn*mass**slope(j)
       end if
    end if
    nstars=dn/norm

  end function nstars

  !************************************************************


  !************************************************************

  subroutine read_para_stellib(stellib_name)

    ! reads stell%Tspec, etc.

    use types_ssps

    implicit none

    character(len=*), intent(in) :: stellib_name


    real :: tmp(nmaxsl*nmaxZl)
    integer :: iz, ispec

    integer unit, status, readonly
    parameter(readonly = 0)
    integer blocksize, nhdu, hdutype, keyval, colnum, frow, felem
    real ::  nullval
    parameter(nullval = -1e37)
    character(len=8) ::  keyword, coltemplate
    character(len=280) ::  comment, string
    logical casesen, anyf
    parameter(casesen = .false.)
    character(len=280) :: stellib_wholepath

    !     Find an unused I/O unit number for the stellar library.
    status=0
    unit=0
    call ftgiou(unit, status) 
    if (status .eq. 0) then
       !     Open the stellar library.
       stellib_wholepath=trim(PEG_ROOT)//'/data/stellibs/'//stellib_name
       call ftopen(unit,stellib_wholepath,&
            readonly, blocksize, status)

       if (status.eq.0) then
          write(*,*) 'Library opened successfully !'
       else 
          write(*,*) 'Fitsio status error : ',status
          write(*,*) 'Failed opening file ',trim(stellib_wholepath)
          stop 'Aborting...'
       endif
       nhdu = 1
       hdutype = 0
       !     Move to the first header data unit (HDU) (fluxes).
       call ftmahd(unit, nhdu, hdutype, status)
       !     Find the value of stell%nspectot (<-> keyword 'naxis2').               
       keyword = 'naxis2'
       call ftgkyj(unit, keyword, keyval, comment, status)
       stell%nspectot = keyval

       stell%grid_type=' '
       if(status.eq.0) then
          call ftgkys(unit,'GRID_TYP', stell%grid_type, comment, status)
          status=0
       endif


       !     Move to header data unit 'TGZ' (results in NHDU=3 for LCB, 2 for ELO)
       hdutype = 2
       call ftmnhd(unit, hdutype, 'TGZ', 0, status)
       if (status.ne.0) then
          write(*,*) 'Fitsio status error : ',status
          stop 'Couldn''t find TGZ hdu number...'
       endif
       call ftghdn(unit, nhdu)
       call ftmahd(unit, nhdu, hdutype, status) 

       !     Find the value of nz (<-> keyword 'NZBIN').               
       keyword = 'NZBIN'
       call ftgkyj(unit, keyword, keyval, comment, status)
       stell%nz = keyval
       !         write(*,*) 'nz=',nz,status
       !     Find the number of stellar spectra (nspecZ) for each metallicity.
       !     The corresponding keywords are NZS1, NZS2, etc.
       do iz = 1, stell%nz
          write(string, *) iz
          do while (string(1:1) .eq. ' ')
             string = string(2:)
          enddo
          keyword = 'NZS'//string
          call ftgkyj(unit, keyword, keyval, comment, status)
          stell%nspecZ(iz) = keyval
       enddo

       stell%firstspec(1) = 1
       do iz = 1, stell%nz
          stell%firstspec(iz+1) = stell%firstspec(iz)&
               +stell%nspecZ(iz)
       enddo

       !     Find the column number corresponding to Z.
       coltemplate = 'Z'
       call ftgcno(unit, casesen, coltemplate, colnum, status)
       !     Read Z for each spectrum.
       frow = 1
       felem = 1
       call ftgcve(unit, colnum, frow, felem, stell%nspectot, nullval,&
            tmp, anyf, status)
       do iz = 1, stell%nz
          stell%z(iz) = tmp(stell%firstspec(iz))
          print*, stell%z(iz)
       enddo
       !     Idem for Teff.
       coltemplate = 'Teff'
       call ftgcno(unit, casesen, coltemplate, colnum, status)
       call ftgcve(unit, colnum, frow, felem, stell%nspectot, nullval,&
            tmp, anyf, status)
       do iz = 1, stell%nz
          stell%Tspec(iz,0)=0.
          do ispec = 1, stell%nspecZ(iz)
             stell%Tspec(iz, ispec) = tmp(ispec+stell%firstspec(iz)-1)
          enddo
       enddo
       !     Idem for logG.
       coltemplate = 'logG'
       call ftgcno(unit, casesen, coltemplate, colnum, status)
       call ftgcve(unit, colnum, frow, felem, stell%nspectot, nullval,&
            tmp, anyf, status)
       do iz = 1, stell%nz
          do ispec = 1, stell%nspecZ(iz)
             stell%gspec(iz, ispec) = tmp(ispec+stell%firstspec(iz)-1)
          enddo
       enddo
       !     Idem for NHI.
       coltemplate = 'NHI'
       call ftgcno(unit, casesen, coltemplate, colnum, status)
       call ftgcve(unit, colnum, frow, felem, stell%nspectot, nullval,&
            tmp, anyf, status)
       do iz = 1, stell%nz
          do ispec = 1, stell%nspecZ(iz)
             stell%NHI(iz, ispec) = tmp(ispec+stell%firstspec(iz)-1)
          enddo
       enddo
       !     Idem for NHeI.
       coltemplate = 'NHeI'
       call ftgcno(unit, casesen, coltemplate, colnum, status)
       call ftgcve(unit, colnum, frow, felem, stell%nspectot, nullval,&
            tmp, anyf, status)
       do iz = 1, stell%nz
          do ispec = 1, stell%nspecZ(iz)
             stell%NHeI(iz, ispec) = tmp(ispec+stell%firstspec(iz)-1)
          enddo
       enddo
       !     Idem for NHeII.
       coltemplate = 'NHeII'
       call ftgcno(unit, casesen, coltemplate, colnum, status)
       call ftgcve(unit, colnum, frow, felem, stell%nspectot, nullval,&
            tmp, anyf, status)
       do iz = 1, stell%nz
          do ispec = 1, stell%nspecZ(iz)
             stell%NHeII(iz, ispec) = tmp(ispec+stell%firstspec(iz)-1)
          enddo
       enddo
       !     Close the stellar library.
       call ftclos(unit, status)
       call ftfiou(unit, status)
    else
       write(*,*) 'Cannot free any unit for the stellar library.'
    endif

  end subroutine read_para_stellib
END MODULE SSPsubr 
