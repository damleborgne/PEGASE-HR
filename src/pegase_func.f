c Correction (MF, 19.10.2001) : valeur de mH rajoutee
c Contains:
c     subroutine weightZ(Z,ZSSP,nZ, > alpha,iZinf,iZsup)
c     subroutine Steffen(n,x,y,condition,t,z,i)
c     subroutine Steffen3D(x1,x2,x3,y,n1,n2,n3,
c                $     t1,t2,t3,z,i01,i02,i03)
c     subroutine Steffen4D(x1,x2,x3,x4,y,n1,n2,n3,n4,
c                $     t1,t2,t3,t4,z,i01,i02,i03,i04)
c     real function interplinlog(x1,x2,y1,y2,x)
c     subroutine dust_composition(Zext,frac,Z,coeffextZ)
c     subroutine ext(nlambdaext,lambdaext,tauext,albedoext,
c                $     asymext,lambda,coeffextZ,tau,albedo,asym,j)

c     Two subroutines connected through common/pegase_extinction/:
c     subroutine redden_r(> istat)
c     subroutine redden(codeext, i912, lambda, nlambda,
c     		 &     lambdaline, nlines, Zgasi, sigmagasi,
c     		 &     <> fluxgal, flinetot,
c     		 &     > fluxext, tauV)

c     subroutine count_used(nused,iused, ntimes, nZ,
c                &     nZstellibLCB, iZstellib1,iZstellib2, nspecLCB, nprecLCB,
c                &     > ntotused, itotused)

c     subroutine increm_SFR(i, timei, SFRparam,
c            &   sigmagasi, twind, > SFRi)
c     subroutine increm_ppties(i, SFRi, timei, SFRlum, iZinf, iZsup, alpha,
c            &   fsub, infall, tinfall, Zinfall, twind, nmaxZtracks, nmaxtimes,
c            &   ejecta, ejectaZ, massBHNS, massWD, massalive, ZSFR,
c            &   <> sigmaZ, sigmasub, Mgal, sigmaBHNS, sigmaWD, sigmagas, Zgas,
c            &   > sigmastarsi, Zstarsi, agestarsi)
cc-------------------------------------------------------------------------
      subroutine weightZ(Z,ZSSP,nZ,alpha,iZinf,iZsup)

      implicit none
      include 'peg_include.f'

      integer j,nZ
      real Z,ZSSP(nmaxZtracks)
      real alpha
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
               do while (((Z-ZSSP(j))*(Z-ZSSP(j+1)).gt.0.)
     $              .and.(j.lt.nZ))
                  j=j+1
               end do
               iZinf=j
               iZsup=j+1
               alpha=log(ZSSP(j+1)/Z)/
     $              log(ZSSP(j+1)/ZSSP(j))
            end if
         end if
      else
         iZinf=1
         iZsup=1
         alpha=1.
      end if

      end

c-----------------------------------------------------------------------
      subroutine Steffen(n,x,y,condition,t,z,i) 
*     Steffen M. (1990), A&A 443, 450                            
      implicit none
      integer n,i
      real x(n),y(n)
      real a,b,c,d,t,z
      real h_i,h_im1,h_ip1
      real s_i,s_im1,s_ip1
      real p_i,p_ip1,y1_i,y1_ip1
      real tmx_i
      logical condition

      if (i.eq.1) then
         h_i=x(i+1)-x(i)
         s_i=(y(i+1)-y(i))/h_i
         h_ip1=x(i+2)-x(i+1)
         s_ip1=(y(i+2)-y(i+1))/h_ip1
         p_ip1=(s_i*h_ip1+s_ip1*h_i)/(h_i+h_ip1)
         y1_ip1=(sign(1.,s_i)+sign(1.,s_ip1))
     $        *min(abs(s_i),abs(s_ip1),0.5*abs(p_ip1))
         if (condition) then    
*     1st derivative is null in x=0
            y1_i=(6.*s_i*x(1)*(x(1)+h_i)
     $           -y1_ip1*x(1)*(3.*x(1)+2.*h_i))
     $           /(3.*x(1)**2+4.*x(1)*h_i+h_i**2)
         else
            y1_i=1.5*s_i-0.5*y1_ip1
         end if
      else
         if (i.eq.n-1) then
            h_i=x(i+1)-x(i)
            s_i=(y(i+1)-y(i))/h_i
            h_im1=x(i)-x(i-1)
            s_im1=(y(i)-y(i-1))/h_im1
            p_i=(s_im1*h_i+s_i*h_im1)/(h_im1+h_i)
            y1_i=(sign(1.,s_im1)+sign(1.,s_i))
     $        *min(abs(s_im1),abs(s_i),0.5*abs(p_i))
            y1_ip1=1.5*s_i-0.5*y1_i
         else
            h_i=x(i+1)-x(i)
            s_i=(y(i+1)-y(i))/h_i
            h_im1=x(i)-x(i-1)
            s_im1=(y(i)-y(i-1))/h_im1
            h_ip1=x(i+2)-x(i+1)
            s_ip1=(y(i+2)-y(i+1))/h_ip1
            p_i=(s_im1*h_i+s_i*h_im1)/(h_im1+h_i)
            p_ip1=(s_i*h_ip1+s_ip1*h_i)/(h_i+h_ip1)
            y1_i=(sign(1.,s_im1)+sign(1.,s_i))
     $        *min(abs(s_im1),abs(s_i),0.5*abs(p_i))
            y1_ip1=(sign(1.,s_i)+sign(1.,s_ip1))
     $        *min(abs(s_i),abs(s_ip1),0.5*abs(p_ip1))
         end if
      end if
      if (t.gt.x(n)) then
         z=y(n)+y1_ip1*(t-x(n))
      else
         tmx_i=t-x(i)
         a=(y1_i+y1_ip1-2.*s_i)/h_i**2
         b=(3.*s_i-2.*y1_i-y1_ip1)/h_i
         c=y1_i
         d=y(i)
         z=((a*tmx_i+b)*tmx_i+c)*tmx_i+d
      endif

      end

c--------------------------------------------------------------------------
      subroutine Steffen3D(x1,x2,x3,y,n1,n2,n3,
     $     t1,t2,t3,z,i01,i02,i03)
      
      implicit none
      integer n1,n2,n3,i1,i2,i3,n,i01,i02,i03
      parameter(n=100)
      real t1,t2,t3,z,x1(n1),x2(n2),x3(n3)
      real y(n1,n2,n3),y1(n),y2(n),y3(n)

      call bracket(n1,x1,t1,i01)
      call bracket(n2,x2,t2,i02)
      call bracket(n3,x3,t3,i03)
      do i1=max(1,i01-1),min(n1,i01+2)
         do i2=max(1,i02-1),min(n2,i02+2)
            do i3=max(1,i03-1),min(n3,i03+2)
              y3(i3)=y(i1,i2,i3)
           end do
           call Steffen(n3,x3,y3,.false.,t3,y2(i2),i03)
        end do
        call Steffen(n2,x2,y2,.false.,t2,y1(i1),i02)
      end do
      call Steffen(n1,x1,y1,.false.,t1,z,i01)

      end

c--------------------------------------------------------------------------
      subroutine Steffen4D(x1,x2,x3,x4,y,n1,n2,n3,n4,
     $     t1,t2,t3,t4,z,i01,i02,i03,i04)
      
      implicit none
      integer n1,n2,n3,n4,i1,i2,i3,i4,n,i01,i02,i03,i04
      parameter(n=100)
      real t1,t2,t3,t4,z,x1(n1),x2(n2),x3(n3),x4(n4)
      real y(n1,n2,n3,n4),y1(n),y2(n),y3(n),y4(n)

      call bracket(n1,x1,t1,i01)
      call bracket(n2,x2,t2,i02)
      call bracket(n3,x3,t3,i03)
      call bracket(n4,x4,t4,i04)
      do i1=max(1,i01-1),min(n1,i01+2)
         do i2=max(1,i02-1),min(n2,i02+2)
            do i3=max(1,i03-1),min(n3,i03+2)
               do i4=max(1,i04-1),min(n4,i04+2)
                 y4(i4)=y(i1,i2,i3,i4)
              end do              
              call Steffen(n4,x4,y4,.true.,t4,y3(i3),i04)
           end do
           call Steffen(n3,x3,y3,.false.,t3,y2(i2),i03)
        end do
        call Steffen(n2,x2,y2,.false.,t2,y1(i1),i02)
      end do
      call Steffen(n1,x1,y1,.false.,t1,z,i01)

      end

c---------------------------------------------------------------------------
      real function interplinlog(x1,x2,y1,y2,x)

      implicit none
      
      real x1,x2,y1,y2,x,epsilon

      epsilon=1.e-37
      if (abs(x2-x1).lt.epsilon) then
         interplinlog=y1
      else
         if (y1.le.epsilon.or.y2.le.epsilon) then
            interplinlog=0.
         else
            interplinlog=y1*(y2/y1)**((x-x1)/(x2-x1))
         endif
      endif

      end

c------------------------------------------------------------------------
      subroutine dust_composition(Zext,frac,Z,coeffextZ)

      implicit none
      real Zext(5),frac(5),Z,coeffextZ(2)
      integer i

      i=0
      call bracket(5,Zext,Z,i)
      call Steffen(5,Zext,frac,.false.,Z,coeffextZ(1),i)
      coeffextZ(2)=1.-coeffextZ(1)

      end

c------------------------------------------------------------------------
      subroutine ext(nlambdaext,lambdaext,tauext,
     $     albedoext,asymext,lambda,coeffextZ,tau,albedo,asym,j)

      implicit none
      include 'peg_include.f'

      integer nlambdaext,j
      real coeffextZ(2)
      real lambdaext(nmaxlambdaext),tauext(nmaxlambdaext,2)
      real albedoext(nmaxlambdaext,2),asymext(nmaxlambdaext,2)
      real lambda,tau,albedo,asym
      real tau1,tau2,albedo1,albedo2,asym1,asym2,alpha

      call bracket(nlambdaext,lambdaext,lambda,j)
      alpha=(1./lambda-1./lambdaext(j))/
     $     (1./lambdaext(j+1)-1./lambdaext(j))

      tau1=tauext(j,1)+alpha*(tauext(j+1,1)-tauext(j,1))
      tau2=tauext(j,2)+alpha*(tauext(j+1,2)-tauext(j,2))
      tau=coeffextZ(1)*tau1+coeffextZ(2)*tau2
      if (tau.gt.0.) then
         albedo1=albedoext(j,1)+alpha*(albedoext(j+1,1)-albedoext(j,1))
         albedo2=albedoext(j,2)+alpha*(albedoext(j+1,2)-albedoext(j,2))
         albedo=(coeffextZ(1)*tau1*albedo1
     $        +coeffextZ(2)*tau2*albedo2)/tau
         if (albedo.gt.1.) albedo=1.
         if (albedo.gt.0.) then
            asym1=asymext(j,1)+alpha*(asymext(j+1,1)-asymext(j,1))
            asym2=asymext(j,2)+alpha*(asymext(j+1,2)-asymext(j,2))
            asym=(coeffextZ(1)*tau1*albedo1*asym1
     $           +coeffextZ(2)*tau2*albedo2*asym2)/tau/albedo
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

      end

c----------------------------------------------------------------------
	subroutine redden_r(istat)

	implicit none
        include 'peg_include.f'

        integer nslab1,nslab2,nslab3,nslab4
        parameter(nslab1=17,nslab2=9,nslab3=6,nslab4=10)
        real tauslab(nslab1),albslab(nslab2)
        real asymslab(nslab3),inclinslab(nslab4)
        real emergslab(nslab1,nslab2,nslab3)
        real emerginclinslab(nslab1,nslab2,nslab3,nslab4)

        integer nK1,nK2,nK3
        parameter(nK1=12,nK2=9,nK3=11)
        real tauKing(nK1),albKing(nK2),asymKing(nK3)
        real emergKing(nK1,nK2,nK3)

        real Zext(5), frac(5)
        integer nlambdaext
        real lambdaext(nmaxlambdaext),tauext(nmaxlambdaext,2)
        real albedoext(nmaxlambdaext,2),asymext(nmaxlambdaext,2)

	integer istat

c       Common to redden_r and redden:
	common/pegase_extinction/tauslab,albslab,asymslab,emergslab,
     &		inclinslab,emerginclinslab,
     &		tauKing,albKing,asymKing,emergKing,
     &		Zext,frac,nlambdaext,lambdaext,tauext,albedoext,asymext


c****** Read extinction data from files.
 
c       Extinction parameters for the slab model
        call data_extin_r(tauslab,albslab,asymslab,emergslab,
     &     inclinslab,emerginclinslab,istat)
 

c       Extinction parameters for the King model
        if(istat.eq.0)
     &        call data_king_r(tauKing,albKing,asymKing,emergKing,istat)
 
c       Dust properties (absorption, albedo, asymmetry)
c       for graphites and silicates.  C/Si ratios depend on the metallicity.
        if (istat.eq.0) call data_dust_r(Zext, frac, nlambdaext,
     $       lambdaext, tauext,albedoext, asymext, istat)
 
	return
	end

c----------------------------------------------------------------------
	subroutine redden(codeext, i912, lambda, nlambda, inclin,
     &			lambdaline, nlines, Zgasi, sigmagasi,
     &			fluxgal, flinetot,
     &			fluxext, tauV)

c  Inputs: codeext, i912, lambda, nlambda, nlines,
c          Zgasi (=current metallicity of the gas),
c          sigmagasi (=current amount of gas),
c  Input/Output (modified by the subroutine) : fluxgal, flinetot
c  Outputs: fluext, tauV

c modif PhP:2001/09/29: pass "inclin as argument.

	implicit none
        include 'peg_include.f'

	integer codeext, i912, nlambda, nlines
        real inclin
	real lambda(*), lambdaline(*)
	real Zgasi, sigmagasi
	real flinetot(*), fluxgal(*)
	real fluxext, tauV

	integer nslab1,nslab2,nslab3,nslab4
        parameter(nslab1=17,nslab2=9,nslab3=6,nslab4=10)
        real tauslab(nslab1),albslab(nslab2)
        real asymslab(nslab3),inclinslab(nslab4)
        real emergslab(nslab1,nslab2,nslab3)
        real emerginclinslab(nslab1,nslab2,nslab3,nslab4)
	
        integer nK1,nK2,nK3
        parameter(nK1=12,nK2=9,nK3=11)
        real tauKing(nK1),albKing(nK2),asymKing(nK3)
        real emergKing(nK1,nK2,nK3)

	real Zext(5), frac(5)
	integer nlambdaext
	real lambdaext(nmaxlambdaext),tauext(nmaxlambdaext,2)
	real albedoext(nmaxlambdaext,2),asymext(nmaxlambdaext,2)

	real coeffextZ(2)

	integer istat, j, j0
	integer i01, i02, i03, i04
	real NH, col_dens, mH, albedo, asym, tau
        parameter(mH=1.67e-24)
	real corextcont(nmaxlambda),corextinclincont(nmaxlambda)
        real corextline(nmaxlines),corextinclinline(nmaxlines)

c	Common to redden_r and redden
        common/pegase_extinction/tauslab,albslab,asymslab,emergslab,
     &          inclinslab,emerginclinslab,
     &          tauKing,albKing,asymKing,emergKing,
     &          Zext,frac,nlambdaext,lambdaext,tauext,albedoext,asymext 


c****** Apply reddening.
 
        fluxext=0.
        tauV=0.
        if (codeext.ne.0) then
            call dust_composition(Zext,frac, Zgasi,coeffextZ)
 
 
c           If there is some extinction, the Lyman continuum photons
c           not absorbed by the gas are absorbed by the dust.

            if (Zgasi.gt.0.) then
c-AL -        ELODIE spectra dont go below 912AA.
c-            Computing this only really makes sense with the Lejeune Library.
              if (i912.gt.1) then
                 do j=1,i912-1
                    fluxext=fluxext+(fluxgal(j)+fluxgal(j+1))
     $                 *(lambda(j+1)-lambda(j))/2.
                    fluxgal(j)=0.
                 end do
                 fluxext=fluxext+(912.-lambda(i912))*fluxgal(i912)
                 fluxgal(i912)=0.
              endif
 
c             Idem for Lyman alpha.
              fluxext=fluxext+flinetot(30)
              flinetot(30)=0.
            endif
                                          

********    Ellipticals.
 
            if (codeext.eq.1) then
               j0=0
               NH=5.3e22*sigmagasi
               col_dens=0.35*Zgasi*NH*mH*1.4
               call ext(nlambdaext,lambdaext,tauext,
     $                 albedoext,asymext,5500.,coeffextZ,
     $                 tauV,albedo,asym,j0)
               tauV=tauV*col_dens

*              Continuum in ellipticals:
 
               i01=0
               i02=0
               i03=0
               j0=0
               do j=1,nlambda
                     call ext(nlambdaext,lambdaext,tauext,
     $                    albedoext,asymext,lambda(j),coeffextZ,
     $                    tau,albedo,asym,j0)
                     tau=tau*col_dens
                     call Steffen3D(tauKing,albKing,asymKing,emergKing,
     $                    nK1,nK2,nK3,tau,albedo,asym,corextcont(j),
     $                    i01,i02,i03)
                     corextcont(j)=exp(corextcont(j))
               end do
               do j=1,nlambda-1
                     fluxext=fluxext+((1.-corextcont(j))*fluxgal(j)
     $                    +(1.-corextcont(j+1))*fluxgal(j+1))/2.
     $                    *(lambda(j+1)-lambda(j))
               end do
               do j=1,nlambda
                     fluxgal(j)=fluxgal(j)*corextcont(j)
               end do
                              
*              Lines in ellipticals:
 
               do j=1,nlines
                     i01=0
                     i02=0
                     i03=0
                     j0=0
                     call ext(nlambdaext,lambdaext,tauext,
     $                    albedoext,asymext,lambdaline(j),
     $                    coeffextZ,tau,albedo,asym,j0)
                     tau=tau*col_dens
                     call Steffen3D(tauKing,albKing,asymKing,emergKing,
     $                    nK1,nK2,nK3,tau,albedo,asym,corextline(j),
     $                    i01,i02,i03)
                     corextline(j)=exp(corextline(j))
                     fluxext=fluxext+(1.-corextline(j))*flinetot(j)
                     flinetot(j)=flinetot(j)*corextline(j)
               end do
            end if
 
********    Disk galaxies.
 
            if (codeext.ge.2) then
               NH=6.8e21*sigmagasi
               col_dens=0.35*Zgasi*NH*mH*1.4
               j0=0
               call ext(nlambdaext,lambdaext,tauext,
     $              albedoext,asymext,5500.,coeffextZ,
     $              tauV,albedo,asym,j0)
               tauV=tauV*col_dens
                                    
*              Continuuum in disks:
 
               i01=0
               i02=0
               i03=0
               i04=0
               j0=0
               do j=1,nlambda
                     call ext(nlambdaext,lambdaext,tauext,
     $                    albedoext,asymext,lambda(j),coeffextZ,
     $                    tau,albedo,asym,j0)
                     tau=tau*col_dens
                     call Steffen3D(tauslab,albslab,asymslab,emergslab,
     $                    nslab1,nslab2,nslab3,tau,albedo,asym,
     $                    corextcont(j),i01,i02,i03)
                     corextcont(j)=exp(corextcont(j))
                     if (codeext.eq.3) then
                        call Steffen4D(tauslab,albslab,asymslab,
     $                       inclinslab,emerginclinslab,nslab1,nslab2,
     $                       nslab3,nslab4,tau,albedo,asym,inclin,
     $                       corextinclincont(j),i01,i02,i03,i04)
                        corextinclincont(j)=exp(corextinclincont(j))
                     else
                        corextinclincont(j)=corextcont(j)
                     endif
               end do
               do j=1,nlambda-1
                     fluxext=fluxext+((1.-corextcont(j))*fluxgal(j)
     $                    +(1.-corextcont(j+1))*fluxgal(j+1))/2.
     $                    *(lambda(j+1)-lambda(j))
               end do
               do j=1,nlambda
                     fluxgal(j)=fluxgal(j)*corextinclincont(j)
               end do          

*              Lines in disks:
 
               do j=1,nlines
                     i01=0
                     i02=0
                     i03=0
                     i04=0
                     j0=0
                     call ext(nlambdaext,lambdaext,tauext,
     $                    albedoext,asymext,lambdaline(j),coeffextZ,
     $                    tau,albedo,asym,j0)
                     tau=tau*col_dens
                     call Steffen3D(tauslab,albslab,asymslab,emergslab,
     $                    nslab1,nslab2,nslab3,tau,albedo,asym,
     $                    corextline(j),i01,i02,i03)
                     corextline(j)=exp(corextline(j))
                     if (codeext.eq.3) then
                        call Steffen4D(tauslab,albslab,asymslab,
     $                       inclinslab,emerginclinslab,nslab1,nslab2,
     $                       nslab3,nslab4,tau,albedo,asym,inclin,
     $                       corextinclinline(j),i01,i02,i03,i04)
                        corextinclinline(j)=exp(corextinclinline(j))
                     else
                        corextinclinline(j)=corextline(j)
                     endif
                     fluxext=fluxext+(1.-corextline(j))*flinetot(j)
                     flinetot(j)=flinetot(j)*corextinclinline(j)
               end do
            end if
        end if
c       End of if (codeext.ne.0)
 
	return
	end

c--------------------------------------------------------------------------
	subroutine count_used(nused,iused, filestellib, ntimes, nZ,
     &		 nZstellibLCB, iZstellib1,iZstellib2, nspecLCB, 
     &		 ntotused, itotused)

c        AL - Total number of spectra actually used
c        After this, ntotused contains the number of spectra actually used,
c        and itotused contains their index (on the INITIAL LCB scale).
*** PhP, 2001/10/06, nspecLCB is now the index of the first spec in a Z bin
***                  (nprecLCB is suppressed)
	implicit none
        include 'peg_include.f'

	character*(*) filestellib

        integer nused(nmaxtimesSSP,nmaxZtracks)
        integer iused(nmaxspec,nmaxtimesSSP,nmaxZtracks)
	integer nspecLCB(*)
	integer nZstellibLCB, iZstellib1,iZstellib2
	integer ntotused, ntimes, nZ
	integer itotused(nmaxspec)
	logical boolused(-nmaxCM:nmaxspecLCB)

	integer i,p,j

* nspecLCB(i) is the index of the 1st spectrum in a Z bin
*  (nspecLCB(nZ+1) is equal to the total number of spectra + 1)
        do i=-nmaxCM,nspecLCB(nZstellibLCB+1)-1
           boolused(i)=.false.
        end do

        if (index(filestellib,'LCB') .ne. 0) then
           do p=1,nZ
              do i=1,ntimes
                 do j=1,nused(i,p)
c                AL- Don't leave out CM spectra:
                    if (iused(j,i,p).lt.0 .or.
     &                   (iused(j,i,p).ge.nspecLCB(iZstellib1)  .and.
     &                   iused(j,i,p).lt.nspecLCB(iZstellib2+1)))
     &                   then 
                       boolused(iused(j,i,p)) = .true.
                    end if
                 end do
              end do  
           enddo
        else                    ! whatever other lib
           do p = 1, nZ
              do i=1,ntimes
                 do j=1,nused(i,p)
c     AL- Leave out CM spectra:
                    if (iused(j,i,p).gt.0  .and.
     &                   iused(j,i,p).ge.nspecLCB(iZstellib1)  .and.
     &                   iused(j,i,p).lt.nspecLCB(iZstellib2+1) )
     &                   then
                       boolused(iused(j,i,p)) = .true.
                    end if
                 end do
              end do
           enddo
        endif
        ntotused=0
        do j=-nmaxCM,nspecLCB(nZstellibLCB+1)-1
          if ( boolused(j) ) then
             ntotused=ntotused+1
             itotused(ntotused)=j
          end if
        end do                              

	return
	end

c-----------------------------------------------------------------------
	subroutine increm_SFR(i, timei, SFRparam,
     &		sigmagasi, twind, SFRi)

c       Compute next SFRi (SFR at time index i)

	implicit none

	integer i
	real timei, twind 
	real SFRparam(*)        ! SFRparam(1) is the type of SFR
	real sigmagasi
	real SFRi

        if (SFRparam(1).eq.0) then !  Instantaneous burst
	   if (i.gt.1) then
               SFRi=0.
	   else
               SFRi=1.
	   endif
 
        elseif (SFRparam(1).eq.1) then !  Constant star formation rate
           if (timei.le.SFRparam(4)) then
              SFRi=SFRparam(3)
           else
              SFRi=0.
           end if
           
        elseif (SFRparam(1).eq.2) then ! Exponentially decreasing or increasing
               SFRi=SFRparam(4)*exp(-timei/SFRparam(3))/SFRparam(3)
 
        else if (SFRparam(1).eq.3) then !  prop to a power of the mass of gas
               SFRi=sigmagasi**SFRparam(3)/SFRparam(4)
 
c       else if (SFRparam(1).eq.n>=10) then
c               SFRi=your SFR law (note that i = time in Myr + 1)
 
        end if
 
        if (timei.ge.twind) SFRi=0. 

	return
	end

c-----------------------------------------------------------------------
      subroutine increm_ppties(i, SFRi, timei, SFRlum, iZinf, iZsup,
     $     alpha,fsub, infall, tinfall, Zinfall, twind,       
     $     ejecta, ejectaZ, massBHNS, massWD, massalive,
     $     ZSFR, sigmaZ, sigmasub, Mgal, sigmaBHNS, sigmaWD, sigmagas,
     $     Zgas,sigmastarsi, Zstarsi, agestarsi)


c	Inputs: i, SFRi, timei, SFRlum, iZinf, iZsup, alpha, 
c		fsub, infall, tinfall, Zinfall, twind, 
c               ejecta, ejectaZ, massBHNS, massWD, massalive, ZSFR,
c       Updates: sigmaZ
c       Outputs: sigmasub(i+1),Mgal(i+1),sigmaBHNS(i+1),sigmaWD(i+1),
c		 sigmagas(i+1),Zgas(i+1),        (tables updated)
c                sigmastarsi, Zstarsi, agestarsi (output only)

      implicit none
      include 'peg_include.f'

      integer i
      integer iZinf(*), iZsup(*)
      integer fsub, infall

      real timei
      real SFRi, SFRlum(*), alpha(*)
      real tinfall, Zinfall, sigmaZ, twind
      real ejecta(nmaxZtracks,nmaxtimes), ejectaZ(nmaxZtracks,nmaxtimes)
      real massBHNS(nmaxZtracks,nmaxtimes), massWD(nmaxZtracks,nmaxtimes)
      real massalive(nmaxZtracks,nmaxtimes)
      real sigmastarsi, Zstarsi, agestarsi
      real sigmasub(*), Mgal(*), sigmaBHNS(*), sigmaWD(*)
      real sigmagas(*), Zgas(*), ZSFR(*)
      
      integer k,q, iZinfk, iZsupk
      real alphak, SFRk, SFRalphak, SFR1malphak
      double precision dejecta, dejectaZ, dsigmaBHNS, dsigmaWD
      double precision dsigmaZ, dsigmagas
      
      
c       Properties at time index i.
      sigmagas(i)=sigmagas(i)-SFRi
      sigmaZ=sigmaZ-SFRi*Zgas(i)
      SFRlum(i)=(1.d0-fsub)*SFRi
      dejecta=0.d0
      dejectaZ=0.d0
      dsigmaBHNS=0.d0
      dsigmaWD=0.d0
      agestarsi=0.d0
      sigmastarsi=0.d0
      Zstarsi=0.d0
      do k=1,i
         iZinfk=iZinf(k)
         iZsupk=iZsup(k)
         q=i+1-k
         alphak=alpha(k)
         SFRk=SFRlum(k)
         if (SFRk.gt.0) then  ! Optim for single bursts (PhP 20011007)
            SFRalphak=SFRk*alphak
            SFR1malphak=SFRk-SFRalphak
            SFR1malphak=SFRk*(1.-alphak)
            dejecta=dejecta+SFRalphak*ejecta(iZinfk,q)
     $           +SFR1malphak*ejecta(iZsupk,q)
            dejectaZ=dejectaZ+SFRalphak*ejectaZ(iZinfk,q)
     $           +SFR1malphak*ejectaZ(iZsupk,q)
     $           +Zgas(k)*(SFRalphak*ejecta(iZinfk,q)
     $           +SFR1malphak*ejecta(iZsupk,q))
            dsigmaBHNS=dsigmaBHNS
     $           +SFRalphak*massBHNS(iZinfk,q)
     $           +SFR1malphak*massBHNS(iZsupk,q)
            dsigmaWD=dsigmaWD
     $           +SFRalphak*massWD(iZinfk,q)
     $           +SFR1malphak*massWD(iZsupk,q)
            sigmastarsi=sigmastarsi+SFRalphak*massalive(iZinfk,q)
     $           +SFR1malphak*massalive(iZsupk,q)
            Zstarsi=Zstarsi+ZSFR(k)
     $           *(SFRalphak*massalive(iZinfk,q)
     $           +SFR1malphak*massalive(iZsupk,q))
            agestarsi=agestarsi+(i-k)
     $           *(SFRalphak*massalive(iZinfk,q)
     $           +SFR1malphak*massalive(iZsupk,q))
         endif
      end do 
      if (sigmastarsi.gt.0.) then
         agestarsi=agestarsi/sigmastarsi
         Zstarsi=Zstarsi/sigmastarsi
      else
         agestarsi=0.
         Zstarsi=0.
      end if
	
c	Properties at time index i+1.
      dsigmagas=dejecta+infall*exp(-timei/tinfall)/tinfall
      dsigmaZ=dejectaZ
     $     +infall*exp(-timei/tinfall)/tinfall*Zinfall
      sigmasub(i+1)=sigmasub(i)+fsub*SFRi
      if (timei.ge.twind) then
         Mgal(i+1)=Mgal(i)-sigmagas(i)-dejecta
         sigmaBHNS(i+1)=sigmaBHNS(i)+dsigmaBHNS
         sigmaWD(i+1)=sigmaWD(i)+dsigmaWD
         sigmagas(i+1)=0.
         sigmaZ=0.
         Zgas(i+1)=0.
      else
         Mgal(i+1)=Mgal(i)+infall
     $        *exp(-timei/tinfall)/tinfall
         sigmaBHNS(i+1)=sigmaBHNS(i)+dsigmaBHNS
         sigmaWD(i+1)=sigmaWD(i)+dsigmaWD
         sigmagas(i+1)=max(sigmagas(i)+dsigmagas,0.d0)
         sigmaZ=max(sigmaZ+dsigmaZ,0.d0)
         if (sigmagas(i+1).le.0.) then
            Zgas(i+1)=0.
         else
            Zgas(i+1)=sigmaZ/sigmagas(i+1)
         end if
      end if                         
      
      return
      end


c----------------------------------------------------------------------
      subroutine redden_cont(codeext, i912, lambda, nlambda, inclin,
     &     Zgasi, sigmagasi,
     &     fluxgal,
     &     fluxext, tauV)
      
c  Inputs: codeext, i912, lambda, nlambda,
c          Zgasi (=current metallicity of the gas),
c          sigmagasi (=current amount of gas),
c  Input/Output (modified by the subroutine) : fluxgal
c  Outputs: fluext, tauV

c modif PhP:2001/09/29: pass "inclin as argument.

      implicit none
      include 'peg_include.f'

      integer codeext, i912, nlambda
      real inclin
      real lambda(*)
      real Zgasi, sigmagasi
      real fluxgal(*)
      real fluxext, tauV
      
      integer nslab1,nslab2,nslab3,nslab4
      parameter(nslab1=17,nslab2=9,nslab3=6,nslab4=10)
      real tauslab(nslab1),albslab(nslab2)
      real asymslab(nslab3),inclinslab(nslab4)
      real emergslab(nslab1,nslab2,nslab3)
      real emerginclinslab(nslab1,nslab2,nslab3,nslab4)
      
      integer nK1,nK2,nK3
      parameter(nK1=12,nK2=9,nK3=11)
      real tauKing(nK1),albKing(nK2),asymKing(nK3)
      real emergKing(nK1,nK2,nK3)
      
      real Zext(5), frac(5)
      integer nlambdaext
      real lambdaext(nmaxlambdaext),tauext(nmaxlambdaext,2)
      real albedoext(nmaxlambdaext,2),asymext(nmaxlambdaext,2)
      
      real coeffextZ(2)
      
      integer istat, j, j0
      integer i01, i02, i03, i04
      real NH, col_dens, mH, albedo, asym, tau
      parameter(mH=1.67e-24)
      real corextcont(nmaxlambda),corextinclincont(nmaxlambda)

      real lambdaV
      parameter(lambdaV = 5500.)

c     Common to redden_r and redden
      common/pegase_extinction/tauslab,albslab,asymslab,emergslab,
     &     inclinslab,emerginclinslab,
     &     tauKing,albKing,asymKing,emergKing,
     &     Zext,frac,nlambdaext,lambdaext,tauext,albedoext,asymext 


c******Apply reddening.
 
      fluxext=0.
      tauV=0.
      if (codeext.ne.0) then
         call dust_composition(Zext,frac, Zgasi,coeffextZ)
 
         
c     If there is some extinction, the Lyman continuum photons
c     not absorbed by the gas are absorbed by the dust.
         
         if (Zgasi.gt.0.) then
            if (i912.gt.1) then
               do j=1,i912-1
                  fluxext=fluxext+(fluxgal(j)+fluxgal(j+1))
     $                 *(lambda(j+1)-lambda(j))/2.
                  fluxgal(j)=0.
               end do
               fluxext=fluxext+(912.-lambda(i912))*fluxgal(i912)
               fluxgal(i912)=0.
            endif
         endif
         
         
********Ellipticals.
         
         if (codeext.eq.1) then
            j0=0
            NH=5.3e22*sigmagasi
            col_dens=0.35*Zgasi*NH*mH*1.4
            call ext(nlambdaext,lambdaext,tauext,
     $           albedoext,asymext,lambdaV,coeffextZ,
     $           tauV,albedo,asym,j0)
            tauV=tauV*col_dens
            
*     Continuum in ellipticals:
            
            i01=0
            i02=0
            i03=0
            j0=0
            do j=1,nlambda
               call ext(nlambdaext,lambdaext,tauext,
     $              albedoext,asymext,lambda(j),coeffextZ,
     $              tau,albedo,asym,j0)
               tau=tau*col_dens
               call Steffen3D(tauKing,albKing,asymKing,emergKing,
     $              nK1,nK2,nK3,tau,albedo,asym,corextcont(j),
     $              i01,i02,i03)
               corextcont(j)=exp(corextcont(j))
            end do
            do j=1,nlambda-1
               fluxext=fluxext+((1.-corextcont(j))*fluxgal(j)
     $              +(1.-corextcont(j+1))*fluxgal(j+1))/2.
     $              *(lambda(j+1)-lambda(j))
            end do
            do j=1,nlambda
               fluxgal(j)=fluxgal(j)*corextcont(j)
            end do
         end if
         
********Disk galaxies.
         
         if (codeext.ge.2) then
            NH=6.8e21*sigmagasi
            col_dens=0.35*Zgasi*NH*mH*1.4
            j0=0
            call ext(nlambdaext,lambdaext,tauext,
     $           albedoext,asymext,lambdaV,coeffextZ,
     $           tauV,albedo,asym,j0)
            tauV=tauV*col_dens
            
*     Continuuum in disks:
            
            i01=0
            i02=0
            i03=0
            i04=0
            j0=0
            do j=1,nlambda
               call ext(nlambdaext,lambdaext,tauext,
     $              albedoext,asymext,lambda(j),coeffextZ,
     $              tau,albedo,asym,j0)
               tau=tau*col_dens
               call Steffen3D(tauslab,albslab,asymslab,emergslab,
     $              nslab1,nslab2,nslab3,tau,albedo,asym,
     $              corextcont(j),i01,i02,i03)
               corextcont(j)=exp(corextcont(j))
               if (codeext.eq.3) then
                  call Steffen4D(tauslab,albslab,asymslab,
     $                 inclinslab,emerginclinslab,nslab1,nslab2,
     $                 nslab3,nslab4,tau,albedo,asym,inclin,
     $                 corextinclincont(j),i01,i02,i03,i04)
                  corextinclincont(j)=exp(corextinclincont(j))
               else
                  corextinclincont(j)=corextcont(j)
               endif
            end do
            do j=1,nlambda-1
               fluxext=fluxext+((1.-corextcont(j))*fluxgal(j)
     $              +(1.-corextcont(j+1))*fluxgal(j+1))/2.
     $              *(lambda(j+1)-lambda(j))
            end do
            do j=1,nlambda
               fluxgal(j)=fluxgal(j)*corextinclincont(j)
            end do                      
         end if
      end if
c     End of if (codeext.ne.0)
      
      return
      end

c----------------------------------------------------------------------
      subroutine redden_lines(codeext, inclin,
     &     lambdaline, nlines, Zgasi, sigmagasi,
     &     flinetot,
     &     fluxext)
      
c     Inputs: codeext, nlines,
c     Zgasi (=current metallicity of the gas),
c     sigmagasi (=current amount of gas),
c     Input/Output (modified by the subroutine) : flinetot
c     Outputs: fluext
      
c     modif PhP:2001/09/29: pass "inclin as argument.

      implicit none
      include 'peg_include.f'

      integer codeext, nlines
      real inclin
      real lambdaline(*)
      real Zgasi, sigmagasi
      real flinetot(*)
      real fluxext
      
      integer nslab1,nslab2,nslab3,nslab4
      parameter(nslab1=17,nslab2=9,nslab3=6,nslab4=10)
      real tauslab(nslab1),albslab(nslab2)
      real asymslab(nslab3),inclinslab(nslab4)
      real emergslab(nslab1,nslab2,nslab3)
      real emerginclinslab(nslab1,nslab2,nslab3,nslab4)
      
      integer nK1,nK2,nK3
      parameter(nK1=12,nK2=9,nK3=11)
      real tauKing(nK1),albKing(nK2),asymKing(nK3)
      real emergKing(nK1,nK2,nK3)
      
      real Zext(5), frac(5)
      integer nlambdaext
      real lambdaext(nmaxlambdaext),tauext(nmaxlambdaext,2)
      real albedoext(nmaxlambdaext,2),asymext(nmaxlambdaext,2)
      
      real coeffextZ(2)
      
      integer istat, j, j0
      integer i01, i02, i03, i04
      real NH, col_dens, mH, albedo, asym, tau
      parameter(mH=1.67e-24)
      real corextline(nmaxlines),corextinclinline(nmaxlines)
      
c     Common to redden_r and redden
      common/pegase_extinction/tauslab,albslab,asymslab,emergslab,
     &     inclinslab,emerginclinslab,
     &     tauKing,albKing,asymKing,emergKing,
     &     Zext,frac,nlambdaext,lambdaext,tauext,albedoext,asymext 
      
      
c******Apply reddening.
      
c      fluxext=0.
      if (codeext.ne.0) then
         call dust_composition(Zext,frac, Zgasi,coeffextZ)
         
         
c     If there is some extinction, the Lyman continuum photons
c     not absorbed by the gas are absorbed by the dust.
         
         if (Zgasi.gt.0.) then
            fluxext=fluxext+flinetot(30)
            flinetot(30)=0.
         endif
         
         
********Ellipticals.
         
         if (codeext.eq.1) then
            j0=0
            NH=5.3e22*sigmagasi
            col_dens=0.35*Zgasi*NH*mH*1.4
            
*     Lines in ellipticals:
 
            do j=1,nlines
               i01=0
               i02=0
               i03=0
               i04=0
               j0=0
               call ext(nlambdaext,lambdaext,tauext,
     $              albedoext,asymext,lambdaline(j),
     $              coeffextZ,tau,albedo,asym,j0)
               tau=tau*col_dens
               call Steffen3D(tauKing,albKing,asymKing,emergKing,
     $              nK1,nK2,nK3,tau,albedo,asym,corextline(j),
     $              i01,i02,i03)
               corextline(j)=exp(corextline(j))
               fluxext=fluxext+(1.-corextline(j))*flinetot(j)
               flinetot(j)=flinetot(j)*corextline(j)
            end do
         end if
         
********Disk galaxies.
 
         if (codeext.ge.2) then
            NH=6.8e21*sigmagasi
            col_dens=0.35*Zgasi*NH*mH*1.4
            
*     Lines in disks:
            
            do j=1,nlines
               i01=0
               i02=0
               i03=0
               i04=0
               j0=0
               call ext(nlambdaext,lambdaext,tauext,
     $              albedoext,asymext,lambdaline(j),coeffextZ,
     $              tau,albedo,asym,j0)
               tau=tau*col_dens
               call Steffen3D(tauslab,albslab,asymslab,emergslab,
     $              nslab1,nslab2,nslab3,tau,albedo,asym,
     $              corextline(j),i01,i02,i03)
               corextline(j)=exp(corextline(j))
               if (codeext.eq.3) then
                  call Steffen4D(tauslab,albslab,asymslab,
     $                 inclinslab,emerginclinslab,nslab1,nslab2,
     $                 nslab3,nslab4,tau,albedo,asym,inclin,
     $                 corextinclinline(j),i01,i02,i03,i04)
                  corextinclinline(j)=exp(corextinclinline(j))
               else
                  corextinclinline(j)=corextline(j)
               endif
               fluxext=fluxext+(1.-corextline(j))*flinetot(j)
               flinetot(j)=flinetot(j)*corextinclinline(j)
            end do
         end if
      end if
c     End of if (codeext.ne.0)
      
      return
      end

*******************

      subroutine convol_SFR_SSPs(
     $     jimpr,
     &     itime,               !< Time in Myr
     &     ntotused,            !< Total number of spectra actually used
     &     itotused,            !< Indices of used spectra
     &     SFRlum,              !< (-> ssp)
     &     iZinf, iZsup,        !< (-> ssp)
     &     invtime,             !< (-> ssp)
     &     beta,                !< Interpolation fact in time
     &     alpha,               !< Interpolation fact in Z
     &     Lspec_SSP,           !< contrib of a spec in SSP
     &     Lspec_gal)           !> integrated spectra (stars)
      
      implicit none
      include 'peg_include.f'

      integer jimpr
      integer itime             ! time Myr
      integer ntotused
      integer itotused(*)       ! dimension nmaxspec
      real    SFRlum(*)         ! dimension nmaxtimes
      integer iZinf(*),iZsup(*) ! dimension nmaxtimes
      integer invtime(*)        ! dimension nmaxtimes
      real    beta(*),alpha(*)  ! dimension nmaxtimes
      real    Lspec_SSP(-nmaxCM:nmaxspecLCB,nmaxtimesSSP,nmaxZtracks)
      real    Lspec_gal(-nmaxCM:nmaxspecLCB,nmaxotimes)
      real    Lbol
c     local variables:
      integer k,j,ij,q
      integer invtimeq,invtimeqp1
      integer iZinfk,iZsupk
      real    contribspecj
      character rien

      Lbol = 0.
      do j=1,ntotused
         contribspecj=0.
         ij=itotused(j)
         do k=1,itime
            if (SFRlum(k).gt.1.e-20) then               
               q=itime+1-k
               iZinfk=iZinf(k)
               iZsupk=iZsup(k)
               invtimeq=invtime(q)
               invtimeqp1=invtimeq+1
               contribspecj=contribspecj + SFRlum(k)*
     &              (beta(q)*
     &              (alpha(k)*Lspec_SSP(ij,invtimeq,iZinfk)
     &              +(1.-alpha(k))*Lspec_SSP(ij,invtimeq,iZsupk))
     &              +(1.-beta(q))*
     &              (alpha(k)*Lspec_SSP(ij,invtimeqp1,iZinfk)
     &              +(1.-alpha(k))*Lspec_SSP(ij,invtimeqp1,iZsupk)))
            end if
         end do
         Lspec_gal(ij, jimpr) = contribspecj
         Lbol = Lbol+contribspecj
      end do  

      end
      
*********
      
      subroutine comput_continuum(Lfits,   
     $     nspecLCB, nlambda, ntimesimpr,
     $     ntotused,itotused,Lspec_gal,flux_gal)
      
      implicit none
      include 'peg_config.f'
      include 'peg_include.f'

      integer Lfits
      integer nspecLCB, nCM, nlambda, ntimesimpr
      real Lspec_gal(-nmaxCM:nmaxspecLCB, nmaxotimes)
      real flux_gal(nmaxlambda, nmaxotimes)
      
      integer ilambda, itimeimpr, ispec, nhdu, hdutype, status
      integer group, fpixel
      real nullval
      parameter(nullval = 1.e-37)
      logical anyf, condition
      real flux_stel(nmaxlambda)
      integer i,ntotused
      integer itotused(*)       ! dimension nmaxspec
      integer LunCM,istat,ii,k
      character*72 a
      real*8  lambdaCM(nlambda)

      do ilambda = 1, nlambda
         do itimeimpr = 1, ntimesimpr
            flux_gal(ilambda, itimeimpr) = 0.
         enddo
      enddo


      do ilambda = 1, nlambda
         do itimeimpr = 1, ntimesimpr
            flux_gal(ilambda, itimeimpr) = 0.
         enddo
      enddo

*     move to primary HDU in stellib FITS file
      nhdu = 1
      status=0
      call ftmahd(Lfits, nhdu, hdutype, status)
      LunCM = 55
      call file_unit(LunCM)
      do i=1,ntotused
         ispec=itotused(i)
         if (ispec.lt.0) then 
*     CM library. We should never enter here with the ELODIE library !
            call file_unit(LunCM)
            call file_open(PEG_ROOT//'data/external/stellibCM.dat',
     $           LunCM,istat)
            read(LunCM,*) nCM
            do ii=1,nCM
               read(LunCM,'(a)') a         
            enddo
            read(LunCM,*) (lambdaCM(k),k=1,nlambda)
            do ii=1,-ispec
               read(LunCM,*) (flux_stel(k),k=1,nlambda)
            end do
            close(LunCM)

         else
*     any other library
*     reading the spectrum
            fpixel = (ispec-1)*nlambda+1
            status=0
            group=1
            call ftgpve(Lfits, group, fpixel, nlambda, nullval, 
     $           flux_stel,anyf, status)
         endif
         do itimeimpr = 1, ntimesimpr
            if (Lspec_gal(ispec,itimeimpr) .gt. 0) then
               do ilambda = 1, nlambda
                  flux_gal(ilambda, itimeimpr) =      
     $                 flux_gal(ilambda, itimeimpr)     
     $                 +flux_stel(ilambda)*Lspec_gal(ispec,itimeimpr)
               enddo
            endif
         enddo         
         
      enddo
      
      end
