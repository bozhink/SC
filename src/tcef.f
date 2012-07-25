c This program calculates maximal critical temperature Tc
c for fixed parameter es for given cuprate as a function of E_Fermi
      subroutine tcef(Jsd, e, t)
      implicit none
      double precision Kelvin
      parameter(Kelvin=1.d0/11604.d0)
      integer n, i
      parameter (n=100)
      double precision z, findtc
      integer iter, ierr
      character(len=5) fname
      double precision Ef, efmin, efmax, Efermi(n)
      double precision e(3), t(3), Jsd
c
      efmin=0.6d0
      efmax=2.4d0
      print*,'Jsd=',Jsd
      print*,'es=',e(2)
      call linspace(n,efmin,efmax,Efermi)
      fname='x.dat'
c
      open(unit=20,file=fname,status='replace')
!
      do i=1,n
         z=findtc(Efermi(i),Jsd,e,t)
         print 200, Efermi(i),z,z/Kelvin
         write(unit=20,fmt=200) Efermi(i),z,z/Kelvin
      enddo
!
      close(unit=20)
!
      stop
  100 format(f6.3,1x,f9.6,1x,f6.1,1x,i4,1x,i4)
  200 format(f6.3,1x,f9.6,1x,f6.1)
  600 write(unit=0,fmt=601) ierr
  601 format('tcef: invalid input argument: ',i6)
      stop 'invalid arguments'
      end

!      function getzero(T)
!      implicit none
!      double precision getzero, T
!      double precision i1
!      external i1
!      double precision Ef, es, Jsd
!      common /tcefroot/ Ef, es, Jsd
!      getzero=2.d0*T-Jsd*i1(T,es,Ef,.false.)
!      return
!      end

c Hg1212
!      Jsd=5.68d0
!      es=3.934d0
!      T=127*Kelvin
!      Ef=1.543d0
c Hg1223
!      Jsd=5.68d0
!      es=3.807d0
!      T=135*Kelvin
!      Ef=1.531d0
c Tl2223
!      Jsd=5.68d0
!      es=3.997d0
!      T=125*Kelvin
!      Ef=1.549d0
