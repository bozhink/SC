      function td(x)
      implicit none
      double precision td, x, cut
      parameter(cut=0.01d0)
      if (dabs(x).lt.cut) then
         td=(0.4d0*x*x-1.d0)*x*x/3.d0+1.d0
      else
         td=dtanh(x)/x
      endif
      return
      end

      function tcintgnd(px,py)
      implicit none
c
c This subroutine calculates Tc-integrand function for
c Bloch-Huckel hamiltonian
c
c chip^2*(td((ep-Ef)/2T)+1)
c
c where ep is the third eigenvalue of Bloch-Huckel (BH) hamiltonian
c and chip = Dp*Sp, where wave-vector is psi_p=(Dp,Sp,Xp,Yp)
c
c Parameter list
c px, py -- input; scalars; components of the wave-vector
c Temp   -- scalar; temperature (in eV)
c Ef     -- scalar; Fermi energy level
c e      -- vector; List of diagonal components of BH hamiltonian
c           e = (ed, es, ep)
c t      -- vector; List of hopping parameters
c           t = (tpd, tsp, tpp)
c tcintgnd -- output; scalar; value of Tc-integrand function
c info   -- scalar; error flag of dsygv
      double precision tcintgnd, px, py
      double precision T, Ef, ed, es, ep, tpd, tsp, tpp
      double precision sx,sy,chip,td
      integer itype, lwork, n, lda, ieigen, info
      character*1 jobz,uplo
      parameter (uplo='L',jobz='V')
      parameter (itype=1,lwork=1024)
      parameter (n=4, lda=n, ieigen=3)
      double precision H(lda,n), EYE4(lda,n)
      double precision w(n), work(lwork)
c Common block Superconductivity Bloch-Huckel hamiltonian
      common /SCBHH/ H, EYE4, w, work
      common /TCPARAM/ T, Ef, ed, es, ep, tpd, tsp, tpp
      common /TCINFO/ info
      info=0
c Calculation of BH hamiltonian matrix
      sx=2.d0*dsin(0.5*px)
      sy=2.d0*dsin(0.5*py)
      H(1,1)=ed
      H(2,2)=es
      H(3,3)=ep
      H(4,4)=ep
      H(1,2)=0
      H(1,3)= tpd*sx
      H(1,4)=-tpd*sy
      H(2,3)= tsp*sx
      H(2,4)= tsp*sy
      H(3,4)=-tpp*sx*sy
      H(2,1)=H(1,2)
      H(3,1)=H(1,3)
      H(3,2)=H(2,3)
      H(4,1)=H(1,4)
      H(4,2)=H(2,4)
      H(4,3)=H(3,4)
c Calculation of Eigenvalues and eigenvectors
c using LAPACK's function dsygv
      call dsygv(itype,jobz,uplo,n,H,lda,EYE4,lda,w,work,lwork,info)
      if (info.ne.0) write(unit=0,fmt=10) info,px,py
      chip=H(1,ieigen)*H(2,ieigen)
      tcintgnd=chip*chip*td(0.5d0*(w(ieigen)-Ef)/T)
      return
   10 format('tcintgnd: WARNING: dsygv returned error flag=',i4,
     * 'at point (',f5.2,',',f5.2,')')
      end

      function i1(Temp,Ef,e,t,ifprint)
      implicit none
c This function calculates integral over tcintgnd
c
c Parameters
c 'Temp' -- input; temperature in [eV]
c 'e'    -- input; BH hamiltonian parameters (ed,es,ep)
c 't'    -- input; BH hamiltonian parameters (tpd, tsp, tpp)
c 'Ef'   -- input; fermi energy level in [eV]
c 'ifprint' -- input; logical; prints information
      double precision i1, Temp, Ef, e(3), t(3)
      logical ifprint
      double precision tcintgnd
      external tcintgnd
      double precision pi, eps, res
      parameter (pi=3.1415926535897932d0)
      parameter (eps=1.d-4)
      integer nevals, iflag
      double precision ed, es, ep, tpd, tsp, tpp
      double precision tc, efermi
      common /TCPARAM/ tc, efermi, ed, es, ep, tpd, tsp, tpp
      tc = Temp
      efermi = Ef
      ed = e(1)
      es = e(2)
      ep = e(3)
      tpd = t(1)
      tsp = t(2)
      tpp = t(3)
      call integ2d(tcintgnd,eps,res,nevals,iflag)
      i1=res/(pi*pi)
      if (ifprint) write(unit=0,fmt=100)Temp,es,Ef,i1,nevals,iflag
      return
  100 format('i1: Temp= ',f9.6,' es= ',f7.3,' Ef= ',f7.3,
     * ' i1= ',f12.6,' nevals= ',i5,' iflag= ',i4)
      end

      function zi1(Temp,Ef,e,t,z,ifprint)
      implicit none
c This function calculates integral over tcintgnd
c
c Parameters
c 'Temp' -- input; temperature in [eV]
c 'e'    -- input; BH hamiltonian parameters (ed,es,ep)
c 't'    -- input; BH hamiltonian parameters (tpd, tsp, tpp)
c 'z'    -- input; renormalization parameter
c 'Ef'   -- input; fermi energy level in [eV]
c 'ifprint' -- input; logical; prints information
      double precision zi1, Temp, Ef, e(3), t(3), z
      logical ifprint
      double precision tcintgnd
      external tcintgnd
      double precision pi, eps, res
      parameter (pi=3.1415926535897932d0)
      parameter (eps=1.d-4)
      integer nevals, iflag
      double precision ed, es, ep, tpd, tsp, tpp
      double precision tc, efermi
      common /TCPARAM/ tc, efermi, ed, es, ep, tpd, tsp, tpp
      tc = Temp
      efermi = Ef
      ed = e(1)/z
      es = e(2)/z
      ep = e(3)/z
      tpd = t(1)/z
      tsp = t(2)/z
      tpp = t(3)/z
      call integ2d(tcintgnd,eps,res,nevals,iflag)
      zi1=res/(pi*pi)
      if (ifprint) write(unit=0,fmt=100)Temp,es,Ef,zi1,nevals,iflag
      return
  100 format('zi1: Temp= ',f9.6,' es= ',f7.3,' Ef= ',f7.3,
     * ' i1= ',f12.6,' nevals= ',i5,' iflag= ',i4)
      end

      function tcinteg()
      implicit none
c This function calculates integral over tcintgnd
      logical ifprint
      double precision tcinteg, tcintgnd
      external tcintgnd
      double precision pi, eps, res
      parameter (pi=3.1415926535897932d0)
      parameter (eps=1.d-4)
      integer nevals, iflag
      common /TCPRINT/ ifprint
      call integ2d(tcintgnd,eps,res,nevals,iflag)
      tcinteg=res/(pi*pi)
      if (ifprint) write(unit=0,fmt=100)tcinteg,nevals,iflag
      return
  100 format('tcinteg: i1= ',f12.6,' nevals= ',i5,' iflag= ',i4)
      end

      subroutine sfindtc(Ef, Jsd, e, t, Tc)
      implicit none
      double precision Ef, Jsd, e(3), t(3), Tc
      double precision findtc
      external findtc
      Tc = findtc(Ef, Jsd, e, t)
      return
      end

      function findtc(Efermi,Jsd,e,t)
      implicit none
      double precision findtc, Jsd, e(3), t(3), Efermi, tcinteg
      external tcinteg
      double precision T0, T1, FT0, FT1, DF, Jsd2
      double precision ABSERR, RELERR, ZERO, Kelvin
      integer NMAX, ITER, i, j, ierr
      parameter (ZERO=1.D-300)
      parameter (NMAX=30)
      parameter(Kelvin=1.d0/11604.d0)
      logical ifprint
      double precision Temp, Ef, ed, es, ep, tpd, tsp, tpp
      common /TCPARAM/ Temp, Ef, ed, es, ep, tpd, tsp, tpp
      common /TCPRINT/ ifprint
C
      ABSERR=1.d-3
      RELERR=1.d-3
      Jsd2 = 0.5d0*Jsd
      ed = e(1)
      es = e(2)
      ep = e(3)
      tpd = t(1)
      tsp = t(2)
      tpp = t(3)
      Ef = Efermi
      T0 = 10*Kelvin
      Temp = T0
      FT0 = T0 - Jsd2*tcinteg()
      T1 = 200*Kelvin
      Temp = T1
      FT1 = T1 - Jsd2*tcinteg()
      ITER = 0
      ! SECANT METHOD STARTS HERE
      DO J=1,NMAX
         ITER = ITER+1                        ! NUMBER OF ITERATIONS
         DF = FT1 - FT0                       ! THIS PARAMETER GOES IN DENOMINATOR
         IF (DABS(DF).LT.ZERO) THEN           ! SO IT MUST NOT BE ZERO
            IERR=1                            ! BUT IF IT IS ZERO
            IF (IFPRINT) WRITE(UNIT=0,FMT=10) ! ERROR IS RAISED
            Temp=0.D0                         ! AND Temp IS SET TO ZERO
            GOTO 1
         ENDIF
         Temp = T1 - FT1*(T1-T0)/DF           ! NET APPROXIMATION: SECANT STEP
         IF (DABS(T1).LT.ZERO) THEN
            IF (IFPRINT) WRITE(UNIT=0,FMT=11)
            IF (DABS(Temp-T1).LT.ABSERR) GOTO 1
         ELSE
            IF (DABS((Temp-T1)/T1).LT.ABSERR) GOTO 1
         ENDIF
         T0  = T1
         FT0 = FT1
         T1  = Temp
         FT1 = T1 - Jsd2*tcinteg()
      ENDDO
    1 CONTINUE
      ! SECANT METHOD ENDS HERE
      findtc = Temp
      RETURN
   10 FORMAT('findtc: ERROR: function finteg is ',
     * 'too slowly changing by EF')
   11 FORMAT('findtc: WARNING: old approximation ',
     * 'is too close to zero')
      END

      function zfindtc(Efermi,Jsd,e,t,z)
      implicit none
      double precision zfindtc, Jsd, e(3), t(3), Efermi, tcinteg, z
      external tcinteg
      double precision T0, T1, FT0, FT1, DF, Jsd2
      double precision ABSERR, RELERR, ZERO, Kelvin
      integer NMAX, ITER, i, j, ierr
      parameter (ZERO=1.D-300)
      parameter (NMAX=30)
      parameter(Kelvin=1.d0/11604.d0)
      logical ifprint
      double precision Temp, Ef, ed, es, ep, tpd, tsp, tpp
      common /TCPARAM/ Temp, Ef, ed, es, ep, tpd, tsp, tpp
      common /TCPRINT/ ifprint
C
      ABSERR=1.d-3
      RELERR=1.d-3
      Jsd2 = 0.5d0*Jsd
      ed = e(1)/z
      es = e(2)/z
      ep = e(3)/z
      tpd = t(1)/z
      tsp = t(2)/z
      tpp = t(3)/z
      Ef = Efermi
      T0 = 10*Kelvin
      Temp = T0
      FT0 = T0 - Jsd2*tcinteg()
      T1 = 200*Kelvin
      Temp = T1
      FT1 = T1 - Jsd2*tcinteg()
      ITER = 0
      ! SECANT METHOD STARTS HERE
      DO J=1,NMAX
         ITER = ITER+1                        ! NUMBER OF ITERATIONS
         DF = FT1 - FT0                       ! THIS PARAMETER GOES IN DENOMINATOR
         IF (DABS(DF).LT.ZERO) THEN           ! SO IT MUST NOT BE ZERO
            IERR=1                            ! BUT IF IT IS ZERO
            IF (IFPRINT) WRITE(UNIT=0,FMT=10) ! ERROR IS RAISED
            Temp=0.D0                         ! AND Temp IS SET TO ZERO
            GOTO 1
         ENDIF
         Temp = T1 - FT1*(T1-T0)/DF           ! NET APPROXIMATION: SECANT STEP
         IF (DABS(T1).LT.ZERO) THEN
            IF (IFPRINT) WRITE(UNIT=0,FMT=11)
            IF (DABS(Temp-T1).LT.ABSERR) GOTO 1
         ELSE
            IF (DABS((Temp-T1)/T1).LT.ABSERR) GOTO 1
         ENDIF
         T0  = T1
         FT0 = FT1
         T1  = Temp
         FT1 = T1 - Jsd2*tcinteg()
      ENDDO
    1 CONTINUE
      ! SECANT METHOD ENDS HERE
      zfindtc = Temp
      RETURN
   10 FORMAT('zfindtc: ERROR: function finteg is ',
     * 'too slowly changing by EF')
   11 FORMAT('findtc: WARNING: old approximation ',
     * 'is too close to zero')
      END

      FUNCTION GETJSD(TC, EFERMI, E, T)
      IMPLICIT NONE
      DOUBLE PRECISION GETJSD, EFERMI, TC, E(3), T(3), tcinteg
      EXTERNAL tcinteg
      LOGICAL ifprint
      DOUBLE PRECISION TEMP, EF, ed, es, ep, tpd, tsp, tpp
      COMMON /TCPARAM/ TEMP, EF, ed, es, ep, tpd, tsp, tpp
      COMMON /TCPRINT/ ifprint
      TEMP=TC
      EF=EFERMI
      ED=E(1)
      ES=E(2)
      EP=E(3)
      TPD=T(1)
      TSP=T(2)
      TPP=T(3)
      GETJSD=2.D0*TC/tcinteg()
      IF (IFPRINT) PRINT 200, TC, GETJSD
      RETURN
  100 FORMAT('ERROR IN GETJSD: Z=0')
  101 FORMAT('ERROR IN GETJSD: Z=0. RETURNED JSD=0')
  200 FORMAT('GETJSD RESULTS: Z=',F6.3,'; TC=',F8.5,
     *  ' EV; JSD=',F6.3,'EV')
      END

      FUNCTION ZGETJSD(TC, EFERMI, Z, E, T)
      IMPLICIT NONE
      DOUBLE PRECISION ZGETJSD, EFERMI, TC, Z, E(3), T(3), tcinteg
      EXTERNAL tcinteg
      LOGICAL ifprint
      DOUBLE PRECISION TEMP, EF, ed, es, ep, tpd, tsp, tpp
      COMMON /TCPARAM/ TEMP, EF, ed, es, ep, tpd, tsp, tpp
      COMMON /TCPRINT/ ifprint
      IF (DABS(Z).LT.1.D-100) THEN
         WRITE(UNIT=0,FMT=100)
         IF (IFPRINT) PRINT 101
         ZGETJSD=0.D0
         RETURN
      ENDIF
      TEMP=TC
      EF=EFERMI
      ED=E(1)/Z
      ES=E(2)/Z
      EP=E(3)/Z
      TPD=T(1)/Z
      TSP=T(2)/Z
      TPP=T(3)/Z
      ZGETJSD=2.D0*TC/tcinteg()
      IF (IFPRINT) PRINT 200, Z, TC, ZGETJSD
      RETURN
  100 FORMAT('ERROR IN GETJSD: Z=0')
  101 FORMAT('ERROR IN GETJSD: Z=0. RETURNED JSD=0')
  200 FORMAT('GETJSD RESULTS: Z=',F6.3,'; TC=',F8.5,
     *  ' EV; JSD=',F6.3,'EV')
      END
!
!
!
!c Test function
!      subroutine test3()
!      double precision temp, e(3), t(3),ef,jsd
!      temp=90.d0/11604.d0
!      jsd=5.647d0
!      e(1)=0.d0
!      e(2)=3.807d0
!      e(3)=-1.d0
!      t(1)=1.5d0
!      t(2)=2.d0
!      t(3)=0.2d0
!      ef=1.531d0
!      temp=findtc(ef,jsd,e,t)
!      print*,'test3 ', temp*11604.d0
!      return
!      end
