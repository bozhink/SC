      doubleprecision function fdist(px,py)
      implicit none
c
c This function calculates Fermi distribution function for
c Bloch-Huckel hamiltonian
c
c (exp((ep-Ef)/T)+1)^{-1}
c
c where ep is the third eigenvalue of Bloch-Huckel (BH) hamiltonian
c
c Parameter list
c px, py -- input; scalars; components of the wave-vector
c T      -- scalar; temperature (in eV)
c Ef     -- scalar; Fermi energy level
c fidst  -- scalar; value of Fermi-distribution function
c info   -- scalar; error flag of dsygv
      integer ieigen, info
      integer itype, lwork, n, lda
      character*1 jobz, uplo
      double precision px, py
      double precision T, Ef, Ed, Es, Ep, Tpd, Tsp, Tpp
      double precision sx, sy
      parameter (uplo='L',jobz='N')
      parameter (itype=1, lwork=1024)
      parameter (n=4, lda=n, ieigen=3)
      double precision H(lda,n), EYE4(lda,n)
      double precision w(n), work(lwork)
c Common block Superconductivity Bloch-Huckel hamiltonian
      common /SCBHH/ H, EYE4, w, work
c Common parameters. All are in eV
      common /FERMIPAR/ T, Ef, Ed, Es, Ep, Tpd, Tsp, Tpp
c Common block: Error flag
      common /EINFO/ info
      info=0
c Calculation of BH hamiltonian matrix
      sx=2.d0*dsin(0.5*px)
      sy=2.d0*dsin(0.5*py)
      H(1,1)=Ed
      H(2,2)=Es
      H(3,3)=Ep
      H(4,4)=Ep
      H(1,2)=0
      H(1,3)= Tpd*sx
      H(1,4)=-Tpd*sy
      H(2,3)= Tsp*sx
      H(2,4)= Tsp*sy
      H(3,4)=-Tpp*sx*sy
      H(2,1)=H(1,2)
      H(3,1)=H(1,3)
      H(3,2)=H(2,3)
      H(4,1)=H(1,4)
      H(4,2)=H(2,4)
      H(4,3)=H(3,4)
c Calculation of Eigenvalues and eigenvectors
c using LAPACK's function dsygv
      call dsygv(itype,jobz,uplo,n,H,lda,EYE4,lda,w,work,lwork,info)
      fdist=1.d0/(dexp((w(ieigen)-Ef)/T)+1.d0)
      return
      end

      doubleprecision function finteg()
      implicit none
c This function calculates the Fermi integral:
c 2D integral of Fermi distribution function over
c First zone of Brillouin
c
c This function has no input psrameters: They are passed through
c common blocks
      external fdist
      double precision res, pi, fdist
      parameter (pi=3.1415926535897932D0)
      integer nevals, iflag
      logical ifprint
      common /LPRINT/ ifprint
      call integ2d(fdist,1.d-4,res,nevals,iflag)
      finteg=res/(pi*pi)
      if (ifprint) write(unit=0,fmt=100) finteg, nevals, iflag
      return
  100 format('finteg:',e10.4,1x,i7,1x,i7)
      end

      FUNCTION FINDEF(TEMP, E, T, FILLINGF)
      IMPLICIT NONE
C NOTE THAT FILLINGF IS THE FOLLOWING FILLING FACTOR
C   FILLINGF = 1/2 - P
C
C E = (ED, ES, EP)
C T = (TPD, TSP, TPP)
      DOUBLE PRECISION FINDEF, FINTEG, EFERMI
      DOUBLE PRECISION TEMP, E(3), T(3), FILLINGF
      EXTERNAL FINTEG
      LOGICAL IFPRINT
      INTEGER NMAX, ITER, IERR, I, J
      DOUBLE PRECISION EF0, F0, EF1, F1, DF
      DOUBLE PRECISION RELERR, ABSERR, ZERO
      PARAMETER (ZERO=1.D-300)
      DOUBLE PRECISION Ed, Es, Ep, Tpd, Tsp, Tpp
      DOUBLE PRECISION TMP, EF
      COMMON /FERMIPAR/ TMP, EF, Ed, Es, Ep, Tpd, Tsp, Tpp
      COMMON /LPRINT/ IFPRINT
C
      TMP = TEMP
      Ed  = E(1)
      Es  = E(2)
      Ep  = E(3)
      Tpd = T(1)
      Tsp = T(2)
      Tpp = T(3)
      NMAX = 30
      RELERR = 0.001D0
      ABSERR = 0.001D0
      IFPRINT = .FALSE.
      EF0 = 0.1D0
      EF  = EF0
      F0  = FINTEG() - FILLINGF
      EF1 = 3.0D0
      EF  = EF1
      F1  = FINTEG() - FILLINGF
      ITER = 0
      ! SECANT METHOD STARTS HERE
      DO J=1,NMAX
         ITER = ITER+1                        ! NUMBER OF ITERATIONS
         DF = F1 - F0                         ! THIS PARAMETER GOES IN DENOMINATOR
         IF (DABS(DF).LT.ZERO) THEN           ! SO IT MUST NOT BE ZERO
            IERR=1                            ! BUT IF IT IS ZERO
            IF (IFPRINT) WRITE(UNIT=0,FMT=10) ! ERROR IS RAISED
            EFERMI=0.D0                       ! AND EFERMI IS SET TO ZERO
            GOTO 1
         ENDIF
         EFERMI = EF1 - F1*(EF1-EF0)/DF       ! NET APPROXIMATION: SECANT STEP
         IF (DABS(EF1).LT.ZERO) THEN
            IF (IFPRINT) WRITE(UNIT=0,FMT=11)
            IF (DABS(EFERMI-EF1).LT.ABSERR) GOTO 1
         ELSE
            IF (DABS((EFERMI-EF1)/EF1).LT.ABSERR) GOTO 1
         ENDIF
         EF0 = EF1
         F0  = F1
         EF1 = EFERMI
         EF  = EF1
         F1  = FINTEG() - FILLINGF
      ENDDO
    1 CONTINUE
      ! SECANT METHOD ENDS HERE
      !
      FINDEF=EFERMI
      RETURN
   10 FORMAT('FINDESEF: ERROR: function finteg is ',
     * 'too slowly changing by EF')
   11 FORMAT('FINDESEF: WARNING: old approximation ',
     * 'is too close to zero')
      END
!
!
!c Test function
!      subroutine test2()
!      double precision temp, ff, e(3), t(3),ef
!      temp=90.d0/11604.d0
!      e(1)=0.d0
!      e(2)=5.212d0
!      e(3)=-1.d0
!      t(1)=1.5d0
!      t(2)=2.d0
!      t(3)=0.2d0
!      ff=0.34d0
!      ef=findef(temp,e,t,ff)
!      print*,'test2 ', ef
!      return
!      end
!      