      SUBROUTINE FINDESEF(S,E,T,TEMP,EFERMI,FILLINGF)
      IMPLICIT NONE
      DOUBLE PRECISION S, E(3), T(3), TEMP, EFERMI, FILLINGF! FILLING FACTOR = 1/2 - P, P=0.16
      DOUBLE PRECISION FINDEF
      EXTERNAL FINDEF
      LOGICAL IFPRINT
      INTEGER NMAX, ITER, IERR, I, J
      DOUBLE PRECISION RELERR, ABSERR, ZERO
      PARAMETER (ZERO=1.D-300)
      DOUBLE PRECISION TMP, EF, Ed, Es, Ep, Tpd, Tsp, Tpp
      COMMON /FERMIPAR/ TMP, EF, Ed, Es, Ep, Tpd, Tsp, Tpp
      COMMON /LPRINT/ IFPRINT
C
      TMP = TEMP
      ed = e(1)
      es = e(2)
      ep = e(3)
      tpd = t(1)
      tsp = t(2)
      tpp = t(3)
      NMAX = 30
      RELERR = 0.001D0
      ABSERR = 0.001D0
      IFPRINT = .FALSE.
      EFERMI = 1.666D0
      E(2) = 4.D0*S*Tsp*Tsp/(EFERMI-Ep) + EFERMI
      Es   = E(2)
      

      DO I=1,NMAX
         EFERMI = FINDEF(TEMP, E, T, FILLINGF)
         ! NEXT APPROXIMATION TO ES AND EF:
         E(2) = 4.D0*S*Tsp*Tsp/(EFERMI-Ep) + EFERMI
         IF (IFPRINT) THEN
            WRITE(UNIT=0,FMT=100)I,E(2),EFERMI,ITER,IERR
         ENDIF
         IF (DABS(E(2)).LT.ZERO) THEN
            IF (DABS(Es-E(2)).LT.ABSERR) RETURN
         ELSE
            IF (DABS((Es-E(2))/E(2)).LT.RELERR) RETURN
         ENDIF
         Es = E(2)
      ENDDO
      WRITE(UNIT=0,FMT=200)
      RETURN
   10 FORMAT('FINDESEF: ERROR: function finteg is ',
     * 'too slowly changing by EF')
   11 FORMAT('FINDESEF: WARNING: old approximation ',
     * 'is too close to zero')
  100 FORMAT('FINDESEF: #',I2,1X,F7.3,1X,F7.3,1X,'ITERATIONS=',I2,
     *       1X,'FLAG=',I2)
  200 FORMAT('FINDESEF: MAXIMUM NUMBER OF ITERATIONS EXCEEDED')
      END
