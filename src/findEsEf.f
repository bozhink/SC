      SUBROUTINE GETESEF(N, SPARAM, TEMP, E, T, ES, EFERMI,
     *                       FILLINGF, IFPRINT)
      IMPLICIT NONE
C
      INTEGER N, I, IFPRINT
      DOUBLE PRECISION SPARAM(*), ES(*), EFERMI(*), E(3), T(3)
      DOUBLE PRECISION FILLINGF, TEMP
      DO I = 1, N
         EFERMI(I) = 1.543D0
         E(2) = 3.986D0
         CALL FINDESEF(SPARAM(I), E, T, TEMP, EFERMI(I), FILLINGF)
         ES(I) = E(2)
         IF(IFPRINT.NE.0) PRINT 100,
     *         SPARAM(I), E(2), EFERMI(I)
      ENDDO
      RETURN
  100 FORMAT(F4.2,1X,F7.3,1X,F7.3)
      END      
