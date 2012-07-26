      blockdata initial
      implicit doubleprecision(a-h,o-z),integer(i-n)
      parameter (n=4, lda=n, lwork=1024)
      parameter (ntriang=25)
      parameter (pi8=3.1415926535897932d0)
      doubleprecision H(lda,n),EYE4(lda,n)
      doubleprecision w(n), work(lwork)
      doubleprecision x(3,2*ntriang*ntriang)
      doubleprecision y(3,2*ntriang*ntriang)
c Constants
      common /CONST/ pi
c Triangulation grid
      common /TRIANGXY/ x, y
c Common block Superconductivity Bloch-Huckel hamiltonian
      common /SCBHH/ H,EYE4,w,work
c
c DATA part
c
c Constants
      data pi /pi8/
c Unity matrix
      data EYE4 /1.d0, 0.d0, 0.d0, 0.d0,
     *           0.d0, 1.d0, 0.d0, 0.d0,
     *           0.d0, 0.d0, 1.d0, 0.d0,
     *           0.d0, 0.d0, 0.d0, 1.d0/
      save
      end
      