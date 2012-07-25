      subroutine spec(px,py,e,t,dspec,info)
      implicit none
c
c This subroutine calculates spectrum of the
c Bloch-Huckel hamiltonian
c
c Parameter list
c px, py -- input; scalars; components of the wave-vector
c e      -- input; vector; List of diagonal components of BH hamiltonian
c           e = (ed, es, ep)
c t      -- input; vector; List of hopping parameters
c           t = (tpd, tsp, tpp)
c dspec  -- output; vector; sorted spectrum
c info   -- output; scalar; error flag of dsygv
      double precision px,py,e(3),t(3),dspec(4)
      integer itype,lwork,n,lda,ieigen,info
      double precision sx,sy
      character*1 jobz,uplo
      parameter (uplo='L',jobz='N')
      parameter (itype=1,lwork=1024)
      parameter (n=4, lda=n)
      double precision H(lda,n),EYE4(lda,n)
      double precision w(n), work(lwork)
c Common block Superconductivity Bloch-Huckel hamiltonian
      common /SCBHH/ H,EYE4,w,work
      info=0
c Calculation of BH hamiltonian matrix
      sx=2.d0*dsin(0.5d0*px)
      sy=2.d0*dsin(0.5d0*py)
      H(1,1)=e(1)
      H(2,2)=e(2)
      H(3,3)=e(3)
      H(4,4)=e(3)
      H(1,2)=0
      H(1,3)= t(1)*sx
      H(1,4)=-t(1)*sy
      H(2,3)= t(2)*sx
      H(2,4)= t(2)*sy
      H(3,4)=-t(3)*sx*sy
      H(2,1)=H(1,2)
      H(3,1)=H(1,3)
      H(3,2)=H(2,3)
      H(4,1)=H(1,4)
      H(4,2)=H(2,4)
      H(4,3)=H(3,4)
c Calculation of Eigenvalues and eigenvectors
c using LAPACK's function dsygv
      call dsygv(itype,jobz,uplo,n,H,lda,EYE4,lda,w,work,lwork,info)
      dspec(1)=w(1)
      dspec(2)=w(2)
      dspec(3)=w(3)
      dspec(4)=w(4)
      return
      end
