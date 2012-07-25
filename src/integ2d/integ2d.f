      subroutine integ2d(f,eps,res,nevals,iflag)
      implicit none
c This subroutine performs numerical 2d integration of a function f(x,y)
c in the region
c x in [0,pi]
c y in [0,pi]
c using the subroutine twodq
c
c Parameters
c 'f' -- input; external; double precision function f(x,y), which will be integrated
c 'eps' -- input; error estimate
c 'res' -- output; returned result
c 'nevals' -- output; number of evaluation of f (see twodq)
c 'iflag'  -- output; error flag (see twodq)
      integer n, nu, nd, nevals, iflag, iwork(100)
      parameter(n=25)
      double precision f, eps, res, err
      double precision x(3,2*n*n), y(3,2*n*n)
      double precision dat(450)
      external f
      common /TRIANGXY/ x, y
      common /cinteg2d/ iwork, dat
      nu=0
      nd=0
      iflag=1
      call twodq(f,2*n*n,x,y,eps,1,50,4000,res,err,nu,nd,
     *  nevals,iflag,dat,iwork)
      return
      end

!
!
!
!
!c Test functions
!      double precision function ss(x,y)
!      double precision x,y
!      ss=(x*x+y*y)
!      return
!      end
!
!      subroutine test1()
!         implicit none
!         double precision ss, eps, res
!         integer nevals, iflag
!         external ss
!         eps=1.d-3
!         call integ2d(ss,eps,res,nevals,iflag)
!         print*,'test1 ',eps
!         print*,'test1 ',res
!         print*,'test1 ',nevals
!         print*,'test1 ',iflag
!         return
!      end

