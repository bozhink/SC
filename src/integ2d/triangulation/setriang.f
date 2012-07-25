      subroutine setriang()
      implicit none
c This subroutine sets fixed triangulation with 2*n simplices,
c calling the subroutine triang
c
c Parameters
c 'a0' -- input; left boundary for variable x
c 'a1' -- input; right boundary for variable x
c 'b0' -- input; left boundary for variable y
c 'b1' -- input; right boundary for variable y
c 'n   -- half the number of simplices; in the current subroutine this nuber is fixed
      integer n, ntr
      parameter (n=25)
      double precision pi
      parameter(pi=3.1415926535897932d0)
      double precision x(3,2*n*n), y(3,2*n*n)
      common /TRIANGXY/ x, y
      call triang(0.d0, 0.0, pi, pi, n, x, y)
      return
      end
