      subroutine vhenergy(e,t,evh)
      implicit none
      double precision e(3), t(3), evh(4)
      double precision px, py
      integer info
      px = 3.1415926535897932d0
      py = 0.d0
      call spec(px,py,e,t,evh,info)
      return
      end
