      subroutine triang(a0,b0,a1,b1,n,x,y)
      implicit none
c This subroutine generates triangulation of rectangular region
c x in [a0, a1]
c y in [b0, b1]
c
c Parameters
c 'a0' -- input; left boundary for variable x
c 'a1' -- input; right boundary for variable x
c 'b0' -- input; left boundary for variable y
c 'b1' -- input; right boundary for variable y
c 'n   -- input; half the number of simplices
c 'x'  -- output; array(3, 2*n**2); x-coordinates for triangles' verteces
c 'y'  -- output; array(3, 2*n**2); y-coordinates for triangles' verteces
      integer n, i, j
      double precision a0, a1, b0, b1
      double precision x(3,2*n*n),y(3,2*n*n)
      double precision x0(3,2), y0(3,2)
      double precision hx, hy, ha, hb
      common /trng0xy/ x0, y0
      ha = (a1-a0)/dble(n)
      hb = (b1-b0)/dble(n)
      x0(1,1)=0.d0 * ha + a0
      y0(1,1)=0.d0 * hb + b0
      x0(2,1)=1.d0 * ha + a0
      y0(2,1)=0.d0 * hb + b0
      x0(3,1)=1.d0 * ha + a0
      y0(3,1)=1.d0 * hb + b0
      x0(1,2)=0.d0 * ha + a0
      y0(1,2)=0.d0 * hb + b0
      x0(2,2)=1.d0 * ha + a0
      y0(2,2)=1.d0 * hb + b0
      x0(3,2)=0.d0 * ha + a0
      y0(3,2)=1.d0 * hb + b0
      do j = 0, n-1
         do i = 0 , n-1
            hx = ha*dble(i);
            x(1,1 + 2*( i + n*j )) = x0(1,1) + hx;
            x(2,1 + 2*( i + n*j )) = x0(2,1) + hx;
            x(3,1 + 2*( i + n*j )) = x0(3,1) + hx;
c
            x(1,2 + 2*( i + n*j )) = x0(1,2) + hx;
            x(2,2 + 2*( i + n*j )) = x0(2,2) + hx;
            x(3,2 + 2*( i + n*j )) = x0(3,2) + hx;
c
            hy = hb*dble(j);
            y(1,1 + 2*( i + n*j )) = y0(1,1) + hy;
            y(2,1 + 2*( i + n*j )) = y0(2,1) + hy;
            y(3,1 + 2*( i + n*j )) = y0(3,1) + hy;
c
            y(1,2 + 2*( i + n*j )) = y0(1,2) + hy;
            y(2,2 + 2*( i + n*j )) = y0(2,2) + hy;
            y(3,2 + 2*( i + n*j )) = y0(3,2) + hy;
         enddo
      enddo
      return
      end

