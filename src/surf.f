      subroutine surf(e,t)
c This program creates energy surfaces of given cuprate
      implicit double precision(a-h,o-z),integer(i-n)
      dimension dspec(4), e(3), t(3)
      parameter (n=500)
      pi=4.d0*datan(1.d0)
      pi3=3.d0*pi
      a=-3.d0*pi
      b=3.d0*pi
      h=(b-a)/dble(n)
      do i=0,n-1
         do j=0,n-1
            px = a+h*dble(i)
            py = a+h*dble(j)
            call spec(px,py,e,t,dspec,info)
            if (info.ne.0) then
               write(unit=0,fmt=500)px,py,info
               cycle
            endif
            print 600,px,py,dspec
         enddo
      enddo
      stop
  500 format("spec: WARNING: spectrum at point",
     * "(",f7.3,",",f7.3,")"," was not calculated correctly: ",
     * "error flag = ",i6)
  600 format(f10.6,1x,f10.6,4(1x,f11.6))
      end

