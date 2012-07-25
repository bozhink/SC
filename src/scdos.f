      subroutine scdos()
c This program calculates density of states of (DOS)
c used cuprates using Monte-Carlo generation df points
      implicit double precision(a-h,o-z),integer(i-n)
c
c nmax  -- number of compounds
c ichan -- number of channels per sign
c n     -- maximal number of iteration per compound
c
      parameter (nmax=13)
      parameter (ichan=18000)
      parameter (nchan=2*ichan+1)
      parameter (nchan4=4*nchan)
      integer*4 n,ii
      parameter (n=5 600 100)
      parameter ( pi = 3.1415926535897932d0)
c     Compounds:
      character*6 compd(nmax)
      character*11 fname
      character*15 dfname
      dimension es(nmax),ef(nmax)
      dimension e(3),t(3),dspec0(4),dspec1(4)
      dimension ispec(4)
      dimension ispcCu4s(-ichan:ichan)
      dimension ispcCu3d(-ichan:ichan)
      dimension ispcO2px(-ichan:ichan)
      dimension ispcO2py(-ichan:ichan)
      data ispcCu4s/nchan*0/
      data ispcCu3d/nchan*0/
      data ispcO2px/nchan*0/
      data ispcO2py/nchan*0/
      data e/0.d0,5.4d0,-1.d0/
      data t/1.5d0,2.d0,0.2d0/
      data compd/"Ca221x","La21xx","La123x","Tl1111","La212x",
     *  "Tl221x","Hg121x","Y123xx","Tl2211","Tl2223","Hg1212",
     *  "Hg1223","Tl221z"/
      data es/11.366d0,16.216d0,5.963d0,6.805d0,5.619d0,
     *  5.387d0,5.212d0,3.614d0,3.614d0,3.997d0,3.934d0,
     *  3.807d0,7.190d0/
      data ef/1.960d0,2.054d0,1.705d0,1.758d0,1.682d0,
     *  1.665d0,1.651d0,1.512d0,1.512d0,1.549d0,
     *  1.543d0,1.531d0,1.780d0/
c
c
      call system_clock(iclock)
      call srand(iclock)
      write(unit=0,fmt=100)
      write(unit=0,fmt=101)
!      do i=-ichan,ichan
!         ispcCu4s(i)=0
!         ispcCu3d(i)=0
!         ispcO2px(i)=0
!         ispcO2py(i)=0
!      enddo
c
c Calculation of maximal and minimal energy levels:
c points (0,0) and (Pi,Pi)
c
      do i=1,nmax
         e(2)=es(i)
         px=0.d0
         py=0.d0
         call spec(px,py,e,t,dspec0,info)
         px=pi
         py=pi
         call spec(px,py,e,t,dspec1,info)
         write(unit=0,fmt=120)compd(i),dspec0,dspec1
      enddo
c
c
c
      do 60 k=1,13
         do i=-ichan,ichan
            ispcCu4s(i)=0
            ispcCu3d(i)=0
            ispcO2px(i)=0
            ispcO2py(i)=0
         enddo
      e(2)=es(k)
c
c Generation of random points
      do ii=1,n
         call nextspec(e,t,ispec,info)
         if (info.ne.0) write(unit=0,fmt=*)info,ii
         ispcCu4s(ispec(4)) = ispcCu4s(ispec(4)) + 1
         ispcCu3d(ispec(3)) = ispcCu3d(ispec(3)) + 1
         ispcO2px(ispec(2)) = ispcO2px(ispec(2)) + 1
         ispcO2py(ispec(1)) = ispcO2py(ispec(1)) + 1
      enddo
c
c Energies
c
c Cu4s
      write(unit=fname,fmt=190)compd(k)
      call wrteng(fname,ichan,ispcCu4s,n,iost)
c Cu3d
      write(unit=fname,fmt=191)compd(k)
      call wrteng(fname,ichan,ispcCu3d,n,iost)
c O2px
      write(unit=fname,fmt=192)compd(k)
      call wrteng(fname,ichan,ispcO2px,n,iost)
c O2py
      write(unit=fname,fmt=193)compd(k)
      call wrteng(fname,ichan,ispcO2py,n,iost)
c
   60 continue
      stop
  100 format("*****************************************************",/,
     *       "______________ Spectrum simulator ___________________",/,
     *       "*****************************************************")
  101 format(/,"Spectrum bounds for all compounds:",/,
     *       "=====================================================")
  120 format(a6,8(1x,f7.3))
  190 format(a6,"-Cu4s")
  191 format(a6,"-Cu3d")
  192 format(a6,"-O2px")
  193 format(a6,"-O2py")
  194 format("dos-",a6,"-Cu4s")
  195 format("dos-",a6,"-Cu3d")
  196 format("dos-",a6,"-O2px")
  197 format("dos-",a6,"-O2py")
      stop
      end

      subroutine nextspec(e,t,ispec,info)
      implicit none
      double precision px,py,pi,pi2
      double precision e(3),t(3),dspec(4)
      integer info,ispec(4)
      parameter ( pi = 3.1415926535897932d0)
      parameter ( pi2 = 2.d0*pi )
      common /spc/ dspec
      px=pi2*rand()
      py=pi2*rand()
      call spec(px,py,e,t,dspec,info)
      ispec(1) = int(dspec(1)*1000.d0)
      ispec(2) = int(dspec(2)*1000.d0)
      ispec(3) = int(dspec(3)*1000.d0)
      ispec(4) = int(dspec(4)*1000.d0)
      return
      end

      subroutine wrteng(fname,ichan,ispc,n,iost)
c
c This subroutine writes energy of an orbital in a file, named fname
      implicit none
      character*11 fname
      integer ichan,ierr,iost,n,i
      integer ispc(-ichan:ichan)
      double precision dn, dspc
      dn = dble(n)
      iost=0
      open(unit=10,file=fname,status="replace",iostat=ierr,err=10)
      do i=-ichan,ichan
         if (ispc(i).eq.0) cycle
         dspc=dble(ispc(i))/dn
         write(unit=10,fmt=200,iostat=ierr,err=12)i,ispc(i),dspc
      enddo
      close(unit=10,iostat=ierr,err=14)
c
c Error handling part of the subroutine:
      return
   10 write(unit=0,fmt=11)fname,ierr
   11 format("Cannot open or create file",a6,": ",i8)
      iost=-1
      return
   12 write(unit=0,fmt=13)fname,ierr
   13 format("Cannot write to file",a6,": ",i8)
      iost=1
      return
   14 write(unit=0,fmt=15)fname,ierr
   15 format("Cannot close file",a6,": ",i8)
      iost=-2
      return
c
c Additional formats:
  200 format(i6,1x,i9,1x,e16.6)
      end

      subroutine wrtdrv(fname,ichan,ispc,k,n,iost)
c
c This subroutine writes derivatives of the energy of orbital k in a file, named fname
      implicit none
      character*15 fname
      integer ichan,k,ierr,iost,n,i
      integer ispc(-ichan:ichan,4)
      double precision dn, deriv, dderiv, h, hh, eps
      parameter (eps=1.d-14)
      parameter (h  = 0.001d0)
      dn = dble(n)
      hh = 1.d0/(2.d0*h)
      iost=0
      open(unit=10,file=fname,status="replace",iostat=ierr,err=10)
      do i=-ichan+1,ichan-1
         deriv=dble(ispc(i+1,k)-ispc(i-1,k))*hh
         dderiv=deriv/dn
         if(dabs(deriv).lt.eps) cycle
         write(unit=10,fmt=201,iostat=ierr,err=12)i,deriv,dderiv
      enddo
      close(unit=10,iostat=ierr,err=14)
c
c Error handling part of the subroutine:
      return
   10 write(unit=0,fmt=11)fname,ierr
   11 format("Cannot open or create file",a6,": ",i8)
      iost=-1
      return
   12 write(unit=0,fmt=13)fname,ierr
   13 format("Cannot write to file",a6,": ",i8)
      iost=1
      return
   14 write(unit=0,fmt=15)fname,ierr
   15 format("Cannot close file",a6,": ",i8)
      iost=-2
      return
c
c Additional formats:
  201 format(i6,1x,e16.6,1x,e16.6)
      end
      
