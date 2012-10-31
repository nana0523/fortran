      program test
      implicit none
       real*8 pi,p,q,r,tmr2(3,3), v(3), dv(3), s(3), vr(3), tmm(3,3),
     +x,y,z,x1,y1,pi1,cpi,pi2,ta(3,3)
      integer k,ka,kb,k1,k2,k3,k4


      pi = 4.d0 * datan(1.d0)    
           
      write (*,*) 'Give a vector (p,q,r):'
      open(1, file='pf.out', status='unknown')
      read (*,*) p,q,r
     
      s(:)=0.d0

      dv(1)=p
      dv(2)=q
      dv(3)=r
c     compose the 3-fold array     
      ta(:,:)=0.d0
      ta(1,3)=90.
      ta(2,1)=-90.
      ta(2,3)=90.
      ta(3,1)=-90.
c
c     combine the 4-fold with 3-fold      
      do 900 ka=1,3
      do 900 kb=1,4
        s(1)=(90*kb-180)+ta(1,ka)
        s(2)=ta(2,ka)
        s(3)=ta(3,ka)     
 
      pi1 = s(1)*pi/180. 
      cpi = s(2)*pi/180.
      pi2 = s(3)*pi/180.
c     
c     calculate 12 vector
      call euler2rot(pi1,cpi,pi2,tmr2)
          
      v(:)=0.d0
      vr(:)=0.d0
      
      do 700 k1=1,3
      do 700 k2=1,3
         v(k1)=v(k1)+tmr2(k1,k2)*dv(k2)
700   continue
c
c     normalization and projection
      call np(v,x1,y1)
       
      write (*,'(3f7.3)') (v(k1),k1=1,3)
      write (*,'(2f7.3)') x1,y1
      write(1,'(2f7.3)') x1,y1
c      
c     calculate other 12 vector converted by mirror plane      
      tmm(:,:)=0.d0
      tmm(1,1)=1.d0
      tmm(2,2)=-1.d0
      tmm(3,3)=1.d0
     
           
      do 800 k1=1,3
      do 800 k2=1,3
         vr(k1)=vr(k1)+tmm(k1,k2)*v(k2)
 800   continue 
c
c     normalization and projection
      call np(vr,x1,y1)
      
      write (*,'(3f7.3)') (vr(k1),k1=1,3)
      write (*,'(2f7.3)') x1,y1
      write(1,'(2f7.3)') x1,y1


 900  continue
 

      close(1)
      read(*,*)
          
      end program test
 
c======================================================================= 
c     Compose the rotation matrix with Euler angles a,b,c 
c=======================================================================    
      subroutine euler2rot(pi1,cpi,pi2,tmr2)
      real*8 pi1,cpi,pi2,tma(3,3),tmb(3,3),tmc(3,3),tmr1(3,3),tmr2(3,3)

      tma(:,:)=0.d0
      tma(1,1)=cos(pi1)
      tma(1,2)=sin(pi1)
      tma(2,1)=-sin(pi1)
      tma(2,2)=cos(pi1)
      tma(3,3)=1.d0
      
      tmb(:,:)=0.d0
      tmb(1,1)=1.d0
      tmb(2,2)=cos(cpi)
      tmb(2,3)=sin(cpi)
      tmb(3,2)=-sin(cpi)
      tmb(3,3)=cos(cpi)
      
      tmc(:,:)=0.d0
      tmc(1,1)=cos(pi2)
      tmc(1,2)=sin(pi2)
      tmc(2,1)=-sin(pi2)
      tmc(2,2)=cos(pi2)
      tmc(3,3)=1.d0
      
      tmr1(:,:)=0.d0
      tmr2(:,:)=0.d0       
            
      do 500 k1=1,3
      do 500 k2=1,3
      do 500 k3=1,3
         tmr1(k1,k2)=tmr1(k1,k2)+tma(k1,k3)*tmb(k3,k2)
 500   continue
      
      do 600 k1=1,3
      do 600 k2=1,3
      do 600 k3=1,3
         tmr2(k1,k2)=tmr2(k1,k2)+tmr1(k1,k3)*tmc(k3,k2)
 600   continue
      
      return
      end subroutine euler2rot
c
c
c======================================================================= 
c     Normalize and project the vector
c=======================================================================       
      subroutine np(v,x1,y1)
      real*8 norm,x,y,z,x1,y1,v(3)

c     normalization
      norm = 0.d0
      do k1=1,3
         norm = norm + v(k1)**2
      end do
      norm = dsqrt(norm)
      x=v(1)/norm
      y=v(2)/norm
      z=v(3)/norm
c     ---

c     projection      
      if (z.GT.0) then
        x1=x/(z+1)
        y1=y/(z+1)
      elseif (z.LT.0) then
        x1=x/(1-z)
        y1=y/(1-z)
      else
        x1=x
        y1=y
      end if
c     ---
      
      return
      end subroutine np
c      
c     