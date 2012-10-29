      program test
      implicit none
      real a,b,c,pi,p,q,r,tma(3,3), tmb(3,3), tmc(3,3), tmr1(3,3),
     +tmr2(3,3), v(3), dv(3), s(3), vr(3), tmm(3,3)
      integer k,ka,kb,k1,k2,k3
     
      write (*,*) 'Give vector (p,q,r):'

      read (*,*) p,q,r
      
      s(:)=0.d0

      dv(1)=p
      dv(2)=q
      dv(3)=r
         
      do 900 kb=0,4
         s(1)=(kb*90)-180
         s(2)=-90
         s(3)=-90  
         if (kb.GT.0) then
            goto 300
         end if
         
      do 900 ka=0,4
         s(1)=(ka*90)-180+90
         s(2)=90
         s(3)=0
         if (ka.GT.0) then
            goto 300
         end if
         
      do 900 k=1,4
         s(1)=(k*90)-180
         s(2)=0
         s(3)=0    
 
300   pi = 4.d0 * datan(1.d0)
      
      a = s(1) * pi / 180.
      b = s(2) * pi / 180.
      c = s(3) * pi / 180.
      
      tma(:,:)=0.d0
      tma(1,1)=cos(a)
      tma(1,2)=sin(a)
      tma(2,1)=-sin(a)
      tma(2,2)=cos(a)
      tma(3,3)=1.d0
      
      tmb(:,:)=0.d0
      tmb(1,1)=1.d0
      tmb(2,2)=cos(b)
      tmb(2,3)=sin(b)
      tmb(3,2)=-sin(b)
      tmb(3,3)=cos(b)
      
      tmc(:,:)=0.d0
      tmc(1,1)=cos(c)
      tmc(1,2)=sin(c)
      tmc(2,1)=-sin(c)
      tmc(2,2)=cos(c)
      tmc(3,3)=1.d0
      
      tmr1(:,:)=0.d0
      tmr2(:,:)=0.d0 
      v(:)=0.d0
      vr(:)=0.d0
      
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
 
      do 700 k1=1,3
      do 700 k2=1,3
         v(k1)=v(k1)+tmr2(k1,k2)*dv(k2)
 700   continue
 
 
      tmm(:,:)=0.d0
      tmm(1,1)=1.d0
      tmm(2,2)=1.d0
      tmm(3,3)=-1.d0
      
      
      do 800 k1=1,3
      do 800 k2=1,3
         vr(k1)=vr(k1)+tmm(k1,k2)*v(k2)
 800   continue     
      
      
      write (*,'(3f7.3)') (v(k1),k1=1,3)
      write (*,'(3f7.3)') (vr(k1),k1=1,3)
     


 900  continue
      
 
 
      read(*,*)
      
           
      end program test
      
