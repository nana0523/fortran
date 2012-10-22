
      program test
      implicit none
      real a,b,c,pi,tma(3,3), tmb(3,3), tmc(3,3), tmr1(3,3), tmr2(3,3)
      integer k1,k2,k3
     
      write (*,*) 'Give Euler angles a,b,c:'
      read (*,*) a,b,c
 
      pi = 4.d0 * datan(1.d0)
      
      a = a * pi / 180.
      b = b * pi / 180.
      c = c * pi / 180.
      
      tma(:,:)=0.d0
      tma(1,1)=dcos(a)
      tma(1,2)=dsin(a)
      tma(2,1)=-dsin(a)
      tma(2,2)=dcos(a)
      tma(3,3)=1.d0
      
      tmb(:,:)=0.d0
      tmb(1,1)=1.d0
      tmb(2,2)=dcos(b)
      tmb(2,3)=dsin(b)
      tmb(3,2)=-dsin(b)
      tmb(3,3)=dcos(b)
      
      tmc(:,:)=0.d0
      tmc(1,1)=dcos(c)
      tmc(1,2)=dsin(c)
      tmc(2,1)=-dsin(c)
      tmc(2,2)=dcos(c)
      tmc(3,3)=1.d0

      do 10 k1=1,3
      do 10 k2=1,3
      do 10 k3=1,3
         tmr1(k1,k2)=tmr1(k1,k2)+tma(k1,k3)*tmb(k3,k2)
 10   continue
      
      do 20 k1=1,3
      do 20 k2=1,3
      do 20 k3=1,3
         tmr2(k1,k2)=tmr2(k1,k2)+tmr1(k1,k3)*tmc(k3,k2)
 20   continue
      
      do k1=1,3
         write (*,'(3f7.3)') (tmr2(k1,k2), k2=1,3)
      enddo
c$$$      read(*,*)
c$$$      do k1=1,3
c$$$            write (*,'(3f7.3)') tmr2(k1,:)
c$$$      end do
      end program test
      
