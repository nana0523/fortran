
      program test
      implicit none
      real a,b,c,pi
      integer k1,k2,k3
      real tma(3,3), tmb(3,3), tmc(3,3), tmr1(3,3), tmr2(3,3)
     

      write (*,*) 'Give Euler angles a,b,c:'
      read (*,*) a,b,c
 

 
      pi=3.141592
      write(*,'(f8.3)') pi
      read(*,*)
      
      a = a * pi/ 180.
      b = b * pi/ 180.
      c = c * pi / 180.
      
      tma(:,:) = 0.d0
      tma(1,1)=cos(a)
      tma(1,2)=sin(a)
      tma(2,1)=-sin(a)
      tma(2,2)=cos(a)
      tma(3,3)=1.d0
      
      tmb(:,:) = 0.d0
      tmb(1,1)=1.d0
      tmb(2,2)=cos(b)
      tmb(2,3)=sin(b)
      tmb(3,2)=-sin(b)
      tmb(3,3)=cos(b)
      
      
      tmc(:,:) = 0.d0
      tmc(1,1)=cos(c)
      tmc(1,2)=sin(c)
      tmc(2,1)=-sin(c)
      tmc(2,2)=cos(c)
      tmc(3,3)=1.d0

      
      do k1=1,3
        do k2=1,3
            do k3=1,3
                tmr1(k1,k2)=tmr1(k1,k2)+tma(k1,k3)*tmb(k3,k2)
            end do
         end do
      end do
      
      do k1=1,3
        do k2=1,3
            do k3=1,3
                tmr2(k1,k2)=tmr2(k1,k2)+tmr1(k1,k3)*tmc(k3,k2)
            end do
         end do
      end do
      
     
      do k1=1,3
            write (*,'(3f7.3)') (tmr2(k1,k2), k2=1,3)
      enddo
      read(*,*)
      do k1=1,3
            write (*,'(3f7.3)') tmr2(k1,:)
      end do
      
      

      read(*,*)
      
      end
      
