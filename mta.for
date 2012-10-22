#

      program test
      implicit none
      real a11,a12,a13,a21,a22,a23,a31,a32,a33,a,b,c,pi
      real tm(3,3)
     

      write (*,*) 'Give 9 componets of matrix:'
      read (*,*) a11,a12,a13,a21,a22,a23,a31,a32,a33
 
      pi=3.141592
      

      
      tm(1,1)=a11
      tm(1,2)=a12
      tm(1,3)=a13
      tm(2,1)=a21
      tm(2,2)=a22
      tm(2,3)=a23
      tm(3,1)=a31
      tm(3,2)=a32
      tm(3,3)=a33
      
      b=dacos(a33)
      a=dacos(a23/dsin(b))
      c=dacos(a32/-dsin(b))
      
      if (dsin(b).EQ.0) then
        c=0
        a=dacos(a11)
      endif
      
      a = a *180./pi
      b = b *180./pi
      c = c *180./pi
        
      
      write (*,*) 'Euler angle a,b,c:', a,b,c
      
      read(*,*)

      end