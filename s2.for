C     LAGRANGE INTERPOLATION by Dwivedi, S.
      real f(10), y(10)
      write(*,*)'X & degree N'
      read(*,*)x,n
      n=n+1
      write(*,*)'f(Xi)'
      read(*,*)(f(i),i=1,n)
      write(*,*)'Xi'
      read(*,*)(y(i),i=1,n)
      s=0
      do  6 i=1,n
      pf=1
      do 5 j=1,n
      if(i.ne.j)pf=pf*(x-y(j))/(y(i)-y(j))
5     continue
      s=s+f(i)*pf
6     continue
10    write(*,*)'f(x=',x,'):=',s
      stop
      end

