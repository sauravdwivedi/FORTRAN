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

C:\students>s2
X & degree N
3.0
3
f(Xi)
22.0
17.8
14.2
38.3
Xi
3.2
2.7
1.0
4.8
f(x=3.0000000):=20.2119600
Stop - Program terminated.

Verification by Mathematica:

In[1]:= InterpolatingPolynomial[{{3.2, 22.0}, {2.7, 17.8}, {1.0, 14.2}, {4.8, 38.3}}, 3.0]

Out[1]= 20.212