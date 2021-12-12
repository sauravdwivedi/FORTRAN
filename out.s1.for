c       Gaussian Quadrature by Dwivedi, S.
        real r(10), x(10),y(10),z(10)
        write(*,*) 'No. of Points'
        read(*,*)n
        write(*,*)'A & B'
        read(*,*)a,b
        write(*,*)'R'
        read(*,*)(r(i),i=1,n)
        write(*,*)'X'
        read(*,*)(x(i),i=1,n)
        p=(a+b)/2.
        q=(b-a)/2.
        s=0.
        do 5 i=1,n
        y(i)=p+q*x(i)
        call epp(y,z,n)
        s=s+r(i)*z(i)
 5      continue
        s=s*q
        write(*,*)'Integral='
        write(*,*)s
        u=1./sqrt(3.)
        t=f(-u)+f(u)
        e=abs(s-3*t)
        write(*,*) 'Error='
        write(*,3) e
 3      format(f10.8)
        stop
        end
        subroutine epp(y,z,n)
        Real y(10),z(10)
        do 6 i=1,n
        z(i)=2.*y(i)**3-3.*y(i)**2+4.*y(i)-5
6       continue
        return
        end
        FUNCTION F(Y)
        F=54.*Y**3+27.*Y**2+12.*Y-2.
        RETURN
        END

C:\students>s1
No. of Points
3
A & B
-2
4
R(i)
0.55555556
0.88888889
0.55555556
X(i)
-0.77459667
0.0
0.77459667
Integral=       42.0000100
Error=		.00001049
Stop - Program terminated.

C:\students>s1
No. of Points
6
A & B
-2
4
R(i)
0.17132449
0.36076157
0.46791394
0.46791394
0.36076157
0.17132449
X(i)
-0.93246951
-0.66120939
-0.23861919
0.23861919
0.66120939
0.93246951
Integral
       42.0000100
Error=
.00001049
Stop - Program terminated.


Verified by Mathematica

In[1]:= Integrate[{2 x^3 - 3 x^2 + 4 x - 5}, {x, -2, 4}]

Out[1]= {42}