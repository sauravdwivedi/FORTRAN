c       Gaussian Quadrature by Dwivedi, S.
        real r(10), x(10),y(10),z(10)
        write(*,*) 'No. of Points'
        read(*,*)n
        write(*,*)'A & B'
        read(*,*)a,b
        write(*,*)'R(i)'
        read(*,*)(r(i),i=1,n)
        write(*,*)'X(i)'
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
