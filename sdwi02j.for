        implicit real*8(a-h,o-z)
        WRITE(*,*) 'Give Xo'
        READ(*,*)Xo
        WRITE(*,*)'Give Tolerance'
        READ(*,*)TOL
10      X=Xo
        FUN=x**4+0.5*x**3-1.5*x**2-10
        DIFF=4.*x**3+1.5*x**2-3.*x
        X=Xo-FUN/DIFF
        IF(DABS(X-Xo).GT.TOL)THEN
        Xo=X
        WRITE(*,*)'X=',X
        GOTO 10
        ENDIF
        WRITE(*,*)'Final Solution=',X
        STOP
        END
