        WRITE(*,*) 'Give Xo'
        READ(*,*)Xo
10      X=Xo
        FUN=X**2+SIN(X)-1.
        DIFF=2.*X+COS(X)
        X=Xo-FUN/DIFF
        IF(ABS(X-Xo).GT.0.00001)THEN
        Xo=X
        WRITE(*,1)X
        GOTO 10
        ENDIF
        WRITE(*,2)X
1       FORMAT(5X, 'X=', F15.8)
2       FORMAT(/,5X,'FINAL SOLUTION=', F15.8)
        STOP
        END
