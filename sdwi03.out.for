        F(X)=EXP(X)
        WRITE(*,3)
1       READ(*,*)A,B,N2
        N=N2/2
        IF(N*2.NE.N2)GO TO 99
        H=(B-A)/FLOAT(N2)
        S=F(A)+4.*F(A+H)
        N1=N-1
        DO 2 I=1,N1
2       S=2.*F(A+H*FLOAT(2*I))+4.*F(A+H*FLOAT(2*I+1))+S
        S=H*(F(B)+S)/3.
        WRITE(*,5)A,B,N2,S
        GO TO 1
3       FORMAT(2X,'SIMPSONS ONE THIRD RULE')
4       FORMAT(2X,2F5.2,I3)
5       FORMAT(2X,2F5.2,2X,I3,E15.8)
99      STOP
        END



C:\students>sdwi03
 SIMPSONS ONE THIRD RULE
1
5
10
  1.00 5.00   10  .14571520E+03
1
9
12
  1.00 9.00   12  .81088090E+04
2
8
16
  2.00 8.00   16  .29738900E+04
1
3
15
Stop - Program terminated.

C:\students>