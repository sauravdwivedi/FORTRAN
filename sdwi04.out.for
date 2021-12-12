        REAL SX, SY, SXY, SXX, SYY
        CHARACTER ANSR
        N=0
        SX=0
        SY=0
        SXY=0
        SXX=0
        SYY=0
11      WRITE(*,*)'INPUT X AND Y'
        READ(*,*)X,Y
        N=N+1
        SX=SX+X
        SY=SY+Y
        SXY=SXY+X*Y
        SXX=SXX+X*X
        SYY=SYY+Y*Y
        WRITE(*,*)'NO. OF DATA PAIRS',N
        WRITE(*,*)'MORE DATA? Y FOR YES, OTHER KEY FOR NO'
        READ(*,'(A)')ANSR
        IF(ANSR.EQ.'Y'.OR.ANSR.EQ.'y')GO TO 11
        AGX=SX/N
        AGY=SY/N
        DM=N*SXX-SX*SX
        A=(SXX*SY-SX*SXY)/DM
        B=(N*SXY-SX*SY)/DM
        DM=SQRT((SXX-N*AGX**2)*(SYY-N*AGY**2))
        CFF=(SXY-N*AGX*AGY)/DM
        WRITE(*,*)
        WRITE(*,*)'A AND B OF Y=A+BX:'
        WRITE(*,'(2X,2(A,F10.4))')'A=',A,'B=',B
        WRITE(*,*)'CORRELATION COEFF.=',CFF
        STOP
        END



C:\students>sdwi04
INPUT X AND Y
1
3
NO. OF DATA PAIRS            1
MORE DATA? Y FOR YES, OTHER KEY FOR NO
y
INPUT X AND Y
5
6
NO. OF DATA PAIRS            2
MORE DATA? Y FOR YES, OTHER KEY FOR NO
y
INPUT X AND Y
1.3
9.3
NO. OF DATA PAIRS            3
MORE DATA? Y FOR YES, OTHER KEY FOR NO
y
INPUT X AND Y
71.3
92.6
NO. OF DATA PAIRS            4
MORE DATA? Y FOR YES, OTHER KEY FOR NO
y
INPUT X AND Y
6.5
4.2
NO. OF DATA PAIRS            5
MORE DATA? Y FOR YES, OTHER KEY FOR NO
8.6

A AND B OF Y=A+BX:
 A=    1.3581B=    1.2727
CORRELATION COEFF.=  9.940249E-001
Stop - Program terminated.
