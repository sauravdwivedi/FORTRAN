C     EULER'S METHOD OF SOLVING ODE
      OPEN (8,FILE='EULEROUT.TXT',STATUS='NEW')
      WRITE(*,*) 'GIVE THE INITIAL VALUE OF X'
      READ(*,*) XSTART
      WRITE(*,*) 'GIVE THE END VALUE OF X'
      READ(*,*) XEND
      WRITE(*,*) 'GIVE THE STEP SIZE FOR X'
      READ(*,*) XSTEP
      WRITE(*,*) 'GIVE THE INITIAL VALUE OF Y'
      READ(*,*) YINIT
      WRITE(*,*)
      WRITE(*,1)XSTART,XEND,XSTEP
      WRITE(8,1)XSTART,XEND,XSTEP
 1    FORMAT('START,END AND STEP VALUES ARE ',3F10.5)
      WRITE(*,2) YINIT
      WRITE(8,2) YINIT
 2    FORMAT('THE INITIAL VALUE OF Y IS',F10.5)
      WRITE(*,*)'   N   XVALUE    CALC_Y   THEOR_Y   ERROR_Y'
      WRITE(8,*)'   N   XVALUE    CALC_Y   THEOR_Y   ERROR_Y'
      NTOT=INT((XEND-XSTART)/XSTEP+0.5)+1
      CALL EULER(XSTART,YINIT,XSTEP,NTOT)
      CLOSE(8)
      STOP
      END
C
      FUNCTION F(X,Y)
      F=X+Y
      END
C
      FUNCTION YTHE(X)
C     THE ANALYTICAL SOLUTION OF ODE
      YTHE=2.0*EXP(X)-X-1.0
      RETURN
      END
C
      SUBROUTINE EULER(X1,Y1,H,NTOT)
      XBEG=X1
      YBEG=Y1
      N=1
 2    YTHBEG=YTHE(XBEG)
      WRITE(*,3) N,XBEG,YBEG,YTHBEG,YBEG-YTHBEG
      WRITE(8,3) N,XBEG,YBEG,YTHBEG,YBEG-YTHBEG
 3    FORMAT(2X,I3,4F10.5)
      SLOPE1=F(XBEG,YBEG)
      YEND=YBEG+H*SLOPE1
      XEND=XBEG+H
      N=N+1
      XBEG=XEND
      YBEG=YEND
      IF(N.LE.NTOT) GO TO 2
      RETURN
      END