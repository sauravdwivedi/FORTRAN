c **  --------------------------------------------------------------------
c **  Runge kutta method forsolving fode (First-order differential eqution)
c **  To find Y(X) as a function of X where Y satisfies FODE :see next line
c **  dy/dx=F(X,Y) where F(X,Y) is to be defined by user and Y(XSTART)=YINIT
c **  Input to program is XSTART,XEND,XSTEP,YINIT :see below
c **  XSTART   : Starting point of range of x where solution is desired
c **  XEND     : End point of range of x
c **  XSTEP    : step size (also called h)
c **  note     : XSTEP should be negative when XEND < XSTART otherwise positive
c **  YINIT    : Initial value of Y at X=XSTART
c **  In case the solution is not known ,output for other  Y will be all 0
c **  We output the result on a file as well so that you may print the result
c **  ------------------------------------------------------------------
c **  In the main program input parameters are read
c **  And subroutine runge is called for calculations & printing
c **  ------------------------------------------------------------------
      PROGRAM MAIN
c **  We are creating a filed named RK.DOC for writing the rsult
      Common YINIT,XSTART
      character   *12 fname
      write (*,'(A)') 'output file -name please as   "name.extention"'
      write (*,'(A)')'name andextention at most 8 and 3 characters long'
      write (*,'(A)') 'For example  :  RK.DOC result.txt'
      write (*,*)
      write (*,*)  'type file name please >'
      read (*,'(A)') fname
      write (*,*) fname
      open  (7,FILE=fname,status='new')
      write  (*,*) 'results for runge-kutta method'
      write  (7,*) 'F(X,Y)= '
      write  (*,*) 'YTHE(X)= '
      write  (*,*)
      WRITE  (7,*)
      write  (*,*) 'give start value of X'
      read   (*,*) XSTART
      write  (*,*) 'give end value of X  '
      read   (*,*) XEND
      write  (*,*) 'give step size for X '
      READ   (*,*) XSTEP
      write  (*,*) 'give start value of Y'
      read   (*,*) YINIT
      write  (*,*)
      write  (*,*)
      write  (*,1) XSTART,XEND,XSTEP
      write  (7,*) XSTART,XEND,XSTEP
 1     format('START,END AND step values are',3F12.5)
      write  (*,*)
      write  (7,*)
      write  (*,2) YINIT
      write  (7,2) YINIT
 2     format('value of Y at start is    ',F12.5)
      write  (*,*)
      write  (7,*)
      write  (*,*) 'N    X VALUE      CA1C_Y       THEOR_Y
     1        ERROR_Y'
      write  (7,*) 'N    X VALUE      CA1C_Y       THEOR_Y
     1        ERROR_Y'
      NTOT=INT((XEND-XSTART)/XSTEP+0.5)+1
      CALL RUNGE (XSTART,YINIT,XSTEP,NTOT)
      CLOSE (7)
      STOP
      END
c
      FUNCTION F(X,Y)
      F=-10.*SIN(X)/Y
      RETURN
      END
c
      FUNCTION YTHE(X)
      COMMON YINIT,XSTART
      YTHE=SQRT(YINIT*YINIT+20.*(cos(X)-cos(XSTART)))
      return
      end
c
      subroutine runge(X1,Y1,H,NTOT)
      xbeg=X1
      ybeg=Y1
      N=1
   2  ythbeg=ythe(xbeg)
      write  (*,3) n,xbeg,ybeg,ythbeg,ybeg-ythbeg
      write  (7,*) n,xbeg,ybeg,ythbeg,ybeg-ythbeg
   3  format(2x,I6,4F15.5)
      slope1=F(XBEG,YBEG)
      Y2=ybeg+h*slope1/2.0
      xmid=xbeg+h/2.0
      slope2=F(xmid,Y2)
      Y3=Ybeg+h*slope2/2.0
      slope3=F(xmid,Y3)
      Y4=ybeg+h*slope3
      xful=xbeg+h
      slope4=F(xful,Y4)
      slopeav=(slope1+2.0*slope2+2.0*slope3+slope4)/6.0
      yful=ybeg+h*slopeav
      n=n+1
      xbeg=xful
      ybeg=yful
      if(n.le.ntot) goto 2
      return
      end