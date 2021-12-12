C     PROGRAM FOR DIFFERENTIAL COEFFICIENT by Dwivedi, S.
      DIMENSION X(100),DF(100,100),A(100),B(100),C(100)
      WRITE (*,*) 'Y'
      READ (*,*) Y
      WRITE(*,*) 'NUMBER OF POINTS (MAXIMUM 6)'
      READ(*,*) N
      DO 1 I=1,N
      WRITE (*,*) 'ENTER DATA POINTS'
    1 READ (*,*) X(I),DF(1,I)
      H=X(2)-X(1)
      DO 2 M=1,N
      DO 3 I=1,N-M+1
    3 DF(M+1,I)=DF(M,I+1)-DF(M,I)
    2 CONTINUE
      WRITE (*,*) 'THE DIFFERENCE TABLE IS'
      WRITE (*,*)
      DO 4 M=1,N
    4 WRITE (*,*) (DF(I,M),I=1,N-M+1)
      DO 7 M=1,N-1
      C(M)=0.0
      DO 6 J=1,M
      B(J)=1.0
      DO 5 K=1,M
      IF (K.NE.J) THEN
      B(J)=B(J)*(Y-X(K))
      ENDIF
    5 CONTINUE
      C(M)=C(M)+B(J)
    6 CONTINUE
      YF=YF+C(M)*DF(M+1,1)/(F(M)*H**M)
    7 CONTINUE
      WRITE(*,*) 'THE DIFFERENTIAL COEFFCIENT IS:',YF
      STOP
      END
      FUNCTION F(N)
      J=1
      DO 8 I=1,N
    8 J=J*I
      F=J
      RETURN
      END