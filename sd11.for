C     PROGRAM FOR MATRIX ALGEBRA
C     PROGRAM MULTIPLI
      DIMENSION A(50,50),ASUM(50,50),ASUB(50,50),B(50,50),F(50,50)
      WRITE(*,*)  ' ORDER  ,INPUT IN MAT12 ; OUTPUT IN MAT3 '
      OPEN(5,FILE='MAT12',STATUS='OLD',ACCESS='SEQUENTIAL',
     +FORM='FORMATTED')
      OPEN(6,FILE='MAT3',STATUS='NEW',ACCESS='SEQUENTIAL',
     1FORM='FORMATTED')
      READ(5,*) N
      WRITE(*,*) 'GIVE MATRIX ELEMENTS OF A:NO FORMAT '
      READ(5,*) ((A(I,J),J=1,N),I=1,N)
      WRITE(*,*)'GIVE MATRIX ELEMENTS OF B:NO FORMAT'
      READ(5,*) ((B(I,J),J=1,N),I=1,N)
      DO 51 I=1,N
      DO 51 J=1,N
      ASUM(I,J)=A(I,J)+B(I,J)
51    ASUB(I,J)=A(I,J)-B(I,J)
      WRITE(6,*) ' SUM OF MATRICES '
      DO 461 I=1,N
461   WRITE(6,9) (ASUM(I,J),J=1,N)
      WRITE(6,*) 'DIFFERENCE OF MATRICES '
      DO 462 I=1,N
462   WRITE(6,9)  (ASUB(I,J),J=1,N)
47    WRITE(6,*) ' MULTIPLICATION RESULT'
      CALL MULT(A,B,F,N)
      DO 45 I=1,N
      DO 45 J=1,N       
45    B(I,J)=F(I,J)
      DO 46 I=1,N
46    WRITE(6,9) (B(I,J),J=1,N)
9     FORMAT(/,8F10.5,/)
      STOP
      END



C----------------------------------------------------------------------

      SUBROUTINE MULT(C,D,E,N)
      DIMENSION C(50,50),D(50,50),E(50,50)
      DO 6 I=1,N
      DO 6 J=1,N
6     E(I,J)=0.0
      DO 50 I=1,N
      DO 50 K=1,N
      DO 50 J=1,N
      E(I,K)=E(I,K)+C(I,J)*D(J,K)
50    CONTINUE
      RETURN
      END