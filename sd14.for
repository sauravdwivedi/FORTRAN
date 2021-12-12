      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(10,10),X(10,10),XX(10,10),G(10,10),F(10,10)
      EPS=1.0E-15
 12   WRITE(*,*) 'GIVE ORDER OF MATRIX:99 TO STOP'
      READ (*,*) N
      IF(N.EQ.99) GOTO 14
      WRITE(*,*) 'GIVE THE MATRIX'
      READ(*,*) ((A(I,J),J=1,N),I=1,N)
 4      DO 1 I=1,N
 1      WRITE(*,2) (A(I,J),J=1,N)
 2      FORMAT(/,8F10.5)
        WRITE(*,*) 'GIVE II,JJ:BOTH EQUAL------>READ NEXT CASE'
        READ(*,*) II,JJ
        IF(II.EQ.JJ) GOTO 11
        THE T=0.5*DATAN(2.0*A(II,JJ)/((A(II,II)-A(JJ,JJ))+EPS))
        C=DCOS(THET)
        S=DSIN(THET)
        WRITE(*,*)'C=',C, 'S=',S
        PAUSE
        DO 8 I=1,N
        DO 8 J=1,N
        IF(I.EQ.J) X(I,J)=1.0
        IF(I.EQ.J) XX(I,J)=1.0
        IF(I.NE.J) XX(I,J)=0.0
 8      IF(I.NE.J) X(I,J)=0.0
        DO 9 I=1,N
        DO 9 J=1,N
        IF(I.EQ.II. AND. J.EQ.II) X(I,I)=C
        IF(I.EQ.JJ. AND. J.EQ.JJ) X(J,J)=C
        IF(I.EQ.II. AND. J.EQ.II) XX(I,I)=C
        IF(I.EQ.JJ. AND. J.EQ.JJ) XX(J,J)=C
        IF(I.EQ.II. AND. J.EQ.JJ) X(I,J)=-S
        IF(I.EQ.JJ. AND. J.EQ.II) X(I,J)=S
        IF(I.EQ.II. AND. J.EQ.JJ) XX(I,J)=S
        IF(I.EQ.JJ. AND. J.EQ.II) XX(I,J)=-S
 9    CONTINUE
      WRITE (*,*) ' X MATRIX BELOW: DEFN. (XX) (A) (X)=> TOWARDS D'
      DO 10 I=1,N
 10   WRITE (*,2) (X(I,J),J=1,N)
      WRITE (*,*)'XX MATRIX'
      DO 15 I=1,N
 15   WRITE(*,2) (XX(I,J),J=1,N)
      CALL MULT(XX,A,G,N)
      WRITE (*,*)'G MATRIX: XX*A'
      DO 6 I=1,N
 6    WRITE(*,2) (G(I,J),J=1,N)
      PAUSE
      CALL MULT(G,X,F,N)
      WRITE(*,*)'F MATRIX :G*X'
      DO 7 I=1,N
 7    WRITE(*,2) (F(I,J),J=1,N)
      PAUSE
      DO 3 I=1,N
      DO 3 J=1,N
 3    A(I,J)=F(I,J)
      WRITE(*,*)'TRANSFORMED MATRIX'
      GO TO 4
 11   GO TO 12
 14   STOP
      END

      SUBROUTINE MULT(C,D,E,N)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION C(10,10),D(10,10),E(10,10)
      DO 6 I=1,N
      DO 6 J=1,N
 6    E(I,J)=0.0
      DO 50 I=1,N
      DO 50 K=1,N
      DO 50 J=1,N
      E(I,K)=E(I,K)+C(I,J)*D(J,K)
 50   CONTINUE
      RETURN
      END