C Progran to calculate area and volume of a unit radius sphere using Mo
C nte Carlo Method
C Area of unit circle is PI
C Volume of the unit sphere is 4*PI/3
C Thus area and volume can be used to determine PI
C Since rantom numbers generated are Positive in the range 0<=r<=1
C We shall be essentially determining 
C Area of the Positive Quadrant of a unit circle:
C x**2+y**2+z**2=1 with x,y and z positive: 
        Program PI
        Write(*,*)'How many random numbers to be generated ?'
        read(*,*) NRAND
C----   initialization of iseed needed for random number generator
        iseed=-1
        NHITS2=0
        NHITS3=0
        Do 10 in=1,NRAND
        x=random(iseed)
        y=random(iseed)
        z=random(iseed)
C testing wether R2 lies in the POSITIVE quadrant of a Unit circle.
        R2=x*x+y*y        
        R3=R2+z*z
C-----------------------------------
C testing whether R2 lies in the Positive quadrant of a Unit circle.
C If YES then increase nhits2 by 1
        IF(R2.le.1.) nhits2=nhits2+1
C-----------------------------------
C testing whether R3 lies in the Positive quadrant of a Unit spheres.
C If YES then increase nhits3 by 1
        IF(R3.le.1.) nhits3=nhits3+1
10      continue
        AN=NRAND
        pi3=float(nhits3)/AN
        pi2=float(nhits2)/AN
        write(*,*) nhits2,pi2,pi2*4.
        write(*,*) nhits3,pi3,pi3*6.
11      format(1x,'No. of hits for circle is', I6, ' hit ratio=',f9.6, 
     +  'PI=',f9.6)
12      format(1x,'No. of hits for sphere is', I6,'hit ratio=',f9.6,'  
     +  PI=',f9.6)
        stop
        end
