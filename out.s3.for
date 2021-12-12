C       PROGRAM FOR MODIFIED EULER METHOD by Dwivedi, S.
        write(*,*)'Xin'
        read(*,*)x1
        write(*,*)'Yin'
        read(*,*)y1
        write(*,*)'Xfin'
        read(*,*)xf
        write(*,*)'Step Size'
        read(*,*)h
        n=ifix((xf-x1)/h+.5)
        write(*,*)'    "X="    ','      "Ycp="','        "Yth="','
     1  "Error"'
        do 6 i=1,n
        s1=f(x1,y1)
        x2=x1+h
        y2=y1+h*s1
        s2=f(x2,y2)
        y2=y1+h*(s1+s2)/2
        x1=x2
        y1=y2
        write(*,7)x1,y1,z(x1),abs(z(x1)-y1)
6       continue
7       format(4f14.8)
        stop
        end
        function f(x,y)
        f=-x*y
        return
        end
        function z(x)
        z=exp(-x*x/2.)
        return
        end


C:\students>s3
Xin
0
Yin
1
Xfin
.25
Step Size
.05
    "X="          "Ycp="        "Yth="        "Error"
    .05000000     .99875000     .99875080     .00000083
    .10000000     .99501090     .99501250     .00000155
    .15000000     .98881080     .98881300     .00000226
    .20000000     .98019580     .98019870     .00000292
    .25000000     .96922980     .96923320     .00000340
Stop - Program terminated.


Verification by Mathematica

In[1]:= DSolve[{y'[x] + x*y[x] == 0, y[0] == 1}, y[x], x]

Out[1]= {{y[x] -> E^(-(x^2/2))}}

In[2]:= Value[y[x] = E^(-(x^2/2)),x = {0, 0.05, 0.10, 0.15, 0.20, 0.25}]

Out[2]= Value[{1, 0.998751, 0.995012, 0.988813, 0.980199, 0.969233}, {0, 0.05, 0.1, 0.15, 0.2, 0.25}]
