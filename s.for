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

