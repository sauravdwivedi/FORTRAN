C       NUMERICAL INTEGRATION by Dwivedi, S.
        character ans
        write(*,*)'Lower & Upper Limits'
        read(*,*)a,b
        write(*,*)'No. of Random Numbers to be Generated'
        read(*,*)n
        iseed=100
        write(*,*)'F(x)=sin(x); G(x)=5x^3-3x^2+6x+1'
        write(*,*)'Press "f" or "F" for F(x), any other key for G(x)'
        read(*,'(A)') ans
        Do 6 i=1,n
        rn=random(iseed)
        x=a+rn*(b-a)
        if(ans.eq.'f'.or.ans.eq.'F')then
        sum=sum+F(x)
	else
        sum=sum+G(x)
        endif
6       continue
        sum=sum*(b-a)/float(n)
        write(*,*)'Integral='
        write(*,9)sum
9       format(f12.8)
        stop
        end
        Function F(x)
        F=sin(x)
        return
        end
        Function G(x)
        G=5.*x*x*x-3.*x*x+6.*x+1.
        return
        end


C:\students>s4
Lower & Upper Limits
0.0
2.0
No. of Random Numbers to be Generated
1245893
F(x)=sin(x); G(x)=5x^3-3x^2+6x+1
Press "f" or "F" for F(x), any other key for G(x)
F
Integral=
 1.41766700
Stop - Program terminated.

C:\students>s4
Lower & Upper Limits
0.0
1.0
No. of Random Numbers to be Generated
21416546
F(x)=sin(x); G(x)=5x^3-3x^2+6x+1
Press "f" or "F" for F(x), any other key for G(x)
G
Integral=
 4.15355600
Stop - Program terminated.

Verification by Mathematica

In[19]:= NIntegrate[Sin[x], {x, 0, 2}]

Out[19]= 1.41615


In[8]:= NIntegrate[5 x^{3} - 3 x^{2} + 6 x + 1, {x, 0, 1}]

Out[8]= {4.25}