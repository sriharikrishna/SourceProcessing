      subroutine oad_s_fltrn(RES)
        real(double),intent(out) :: RES

        integer :: k
        integer,parameter :: r1 = 12211, m1 = 2147483563
        integer,parameter :: a1 = 40014, q1 = 53668
        integer,parameter :: r2 =  3791, m2 = 2147483399
        integer,parameter :: a2 = 40692, q2 = 52774
        k = s1/q1
        s1 = a1*(s1-k*q1)-k*r1
        if (s1.lt.0) then
          s1 = s1+m1
        endif
        k = s2/q2
        s2 = a2*(s2-k*q2)-k*r2
        if (s2.lt.0) then
          s2 = s2+m2
        endif
        RES = s1-s2
        if (RES.lt.1) then
          RES = RES+m1-1
        endif
        RES = RES/m1
      end subroutine oad_s_fltrn
