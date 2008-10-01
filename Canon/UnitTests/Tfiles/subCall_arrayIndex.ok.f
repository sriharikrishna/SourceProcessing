      subroutine foo(x,y)
        double precision :: x
        double precision :: y
        y = x * 2
      end

      subroutine head(x,y)
        double precision,dimension(2) :: x
        double precision :: y
        integer :: k
        double precision :: ad_ctmp0
        double precision :: ad_ctmp1
        double precision :: ad_ctmp2

        k = 1
        ad_ctmp0 = x(k)
        ad_ctmp1 = x(k + 1)
        call foo(ad_ctmp0,ad_ctmp1)
        ad_ctmp2 = x(k)
        call foo(ad_ctmp2,y)

      end
