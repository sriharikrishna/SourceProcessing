      subroutine foo(x,y)
        double precision x
        double precision y
        y = x * 2
      end subroutine

      subroutine head(x,y)
        double precision, dimension(2) :: x
        double precision y
        integer k

        k = 1
        call foo(x(k),x(k + 1))
        call foo(x(k),y)

      end subroutine
