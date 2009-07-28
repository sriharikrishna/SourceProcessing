    program functionComment
      real :: x(2)

      x(1) = 1.5D0
      x(2) = -3.6D0
      print *,f(x(1)) + f(x(2))
    end program functionComment
    subroutine oad_s_f(x,f)
!     blah blah
!     aren't these names interesting?: pomentale and gargantini
      real,intent(out) :: f
      real :: x
       if (x<=0) then
        f = 4.0D0
      elseif (x<=39) then
        f = exp(x*(1.0D0/(x+2)))
      else
        f = 0
      end if
    end subroutine oad_s_f

