      real function bar(x)
        real :: x
        bar = x*2
        return
      end function bar
      subroutine oad_s_bar(x,bar)
        real :: x
        real,intent(out) :: bar
        bar = x*2
        return
      end subroutine oad_s_bar

