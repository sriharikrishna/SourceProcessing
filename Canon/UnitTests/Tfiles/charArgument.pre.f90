      subroutine foo(x)
        use OAD_intrinsics
        character(*) x
        print *,x
      end subroutine
      program p
        use OAD_intrinsics
        call foo("bar")
      end program p
