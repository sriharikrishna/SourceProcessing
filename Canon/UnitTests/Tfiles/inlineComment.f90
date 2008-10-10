subroutine head(y)
  real y
  external foo
  y = foo(y) ! we better save this comment!
end subroutine head

