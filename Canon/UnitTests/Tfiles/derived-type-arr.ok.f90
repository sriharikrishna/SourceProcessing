subroutine bar(a)
   type baz
      integer :: f1
   end type

   type(baz), dimension(4) :: it
   real :: ad_ctmp0

   it(1)%f1 = 3
   call ad_s_ff(it(2),ad_ctmp0)
   it(1)%fl = 6 + ad_ctmp0
end

