subroutine bar(a)
   type baz
      integer :: f1
   end type

   type(baz),dimension(4) :: it
  real :: ad_ctmp0
  type(baz) :: ad_ctmp1

   it(1)%f1 = 3
   ad_ctmp1 = it(2)
   call ad_s_ff(ad_ctmp1,ad_ctmp0)
   it(1)%f1 = 6 + ad_ctmp0
end

