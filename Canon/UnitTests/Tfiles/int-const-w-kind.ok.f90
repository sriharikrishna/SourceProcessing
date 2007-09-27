subroutine foo(a,b)
   real a,b
   integer(w2f__i4) NNZ
   integer ifoo

   call ad_s_MAX_i(NNZ,0_w2f__i8,ad_ctmp0)
   ifoo = ad_ctmp0
end
