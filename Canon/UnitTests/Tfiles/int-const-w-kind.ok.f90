subroutine foo(a,b)
   real a,b
   integer(w2f__i4) NNZ
   integer ifoo
  integer(w2f__i8) :: oad_ctmp0
  integer(w2f__i8) :: oad_ctmp1

   oad_ctmp1 = 0_w2f__i8
   call oad_s_MAX_i(NNZ,oad_ctmp1,oad_ctmp0)
   ifoo = oad_ctmp0
end
