      subroutine foo(v)
      use OAD_intrinsics
        real(w2f__4) v
        real(w2f__4) ifoo
        real(w2f__8) :: oad_ctmp0

        call oad_s_max(v,6e0_w2f__8,oad_ctmp0)
        ifoo = oad_ctmp0
      end
