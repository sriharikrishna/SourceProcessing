      program p
        real :: u,v,w,x,y,z
        real :: oad_ctmp0
        real :: oad_ctmp1
        real :: oad_ctmp2
        real :: oad_ctmp3
        real :: oad_ctmp4
        call oad_s_max_r(x,y,oad_ctmp4)
        call oad_s_max_r(w,oad_ctmp4,oad_ctmp3)
        call oad_s_max_r(v,oad_ctmp3,oad_ctmp2)
        call oad_s_max_r(u,oad_ctmp2,oad_ctmp1)
        z = oad_ctmp1
      end program p
