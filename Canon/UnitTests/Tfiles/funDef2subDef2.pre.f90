      subroutine oad_s_average(X,Y,Z,RES)
        REAL X,Y,Z,SUM
        real,intent(out) :: RES
        SUM = X + Y + Z
        RES = SUM/3.0
        RETURN
      end subroutine oad_s_average
