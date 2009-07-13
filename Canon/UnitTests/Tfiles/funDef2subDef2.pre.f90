      subroutine oad_s_average(X,Y,Z,RES)
        real,intent(out) :: RES
        REAL X,Y,Z,SUM
        SUM = X+Y+Z
        RES = SUM/3.0
        return
      end subroutine oad_s_average
