      subroutine oad_s_average(X,Y,Z,average)
        real,intent(out) :: average
        REAL X,Y,Z,SUM
        SUM = X+Y+Z
        AVERAGE = SUM/3.0
        RETURN
      end subroutine oad_s_average
