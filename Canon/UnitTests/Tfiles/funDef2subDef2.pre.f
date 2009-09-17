      REAL FUNCTION AVERAGE(X,Y,Z)
        REAL X,Y,Z,SUM
        SUM = X+Y+Z
        AVERAGE = SUM/3.0
        RETURN
      END FUNCTION AVERAGE
      subroutine oad_s_average(X,Y,Z,average)
        REAL X,Y,Z,SUM
        real,intent(out) :: average
        SUM = X+Y+Z
        AVERAGE = SUM/3.0
        RETURN
      end subroutine oad_s_average

