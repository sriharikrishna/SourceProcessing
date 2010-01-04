      REAL FUNCTION AVERAGE(X,Y,Z)
      use OAD_intrinsics
        REAL X,Y,Z,SUM
        SUM = X+Y+Z
        AVERAGE = SUM/3.0
        return
      END FUNCTION AVERAGE
      subroutine oad_s_average(X,Y,Z,average)
      use OAD_intrinsics
        REAL X,Y,Z,SUM
        real,intent(out) :: average
        SUM = X+Y+Z
        AVERAGE = SUM/3.0
        return
      end subroutine oad_s_average
