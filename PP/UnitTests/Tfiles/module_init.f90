MODULE globals
  use w2f_types
  IMPLICIT NONE
  SAVE
  
  TYPE (oadactive) GX
  TYPE (oadactive) GY
  
END MODULE globals

SUBROUTINE bar(BARX, BARY)
  use w2f__types
  IMPLICIT NONE
  
  REAL(w2f__8) BARX
  REAL(w2f__8) BARY
  
  GX%v = BARX*2
  GY%v = BARY*2
END SUBROUTINE bar
