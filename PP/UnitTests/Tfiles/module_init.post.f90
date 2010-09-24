MODULE globals
  use w2f_types
use OAD_active
  IMPLICIT NONE
  SAVE
  type(active) :: GX
  type(active) :: GY
contains
subroutine mod_globals_init()
use OAD_active
    type(active) :: GX
    type(active) :: GY
    GX%d = 0
    GY%d = 0
  end subroutine mod_globals_init
  
END MODULE globals
subroutine OAD_globalVar_init()
  use globals
  call mod_globals_init()
end subroutine OAD_globalVar_init

SUBROUTINE bar(BARX, BARY)
  use OAD_active
  IMPLICIT NONE
  REAL(w2f__8) BARX
  REAL(w2f__8) BARY
  call OAD_globalVar_init()
  
  GX%v = BARX*2
  GY%v = BARY*2
END SUBROUTINE bar

