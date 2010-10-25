MODULE globals
  use OAD_active
  use w2f_types
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
end subroutine
  
END MODULE globals
subroutine OAD_globalVar_init()
	use globals
	call mod_globals_init()
end subroutine

SUBROUTINE bar(BARX, BARY)
  use OAD_active
  use w2f__types
  IMPLICIT NONE
  REAL(w2f__8) BARX
  REAL(w2f__8) BARY
	call OAD_globalVar_init()
  GX%v = BARX*2
  GY%v = BARY*2
end subroutine
