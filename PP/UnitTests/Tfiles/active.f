      MODULE all_globals_mod
      use w2f__types
      IMPLICIT NONE
      SAVE
C
C     **** Statements ****
C
      END MODULE

      MODULE globals
      use w2f__types
      IMPLICIT NONE
      SAVE
C
C     **** Global Variables & Derived Type Definitions ****
C
      type(active) :: AGLOBAL

      END MODULE

      SUBROUTINE head(X, Y)
      type (OpenADTy_active) AGLOBAL
      type (OpenADTy_active) :: X(1 : 2)
      type (OpenADTy_active) :: Y(1 : 1)
      END SUBROUTINE
