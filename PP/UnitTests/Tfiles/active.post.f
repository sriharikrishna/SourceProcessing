      MODULE all_globals_mod
      use w2f__types
      use OAD_active
      IMPLICIT NONE
      SAVE
C
C     **** Statements ****
C
      END MODULE

      MODULE globals
      use w2f__types
      use OAD_active
      IMPLICIT NONE
      SAVE
C
C     **** Global Variables & Derived Type Definitions ****
C
      type(active) :: AGLOBAL

      END MODULE

      SUBROUTINE head(X, Y)
      type(active) :: AGLOBAL
      type(active) :: X(1:2)
      type(active) :: Y(1:1)
      END SUBROUTINE
