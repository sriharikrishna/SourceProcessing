      module all_globals_mod
      use w2f__types
      use OAD_active
      implicit none
      SAVE
C
C     **** Statements ****
C
      END MODULE
      module globals
      use w2f__types
      use OAD_active
      implicit none
      SAVE
C
C     **** Global Variables & Derived Type Definitions ****
C
      type(active) :: AGLOBAL
       END MODULE
      subroutine head(X,Y)
      type(active) :: AGLOBAL
      type(active) :: X(1:2)
      type(active) :: Y(1:1)
      type(active) :: Z(1:)
      END SUBROUTINE
