
      MODULE all_globals_mod
      use w2f__types
c use active module (OAD_active)
      use OAD_active
      IMPLICIT NONE
      SAVE
C
C     **** Statements ****
C
      END MODULE

      MODULE globals
      use w2f__types
c use active module (OAD_active)
      use OAD_active
      IMPLICIT NONE
      SAVE
C
C     **** Global Variables & Derived Type Definitions ****
C
      type(active) :: AGLOBAL
C
C     **** Statements ****
C
      END MODULE

      SUBROUTINE head(X, Y)
      use w2f__types
c use active module (OAD_active)
      use OAD_active
      use globals
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      REAL(w2f__8) OpenAD_Symbol_0
      REAL(w2f__8) OpenAD_Symbol_1
C
C     **** Parameters and Result ****
C
      type(active) :: X(1 : 2)
      type(active) :: Y(1 : 1)
C
C     **** Top Level Pragmas ****
C
C$OPENAD INDEPENDENT(X)
C$OPENAD DEPENDENT(Y)
C
C     **** Statements ****
C
C$OPENAD XXX Template ad_template.f
      call sax(OpenAD_Symbol_0,X(1)%d,AGLOBAL%d)
      call saxpy(OpenAD_Symbol_1,X(2)%d,AGLOBAL%d)
      call sax(OpenAD_Symbol_0,X(1)%d,Y(1)%d)
      call saxpy(OpenAD_Symbol_1,X(2)%d,Y(1)%d)
      END SUBROUTINE
