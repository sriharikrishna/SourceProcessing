
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

      SUBROUTINE head(X, Y)
      use w2f__types
c use active module (OAD_active)
      use OAD_active
      IMPLICIT NONE
C
C     **** Parameters and Result ****
C
      type(active) :: X(1 : 2)
      type(active) :: Y(1 : 2)
C
C     **** Local Variables and Functions ****
C
      INTEGER(w2f__i4) N
      SAVE N
      type(active) :: Z(1 : INT(SIZE((X)%v)))
C
C     **** Initializers ****
C
      DATA N / 2 /
C
C     **** Top Level Pragmas ****
C
C$OPENAD INDEPENDENT(X)
C$OPENAD DEPENDENT(Y)
C
C     **** Statements ****
C
C$OPENAD XXX Template ad_template.f
      Z(1 : INT(SIZE(X)))%v = X(1 : 2)%v
      OpenAD_Symbol_5 = SIN(X(3)%v)
      OpenAD_Symbol_4 = COS(X(3)%v)
      call setderiv(Z(1 : INT(SIZE(X)))%d,X(1 : 2)%d)
      Y(1 : 2)%v = Z(1 : INT(SIZE(X)))%v
      call setderiv(Y(1 : 2)%d,Z(1 : INT(SIZE(X)))%d)
      END SUBROUTINE
