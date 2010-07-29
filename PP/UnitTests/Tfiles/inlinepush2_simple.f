        SUBROUTINE OpenAD_MASKER(N,M,THCK)
        use w2f__types
        INTEGER(w2f__i4) N
        INTEGER(w2f__i4) M
        REAL(w2f__8) THCK(1 : N, 1 : M)
        INTEGER(w2f__i4) I=2
        INTEGER(w2f__i4) J=2
C       $OpenAD$ BEGIN REPLACEMENT 2
C           $OpenAD$ INLINE push_s2(subst)
            CALL push_s2(THCK(I +(-1) : I + 1, J +(-1) : J + 1))
C       $OpenAD$ END REPLACEMENT
        END SUBROUTINE
