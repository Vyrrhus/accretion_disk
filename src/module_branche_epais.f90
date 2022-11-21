MODULE module_branche_epais
IMPLICIT NONE

CONTAINS

REAL(KIND=xp) FUNCTION F_epais(Prad,Pgaz,third_term_epais) RESULT(u)

u=1.0_xp/(4.0_xp*3.0_xp**(3.0_xp/2.0_xp))*(Prad+Pgaz)+third_term_epais
END FUNCTION F_epais

END MODULE module_branche_epais