MODULE module_branche_epais
USE module_declarations
IMPLICIT NONE

CONTAINS

SUBROUTINE calc_F(Prad,Pgaz,third_term,u)

REAL(KIND=xp), INTENT(IN)  :: Prad
REAL(KIND=xp), INTENT(IN)  :: Pgaz
REAL(KIND=xp), INTENT(IN)  :: third_term
REAL(KIND=xp), INTENT(out) :: u

u=1.0_xp/(4.0_xp*3.0_xp**(3.0_xp/2.0_xp))*(Prad+Pgaz)+third_term

END SUBROUTINE calc_F

END MODULE module_branche_epais