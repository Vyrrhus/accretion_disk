MODULE module_branche_epais
USE module_declarations
USE module_fonctions_utiles
USE module_dicho
IMPLICIT NONE

CONTAINS

real(kind=xp) function F_epais(P_rad_ad,Pgaz,third_term_epais)

F_epais=1.0_xp/(4.0_xp*3.0_xp**(3.0_xp/2.0_xp))*(Prad+Pgaz)+third_term_epais
end function F_epais

end module module_branche_epais