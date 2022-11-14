MODULE module_branche_epais
USE module_constantes
USE module_dicho
USE module_fonctions_utiles
IMPLICIT NONE

CONTAINS

real(kind=xp) function F_epais(P_rad_ad,Pgaz,third_term)

F_epais=1.0_xp/(4.0_xp*3.0_xp**(3.0_xp/2.0_xp))*(Prad+Pgaz)-F_Z_DIFF_0*2.0_xp*T**4.0_xp/((KAPPA_E+KAPPA_FF*rho*T**(-7.0_xp/2.0_xp)*rho_0*T_0**(-7.0_xp/2.0_xp))*S_0*rho**2.0_xp*H)

end function F_epais

end module module_branche_epais