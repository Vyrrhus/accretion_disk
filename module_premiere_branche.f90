module module_premiere_branche
use module_constantes
use module_dicho
implicit none

CONTAINS

real(kind=xp) function F_epais(T,rho,P_0,P_rad_0,P_GAZ_0,F_Z_DIFF_0,rho_0,T_0,S_0)

!Manque trinome pour H

F_epais=1/(4*3**(3/2))*(P_rad_0/P_0*T**4+P_GAZ_0/P_0*rho*T)-F_Z_DIFF_0*2*T**4/((Ke+Kff*rho*T**(-7/2)*rho_0*T_0**(-7/2))*S_0*rho**2*H)

end function F_epais


end module module_premiere_branche