module constantes

integer,parameter :: xp =  selected_real_kind(15)

real(kind=xp),parameter :: k_b = 1.380649e-23_xp                   ! CONSTANTE DE BOLTZMANN
real(kind=xp),parameter :: pi = 3.1415926535897932384626433_xp
real(kind=xp),parameter :: c = 2.99792458e8_xp 			   !VITESSE LUMIERE
real(kind=xp),parameter :: m_p = 1.672 621 923 69e-27_xp !MASSE PROTON
real(kind=xp),parameter :: G = 6.6743e-11_xp				 !CONSTANTE GRAVITE
real(kind=xp),parameter :: sigma_stefan = 5.670374419e-8_xp 		!CONSTANTE DE STEFAN-BOLTZMANN
real(kind=xp),parameter :: a_radiation = 7.56573085e-16 		!CONSTANTE DE RADIATION
real(kind=xp),parameter :: M_o = 1.989e30_xp 				! MASSE DU SOLEIL
real(kind=xp),parameter :: gamma_g = 5.0_xp/3.0_xp 			!INDICE POLYTROPIQUE

end module constantes

module declarations 
use input
use constantes

real(kind=xp),parameter :: mu = 1.0_xp/(2.0_xp*X+3.0_x/4.0_xp*Y+Z/2.0_xp)  !POID MOLECULAIRE MOYEN

real(kind=xp),parameter :: R = k_b / m_p 
real(kind=xp),parameter :: r_s = 2.0_xp * G * M / c**2.0_xp !RAYON DE SCHWARZSCHILD
real(kind=xp),parameter :: r_min = 3.0_xp*r_s !RAYON MINIMALE DU DISQUE D'ACCRETION
real(kind=xp),parameter :: omega_max = (G*M/r_min**3.0_xp)**(1.0_xp/2.0_xp) ! VITESSE ROTATION MAX
real(kind=xp),parameter :: L_tot = M_0_dot * c**2.0_xp / 12.0_xp !LUMINOSITE MAXIMALE

real(kind=xp),parameter :: v_0 = r_s * omega_max !CONSTANTE DE VITESSE 
real(kind=xp),parameter :: nu_0 = 4.0_xp/3.0_xp r_s**2.0_xp * omega_max !CONSTANTE DE VISCOSITE 
real(kind=xp),parameter :: rho_0 = M_0_dot / 4.0_xp / pi / omega_max / r_s**3.0_xp !CONSTANTE DE DENSITE
real(kind=xp),parameter :: T_0 = (L_tot/9.0_xp/4.0_xp/pi/r_s**2/sigma_stefan)**(1.0_xp/4.0_xp) !CONSTANTE DE TEMPERATURE 
real(kind=xp),parameter :: S_0 = M_0_dot/omega_max/2.0_xp/pi/r_s**2.0_xp 
real(kind=xp),parameter :: sigma_0 = M_0_dot/omega_max/2.0_xp/pi/r_s**2.0_xp

real(kind=xp),parameter :: P_0 = M_0_dot * omega_max / 4.0_xp / pi / r_s !CONSTANTE DE PRESSION 
real(kind=xp),parameter :: P_rad_0 = a_radiation * T_0**4.0_xp / 3.0_xp !CONSTANTE DE PRESSION RAD
real(kind=xp),parameter :: P_gaz_0 = R * rho_0 * T_0 / mu  !CONSTANTE DE PRESSION GAZ
real(kind=xp),parameter :: C_v_0 = R / mu ! CONSTANTE DE CAPACITE CALORIFIQUE
real(kind=xp),parameter :: F_Z_DIFF_0 = 2.0_xp*A_RADIATION*C*T_0**4.0_XP / 3.0_XP / S_0
REAL(KIND=XP),PARAMETER :: F_Z_RAD_0 = R_S * RHO_0 * T_0**(1.0_XP/2.0_XP) * 6.22E19_XP
REAL(KIND=XP),PARAMETER :: Q_PLUS_0 = 3.0_XP*R_S**2.0_XP * OMEGA_MAX**3.0_XP
REAL(KIND=XP),PARAMETER :: Q_ADV_0 = OMEGA_MAX * T_0 * R / MU

REAL(KIND=XP),PARAMETER :: B_0 = 2.0_XP*A_RADIATION*T_0**4.0_XP/3.0_XP/R_S/OMEGA_MAX**2/S_0
REAL(KIND=XP),PARAMETER :: C_0 = R*T_0 / R_S**2.0_XP / MU / OMEGA_MAX**2.0_XP

end module declarations
