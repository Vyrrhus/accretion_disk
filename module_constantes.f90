module constantes

use INPUT
real(kind=xp),parameter :: k_b = 1.380649e-23_xp                   ! CONSTANTE DE BOLTZMANN
real(kind=xp),parameter :: pi = 3.1415926535897932384626433_xp
real(kind=xp),parameter :: c = 2.99792458e8_xp 			   !VITESSE LUMIERE
real(kind=xp),parameter :: m_p = 1.67262192369e-27_xp !MASSE PROTON
real(kind=xp),parameter :: G = 6.6743e-11_xp				 !CONSTANTE GRAVITE
real(kind=xp),parameter :: sigma_stefan = 5.670374419e-8_xp 		!CONSTANTE DE STEFAN-BOLTZMANN
real(kind=xp),parameter :: a_radiation = 7.56573085e-16 		!CONSTANTE DE RADIATION
real(kind=xp),parameter :: M_o = 1.989e30_xp 				! MASSE DU SOLEIL
real(kind=xp),parameter :: gamma_g = 5.0_xp/3.0_xp 			!INDICE POLYTROPIQUE

end module constantes
