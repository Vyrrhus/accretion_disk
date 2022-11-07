MODULE MODULE_DECLARATIONS

IMPLICIT NONE

integer,parameter :: XP =  selected_real_kind(15)

! CONSTANTES PHYSIQUES 

real(kind=xp),parameter :: k_b = 1.380649e-23_xp                             ! CONSTANTE DE BOLTZMANN
real(kind=xp),parameter :: pi = 3.1415926535897932384626433_xp               ! PI
real(kind=xp),parameter :: c = 2.99792458e8_xp 			             ! VITESSE LUMIERE
real(kind=xp),parameter :: m_p = 1.67262192369e-27_xp                        ! MASSE PROTON
real(kind=xp),parameter :: G = 6.6743e-11_xp				     ! CONSTANTE GRAVITE
real(kind=xp),parameter :: sigma_stefan = 5.670374419e-8_xp 		     ! CONSTANTE DE STEFAN-BOLTZMANN
real(kind=xp),parameter :: a_radiation = 7.56573085e-16 		     ! CONSTANTE DE RADIATION
real(kind=xp),parameter :: M_o = 1.989e30_xp 				     ! MASSE DU SOLEIL
real(kind=xp),parameter :: gamma_g = 1.6666666666666666667_xp                ! INDICE POLYTROPIQUE

! DECLARATION DES VARIABLES EN INPUT

REAL(KIND=XP)  :: M
REAL(KIND=XP)  :: M_0_DOT
REAL(KIND=XP)  :: R_MAX_DECLARED
REAL(KIND=XP)  :: ALPHA
REAL(KIND=XP)  :: X
REAL(KIND=XP)  :: Y

! DECLARATION DES CONSTANTES DU SYSTEME

REAL(KIND=XP)  :: Z                                                 ! FRACTION DE METALLICITÉ
real(kind=xp)  :: mu                                                ! POID MOLECULAIRE MOYEN
REAL(KIND=XP)  :: M_BH                                              ! MASSE DU TROU NOIR
real(kind=xp)  :: R                                                 ! BOLTZMANN OVER PROTON_MASS  / LA CONSTANTE DE COME
real(kind=xp)  :: r_s                                               ! RAYON DE SCHWARZSCHILD
REAL(KIND=XP)  :: R_MAX                                             ! RAYON MAXIMAL
real(kind=xp)  :: r_min                                             ! RAYON MINIMALE DU DISQUE D'ACCRETION
real(kind=xp)  :: omega_max                                         ! VITESSE ROTATION MAX
real(kind=xp)  :: L_tot                                             ! LUMINOSITE MAXIMALE

real(kind=xp)  :: v_0                                               ! CONSTANTE DE VITESSE 
real(kind=xp)  :: nu_0                                              ! CONSTANTE DE VISCOSITE 
real(kind=xp)  :: rho_0                                             ! CONSTANTE DE DENSITE
real(kind=xp)  :: T_0                                               ! CONSTANTE DE TEMPERATURE 
real(kind=xp)  :: S_0 
real(kind=xp)  :: sigma_0

real(kind=xp)  :: P_0                                               ! CONSTANTE DE PRESSION 
real(kind=xp)  :: P_rad_0                                           ! CONSTANTE DE PRESSION RAD
real(kind=xp)  :: P_gaz_0                                           ! CONSTANTE DE PRESSION GAZ
real(kind=xp)  :: C_v_0                                             ! CONSTANTE DE CAPACITE CALORIFIQUE
real(kind=xp)  :: F_Z_DIFF_0 
REAL(KIND=XP)  :: F_Z_RAD_0 
REAL(KIND=XP)  :: Q_PLUS_0 
REAL(KIND=XP)  :: Q_ADV_0 

REAL(KIND=XP)  :: B_0
REAL(KIND=XP)  :: C_0 

CONTAINS 

!-------------------------------------------------------------------------------------------

SUBROUTINE APPEL_PARAM_INPUT()
IMPLICIT NONE
integer :: unt
	namelist /input/ M,M_0_DOT,R_MAX_DECLARED,ALPHA,X,Y
	open(newunit=unt,file='input.txt',action='read',status='old')
	read(unt,input)
	close(unt)

END SUBROUTINE APPEL_PARAM_INPUT

!-------------------------------------------------------------------------------------------

SUBROUTINE CALCUL_CONSTANTES()
	IMPLICIT NONE

        Z = 1.0_XP - X - Y
	mu = 1.0_xp/(2.0_xp*X+3.0_xp/4.0_xp*Y+Z/2.0_xp)
	M_BH = M * M_O
	R = k_b / m_p 
	r_s = 2.0_xp * G * M_BH / c**2.0_xp
	R_MAX = R_S * R_MAX_DECLARED
	r_min = 3.0_xp*r_s
	omega_max = (G*M_BH/r_min**3.0_xp)**(1.0_xp/2.0_xp) 
	L_tot = M_0_dot * c**2.0_xp / 12.0_xp 

	v_0 = r_s * omega_max
	nu_0 = 4.0_xp/3.0_xp * r_s**2.0_xp * omega_max 
	rho_0 = M_0_dot / 4.0_xp / pi / omega_max / r_s**3.0_xp
	T_0 = (L_tot/9.0_xp/4.0_xp/pi/r_s**2.0_xp/sigma_stefan)**(1.0_xp/4.0_xp)
	S_0 = M_0_dot/omega_max/2.0_xp/pi/r_s**2.0_xp 
	sigma_0 = M_0_dot/omega_max/2.0_xp/pi/r_s**2.0_xp

	P_0 = M_0_dot * omega_max / 4.0_xp / pi / r_s
	P_rad_0 = a_radiation * T_0**4.0_xp / 3.0_xp
	P_gaz_0 = R * rho_0 * T_0 / mu
	 C_v_0 = R / mu
	F_Z_DIFF_0 = 2.0_xp*A_RADIATION*C*T_0**4.0_XP / 3.0_XP / S_0
	F_Z_RAD_0 = R_S * RHO_0 * T_0**(1.0_XP/2.0_XP) * 6.22E19_XP
	Q_PLUS_0 = 3.0_XP*R_S**2.0_XP * OMEGA_MAX**3.0_XP
	Q_ADV_0 = OMEGA_MAX * T_0 * R / MU


	B_0 = 2.0_XP*A_RADIATION*T_0**4.0_XP/3.0_XP/R_S/OMEGA_MAX**2.0_xp/S_0
        C_0 = R*T_0 / R_S**2.0_XP / MU / OMEGA_MAX**2.0_XP

END SUBROUTINE CALCUL_CONSTANTES

!------------------------------------------------------------------------------------------------

SUBROUTINE AFFICHAGE()

	IMPLICIT NONE
	
	PRINT*,'----------INITIALISATION---------'
	PRINT*,'---VALEURS PARAMETRES EN INPUT---'
	PRINT*,'MASSE DU TROU NOIR EN MASSE SOLAIRES = ',M
	PRINT*,'CONSTANTE DE TAUX D ACCRETION        = ',M_0_DOT  
	PRINT*,'R_MAX DECLARE                        = ',R_MAX_DECLARED
	PRINT*,'ALPHA                                = ',ALPHA
	PRINT*,'X                                    = ',X
	PRINT*,'Y                                    = ',Y
	
	PRINT*,'--------------------------------'
	PRINT*,'----CONSTANTES DE SIMULATION----'
	PRINT*,'MASSE DU TROU NOIR                   = ',M_BH
	PRINT*,'RAYON MAXIMAL DU DISQUE        R_MAX = ',R_MAX
	PRINT*,'POID MOLECULAIRE MOYEN           MU  = ',MU
	PRINT*,'RAYONS DE SCHWARZSCHILD          R_S = ',R_S
	PRINT*,'RAYON MIN DISQUE ACCRETION     R_MIN = ',R_MIN
	PRINT*,'VITESSE ROTATION MAX       OMEGA_MAX = ',OMEGA_MAX
	PRINT*,'LUMINOSITE TOTALE              L_TOT = ',L_TOT
	PRINT*,'CONSTANTE DE VITESSE             V_0 = ',V_0
	PRINT*,'CONSTANTE DE VISCOSITE          NU_0 = ',NU_0
	PRINT*,'CONSTANTE DE DENSITE           RHO_0 = ',RHO_0
	PRINT*,'CONSTANTE DE TEMPERATURE         T_0 = ',T_0
	PRINT*,'CONSTANTE DE DENSITE SURFACIQUE SIGMA_0 = ', SIGMA_0
	PRINT*,'CONSTANTE DE PRESSION            P_0 = ',P_0
	PRINT*,'CONSTANTE DE PRESSON RAD     P_RAD_0 = ',P_RAD_0
	PRINT*,'CONSTANTE DE PRESSION GAZ    P_GAZ_0 = ',P_GAZ_0
	PRINT*,'CONSTANTE DE CAPACITE CALORIFIQUE C_V_0 = ',C_V_0

END SUBROUTINE AFFICHAGE
	
END MODULE MODULE_DECLARATIONS
