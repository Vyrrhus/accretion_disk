!===================================================================================================
            MODULE MODULE_DECLARATIONS
!===================================================================================================
!> Ce module contient :
!> - la précision utilisée pour faire les calculs (15 pour double precision)
!> - les constantes mathématiques ou physiques
!> - les paramètres d'entrée
!> - les constantes du modèle
!>
!> Pour toutes les grandeurs physiques on utilise les unités SI.
!>
!> Les subroutines du module permettent de lire les paramètres d'entrée et calculer les constantes
!> du modèle.
!===================================================================================================

IMPLICIT NONE

!! PRECISION
integer,parameter :: xp =  selected_real_kind(15) !! Précision des calculs

!! CONSTANTES PHYSIQUES 
real(kind=xp),parameter :: k_b             = 1.380649e-23_xp                   !! Constante de Boltzmann
real(kind=xp),parameter :: pi              = 3.1415926535897932384626433_xp    !! \(\pi\)
real(kind=xp),parameter :: c_speed         = 2.99792458e8_xp                   !! Vitesse de la lumière
real(kind=xp),parameter :: m_p             = 1.67262192369e-27_xp              !! Masse du proton
real(kind=xp),parameter :: G               = 6.6743e-11_xp                     !! Constante de gravitation
real(kind=xp),parameter :: sigma_stefan    = 5.670374419e-8_xp                 !! Constante de Stefan-Boltzmann
real(kind=xp),parameter :: a_radiation     = 7.56573085e-16_xp                 !! Constante de radiation
real(kind=xp),parameter :: M_o             = 1.989e30_xp                       !! Masse du Soleil
real(kind=xp),parameter :: gamma_g         = 1.6666666666666666667_xp          !! Indice polytropique
real(kind=xp), parameter :: sigma_e        = 6.6524587321e-29_xp               !! Section efficace de Thomson

!! PARAMETRES MODELE
real(kind=xp)  :: M                  !! Masse trou noir de Schwarzschild
real(kind=xp)  :: M_0_DOT            !! Taux d'accrétion du milieu ext. sur le disque
real(kind=xp)  :: f_accretion        !! Rapport entre le taux d'accrétion et le taux d'accrétion critique
real(kind=xp)  :: r_max              !! Rayon maximal du disque
real(kind=xp)  :: ALPHA              !! Paramètre phénoménologique (taux de production d'énergie par friction de la matière)
real(kind=xp)  :: X_FRAC             !! Abondance H
real(kind=xp)  :: Y_FRAC             !! Abondance He
integer        :: NX                 !! Nombre de points de discrétisation spatiale

!! CONSTANTES DU SYSTEME
real(kind=xp)  :: Z_FRAC        !! Abondance éléments lourds
real(kind=xp)  :: mu            !! Masse atomique moyenne \(\mu\)
real(kind=xp)  :: R             !! BOLTZMANN OVER PROTON_MASS / LA CONSTANTE DE COME
real(kind=xp)  :: r_s           !! Rayon de Schwarzschild
real(kind=xp)  :: r_min         !! Rayon minimal du disque
real(kind=xp)  :: omega_max     !! Vitesse angulaire maximale (au rayon minimal)
real(kind=xp)  :: L_tot         !! Luminosité maximale
real(kind=xp)  :: L_Edd         !! Luminosité d'Eddington
real(kind=xp)  :: M_crit_dot    !! Taux d'acrétion critique

!! CONSTANTES DE NORMALISATION
real(kind=xp)  :: v_0           !! Vitesse d'accrétion
real(kind=xp)  :: nu_0          !! Viscosité
real(kind=xp)  :: Sigma_0       !! Densité de surface
real(kind=xp)  :: S_0           !! \(S_0 = \Sigma_0\)
real(kind=xp)  :: rho_0         !! Densité volumique
real(kind=xp)  :: T_0           !! Température
real(kind=xp)  :: P_0           !! Pression totale
real(kind=xp)  :: P_rad_0       !! Pression de radiation
real(kind=xp)  :: P_gaz_0       !! Pression gazeuse
real(kind=xp)  :: C_v_0         !! Capacité calorifique
real(kind=xp)  :: F_Z_DIFF_0    !! Flux (diffusif) lorsque la profondeur optique \(\tau_{eff} > 1\)
real(kind=xp)  :: F_Z_RAD_0     !! Flux (radiatif) lorsque \(\tau_{eff} < 1\)
real(kind=xp)  :: KAPPA_E       !! Opacité diffusion Thomson
real(kind=xp)  :: Q_PLUS_0      !! Terme de chauffage
real(kind=xp)  :: Q_ADV_0       !! Chaleur advectée
real(kind=xp)  :: B_0           !! Coefficient b du trinôme pour le calcul de H
real(kind=xp)  :: C_0           !! Coefficient c du trinôme pour le calcul de H

!! VARIABLES ADIMENSIONNÉES
real(kind=xp) :: T_AD            !! Temps adimensionné
real(kind=xp) :: X_AD            !! Rayon adimensionné
real(kind=xp) :: OMEGA_AD        !! Vitesse de rotation adimensionnée
real(kind=xp) :: P_AD            !! Pression totale adimensionnée
real(kind=xp) :: P_GAZ_AD        !! Pression gazeuse adimensionnée
real(kind=xp) :: P_RAD_AD        !! Pression de radiation adimensionnée
real(kind=xp) :: BETA            !! Indicateur de pression
real(kind=xp) :: C_S_AD          !! Vitesse de son adimensionnée
real(kind=xp) :: H_AD            !! Demi-hauteur du disque adimensionnée
real(kind=xp) :: RHO_AD          !! Densité volumique
real(kind=xp) :: NU_AD           !! Viscosité
real(kind=xp) :: S_AD            !! Densité de surface
real(kind=xp) :: V_AD            !! Vitese d'accrétion
real(kind=xp) :: M_DOT_AD        !! Taux d'accrétion
real(kind=xp) :: TEMP_AD         !! Température
real(kind=xp) :: Q_PLUS_AD       !! Chaleur apportée
real(kind=xp) :: Q_ADV_AD        !! Chaleur advectée
real(kind=xp) :: C_V_AD          !! Capacité calorifique

!! VARIABLES AVEC CONDITIONS
real(kind=xp) :: Q_MOINS_AD  !! Chaleur dissipée adimensionnée
real(kind=xp) :: F_Z_AD      !! Flux adimensionnée
real(kind=xp) :: TAU_EFF     !! Profondeur optique effective
real(kind=xp) :: KAPPA_FF    !! Opacité free-free
real(kind=xp) :: EPSILON_FF  !! Emissivité free-free


!===================================================================================================
            CONTAINS 
!===================================================================================================

SUBROUTINE APPEL_PARAM_INPUT()
!---------------------------------------------------------------------------------------------------
!>    Cette routine lit les paramètres d'entrée du fichier input.txt et attribue les valeurs associées 
!>    à M, M_0_DOT, R_MAX, ALPHA, X et Y.
!>    On utilise des valeurs par défaut en cas d'erreur.
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE
    integer :: file_id
    namelist /input/ M,f_accretion,r_max,ALPHA,X_FRAC,Y_FRAC,NX

    ! VALEURS PAR DEFAUT DES PARAMETRES D'ENTREE
    M             = 3._xp        ! [M_{sol}]
    f_accretion   = 0.001_xp
    r_max         = 100._xp    ! [r_{Schwarzschild}]
    ALPHA         = 1._xp
    X_FRAC        = 0.7_xp
    Y_FRAC        = 0.28_xp
    NX            = 100

    ! LECTURE DES PARAMETRES DEPUIS LE FICHIER
    OPEN(newunit=file_id,file='input.txt',action='read',status='old')
    READ(file_id,input)
    CLOSE(file_id)

    ! AFFICHAGE PARAMETRES D'ENTREE
    PRINT*,"-----------------INITIALISATION-----------------"
    PRINT*,"------------VALEURS PARAMETRES EN INPUT---------"
    WRITE(*,"(49('-'))")
    
    PRINT*,"MASSE DU TROU NOIR [MASSE SOLAIRE]             = ",M
    PRINT*,"RAPPORT TAUX D'ACCRETION / TAUX CRITIQUE       = ",f_accretion  
    PRINT*,"R_MAX [RAYON DE SCHWARZSCHILD]                 = ",r_max
    PRINT*,"ALPHA                                          = ",ALPHA
    PRINT*,"X                                              = ",X_FRAC
    PRINT*,"Y                                              = ",Y_FRAC
    PRINT*,"NB POINTS DE DISCRETISATION SPATIALE           = ",NX

    ! CONVERSION UNITES SI
    M = M * M_o
    
!---------------------------------------------------------------------------------------------------
END SUBROUTINE APPEL_PARAM_INPUT
!---------------------------------------------------------------------------------------------------

SUBROUTINE CALCUL_CONSTANTES()
!---------------------------------------------------------------------------------------------------
!>    Cette routine calcule l'ensemble des constantes du modèle et les constantes d'adimensionnement. 
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE

    ! TAUX D'ACCRETION
    L_Edd       = 4._xp * pi * G * M * m_p * C_SPEED / sigma_e
    M_crit_dot  = 12._xp * L_Edd / C_SPEED**2._xp
    M_0_DOT     = f_accretion * M_crit_dot

    ! CONSTANTES DU SYSTEME
    Z_FRAC     = 1.0_XP - X_FRAC - Y_FRAC
    mu         = 1.0_xp / (2.0_xp * X_FRAC + 3.0_xp/4.0_xp * Y_FRAC + Z_FRAC/2.0_xp)
    R          = k_b / m_p 
    r_s        = 2.0_xp * G * M / C_SPEED**2.0_xp
    r_min      = 3.0_xp * r_s
    r_max      = r_max * r_s
    omega_max  = (G * M / r_min**3.0_xp)**(0.5_xp) 
    L_tot      = M_0_dot * C_SPEED**2.0_xp / 12.0_xp 

    ! CONSTANTES DE NORMALISATION
    v_0         = r_s * omega_max
    nu_0        = 4.0_xp/3.0_xp * r_s**2.0_xp * omega_max
    Sigma_0     = M_0_dot / (omega_max * 2.0_xp * pi * r_s**2.0_xp)
    S_0         = Sigma_0
    rho_0       = Sigma_0 / (2._xp * r_s)
    T_0         = (L_tot / (9.0_xp * 4.0_xp * pi * r_s**2.0_xp * sigma_stefan))**(0.25_xp)
    P_0         = M_0_dot * omega_max / (4.0_xp * pi * r_s)
    P_rad_0     = a_radiation * T_0**4.0_xp / 3.0_xp
    P_gaz_0     = R * rho_0 * T_0 / mu
    C_v_0       = R / mu
    F_Z_DIFF_0  = 2.0_xp * a_radiation * C_SPEED * T_0**4.0_xp / (3.0_xp * S_0)
    F_Z_RAD_0   = r_s * rho_0**2._xp * T_0**(0.5_xp) * 6.22e13_xp
    KAPPA_E     = 0.02_xp * (1.0_xp + X_FRAC) 
    Q_PLUS_0    = 3.0_xp * r_s**2.0_xp * omega_max**3.0_xp
    Q_ADV_0     = omega_max * T_0 * R / mu
    B_0         = 2.0_xp * a_radiation * T_0**4.0_xp / (3.0_xp * r_s * S_0 * omega_max**2.0_xp)
    C_0         = R * T_0 / (r_s**2.0_xp / mu / omega_max**2.0_xp)

    WRITE(*,"(49('-'))")
    PRINT*,'------------CONSTANTES DE SIMULATION------------'
    WRITE(*,"(49('-'))")
    
    PRINT*,"MASSE DU TROU NOIR                           M = ",M
    PRINT*,"TAUX D'ACCRETION                       M_0_DOT = ",M_0_DOT
    PRINT*,"FRACTION D'ELEMENTS LOURDS                   Z = ",Z_FRAC
    PRINT*,"POID MOLECULAIRE MOYEN                      MU = ",MU
    PRINT*,"RAYONS DE SCHWARZSCHILD                    R_S = ",R_S
    PRINT*,"RAYON MIN DISQUE ACCRETION               R_MIN = ",R_MIN
    PRINT*,"RAYON MAXIMAL DU DISQUE                  R_MAX = ",R_MAX
    PRINT*,"VITESSE ROTATION MAX                 OMEGA_MAX = ",OMEGA_MAX
    PRINT*,"LUMINOSITE TOTALE                        L_TOT = ",L_TOT
    
    WRITE(*,"(49('-'))")
    
    PRINT*,"CONSTANTE DE VITESSE                       V_0 = ",V_0
    PRINT*,"CONSTANTE DE VISCOSITE                    NU_0 = ",NU_0
    PRINT*,"CONSTANTE DE DENSITE                     RHO_0 = ",RHO_0
    PRINT*,"CONSTANTE DE TEMPERATURE                   T_0 = ",T_0
    PRINT*,"CONSTANTE DE DENSITE SURFACIQUE        SIGMA_0 = ",SIGMA_0
    PRINT*,"CONSTANTE DE DENSITE                       S_0 = ",S_0
    PRINT*,"CONSTANTE DE PRESSION                      P_0 = ",P_0
    PRINT*,"CONSTANTE DE PRESSON RAD               P_RAD_0 = ",P_RAD_0
    PRINT*,"CONSTANTE DE PRESSION GAZ              P_GAZ_0 = ",P_GAZ_0
    PRINT*,"CONSTANTE DE CAPACITE CALORIFIQUE        C_V_0 = ",C_V_0
    PRINT*,"CONSTANTE DE FLUX DIFFUSIF          F_Z_DIFF_0 = ",F_Z_DIFF_0
    PRINT*,"CONSTANTE DE FLUX RADIATIF           F_Z_RAD_0 = ",F_Z_RAD_0
    PRINT*,"CONSTANTE OPACITÉ DE THOMPSON          KAPPA_E = ",KAPPA_E
    PRINT*,"CONSTANTE DE CHALEURE APPORTÉE        Q_PLUS_0 = ",Q_PLUS_0
    PRINT*,"CONSTANTE DE CHALEURE ADVECTÉE         Q_ADV_0 = ",Q_ADV_0
    PRINT*,"COEFFICIENT B DU POLYNOME DE H             B_0 = ",B_0
    PRINT*,"COEFFICIENT C DU POLYNOME DE H             C_0 = ",C_0  
    
    WRITE(*,"(49('-'))")

!---------------------------------------------------------------------------------------------------
END SUBROUTINE CALCUL_CONSTANTES
!---------------------------------------------------------------------------------------------------

!===================================================================================================
END MODULE MODULE_DECLARATIONS
!===================================================================================================

