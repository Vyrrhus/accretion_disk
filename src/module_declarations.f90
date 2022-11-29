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
INTEGER,PARAMETER :: XP =  SELECTED_REAL_KIND(15) !! Précision des calculs

!! CONSTANTES PHYSIQUES 
REAL(KIND=XP),PARAMETER :: K_B             = 1.380649E-23_XP                   !! Constante de Boltzmann
REAL(KIND=XP),PARAMETER :: PI              = 3.1415926535897932384626433_XP    !! \(\pi\)
REAL(KIND=XP),PARAMETER :: C_SPEED         = 2.99792458E8_XP                   !! Vitesse de la lumière
REAL(KIND=XP),PARAMETER :: M_P             = 1.67262192369E-27_XP              !! Masse du proton
REAL(KIND=XP),PARAMETER :: G               = 6.6743E-11_XP                     !! Constante de gravitation
REAL(KIND=XP),PARAMETER :: SIGMA_STEFAN    = 5.670374419E-8_XP                 !! Constante de Stefan-Boltzmann
REAL(KIND=XP),PARAMETER :: A_RADIATION     = 7.56573085E-16_XP                 !! Constante de radiation
REAL(KIND=XP),PARAMETER :: M_O             = 1.989e30_xp                       !! Masse du Soleil
REAL(KIND=XP),PARAMETER :: GAMMA_G         = 1.6666666666666666667_XP          !! Indice polytropique
REAL(KIND=XP),PARAMETER :: SIGMA_E         = 6.6524587321E-29_XP               !! Section efficace de Thomson

!! PARAMETRES MODELE
REAL(KIND=XP)  :: M                  !! Masse trou noir de Schwarzschild
REAL(KIND=XP)  :: M_0_DOT            !! Taux d'accrétion du milieu ext. sur le disque
REAL(KIND=XP)  :: f_accretion        !! Rapport entre le taux d'accrétion et le taux d'accrétion critique
REAL(KIND=XP)  :: r_max              !! Rayon maximal du disque
REAL(KIND=XP)  :: ALPHA              !! Paramètre phénoménologique (taux de production d'énergie par friction de la matière)
REAL(KIND=XP)  :: X_FRAC             !! Abondance H
REAL(KIND=XP)  :: Y_FRAC             !! Abondance He

!! PARAMÈTRES SPATIAUX
INTEGER,PARAMETER :: NX = 501        !! Nombre de points de discrétisation spatiale
REAL(KIND=XP)     :: DX              !! Pas de discrétisation spatiale
REAL(KIND=XP)     :: X_MIN           !! Rayon minimal adimensionné
REAL(KIND=XP)     :: X_MAX           !! Rayon maximal adimensionné

!! PARAMÈTRES TEMPORELS
INTEGER        :: NT
REAL(KIND=XP)  :: DT
REAL(KIND=XP)  :: T_MIN
REAL(KIND=XP)  :: T_MAX  

!! CONSTANTES DU SYSTEME
REAL(KIND=XP)  :: Z_FRAC        !! Abondance éléments lourds
REAL(KIND=XP)  :: MU            !! Masse atomique moyenne \(\mu\)
REAL(KIND=XP)  :: R             !! Boltzmann over proton_mass / La constante de Come
REAL(KIND=XP)  :: R_S           !! Rayon de Schwarzschild
REAL(KIND=XP)  :: R_MIN         !! Rayon minimal du disque
REAL(KIND=XP)  :: OMEGA_MAX     !! Vitesse angulaire maximale (au rayon minimal)
REAL(KIND=XP)  :: L_TOT         !! Luminosité maximale
REAL(KIND=XP)  :: L_EDD         !! Luminosité d'Eddington
REAL(KIND=XP)  :: M_CRIT_DOT    !! Taux d'acrétion critique

!! CONSTANTES DE NORMALISATION
REAL(KIND=XP)  :: V_0           !! Vitesse d'accrétion
REAL(KIND=XP)  :: NU_0          !! Viscosité
REAL(KIND=XP)  :: SIGMA_0       !! Densité de surface
REAL(KIND=XP)  :: S_0           !! \(S_0 = \Sigma_0\)
REAL(KIND=XP)  :: RHO_0         !! Densité volumique
REAL(KIND=XP)  :: T_0           !! Température
REAL(KIND=XP)  :: P_0           !! Pression totale
REAL(KIND=XP)  :: P_RAD_0       !! Pression de radiation
REAL(KIND=XP)  :: P_GAZ_0       !! Pression gazeuse
REAL(KIND=XP)  :: C_V_0         !! Capacité calorifique
REAL(KIND=XP)  :: F_Z_DIFF_0    !! Flux (diffusif) lorsque la profondeur optique \(\tau_{eff} > 1\)
REAL(KIND=XP)  :: F_Z_RAD_0     !! Flux (radiatif) lorsque \(\tau_{eff} < 1\)
REAL(KIND=XP)  :: KAPPA_E       !! Opacité diffusion Thomson
REAL(KIND=XP)  :: Q_PLUS_0      !! Terme de chauffage
REAL(KIND=XP)  :: Q_ADV_0       !! Chaleur advectée
REAL(KIND=XP)  :: B_0           !! Coefficient b du trinôme pour le calcul de H
REAL(KIND=XP)  :: C_0           !! Coefficient c du trinôme pour le calcul de H

!! VARIABLES ADIMENSIONNÉES
REAL(KIND=XP) :: T_AD(NX)            !! Temps Adimensionné
REAL(KIND=XP) :: X_AD(NX)            !! Rayon Adimensionné
REAL(KIND=XP) :: OMEGA_AD(NX)        !! Vitesse De Rotation Adimensionnée
REAL(KIND=XP) :: P_AD(NX)            !! Pression totale adimensionnée
REAL(KIND=XP) :: P_GAZ_AD(NX)        !! Pression gazeuse adimensionnée
REAL(KIND=XP) :: P_RAD_AD(NX)        !! Pression de radiation adimensionnée
REAL(KIND=XP) :: BETA(NX)            !! Indicateur De Pression
REAL(KIND=XP) :: C_S_AD(NX)          !! Vitesse De Son Adimensionnée
REAL(KIND=XP) :: H_AD(NX)            !! Demi-Hauteur Du Disque Adimensionnée
REAL(KIND=XP) :: RHO_AD(NX)          !! Densité Volumique
REAL(KIND=XP) :: NU_AD(NX)           !! Viscosité
REAL(KIND=XP) :: S_AD(NX)            !! Densité De Surface
REAL(KIND=XP) :: V_AD(NX)            !! Vitese D'Accrétion
REAL(KIND=XP) :: M_DOT_AD(NX)        !! Taux D'Accrétion
REAL(KIND=XP) :: TEMP_AD(NX)         !! Température
REAL(KIND=XP) :: Q_PLUS_AD(NX)       !! Chaleur Apportée
REAL(KIND=XP) :: Q_ADV_AD(NX)        !! Chaleur Advectée
REAL(KIND=XP) :: C_V_AD(NX)          !! Capacité Calorifique
REAL(KIND=XP) :: B_AD(NX)            !! Coefficient b du trinôme pour le calcul de H
REAL(KIND=XP) :: C_AD(NX)            !! Coefficient c du trinôme pour le calcul de H

!! VARIABLES AVEC CONDITIONS
REAL(KIND=XP) :: Q_MOINS(NX)     !! Chaleur Dissipée Adimensionnée
REAL(KIND=XP) :: F_Z(NX)         !! Flux Adimensionnée
REAL(KIND=XP) :: TAU_EFF(NX)     !! Profondeur Optique Effective
REAL(KIND=XP) :: KAPPA_FF(NX)    !! Opacité Free-Free
REAL(KIND=XP) :: EPSILON_FF(NX)  !! Emissivité Free-Free

!! VARIABLES 
REAL(KIND=XP) :: T(NX)            !! Temps
REAL(KIND=XP) :: RAYON(NX)        !! Rayon
REAL(KIND=XP) :: OMEGA(NX)        !! Vitesse De Rotation
REAL(KIND=XP) :: P(NX)            !! Pression totale
REAL(KIND=XP) :: P_GAZ(NX)        !! Pression gazeuse
REAL(KIND=XP) :: P_RAD(NX)        !! Pression de radiation
REAL(KIND=XP) :: C_S(NX)          !! Vitesse De Son
REAL(KIND=XP) :: H(NX)            !! Demi-Hauteur Du Disque
REAL(KIND=XP) :: RHO(NX)          !! Densité Volumique
REAL(KIND=XP) :: NU(NX)           !! Viscosité
REAL(KIND=XP) :: SIGMA(NX)        !! Densité De Surface
REAL(KIND=XP) :: V(NX)            !! Vitesse D'Accrétion
REAL(KIND=XP) :: M_DOT(NX)        !! Taux D'Accrétion
REAL(KIND=XP) :: TEMP(NX)         !! Température
REAL(KIND=XP) :: Q_PLUS(NX)       !! Chaleur Apportée
REAL(KIND=XP) :: Q_ADV(NX)        !! Chaleur Advectée
REAL(KIND=XP) :: C_V(NX)          !! Capacité Calorifique

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
    INTEGER :: FILE_ID
    NAMELIST /INPUT/ M,F_ACCRETION,R_MAX,ALPHA,X_FRAC,Y_FRAC

    ! VALEURS PAR DEFAUT DES PARAMETRES D'ENTREE
    M             = 1._xp        ! [M_{sol}]
    f_accretion   = 0.001_xp
    r_max         = 100._xp    ! [r_{Schwarzschild}]
    ALPHA         = 1._xp
    X_FRAC        = 0.7_xp
    Y_FRAC        = 0.28_xp

    ! LECTURE DES PARAMETRES DEPUIS LE FICHIER
    OPEN(newunit=file_id,file='config/input.config',action='read',status='old')
    READ(file_id,input)
    CLOSE(file_id)

    ! AFFICHAGE PARAMETRES D'ENTREE
    WRITE(*,"(48('-'))")
    WRITE(*,"('-----------------INITIALISATION-----------------')")
    WRITE(*,"('------------VALEURS PARAMETRES EN INPUT---------')")
    WRITE(*,"(48('-'))")
    
    WRITE(*,"('MASSE DU TROU NOIR [MASSE SOLAIRE]             = ',F12.1)") M
    WRITE(*,"('RAPPORT TAUX D ACCRETION / TAUX CRITIQUE       = ',F12.3)") f_accretion  
    WRITE(*,"('R_MAX [RAYON DE SCHWARZSCHILD]                 = ',F12.1)") r_max
    WRITE(*,"('ALPHA                                          = ',F12.2)") ALPHA
    WRITE(*,"('X                                              = ',F12.2)") X_FRAC
    WRITE(*,"('Y                                              = ',F12.2)") Y_FRAC
    WRITE(*,"('NB POINTS DE DISCRETISATION SPATIALE           = ',I12)") NX

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
    INTEGER :: I

    ! TAUX D'ACCRETION
    L_EDD       = 4._XP * PI * G * M * M_P * C_SPEED / SIGMA_E
    M_CRIT_DOT  = 12._XP * L_EDD / C_SPEED**2._XP
    M_0_DOT     = F_ACCRETION * M_CRIT_DOT

    ! CONSTANTES DU SYSTEME
    Z_FRAC     = 1.0_XP - X_FRAC - Y_FRAC
    MU         = 1.0_XP / (2.0_XP * X_FRAC + 3.0_XP/4.0_XP * Y_FRAC + Z_FRAC/2.0_XP)
    R          = K_B / M_P 
    R_S        = 2.0_XP * G * M / C_SPEED**2.0_XP
    R_MIN      = 3.0_XP * R_S
    R_MAX      = R_MAX * R_S
    OMEGA_MAX  = (G * M / R_MIN**3.0_XP)**(0.5_XP) 
    L_TOT      = M_0_DOT * C_SPEED**2.0_XP / 12.0_XP 
    
    ! DECLARATION DES VARIABLES SPATIALES 
    X_MAX = SQRT(R_MAX/R_S)
    X_MIN = SQRT(R_MIN/R_S)

    DX = ( X_MAX - X_MIN ) / (NX)
    X_AD = X_MIN + DX * (/(I,I=1,NX)/)
    
    ! DECLARATION DES VARIABLES TEMPORELLES
    T_MAX = 1.0E10_XP * OMEGA_MAX
    T_MIN = 0.0_XP * OMEGA_MAX
    
    DT = ( T_MAX - T_MIN ) / (NT-1)
    T_AD = T_MIN + DT * (/(I,I=0,NT-1)/)
    
    ! CONSTANTES DE NORMALISATION
    V_0         = R_S * OMEGA_MAX
    NU_0        = 4.0_XP/3.0_XP * R_S**2.0_XP * OMEGA_MAX
    SIGMA_0     = M_0_DOT / (OMEGA_MAX * 2.0_XP * PI * R_S**2.0_XP)
    S_0         = SIGMA_0
    RHO_0       = SIGMA_0 / (2._XP * R_S)
    T_0         = (L_TOT / (9.0_XP * 4.0_XP * PI * R_S**2.0_XP * SIGMA_STEFAN))**(0.25_XP)
    P_0         = M_0_DOT * OMEGA_MAX / (4.0_XP * PI * R_S)
    P_RAD_0     = A_RADIATION * T_0**4.0_XP / 3.0_XP
    P_GAZ_0     = R * RHO_0 * T_0 / MU
    C_V_0       = R / MU
    F_Z_DIFF_0  = 2.0_XP * A_RADIATION * C_SPEED * T_0**4.0_XP / (3.0_XP * S_0)
    F_Z_RAD_0   = R_S * RHO_0**2._XP * T_0**(0.5_XP) * 6.22E13_XP
    KAPPA_E     = 0.02_XP * (1.0_XP + X_FRAC) 
    Q_PLUS_0    = 3.0_XP * R_S**2.0_XP * OMEGA_MAX**3.0_XP
    Q_ADV_0     = OMEGA_MAX * T_0 * R / MU
    B_0         = 2.0_XP * A_RADIATION * T_0**4.0_XP / (3.0_XP * R_S * S_0 * OMEGA_MAX**2.0_XP)
    C_0         = R * T_0 / (R_S**2.0_XP * MU * OMEGA_MAX**2.0_XP)

    WRITE(*,"(48('-'))")
    WRITE(*,"('------------CONSTANTES DE SIMULATION------------')")
    WRITE(*,"(48('-'))")
    
    WRITE(*,"('MASSE DU TROU NOIR                           M = ',1pE12.4)") M
    WRITE(*,"('TAUX D ACCRETION                       M_0_DOT = ',1pE12.4)") M_0_DOT
    WRITE(*,"('TAUX D ACCRETION CRITIQUE           M_CRIT_DOT = ',1pE12.4)") M_CRIT_DOT
    WRITE(*,"('LUMINOSITÉ D EDDINGTON                   L_EDD = ',1pE12.4)") L_EDD
    
    WRITE(*,"(48('-'))")
    
    WRITE(*,"('FRACTION D ELEMENTS LOURDS                   Z = ',F12.2)") Z_FRAC
    WRITE(*,"('POID MOLECULAIRE MOYEN                      MU = ',F12.4)") MU
    WRITE(*,"('RAYONS DE SCHWARZSCHILD                    R_S = ',1pE12.4)") R_S
    WRITE(*,"('RAYON MIN DISQUE ACCRETION               R_MIN = ',1pE12.4)") R_MIN
    WRITE(*,"('RAYON MAXIMAL DU DISQUE                  R_MAX = ',1pE12.4)") R_MAX
    WRITE(*,"('VITESSE ROTATION MAX                 OMEGA_MAX = ',F12.4)") OMEGA_MAX
    WRITE(*,"('LUMINOSITE TOTALE                        L_TOT = ',1pE12.4)") L_TOT
    
    WRITE(*,"(48('-'))")
    WRITE(*,"('------------VARIABLES SPATIALES-----------------')")
    WRITE(*,"(48('-'))")
    
    WRITE(*,"('PAS DE DISCRETISATION DX                    DX = ',1pE12.3)") DX
    WRITE(*,"('RAYON NORMALISÉ MINIMAL                  X_MIN = ',F12.3)") X_MIN
    WRITE(*,"('RAYON NORMALISÉ MAXIMAL                  X_MAX = ',F12.1)") X_MAX
    
    WRITE(*,"(48('-'))")
    WRITE(*,"('--------CONSTANTES DE NORMALISATION-------------')")
    WRITE(*,"(48('-'))")
    
    WRITE(*,"('CONSTANTE DE VITESSE                       V_0 = ',1pE12.4)") V_0
    WRITE(*,"('CONSTANTE DE VISCOSITE                    NU_0 = ',1pE12.4)") NU_0
    WRITE(*,"('CONSTANTE DE DENSITE                     RHO_0 = ',1pE12.4)") RHO_0
    WRITE(*,"('CONSTANTE DE TEMPERATURE                   T_0 = ',1pE12.4)") T_0
    WRITE(*,"('CONSTANTE DE DENSITE SURFACIQUE        SIGMA_0 = ',1pE12.4)") SIGMA_0
    WRITE(*,"('CONSTANTE DE DENSITE                       S_0 = ',1pE12.4)") S_0
    WRITE(*,"('CONSTANTE DE PRESSION                      P_0 = ',1pE12.4)") P_0
    WRITE(*,"('CONSTANTE DE PRESSON RAD               P_RAD_0 = ',1pE12.4)") P_RAD_0
    WRITE(*,"('CONSTANTE DE PRESSION GAZ              P_GAZ_0 = ',1pE12.4)") P_GAZ_0
    WRITE(*,"('CONSTANTE DE CAPACITE CALORIFIQUE        C_V_0 = ',1pE12.4)") C_V_0
    WRITE(*,"('CONSTANTE DE FLUX DIFFUSIF          F_Z_DIFF_0 = ',1pE12.4)") F_Z_DIFF_0
    WRITE(*,"('CONSTANTE DE FLUX RADIATIF           F_Z_RAD_0 = ',1pE12.4)") F_Z_RAD_0
    WRITE(*,"('CONSTANTE OPACITÉ DE THOMPSON          KAPPA_E = ',1pE12.4)") KAPPA_E
    WRITE(*,"('CONSTANTE DE CHALEURE APPORTÉE        Q_PLUS_0 = ',1pE12.4)") Q_PLUS_0
    WRITE(*,"('CONSTANTE DE CHALEURE ADVECTÉE         Q_ADV_0 = ',1pE12.4)") Q_ADV_0
    WRITE(*,"('COEFFICIENT B DU POLYNOME DE H             B_0 = ',1pE12.4)") B_0
    WRITE(*,"('COEFFICIENT C DU POLYNOME DE H             C_0 = ',1pE12.4)") C_0  
    
    WRITE(*,"(48('-'))")

!---------------------------------------------------------------------------------------------------
END SUBROUTINE CALCUL_CONSTANTES
!---------------------------------------------------------------------------------------------------

!===================================================================================================
END MODULE MODULE_DECLARATIONS
!===================================================================================================
