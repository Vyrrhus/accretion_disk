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

! PRECISION
INTEGER,PARAMETER :: xp =  SELECTED_REAL_KIND(15) !! Précision des calculs

! CONSTANTES PHYSIQUES 
REAL(KIND=xp),PARAMETER :: K_B             = 1.380649E-23_xp                   !! Constante de Boltzmann
REAL(KIND=xp),PARAMETER :: PI              = 3.1415926535897932384626433_xp    !! \(\pi\)
REAL(KIND=xp),PARAMETER :: C_SPEED         = 2.99792458E8_xp                   !! Vitesse de la lumière
REAL(KIND=xp),PARAMETER :: M_P             = 1.67262192369E-27_xp              !! Masse du proton
REAL(KIND=xp),PARAMETER :: G_CONST         = 6.6743E-11_xp                     !! Constante de gravitation
REAL(KIND=xp),PARAMETER :: SIGMA_STEFAN    = 5.670374419E-8_xp                 !! Constante de Stefan-Boltzmann
REAL(KIND=xp),PARAMETER :: A_RADIATION     = 7.56573085E-16_xp                 !! Constante de radiation
REAL(KIND=xp),PARAMETER :: M_O             = 1.989e30_xp                       !! Masse du Soleil
REAL(KIND=xp),PARAMETER :: GAMMA_G         = 1.6666666666666666667_xp          !! Indice polytropique
REAL(KIND=xp),PARAMETER :: SIGMA_E         = 6.6524587321E-29_xp               !! Section efficace de Thomson

! PARAMETRES MODELE
REAL(KIND=xp)  :: MASS               !! Masse trou noir de Schwarzschild
REAL(KIND=xp)  :: M_0_DOT            !! Taux d'accrétion du milieu ext. sur le disque
REAL(KIND=xp)  :: f_accretion        !! Rapport entre le taux d'accrétion et le taux d'accrétion critique
REAL(KIND=xp)  :: r_max              !! Rayon maximal du disque
REAL(KIND=xp)  :: ALPHA              !! Paramètre phénoménologique (taux de production d'énergie par friction de la matière)
REAL(KIND=xp)  :: X_FRAC             !! Abondance H
REAL(KIND=xp)  :: Y_FRAC             !! Abondance He

! PARAMÈTRE DE TEMPS À ATTEINDRE
REAL(KIND=xp) :: TIME_TO_REACH       !! Temps dimensionné à atteindre par la simulation 

! PARAMÈTRES SPATIAUX
INTEGER,PARAMETER :: NX = 100       !! Nombre de points de discrétisation spatiale
REAL(KIND=xp)     :: DX              !! Pas de discrétisation spatiale
REAL(KIND=xp)     :: X_MIN           !! Rayon minimal adimensionné
REAL(KIND=xp)     :: X_MAX           !! Rayon maximal adimensionné

! PARAMÈTRES COURBES EN S
INTEGER       :: COURBE_EN_S    !! Paramètre pour générer les courbes en S si = 1
INTEGER       :: N_S            !! Nombre de points
REAL(KIND=xp) :: TEMP_MIN_AD    !! Borne inférieure température (adim)
REAL(KIND=xp) :: TEMP_MAX_AD    !! Borne supérieure température (adim)
REAL(KIND=xp) :: SG_AD          !! Borne inférieure densité de surface (adim)
REAL(KIND=xp) :: SD_AD          !! Borne supérieure densité de surface (adim)

! FRAMES 2D
INTEGER :: FRAME_COND       !! Paramètre pour générer les frames si = 1
INTEGER :: FRAME_ID         !! Identifiant du frame en cours

! CONSTANTES DU SYSTEME
REAL(KIND=xp)  :: Z_FRAC        !! Abondance éléments lourds
REAL(KIND=xp)  :: MU            !! Masse atomique moyenne \(\mu\)
REAL(KIND=xp)  :: R_BOLTZ       !! Boltzmann over proton_mass / La constante de Come
REAL(KIND=xp)  :: R_S           !! Rayon de Schwarzschild
REAL(KIND=xp)  :: R_MIN         !! Rayon minimal du disque
REAL(KIND=xp)  :: OMEGA_MAX     !! Vitesse angulaire maximale (au rayon minimal)
REAL(KIND=xp)  :: L_TOT         !! Luminosité maximale
REAL(KIND=xp)  :: L_EDD         !! Luminosité d'Eddington
REAL(KIND=xp)  :: M_CRIT_DOT    !! Taux d'acrétion critique

! CONSTANTES DE NORMALISATION
REAL(KIND=xp)  :: SPEED_0       !! Vitesse d'accrétion
REAL(KIND=xp)  :: NU_0          !! Viscosité
REAL(KIND=xp)  :: L_STEFAN_0    !! Luminosié  
REAL(KIND=xp)  :: SIGMA_0       !! Densité de surface
REAL(KIND=xp)  :: S_0           !! \(S_0 = \Sigma_0\)
REAL(KIND=xp)  :: RHO_0         !! Densité volumique
REAL(KIND=xp)  :: TEMP_0        !! Température
REAL(KIND=xp)  :: P_0           !! Pression totale
REAL(KIND=xp)  :: P_RAD_0       !! Pression de radiation
REAL(KIND=xp)  :: P_GAZ_0       !! Pression gazeuse
REAL(KIND=xp)  :: C_V_0         !! Capacité calorifique
REAL(KIND=xp)  :: F_Z_DIFF_0    !! Flux (diffusif) lorsque la profondeur optique \(\tau_{eff} > 1\)
REAL(KIND=xp)  :: F_Z_RAD_0     !! Flux (radiatif) lorsque \(\tau_{eff} < 1\)
REAL(KIND=xp)  :: KAPPA_E       !! Opacité diffusion Thomson
REAL(KIND=xp)  :: Q_PLUS_0      !! Terme de chauffage
REAL(KIND=xp)  :: Q_ADV_0       !! Chaleur advectée
REAL(KIND=xp)  :: B_0           !! Coefficient b du trinôme pour le calcul de H
REAL(KIND=xp)  :: C_0           !! Coefficient c du trinôme pour le calcul de H

! VARIABLES ADIMENSIONNÉES
REAL(KIND=xp) :: TIME_AD             !! Temps Adimensionné
REAL(KIND=xp) :: X_AD(NX)            !! Rayon Adimensionné
REAL(KIND=xp) :: OMEGA_AD(NX)        !! Vitesse De Rotation Adimensionnée
REAL(KIND=xp) :: P_AD(NX)            !! Pression totale adimensionnée
REAL(KIND=xp) :: P_GAZ_AD(NX)        !! Pression gazeuse adimensionnée
REAL(KIND=xp) :: P_RAD_AD(NX)        !! Pression de radiation adimensionnée
REAL(KIND=xp) :: C_S_AD(NX)          !! Vitesse De Son Adimensionnée
REAL(KIND=xp) :: H_AD(NX)            !! Demi-Hauteur Du Disque Adimensionnée
REAL(KIND=xp) :: RHO_AD(NX)          !! Densité Volumique
REAL(KIND=xp) :: NU_AD(NX)           !! Viscosité
REAL(KIND=xp) :: S_AD(NX)            !! Densité De Surface
REAL(KIND=xp) :: SPEED_AD(NX)        !! Vitese D'Accrétion
REAL(KIND=xp) :: M_DOT_AD(NX)        !! Taux D'Accrétion
REAL(KIND=xp) :: TEMP_AD(NX)         !! Température
REAL(KIND=xp) :: Q_PLUS_AD(NX)       !! Chaleur Apportée
REAL(KIND=xp) :: Q_MOINS_AD(NX)      !! Chaleur dissipée adimensionnée
REAL(KIND=xp) :: Q_ADV_AD(NX)        !! Chaleur Advectée
REAL(KIND=xp) :: C_V_AD(NX)          !! Capacité Calorifique
REAL(KIND=xp) :: B_AD(NX)            !! Coefficient b du trinôme pour le calcul de H
REAL(KIND=xp) :: C_AD(NX)            !! Coefficient c du trinôme pour le calcul de H
REAL(KIND=xp) :: L_STEFAN_AD         !! Luminosité Adimensionnée

! VARIABLES 
REAL(KIND=xp) :: TIME             !! Temps
REAL(KIND=xp) :: RADIUS(NX)       !! Rayon
REAL(KIND=xp) :: OMEGA(NX)        !! Vitesse De Rotation
REAL(KIND=xp) :: P(NX)            !! Pression totale
REAL(KIND=xp) :: P_GAZ(NX)        !! Pression gazeuse
REAL(KIND=xp) :: P_RAD(NX)        !! Pression de radiation
REAL(KIND=xp) :: BETA(NX)         !! Indicateur De Pression
REAL(KIND=xp) :: C_S(NX)          !! Vitesse De Son
REAL(KIND=xp) :: H(NX)            !! Demi-Hauteur Du Disque
REAL(KIND=xp) :: RHO(NX)          !! Densité Volumique
REAL(KIND=xp) :: NU(NX)           !! Viscosité
REAL(KIND=xp) :: SIGMA(NX)        !! Densité De Surface
REAL(KIND=xp) :: SPEED(NX)        !! Vitesse D'Accrétion
REAL(KIND=xp) :: M_DOT(NX)        !! Taux D'Accrétion
REAL(KIND=xp) :: TEMP(NX)         !! Température
REAL(KIND=xp) :: Q_PLUS(NX)       !! Chaleur Apportée
REAL(KIND=xp) :: Q_ADV(NX)        !! Chaleur Advectée
REAL(KIND=xp) :: C_V(NX)          !! Capacité Calorifique
REAL(KIND=xp) :: Q_MOINS(NX)      !! Chaleur Dissipée
REAL(KIND=xp) :: F_Z(NX)          !! Flux Adimensionnée
REAL(KIND=xp) :: TAU_EFF(NX)      !! Profondeur Optique Effective
REAL(KIND=xp) :: KAPPA_FF(NX)     !! Opacité Free-Free
REAL(KIND=xp) :: EPSILON_FF(NX)   !! Emissivité Free-Free
REAL(KIND=xp) :: L_STEFAN         !! Luminosité

! PAS DE TEMPS
REAL(KIND=xp) :: DELTA_T_VISQ_AD    !! Pas de temps visqueux (adim)
REAL(KIND=xp) :: DELTA_T_TH_AD      !! Pas de temps thermique (adim)

! POINTS CRITIQUES COURBES EN S
REAL(KIND=xp) :: TEMP_CRITIQUE(NX)      !! Température point critique
REAL(KIND=xp) :: SIGMA_CRITIQUE(NX)     !! Densité de surface point critique

!===================================================================================================
            CONTAINS 
!===================================================================================================

SUBROUTINE APPEL_PARAM_INPUT()
!---------------------------------------------------------------------------------------------------
!> Cette routine lit les paramètres d'entrée du fichier input.txt et attribue les valeurs associées 
!> à M, M_0_DOT, R_MAX, ALPHA, X et Y.
!> On utilise des valeurs par défaut en cas d'erreur.
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE
    INTEGER :: FILE_ID
    NAMELIST /INPUT/ MASS, F_ACCRETION, R_MAX, ALPHA, X_FRAC, Y_FRAC,           &
                &    TIME_TO_REACH,                                             &
                &    COURBE_EN_S, TEMP_MIN_AD, TEMP_MAX_AD, SG_AD, SD_AD, N_S,  &
                &    FRAME_COND

    ! VALEURS PAR DEFAUT DES PARAMETRES D'ENTREE
    MASS          = 1._xp      ! [M_{sol}]
    f_accretion   = 0.001_xp
    r_max         = 100._xp    ! [r_{Schwarzschild}]
    ALPHA         = 1._xp
    X_FRAC        = 0.7_xp
    Y_FRAC        = 0.28_xp
    
    TIME_TO_REACH = 125.0_xp
    
    COURBE_EN_S   = 1
    TEMP_MIN_AD   = 5.0e-2_xp
    TEMP_MAX_AD   = 5.0e1_xp
    SG_AD         = 1.0_xp
    SD_AD         = 1.0e6_xp
    N_S           = 5000
    
    FRAME_COND = 0
    FRAME_ID = 0

    ! LECTURE DES PARAMETRES DEPUIS LE FICHIER
    OPEN(newunit=FILE_ID, file='config/input.config', action='read', status='old')
    READ(FILE_ID,input)
    CLOSE(FILE_ID)
    
    ! AFFICHAGE PARAMETRES D'ENTREE
    WRITE(*,"(48('-'))")
    WRITE(*,"('-----------------INITIALISATION-----------------')")
    WRITE(*,"('------------VALEURS PARAMETRES EN INPUT---------')")
    WRITE(*,"(48('-'))")
    WRITE(*,"('MASSE DU TROU NOIR [MASSE SOLAIRE]             = ',F12.1)") MASS
    WRITE(*,"('RAPPORT TAUX D ACCRETION / TAUX CRITIQUE       = ',F12.5)") f_accretion  
    WRITE(*,"('R_MAX [RAYON DE SCHWARZSCHILD]                 = ',F12.1)") r_max
    WRITE(*,"('ALPHA                                          = ',F12.2)") ALPHA
    WRITE(*,"('X                                              = ',F12.2)") X_FRAC
    WRITE(*,"('Y                                              = ',F12.2)") Y_FRAC
    WRITE(*,"('TIME_TO_REACH                                  = ',F12.4)") TIME_TO_REACH
    WRITE(*,"('NB POINTS DE DISCRETISATION SPATIALE           = ',I12)") NX
    
    ! CONVERSION UNITES SI
    MASS = MASS * M_O
    
!---------------------------------------------------------------------------------------------------
END SUBROUTINE APPEL_PARAM_INPUT
!---------------------------------------------------------------------------------------------------

!---------------------------------------------------------------------------------------------------
SUBROUTINE CALCUL_CONSTANTES()
!---------------------------------------------------------------------------------------------------
!> Cette routine calcule l'ensemble des constantes du modèle et les constantes d'adimensionnement. 
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE
    INTEGER :: I
   
    ! TAUX D'ACCRETION
    L_EDD       = 4._xp * PI * G_CONST * MASS * M_P * C_SPEED / SIGMA_E
    M_CRIT_DOT  = 12._xp * L_EDD / C_SPEED**2._xp
    M_0_DOT     = F_ACCRETION * M_CRIT_DOT

    ! CONSTANTES DU SYSTEME
    Z_FRAC     = 1.0_xp - X_FRAC - Y_FRAC
    MU         = 1.0_xp / (2.0_xp * X_FRAC + 3.0_xp / 4.0_xp * Y_FRAC + Z_FRAC / 2.0_xp)
    R_BOLTZ    = K_B / M_P 
    R_S        = 2.0_xp * G_CONST * MASS / C_SPEED**2.0_xp
    R_MIN      = 3.0_xp * R_S
    R_MAX      = R_MAX * R_S
    OMEGA_MAX  = (G_CONST * MASS / R_MIN**3.0_xp)**(0.5_xp) 
    L_TOT      = M_0_DOT * C_SPEED**2.0_xp / 12.0_xp 
    
    ! DECLARATION DES VARIABLES SPATIALES 
    X_MAX = SQRT(R_MAX/R_S)
    X_MIN = SQRT(R_MIN/R_S)

    DX = ( X_MAX - X_MIN ) / (NX)
    X_AD = X_MIN + DX * (/(I,I=1,NX)/)
    
    ! OMEGA_AD
    OMEGA_AD = 3.0_xp**(1.5_xp) * X_AD**(-3.0_xp)
    
    ! DECLARATION DES VARIABLES TEMPORELLES
    TIME_AD = 0.0_xp
    
    ! CONSTANTES DE NORMALISATION
    SPEED_0     = R_S * OMEGA_MAX
    NU_0        = 4.0_xp/3.0_xp * R_S**2.0_xp * OMEGA_MAX
    SIGMA_0     = M_0_DOT / (OMEGA_MAX * 2.0_xp * PI * R_S**2.0_xp)
    S_0         = SIGMA_0
    RHO_0       = SIGMA_0 / (2._xp * R_S)
    TEMP_0      = (L_TOT / (9.0_xp * 4.0_xp * PI * R_S**2.0_xp * SIGMA_STEFAN))**(0.25_xp)
    L_STEFAN_0  = 4.0_xp * PI * R_S**2.0_xp * SIGMA_STEFAN * TEMP_0**4_xp
    P_0         = M_0_DOT * OMEGA_MAX / (4.0_xp * PI * R_S)
    P_RAD_0     = A_RADIATION * TEMP_0**4.0_xp / 3.0_xp
    P_GAZ_0     = R_BOLTZ * RHO_0 * TEMP_0 / MU
    C_V_0       = R_BOLTZ/ MU
    F_Z_DIFF_0  = 2.0_xp * A_RADIATION * C_SPEED * TEMP_0**4.0_xp / (3.0_xp * S_0)
    F_Z_RAD_0   = R_S * RHO_0**2._xp * TEMP_0**(0.5_xp) * 6.22E13_xp
    KAPPA_E     = 0.02_xp * (1.0_xp + X_FRAC) 
    Q_PLUS_0    = 3.0_xp * R_S**2.0_xp * OMEGA_MAX**3.0_xp
    Q_ADV_0     = OMEGA_MAX * TEMP_0 * R_BOLTZ/ MU
    B_0         = 2.0_xp * A_RADIATION * TEMP_0**4.0_xp / (3.0_xp * R_S * S_0 * OMEGA_MAX**2.0_xp)
    C_0         = R_BOLTZ* TEMP_0 / (R_S**2.0_xp * MU * OMEGA_MAX**2.0_xp)
    
   ! AFFICHAGE CONSTANTES
    WRITE(*,"(48('-'))")
    WRITE(*,"('------------CONSTANTES DE SIMULATION------------')")
    WRITE(*,"(48('-'))")
    
    WRITE(*,"('MASSE DU TROU NOIR                        MASS = ',1pE12.4)") MASS
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
    
    WRITE(*,"('CONSTANTE DE VITESSE                   SPEED_0 = ',1pE12.4)") SPEED_0
    WRITE(*,"('CONSTANTE DE VISCOSITE                    NU_0 = ',1pE12.4)") NU_0
    WRITE(*,"('CONSTANTE DE DENSITE                     RHO_0 = ',1pE12.4)") RHO_0
    WRITE(*,"('CONSTANTE DE TEMPERATURE                TEMP_0 = ',1pE12.4)") TEMP_0
    WRITE(*,"('CONSTANTE DE LUMINOSITÉ             L_STEFAN_0 = ',1pE12.4)") L_STEFAN_0
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

SUBROUTINE WRITE_CONSTANTES(FILE_UNIT)
!---------------------------------------------------------------------------------------------------
!> Cette routine écrit dans un fichier de sortie les constantes de simulation et de normalisation.
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: FILE_UNIT

    WRITE( FILE_UNIT,"(48('-'))")
    WRITE( FILE_UNIT,"('------------CONSTANTES DE SIMULATION------------')")
    WRITE(FILE_UNIT,"(48('-'))")
    
    WRITE(FILE_UNIT,"('MASSE DU TROU NOIR                        MASS = ',1pE12.4)") MASS
    WRITE(FILE_UNIT,"('TAUX D ACCRETION                       M_0_DOT = ',1pE12.4)") M_0_DOT
    WRITE(FILE_UNIT,"('TAUX D ACCRETION CRITIQUE           M_CRIT_DOT = ',1pE12.4)") M_CRIT_DOT
    WRITE(FILE_UNIT,"('LUMINOSITÉ D EDDINGTON                   L_EDD = ',1pE12.4)") L_EDD
    
    WRITE(FILE_UNIT,"(48('-'))")
    
    WRITE(FILE_UNIT,"('FRACTION D ELEMENTS LOURDS                   Z = ',F12.2)") Z_FRAC
    WRITE(FILE_UNIT,"('POID MOLECULAIRE MOYEN                      MU = ',F12.4)") MU
    WRITE(FILE_UNIT,"('RAYONS DE SCHWARZSCHILD                    R_S = ',1pE12.4)") R_S
    WRITE(FILE_UNIT,"('RAYON MIN DISQUE ACCRETION               R_MIN = ',1pE12.4)") R_MIN
    WRITE(FILE_UNIT,"('RAYON MAXIMAL DU DISQUE                  R_MAX = ',1pE12.4)") R_MAX
    WRITE(FILE_UNIT,"('VITESSE ROTATION MAX                 OMEGA_MAX = ',F12.4)") OMEGA_MAX
    WRITE(FILE_UNIT,"('LUMINOSITE TOTALE                        L_TOT = ',1pE12.4)") L_TOT
    
    WRITE(FILE_UNIT,"(48('-'))")
    WRITE(FILE_UNIT,"('------------VARIABLES SPATIALES-----------------')")
    WRITE(FILE_UNIT,"(48('-'))")
    
    WRITE(FILE_UNIT,"('NB POINTS DE DISCRETISATION SPATIALE           = ',I12)") NX
    WRITE(FILE_UNIT,"('PAS DE DISCRETISATION DX                    DX = ',1pE12.3)") DX
    WRITE(FILE_UNIT,"('RAYON NORMALISÉ MINIMAL                  X_MIN = ',F12.3)") X_MIN
    WRITE(FILE_UNIT,"('RAYON NORMALISÉ MAXIMAL                  X_MAX = ',F12.1)") X_MAX
    
    WRITE(FILE_UNIT,"(48('-'))")
    WRITE(FILE_UNIT,"('--------CONSTANTES DE NORMALISATION-------------')")
    WRITE(FILE_UNIT,"(48('-'))")
    
    WRITE(FILE_UNIT,"('CONSTANTE DE VITESSE                   SPEED_0 = ',1pE12.4)") SPEED_0
    WRITE(FILE_UNIT,"('CONSTANTE DE VISCOSITE                    NU_0 = ',1pE12.4)") NU_0
    WRITE(FILE_UNIT,"('CONSTANTE DE DENSITE                     RHO_0 = ',1pE12.4)") RHO_0
    WRITE(FILE_UNIT,"('CONSTANTE DE TEMPERATURE                TEMP_0 = ',1pE12.4)") TEMP_0
    WRITE(FILE_UNIT,"('CONSTANTE DE LUMINOSITÉ             L_STEFAN_0 = ',1pE12.4)") L_STEFAN_0
    WRITE(FILE_UNIT,"('CONSTANTE DE DENSITE SURFACIQUE        SIGMA_0 = ',1pE12.4)") SIGMA_0
    WRITE(FILE_UNIT,"('CONSTANTE DE DENSITE                       S_0 = ',1pE12.4)") S_0
    WRITE(FILE_UNIT,"('CONSTANTE DE PRESSION                      P_0 = ',1pE12.4)") P_0
    WRITE(FILE_UNIT,"('CONSTANTE DE PRESSON RAD               P_RAD_0 = ',1pE12.4)") P_RAD_0
    WRITE(FILE_UNIT,"('CONSTANTE DE PRESSION GAZ              P_GAZ_0 = ',1pE12.4)") P_GAZ_0
    WRITE(FILE_UNIT,"('CONSTANTE DE CAPACITE CALORIFIQUE        C_V_0 = ',1pE12.4)") C_V_0
    WRITE(FILE_UNIT,"('CONSTANTE DE FLUX DIFFUSIF          F_Z_DIFF_0 = ',1pE12.4)") F_Z_DIFF_0
    WRITE(FILE_UNIT,"('CONSTANTE DE FLUX RADIATIF           F_Z_RAD_0 = ',1pE12.4)") F_Z_RAD_0
    WRITE(FILE_UNIT,"('CONSTANTE OPACITÉ DE THOMPSON          KAPPA_E = ',1pE12.4)") KAPPA_E
    WRITE(FILE_UNIT,"('CONSTANTE DE CHALEURE APPORTÉE        Q_PLUS_0 = ',1pE12.4)") Q_PLUS_0
    WRITE(FILE_UNIT,"('CONSTANTE DE CHALEURE ADVECTÉE         Q_ADV_0 = ',1pE12.4)") Q_ADV_0
    WRITE(FILE_UNIT,"('COEFFICIENT B DU POLYNOME DE H             B_0 = ',1pE12.4)") B_0
    WRITE(FILE_UNIT,"('COEFFICIENT C DU POLYNOME DE H             C_0 = ',1pE12.4)") C_0  
    
    WRITE(FILE_UNIT,"(48('-'))")
    
!---------------------------------------------------------------------------------------------------
END SUBROUTINE WRITE_CONSTANTES
!---------------------------------------------------------------------------------------------------

!===================================================================================================
END MODULE MODULE_DECLARATIONS
!===================================================================================================
