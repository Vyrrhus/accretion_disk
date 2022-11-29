!===================================================================================================
            MODULE MODULE_FUNCTION
!===================================================================================================
!>   Ce module permet de calculer les variables adimensionnées dans l'ordre adéquat
!===================================================================================================

USE MODULE_DECLARATIONS
IMPLICIT NONE

!===================================================================================================
            CONTAINS 
!===================================================================================================

SUBROUTINE COMPUTE_EQS()
!---------------------------------------------------------------------------------------------------
!> Subroutine pour calculer les valeurs de l'ensemble des variables du modèle.
!> On détermine chaque variable adimensionnée à partir de l'équation adéquate, en partant de la
!> température TEMP et de la densité de surface S.
!> La procédure de calcul se fait dans l'ordre suivant :
!> (TEMP, S) => (OMEGA, P_RAD) 
!>           => H 
!>           => (C_S, RHO)
!>               C_S => NU => (Q+, V => M_DOT)
!>               RHO => (P_GAZ, KAPPA_FF)
!>                       P_GAZ    => P => BETA => (CV, GAMMA_3) => Q_ADV 
!>                       KAPPA_FF => TAU_EFF => FZ => Q-
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE
            
    REAL(KIND=XP),DIMENSION(NX) :: GAMMA_3     !! Exposant adiabatique
    REAL(KIND=XP) :: CONDITION_EXT_V           !! Condition au bord extérieur sur la vitesse d'accrétion
    
    ! Vitesse de rotation
    OMEGA_AD = 3.0_XP**(1.5_XP) * X_AD**(-3.0_XP)

    ! Pression de radiation
    P_RAD_AD = TEMP_AD**4.0_XP

    ! Demi-hauteur du disque (calcul coefficients d'un trinôme du 2nd degré dont H est la solution positive)
    B_AD = (TEMP_AD**4._XP * X_AD) / (OMEGA_AD**2._XP * S_AD)
    C_AD = TEMP_AD / (OMEGA_AD**2._XP)
    H_AD = (B_0 * B_AD + ((B_0 * B_AD)**2._XP + 4._XP * C_0 * C_AD)**0.5_XP) / 2._XP

    ! Vitesse du son
    C_S_AD = OMEGA_AD * H_AD

    ! Densité volumique
    RHO_AD = S_AD / (X_AD * H_AD)
    
    ! Viscosité
    NU_AD = 0.5_XP * ALPHA * C_S_AD * H_AD

    ! Q+ (chaleur apportée)
    Q_PLUS_AD = NU_AD * OMEGA_AD**2.0_XP

    ! Vitesse d'accrétion
    CONDITION_EXT_V = -1.0_XP / (X_MAX * S_AD(NX))
    V_AD            = -2.0_XP / (X_AD * S_AD) * EULER_SPATIAL(NU_AD * S_AD, CONDITION_EXT_V)

    ! Taux d'accrétion
    M_DOT_AD = - V_AD * S_AD * X_AD
    
    ! Pression gazeuse
    P_GAZ_AD = RHO_AD * TEMP_AD

    ! Pression totale
    P_AD = (P_RAD_0 * P_RAD_AD + P_GAZ_0 * P_GAZ_AD) / P_0

    ! Indicateur de pression
    BETA = P_GAZ_0 * P_GAZ_AD / (P_0 * P_AD)
    
    ! C_v
    C_V_AD = (12.0_XP * (GAMMA_G - 1.0_XP) * (1.0_XP - BETA) + BETA) / (( GAMMA_G - 1.0_XP ) * BETA)

    ! Gamma_3
    GAMMA_3 = (4.0_XP - 3.0_XP * BETA) / (BETA * C_V_AD) + 1.0_XP
    
    ! KAPPA_FF
    KAPPA_FF = RHO_AD * TEMP_AD**(-3.5_XP) * (RHO_0 * T_0**(-3.5_XP) * 6.13E18_XP)
    
    ! TAU_EFF
    TAU_EFF = 0.5_XP * S_AD / X_AD * KAPPA_FF * KAPPA_E * S_0
    
    !EPSILON_FF
    EPSILON_FF = (RHO_AD*RHO_0) ** 2.0_XP * (TEMP_AD*T_0) ** 0.5_XP * 6.22E13_XP
    
    ! FZ
    WHERE (TAU_EFF >= 1.0_XP)
        F_Z = F_Z_DIFF_0 * X_AD * TEMP_AD**4.0_XP / ((KAPPA_FF + KAPPA_E) * S_AD)
    ELSEWHERE
        F_Z = F_Z_RAD_0 * RHO_AD**2.0_XP * TEMP_AD**0.5_XP * H_AD
    END WHERE
    
    ! Q- (chaleur dissipée)
    Q_MOINS = 2.0_XP * X_AD * F_Z / (S_AD * S_0)
      
!---------------------------------------------------------------------------------------------------
END SUBROUTINE COMPUTE_EQS
!---------------------------------------------------------------------------------------------------

!===================================================================================================
! CALCUL DE DERIVEES
!===================================================================================================
FUNCTION EULER_SPATIAL(VECTOR,CONDITION_LIMITE) RESULT(DERIVEE)
!---------------------------------------------------------------------------------------------------
!> Calcul de la dérivée spatiale d'un vecteur de dimension NX
!> On utilise une méthode d'Euler explicite avec une condition limite au bord droit
!> Le pas utilisé est fixe : DX (cf. module_declarations)
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE
    REAL(KIND=XP) :: VECTOR(NX)
    REAL(KIND=XP) :: DERIVEE(NX)
    REAL(KIND=XP) :: CONDITION_LIMITE

    DERIVEE(1:NX-1) = (VECTOR(1:NX-1) - VECTOR(2:NX)) / DX
    DERIVEE(NX) = CONDITION_LIMITE 

!---------------------------------------------------------------------------------------------------
END FUNCTION EULER_SPATIAL
!---------------------------------------------------------------------------
            END MODULE MODULE_FUNCTION
!---------------------------------------------------------------------------
