!===================================================================================================
            MODULE EQUATIONS
!===================================================================================================
!> Ce module permet de calculer les variables adimensionnées dans l'ordre adéquat
!===================================================================================================

USE DECLARATIONS
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
!> (TEMP, S) => (P_RAD) 
!>           => H 
!>           => (C_S, RHO)
!>               C_S => NU => (Q+, V => M_DOT)
!>               RHO => (P_GAZ, KAPPA_FF)
!>                       P_GAZ    => P => BETA => (CV, GAMMA_3) => Q_ADV 
!>                       KAPPA_FF => TAU_EFF => FZ => Q-
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE
            
    REAL(KIND=xp) ,DIMENSION(NX) :: GAMMA_3           !! Exposant adiabatique
    REAL(KIND=xp)                :: CONDITION_EXT_V   !! Condition au bord extérieur sur la vitesse d'accrétion
    INTEGER :: I
    
    ! Pression de radiation
    P_RAD_AD = TEMP_AD**4.0_xp
    
    
    
    ! Demi-hauteur du disque (calcul coefficients d'un trinôme du 2nd degré dont H est la solution positive)
    B_AD = (TEMP_AD**4._xp * X_AD) / (OMEGA_AD**2._xp * S_AD)
    C_AD = TEMP_AD / (OMEGA_AD**2._xp)
    H_AD = (B_0 * B_AD + ((B_0 * B_AD)**2._xp + 4._xp * C_0 * C_AD)**0.5_xp) / 2._xp

    ! Vitesse du son
    C_S_AD = OMEGA_AD * H_AD

    ! Densité volumique
    RHO_AD = S_AD / (X_AD * H_AD)
    
    ! Viscosité
    NU_AD = 0.5_xp * ALPHA * C_S_AD * H_AD

    ! Q+ (chaleur apportée)
    Q_PLUS_AD = NU_AD * OMEGA_AD**2.0_xp

    ! Vitesse d'accrétion
    CONDITION_EXT_V = 0.5_xp
    SPEED_AD        = -2.0_xp / (X_AD * S_AD) * EULER_SPATIAL(NU_AD * S_AD, CONDITION_EXT_V)

    ! Taux d'accrétion
    M_DOT_AD = - SPEED_AD * S_AD * X_AD
    
    ! Pression gazeuse
    P_GAZ_AD = RHO_AD * TEMP_AD

    ! Pression totale
    P_AD = (P_RAD_0 * P_RAD_AD + P_GAZ_0 * P_GAZ_AD) / P_0

    ! Indicateur de pression
    BETA = P_GAZ_0 * P_GAZ_AD / (P_0 * P_AD)
    
    ! C_v
    C_V_AD = (12.0_xp * (GAMMA_G - 1.0_xp) * (1.0_xp - BETA) + BETA) / (( GAMMA_G - 1.0_xp ) * BETA)

    ! Gamma_3
    GAMMA_3 = (4.0_xp - 3.0_xp * BETA) / (BETA * C_V_AD) + 1.0_xp
    
    ! KAPPA_FF
    KAPPA_FF = RHO_AD * TEMP_AD**(-3.5_xp) * (RHO_0 * TEMP_0**(-3.5_xp) * 6.13E18_xp)
    
    ! TAU_EFF
    TAU_EFF = 0.5_xp * S_AD / X_AD * ( KAPPA_FF * KAPPA_E ) ** (0.5_xp) * S_0
    
    !EPSILON_FF
    EPSILON_FF = (RHO_AD * RHO_0) ** 2.0_xp * (TEMP_AD * TEMP_0) ** 0.5_xp * 6.22E13_xp
    
    ! FZ
    WHERE (TAU_EFF >= 1.0_xp)
        F_Z = F_Z_DIFF_0 * X_AD * TEMP_AD**4.0_xp / ((KAPPA_FF + KAPPA_E) * S_AD)
    ELSEWHERE
        F_Z = F_Z_RAD_0 * RHO_AD**2.0_xp * TEMP_AD**0.5_xp * H_AD
    END WHERE
    
    ! Q- (chaleur dissipée)
    Q_MOINS_AD = 2.0_xp * X_AD * F_Z / (S_AD * S_0)  / Q_PLUS_0
      
    ! Luminosité du disque
    L_STEFAN = 0.0_xp
    DO I=1,NX-1
        L_STEFAN = L_STEFAN + F_Z(I)*8.0_xp*PI*R_S**2.0_xp*X_AD(I)**3.0_xp*(X_AD(I+1)-X_AD(I))
    ENDDO
    
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
    REAL(KIND=xp) :: VECTOR(NX)         !! Vecteur 1D initial
    REAL(KIND=xp) :: DERIVEE(NX)        !! Vecteur dérivée
    REAL(KIND=xp) :: CONDITION_LIMITE   !! Condition limite au bord droit

    DERIVEE(1:NX-1) = (VECTOR(2:NX) - VECTOR(1:NX-1)) / DX
    DERIVEE(NX)     = CONDITION_LIMITE 

!---------------------------------------------------------------------------------------------------
END FUNCTION EULER_SPATIAL
!---------------------------------------------------------------------------------------------------

!===================================================================================================
END MODULE EQUATIONS
!===================================================================================================
