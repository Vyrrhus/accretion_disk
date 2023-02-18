!===================================================================================================
            MODULE DIMENSIONNEMENT
!===================================================================================================
!> Ce module permet de passer de variables sans dimension aux variables physiques et 
!> vice-versa.
!> On peut également convertir toutes les variables physiques du système cgs au système SI
!> et inversement.
!> Système SI  : kg,  m, s, J
!> Système CGS :  g, cm, s, erg
!===================================================================================================

USE DECLARATIONS 
IMPLICIT NONE

!===================================================================================================
            CONTAINS 
!===================================================================================================

SUBROUTINE ADIM_TO_PHYSIQUE()
!---------------------------------------------------------------------------------------------------
!> Cette routine donne une valeur aux variables physiques à partir des variables adimensionnées.
!> /!\ Les variables sont exprimées dans les unités SI (les constantes étant en SI)
!> Conversion :
!>    T_AD        ==>       T
!>    X_AD        ==>       RAYON
!>    OMEGA_AD    ==>       OMEGA
!>    P_AD        ==>       P
!>    P_GAZ_AD    ==>       P_GAZ
!>    P_RAD_AD    ==>       P_RAD
!>    C_S_AD      ==>       C_S
!>    H_AD        ==>       H
!>    RHO_AD      ==>       RHO
!>    NU_AD       ==>       NU
!>    S_AD        ==>       SIGMA
!>    V_AD        ==>       V
!>    M_DOT_AD    ==>       M_DOT
!>    TEMP_AD     ==>       TEMP
!>    Q_PLUS_AD   ==>       Q_PLUS
!>    Q_ADV_AD    ==>       Q_ADV
!>    C_V_AD      ==>       C_V
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE

    TIME     = TIME_AD / OMEGA_MAX
    RADIUS   = R_S * X_AD**2._xp
    OMEGA    = OMEGA_AD * OMEGA_MAX
    P        = P_AD * P_0
    P_GAZ    = P_GAZ_AD * P_GAZ_0
    P_RAD    = P_RAD_AD * P_RAD_0
    C_S      = C_S_AD * SPEED_0
    H        = H_AD * R_S
    RHO      = RHO_AD * RHO_0
    NU       = NU_AD * NU_0
    SIGMA    = S_AD / X_AD * S_0
    SPEED    = SPEED_AD * SPEED_0
    M_DOT    = M_DOT_AD * M_0_DOT
    TEMP     = TEMP_AD * TEMP_0
    Q_PLUS   = Q_PLUS_AD * Q_PLUS_0
    Q_MOINS  = Q_MOINS_AD * Q_PLUS_0
    Q_ADV    = Q_ADV_AD * Q_ADV_0
    C_V      = C_V_AD * C_V_0
    
!---------------------------------------------------------------------------------------------------
END SUBROUTINE ADIM_TO_PHYSIQUE
!---------------------------------------------------------------------------------------------------

SUBROUTINE PHYSIQUE_TO_ADIM()
!---------------------------------------------------------------------------------------------------
!> Cette routine donne une valeur aux variables adimensionnées à partir des variables physiques
!> /!\ Les variables initiales doivent être exprimées dans des unités SI 
!> (les constantes physiques étant en SI).
!> Conversion :
!>    T        ==>     T_AD
!>    RAYON    ==>     X_AD
!>    OMEGA    ==>     OMEGA_AD
!>    P        ==>     P_AD
!>    P_GAZ    ==>     P_GAZ_AD
!>    P_RAD    ==>     P_RAD_AD
!>    C_S      ==>     C_S_AD
!>    H        ==>     H_AD
!>    RHO      ==>     RHO_AD
!>    NU       ==>     NU_AD
!>    S        ==>     SIGMA_AD
!>    V        ==>     V_AD
!>    M_DOT    ==>     M_DOT_AD
!>    TEMP     ==>     TEMP_AD
!>    Q_PLUS   ==>     Q_PLUS_AD
!>    Q_ADV    ==>     Q_ADV_AD
!>    C_V      ==>     C_V_AD
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE

    TIME_AD     = TIME * OMEGA_MAX
    X_AD        = (RADIUS / R_S)**(0.5_xp)
    OMEGA_AD    = OMEGA / OMEGA_MAX
    P_AD        = P / P_0
    P_GAZ_AD    = P_GAZ_AD * P_GAZ_0
    P_RAD_AD    = P_RAD / P_RAD_0
    C_S_AD      = C_S / SPEED_0
    H_AD        = H / R_S
    RHO_AD      = RHO / RHO_0
    NU_AD       = NU / NU_0
    S_AD        = SIGMA / S_0 * X_AD
    SPEED_AD    = SPEED / SPEED_0
    M_DOT_AD    = M_DOT / M_0_DOT
    TEMP_AD     = TEMP / TEMP_0
    Q_PLUS_AD   = Q_PLUS / Q_PLUS_0
    Q_PLUS_AD   = Q_MOINS / Q_PLUS_0
    Q_ADV_AD    = Q_ADV / Q_ADV_0
    C_V_AD      = C_V / C_V_0
    
!---------------------------------------------------------------------------------------------------
END SUBROUTINE PHYSIQUE_TO_ADIM
!---------------------------------------------------------------------------------------------------

SUBROUTINE SI_TO_CGS()
!---------------------------------------------------------------------------------------------------
!> Cette routine convertit les variables physiques principales dans le système CGS
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE

    ! Longueurs & vitesses
    RADIUS = RADIUS * 1e2_xp
    H      = H * 1e2_xp
    C_S    = C_S * 1e2_xp
    SPEED  = SPEED * 1e2_xp
    
    ! Taux d'accrétion
    M_DOT = M_DOT * 1e3_xp

    ! Masse volumique
    RHO = RHO * 1e-3_xp

    ! Pressions
    P     = P * 1e1_xp
    P_RAD = P_RAD * 1e1_xp
    P_GAZ = P_GAZ * 1e1_xp

    ! Viscosité & Chaleur
    NU      = NU * 1e4_xp
    Q_PLUS  = Q_PLUS * 1e4_xp
    Q_MOINS = Q_MOINS * 1e4_xp
    Q_ADV   = Q_ADV * 1e4_xp
 
    ! Flux
    F_Z = F_Z * 1e3_xp

    ! Densité de surface
    SIGMA = SIGMA * 1e-1_xp
    
!---------------------------------------------------------------------------------------------------
END SUBROUTINE SI_TO_CGS
!---------------------------------------------------------------------------------------------------

SUBROUTINE CGS_TO_SI()
!---------------------------------------------------------------------------------------------------
!> Cette routine convertit les variables physiques principales dans le système SI
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE

    ! Longueurs & vitesses
    RADIUS = RADIUS * 1e-2_xp
    H      = H * 1e-2_xp
    C_S    = C_S * 1e-2_xp
    SPEED  = SPEED * 1e-2_xp
    
    ! Taux d'accrétion
    M_DOT = M_DOT * 1e-3_xp

    ! Masse volumique
    RHO = RHO * 1e3_xp

    ! Pressions
    P     = P * 1e-1_xp
    P_RAD = P_RAD * 1e-1_xp
    P_GAZ = P_GAZ * 1e-1_xp

    ! Viscosité et Chaleur
    NU      = NU * 1e-4_xp
    Q_PLUS  = Q_PLUS * 1e-4_xp
    Q_MOINS = Q_MOINS * 1e-4_xp
    Q_ADV   = Q_ADV * 1e-4_xp

    ! Flux
    F_Z = F_Z * 1e-3_xp

    ! Densité de surface
    SIGMA = SIGMA * 1e1_xp
    
!---------------------------------------------------------------------------------------------------
END SUBROUTINE CGS_TO_SI
!---------------------------------------------------------------------------------------------------

!===================================================================================================
END MODULE DIMENSIONNEMENT
!===================================================================================================
