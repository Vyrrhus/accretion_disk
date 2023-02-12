!===================================================================================================
            MODULE MODULE_SCHEMAS_INSTABILITE
!===================================================================================================
!> Ce module contient :
!> SCHEMA_INSTABLE_TS: Un schéma couplé solvant S et T en parallèle avec un pas de temps DELTA_T_INSTABLE_AD
!> SETUP_SCHEMA_INSTABLE_TS : Routine à appeler avant l'utilisation du schéma
!> COMPUTE_Q_ADV_AD: La routine du calcul de Q_adv
!===================================================================================================
USE MODULE_DECLARATIONS
USE MODULE_SCHEMAS_SIGMA
USE MODULE_SCHEMAS_T
IMPLICIT NONE

! REELS
REAL(KIND=xp) :: DELTA_T_INSTABLE_AD !! pas de temps instable

!===================================================================================================
            CONTAINS
!===================================================================================================

SUBROUTINE SCHEMA_INSTABLE_TS(PARAM_CN_INSTABLE)
!---------------------------------------------------------------------------------------------------
!> Cette routine actualise TEMP_AD et S_AD au pas de temps DELTA_T_INSTABLE_AD suivant
!> en prenant en compte Q_adv, et un schéma Crank-Nicolson pour S de param PARAM_CN_INSTABLE
!> /!\ penser a appeler SETUP_SCHEMA_INSTABLE_TS avant ! /!\
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE
    REAL(KIND=xp) :: PARAM_CN_INSTABLE
    REAL(KIND=xp), DIMENSION(NX) :: S_AD_TEMPS_PRECEDENT !! variable pour stocker S_AD au temps précédent pour Q_adv
    REAL(kind=xp), dimension(NX) :: DTEMP_AD_DT

    S_AD_TEMPS_PRECEDENT = S_AD !pour le calcul de Q_adv (derivees en temps de S)

    CALL SCHEMA_CN_S(NU_AD, PARAM_CN_INSTABLE)
    CALL COMPUTE_Q_ADV_AD(DELTA_T_INSTABLE_AD, S_AD_TEMPS_PRECEDENT)

    ! Calculs de la dérivée temporelle de TEMP_ADV
    DTEMP_AD_DT = MU / (R_BOLTZ * OMEGA_MAX * TEMP_0 * C_V_AD) &
    &           * (Q_PLUS_0 * Q_PLUS_AD - Q_PLUS_0 * Q_MOINS_AD + 1.0_xp * Q_ADV_0 * Q_ADV_AD)

    CALL SCHEMA_EULER(TEMP_AD, DTEMP_AD_DT, DELTA_T_TH_AD)

!---------------------------------------------------------------------------------------------------
END SUBROUTINE SCHEMA_INSTABLE_TS
!---------------------------------------------------------------------------------------------------

SUBROUTINE SETUP_SCHEMA_INSTABLE_TS()
!---------------------------------------------------------------------------------------------------
!> Cette routine prépare l'utilisation de SCHEMA_INSTABLE_TS :
!> elle initialise les pas de temps a DELTA_T_INSTABLE_AD_bis et créer le vecteur Lambda pour les schémas en S
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE

    DELTA_T_VISQ_AD = DELTA_T_INSTABLE_AD
    DELTA_T_TH_AD   = DELTA_T_INSTABLE_AD

    CALL CREER_LAMBDA()

!---------------------------------------------------------------------------------------------------
END SUBROUTINE SETUP_SCHEMA_INSTABLE_TS
!---------------------------------------------------------------------------------------------------

SUBROUTINE COMPUTE_Q_ADV_AD(DT, S_AD_SAVE)
!---------------------------------------------------------------------------------------------------
!> Cette routine calcul Q_ADV_AD à partir de l'ancienne valeur S_AD stocké dans S_AD_SAVE, 
!> représentant le tableau S_AD au pas de temps DT antérieur. 
!---------------------------------------------------------------------------------------------------  
    IMPLICIT NONE
    REAL(kind=xp),                intent(in)         :: DT
    REAL(kind=xp), dimension(NX), intent(in)         :: S_AD_SAVE
    REAL(kind=xp), dimension(NX)                     :: GAMMA_3
    REAL(kind=xp), dimension(NX)                     :: DS_AD_DT
    REAL(kind=xp), dimension(NX)                     :: DS_AD_DX
    REAL(kind=xp), dimension(NX)                     :: DTEMP_AD_DX

    INTEGER                                          :: i

    ! Calcul de Gamma_3
    GAMMA_3 = 1.0_xp + (4.0_xp - 3.0_xp * BETA) / (BETA * C_V_AD)

    ! Dérivée temporelle de S_AD
    DS_AD_DT = (S_AD - S_AD_SAVE) / DT

    ! Dérivée spatiale de S_AD
    DO i = 2, NX-1
        IF(SPEED_AD(i) <= 0) THEN
            DS_AD_DX(i) = (S_AD(i+1)-S_AD(i)) / DX
        ELSE 
            DS_AD_DX(i) = (S_AD(i)-S_AD(i-1)) / DX
        END IF
    END DO

    IF(SPEED_AD(1) <= 0) THEN
        DS_AD_DX(1) = (S_AD(2)-S_AD(1)) / DX
    ELSE 
        DS_AD_DX(1) = (S_AD(1)) / DX
    END IF

    DS_AD_DX(NX) = S_AD(NX)-S_AD(NX-1) / DX ! bord externe: on ne respecte pas la condition upwind mais pas d'influence

    ! Dérivée spatiale de TEMP_AD
    DO i = 2, NX-1
        IF(SPEED_AD(i) <= 0) THEN
            DTEMP_AD_DX(i) = (TEMP_AD(i+1)-TEMP_AD(i)) / DX
        ELSE 
            DTEMP_AD_DX(i) = (TEMP_AD(i)-TEMP_AD(i-1)) / DX
        END IF
    END DO

        
    DTEMP_AD_DX(1)  = (TEMP_AD(2)-TEMP_AD(1)) / DX      ! ok car v < 0 au bord interne

    DTEMP_AD_DX(NX) = (TEMP_AD(NX)-TEMP_AD(NX-1)) / DX

    ! Calculs de Q_ADV
    Q_ADV_AD = C_V_AD*((GAMMA_3 - 1.0_xp) * TEMP_AD / S_AD * (DS_AD_DT + SPEED_AD / (2.0_xp * X_AD) * DS_AD_DX   &
    &        - SPEED_AD / (2.0_xp * X_AD**2) * S_AD)                                                             &
    &        - SPEED_AD / (2.0_xp * X_AD) * DTEMP_AD_DX)

!---------------------------------------------------------------------------------------------------
END SUBROUTINE COMPUTE_Q_ADV_AD
!---------------------------------------------------------------------------------------------------

!===================================================================================================
END MODULE MODULE_SCHEMAS_INSTABILITE
!===================================================================================================