!===================================================================================================
            MODULE MODULE_SCHEMAS_INSTABILITE
!===================================================================================================
!> Ce module contient :
!> Un schéma couplé solvant S et T en parallèle avec un pas de temps DELTA_T_INSTABLE_AD
!===================================================================================================
USE module_declarations
USE MODULE_SCHEMAS_SIGMA
USE MODULE_SCHEMAS_T

IMPLICIT NONE

!!REELS
REAL(KIND=xp) :: DELTA_T_INSTABLE_AD ! pas de temps instable

!===================================================================================================
            CONTAINS 
!===================================================================================================

SUBROUTINE SCHEMA_INSTABLE_TS(PARAM_CN_INSTABLE)
!---------------------------------------------------------------------------------------------------
!>    Cette routine actualise TEMP_AD et S_AD au pas de temps DELTA_T_INSTABLE_AD suivant
!>    en utilisant Q_adv pour T, et un schéma CN pour S de param PARAM_CN_INSTABLE
!>    Entrée: PARAM_CN_INSTABLE : valeur du param du CN de S
!>    /!\ penser a appeler SETUP_SCHEMA_INSTABLE_TS avant ! /!\
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE
    REAL(KIND=xp) :: PARAM_CN_INSTABLE
    REAL(KIND=xp), DIMENSION(NX) :: S_AD_TEMPSPRECEDENT !variable pour stocker S_AD au temps précédent pour Q_adv
    REAL(kind=xp), dimension(NX) :: DTEMP_AD_DT !

    S_AD_TEMPSPRECEDENT=S_AD !pour le calcul de Q_adv (derivees en temps de S)

    CALL SCHEMA_CN_S(NU_AD, PARAM_CN_INSTABLE)
    CALL COMPUTE_Q_ADV_AD(DELTA_T_INSTABLE_AD, S_AD_TEMPSPRECEDENT)

    !Calculs de la dérivée temporelle de TEMP_ADV
    DTEMP_AD_DT = MU/(R_BOLTZ*OMEGA_MAX*TEMP_0*C_V_AD) &
    &* (Q_PLUS_0*Q_PLUS_AD - Q_PLUS_0*Q_MOINS_AD + Q_ADV_0*Q_ADV_AD)

    CALL SCHEMA_EULER(TEMP_AD, DTEMP_AD_DT, DELTA_T_TH_AD)

END SUBROUTINE

SUBROUTINE SETUP_SCHEMA_INSTABLE_TS(DELTA_T_INSTABLE_AD_bis)
!---------------------------------------------------------------------------------------------------
!>    Cette routine prépare l'utilisation de SCHEMA_INSTABLE_TS :
!>    elle initialise les pas de temps a DELTA_T_INSTABLE_AD_bis et créer le vecteur Lambda pour les schémas en S
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE
    REAL(kind=xp) :: DELTA_T_INSTABLE_AD_bis

    DELTA_T_VISQ=DELTA_T_INSTABLE_AD_bis
    DELTA_T_TH_AD=DELTA_T_INSTABLE_AD_bis

    CALL CREER_LAMBDA

END SUBROUTINE

!===================================================================================================
END MODULE MODULE_SCHEMAS_INSTABILITE
!===================================================================================================

