!===================================================================================================
                            MODULE MODULE_SCHEMAS_T
!===================================================================================================
!> Ce module contient :
!> Le schéma pour le calcul de T
!===================================================================================================
    USE module_declarations

    IMPLICIT NONE

    REAL(kind=xp)             :: DELTA_T_TH_AD     !!Pas de temps pour l'intégration de T

    CONTAINS

    SUBROUTINE  SCHEMA_EULER(FONCTION, DFONCTION_DT, DTIME)
    !---------------------------------------------------------------------------------------------------
    !>    Cette routine prend en entrée les valeurs de FONCTION et de DFONCTION_DT (la dérivée 
    !>    temporelle de FONCTION)
    !>    pour calculer au pas de temps suivant DTIME.
    !---------------------------------------------------------------------------------------------------
        IMPLICIT NONE

        REAL(kind=xp), dimension(NX), intent(inout)     :: FONCTION
        REAL(kind=xp), dimension(NX), intent(in)        :: DFONCTION_DT
        REAL(kind=xp), intent(in)                       :: DTIME

        FONCTION = FONCTION + DFONCTION_DT * DTIME

    END SUBROUTINE SCHEMA_EULER

    SUBROUTINE ITERATION_TEMP_AD()
    !---------------------------------------------------------------------------------------------------
    !>    Cette routine itère sur un pas de temps thermique le tableau de température adimensionné TEMP_AD
    !---------------------------------------------------------------------------------------------------
        IMPLICIT NONE
        
        REAL(kind=xp), dimension(NX)                     :: DTEMP_AD_DT

        DTEMP_AD_DT = MU/(R_BOLTZ*OMEGA_MAX*TEMP_0*C_V_AD) * (Q_PLUS_0*Q_PLUS_AD - Q_PLUS_0*Q_MOINS_AD)
        CALL SCHEMA_EULER(TEMP_AD, DTEMP_AD_DT, DELTA_T_TH_AD)

    END SUBROUTINE ITERATION_TEMP_AD

                            END MODULE MODULE_SCHEMAS_T