!===================================================================================================
                            MODULE MODULE_SCHEMAS_T
!===================================================================================================
!> Ce module contient :
!> Le schéma pour le calcul de T
!===================================================================================================
    USE module_declarations

    IMPLICIT NONE

    REAL(kind=xp)             :: DELTA_T_TH     !!Pas de temps pour l'intégration de T

    CONTAINS

    SUBROUTINE  SCHEMA_EULER(T_AD, DT_AD)
    !---------------------------------------------------------------------------------------------------
    !>    Cette routine prend en entrée les valeurs de T_AD et de dT_AD (la dérivée temporelle de T_AD)
    !>    pour calculer au pas de temps thermique suivant T_AD.
    !---------------------------------------------------------------------------------------------------
        IMPLICIT NONE

        REAL(kind=xp), intent(inout)     :: T_AD
        REAL(kind=xp), intent(in)        :: DT_AD

        T_AD = T_AD + DT_AD*DELTA_T_TH

    END SUBROUTINE SCHEMA_EULER


                            END MODULE MODULE_SCHEMAS_T