!===================================================================================================
                            MODULE MODULE_SCHEMAS_T
!===================================================================================================
!> Ce module contient :
!> Le schéma pour le calcul de T
!===================================================================================================
    USE MODULE_DECLARATIONS
    USE MODULE_SCHEMAS_SIGMA

    IMPLICIT NONE

    REAL(kind=xp)             :: DELTA_T_TH_AD     !!Pas de temps pour l'intégration de T sans advection
    REAL(kind=xp)             :: DELTA_T_AD        !!Pas de temps pour l'intégration de T et S avec l'advection
    REAL(kind=xp)             :: T_EXT_AD             !!Temperature de la matière arivant dans le disque

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
    !>    sans prendre en compte le terme d'advection.
    !---------------------------------------------------------------------------------------------------
        IMPLICIT NONE
        
        REAL(kind=xp), dimension(NX)                     :: DTEMP_AD_DT

        DTEMP_AD_DT = MU/(R_BOLTZ*OMEGA_MAX*TEMP_0*C_V_AD) * (Q_PLUS_0*Q_PLUS_AD - Q_PLUS_0*Q_MOINS_AD)
        CALL SCHEMA_EULER(TEMP_AD, DTEMP_AD_DT, DELTA_T_TH_AD)

    END SUBROUTINE ITERATION_TEMP_AD

    SUBROUTINE ITERATION_TEMP_SIGMA_AD_ADVECTION()
    !---------------------------------------------------------------------------------------------------
    !>    Cette routine itère sur un pas de temps thermique le tableau de température adimensionné TEMP_AD
    !>    en prenant en compte le therme d'advection, ansi 
    !---------------------------------------------------------------------------------------------------      
        IMPLICIT NONE

        REAL(kind=xp), dimension(NX)                     :: DTEMP_AD_DT
        REAL(kind=xp), dimension(NX)                     :: GAMMA_3
        REAL(kind=xp), dimension(NX)                     :: DS_AD_DT
        REAL(kind=xp), dimension(NX)                     :: DS_AD_DX
        REAL(kind=xp), dimension(NX)                     :: DTEMP_AD_DX
        REAL(kind=xp), dimension(NX)                     :: S_AD_SAVE

        INTEGER                                          :: i

        !Evolution sur un pas de temps pour S_AD
        S_AD_SAVE = S_AD
        CALL SCHEMA_IMPLICITE_S(NU_AD)

        !Calcul de Gamma_3
        GAMMA_3 = 1 + (4-3*BETA)/(BETA*C_V_0)

        !Dérivée temporelle de S_AD
        DS_AD_DT = (S_AD-S_AD_SAVE)/DELTA_T_AD

        !Dérivée spatiale de S_AD
        DO i = 2, NX
            DS_AD_DX(i) = (S_AD(i)-S_AD(i-1))/DX
        END DO
        DS_AD_DX(1)  = S_AD(1)/DX

        !Dérivée spatiale de TEMP_AD
        DO i = 2, NX-1
            IF(SPEED_AD(i)<=0) THEN
                DTEMP_AD_DX(i) = (TEMP_AD(i+1)-TEMP_AD(i))/DX
            ELSE 
                DTEMP_AD_DX(i) = (TEMP_AD(i)-TEMP_AD(i-1))/DX
            END IF
        END DO
        DTEMP_AD_DX(1)   = (TEMP_AD(2)-TEMP_AD(1))/DX
        DTEMP_AD_DX(NX)  = (T_EXT_AD-TEMP_AD(NX))/DX

        !Calculs de Q_ADV
        Q_ADV_AD = C_V_AD*((GAMMA_3-1) * TEMP_AD/S_AD *&
        & (DS_AD_DT + SPEED_AD/(2*X_AD)*DS_AD_DX - SPEED_AD/(2*X_AD**2)*S_AD)  &
        & - SPEED_AD/(2*X_AD)*DTEMP_AD_DX)
        
        !Calculs de la dérivée temporelle de TEMP_ADV
        DTEMP_AD_DT = MU/(R_BOLTZ*OMEGA_MAX*TEMP_0*C_V_AD) &
        &* (Q_PLUS_0*Q_PLUS_AD - Q_PLUS_0*Q_MOINS_AD + Q_ADV_0*Q_ADV_AD)

        CALL SCHEMA_EULER(TEMP_AD, DTEMP_AD_DT, DELTA_T_AD)


    END SUBROUTINE ITERATION_TEMP_SIGMA_AD_ADVECTION

                            END MODULE MODULE_SCHEMAS_T
