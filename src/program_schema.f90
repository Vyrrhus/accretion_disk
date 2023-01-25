!------------------------------------------------------------------------
                                   PROGRAM TEST_SCHEMA
!------------------------------------------------------------------------

USE MODULE_DECLARATIONS
USE MODULE_SCHEMAS_SIGMA
USE MODULE_FUNCTION
USE MODULE_CONDITIONS_INITIALES
USE MODULE_SCHEMAS_T
USE MODULE_S_CURVE
USE MODULE_SCHEMAS_INSTABILITE
USE DIMENSIONNEMENT

IMPLICIT NONE

REAL(KIND=xp), PARAMETER     :: FRACTION_DT_TH       = 1.0E-1_xp   !! Fraction du pas de temps thermique
REAL(KIND=XP), PARAMETER     :: FRACTION_DT_VISQ     = 1.0E-4_XP   !! Fraction du pas de temps visqueux
REAL(KIND=XP), PARAMETER     :: FRACTION_DT_INSTABLE = 1.0E-5*FRACTION_DT_VISQ   !! Fraction du pas de temps instable


REAL(kind=xp), DIMENSION(NX) :: S_AD_SAVE

INTEGER :: i_temps
INTEGER :: j_temps
INTEGER :: j_check

!--- Appel des fonctions
CALL APPEL_PARAM_INPUT()
CALL CALCUL_CONSTANTES()
CALL INIT_FILES

IF (COURBE_EN_S == 1) THEN
    CALL S_curve(Temp_min_AD, Temp_max_AD, Sg_AD, Sd_AD, n_s)
 ENDIF

CALL CREATION_CONDITIONS_INITIALES()

DO j_check=1, 3
    TEMP_AD(j_check) = TEMP_AD(4)
    S_AD(j_check) = S_AD(4)
END DO

CALL COMPUTE_EQS


DELTA_T_TH_AD = FRACTION_DT_TH / MAXVAL(OMEGA_AD)
DELTA_T_VISQ = FRACTION_DT_VISQ * MAXVAL( X_AD ** 4.0_xp / NU_AD ) 

CALL CREER_LAMBDA()



PRINT *, 't_visq', 10000/(SUM(NU_AD)/NX)

PRINT *, 't_therm', 10000/((SUM(NU_AD)/NX))*(SUM(H_AD)/NX/100)**2

PRINT *, 'R/H', 100/(SUM(H_AD)/NX)




CALL ADIM_TO_PHYSIQUE
CALL ECRITURE_DIM


PRINT*, '=========Début Schema Stable============'

DO i_temps = 1,1000

    j_temps = 0
    DO WHILE (maxval(ABS(Q_PLUS_0*Q_PLUS_AD - Q_PLUS_0*Q_MOINS_AD)) > 100)


        CALL ITERATION_TEMP_AD

        DO j_check = 1, NX
            IF(ISNAN(TEMP_AD(j_check))) THEN
                print*, 'NaN', j_check, j_temps
                TEMP_AD(j_check) = TEMP_AD(10)
                S_AD(j_check) = S_AD(10)
            END IF
        END DO
        CALL COMPUTE_EQS
        
        j_temps = j_temps + 1
        if (j_temps > 1000000) THEN
            exit
        END IF 
    END DO


    print*, i_temps, MAXVAL(MU/(R_BOLTZ*OMEGA_MAX*TEMP_0*C_V_AD) * Q_PLUS_0*(Q_PLUS_AD - Q_MOINS_AD)* &
                DELTA_T_TH_AD/TEMP_AD),&
                MAXVAL(Q_PLUS_0*(Q_PLUS_AD - Q_MOINS_AD)), j_temps

    TIME_AD = TIME_AD + j_temps*DELTA_T_TH_AD
    PRINT*, 'TIME =', TIME
    CALL ADIM_TO_PHYSIQUE
    CALL ECRITURE_DIM


    IF (TIME>80 .or. i_temps==1000) THEN
        PRINT*, 'Instabilité Q_ADV'
        exit
    END IF

    S_AD_SAVE = S_AD
    CALL SCHEMA_IMPLICITE_S(NU_AD)
    CALL COMPUTE_Q_ADV_AD(DELTA_T_VISQ, S_AD_SAVE)
    CALL COMPUTE_EQS
    TIME_AD = TIME_AD + (DELTA_T_VISQ - j_temps*DELTA_T_TH_AD)

    CALL ADIM_TO_PHYSIQUE
    CALL ECRITURE_DIM

    

END DO


PRINT*, '==========FIN SCHEMA STABLE============'


DELTA_T_INSTABLE_AD = FRACTION_DT_INSTABLE * MAXVAL( X_AD ** 4.0_xp / NU_AD ) 
CALL SETUP_SCHEMA_INSTABLE_TS(DELTA_T_INSTABLE_AD)
PRINT*, 'Initialisation : dt_instable =', DELTA_T_INSTABLE_AD

DO i_temps=1, 10**8
    CALL SCHEMA_INSTABLE_TS(0.8_xp)
    CALL COMPUTE_EQS
    TIME_AD = TIME_AD + DELTA_T_INSTABLE_AD
    CALL ADIM_TO_PHYSIQUE
    IF (modulo(i_temps, 10**5)==0) THEN
        CALL ECRITURE_DIM
        DELTA_T_INSTABLE_AD = FRACTION_DT_INSTABLE * MAXVAL( X_AD ** 4.0_xp / NU_AD ) 
        PRINT*, i_temps, 'dt_instable = ',DELTA_T_INSTABLE_AD
        CALL SETUP_SCHEMA_INSTABLE_TS(DELTA_T_INSTABLE_AD)
    END IF
    
END DO
PRINT*, '==========END============'

!------------------------------------------------------------------------
                                 END PROGRAM TEST_SCHEMA
!------------------------------------------------------------------------
