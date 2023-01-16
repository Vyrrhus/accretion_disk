!------------------------------------------------------------------------
                                   PROGRAM TEST_SCHEMA
!------------------------------------------------------------------------

USE MODULE_DECLARATIONS
USE MODULE_SCHEMAS_SIGMA
USE MODULE_FUNCTION
USE MODULE_CONDITIONS_INITIALES
USE MODULE_SCHEMAS_T
USE DIMENSIONNEMENT

IMPLICIT NONE

REAL(KIND=xp), DIMENSION(NX) :: NU_TEST

INTEGER :: i_temps
INTEGER :: j_temps
INTEGER :: j_check

!--- Appel des fonctions
CALL APPEL_PARAM_INPUT()
CALL CALCUL_CONSTANTES()
CALL CREATION_CONDITIONS_INITIALES()
TEMP_AD=TEMP_AD_INI*2
S_AD=S_AD_INI

DO j_check=1, 3
    TEMP_AD(j_check) = TEMP_AD(4)
    S_AD(j_check) = S_AD(4)
END DO

CALL COMPUTE_EQS()
S_AD=S_AD_INI*4

NU_TEST=SUM(NU_AD)/NX

PRINT*, 'nu moyen', NU_TEST(1)
PRINT*, 'final expected value', 0.5_xp*(10.0_xp-SQRT(3.0_xp))/NU_TEST(1)
DELTA_T_VISQ=0.005*10000/(SUM(NU_AD)/NX)

CALL CREER_LAMBDA()



PRINT *, 't_visq', 10000/(SUM(NU_AD)/NX)

PRINT *, 't_therm', 10000/((SUM(NU_AD)/NX))*(SUM(H_AD_INI)/NX/100)**2

PRINT *, 'R/H', 100/(SUM(H_AD_INI)/NX)

DELTA_T_TH_AD=0.001*10000/((SUM(NU_AD)/NX))*(SUM(H_AD_INI)/NX/100)**2


OPEN(1, file='test_sigma.out')
OPEN(3, file='test_temp.out')
OPEN(4, file='Qvalue.out')
OPEN(5, file='test_M_dot.out')
WRITE(1,*) X_AD
WRITE(3,*) X_AD
WRITE(5,*) X_AD
WRITE(4,*) X_AD

CALL ADIM_TO_PHYSIQUE
CALL SI_TO_CGS
WRITE(3,*) TEMP
WRITE(1,*) SIGMA
WRITE(4,*) Q_PLUS_0*(Q_PLUS_AD - Q_MOINS_AD)
WRITE(5,*) M_DOT

DO i_temps = 1,40

    j_temps = 0
    DO WHILE (maxval(ABS(Q_PLUS_0*Q_PLUS_AD - Q_PLUS_0*Q_MOINS_AD)) > 100)

        if(j_temps==0) Then
            DELTA_T_TH_AD = DELTA_T_TH_AD/1.0e2
        ELSE IF (j_temps==1) THEN
            DELTA_T_TH_AD = DELTA_T_TH_AD*1.0e2
        END IF

        ! IF(j_temps==1) THEN
        !     PRINT*, TAU_EFF(4), j_temps
        !     PRINT*, Q_MOINS_AD(1)
        ! END IF

        CALL ITERATION_TEMP_AD

        DO j_check = 1, NX
        !     IF(TAU_EFF(j_check)<=1) THEN
        !         TAU_EFF(j_check) = TAU_EFF(4)
        !         TEMP_AD(j_check) = TEMP_AD(4)
        !         S_AD(j_check) = S_AD(4)
        !         print*, Q_MOINS_AD(j_check), j_check, j_temps
        !     END IF

        !     IF(TEMP_AD(j_check)<=0) THEN
        !         TEMP_AD(j_check) = 1.0E6
        !         print*, Q_MOINS_AD(j_check), j_check, j_temps
        !     END IF
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
    CALL ADIM_TO_PHYSIQUE
    CALL SI_TO_CGS
    WRITE(3,*) TEMP
    WRITE(1,*) SIGMA
    WRITE(4,*) Q_PLUS_0*(Q_PLUS_AD - Q_MOINS_AD)
    WRITE(5,*) M_DOT

    CALL SCHEMA_IMPLICITE_S(NU_TEST)
    CALL COMPUTE_EQS

    CALL ADIM_TO_PHYSIQUE
    CALL SI_TO_CGS
    WRITE(3,*) TEMP
    WRITE(1,*) SIGMA
    WRITE(4,*) Q_PLUS_0*(Q_PLUS_AD - Q_MOINS_AD)
    WRITE(5,*) M_DOT

END DO
CLOSE(3)

!------------------------------------------------------------------------
                                 END PROGRAM TEST_SCHEMA
!------------------------------------------------------------------------
