!------------------------------------------------------------------------
                                   PROGRAM TEST_SCHEMA
!------------------------------------------------------------------------

USE MODULE_DECLARATIONS
USE MODULE_SCHEMAS_SIGMA
USE MODULE_FUNCTION
USE MODULE_CONDITIONS_INITIALES
USE MODULE_SCHEMAS_T

IMPLICIT NONE

REAL(KIND=xp), DIMENSION(NX) :: NU_TEST

INTEGER :: i_temps

!--- Appel des fonctions
CALL APPEL_PARAM_INPUT()
CALL CALCUL_CONSTANTES()
CALL CREATION_CONDITIONS_INITIALES()
TEMP_AD=TEMP_AD_INI
S_AD=S_AD_INI
CALL COMPUTE_EQS()
S_AD=S_AD_INI

NU_TEST=SUM(NU_AD)/NX

PRINT*, 'nu moyen', NU_TEST(1)
PRINT*, 'final expected value', 0.5_xp*(10.0_xp-SQRT(3.0_xp))/NU_TEST(1)
DELTA_T_VISQ=0.005*10000/(SUM(NU_AD)/NX)

CALL CREER_LAMBDA()

OPEN(1, file='test.out')
WRITE(1,*) X_AD
DO i_temps = 1,1000
    WRITE(1,*) S_AD
    CALL SCHEMA_IMPLICITE_S(NU_TEST)
END DO
CLOSE(1)

PRINT*, 'schema CN'


S_AD=S_AD_INI

OPEN(2, file='test_cn.out')
WRITE(2,*) X_AD
DO i_temps = 1,1000
    WRITE(2,*) S_AD
    CALL SCHEMA_CN_S(NU_TEST, 0.7_xp)
END DO
CLOSE(2)


PRINT *, 't_visq', 10000/(SUM(NU_AD)/NX)

PRINT *, 't_therm', 10000/((SUM(NU_AD)/NX))*(SUM(H_AD_INI)/NX/100)**2

PRINT *, 'R/H', 100/(SUM(H_AD_INI)/NX)

DELTA_T_TH=0.000001*10000/((SUM(NU_AD)/NX))*(SUM(H_AD_INI)/NX/100)**2

OPEN(3, file='test_temp.out')
WRITE(3,*) X_AD
DO i_temps = 1,10000
    WRITE(3,*) TEMP_AD
    CALL ITERATION_TEMP_AD
    CALL COMPUTE_EQS
END DO
CLOSE(3)

!------------------------------------------------------------------------
                                 END PROGRAM TEST_SCHEMA
!------------------------------------------------------------------------
