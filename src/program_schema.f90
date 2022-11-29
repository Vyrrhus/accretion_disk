!------------------------------------------------------------------------
                                   PROGRAM TEST_SCHEMA
!------------------------------------------------------------------------

USE MODULE_DECLARATIONS
USE MODULE_SCHEMAS_SIGMA
USE MODULE_FUNCTION
USE MODULE_CONDITIONS_INITIALES


IMPLICIT NONE

REAL(KIND=xp), DIMENSION(NX) :: NU_TEST

INTEGER :: i_temps

!--- Appel des fonctions
CALL APPEL_PARAM_INPUT()
CALL CALCUL_CONSTANTES()
CALL CREATION_CONDITIONS_INITIALES()

S_AD=S_AD_INI

NU_TEST=1.0_xp

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
!------------------------------------------------------------------------
                                 END PROGRAM TEST_SCHEMA
!------------------------------------------------------------------------
