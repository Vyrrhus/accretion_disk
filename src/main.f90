!========================================================================
PROGRAM MAIN 
!========================================================================
USE MODULE_DECLARATIONS
USE DIMENSIONNEMENT
USE MODULE_FUNCTION
USE MODULE_CONDITIONS_INITIALES
USE MODULE_ECRITURE
USE MODULE_SCHEMAS_SIGMA
USE MODULE_SCHEMAS_T
USE MODULE_BOUCLE

IMPLICIT NONE

!-----------------------------------------------------------------------
!-- INITIALISATION
!-----------------------------------------------------------------------
CALL APPEL_PARAM_INPUT()
CALL CALCUL_CONSTANTES()
CALL INIT_FILES()
!-----------------------------------------------------------------------
!-- CONDITIONS INITIALES
!-----------------------------------------------------------------------
CALL CREATION_CONDITIONS_INITIALES()

TEMP_AD = TEMP_AD_INI
S_AD = S_AD_INI

!-- CALCUL L'ETAT DU DISQUE 
!-----------------------------------------------------------------------
CALL COMPUTE_EQS()
CALL ADIM_TO_PHYSIQUE()

!-- BOUCLES DE CALCULS
TIME_AD = 0.0_xp
OPEN(22,FILE='test1.out',status='unknown')
OPEN(11,FILE='test.out',status='unknown')
CALL SCHEMA_FIRST()
CALL CLOSE_OUTPUT()
 close(11)
 close(22)
!========================================================================
END PROGRAM MAIN 
!========================================================================
