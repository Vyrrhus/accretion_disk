!========================================================================
                 PROGRAM MAIN 
!========================================================================
!> Programme principal
!========================================================================
USE MODULE_DECLARATIONS
USE DIMENSIONNEMENT
USE MODULE_FUNCTION
USE MODULE_CONDITIONS_INITIALES
USE MODULE_ECRITURE
USE MODULE_SCHEMAS_SIGMA
USE MODULE_SCHEMAS_T
USE MODULE_BOUCLE
USE MODULE_DICHO
USE MODULE_FONCTIONS_UTILES
USE MODULE_S_CURVE

IMPLICIT NONE

!---------------------------------
!-- INITIALISATION
!---------------------------------
CALL APPEL_PARAM_INPUT()
CALL CALCUL_CONSTANTES()
CALL INIT_FILES()

!---------------------------------
!-- Calcul des courbes en S
!---------------------------------

IF (COURBE_EN_S == 1) THEN
   CALL S_curve(Temp_min_AD, Temp_max_AD, Sg_AD, Sd_AD, n_s)
ENDIF

!---------------------------------
!-- CONDITIONS INITIALES
!---------------------------------
CALL CREATION_CONDITIONS_INITIALES()
!CALL REPRISE_CONDITIONS_INITIALES()

!----------------------------------
!-- BOUCLES DE CALCULS
!----------------------------------
CALL SCHEMA_FIRST_BRANCH()

CALL SCHEMA_SECOND_BRANCH(1.0E-10_xp)
CALL CLOSE_OUTPUT()
			
!========================================================================
                      END PROGRAM MAIN 
!========================================================================
