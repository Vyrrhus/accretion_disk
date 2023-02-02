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
USE MODULE_CRITICAL_POINTS

IMPLICIT NONE
INTEGER :: FRAME_ITE
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
   CALL POINTS_CRITIQUES()
   Temp_min_AD=1.0e-1
   Temp_max_AD=1.0e1
   Sg_AD = 790._xp * 10 / S_0 * X_AD(10)
   CALL map_QpmQm(Temp_min_AD, Temp_max_AD, Sg_AD, 100)
ENDIF

CALL READ_CRITICAL_POINTS()

!---------------------------------
!-- CONDITIONS INITIALES
!---------------------------------
!CALL CREATION_CONDITIONS_INITIALES()
CALL REPRISE_CONDITIONS_INITIALES()

!----------------------------------
!-- BOUCLES DE CALCULS
!----------------------------------

TIME = 120.0_XP
TIME_AD = TIME*OMEGA_MAX
FRAME_ITE=1 
!CALL SCHEMA_FIRST_BRANCH(FRAME_ITE)
!CALL SCHEMA_TEST(1.0E-7_xp,2000)
!CALL SCHEMA_ADAPTE(1.0E-7_xp,10000,121.55_xp,FRAME_ITE)
!CALL SCHEMA_ADAPTE(1.0E-9_xp,5000,121.8_xp,FRAME_ITE)
!CALL SCHEMA_ADAPTE(1.0E-10_xp,10000,121.996_xp,FRAME_ITE)
CALL SCHEMA_ADAPTE(1.0E-9_XP,4000,122.05_XP,FRAME_ITE)
!CALL SCHEMA_ADAPTE(1.0E-8_XP,4000,122.20_XP,FRAME_ITE)


CALL CLOSE_OUTPUT()
			
!========================================================================
                      END PROGRAM MAIN 
!========================================================================
