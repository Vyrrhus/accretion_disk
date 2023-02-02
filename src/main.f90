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
   WRITE(*, "('Calcul des courbes en S')")
   CALL S_curve(Temp_min_AD, Temp_max_AD, Sg_AD, Sd_AD, n_s)
   WRITE(*, "('Calcul des points critiques')")
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
CALL CREATION_CONDITIONS_INITIALES()
!CALL REPRISE_CONDITIONS_INITIALES()

!----------------------------------
!-- BOUCLES DE CALCULS
!----------------------------------
FRAME_ITE=55
CALL BOUCLE_BRANCHE_EPAISSE()
!CALL BOUCLE_PARALLELE(1.0E-10_xp,7000)
CALL BOUCLE_TEMP_A_ATTEINDRE(1.0E-7_XP,5000,121.55_XP,FRAME_ITE)
CALL BOUCLE_TEMP_A_ATTEINDRE(1.0E-9_XP,5000,122.20_XP,FRAME_ITE)

CALL CLOSE_OUTPUT()
			
!========================================================================
                      END PROGRAM MAIN 
!========================================================================
