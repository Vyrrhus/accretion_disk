!========================================================================
                 PROGRAM MAIN 
!========================================================================
!> Programme principal
!========================================================================
USE DECLARATIONS
USE ECRITURE
USE CONDITIONS_INITIALES
USE BOUCLES
USE SCURVE_UTILS
USE SCURVE

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
   CALL S_CURVE()

   WRITE(*, "('Calcul des points critiques')")
   CALL POINTS_CRITIQUES()

   Temp_min_AD = 5.0e-2
   Temp_max_AD = 5.0e0
   Sg_AD       = 1.0e0
   Sd_AD       = 1.0e3
   CALL map_QpmQm(Temp_min_AD, Temp_max_AD, Sg_AD, Sd_AD, 100)
ENDIF

CALL LECTURE_POINTS_CRITIQUES()

!---------------------------------
!-- CONDITIONS INITIALES
!---------------------------------
CALL CREATION_CONDITIONS_INITIALES()
! CALL REPRISE_CONDITIONS_INITIALES(PRECISION)

!----------------------------------
!-- SIMULATION
!----------------------------------
CALL EVOLUTION_DISQUE()
CALL CLOSE_OUTPUT()

!========================================================================
END PROGRAM MAIN 
!========================================================================