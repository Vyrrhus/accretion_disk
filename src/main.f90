!========================================================================
                 PROGRAM MAIN 
!========================================================================
!> Programme principal
!========================================================================
USE MODULE_DECLARATIONS
USE MODULE_CONDITIONS_INITIALES
USE MODULE_ECRITURE
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
!-- BOUCLES DE CALCULS
!----------------------------------
CALL EVOLUTION_SYSTEM()
CALL CLOSE_OUTPUT()

!========================================================================
END PROGRAM MAIN 
!========================================================================
