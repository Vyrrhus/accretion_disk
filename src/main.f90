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
   WRITE(*,"(48('-'))")
   WRITE(*, "('Calcul des courbes en S')")
   CALL S_CURVE()
   WRITE(*, "('Calcul des points critiques')")
   CALL POINTS_CRITIQUES()
   Temp_min_AD=1.0e-1
   Temp_max_AD=1.0e1
   Sg_AD = 790._xp * 10 / S_0 * X_AD(10)
   CALL map_QpmQm(Temp_min_AD, Temp_max_AD, Sg_AD, 100)
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
WRITE(*,"(48('-'))")
WRITE(*, "('--------------DEBUT DE SIMULATION---------------')")

! ascension de la branche épaisse
CALL BOUCLE_BRANCHE_EPAISSE(0, 0.99_xp)

!approche du point critique
CALL BOUCLE_PARALLELE(1.0E-7_xp, 0, 0, 1.00_xp)
!suivi de la spirale
CALL BOUCLE_PARALLELE(5.0E-8_xp, 0, 0, 1.1_xp, 25.0_xp)
!branche mince et redescente
CALL BOUCLE_PARALLELE(1.0E-10_xp, 0, -1, 0.7_xp, 0.5_xp)
!ascension de la branche épaisse à nouveau
CALL BOUCLE_BRANCHE_EPAISSE(0, 0.99_xp)

WRITE(*,"(48('-'))")
WRITE(*, "('----------------FIN DE SIMULATION---------------')")

CALL CLOSE_OUTPUT()	
!========================================================================
                      END PROGRAM MAIN 
!========================================================================
