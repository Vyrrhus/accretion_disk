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

INTEGER :: I,J

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

! CONDITIONS PARTICULIERES POUR OBTENIR Q+ - Q- = 0
!RADIUS = 1.95143549e4_xp		! <=> X_AD    = 2.570185704218493_xp
!TEMP  = 9906635.9_xp
!SIGMA = 395.08_xp
!CALL PHYSIQUE_TO_ADIM()

 TEMP_AD = TEMP_AD_INI
 S_AD = S_AD_INI

!-- CALCUL L'ETAT DU DISQUE 
!-----------------------------------------------------------------------
CALL COMPUTE_EQS()
CALL ADIM_TO_PHYSIQUE()

PRINT*, 'Fz =      ', F_Z(30)
PRINT*, 'Q+ =      ', Q_PLUS(30) ! 1 J/s/m² = 10-7 erg/s/m² = 10e-11 erg/s/cm²
PRINT*, 'Q- =      ', Q_MOINS(30)
PRINT*, 'Q+ - Q- = ', (Q_PLUS(30)- Q_MOINS(30))

!-- BOUCLES DE CALCULS
CALL CREER_LAMBDA()
TIME_AD = 0.0_xp
OPEN(11,FILE='test.out',status='unknown')
CALL SCHEMA_FIRST()
CALL CLOSE_OUTPUT()
 close(11)
!========================================================================
END PROGRAM MAIN 
!========================================================================
