!========================================================================
PROGRAM MAIN 
!========================================================================

USE MODULE_DECLARATIONS
USE MODULE_FUNCTION
USE MODULE_CONDITIONS_INITIALES
USE MODULE_ECRITURE
USE MODULE_SCHEMAS_T
USE MODULE_SCHEMAS_SIGMA

IMPLICIT NONE

INTEGER :: I

!-----------------------------------------------------------------------
!-- INITIALISATION
!-----------------------------------------------------------------------
CALL APPEL_PARAM_INPUT()
CALL CALCUL_CONSTANTES()

!-----------------------------------------------------------------------
!-- CONDITIONS INITIALES
!-----------------------------------------------------------------------
CALL CREATION_CONDITIONS_INITIALES()

!TEMP_AD = 1.0E-2_XP
!S_AD = 1.0E+02_XP

TEMP_AD = TEMP_AD_INI
S_AD = S_AD_INI

!-----------------------------------------------------------------------
!-- CALCUL L'ETAT DU DISQUE 
!-----------------------------------------------------------------------
CALL COMPUTE_EQS()

!-------------------------------------------------
!-- INITIALISER VECTEUR DES SCHEMA EXPLICITES-----
CALL CREER_LAMBDA()

DO I=1,20
    
    
    !---APPEL DU SCHEMA NUMÉRIQUES
    CALL SCHEMA_IMPLICITE_S(NU_AD)
    CALL SCHEMA_EULER(TEMP_AD,
    
    !-- CALCUL ETAT SYSTÈME
    CALL COMPUTE_EQS()
    
    
    !-----------------------------------------------------------------------
    !-- SORTIES ADIMENSIONNEES
    !-----------------------------------------------------------------------
    CALL ECRITURE_AD_2(101)
    
ENDDO

!========================================================================
END PROGRAM MAIN 
!========================================================================
