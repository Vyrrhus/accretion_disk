!========================================================================
PROGRAM MAIN 
!========================================================================
USE MODULE_DECLARATIONS
USE MODULE_FUNCTION
USE MODULE_CONDITIONS_INITIALES
USE MODULE_ECRITURE
USE MODULE_SCHEMAS_SIGMA
USE MODULE_SCHEMAS_T

IMPLICIT NONE

INTEGER :: I,J

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

DELTA_T_TH=0.001*DELTA_T_VISQ

DO I=1,20
    
    
    !---APPEL DU SCHEMA NUMÉRIQUES
    CALL SCHEMA_IMPLICITE_S(NU_AD)
    
    DO J=1,1000
        CALL ITERATION_TEMP_AD()
        
        CALL COMPUTE_EQS()
        IF (MOD(100,J)==1) THEN
        CALL ECRITURE_AD_2(101,I)
        ENDIF
    ENDDO
    
    
    !-----------------------------------------------------------------------
    !-- SORTIES ADIMENSIONNEES
    !-----------------------------------------------------------------------
    
    
ENDDO

!========================================================================
END PROGRAM MAIN 
!========================================================================
