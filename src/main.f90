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
real(kind=xp) :: t_cur

!-----------------------------------------------------------------------
!-- INITIALISATION
!-----------------------------------------------------------------------
CALL APPEL_PARAM_INPUT()
CALL CALCUL_CONSTANTES()

!-----------------------------------------------------------------------
!-- CONDITIONS INITIALES
!-----------------------------------------------------------------------
CALL CREATION_CONDITIONS_INITIALES()

TEMP_AD = TEMP_AD_INI
S_AD = S_AD_INI

!-- CALCUL L'ETAT DU DISQUE 
!-----------------------------------------------------------------------
CALL COMPUTE_EQS()

!-- INITIALISER VECTEUR DES SCHEMA EXPLICITES-----
CALL CREER_LAMBDA()

DELTA_T_TH=0.001*DELTA_T_VISQ

DO I=1,4
    
    
    !---APPEL DU SCHEMA NUMÉRIQUES
    CALL SCHEMA_IMPLICITE_S(NU_AD)
    
    DO J=1,1000
    
        CALL ITERATION_TEMP_AD()
        
	CALL COMPUTE_EQS()
	
	!--- SORTIE ADIMENSIONNEES
	IF (MOD(J,100)==1) THEN
	T_cur = REAL(I,KIND=XP)+REAL(J,KIND=XP)*DELTA_T_TH
	CALL ECRITURE_AD_2(101,T_cur)
	
	ENDIF
		
    ENDDO
    
ENDDO

!========================================================================
END PROGRAM MAIN 
!========================================================================
