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
! CALL CREATION_CONDITIONS_INITIALES()

! CONDITIONS PARTICULIERES POUR OBTENIR Q+ - Q- = 0
RAYON = 1.95143549e4_xp		! <=> X_AD    = 2.570185704218493_xp
TEMP  = 9906635.9_xp
SIGMA = 395.08_xp
CALL PHYSIQUE_TO_ADIM()

! TEMP_AD = TEMP_AD_INI
! S_AD = S_AD_INI

!-- CALCUL L'ETAT DU DISQUE 
!-----------------------------------------------------------------------
CALL COMPUTE_EQS()
CALL ADIM_TO_PHYSIQUE()

PRINT*, 'R =       ', RAYON(30)
PRINT*, 'OMEGA =   ', OMEGA(30)
PRINT*, 'H =       ', H(30)
PRINT*, 'CS =      ', C_S(30)
PRINT*, 'RHO =     ', RHO(30)
PRINT*, 'NU =      ', NU(30)
PRINT*, 'v =       ', V(30)
PRINT*, 'M_DOT =   ', M_DOT(30)

PRINT*, 'X =       ', X_AD(30)
PRINT*, 'B =       ', B_AD(30) * B_0
PRINT*, 'C =       ', C_AD(30) * C_0
PRINT*, 'Prad =    ', P_RAD_AD(30) 
PRINT*, 'Pgaz =    ', P_GAZ_AD(30) * P_GAZ_0
PRINT*, 'Kff =     ', KAPPA_FF(30)
PRINT*, 'TAU_EFF=  ', TAU_EFF(30)
PRINT*, 'Fz =      ', F_Z(30)
PRINT*, 'Q+ =      ', Q_PLUS_AD(30) * Q_PLUS_0 * 1e-11_xp  ! 1 J/s/m² = 10-7 erg/s/m² = 10e-11 erg/s/cm²
PRINT*, 'Q- =      ', Q_MOINS(30) * 1e-11_xp
PRINT*, 'Q+ - Q- = ', (Q_PLUS_AD(30) * Q_PLUS_0 - Q_MOINS(30)) * 1e-11_xp


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
		T_AD = T_AD + REAL(J, KIND=XP) * DELTA_T_TH

		CALL ECRITURE_ADIM()
	
    ENDDO
ENDDO

CALL CLOSE_OUTPUT()

!========================================================================
END PROGRAM MAIN 
!========================================================================
