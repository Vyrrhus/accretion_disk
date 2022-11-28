!========================================================================
PROGRAM MAIN 
!========================================================================
USE MODULE_DECLARATIONS
USE MODULE_FUNCTION
USE MODULE_CONDITIONS_INITIALES
USE MODULE_ECRITURE

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

PRINT*, 'X = ', X_AD(30)
PRINT*, 'P_RAD = ', P_RAD_AD(30)
PRINT*, 'OMEGA = ', OMEGA_AD(30)
PRINT*, 'B = ', B_AD(30)
PRINT*, 'C = ', C_AD(30)
PRINT*, 'Ha = ', H_AD(30)
PRINT*, 'RHO = ', RHO_AD(30)
PRINT*, 'Pgaz = ', P_GAZ_AD(30)
PRINT*, 'Ptot = ', P_AD(30)
PRINT*, 'NU = ', NU_AD(30)
PRINT*, 'Kff = ', KAPPA_FF(30)
PRINT*, 'Fz = ', F_Z(30)
PRINT*, 'Q+ = ', Q_PLUS_AD(30)
PRINT*, 'Q- = ', Q_MOINS(30)

!-----------------------------------------------------------------------
!-- SORTIES ADIMENSIONNEES
!-----------------------------------------------------------------------
CALL ECRITURE_AD(101)

!========================================================================
END PROGRAM MAIN 
!========================================================================
