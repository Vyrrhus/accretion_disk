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
! CALL CREATION_CONDITIONS_INITIALES()

! CONDITIONS PARTICULIERES POUR OBTENIR Q+ - Q- = 0
X_AD    = 2.570185704218493_xp
TEMP_AD = 9906635.9_xp / T_0
S_AD    = 395.08_xp / S_0 * X_AD

! TEMP_AD = TEMP_AD_INI
! S_AD = S_AD_INI

!-----------------------------------------------------------------------
!-- CALCUL L'ETAT DU DISQUE
!-----------------------------------------------------------------------
CALL COMPUTE_EQS()

PRINT*, 'X =       ', X_AD(30)
PRINT*, 'P_RAD =   ', P_RAD_AD(30) 
PRINT*, 'OMEGA =   ', OMEGA_AD(30) * OMEGA_MAX
PRINT*, 'B =       ', B_AD(30) * B_0
PRINT*, 'C =       ', C_AD(30) * C_0
PRINT*, 'Ha =      ', H_AD(30) * R_S
PRINT*, 'CS =      ', C_S_AD(30) * V_0
PRINT*, 'RHO =     ', RHO_AD(30) * RHO_0
PRINT*, 'NU =      ', NU_AD(30) * NU_0
PRINT*, 'v =       ', V_AD(30) * V_0
PRINT*, 'M_DOT =   ', M_DOT_AD(30) * M_0_DOT
PRINT*, 'Pgaz =    ', P_GAZ_AD(30) * P_GAZ_0
PRINT*, 'Ptot =    ', P_AD(30) * P_0
PRINT*, 'Kff =     ', KAPPA_FF(30)

PRINT*, 'TAU_EFF=  ', TAU_EFF(30)
PRINT*, 'Fz =      ', F_Z(30)
PRINT*, 'Q+ =      ', Q_PLUS_AD(30) * Q_PLUS_0 * 1e-11_xp  ! 1 J/s/m² = 10-7 erg/s/m² = 10e-11 erg/s/cm²
PRINT*, 'Q- =      ', Q_MOINS(30) * 1e-11_xp
PRINT*, 'Q+ - Q- = ', (Q_PLUS_AD(30) * Q_PLUS_0 - Q_MOINS(30)) * 1e-11_xp

!-----------------------------------------------------------------------
!-- SORTIES ADIMENSIONNEES
!-----------------------------------------------------------------------
CALL ECRITURE_AD(101)

!========================================================================
END PROGRAM MAIN 
!========================================================================
