!------------------------------------------------------------------------
                                   PROGRAM MAIN 
!------------------------------------------------------------------------

USE MODULE_DECLARATIONS
USE MODULE_FUNCTION
!USE MODULE_CONDITIONS_INITIALES

IMPLICIT NONE


!--- Appel des fonctions
CALL APPEL_PARAM_INPUT()
CALL CALCUL_CONSTANTES()

CALL EQU_ALGEBRIQUES(TEMP_AD_INI,S_AD_INI,X_AD,NU_AD,V_AD,OMEGA_AD,RHO_AD,C_S_AD,M_DOT_AD&
&,P_AD,BETA,H_AD,F_Z,Q_PLUS_AD,Q_MOINS,TAU_EFF,KAPPA_FF,DIFF_FINIE)


PRINT*, 'F_Z             RHO_AD              H_AD                OMEGA_AD               TAU_EFF '
DO I=1,NX
PRINT*,F_Z(I),RHO_AD(I),H_AD(I),OMEGA_AD(I),TAU_EFF(I),KAPPA_FF(I)
ENDDO
!------------------------------------------------------------------------
                                 END PROGRAM MAIN 
!------------------------------------------------------------------------
