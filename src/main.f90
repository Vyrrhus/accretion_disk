!------------------------------------------------------------------------
                                   PROGRAM MAIN 
!------------------------------------------------------------------------

USE MODULE_DECLARATIONS
USE MODULE_FUNCTION

IMPLICIT NONE


!--- Appel des fonctions
CALL APPEL_PARAM_INPUT()
CALL CALCUL_CONSTANTES()

print*,x_ad

!------------------------------------------------------------------------
                                 END PROGRAM MAIN 
!------------------------------------------------------------------------
