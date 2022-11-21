!------------------------------------------------------------------------
                                   PROGRAM MAIN 
!------------------------------------------------------------------------

USE MODULE_DECLARATIONS
USE MODULE_FUNCTION
USE MODULE_CONDITIONS_INITIALES

IMPLICIT NONE


!--- Appel des fonctions
CALL APPEL_PARAM_INPUT()
CALL CALCUL_CONSTANTES()
CALL CREATION_CONDITIONS_INITIALES()

!------------------------------------------------------------------------
                                 END PROGRAM MAIN 
!------------------------------------------------------------------------
