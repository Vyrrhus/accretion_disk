                               MODULE MODUE_BOUCLE 
                               
USE MODULE_DECLARATIONS
USE MODULE_DIMENSIONNEMENT
USE MODULE_FUNCTION
USE MODULE_SCHEMAS_SIGMA
USE MODULE_ECRITURE
USE MODULE_SCHEMAS_T
                               
                               IMPLICIT NONE

                                CONTAINS
                                
!---------------------------------------------------------------------------------------------------
SUBROUTINE SCHEMA_GENERAL()

    IMPLICIT NONE
    
    CALL CREER_LAMBDA()
    
    DO 
    
         CALL SCHEMA_IMPLICITE_S(NU_AD)
         
         DO 
         
              CALL ITERATION_TEMP_AD()
              TIME_AD = TIM_AD !
              CALL ECRITURE_ADMIN() 
         ENDDO
         
    ENDDO
    
CALL CLOSE_OUTPUT()

END SUBROUTINE SCHEMA_GENERAL

                          END MODULE MODULE_BOUCLE
