!---------------------------------------------------------------------------------------------------
                               MODULE MODULE_BOUCLE 
!---------------------------------------------------------------------------------------------------
USE MODULE_DECLARATIONS
USE MODULE_DIMENSIONNEMENT
USE MODULE_FUNCTION
USE MODULE_SCHEMAS_SIGMA
USE MODULE_ECRITURE
USE MODULE_SCHEMAS_T
                               
                               IMPLICIT NONE

REAL(KIND=xp),PARAMETER, PRIVATE :: FRACTION_DT = 1.0E-4_xp
                                CONTAINS
                                
!---------------------------------------------------------------------------------------------------
SUBROUTINE SCHEMA_TH_TIME()

    IMPLICIT NONE
    
    REAL(KIND=XP) :: SWITCH
    
    DELTA_T_TH = FRACTION_DT / MAXVAL(OMEGA_AD)
    
    DO WHILE(MAXVAL(ABS(Q_PLUS_AD*Q_PLUS_0 - Q_MOINS)) > SWITCH)
         
              CALL ITERATION_TEMP_AD()
              TIME_AD = TIME_AD + DELTA_T_TH 
              CALL ADIM_TO_PHYSIQUE()
              CALL ECRITURE_DIM() 
              
    ENDDO

END SUBROUTINE SCHEMA_TH_TIME
!---------------------------------------------------------------------------------------------------
SUBROUTINE SCHEMA_VISQ_TIME()

     IMPLICIT NONE
     
     DELTA_T_VISQ = MAXVAL( X_AD ** 4.0_xp / NU_AD ) 
     
     CALL SCHEMA_IMPLICIT_S(NU_AD)
     TIME_AD = TIME_AD + DELTA_T_VISQ
     CALL SCHEMA_TH_TIME()
     
END SUBROUTINE SCHEMA_VISQ_TIME
!---------------------------------------------------------------------------------------------------

!---------------------------------------------------------------------------------------------------
                          END MODULE MODULE_BOUCLE
!---------------------------------------------------------------------------------------------------
