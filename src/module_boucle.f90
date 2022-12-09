!---------------------------------------------------------------------------------------------------
                               MODULE MODULE_BOUCLE 
!---------------------------------------------------------------------------------------------------
USE MODULE_DECLARATIONS
USE DIMENSIONNEMENT
USE MODULE_FUNCTION
USE MODULE_SCHEMAS_SIGMA
USE MODULE_ECRITURE
USE MODULE_SCHEMAS_T
                               
                               IMPLICIT NONE

REAL(KIND=xp),PARAMETER, PRIVATE :: FRACTION_DT = 1.0E-2_xp
                                CONTAINS
                                
!---------------------------------------------------------------------------------------------------
SUBROUTINE SCHEMA_TH_TIME()

    IMPLICIT NONE
    
    REAL(KIND=XP) :: SWITCH
    
    
    SWITCH = 1.0E-3_xp
    DELTA_T_TH = FRACTION_DT / MAXVAL(OMEGA_AD)
    
    WRITE(*,"(40('-'))")
    WRITE(*,"('Q+ - Q- = ',1pe12.4)") MAXVAL(ABS(Q_PLUS_AD*Q_PLUS_0 - Q_MOINS)*1.0e-11_xp)
    
    DO WHILE(MAXVAL(ABS(Q_PLUS_AD*Q_PLUS_0 - Q_MOINS)*1.0e-11_xp) > SWITCH)
              
              CALL ITERATION_TEMP_AD()
              CALL COMPUTE_EQS()
              TIME_AD = TIME_AD + DELTA_T_TH 
              CALL ADIM_TO_PHYSIQUE()
              CALL ECRITURE_DIM()
              
    ENDDO
    
    WRITE(*,"('BOUCLE TEMPS THERMIQUE DONE')")
    WRITE(*,"(40('-'))")
    
END SUBROUTINE SCHEMA_TH_TIME
!---------------------------------------------------------------------------------------------------
SUBROUTINE SCHEMA_FIRST()

     IMPLICIT NONE
     INTEGER :: I
     
     DELTA_T_VISQ = FRACTION_DT * MAXVAL( X_AD ** 4.0_xp / NU_AD ) 
     CALL SCHEMA_TH_TIME()
     
     DO I=1,10
     
	     CALL SCHEMA_IMPLICITE_S(NU_AD)
	     CALL COMPUTE_EQS()
	     TIME_AD = TIME_AD + DELTA_T_VISQ
	     CALL SCHEMA_TH_TIME()
             
     ENDDO
     
END SUBROUTINE SCHEMA_FIRST
!---------------------------------------------------------------------------------------------------

!---------------------------------------------------------------------------------------------------
                          END MODULE MODULE_BOUCLE
!---------------------------------------------------------------------------------------------------
