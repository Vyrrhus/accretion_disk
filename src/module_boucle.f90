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

REAL(KIND=xp), PARAMETER, PRIVATE :: FRACTION_DT_TH = 1.0E-2_xp
REAL(KIND=XP), PARAMETER, PRIVATE :: FRACTION_DT_VISQ = 1.0E-2_XP 
                                CONTAINS
                                
!---------------------------------------------------------------------------------------------------
SUBROUTINE SCHEMA_TH_TIME()

    IMPLICIT NONE
    
    REAL(KIND=XP) :: SWITCH
    
    
    SWITCH = 5.0e4_xp
    DELTA_T_TH = FRACTION_DT_TH / MAXVAL(OMEGA_AD)
    
    WRITE(*,"(40('-'))")
    WRITE(*,"('Q+ - Q- = ',1pe12.4)") MAXVAL(ABS(Q_PLUS - Q_MOINS))
    
    DO WHILE(MAXVAL(ABS(Q_PLUS - Q_MOINS)) > SWITCH)
              
              WRITE(11,"(2(1Pe15.7,2X)))") TEMP_AD(30),Q_PLUS(30) - Q_MOINS(30)
              CALL ITERATION_TEMP_AD()
              CALL COMPUTE_EQS()
              CALL ADIM_TO_PHYSIQUE()
              CALL SI_TO_CGS
              CALL ECRITURE_DIM()
              
    ENDDO
    
    WRITE(*,"('BOUCLE TEMPS THERMIQUE DONE')")
    WRITE(*,"(40('-'))")
    
END SUBROUTINE SCHEMA_TH_TIME
!---------------------------------------------------------------------------------------------------
SUBROUTINE SCHEMA_FIRST()

     IMPLICIT NONE
     INTEGER :: I
     
     DELTA_T_VISQ = FRACTION_DT_VISQ * MAXVAL( X_AD ** 4.0_xp / NU_AD ) 
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
