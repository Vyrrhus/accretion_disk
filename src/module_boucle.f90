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
REAL(KIND=XP), PARAMETER, PRIVATE :: FRACTION_DT_VISQ = 1.0_XP 
                                CONTAINS
                                
!---------------------------------------------------------------------------------------------------
SUBROUTINE SCHEMA_TH_TIME()

    IMPLICIT NONE
    
    REAL(KIND=XP) :: SWITCH
    
    
    SWITCH = 1.0e-17_xp
    DELTA_T_TH = FRACTION_DT_TH / MAXVAL(OMEGA_AD)
    
    WRITE(*,"(40('-'))")
    WRITE(*,"('Q+ - Q- = ',1pe12.4)") MAXVAL(ABS(Q_PLUS_AD - Q_MOINS_AD))
    
    DO WHILE(MAXVAL(ABS(Q_PLUS_AD - Q_MOINS_AD)) > SWITCH)
              
              WRITE(11,"(2(1pE20.7,2X))") TEMP_AD(30),Q_PLUS_AD(30)-Q_MOINS_AD(30)
              CALL ITERATION_TEMP_AD()
              CALL COMPUTE_EQS()
              CALL ECRITURE_ADIM()
              
    ENDDO
    
    
    WRITE(*,"('BOUCLE TEMPS THERMIQUE DONE')")
    WRITE(*,"(40('-'))")
    
END SUBROUTINE SCHEMA_TH_TIME
!---------------------------------------------------------------------------------------------------
SUBROUTINE SCHEMA_FIRST()

     IMPLICIT NONE
     
     DELTA_T_VISQ = FRACTION_DT_VISQ * MAXVAL( X_AD ** 4.0_xp / NU_AD ) 
     
     CALL CREER_LAMBDA()
     CALL SCHEMA_TH_TIME()
     
     DO WHILE(MINVAL(M_DOT_AD-1.0_xp)<=0.01_xp)
     
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
