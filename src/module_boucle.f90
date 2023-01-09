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

REAL(KIND=xp), PARAMETER, PRIVATE :: FRACTION_DT_TH     = 1.0E-3_xp
REAL(KIND=XP), PARAMETER, PRIVATE :: FRACTION_DT_VISQ   = 1.0E-3_XP 
                                CONTAINS
                                
!---------------------------------------------------------------------------------------------------
SUBROUTINE SCHEMA_TH_TIME()

    IMPLICIT NONE
    
    REAL(KIND=XP) :: SWITCH
    INTEGER :: I
    SWITCH = 1.0e-17_xp
    DELTA_T_TH_AD = FRACTION_DT_TH / MAXVAL(OMEGA_AD)
    
    WRITE(*,"('Q+ - Q- = ',1pe12.4,'          Temperature AD = ',1pE12.4)") &
    & MAXVAL(ABS(Q_PLUS_AD - Q_MOINS_AD)) , &
    & TEMP_AD(50)
     
    I=0
    DO WHILE(MAXVAL(ABS(Q_PLUS_AD - Q_MOINS_AD)) > SWITCH)
              
              CALL ITERATION_TEMP_AD()
              CALL COMPUTE_EQS()
              IF (MODULO(I,10000)==1) THEN
              WRITE (*,"('Q+-Q- = ',1pE12.4,'  M_DOT = ',1pE12.4)")&
              & MAXVAL(ABS(Q_PLUS_AD - Q_MOINS_AD)), &
              & ABS(MINVAL(M_DOT_AD-1.0_xp))
              ENDIF
              !CALL ADIM_TO_PHYSIQUE()
              CALL ECRITURE_ADIM()
              I=I+1
              
    ENDDO
    
END SUBROUTINE SCHEMA_TH_TIME
!---------------------------------------------------------------------------------------------------
SUBROUTINE SCHEMA_FIRST()

     IMPLICIT NONE
     
     INTEGER :: ITE
     DELTA_T_VISQ = FRACTION_DT_VISQ * MAXVAL( X_AD ** 4.0_xp / NU_AD ) 
     
     CALL CREER_LAMBDA()
     WRITE(*,"(48('-'))")
     WRITE(*,"('1e iteration de temps thermique')")
     CALL SCHEMA_TH_TIME()
     
     ITE=2
     DO WHILE(ABS(MINVAL(M_DOT_AD-1.0_xp))>=0.01_xp)
             
             WRITE(*,"(48('-'))")
             WRITE(*,"(I0,'e iteration de temps thermique ')") ITE 
	     CALL SCHEMA_IMPLICITE_S(NU_AD)
	     WRITE(*,"('S_AD(50) = ',1pE12.4)") S_AD(50)
	     CALL COMPUTE_EQS()
	     WRITE (11,"(2(1pE12.4,2x))") TEMP_AD(30),S_AD(30)
	     TIME_AD = TIME_AD + DELTA_T_VISQ
	     CALL SCHEMA_TH_TIME()
	     ITE=ITE+1
             
     ENDDO
     
END SUBROUTINE SCHEMA_FIRST
!---------------------------------------------------------------------------------------------------

!---------------------------------------------------------------------------------------------------
                          END MODULE MODULE_BOUCLE
!---------------------------------------------------------------------------------------------------
