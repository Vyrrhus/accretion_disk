                  MODULE MODULE_CRITICAL_POINTS
                  !> Module qui contient la lecture des points critiques
                  !> et une subroutine qui teste les courbes en S pour déterminer si on est arrivé au point critique
                  USE MODULE_DECLARATIONS
                  USE DIMENSIONNEMENT
                  
                  IMPLICIT NONE
                  
                  CONTAINS
                  
                  SUBROUTINE READ_CRITICAL_POINTS()
                  
                    INTEGER :: I,UNTC
                  
                    OPEN(NEWUNIT=UNTC,FILE='output/coord_turning_points.out',status='old',action='read')
                  
                    DO I=1,NX-2 !!!!a corriger plus tard
                             READ(UNTC,*) TEMP_CRITIQUE(I), SIGMA_CRITIQUE(I)
                    ENDDO
                    SIGMA_CRITIQUE=SIGMA_CRITIQUE*10.0_xp !conversion SI
                    TEMP_CRITIQUE(NX-1)=TEMP_CRITIQUE(NX-2)
                    TEMP_CRITIQUE(NX)=TEMP_CRITIQUE(NX-2)
                    SIGMA_CRITIQUE(NX-1)=SIGMA_CRITIQUE(NX-2)
                    SIGMA_CRITIQUE(NX)=SIGMA_CRITIQUE(NX-2)
                  END SUBROUTINE READ_CRITICAL_POINTS
                  
                  
                  
                  SUBROUTINE TURNING_POINT(TURN)
                  
                  IMPLICIT NONE
                  INTEGER :: I
                  LOGICAL,INTENT(INOUT) :: TURN
                  
                  TURN = .TRUE.
                  CALL ADIM_TO_PHYSIQUE()
                  
                  DO I=1,NX-2
                              IF ( SIGMA(I)<SIGMA_CRITIQUE(I)) THEN
                                   TURN = .FALSE.
                                   EXIT
                              ENDIF
                  ENDDO
                  
                  END SUBROUTINE TURNING_POINT
                  
                  END MODULE MODULE_CRITICAL_POINTS
