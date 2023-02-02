                  MODULE MODULE_CRITICAL_POINTS
                  !> Module qui contient la lecture des points critiques
                  !> et une subroutine qui teste les courbes en S pour déterminer si on est arriver au point critique
                  USE MODULE_DECLARATIONS
                  USE DIMENSIONNEMENT
                  
                  IMPLICIT NONE
                  
                  CONTAINS
                  
                  SUBROUTINE READ_CRITICAL_POINTS()
                  
                  INTEGER :: I,UNTC
                  
                  OPEN(NEWUNIT=UNTC,FILE='output/coord_turning_points.out',status='old',action='read')
                  
                  DO I=1,NX-2
                             READ(UNTC,*) TEMP_CRIT(I), S_CRIT(I)
                  ENDDO
                  
                  END SUBROUTINE READ_CRITICAL_POINTS
                  
                  
                  
                  SUBROUTINE TURNING_POINT(TURN)
                  
                  IMPLICIT NONE
                  INTEGER :: I
                  LOGICAL,INTENT(INOUT) :: TURN
                  
                  TURN = .TRUE.
                  CALL ADIM_TO_PHYSIQUE()
                  
                  DO I=1,NX-2
                              IF ( SIGMA(I)<S_CRIT(I)) THEN
                                   TURN = .FALSE.
                                   EXIT
                              ENDIF
                  ENDDO
                  
                  END SUBROUTINE TURNING_POINT
                  
                  END MODULE MODULE_CRITICAL_POINTS
