!===================================================================================================
            MODULE FRAMES_2D
!===================================================================================================
!> Ce module contient les subroutines nécessaires pour produire les fichiers de sortie contenant
!> les tableaux 2D dans ./frame_array
!> Ces tableaux sont utilisés pour l'affichage en 2D par Python.
!===================================================================================================

USE MODULE_DECLARATIONS

IMPLICIT NONE

!===================================================================================================
            CONTAINS    
!===================================================================================================

SUBROUTINE FRAME(VAR,INDEX)
!---------------------------------------------------------------------------------------------------
!> Cette subroutine est appelée en mettant frame_cond = 1 en input.config
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE
    INTEGER, INTENT(IN)                       :: INDEX
    REAL(KIND=XP),INTENT(IN),DIMENSION(NX)    :: VAR
    INTEGER,PARAMETER                         :: SIZE = 2*NX
    INTEGER                                   :: CENTER,I,J,IND,UNTY
    REAL(KIND=XP),DIMENSION(SIZE,SIZE)        :: IMG 
    CHARACTER(LEN=1024)                       :: FRAME_NAME
    CHARACTER(LEN=1024)                       :: NUMB
    REAL(KIND=XP)                             :: NAN_VALUE 
    CHARACTER(LEN=30)                         :: FMT_frame
    
    ! Condition pour utiliser la subroutine
    IF (FRAME_COND /= 1) RETURN
    
    WRITE(FMT_frame,"('(',I0,'(1pE12.4, 2X))')") SIZE
    ! NaN value
    NAN_VALUE = 0.0
    NAN_VALUE = 0.0/NAN_VALUE

    ! Nom fichier de sortie
    11 FORMAT(I0)
    WRITE(NUMB,11) INDEX
    FRAME_NAME = 'frame_'//TRIM(NUMB)//'.out'

    ! Construction image : NaN en-dehors du disque
    CENTER = SIZE/2
    DO I=1,SIZE
        DO J=1,SIZE
            IND = INT((ABS(I - CENTER)**2 + ABS(J - CENTER)**2)**0.5)
            IF (IND<SIZE/2) THEN
                IMG(I,J) = VAR(IND)
            ELSE 
            
            IMG(I,J) = NAN_VALUE

            ENDIF
        ENDDO
    ENDDO
    
    ! écriture dans le fichier de sortie
    OPEN(NEWUNIT=UNTY,FILE="frame_array/"//TRIM(ADJUSTL(FRAME_NAME)),ACTION='WRITE')
    DO I=1,SIZE
        WRITE(UNTY,FMT_frame) IMG(I,:)
    ENDDO
    
    CLOSE(UNTY)

!---------------------------------------------------------------------------------------------------
END SUBROUTINE FRAME
!---------------------------------------------------------------------------------------------------

!===================================================================================================
            END MODULE FRAMES_2D
!===================================================================================================
