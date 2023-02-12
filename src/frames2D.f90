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

SUBROUTINE FRAME(VAR)
!---------------------------------------------------------------------------------------------------
!> Cette subroutine est appelée en mettant frame_cond = 1 en input.config
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE
    REAL(KIND=xp), INTENT(IN), DIMENSION(NX) :: VAR             !! Variable à afficher
    INTEGER, PARAMETER                       :: SIZE = 2 * NX   !! Taille de l'image
    REAL(KIND=xp), DIMENSION(SIZE,SIZE)      :: IMG             !! Tableau contenant l'image
    INTEGER                                  :: I,J,IND         !! Index pour parcourir le tableau
    
    CHARACTER(LEN=1024) :: FRAME_NAME   !! Path du fichier de sortie
    CHARACTER(LEN=1024) :: NUMB         !! Numéro de la frame
    CHARACTER(LEN=30)   :: FMT_FRAME    !! Format
    INTEGER             :: UNTY         !! Unit du fichier de sortie

    REAL(KIND=xp) :: NAN_VALUE  !! None
    
    ! Condition pour utiliser la subroutine
    IF (FRAME_COND /= 1) RETURN
    
    ! Fichier de sortie & format
    11 FORMAT(I0)
    WRITE(NUMB,11) FRAME_ID
    FRAME_NAME = 'frame_'//TRIM(NUMB)//'.out'
    WRITE(FMT_FRAME,"('(',I0,'(1pE12.4, 2X))')") SIZE

    ! NaN value
    NAN_VALUE = 0.0
    NAN_VALUE = 0.0 / NAN_VALUE

    ! Construction image : NaN en-dehors du disque de rayon NX
    DO I=1,SIZE
        DO J=1,SIZE
            IND = INT((ABS(I - NX)**2 + ABS(J - NX)**2)**0.5)
            IF (IND < NX) THEN
                IMG(I,J) = VAR(IND)
            ELSE 
                IMG(I,J) = NAN_VALUE
            ENDIF
        ENDDO
    ENDDO
    
    ! Ecriture dans le fichier de sortie
    OPEN(NEWUNIT=UNTY, FILE="frame_array/"//TRIM(ADJUSTL(FRAME_NAME)), ACTION='WRITE')
    DO I=1,SIZE
        WRITE(UNTY,FMT_FRAME) IMG(I,:)
    ENDDO
    
    CLOSE(UNTY)

!---------------------------------------------------------------------------------------------------
END SUBROUTINE FRAME
!---------------------------------------------------------------------------------------------------

!===================================================================================================
END MODULE FRAMES_2D
!===================================================================================================
