!===================================================================================================
MODULE MODULE_CONDITIONS_INITIALES
!===================================================================================================
!> Ce module contient :
!> - Les conditions initiales sur T_AD, S_AD et H_AD.
!===================================================================================================

    USE MODULE_DECLARATIONS
    USE MODULE_FUNCTION
    USE MODULE_ECRITURE
    USE DIMENSIONNEMENT
    IMPLICIT NONE

!===================================================================================================
    CONTAINS
!===================================================================================================

SUBROUTINE CREATION_CONDITIONS_INITIALES()
!---------------------------------------------------------------------------------------------------
!> Cette subroutine initialise TEMP_AD, S_AD et H_AD puis toutes les variables adimensionnées
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE

    REAL(KIND=XP), DIMENSION(NX)   :: TEMP_AD_INI, S_AD_INI, H_AD_INI     !! Tableaux initiaux de TEMP_AD, S_AD et H_AD
    REAL(KIND=XP) :: M_0_DOT_FRAC = 0.01_XP       !! Fraction du M_0_DOT utilisé pour calculer les conditions initiales 
    REAL(KIND=XP), DIMENSION(NX)   :: F_RG        !! Facteur correctif de relativité générale
    
    F_RG = 1.0_XP - (X_AD**2.0_XP/3.0_XP)**(-1.0_XP/2.0_XP)

    TEMP_AD_INI = 1.4E4_XP / TEMP_0 * ALPHA**(-1.0_XP/5._XP)  * F_RG**(3.0_XP/10.0_XP)                   &
                * (M_0_DOT * M_0_DOT_FRAC / (1E13_XP))**(3.0_XP/10.0_XP) * (MASS/M_O)**(1.0_XP/4.0_XP)   &
                * (R_S/1.0E8_XP)**(-3._XP/4._XP) * X_AD**(-3.0_XP/2.0_XP) 

    S_AD_INI    = 52.0_XP / S_0 * ALPHA**(-4.0_XP/5.0_XP) * F_RG**(7.0_XP/10.0_XP)                    &
                * (M_0_DOT * M_0_DOT_FRAC / (1E13_XP))**(7.0_XP/10.0_XP) * (MASS/M_O)**(1.0_XP/4.0_XP)  &
                * (R_S / 1E8)**(-3.0_XP/4.0_XP) * X_AD**(-1.0_XP/2.0_XP)

    H_AD_INI    = 1.7E-2_XP   * ALPHA**(-1.0_XP/10.0_XP) * F_RG**(3.0_XP/5.0_XP)                   &
                * (M_0_DOT*M_0_DOT_FRAC/(1E13_XP))**(3.0_XP/20.0_XP) * (MASS/M_O)**(-3.0_XP/5.0_XP)  &
                * (R_S/1E8_XP)**(1.0_XP/8.0_XP)  * X_AD**(9.0_XP/4.0_XP)
    
    WRITE(*, "('--------------CONDITIONS INITIALES--------------')")
    WRITE(*, "('------------------------------------------------')")
    WRITE(*, "('FRACTION DE M_0_DOT CHOISIE                    = ', 1pE12.2)") M_0_DOT_FRAC
    WRITE(*, "('T_AD MOYEN INITIALE                            = ', 1pE12.2)") SUM(TEMP_AD_INI)/NX
    WRITE(*, "('T MOYEN INITIALE                               = ', 1pE12.2)") SUM(TEMP_AD_INI)/NX*TEMP_0
    WRITE(*, "('S_AD MOYEN INITIALE                            = ', 1pE12.2)") SUM(S_AD_INI)/NX
    WRITE(*, "('SIGMA MOYEN INITIALE                           = ', 1pE12.2)") SUM(S_AD_INI/X_AD)/NX*SIGMA_0
    WRITE(*, "('H_AD MOYEN INITIALE                            = ', 1pE12.2)") SUM(H_AD_INI)/NX

    ! Variables adimensionnées
    TEMP_AD = TEMP_AD_INI
    S_AD = S_AD_INI
    CALL COMPUTE_EQS()

!---------------------------------------------------------------------------------------------------
END SUBROUTINE CREATION_CONDITIONS_INITIALES
!---------------------------------------------------------------------------------------------------

SUBROUTINE REPRISE_CONDITIONS_INITIALES()
!---------------------------------------------------------------------------------------------------
!> Cette subroutine initialise TEMP_AD et S_AD à partir d'un fichier externe
!> /!\ Il faut s'assurer que le fichier externe a bien les mêmes paramètres de sortie :
!>     (NX, PAS_ECRITURE_SPATIAL, PRECISION) (cf output.config et MODULE_DECLARATIONS pour NX)
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE
    INTEGER :: INPUT_UNT 
    INTEGER :: IOS
    CHARACTER(LEN=12) :: HEADER
    REAL(KIND=XP), DIMENSION(NX) :: TEMP_INI, S_INI

    OPEN(NEWUNIT=INPUT_UNT, file="config/conditions_initiales.config", ACTION="read", STATUS="old", IOSTAT=IOS)
    IF (IOS /= 0) THEN
        PRINT*, "==========================================================="
        PRINT*, "Erreur lecture fichier : config/conditions_initiales.config"
        PRINT*, "Initialisation des conditions initiales"

        CALL CREATION_CONDITIONS_INITIALES()
    
    ELSE
        READ(INPUT_UNT, FMT, IOSTAT=IOS) HEADER, S_INI
        READ(INPUT_UNT, FMT, IOSTAT=IOS) HEADER, TEMP_INI
        CLOSE(INPUT_UNT)

        ! Variables adimensionnées
        CALL ADIM_TO_PHYSIQUE()
        TEMP = TEMP_INI
        SIGMA = S_INI
        CALL PHYSIQUE_TO_ADIM()
        CALL COMPUTE_EQS()

    ENDIF

!---------------------------------------------------------------------------------------------------
END SUBROUTINE REPRISE_CONDITIONS_INITIALES
!---------------------------------------------------------------------------------------------------

!===================================================================================================
END MODULE MODULE_CONDITIONS_INITIALES
!===================================================================================================
