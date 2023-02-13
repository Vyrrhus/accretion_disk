!===================================================================================================
            MODULE CONDITIONS_INITIALES
!===================================================================================================
!> Ce module contient :
!> - Les conditions initiales sur TEMP_AD, S_AD et H_AD.
!> - Une routine pour reprendre des valeurs existantes de TEMP et SIGMA comme conditions initiales.
!>   Pour cela, on doit commenter / décommenter les routines relatives aux conditions initiales dans
!>   le programme principal.
!>   Les valeurs initiales sont alors lues depuis ./config/conditions_initiales.config
!===================================================================================================

USE DECLARATIONS
USE EQUATIONS
USE ECRITURE
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
    REAL(KIND=xp), DIMENSION(NX)  :: TEMP_AD_INI, S_AD_INI, H_AD_INI  !! Tableaux initiaux de TEMP_AD, S_AD et H_AD
    REAL(KIND=xp), PARAMETER      :: M_0_DOT_FRAC = 0.01_xp           !! Fraction du M_0_DOT utilisé pour calculer les conditions initiales 
    REAL(KIND=xp), DIMENSION(NX)  :: F_RG                             !! Facteur correctif de relativité générale
    
    F_RG = 1.0_xp - (X_AD**2.0_xp / 3.0_xp)**(-1.0_xp / 2.0_xp)

    TEMP_AD_INI = 1.4E4_xp / TEMP_0 * ALPHA**(-1.0_xp / 5._xp) * F_RG**(3.0_xp / 10.0_xp)                    &
                * (M_0_DOT * M_0_DOT_FRAC / (1E13_xp))**(3.0_xp / 10.0_xp) * (MASS / M_O)**(1.0_xp / 4.0_xp) &
                * (R_S / 1.0E8_xp)**(-3._xp / 4._xp) * X_AD**(-3.0_xp / 2.0_xp)

    S_AD_INI    = 52.0_xp / S_0 * ALPHA**(-4.0_xp / 5.0_xp) * F_RG**(7.0_xp / 10.0_xp)                        &
                * (M_0_DOT * M_0_DOT_FRAC / (1E13_xp))**(7.0_xp / 10.0_xp) * (MASS / M_O)**(1.0_xp / 4.0_xp)  &
                * (R_S / 1E8)**(-3.0_xp / 4.0_xp) * X_AD**(-1.0_xp / 2.0_xp)

    H_AD_INI    = 1.7E-2_xp * ALPHA**(-1.0_xp / 10.0_xp) * F_RG**(3.0_xp / 5.0_xp)                             &
                * (M_0_DOT * M_0_DOT_FRAC / (1E13_xp))**(3.0_xp / 20.0_xp) * (MASS / M_O)**(-3.0_xp / 5.0_xp)  &
                * (R_S / 1E8_xp)**(1.0_xp / 8.0_xp)  * X_AD**(9.0_xp / 4.0_xp)
    
    WRITE(*, "('--------------CONDITIONS INITIALES--------------')")
    WRITE(*, "('------------------------------------------------')")
    WRITE(*, "('FRACTION DE M_0_DOT CHOISIE                    = ', 1pE12.2)") M_0_DOT_FRAC
    WRITE(*, "('T_AD MOYEN INITIALE                            = ', 1pE12.2)") SUM(TEMP_AD_INI) / NX
    WRITE(*, "('T MOYEN INITIALE                               = ', 1pE12.2)") SUM(TEMP_AD_INI) / NX * TEMP_0
    WRITE(*, "('S_AD MOYEN INITIALE                            = ', 1pE12.2)") SUM(S_AD_INI) / NX
    WRITE(*, "('SIGMA MOYEN INITIALE                           = ', 1pE12.2)") SUM(S_AD_INI / X_AD) / NX * SIGMA_0
    WRITE(*, "('H_AD MOYEN INITIALE                            = ', 1pE12.2)") SUM(H_AD_INI) / NX

    ! Variables adimensionnées
    TEMP_AD = TEMP_AD_INI
    S_AD    = S_AD_INI
    CALL COMPUTE_EQS()

!---------------------------------------------------------------------------------------------------
END SUBROUTINE CREATION_CONDITIONS_INITIALES
!---------------------------------------------------------------------------------------------------

SUBROUTINE REPRISE_CONDITIONS_INITIALES(PRECISION_REPRISE)
!---------------------------------------------------------------------------------------------------
!> Cette subroutine initialise TEMP_AD et S_AD à partir d'un fichier externe
!> /!\ Il faut s'assurer que le fichier externe a bien les mêmes paramètres de sortie :
!>     (NX, PAS_ECRITURE_SPATIAL, PRECISION) (cf output.config et MODULE_DECLARATIONS pour NX)
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE
    REAL(KIND=xp), DIMENSION(NX) :: TEMP_INI, S_INI  !! Tableaux initiaux de TEMP et SIGMA

    INTEGER, INTENT(IN) :: PRECISION_REPRISE   !! Nb de décimales après la virgule dans le fichier externe
    INTEGER             :: INPUT_UNT, IOS      !! Unit du fichier & Erreur de lecture
    CHARACTER(LEN=12)   :: HEADER              !! Nom de la variable
    CHARACTER(LEN=30)   :: FMT_PYT             !! Format

    ! Formats
    IF (MODULO(NX,PAS_ECRITURE_SPATIAL) == 1) THEN
        WRITE(FMT_PYT, "('(A11,X,', I0, '(1pE', I0, '.', I0, 'E3,2X))')") &
        & NX / PAS_ECRITURE_SPATIAL, PRECISION_REPRISE + 8, PRECISION_REPRISE
    ELSE
        WRITE(FMT_PYT, "('(A11,X,', I0, '(1pE', I0, '.', I0, 'E3,2X))')") &
        & NX / PAS_ECRITURE_SPATIAL+1, PRECISION_REPRISE + 8, PRECISION_REPRISE
    ENDIF
    
    ! Ouverture du fichier ; s'il n'existe pas, on crée les conditions initiales avec la subroutine précédente
    OPEN(NEWUNIT=INPUT_UNT, FILE="config/conditions_initiales.config", ACTION="read", STATUS="old", IOSTAT=IOS)
    IF (IOS /= 0) THEN
        PRINT*, "==========================================================="
        PRINT*, "Erreur lecture fichier : config/conditions_initiales.config"
        PRINT*, "Initialisation des conditions initiales"

        CALL CREATION_CONDITIONS_INITIALES()
    
    ! Lecture des conditions initiales
    ELSE
        READ(INPUT_UNT, FMT_PYT, IOSTAT=IOS) HEADER, S_INI
        READ(INPUT_UNT, FMT_PYT, IOSTAT=IOS) HEADER, TEMP_INI
        CLOSE(INPUT_UNT)

        ! Conversion TEMP, SIGMA => variables adim
        CALL ADIM_TO_PHYSIQUE()
        TEMP  = TEMP_INI
        SIGMA = S_INI
        CALL PHYSIQUE_TO_ADIM()
        CALL COMPUTE_EQS()

    ENDIF

!---------------------------------------------------------------------------------------------------
END SUBROUTINE REPRISE_CONDITIONS_INITIALES
!---------------------------------------------------------------------------------------------------

!===================================================================================================
END MODULE CONDITIONS_INITIALES
!===================================================================================================