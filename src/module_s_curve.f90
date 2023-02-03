!===================================================================================================
            MODULE MODULE_S_CURVE
!===================================================================================================
!>   Ce module permet de calculer les courbes en S
!===================================================================================================

USE MODULE_DECLARATIONS
USE MODULE_DICHO
IMPLICIT NONE

CHARACTER(LEN=*), PRIVATE, PARAMETER :: FMT_SCURVE     = "(4(1pE19.12, X))"
CHARACTER(LEN=*),          PARAMETER :: FILENAME_EPAIS = "./output/scurve/epais.out"
CHARACTER(LEN=*),          PARAMETER :: FILENAME_MINCE = "./output/scurve/mince.out"

!===================================================================================================
            CONTAINS 
!===================================================================================================

SUBROUTINE S_CURVE()
!---------------------------------------------------------------------------------------------------
!> Subroutine de calcul des courbes en S
!> Pour chaque point spatial (total NX), on cherche les zéros de TEMP(SIGMA)
!> Comme on peut avoir plusieurs zéros associés à un même Sigma, on itère le long de la température
!> en discrétisant la température avec N_S points entre TEMP_MAX_AD et TEMP_MIN_AD
!> Pour trouver les zéros, on procède par dichotomie selon Sigma
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE
    REAL(KIND=xp), DIMENSION(N_S)  :: TEMP_EPAIS_AD  !! Tableau des températures pour la branche épaisse
    REAL(KIND=xp), DIMENSION(N_S)  :: TEMP_MINCE_AD  !! Tableau des températures pour la branche mince

    REAL(KIND=xp) :: Sa_AD, Sb_AD, Sc_AD   !! Points de gauche, droite et milieu pour la dicho
    
    LOGICAL  :: mince     !! Booléen pour changer de branche
    LOGICAL  :: ecriture  !! Booléen pour écrire ou non dans le fichier
    INTEGER  :: j, i
    INTEGER  :: UNT_MINCE, UNT_EPAIS

    !-----------------------------------------------------------------------------------------------
    ! Branche épaisse
    mince = .false.
    OPEN (newunit=UNT_EPAIS, file=FILENAME_EPAIS, status="unknown")

    DO j=1,NX
    
        ! 1ère température
        TEMP_EPAIS_AD(1) = TEMP_MIN_AD
        Sa_AD = Sg_AD
        Sb_AD = Sd_AD

        CALL DICHOTOMIE(TEMP_EPAIS_AD(1), Sa_AD, Sb_AD, j, mince, Sc_AD, ecriture)

        ! Si la dichotomie trouve un zéro, on écrit dans le fichier en [SI]
        IF (ecriture .eqv. .true.) THEN
            WRITE(UNT_EPAIS, FMT_SCURVE) TEMP_EPAIS_AD(1) * TEMP_0, Sc_AD * S_0 / X_AD(j), X_AD(j)**2._xp * R_S, X_AD(j)
        ENDIF

        ! Températures suivantes
        DO i=2,N_S

            ! Reset des bornes de la dichotomie (qui changent à chaque CALL DICHOTOMIE())
            Sa_AD = Sg_AD
            Sb_AD = Sd_AD

            ! Itération sur la température suivante
            TEMP_EPAIS_AD(i) = TEMP_EPAIS_AD(i-1) + (TEMP_MAX_AD - TEMP_MIN_AD) / N_S

            CALL DICHOTOMIE(TEMP_EPAIS_AD(i), Sa_AD, Sb_AD, j, mince, Sc_AD, ecriture)

            ! Si la dichotomie trouve un zéro, on écrit dans le fichier en [SI]
            IF (ecriture .eqv. .true.) THEN
                WRITE(UNT_EPAIS, FMT_SCURVE) TEMP_EPAIS_AD(i) * TEMP_0, Sc_AD * S_0 / X_AD(j), X_AD(j)**2._xp * R_S, X_AD(j)
            ENDIF

        ENDDO
    ENDDO

    CLOSE(UNT_EPAIS)

    !-----------------------------------------------------------------------------------------------
    ! Branche mince
    mince = .true.
    OPEN (newunit=UNT_MINCE, file=FILENAME_MINCE, status="unknown")

    !Bornes pour la dichotomie
    Sa_AD = Sg_AD
    Sb_AD = Sd_AD

    DO j=1,NX

        ! Calcul pour la 1ere température
        TEMP_MINCE_AD(1) = TEMP_MIN_AD

        CALL DICHOTOMIE(TEMP_MINCE_AD(1), Sa_AD, Sb_AD, j, mince, Sc_AD, ecriture)

        ! Si la dichotomie trouve un zéro, on écrit dans le fichier en [SI]
        IF (ecriture .eqv. .true.) THEN
            WRITE(UNT_MINCE, FMT_SCURVE) TEMP_MINCE_AD(1) * TEMP_0, Sc_AD * S_0 / X_AD(j), X_AD(j)**2._xp * R_S, X_AD(j)
        ENDIF

        ! Températures suivantes
        DO i=2,N_S

            ! Reset des bornes de la dichotomie (qui changent à chaque CALL DICHOTOMIE())
            Sa_AD = Sg_AD
            Sb_AD = Sd_AD

            ! Itération sur la température suivante
            TEMP_MINCE_AD(i) = TEMP_MINCE_AD(i-1) + ( TEMP_MAX_AD - TEMP_MIN_AD ) / N_S

            CALL DICHOTOMIE(TEMP_MINCE_AD(i), Sa_AD, Sb_AD, j, mince, Sc_AD, ecriture)

            ! Si la dichotomie trouve un zéro, on écrit dans le fichier en [SI]
            IF (ecriture .eqv. .true.) THEN
                WRITE(UNT_MINCE, FMT_SCURVE) TEMP_MINCE_AD(i) * TEMP_0, Sc_AD * S_0 / X_AD(j), X_AD(j)**2._xp * R_S, X_AD(j)
            ENDIF

        ENDDO

    ENDDO

    CLOSE(UNT_MINCE)

!---------------------------------------------------------------------------------------------------
END SUBROUTINE S_CURVE
!---------------------------------------------------------------------------------------------------

!===================================================================================================
END MODULE MODULE_S_CURVE
!===================================================================================================