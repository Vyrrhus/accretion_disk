!===================================================================================================
            MODULE MODULE_ECRITURE
!===================================================================================================
!> Ce module écrit dans des fichiers .dat les tableaux de variables qu'on veut pouvoir étudier
!>  Mettre en argument le nombres de valeurs qu'on veut à chaque pas de temps (finissant par 01) et le temps t
!>      >>>> pour choisir les variables à écrire :
!>          >>> /config/wanted_variables
!>              >>>> mettre 1 pour écrire une variables et 0 pour l'inverse
!===================================================================================================
USE MODULE_DECLARATIONS
IMPLICIT NONE

INTEGER, PRIVATE :: UNT
INTEGER, PRIVATE :: PAS_ECRITURE_SPATIAL
INTEGER, PRIVATE :: PAS_ECRITURE_TEMPOREL
INTEGER, PRIVATE :: PRECISION

INTEGER,PARAMETER, PRIVATE :: NB_VARIABLES = 20
INTEGER, PRIVATE :: LIST(NB_VARIABLES)
                 
!===================================================================================================
            CONTAINS 
!===================================================================================================

SUBROUTINE INIT_FILES()
!---------------------------------------------------------------------------------------------------
!> Subroutine qui initialise les fichiers de sortie du programme
!> Ceux-ci sont générés à partir des fichiers de config/*.config
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE
    INTEGER :: OUTPUT_CONFIG_UNT, INPUT_CONFIG_UNT, ID_WANTED_VARIABLES
    INTEGER :: IOS
    INTEGER :: IDX
    CHARACTER(LEN=1024) :: LINE
    CHARACTER(LEN=1024) :: FILENAME
    LOGICAL :: HEADER = .FALSE.

    ! LECTURE VARIABLES A AFFICHER EN SORTIE
    NAMELIST /WANTED_VARIABLES/ LIST
    OPEN(NEWUNIT=ID_WANTED_VARIABLES,FILE='config/wanted_variables.txt',ACTION='read',STATUS='old')
    READ(ID_WANTED_VARIABLES,WANTED_VARIABLES)
    CLOSE(ID_WANTED_VARIABLES)

    ! LECTURE DES PARAMETRES DU HEADER
    OPEN(newunit=OUTPUT_CONFIG_UNT, file="config/output.config", action="read", status="old")
    DO
        ! Lecture ligne par ligne et sortie à la fin du fichier
        READ(OUTPUT_CONFIG_UNT, "(A)", iostat=IOS) LINE
        IF (IOS /= 0) EXIT 

        ! Fichier de sortie
        IF (INDEX(LINE, "@output") /= 0) THEN
            IDX = INDEX(LINE, "=", back=.TRUE.)
            FILENAME = LINE(IDX+1:)
            CYCLE
        END IF

        ! Pas d'écriture & précision
        PAS_ECRITURE_SPATIAL  = parse_int(LINE, "dx", PAS_ECRITURE_SPATIAL)
        PAS_ECRITURE_TEMPOREL = parse_int(LINE, "dt", PAS_ECRITURE_TEMPOREL)
        PRECISION             = parse_int(LINE, "prec", PRECISION)

        ! Ajout du header au fichier de sortie
        IF (HEADER) THEN
            IF (INDEX(LINE, "@input") /= 0) THEN
                OPEN(newunit=INPUT_CONFIG_UNT, file="config/input.config", action="read", status="old")
                DO 
                    READ(INPUT_CONFIG_UNT, "(A)", iostat=IOS) LINE
                    IF (IOS /= 0) EXIT 
                    WRITE(UNT, "(A)") LINE
                END DO
                CLOSE(INPUT_CONFIG_UNT)
            ELSE 
                WRITE(UNT, "(A)") TRIM(LINE)
            END IF
        END IF

        IF (INDEX(LINE, "@header") /= 0) THEN
            HEADER = .TRUE.
            OPEN(newunit=UNT, file="output/"//TRIM(ADJUSTL(FILENAME)), action="write")
        END IF

    END DO

    CLOSE(OUTPUT_CONFIG_UNT)

    ! Format
    WRITE(FMT, "('(A,', I0, '(1pE', I0, '.', I0, ',2X))')") NX / PAS_ECRITURE_SPATIAL, PRECISION + 7, PRECISION

!---------------------------------------------------------------------------------------------------
END SUBROUTINE INIT_FILES
!---------------------------------------------------------------------------------------------------

SUBROUTINE ECRITURE_ADIM()
!---------------------------------------------------------------------------------------------------
!> Subroutine qui écrit les variables en sortie, au format :
!> [temps] x.xxxxx
!> [var1] x.xxxxx x.xxxxx x.xxxxx...
!> [var2] x.xxxxx x.xxxxx x.xxxxx...
!> [...]
!>
!> Les variables sont sans dimension lorsque c'est possible
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE
     
    ! TEMPS
    IF (MODULO(II_TIME, PAS_ECRITURE_TEMPOREL) /= 0) RETURN
    WRITE(UNT, "('# T   =   ',1PE11.4)") T_AD

    ! ESPACE
    WRITE(UNT, FMT) "X_AD      ", X_AD(::PAS_ECRITURE_SPATIAL)

    ! VARIABLES
    IF (LIST(1)==1)  WRITE(UNT, FMT) "OMEGA_AD  ", OMEGA_AD(::PAS_ECRITURE_SPATIAL)
    IF (LIST(2)==1)  WRITE(UNT, FMT) "P_AD      ", P_AD(::PAS_ECRITURE_SPATIAL)
    IF (LIST(3)==1)  WRITE(UNT, FMT) "BETA      ", BETA(::PAS_ECRITURE_SPATIAL)
    IF (LIST(4)==1)  WRITE(UNT, FMT) "C_S_AD    ", C_S_AD(::PAS_ECRITURE_SPATIAL)
    IF (LIST(5)==1)  WRITE(UNT, FMT) "H_AD      ", H_AD(::PAS_ECRITURE_SPATIAL)
    IF (LIST(6)==1)  WRITE(UNT, FMT) "RHO_AD    ", RHO_AD(::PAS_ECRITURE_SPATIAL)
    IF (LIST(7)==1)  WRITE(UNT, FMT) "NU_AD     ", NU_AD(::PAS_ECRITURE_SPATIAL)
    IF (LIST(8)==1)  WRITE(UNT, FMT) "S_AD      ", S_AD(::PAS_ECRITURE_SPATIAL)
    IF (LIST(9)==1)  WRITE(UNT, FMT) "V_AD      ", V_AD(::PAS_ECRITURE_SPATIAL)
    IF (LIST(10)==1) WRITE(UNT, FMT) "TEMP_AD   ", TEMP_AD(::PAS_ECRITURE_SPATIAL)
    IF (LIST(11)==1) WRITE(UNT, FMT) "M_DOT_AD  ", M_DOT_AD(::PAS_ECRITURE_SPATIAL)
    IF (LIST(12)==1) WRITE(UNT, FMT) "F_Z_AD    ", F_Z_AD(::PAS_ECRITURE_SPATIAL)
    IF (LIST(13)==1) WRITE(UNT, FMT) "P_GAZ_AD  ", P_GAZ_AD(::PAS_ECRITURE_SPATIAL)
    IF (LIST(14)==1) WRITE(UNT, FMT) "P_RAD_AD  ", P_RAD_AD(::PAS_ECRITURE_SPATIAL)
    IF (LIST(15)==1) WRITE(UNT, FMT) "Q_PLUS_AD ", Q_PLUS_AD(::PAS_ECRITURE_SPATIAL)
    IF (LIST(16)==1) WRITE(UNT, FMT) "Q_ADV_AD  ", Q_ADV_AD(::PAS_ECRITURE_SPATIAL)
    IF (LIST(17)==1) WRITE(UNT, FMT) "Q_MOINS   ", Q_MOINS(::PAS_ECRITURE_SPATIAL)
    IF (LIST(18)==1) WRITE(UNT, FMT) "TAU_EFF   ", TAU_EFF(::PAS_ECRITURE_SPATIAL)
    IF (LIST(19)==1) WRITE(UNT, FMT) "K_FF      ", KAPPA_FF(::PAS_ECRITURE_SPATIAL)
    IF (LIST(20)==1) WRITE(UNT, FMT) "E_FF      ", EPSILON_FF(::PAS_ECRITURE_SPATIAL)
 
    WRITE(UNT,*)

!---------------------------------------------------------------------------------------------------
END SUBROUTINE ECRITURE_ADIM
!---------------------------------------------------------------------------------------------------

SUBROUTINE ECRITURE_DIM()
!---------------------------------------------------------------------------------------------------
!> Subroutine qui écrit les variables en sortie, au format :
!> [temps] x.xxxxx
!> [var1] x.xxxxx x.xxxxx x.xxxxx...
!> [var2] x.xxxxx x.xxxxx x.xxxxx...
!> [...]
!>
!> Les variables sont dimensionnés (unités SI)
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE
     
    ! TEMPS
    IF (MODULO(II_TIME, PAS_ECRITURE_TEMPOREL) /= 0) RETURN
    WRITE(UNT, "('# T   =   ',1PE11.4)") T

    ! ESPACE
    WRITE(UNT, FMT) "X_AD      ", RAYON(::PAS_ECRITURE_SPATIAL)

    ! VARIABLES
    IF (LIST(1)==1)  WRITE(UNT, FMT) "OMEGA     ", OMEGA   (::PAS_ECRITURE_SPATIAL)
    IF (LIST(2)==1)  WRITE(UNT, FMT) "P         ", P   (::PAS_ECRITURE_SPATIAL)
    IF (LIST(3)==1)  WRITE(UNT, FMT) "BETA      ", BETA(::PAS_ECRITURE_SPATIAL)
    IF (LIST(4)==1)  WRITE(UNT, FMT) "C_S       ", C_S   (::PAS_ECRITURE_SPATIAL)
    IF (LIST(5)==1)  WRITE(UNT, FMT) "H         ", H   (::PAS_ECRITURE_SPATIAL)
    IF (LIST(6)==1)  WRITE(UNT, FMT) "RHO       ", RHO   (::PAS_ECRITURE_SPATIAL)
    IF (LIST(7)==1)  WRITE(UNT, FMT) "NU        ", NU   (::PAS_ECRITURE_SPATIAL)
    IF (LIST(8)==1)  WRITE(UNT, FMT) "SIGMA     ", SIGMA   (::PAS_ECRITURE_SPATIAL)
    IF (LIST(9)==1)  WRITE(UNT, FMT) "V         ", V   (::PAS_ECRITURE_SPATIAL)
    IF (LIST(10)==1) WRITE(UNT, FMT) "TEMP      ", TEMP   (::PAS_ECRITURE_SPATIAL)
    IF (LIST(11)==1) WRITE(UNT, FMT) "M_DOT     ", M_DOT   (::PAS_ECRITURE_SPATIAL)
    IF (LIST(12)==1) WRITE(UNT, FMT) "F_Z       ", F_Z   (::PAS_ECRITURE_SPATIAL)
    IF (LIST(13)==1) WRITE(UNT, FMT) "P_GAZ     ", P_GAZ   (::PAS_ECRITURE_SPATIAL)
    IF (LIST(14)==1) WRITE(UNT, FMT) "P_RAD     ", P_RAD   (::PAS_ECRITURE_SPATIAL)
    IF (LIST(15)==1) WRITE(UNT, FMT) "Q_PLUS    ", Q_PLUS   (::PAS_ECRITURE_SPATIAL)
    IF (LIST(16)==1) WRITE(UNT, FMT) "Q_ADV     ", Q_ADV   (::PAS_ECRITURE_SPATIAL)
    IF (LIST(17)==1) WRITE(UNT, FMT) "Q_MOINS   ", Q_MOINS(::PAS_ECRITURE_SPATIAL)
    IF (LIST(18)==1) WRITE(UNT, FMT) "TAU_EFF   ", TAU_EFF(::PAS_ECRITURE_SPATIAL)
    IF (LIST(19)==1) WRITE(UNT, FMT) "K_FF      ", KAPPA_FF(::PAS_ECRITURE_SPATIAL)
    IF (LIST(20)==1) WRITE(UNT, FMT) "E_FF      ", EPSILON_FF(::PAS_ECRITURE_SPATIAL)
 
    WRITE(UNT,*)

!---------------------------------------------------------------------------------------------------
END SUBROUTINE ECRITURE_DIM
!---------------------------------------------------------------------------------------------------

SUBROUTINE CLOSE_OUTPUT()
!---------------------------------------------------------------------------------------------------
!> Subroutine qui ferme le fichier de sortie généré par le programme.
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE

    CLOSE(UNT)
!---------------------------------------------------------------------------------------------------
END SUBROUTINE CLOSE_OUTPUT
!---------------------------------------------------------------------------------------------------

FUNCTION parse_int(STRING, KEY, CURRENT_VALUE) result(VALUE)
!---------------------------------------------------------------------------------------------------
!> Parseur d'une ligne pour déterminer la valeur d'un integer
!> La ligne (STRING) est de la forme:
!> (A) = value
!>       avec (KEY) dans (STRING)
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE

    CHARACTER(LEN=*), INTENT(IN) :: STRING, KEY
    CHARACTER(LEN=1024) :: SUBSTRING
    INTEGER :: VALUE, CURRENT_VALUE
    INTEGER :: IDX
    IF (INDEX(STRING, TRIM(KEY)) /= 0) THEN
        IDX = INDEX(STRING, "=", back=.TRUE.)
        SUBSTRING = STRING(IDX+1:)
        READ(SUBSTRING, *) VALUE
    ELSE 
        VALUE = CURRENT_VALUE
    END IF

!---------------------------------------------------------------------------------------------------
END FUNCTION
!---------------------------------------------------------------------------------------------------

!--------------------------------------------------------------------------------------------------
                                  END MODULE MODULE_ECRITURE
!--------------------------------------------------------------------------------------------------
