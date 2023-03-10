!===================================================================================================
            MODULE ECRITURE
!===================================================================================================
!> Ce module écrit dans des fichiers .dat les tableaux de variables qu'on veut pouvoir étudier
!>  Mettre en argument le nombres de valeurs qu'on veut à chaque pas de temps (finissant par 01) et le temps t
!>      >>>> pour choisir les variables à écrire :
!>          >>> /config/wanted_variables
!>              >>>> mettre 1 pour écrire une variables et 0 pour l'inverse
!===================================================================================================
USE DECLARATIONS
IMPLICIT NONE

INTEGER          :: PAS_ECRITURE_SPATIAL   !! Pas d'écriture du fichier de sortie

INTEGER, PRIVATE           :: PRECISION           !! Nb de décimales après la virgule pour les variables écrites
INTEGER, PRIVATE           :: UNT, UNT_AD         !! Unit des fichiers de sortie
CHARACTER(LEN=30), PRIVATE :: FMT                 !! Format pour écrire un vecteur de dimension NX
CHARACTER(LEN=30), PRIVATE :: FMT_SINGLE_VALUE    !! Format pour écrire un float
INTEGER,PARAMETER, PRIVATE :: NB_VARIABLES = 22   !! Nombre de variables pouvant être écrites
INTEGER, PRIVATE           :: LIST(NB_VARIABLES)  !! Liste des variables
                 
!===================================================================================================
            CONTAINS 
!===================================================================================================

SUBROUTINE INIT_FILES()
!---------------------------------------------------------------------------------------------------
!> Subroutine qui initialise les fichiers de sortie du programme
!> Ceux-ci sont générés à partir des fichiers de config/.config
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE
    INTEGER :: OUTPUT_CONFIG_UNT, INPUT_CONFIG_UNT, ID_WANTED_VARIABLES 
    INTEGER :: IOS
    INTEGER :: IDX
    CHARACTER(LEN=1024) :: LINE
    CHARACTER(LEN=1024) :: FILENAME,FILENAME_2
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
        IF (INDEX(LINE, "@output_dim") /= 0) THEN
            IDX = INDEX(LINE, "=", back=.TRUE.)
            FILENAME = LINE(IDX+1:)
            CYCLE
        END IF
        
        IF (INDEX(LINE, "@output_ad") /=0) THEN
            IDX = INDEX(LINE, "=", back=.TRUE.)
            FILENAME_2 = LINE(IDX+1:)
            CYCLE
        END IF

        ! Pas d'écriture & précision
        PAS_ECRITURE_SPATIAL  = parse_int(LINE, "dx", PAS_ECRITURE_SPATIAL)
        PRECISION             = parse_int(LINE, "prec", PRECISION)
        
        ! Ajout du header au fichier de sortie
        IF (HEADER) THEN
            ! Affichage paramètres d'entrée
            IF (INDEX(LINE, "@input") /= 0) THEN
                OPEN(newunit=INPUT_CONFIG_UNT, file="config/input.config", action="read", status="old")
                DO 
                    READ(INPUT_CONFIG_UNT, "(A)", iostat=IOS) LINE
                    IF (IOS /= 0) EXIT 
                    WRITE(UNT, "(A)") LINE
                    WRITE(UNT_AD,"(A)") LINE
                END DO
                CLOSE(INPUT_CONFIG_UNT)

            ! Affichage constantes de simulation et de normalisation
            ELSE IF (INDEX(LINE, "@constantes") /=0) THEN
                CALL WRITE_CONSTANTES(UNT)
                CYCLE
            ELSE
                WRITE(UNT, "(A)") TRIM(LINE)
                WRITE(UNT_AD, "(A)") TRIM(LINE)
            END IF
        END IF

        IF (INDEX(LINE, "@header") /= 0) THEN
            HEADER = .TRUE.
            OPEN(newunit=UNT, file="output/"//TRIM(ADJUSTL(FILENAME)), action="write")
            OPEN(newunit=UNT_AD, file="output/"//TRIM(ADJUSTL(FILENAME_2)), action="write")
        END IF

    END DO

    CLOSE(OUTPUT_CONFIG_UNT)

    ! Formats
    IF (MODULO(NX,PAS_ECRITURE_SPATIAL) == 1) THEN
        WRITE(FMT, "('(A11,X,', I0, '(1pE', I0, '.', I0, 'E3,2X))')") NX / PAS_ECRITURE_SPATIAL, PRECISION + 8, PRECISION
    ELSE
        WRITE(FMT, "('(A11,X,', I0, '(1pE', I0, '.', I0, 'E3,2X))')") NX / PAS_ECRITURE_SPATIAL+1, PRECISION + 8, PRECISION
    ENDIF

    WRITE(FMT_SINGLE_VALUE, "('(A11,X,(1pE', I0, '.', I0, 'E3,2X))')") PRECISION + 8, PRECISION

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
        WRITE(UNT_AD, FMT_SINGLE_VALUE) "T         ", TIME_AD
        
        IF(MODULO(NX-1,PAS_ECRITURE_SPATIAL) == 0) THEN
        ! ESPACE
        WRITE(UNT_AD, FMT) "X_AD      ", X_AD(::PAS_ECRITURE_SPATIAL)

        ! VARIABLES
        IF (LIST(1)==1)  WRITE(UNT_AD, FMT) "OMEGA_AD  ", OMEGA_AD  (::PAS_ECRITURE_SPATIAL)
        IF (LIST(2)==1)  WRITE(UNT_AD, FMT) "P_AD      ", P_AD      (::PAS_ECRITURE_SPATIAL)
        IF (LIST(3)==1)  WRITE(UNT_AD, FMT) "BETA      ", BETA      (::PAS_ECRITURE_SPATIAL)
        IF (LIST(4)==1)  WRITE(UNT_AD, FMT) "C_S_AD    ", C_S_AD    (::PAS_ECRITURE_SPATIAL)
        IF (LIST(5)==1)  WRITE(UNT_AD, FMT) "H_AD      ", H_AD      (::PAS_ECRITURE_SPATIAL)
        IF (LIST(6)==1)  WRITE(UNT_AD, FMT) "RHO_AD    ", RHO_AD    (::PAS_ECRITURE_SPATIAL)
        IF (LIST(7)==1)  WRITE(UNT_AD, FMT) "NU_AD     ", NU_AD     (::PAS_ECRITURE_SPATIAL)
        IF (LIST(8)==1)  WRITE(UNT_AD, FMT) "S_AD      ", S_AD      (::PAS_ECRITURE_SPATIAL)
        IF (LIST(9)==1)  WRITE(UNT_AD, FMT) "SPEED_AD  ", SPEED_AD  (::PAS_ECRITURE_SPATIAL)
        IF (LIST(10)==1) WRITE(UNT_AD, FMT) "TEMP_AD   ", TEMP_AD   (::PAS_ECRITURE_SPATIAL)
        IF (LIST(11)==1) WRITE(UNT_AD, FMT) "M_DOT_AD  ", M_DOT_AD  (::PAS_ECRITURE_SPATIAL)
        IF (LIST(12)==1) WRITE(UNT_AD, FMT) "F_Z       ", F_Z       (::PAS_ECRITURE_SPATIAL)
        IF (LIST(13)==1) WRITE(UNT_AD, FMT) "P_GAZ_AD  ", P_GAZ_AD  (::PAS_ECRITURE_SPATIAL)
        IF (LIST(14)==1) WRITE(UNT_AD, FMT) "P_RAD_AD  ", P_RAD_AD  (::PAS_ECRITURE_SPATIAL)
        IF (LIST(15)==1) WRITE(UNT_AD, FMT) "Q_PLUS_AD ", Q_PLUS_AD (::PAS_ECRITURE_SPATIAL)
        IF (LIST(16)==1) WRITE(UNT_AD, FMT) "Q_ADV_AD  ", Q_ADV_AD  (::PAS_ECRITURE_SPATIAL)
        IF (LIST(17)==1) WRITE(UNT_AD, FMT) "Q_MOINS_AD", Q_MOINS_AD(::PAS_ECRITURE_SPATIAL)
        IF (LIST(18)==1) WRITE(UNT_AD, FMT) "TAU_EFF   ", TAU_EFF   (::PAS_ECRITURE_SPATIAL)
        IF (LIST(19)==1) WRITE(UNT_AD, FMT) "K_FF      ", KAPPA_FF  (::PAS_ECRITURE_SPATIAL)
        IF (LIST(20)==1) WRITE(UNT_AD, FMT) "E_FF      ", EPSILON_FF(::PAS_ECRITURE_SPATIAL)
        IF (LIST(21)==1) WRITE(UNT_AD, FMT) "C_V_AD    ", C_V_AD    (::PAS_ECRITURE_SPATIAL)
        IF (LIST(22)==1) WRITE(UNT_AD, FMT_SINGLE_VALUE) "LSTEFAN_AD", L_STEFAN

    ELSE
        ! ESPACE
        WRITE(UNT_AD, FMT) "X_AD      ", X_AD(::PAS_ECRITURE_SPATIAL), X_AD(NX)

        ! VARIABLES
        IF (LIST(1)==1)  WRITE(UNT_AD, FMT) "OMEGA_AD  ", OMEGA_AD  (::PAS_ECRITURE_SPATIAL), OMEGA_AD(NX)
        IF (LIST(2)==1)  WRITE(UNT_AD, FMT) "P_AD      ", P_AD      (::PAS_ECRITURE_SPATIAL), P_AD(NX)
        IF (LIST(3)==1)  WRITE(UNT_AD, FMT) "BETA      ", BETA      (::PAS_ECRITURE_SPATIAL), BETA(NX)
        IF (LIST(4)==1)  WRITE(UNT_AD, FMT) "C_S_AD    ", C_S_AD    (::PAS_ECRITURE_SPATIAL), C_S_AD(NX)
        IF (LIST(5)==1)  WRITE(UNT_AD, FMT) "H_AD      ", H_AD      (::PAS_ECRITURE_SPATIAL), H_AD(NX)
        IF (LIST(6)==1)  WRITE(UNT_AD, FMT) "RHO_AD    ", RHO_AD    (::PAS_ECRITURE_SPATIAL), RHO_AD(NX)
        IF (LIST(7)==1)  WRITE(UNT_AD, FMT) "NU_AD     ", NU_AD     (::PAS_ECRITURE_SPATIAL), NU_AD(NX)
        IF (LIST(8)==1)  WRITE(UNT_AD, FMT) "S_AD      ", S_AD      (::PAS_ECRITURE_SPATIAL), S_AD(NX)
        IF (LIST(9)==1)  WRITE(UNT_AD, FMT) "SPEED_AD  ", SPEED_AD  (::PAS_ECRITURE_SPATIAL), SPEED_AD(NX)
        IF (LIST(10)==1) WRITE(UNT_AD, FMT) "TEMP_AD   ", TEMP_AD   (::PAS_ECRITURE_SPATIAL), TEMP_AD(NX)
        IF (LIST(11)==1) WRITE(UNT_AD, FMT) "M_DOT_AD  ", M_DOT_AD  (::PAS_ECRITURE_SPATIAL), M_DOT_AD(NX)
        IF (LIST(12)==1) WRITE(UNT_AD, FMT) "F_Z       ", F_Z       (::PAS_ECRITURE_SPATIAL), F_Z(NX)
        IF (LIST(13)==1) WRITE(UNT_AD, FMT) "P_GAZ_AD  ", P_GAZ_AD  (::PAS_ECRITURE_SPATIAL), P_GAZ_AD(NX)
        IF (LIST(14)==1) WRITE(UNT_AD, FMT) "P_RAD_AD  ", P_RAD_AD  (::PAS_ECRITURE_SPATIAL), P_RAD_AD(NX)
        IF (LIST(15)==1) WRITE(UNT_AD, FMT) "Q_PLUS_AD ", Q_PLUS_AD (::PAS_ECRITURE_SPATIAL), Q_PLUS_AD(NX)
        IF (LIST(16)==1) WRITE(UNT_AD, FMT) "Q_ADV_AD  ", Q_ADV_AD  (::PAS_ECRITURE_SPATIAL), Q_ADV_AD(NX)
        IF (LIST(17)==1) WRITE(UNT_AD, FMT) "Q_MOINS_AD", Q_MOINS_AD(::PAS_ECRITURE_SPATIAL), Q_MOINS_AD(NX)
        IF (LIST(18)==1) WRITE(UNT_AD, FMT) "TAU_EFF   ", TAU_EFF   (::PAS_ECRITURE_SPATIAL), TAU_EFF(NX)
        IF (LIST(19)==1) WRITE(UNT_AD, FMT) "K_FF      ", KAPPA_FF  (::PAS_ECRITURE_SPATIAL), KAPPA_FF(NX)
        IF (LIST(20)==1) WRITE(UNT_AD, FMT) "E_FF      ", EPSILON_FF(::PAS_ECRITURE_SPATIAL), EPSILON_FF(NX)
        IF (LIST(21)==1) WRITE(UNT_AD, FMT) "C_V_AD    ", C_V_AD    (::PAS_ECRITURE_SPATIAL), C_V_AD(NX)
        IF (LIST(22)==1) WRITE(UNT_AD, FMT_SINGLE_VALUE) "L_STEFAN", L_STEFAN
    ENDIF
    WRITE(UNT_AD,*)

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
    WRITE(UNT, FMT_SINGLE_VALUE) "T         ", TIME
    
    IF(MODULO(NX-1,PAS_ECRITURE_SPATIAL) == 0) THEN

        ! ESPACE
        WRITE(UNT, FMT) "RADIUS    ", RADIUS(::PAS_ECRITURE_SPATIAL)

        ! VARIABLES
        IF (LIST(1)==1)  WRITE(UNT, FMT) "OMEGA     ", OMEGA     (::PAS_ECRITURE_SPATIAL)
        IF (LIST(2)==1)  WRITE(UNT, FMT) "P         ", P         (::PAS_ECRITURE_SPATIAL)
        IF (LIST(3)==1)  WRITE(UNT, FMT) "BETA      ", BETA      (::PAS_ECRITURE_SPATIAL)
        IF (LIST(4)==1)  WRITE(UNT, FMT) "C_S       ", C_S       (::PAS_ECRITURE_SPATIAL)
        IF (LIST(5)==1)  WRITE(UNT, FMT) "H         ", H         (::PAS_ECRITURE_SPATIAL)
        IF (LIST(6)==1)  WRITE(UNT, FMT) "RHO       ", RHO       (::PAS_ECRITURE_SPATIAL)
        IF (LIST(7)==1)  WRITE(UNT, FMT) "NU        ", NU        (::PAS_ECRITURE_SPATIAL)
        IF (LIST(8)==1)  WRITE(UNT, FMT) "SIGMA     ", SIGMA     (::PAS_ECRITURE_SPATIAL)
        IF (LIST(9)==1)  WRITE(UNT, FMT) "SPEED     ", SPEED     (::PAS_ECRITURE_SPATIAL)
        IF (LIST(10)==1) WRITE(UNT, FMT) "TEMP      ", TEMP      (::PAS_ECRITURE_SPATIAL)
        IF (LIST(11)==1) WRITE(UNT, FMT) "M_DOT     ", M_DOT     (::PAS_ECRITURE_SPATIAL)
        IF (LIST(12)==1) WRITE(UNT, FMT) "F_Z       ", F_Z       (::PAS_ECRITURE_SPATIAL)
        IF (LIST(13)==1) WRITE(UNT, FMT) "P_GAZ     ", P_GAZ     (::PAS_ECRITURE_SPATIAL)
        IF (LIST(14)==1) WRITE(UNT, FMT) "P_RAD     ", P_RAD     (::PAS_ECRITURE_SPATIAL)
        IF (LIST(15)==1) WRITE(UNT, FMT) "Q_PLUS    ", Q_PLUS    (::PAS_ECRITURE_SPATIAL)
        IF (LIST(16)==1) WRITE(UNT, FMT) "Q_ADV     ", Q_ADV     (::PAS_ECRITURE_SPATIAL)
        IF (LIST(17)==1) WRITE(UNT, FMT) "Q_MOINS   ", Q_MOINS   (::PAS_ECRITURE_SPATIAL)
        IF (LIST(18)==1) WRITE(UNT, FMT) "TAU_EFF   ", TAU_EFF   (::PAS_ECRITURE_SPATIAL)
        IF (LIST(19)==1) WRITE(UNT, FMT) "K_FF      ", KAPPA_FF  (::PAS_ECRITURE_SPATIAL)
        IF (LIST(20)==1) WRITE(UNT, FMT) "E_FF      ", EPSILON_FF(::PAS_ECRITURE_SPATIAL)
        IF (LIST(21)==1) WRITE(UNT, FMT) "C_V       ", C_V       (::PAS_ECRITURE_SPATIAL)
        IF (LIST(22)==1) WRITE(UNT, FMT_SINGLE_VALUE) "L_STEFAN  ", L_STEFAN
    
    ELSE
        ! ESPACE
        WRITE(UNT, FMT) "RADIUS    ", RADIUS(::PAS_ECRITURE_SPATIAL), RADIUS(NX)

        ! VARIABLES
        IF (LIST(1)==1)  WRITE(UNT, FMT) "OMEGA     ", OMEGA     (::PAS_ECRITURE_SPATIAL), OMEGA(NX)
        IF (LIST(2)==1)  WRITE(UNT, FMT) "P         ", P         (::PAS_ECRITURE_SPATIAL), P(NX)
        IF (LIST(3)==1)  WRITE(UNT, FMT) "BETA      ", BETA      (::PAS_ECRITURE_SPATIAL), BETA(NX)
        IF (LIST(4)==1)  WRITE(UNT, FMT) "C_S       ", C_S       (::PAS_ECRITURE_SPATIAL), C_S(NX)
        IF (LIST(5)==1)  WRITE(UNT, FMT) "H         ", H         (::PAS_ECRITURE_SPATIAL), H(NX)
        IF (LIST(6)==1)  WRITE(UNT, FMT) "RHO       ", RHO       (::PAS_ECRITURE_SPATIAL), RHO(NX)
        IF (LIST(7)==1)  WRITE(UNT, FMT) "NU        ", NU        (::PAS_ECRITURE_SPATIAL), NU(NX)
        IF (LIST(8)==1)  WRITE(UNT, FMT) "SIGMA     ", SIGMA     (::PAS_ECRITURE_SPATIAL), SIGMA(NX)
        IF (LIST(9)==1)  WRITE(UNT, FMT) "SPEED     ", SPEED     (::PAS_ECRITURE_SPATIAL), SPEED(NX)
        IF (LIST(10)==1) WRITE(UNT, FMT) "TEMP      ", TEMP      (::PAS_ECRITURE_SPATIAL), TEMP(NX)
        IF (LIST(11)==1) WRITE(UNT, FMT) "M_DOT     ", M_DOT     (::PAS_ECRITURE_SPATIAL), M_DOT(NX)
        IF (LIST(12)==1) WRITE(UNT, FMT) "F_Z       ", F_Z       (::PAS_ECRITURE_SPATIAL), F_Z(NX)
        IF (LIST(13)==1) WRITE(UNT, FMT) "P_GAZ     ", P_GAZ     (::PAS_ECRITURE_SPATIAL), P_GAZ(NX)
        IF (LIST(14)==1) WRITE(UNT, FMT) "P_RAD     ", P_RAD     (::PAS_ECRITURE_SPATIAL), P_RAD(NX)
        IF (LIST(15)==1) WRITE(UNT, FMT) "Q_PLUS    ", Q_PLUS    (::PAS_ECRITURE_SPATIAL), Q_PLUS(NX)
        IF (LIST(16)==1) WRITE(UNT, FMT) "Q_ADV     ", Q_ADV     (::PAS_ECRITURE_SPATIAL), Q_ADV(NX)
        IF (LIST(17)==1) WRITE(UNT, FMT) "Q_MOINS   ", Q_MOINS   (::PAS_ECRITURE_SPATIAL), Q_MOINS(NX)
        IF (LIST(18)==1) WRITE(UNT, FMT) "TAU_EFF   ", TAU_EFF   (::PAS_ECRITURE_SPATIAL), TAU_EFF(NX)
        IF (LIST(19)==1) WRITE(UNT, FMT) "K_FF      ", KAPPA_FF  (::PAS_ECRITURE_SPATIAL), KAPPA_FF(NX)
        IF (LIST(20)==1) WRITE(UNT, FMT) "E_FF      ", EPSILON_FF(::PAS_ECRITURE_SPATIAL), EPSILON_FF(NX)
        IF (LIST(21)==1) WRITE(UNT, FMT) "C_V       ", C_V       (::PAS_ECRITURE_SPATIAL), C_V(NX)
        IF (LIST(22)==1) WRITE(UNT, FMT_SINGLE_VALUE) "L_STEFAN  ", L_STEFAN
    
    ENDIF
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
    CLOSE(UNT_AD)
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

!===================================================================================================
END MODULE ECRITURE
!===================================================================================================
