!------------------------------------------------------------------------
                                   PROGRAM TEST_SCHEMA
!------------------------------------------------------------------------

USE MODULE_DECLARATIONS
USE DIMENSIONNEMENT
USE MODULE_FUNCTION
USE MODULE_CONDITIONS_INITIALES
USE MODULE_ECRITURE
USE MODULE_SCHEMAS_SIGMA
USE MODULE_SCHEMAS_T
USE MODULE_BOUCLE
USE module_dicho

IMPLICIT NONE

INTEGER :: iterateur
REAL(KIND=xp), DIMENSION(NX) :: NU_AD_TEST
REAL(KIND=xp) :: FRACTION_DT_VISQ

!-----------------------------------------------------------------------
!-- INITIALISATION
!-----------------------------------------------------------------------
CALL APPEL_PARAM_INPUT()
CALL CALCUL_CONSTANTES()
CALL INIT_FILES()
!-----------------------------------------------------------------------
!-- CONDITIONS INITIALES
!-----------------------------------------------------------------------

CALL CREATION_CONDITIONS_INITIALES()

CALL COMPUTE_EQS

DO iterateur=1,NX
    S_AD(iterateur)=COS(REAL(iterateur)*6.0_xp/REAL(NX))*60000.0_xp
END DO

NU_AD_TEST = NU_AD(50)

FRACTION_DT_VISQ = 1.0E-5_XP

DELTA_T_VISQ = FRACTION_DT_VISQ * MAXVAL( X_AD ** 4.0_xp / NU_AD(50) )

PRINT*, 'Valeur de nu_ad test', NU_AD_TEST(50)

P_AD=S_AD
P_RAD_AD=S_AD
P_GAZ_AD=S_AD
BETA=0.0_xp
CALL ECRITURE_ADIM

CALL CREER_LAMBDA

DO iterateur=1,1000
    P_AD=S_AD

    CALL SCHEMA_CN_S(NU_AD_TEST, 0.7_xp)
    P_RAD_AD=S_AD   !P_rad contient schema CN

    ! S_AD=P_AD

    ! CALL SCHEMA_IMPLICITE_S(NU_AD_TEST)
    ! P_GAZ_AD=S_AD  !P_gaz contient schema implicite
    

    ! BETA=P_GAZ_AD-P_RAD_AD

    TIME_AD = TIME_AD + DELTA_T_VISQ

    IF (MODULO(iterateur,100)==0) THEN
        CALL ECRITURE_ADIM
        PRINT*, MAXVAL(ABS(P_GAZ_AD-P_RAD_AD)/MAXVAL(ABS(P_GAZ_AD)))
    ENDIF

END DO

!------------------------------------------------------------------------
                                 END PROGRAM TEST_SCHEMA
!------------------------------------------------------------------------
