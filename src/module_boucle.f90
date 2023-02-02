!===================================================================================================
            MODULE MODULE_BOUCLE 
!===================================================================================================
!> Ce module permet de calculer l'évolution des variables du disque d'accrétion au cours du temps.
!> Il contient plusieurs subroutines :
!> - boucle_thermique fait une boucle sur le schéma numérique de l'équation thermique et calcule 
!>   ensuite le reste des variables.
!>   La boucle s'arrêtera quand Q+-Q- atteindra une valeur de e-17. 
!> - boucle_branche_epaisse fait une boucle en appelant boucle_thermique et le schema implicite de S puis recalcule 
!>   le reste des variables.
!>   La boucle s'arrêtera quand on arrivera à m_dot égal à 1 dans tout le disque, ou quand on rentre en zone critique
!> - boucle_parallele résoud Sigma et Température en parallele
!===================================================================================================

USE MODULE_DECLARATIONS
USE DIMENSIONNEMENT
USE FRAMES_2D
USE MODULE_FUNCTION
USE MODULE_SCHEMAS_SIGMA
USE MODULE_ECRITURE
USE MODULE_SCHEMAS_T
USE MODULE_SCHEMAS_INSTABILITE

IMPLICIT NONE

REAL(KIND=XP), PARAMETER, PRIVATE :: SWITCH_TH = 1.0e-15_xp         !! valeur d'arrêt de boucle pour Q+ - Q-
REAL(KIND=xp), PARAMETER, PRIVATE :: FRACTION_DT_TH   = 1.0E-1_xp   !! Fraction du pas de temps thermique
REAL(KIND=XP), PARAMETER, PRIVATE :: FRACTION_DT_VISQ = 5.0E-4_XP   !! Fraction du pas de temps visqueux

INTEGER, PRIVATE :: NB_IT_TH    !! Nombre d'itérations réalisées dans le régime thermique

!===================================================================================================
            CONTAINS    
!===================================================================================================

SUBROUTINE BOUCLE_THERMIQUE()
!---------------------------------------------------------------------------------------------------
!> Cette subroutine itère depuis une température et densité de surface initiale jusqu'à converger 
!> vers Q+ - Q- = 0.
!> On considère que cette convergence se fait sur un temps thermique << temps visqueux et donc que
!> la densité de surface est constante.
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE
    
    INTEGER :: I
    
    ! Lancement de la boucle qui tournera tant que Q+ - Q- est > switch
    I = 0
    
    DO WHILE( MAXVAL(ABS(Q_PLUS_AD - Q_MOINS_AD)) > SWITCH_TH)

        CALL ITERATION_TEMP_AD()   ! on appelle le schéma de l'équation de T
        CALL COMPUTE_EQS()         ! on calcule le reste des variables
        
        ! Affichage pour observer l'évolution du système ( q+-q- et m_dot)
        IF (MODULO(I,50000)==1) THEN
            WRITE (*,"('Q+-Q- = ',1pE12.4,'  ABS(M_DOT-1) = ',1pE12.4)") &
                & MAXVAL(ABS(Q_PLUS_AD - Q_MOINS_AD)), &
                & ABS(MINVAL(M_DOT_AD-1.0_xp))
        ENDIF
        
        TIME_AD = TIME_AD + DELTA_T_TH_AD
        I = I+1
              
    ENDDO
    
    NB_IT_TH = I

    ! Affichage des variables de sortie de boucle
    WRITE(*,"('Nombre d iterations thermique       : ',I12)") NB_IT_TH
    WRITE(*,"('Temp thermique adimensionné atteint = ',1pE12.4)") DELTA_T_TH_AD * I

!---------------------------------------------------------------------------------------------------
END SUBROUTINE BOUCLE_THERMIQUE
!---------------------------------------------------------------------------------------------------

SUBROUTINE BOUCLE_BRANCHE_EPAISSE()
!---------------------------------------------------------------------------------------------------
!> Cette subroutine itère depuis une température et densité de surface initiale jusqu'à converger
!> vers le point critique de la courbe en S (sur sa partie optiquement épaisse).
!> On converge vers cette courbe (ie Q+ - Q- = 0) en appelant schema_th_time, puis on réalise un
!> pas de temps visqueux pour faire évoluer la densité de surface.
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE
     
    INTEGER :: ITE,I
    REAL(KIND=XP) :: M_DOT_MIN
    REAL(KIND=XP) :: S_SAVE(NX)
    
    ! Pas de temps visqueux et thermique
    DELTA_T_VISQ_AD = FRACTION_DT_VISQ * MAXVAL( X_AD ** 4.0_xp / NU_AD ) 
    DELTA_T_TH_AD = FRACTION_DT_TH / MAXVAL(OMEGA_AD)
    
    ! Génération des grilles de calcul pour le schéma implicite de S
    CALL CREER_LAMBDA()
     
    ! AFfichage des données d'entrée de boucle
    WRITE(*,"(48('-'))")
    WRITE(*,"(48('-'))")
    
    WRITE(*,"('PAS DE TEMPS VISQUEUX          DELTA_T_VISQ_AD = ',1pE12.4)") DELTA_T_VISQ_AD
    WRITE(*,"('PAS DE TEMPS THERMIQUE           DELTA_T_TH_AD = ',1pE12.4)") DELTA_T_TH_AD
    
    WRITE(*,"(48('-'))")
    WRITE(*,"(48('-'))")
    
    M_DOT_MIN = MAXVAL(ABS(M_DOT_AD-1.0_xp))
    
    ! Lancement boucle pour arriver à m_dot = 1
    ITE=1
    I = 0
    DO WHILE((M_DOT_MIN>=0.01_xp).and.(ITE<56)) ! : 55 pour proche instabilité

        WRITE(*,"(48('-'))")
        WRITE(*,"(I0,'e iteration de temps thermique ')") ITE 
        
        ! Ecriture avant itérations du schéma numérique thermique
        CALL ADIM_TO_PHYSIQUE()
        CALL ECRITURE_DIM()
        
        I = I+1
        ! Ecriture frame
        CALL FRAME(TEMP,I)
        
        ! Schéma numérique thermique
        CALL BOUCLE_THERMIQUE()

        ! Ecriture après itérations
        CALL ADIM_TO_PHYSIQUE()
        CALL ECRITURE_DIM()
        
        I = I+1
        ! Ecriture frame
        CALL FRAME(TEMP,I)

        ! Calcul de Q_ADV et schéma numérique visqueux
        S_SAVE = S_AD
        CALL COMPUTE_Q_ADV_AD(DELTA_T_VISQ_AD,S_SAVE)
        CALL SCHEMA_IMPLICITE_S(NU_AD)
        
        ! Calcul des autres variables
        CALL COMPUTE_EQS()
        
        TIME_AD = TIME_AD + DELTA_T_VISQ_AD - NB_IT_TH * DELTA_T_TH_AD
        ITE=ITE+1
        M_DOT_MIN = ABS(MINVAL(M_DOT_AD-1.0_xp))
            
    ENDDO  

!---------------------------------------------------------------------------------------------------
END SUBROUTINE BOUCLE_BRANCHE_EPAISSE
!---------------------------------------------------------------------------------------------------

SUBROUTINE BOUCLE_PARALLELE(FRACTION_DT_INSTABLE,ECRIT_PAS)
!---------------------------------------------------------------------------------------------------
!> Calcul précis de l'instabilité, à la fraction de temps caracteristique donnée en entrée (usuel 10e-7)
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE

    INTEGER :: iterateur
    INTEGER, INTENT(IN) :: ECRIT_PAS
    REAL(KIND=xp) :: FRACTION_DT_INSTABLE
    

    DELTA_T_INSTABLE_AD = FRACTION_DT_INSTABLE * MAXVAL( X_AD ** 4.0_xp / NU_AD )
    CALL SETUP_SCHEMA_INSTABLE_TS()
    
    WRITE(*,"(48('-'))")
    WRITE(*,"(48('-'))")
    WRITE(*,"('BOUCLE INSTABLE            DELTA_T_INSTABLE_AD = ',1pE12.4)") DELTA_T_INSTABLE_AD
    WRITE(*,"(48('-'))")
    WRITE(*,"(48('-'))")

    DO iterateur=1, 7000000
    
        CALL SCHEMA_INSTABLE_TS(0.8_xp)
        CALL COMPUTE_EQS()
        TIME_AD = TIME_AD + DELTA_T_INSTABLE_AD
        CALL ADIM_TO_PHYSIQUE()
        IF (MODULO(iterateur,ECRIT_PAS)==0) THEN
            PRINT*,iterateur,'   TEMPS AD =  ',TIME_AD
            
            CALL ECRITURE_DIM()
        ENDIF
        
    END DO
    
!---------------------------------------------------------------------------------------------------
END SUBROUTINE BOUCLE_PARALLELE
!---------------------------------------------------------------------------------------------------

!---------------------------------------------------------------------------------------------------
SUBROUTINE BOUCLE_TEMP_A_ATTEINDRE(FRACTION_DT_INSTABLE,ECRIT_PAS,T_TO_REACH,I)
!---------------------------------------------------------------------------------------------------
!> Subroutine qui utilise un mode fontionnement temporel
!> Calcul précis de l'instabilité, à la fraction de temps caracteristique donnée en entrée 
!> La boucle tourne jusqu'à atteindre un temps dimensionné mit en argument
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE

    INTEGER :: iterateur
    INTEGER,INTENT(INOUT) :: I
    INTEGER, INTENT(IN) :: ECRIT_PAS
    REAL(KIND=XP), INTENT(IN) :: T_TO_REACH
    REAL(KIND=xp) :: FRACTION_DT_INSTABLE
    

    DELTA_T_INSTABLE_AD = FRACTION_DT_INSTABLE * MAXVAL( X_AD ** 4.0_xp / NU_AD )
    CALL SETUP_SCHEMA_INSTABLE_TS()
    
    WRITE(*,"(48('-'))")
    WRITE(*,"(48('-'))")
    WRITE(*,"('BOUCLE INSTABLE            DELTA_T_INSTABLE_AD = ',1pE12.4)") DELTA_T_INSTABLE_AD
    WRITE(*,"(48('-'))")
    WRITE(*,"(48('-'))")
    
    iterateur = 1
    DO WHILE( TIME_AD/OMEGA_MAX <= T_TO_REACH)
    
        CALL SCHEMA_INSTABLE_TS(0.8_xp)
        CALL COMPUTE_EQS()
        TIME_AD = TIME_AD + DELTA_T_INSTABLE_AD
        CALL ADIM_TO_PHYSIQUE()
        IF (MODULO(iterateur,ECRIT_PAS)==0) THEN
            PRINT*,iterateur,'   TEMPS  =  ',TIME_AD/OMEGA_MAX
            
            CALL ECRITURE_DIM()
            CALL FRAME(TEMP,I)
            I=I+1
        ENDIF
        
        iterateur = iterateur +1
    END DO
    
!---------------------------------------------------------------------------------------------------
END SUBROUTINE BOUCLE_TEMP_A_ATTEINDRE
!---------------------------------------------------------------------------------------------------

!===================================================================================================
            END MODULE MODULE_BOUCLE
!===================================================================================================
