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
USE MODULE_ECRITURE
USE MODULE_SCHEMAS_T
USE MODULE_SCHEMAS_SIGMA
USE MODULE_SCHEMAS_INSTABILITE

IMPLICIT NONE

REAL(KIND=XP), PARAMETER, PRIVATE :: SWITCH_THERMIQUE = 1.0e-15_xp  !! valeur d'arrêt de boucle pour Q+ - Q-
REAL(KIND=xp), PARAMETER, PRIVATE :: FRACTION_DT_TH   = 1.0E-1_xp   !! Fraction du pas de temps thermique
REAL(KIND=XP), PARAMETER, PRIVATE :: FRACTION_DT_VISQ = 5.0E-4_xp   !! Fraction du pas de temps visqueux
REAL(KIND=XP), PARAMETER, PRIVATE :: PRECISION_MDOT   = 1.0e-4_xp   !! Précision attendue sur Mdot == 1

INTEGER, PRIVATE :: NB_ITE_THERMIQUE    !! Nombre d'itérations réalisées dans le régime thermique

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
    
    DO WHILE( MAXVAL(ABS(Q_PLUS_AD - Q_MOINS_AD)) > SWITCH_THERMIQUE)

        CALL ITERATION_TEMP_AD()   ! on appelle le schéma de l'équation de T
        CALL COMPUTE_EQS()         ! on calcule le reste des variables
        
        ! Affichage pour observer l'évolution du système ( q+-q- et m_dot)
        IF (MODULO(I,50000) == 1) THEN
            WRITE (*,"('Q+-Q- = ',1pE12.4,'  ABS(M_DOT-1) = ',1pE12.4)") &
                & MAXVAL(ABS(Q_PLUS_AD - Q_MOINS_AD)), &
                & ABS(MINVAL(M_DOT_AD-1.0_xp))
        ENDIF
        
        TIME_AD = TIME_AD + DELTA_T_TH_AD
        I = I+1
              
    ENDDO
    
    NB_ITE_THERMIQUE = I

    ! Affichage des variables de sortie de boucle
    WRITE(*,"('Nombre d iterations thermique       : ',I12)") NB_ITE_THERMIQUE
    WRITE(*,"('Temps thermique écoulé = ',1pE12.4)") DELTA_T_TH_AD * I

!---------------------------------------------------------------------------------------------------
END SUBROUTINE BOUCLE_THERMIQUE
!---------------------------------------------------------------------------------------------------

SUBROUTINE BOUCLE_BRANCHE_EPAISSE(mode_arret, choix_facteur_securite)
!---------------------------------------------------------------------------------------------------
!> Une itération de cette boucle consiste à converger à l'eq thermique (ie Q+ - Q- = 0) en appelant
!> BOUCLE_THERMIQUE, puis à itérer la densité surfacique sur un pas de temps visqueux.
!>
!> Si mode_arret=0:
!> Cette subroutine itère  une température et densité de surface sur la branche épaisse jusqu'à approcher
!> un point critique de la courbe en S, ou jusqu'a la situation d'equilibre (M_DOT_AD==1).
!> Si choix_facteur_securite est spécifié, on s'arrete avant d'atteindre
!> choix_facteur_securite*Sigma_critique, sinon à 0.95*Sigma_critique par défaut.
!>
!> Si mode_arret>0: 
!> Cette subroutine réalise un nombre mode_arret de pas de temps visqueux ou s'arrete a la situation
!> d'equilibre (M_DOT_AD==1)
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE

    ! Variables d'entrée
    INTEGER, INTENT(IN) :: mode_arret
    REAL(KIND=xp), INTENT(IN), OPTIONAL :: choix_facteur_securite

    ! Variables internes
    INTEGER :: ITE,I
    REAL(KIND=XP) :: M_DOT_MIN
    REAL(KIND=XP) :: S_SAVE(NX), SIGMA_SAVE(NX)
    REAL(KIND=XP) :: FACTEUR_SECURITE           !!fraction du Sigma critique avant laquelle s'arreter

    ! Initialisation du facteur_securite si spécifié
    IF (PRESENT(choix_facteur_securite)) THEN
        FACTEUR_SECURITE = choix_facteur_securite
    ELSE
        FACTEUR_SECURITE = 0.95_xp
    ENDIF

    ! Pas de temps visqueux et thermique
    DELTA_T_VISQ_AD = FRACTION_DT_VISQ * MAXVAL( X_AD ** 4.0_xp / NU_AD ) 
    DELTA_T_TH_AD   = FRACTION_DT_TH / MAXVAL(OMEGA_AD)
    
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
    
    ! Lancement boucle
    ITE=1 ! ordinal de l'itération actuelle
    I = 0 ! relatif a l'ecriture de frame2D

    DO
        WRITE(*,"(48('-'))")
        WRITE(*,"(I0,'e iteration de temps thermique ')") ITE
        
        ! Ecriture avant itérations du schéma numérique thermique
        CALL ADIM_TO_PHYSIQUE()
        CALL ECRITURE_DIM()
        
        ! Ecriture frame
        I = I+1
        CALL FRAME(TEMP,I)
        
        ! Schéma numérique thermique
        CALL BOUCLE_THERMIQUE()

        ! Ecriture après itérations
        CALL ADIM_TO_PHYSIQUE()
        CALL ECRITURE_DIM()
        
        ! Ecriture frame
        I = I+1
        CALL FRAME(TEMP,I)

        ! Calcul de Q_ADV et schéma numérique visqueux
        S_SAVE = S_AD   ! sauvegarde de l'ancienne S_AD
        SIGMA_SAVE = SIGMA
        CALL COMPUTE_Q_ADV_AD(DELTA_T_VISQ_AD,S_SAVE)
        CALL SCHEMA_IMPLICITE_S(NU_AD)
        
        ! Calcul des autres variables
        CALL COMPUTE_EQS()
        
        TIME_AD = TIME_AD + DELTA_T_VISQ_AD - NB_ITE_THERMIQUE * DELTA_T_TH_AD
        M_DOT_MIN = ABS(MINVAL(M_DOT_AD-1.0_xp))

        ITE = ITE + 1

        ! CALCUL DE LA CONDITION D'ARRET POUR LA PROCHAINE ITERATION
        ! Cas mode_arret=0 : verification de la proximité des points critiques ou de l'equilibre
        IF (mode_arret == 0) THEN
            CALL ADIM_TO_PHYSIQUE
            !on teste si la prochaine itération (estimée par l'ancienne) dépasserait le seuil, ou si M_dot=1
            IF (    (MAXVAL(SIGMA + 1.0_xp*(SIGMA - SIGMA_SAVE) - SIGMA_CRITIQUE * FACTEUR_SECURITE) >= 0.0_xp) &
             &  .or.(M_DOT_MIN <= PRECISION_MDOT)) THEN
                EXIT
            ENDIF

        ! Cas mode_arret=i : verification de l'equilibre ou que l'on a fait les i itérations voulues
        ELSE
            IF ((ITE > mode_arret).or.(M_DOT_MIN <= PRECISION_MDOT)) THEN
                EXIT
            ENDIF
        ENDIF
            
    ENDDO  
!---------------------------------------------------------------------------------------------------
END SUBROUTINE BOUCLE_BRANCHE_EPAISSE
!---------------------------------------------------------------------------------------------------

SUBROUTINE BOUCLE_PARALLELE(FRACTION_DT_INSTABLE, ECRIT_PAS , mode_arret, choix_facteur_securite, choix_precision_ecriture)
!---------------------------------------------------------------------------------------------------
!> Calcul précis de l'instabilité, à la fraction de temps caracteristique donnée en entrée.
!> mode_arret = -1 : s'arrete quand tous les points sont descendus dessous choix_facteur_securite*Temperature_critique:
!> à utiliser pour retourner sur la branche épaisse avec un petit pas de temps (1e-10 typqiue)
!>
!> mode_arret = 0 : s'arrete en atteignant choix_facteur_securite*Temperature_critique ou
!> choix_facteur_securite*Sigma_critique:
!> à utiliser pour s'approcher des points critiques avec un pas de temps intermediaire
!>
!> mode_arret > 0 : réalise mode_arret pas de temps 
!>
!> Par défaut, choix_facteur_securite = 0.99
!>
!> ECRIT_PAS > 0 : Ecrit les données tous les ECRIT_PAS pas de temps.
!> ECRIT_PAS = 0 : Ecrit les données quand les variables ont suffisamment changé, ajustable avec un facteur choix_precision_ecriture
!> (par défaut 1.0_xp, correspond a environ un centieme de la taille caracteristique de T et Sigma)
!>
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE

    ! Variables d'entrée
    REAL(KIND=xp), INTENT(IN) :: FRACTION_DT_INSTABLE
    INTEGER, INTENT(IN) :: ECRIT_PAS
    INTEGER, INTENT(IN) :: mode_arret
    REAL(KIND=xp), INTENT(IN), OPTIONAL :: choix_facteur_securite
    REAL(KIND=xp), INTENT(IN), OPTIONAL :: choix_precision_ecriture

    ! Variables internes
    INTEGER :: ITERATEUR, COMPTE_ECRITURE
    REAL(KIND=xp) :: FACTEUR_SECURITE
    REAL(KIND=xp) :: TEMP_DERNIERE_ECRITURE(NX), SIGMA_DERNIERE_ECRITURE(NX)
    REAL(KIND=xp) :: PRECISION_ECRITURE
    
    ! Initialisation du facteur_securite
    IF (PRESENT(choix_facteur_securite)) THEN
        FACTEUR_SECURITE = choix_facteur_securite
    ELSE
        FACTEUR_SECURITE = 0.99_xp
    ENDIF

    ! Initialisation de la precision ecriture si ECRIT_PAS=0
    IF (PRESENT(choix_precision_ecriture).and.(ECRIT_PAS == 0)) THEN
        PRECISION_ECRITURE = choix_precision_ecriture
    ELSE
        PRECISION_ECRITURE = 1.0_xp
    ENDIF

    ! Initialisation
    DELTA_T_INSTABLE_AD = FRACTION_DT_INSTABLE * MAXVAL( X_AD ** 4.0_xp / NU_AD )
    CALL SETUP_SCHEMA_INSTABLE_TS()
    CALL ADIM_TO_PHYSIQUE()
    ITERATEUR = 1
    TEMP_DERNIERE_ECRITURE  = TEMP
    SIGMA_DERNIERE_ECRITURE = SIGMA
    COMPTE_ECRITURE = 0
    
    WRITE(*,"(48('-'))")
    WRITE(*,"(48('-'))")
    WRITE(*,"('BOUCLE INSTABLE            DELTA_T_INSTABLE_AD = ',1pE12.4)") DELTA_T_INSTABLE_AD
    WRITE(*,"('FRACTION DE TEMP_CRITIQUE D ARRET = ',1pE12.4)") FACTEUR_SECURITE
    WRITE(*,"(48('-'))")
    WRITE(*,"(48('-'))")

    DO
        ! CALCUL DE LA CONDITION D'ARRET---------------------------------------------------------------------------
        ! Cas mode_arret = -1 : verification que tous les points sont sous facteur_securite*temp_critique
        IF (mode_arret == -1) THEN
            IF (MAXVAL(TEMP - FACTEUR_SECURITE * TEMP_CRITIQUE) < 0.0_xp) THEN
                EXIT
            ENDIF
        ! Cas mode_arret = 0 : verification de la proximité des points critiques
        ELSE IF (mode_arret == 0) THEN
            ! On teste si on est proche à facteur_securite des points critiques en relatif
            IF (MAXVAL(TEMP - FACTEUR_SECURITE*TEMP_CRITIQUE) >= 0.0_xp) THEN
                EXIT
            ENDIF
        ! Cas mode_arret = i > 0 : on fait i itérations
        ELSE
            IF ((ITERATEUR > mode_arret)) THEN
                EXIT
            ENDIF
        ENDIF

        ! SIMULATION PHYSIQUE: ITERATION DES SCHEMAS---------------------------------------------------------------
        CALL SCHEMA_INSTABLE_TS(0.8_xp)
        CALL COMPUTE_EQS()
        TIME_AD = TIME_AD + DELTA_T_INSTABLE_AD
        CALL ADIM_TO_PHYSIQUE()

        ! ECRITURE------------------------------------------------------------------------------------------------
        ! Mode ecrit_pas > 0: écriture tout les ecrit_pas
        IF ((ECRIT_PAS > 0).and.(MODULO(iterateur, ECRIT_PAS) == 0)) THEN
            CALL ECRITURE_DIM()
            COMPTE_ECRITURE = COMPTE_ECRITURE + 1
        ENDIF
        !on écrit sur la console tous les 100 ECRIT_PAS
        IF ((ECRIT_PAS > 0).and.(MODULO(iterateur, 100 * ECRIT_PAS) == 0)) THEN
            PRINT*,iterateur,'   TEMPS AD =  ',TIME_AD
            PRINT*,'Distance a la valeur critique : ', MAXVAL(TEMP-FACTEUR_SECURITE*TEMP_CRITIQUE)
        ENDIF

        ! Mode ecrit_pas = 0: on vérifie la distance du dernier point tracé, on écrit si on s'est suffisamment éloigné
        IF ((ECRIT_PAS == 0).and.(MAXVAL((ABS((TEMP - TEMP_DERNIERE_ECRITURE)) / 5e5_xp) &
                                    &   +(ABS((SIGMA - SIGMA_DERNIERE_ECRITURE)) / 2500.0_xp)) > 1.0_xp / PRECISION_ECRITURE)) THEN
            CALL ECRITURE_DIM()
            TEMP_DERNIERE_ECRITURE  = TEMP
            SIGMA_DERNIERE_ECRITURE = SIGMA
            COMPTE_ECRITURE = COMPTE_ECRITURE+1
        ENDIF
        ! on écrit sur la console tous les millions de pas de temps
        IF ((ECRIT_PAS == 0).and.(MODULO(iterateur, 1000000) == 0)) THEN
            PRINT*,iterateur,'   TEMPS AD =  ',TIME_AD
            PRINT*,'Distance a la valeur critique : ', MAXVAL(TEMP-FACTEUR_SECURITE*TEMP_CRITIQUE)
        ENDIF

        !passage a la boucle suivante
        ITERATEUR=ITERATEUR+1
        
    END DO
    
    WRITE(*, "('Nombre iterations : ',I0)") ITERATEUR-1
    WRITE(*, "('Nombre ecritures  : ',I0)") COMPTE_ECRITURE
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
