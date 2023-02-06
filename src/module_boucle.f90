!===================================================================================================
            MODULE MODULE_BOUCLE 
!===================================================================================================
!> Ce module permet de calculer l'évolution des variables du disque d'accrétion au cours du temps.
!> Il contient plusieurs subroutines :
!> - BOUCLE_THERMIQUE itère le schéma numérique de l'équation thermique à Sigma constant
!>   et s'arrête quand Q+-Q- atteint zero a precision donnée. Utilisé par BOUCLE_BRANCHE_EPAISSE.
!> - BOUCLE_BRANCHE_EPAISSE itère boucle_thermique et le schema implicite de S pour remonter le long
!>   de la branche épaisse. La boucle s'arrête quand m_dot = 1 dans tout le disque (equilibre),
!>   ou quand on approche une zone critique.
!> - BOUCLE_PARALLELE résoud Sigma et Température en parallèle, pour gérer l'instabilité.
!>   Deux modes d'arret sont possible: approcher un point critique ou attendre que tous les points
!>   du disques soient redescendus sous la température critique (fin de l'instabilité)
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

REAL(KIND=XP), PARAMETER, PRIVATE :: SWITCH_THERMIQUE = 1.0e-10_xp  !! valeur d'arrêt de boucle pour Q+ - Q-
REAL(KIND=xp), PARAMETER, PRIVATE :: FRACTION_DT_TH   = 1.0E-1_xp   !! Fraction du pas de temps thermique
REAL(KIND=XP), PARAMETER, PRIVATE :: FRACTION_DT_VISQ = 5.0E-4_xp   !! Fraction du pas de temps visqueux
REAL(KIND=XP), PARAMETER, PRIVATE :: PRECISION_MDOT   = 1.0e-3_xp   !! Précision attendue sur Mdot == 1

INTEGER, PRIVATE                  :: NB_ITE_THERMIQUE               !! Nombre d'itérations réalisées dans boucle_thermique

!===================================================================================================
            CONTAINS    
!===================================================================================================

SUBROUTINE BOUCLE_THERMIQUE()
!---------------------------------------------------------------------------------------------------
!> Cette subroutine itère la température jusqu'à converger vers Q+ - Q- = 0.
!> Sur la branche épaisse, cette convergence se fait sur un temps thermique << temps visqueux et la
!> densité de surface est donc considérée constante.
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE
    
    ! INITIALISATION -------------------------------------------------------------------------------
    NB_ITE_THERMIQUE = 0
    
    ! BOUCLE ---------------------------------------------------------------------------------------

    ! tourne tant que l'equilibre thermique n'est pas atteint
    DO WHILE( MAXVAL(ABS(Q_PLUS_AD - Q_MOINS_AD)) > SWITCH_THERMIQUE)

        !ACTUALISATION DES VARIABLES --------------------------------------

        CALL SCHEMA_TEMP_EPAIS()              ! appel du schéma explicite de T (sans Q_adv)
        CALL COMPUTE_EQS()                    ! actualisation des variables
        TIME_AD = TIME_AD + DELTA_T_TH_AD     ! actualisation du temps
        NB_ITE_THERMIQUE = NB_ITE_THERMIQUE+1 ! actualisation du nombre d'itérations réalisées

        !AFFICHAGE ---------------------------------------------------------
        ! Affichage pour observer l'évolution du système si l'equilibre n'est pas atteint rapidement
        IF (MODULO(NB_ITE_THERMIQUE,100000) == 99999) THEN
            WRITE (*,"('Q+-Q- = ',1pE12.4)") MAXVAL(ABS(Q_PLUS_AD - Q_MOINS_AD))
            WRITE(*,"(48('-'))")
            
        ENDIF

    ENDDO

!---------------------------------------------------------------------------------------------------
END SUBROUTINE BOUCLE_THERMIQUE
!---------------------------------------------------------------------------------------------------

SUBROUTINE BOUCLE_BRANCHE_EPAISSE(mode_arret, choix_facteur_securite)
!---------------------------------------------------------------------------------------------------
!> Cette boucle consiste à converger à l'eq thermique (ie Q+ - Q- = 0) en appelant
!> BOUCLE_THERMIQUE, puis à itérer la densité surfacique sur un pas de temps visqueux.
!>
!> Deux conditions d'arret sont possibles:
!>
!> Si mode_arret=0:
!> Cette subroutine itère le long de la branche épaisse, soit jusqu'à approcher
!> un point critique de la courbe en S, soit jusqu'a la situation d'equilibre (M_DOT_AD==1).
!> Si choix_facteur_securite est spécifié, on s'arrete avant d'atteindre
!> choix_facteur_securite*Sigma_critique, (0.95 par défaut).
!>
!> Si mode_arret>0: 
!> Cette subroutine réalise un nombre mode_arret de pas de temps visqueux ou s'arrete a la situation
!> d'equilibre (M_DOT_AD==1)
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE

    ! DECLARATIONS ----------------------------------------------------------------------------------

    ! Variables d'entrée
    INTEGER, INTENT(IN) :: mode_arret
    REAL(KIND=xp), INTENT(IN), OPTIONAL :: choix_facteur_securite

    ! Variables internes
    INTEGER :: ITE                              ! Ordinal de l'itération actuelle
    REAL(KIND=XP) :: ECART_M_DOT                ! Ecart à M_DOT=1 ( MAX(ABS(M_DOT-1)) )
    REAL(KIND=XP) :: S_SAVE(NX), SIGMA_SAVE(NX) ! Variables de sauvegarde de la densité
    REAL(KIND=XP) :: FACTEUR_SECURITE           ! Fraction du Sigma critique avant laquelle s'arreter

    ! INITIALISATION ---------------------------------------------------------------------------------

    ! Initialisation du facteur_securite
    IF (PRESENT(choix_facteur_securite)) THEN
        FACTEUR_SECURITE = choix_facteur_securite
    ELSE
        FACTEUR_SECURITE = 0.95_xp
    ENDIF

    ! Initialisation pas de temps visqueux et thermique
    DELTA_T_VISQ_AD = FRACTION_DT_VISQ * MAXVAL( X_AD ** 4.0_xp / NU_AD )
    DELTA_T_TH_AD   = FRACTION_DT_TH / MAXVAL(OMEGA_AD)
    
    ! Initialisation des tableaux pour le schéma implicite de S
    CALL CREER_LAMBDA()
     
    ! AFfichage des données d'entrée de boucle
    WRITE(*,"(48('-'))")
    WRITE(*,"('DEBUT DE BOUCLE BRANCHE EPAISSE')")
    WRITE(*,"('PAS DE TEMPS VISQUEUX          DELTA_T_VISQ_AD = ',1pE12.4)") DELTA_T_VISQ_AD
    WRITE(*,"('PAS DE TEMPS THERMIQUE           DELTA_T_TH_AD = ',1pE12.4)") DELTA_T_TH_AD
    WRITE(*,"(48('-'))")
    
    ITE=1 ! ordinal de l'itération actuelle

    ! BOUCLE ---------------------------------------------------------------------------------------

    DO
        !ECRITURE --------------------------------------------------
        ! Ecriture avant itérations du schéma numérique thermique
        CALL ADIM_TO_PHYSIQUE()
        CALL ECRITURE_DIM()
        ! Ecriture frame
        FRAME_ID = FRAME_ID + 1
        CALL FRAME(TEMP, FRAME_ID)
        
        !CALCUL TEMPERATURE-----------------------------------------
        ! Actualisation de T jusqu'à l'equilibre thermique
        CALL BOUCLE_THERMIQUE()

        !ECRITURE---------------------------------------------------
        ! Ecriture après itérations
        CALL ADIM_TO_PHYSIQUE()
        CALL ECRITURE_DIM()
        ! Ecriture frame
        FRAME_ID = FRAME_ID + 1
        CALL FRAME(TEMP, FRAME_ID)

        !CALCUL DE S ------------------------------------------------
        !sauvegarde de la densité avant itération
        S_SAVE = S_AD                                ! sauvegarde de l'ancienne S_AD (pour Q_adv)
        SIGMA_SAVE = SIGMA                           ! sauvegarde de l'ancien SIGMA (pour la condition d'arret)
         !Actualisation de S
        CALL SCHEMA_IMPLICITE_S(NU_AD)
        ! Calcul de Q_ADV (non pris en compte)
        CALL COMPUTE_Q_ADV_AD(DELTA_T_VISQ_AD,S_SAVE)

        !ACTUALISATION DES AUTRES VARIABLES-------------------------
        CALL COMPUTE_EQS()
        ! Actualisation du temps
        TIME_AD = TIME_AD + DELTA_T_VISQ_AD - NB_ITE_THERMIQUE * DELTA_T_TH_AD
        ! Calcul de l'écart de M_dot à 1
        ECART_M_DOT = ABS(MINVAL(M_DOT_AD-1.0_xp))

        
        !AFFICHAGE--------------------------------------------------
        IF (MODULO(ITE,10)==1) THEN
            WRITE(*,"('---',I0,'e iteration de boucle branche epaisse faite ')") ITE
            WRITE(*,"('---Nombre d iterations faites dans la boucle thermique : ',I12)") NB_ITE_THERMIQUE
            WRITE(*,"(48('-'))")
        ENDIF
        
        !Passage à l'itération suivante
        ITE = ITE + 1

        !CALCUL DE LA CONDITION D'ARRET ----------------------------

        ! Cas mode_arret=0 : verification de la proximité des points critiques ou de l'equilibre
        IF (mode_arret == 0) THEN
            CALL ADIM_TO_PHYSIQUE
            !on teste si la prochaine itération (estimée par l'ancienne) dépasserait le seuil, ou si M_dot=1
            IF (    (MAXVAL(SIGMA + (SIGMA - SIGMA_SAVE) - SIGMA_CRITIQUE * FACTEUR_SECURITE) >= 0.0_xp) &
             &  .or.(ECART_M_DOT <= PRECISION_MDOT)) THEN
                EXIT
            ENDIF

        ! Cas mode_arret=i : verification de l'equilibre ou que l'on a fait les i itérations voulues
        ELSE
            IF ((ITE > mode_arret).or.(ECART_M_DOT <= PRECISION_MDOT)) THEN
                EXIT
            ENDIF
        ENDIF
            
    ENDDO

    ! AFFICHAGE SORTANT ----------------------------------------------------------------------------
    WRITE(*,"('SORTIE DE BOUCLE BRANCHE EPAISSE')")
    WRITE(*, "('Nombre d iterations : ',I0)") ITE-1
    WRITE(*,"(48('-'))")
!---------------------------------------------------------------------------------------------------
END SUBROUTINE BOUCLE_BRANCHE_EPAISSE
!---------------------------------------------------------------------------------------------------

SUBROUTINE BOUCLE_PARALLELE(FRACTION_DT_INSTABLE, ECRIT_PAS , mode_arret, choix_facteur_securite, choix_precision_ecriture)
!---------------------------------------------------------------------------------------------------
!> Calcul précis de l'instabilité, à la fraction de temps caracteristique donnée en entrée FRACTION_DT_INSTABLE.
!>
!> Plusieurs modes d'arret sont possible:
!>
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
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE

    ! DECLARATIONS -------------------------------------------------------------------------------

    ! Variables d'entrée
    REAL(KIND=xp), INTENT(IN) :: FRACTION_DT_INSTABLE               !pas de temps (en fraction du temps caracteristique)
    INTEGER, INTENT(IN) :: ECRIT_PAS                                !pas d'écriture
    INTEGER, INTENT(IN) :: mode_arret                               !choix du critère d'arret de la boucle
    REAL(KIND=xp), INTENT(IN), OPTIONAL :: choix_facteur_securite   !choix de fraction du point critique à laquelle s'arreter
    REAL(KIND=xp), INTENT(IN), OPTIONAL :: choix_precision_ecriture !pour ecrire plus ou moins précisement (si ECRIT_PAS=0)

    ! Variables internes
    INTEGER :: ITERATEUR, COMPTE_ECRITURE                           !compteur d'iterations, compteur d'ecritures
    REAL(KIND=xp) :: FACTEUR_SECURITE                               !fraction du point critique à laquelle s'arreter
    REAL(KIND=xp) :: TEMP_DERNIERE_ECRITURE(NX)                     !sauvegarde de la derniere ecriture de temp (si ECRIT_PAS=0) 
    REAL(KIND=xp) :: SIGMA_DERNIERE_ECRITURE(NX)                    !sauvegarde de la derniere ecriture de sigma (si ECRIT_PAS=0)
    REAL(KIND=xp) :: PRECISION_ECRITURE                             !pour ecrire plus ou moins précisement (si ECRIT_PAS=0)

    ! INITIALISATION ------------------------------------------------------------------------------
    
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

    ! Initialisation du pas de temps
    DELTA_T_INSTABLE_AD = FRACTION_DT_INSTABLE * MAXVAL( X_AD ** 4.0_xp / NU_AD )

    ! Initialisation du schema instable
    CALL SETUP_SCHEMA_INSTABLE_TS()

    ! sauvegarde des variables de derniere ecriture
    CALL ADIM_TO_PHYSIQUE()
    TEMP_DERNIERE_ECRITURE  = TEMP
    SIGMA_DERNIERE_ECRITURE = SIGMA
    COMPTE_ECRITURE = 0

    !Compteur d'itérations
    ITERATEUR = 1

    !AFFICHAGE ------------------------------------------------------------------------------------
    WRITE(*,"(48('-'))")
    
    WRITE(*,"('DEBUT DE BOUCLE PARALLELE')")
    WRITE(*,"('DELTA_T_INSTABLE_AD = ',1pE12.4)") DELTA_T_INSTABLE_AD
    WRITE(*,"('FRACTION DE TEMP_CRITIQUE D ARRET = ',1pE12.4)") FACTEUR_SECURITE

    WRITE(*,"(48('-'))")


    !BOUCLE --------------------------------------------------------------------------------------

    DO
        ! CALCUL DE LA CONDITION D'ARRET---------------------------------------------------------------------------
        ! Cas mode_arret = -1 : verification que tous les points sont sous facteur_securite*temp_critique
        IF (mode_arret == -1) THEN
            IF (MAXVAL(TEMP - FACTEUR_SECURITE * TEMP_CRITIQUE) < 0.0_xp) THEN
                EXIT
            ENDIF
        ! Cas mode_arret = 0 : verification de la proximité des points critiques
        ELSE IF (mode_arret == 0) THEN
            IF (MAXVAL(TEMP - FACTEUR_SECURITE*TEMP_CRITIQUE) >= 0.0_xp) THEN
                EXIT
            ENDIF
        ! Cas mode_arret = i > 0 : on fait i itérations
        ELSE
            IF ((ITERATEUR > mode_arret)) THEN
                EXIT
            ENDIF
        ENDIF

        ! ACTUALISATION DES GRANDEURS PHYSIQUES: ITERATION DES SCHEMAS--------------------------------------------
        CALL SCHEMA_INSTABLE_TS(0.8_xp) !l'argument est le parametre de Crank-Nicolson
        CALL COMPUTE_EQS()
        TIME_AD = TIME_AD + DELTA_T_INSTABLE_AD
        CALL ADIM_TO_PHYSIQUE()

        ! ECRITURE------------------------------------------------------------------------------------------------
        ! Mode ecrit_pas > 0: écriture tout les ecrit_pas
        IF (ECRIT_PAS > 0) THEN
            IF (MODULO(iterateur, ECRIT_PAS) == 0) THEN
                CALL FRAME(TEMP, FRAME_ID)
                FRAME_ID = FRAME_ID + 1
                CALL ECRITURE_DIM()
                COMPTE_ECRITURE = COMPTE_ECRITURE + 1
            ENDIF
            !on affiche egalament sur la console tous les 100 ECRIT_PAS
            IF (MODULO(iterateur, 100 * ECRIT_PAS) == 0) THEN
                PRINT*,'---Nombre d iterations :', iterateur,'   TEMPS AD : ',TIME_AD
                PRINT*,'---Ecart a la valeur d arret : ', MAXVAL(TEMP-FACTEUR_SECURITE*TEMP_CRITIQUE)
            ENDIF

        ! Mode ecrit_pas = 0: on vérifie la distance du dernier point tracé, on écrit si on s'est suffisamment éloigné
        ELSE
            IF (MAXVAL((ABS((TEMP - TEMP_DERNIERE_ECRITURE)) / 5e5_xp) &
                        &   +(ABS((SIGMA - SIGMA_DERNIERE_ECRITURE)) / 2500.0_xp)) > 1.0_xp / PRECISION_ECRITURE) THEN
                CALL FRAME(TEMP, FRAME_ID)
                FRAME_ID = FRAME_ID + 1
                CALL ECRITURE_DIM()
                TEMP_DERNIERE_ECRITURE  = TEMP
                SIGMA_DERNIERE_ECRITURE = SIGMA
                COMPTE_ECRITURE = COMPTE_ECRITURE+1
            ENDIF
            ! on écrit sur la console tous les millions de pas de temps
            IF ((ECRIT_PAS == 0).and.(MODULO(iterateur, 1000000) == 0)) THEN
                PRINT*,'---Nombre d iterations :', iterateur,'   TEMPS AD =  ',TIME_AD
                PRINT*,'---Ecart a la valeur d arret : ', MAXVAL(TEMP-FACTEUR_SECURITE*TEMP_CRITIQUE)
                WRITE(*,"(48('-'))")
            ENDIF
        ENDIF

        !passage a la boucle suivante
        ITERATEUR=ITERATEUR+1
        
    END DO

    ! AFFICHAGE SORTANT ----------------------------------------------------------------------------
    WRITE(*,"('SORTIE DE BOUCLE PARALLELE')")
    WRITE(*, "('Nombre d iterations : ',I0)") ITERATEUR-1
    WRITE(*, "('Nombre d ecritures  : ',I0)") COMPTE_ECRITURE
    WRITE(*,"(48('-'))")
!---------------------------------------------------------------------------------------------------
END SUBROUTINE BOUCLE_PARALLELE
!---------------------------------------------------------------------------------------------------


!===================================================================================================
            END MODULE MODULE_BOUCLE
!===================================================================================================
