!===================================================================================================
            MODULE MODULE_BOUCLE 
!===================================================================================================
!> Ce module permet de calculer l'évolution des variables du disque d'accrétion au cours du temps.

!> Il contient plusieurs subroutines :
!> - creer_frame qui permet de réaliser une symétrie circulaire sur l'array d'une des variables ( avec la dernière orbite stable pour point central ) et d'en faire un fichier .out stocké dans le folder frame_array à ensuite plotter en python pour faire un animation
!> - frame prend la condition en input frame_cond, un indice d'écriture et lance la subroutine creer_frame
!> - schema_th_time fait une boucle sur le schéma numérique de l'équation thermique et calcule 
!>   ensuite le reste des variables.
!>   La boucle s'arrêtera quand Q+-Q- atteindra une valeur de e-17. 
!> - schema_first_branch fait une boucle en appelant le schema thermique et le schema implicit de S puis recalcule 
!>   le reste des variables.
!>   La boucle s'arrêtera quand on arrivera à m_dot égal à 1 dans tout le disque.
!===================================================================================================

USE MODULE_DECLARATIONS
USE DIMENSIONNEMENT
USE MODULE_FUNCTION
USE MODULE_SCHEMAS_SIGMA
USE MODULE_ECRITURE
USE MODULE_SCHEMAS_T
USE MODULE_SCHEMAS_INSTABILITE

IMPLICIT NONE

REAL(KIND=xp), PARAMETER, PRIVATE :: FRACTION_DT_TH   = 1.0E-1_xp   !! Fraction du pas de temps thermique
REAL(KIND=XP), PARAMETER, PRIVATE :: FRACTION_DT_VISQ = 5.0E-4_XP   !! Fraction du pas de temps visqueux

INTEGER, PRIVATE :: NB_IT_TH    !! Nombre d'itérations réalisées dans le régime thermique

!===================================================================================================
            CONTAINS    
!===================================================================================================

!---------------------------------------------------------------------------------------------------
! subroutine qui permet d'écrire un array 2D qui contient l'image 2D, cette subroutine est appelée en mettant frame_cond = 1 en input.config
!---------------------------------------------------------------------------------------------------
SUBROUTINE CREER_FRAME(VAR,INDEX)

	IMPLICIT NONE
	INTEGER, INTENT(IN) :: INDEX
	REAL(KIND=XP),INTENT(IN),DIMENSION(NX) :: VAR
	INTEGER,PARAMETER :: SIZE = 2*NX
	INTEGER :: CENTER,I,J,IND,UNTY
	REAL(KIND=XP),DIMENSION(SIZE,SIZE) :: IMG 
	CHARACTER(LEN=1024) :: FRAME_NAME
	CHARACTER(LEN=1024) :: NUMB
	REAL(KIND=XP):: NAN_VALUE 
	
	NAN_VALUE = 0.0
	NAN_VALUE = 0.0/NAN_VALUE
	11 FORMAT(I0)
	WRITE(NUMB,11) INDEX
	FRAME_NAME = 'frame_'//TRIM(NUMB)//'.out'
        
        
        CENTER = SIZE/2
        DO I=1,SIZE
                 DO J=1,SIZE
                 
                          IND = (ABS(I-CENTER)**2+ABS(J-CENTER)**2)**0.5
                          IF (IND<SIZE/2) THEN
                          		IMG(I,J) = VAR(IND)
                          ELSE 
                          
                          IMG(I,J) = NAN_VALUE
                          
                          ENDIF
                 ENDDO
        ENDDO
        
        OPEN(UNIT=UNTY,FILE="frame_array/"//TRIM(ADJUSTL(FRAME_NAME)),ACTION='WRITE')
        DO I=1,SIZE
        WRITE(UNTY,"(200(1pE12.4, 2X))") IMG(I,:)
        ENDDO
        
        CLOSE(UNTY)

END SUBROUTINE CREER_FRAME
!---------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------
SUBROUTINE FRAME(VAR,FRAME_COND,INDEX)
!---------------------------------------------------------------------------------------------------
!> Cette subroutine est appelée dans la first_branch pour ecrire un fichier stocké dans le folder frame_array contenant l'image 2D à plot en pyhon
!---------------------------------------------------------------------------------------------------

    IMPLICIT NONE
    INTEGER, INTENT(IN) :: FRAME_COND
    INTEGER, INTENT(IN) :: INDEX
    REAL(KIND=XP), INTENT(IN) :: VAR(NX)
    
     IF (FRAME_COND==1)THEN
        CALL CREER_FRAME(VAR,INDEX)
     ENDIF

END SUBROUTINE FRAME
!---------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------
SUBROUTINE SCHEMA_TH_TIME()
!---------------------------------------------------------------------------------------------------
!> Cette subroutine itère depuis une température et densité de surface initiale jusqu'à converger 
!> vers Q+ - Q- = 0.
!> On considère que cette convergence se fait sur un temps thermique << temps visqueux et donc que
!> la densité de surface est constante.
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE
    
    REAL(KIND=XP) :: SWITCH      !! valeur d'arrêt de boucle pour Q+ - Q-
    INTEGER :: I
    
    SWITCH = 1.0e-15_xp
    
    ! Lancement de la boucle qui tournera tant que Q+ - Q- est > switch
    I=0
    
    DO WHILE( MAXVAL(ABS(Q_PLUS_AD - Q_MOINS_AD)) > SWITCH)
              
        CALL ITERATION_TEMP_AD()   ! on appel le schéma de l'équation de T
        CALL COMPUTE_EQS()         ! on calcul le reste des variables
        
        ! Affichage pour observer l'évolution du système ( q+-q- et m_dot)
        IF (MODULO(I,50000)==1) THEN
            WRITE (*,"('Q+-Q- = ',1pE12.4,'  ABS(M_DOT-1) = ',1pE12.4)")&
                & MAXVAL(ABS(Q_PLUS_AD - Q_MOINS_AD)), &
                & ABS(MINVAL(M_DOT_AD-1.0_xp))
        ENDIF
        
        TIME_AD = TIME_AD + DELTA_T_TH_AD
        
        I=I+1
              
    ENDDO
    
    NB_IT_TH = I

    ! Affichage des variables de sortie de boucle
    WRITE(*,"('Nombre d iterations thermique       : ',I12)") NB_IT_TH
    WRITE(*,"('Temp thermique adimensionné atteint = ',1pE12.4)") DELTA_T_TH_AD * I

!---------------------------------------------------------------------------------------------------
END SUBROUTINE SCHEMA_TH_TIME
!---------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------
SUBROUTINE SCHEMA_FIRST_BRANCH()
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
    
    DELTA_T_VISQ_AD = FRACTION_DT_VISQ * MAXVAL( X_AD ** 4.0_xp / NU_AD ) 
    DELTA_T_TH_AD = FRACTION_DT_TH / MAXVAL(OMEGA_AD)
    
    ! Génération des grilles de calcul pour le schema implicit de S
    CALL CREER_LAMBDA()
     
    !AFfichage des données d'entrée de boucle
    WRITE(*,"(48('-'))")
    WRITE(*,"(48('-'))")
    
    WRITE(*,"('PAS DE TEMPS VISQUEUX          DELTA_T_VISQ_AD = ',1pE12.4)") DELTA_T_VISQ_AD
    WRITE(*,"('PAS DE TEMPS THERMIQUE           DELTA_T_TH_AD = ',1pE12.4)") DELTA_T_TH_AD
    
    WRITE(*,"(48('-'))")
    WRITE(*,"(48('-'))")
    
    M_DOT_MIN = MAXVAL(ABS(M_DOT_AD-1.0_xp))
    
    ! lancement boucle pour arriver à m_dot = 1
    ITE=1
    I = 0
    DO WHILE((M_DOT_MIN>=0.01_xp).and.(ITE<56)) !55 pour proche instab
            
            
        WRITE(*,"(48('-'))")
        WRITE(*,"(I0,'e iteration de temps thermique ')") ITE 
        
        ! Ecriture avant itérations du schéma numérique thermique
        CALL ADIM_TO_PHYSIQUE()
        CALL ECRITURE_DIM()
        
        I = I+1
        !ecriture frame
        CALL FRAME(TEMP,FRAME_COND,I)
        
        CALL SCHEMA_TH_TIME()

        ! Ecriture après itérations
        CALL ADIM_TO_PHYSIQUE()
        CALL ECRITURE_DIM()
        
        I=I+1
        !ecriture frame
        CALL FRAME(TEMP,FRAME_COND,I)

        S_SAVE = S_AD
        CALL COMPUTE_Q_ADV_AD(DELTA_T_VISQ_AD,S_SAVE)
        CALL SCHEMA_IMPLICITE_S(NU_AD)
        
        CALL COMPUTE_EQS()
        
        TIME_AD = TIME_AD + DELTA_T_VISQ_AD - NB_IT_TH * DELTA_T_TH_AD
        ITE=ITE+1
        M_DOT_MIN = ABS(MINVAL(M_DOT_AD-1.0_xp))
            
    ENDDO  

!---------------------------------------------------------------------------------------------------
END SUBROUTINE SCHEMA_FIRST_BRANCH
!---------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------
SUBROUTINE SCHEMA_SECOND_BRANCH(FRACTION_DT_INSTABLE)
!---------------------------------------------------------------------------------------------------
!> Calcul précis de l'instabilité, à la fraction de temps caracteristique donnée en entrée (usuel 10e-7)
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE
    INTEGER :: iterateur
    REAL(KIND=xp) :: FRACTION_DT_INSTABLE


    DELTA_T_INSTABLE_AD = FRACTION_DT_INSTABLE * MAXVAL( X_AD ** 4.0_xp / NU_AD )
    CALL SETUP_SCHEMA_INSTABLE_TS
    
    WRITE(*,"(48('-'))")
    WRITE(*,"(48('-'))")
    WRITE(*,"('BOUCLE INSTABLE            DELTA_T_INSTABLE_AD = ',1pE12.4)") DELTA_T_INSTABLE_AD
    WRITE(*,"(48('-'))")
    WRITE(*,"(48('-'))")

    DO iterateur=1, 1000000
    
        CALL SCHEMA_INSTABLE_TS(1.0_xp)
        CALL COMPUTE_EQS
        TIME_AD=TIME_AD+DELTA_T_INSTABLE_AD
        
        IF (MODULO(iterateur,10000)==0) THEN
            CALL ADIM_TO_PHYSIQUE()
            CALL ECRITURE_DIM()
        ENDIF
        
    END DO
    
    DELTA_T_INSTABLE_AD = FRACTION_DT_INSTABLE * 1.0E-1 * MAXVAL( X_AD ** 4.0_xp / NU_AD )
    CALL SETUP_SCHEMA_INSTABLE_TS
    
    DO iterateur=1, 1500000
    
        CALL SCHEMA_INSTABLE_TS(1.0_xp)
        CALL COMPUTE_EQS
        TIME_AD=TIME_AD+DELTA_T_INSTABLE_AD
        
        IF (MODULO(iterateur,100)==0) THEN
            CALL ADIM_TO_PHYSIQUE()
            CALL ECRITURE_DIM()
        ENDIF
        
    END DO

!---------------------------------------------------------------------------------------------------
END SUBROUTINE SCHEMA_SECOND_BRANCH
!---------------------------------------------------------------------------------------------------
                          END MODULE MODULE_BOUCLE
!---------------------------------------------------------------------------------------------------
