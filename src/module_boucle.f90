!---------------------------------------------------------------------------------------------------
                               MODULE MODULE_BOUCLE 
!---------------------------------------------------------------------------------------------------
USE MODULE_DECLARATIONS
USE DIMENSIONNEMENT
USE MODULE_FUNCTION
USE MODULE_SCHEMAS_SIGMA
USE MODULE_ECRITURE
USE MODULE_SCHEMAS_T
                               
                               IMPLICIT NONE

REAL(KIND=xp), PARAMETER, PRIVATE :: FRACTION_DT_TH     = 1.0E-2_xp
REAL(KIND=XP), PARAMETER, PRIVATE :: FRACTION_DT_VISQ   = 1.0E-2_XP
INTEGER, PRIVATE :: NB_IT_TH

                                CONTAINS


!! La subroutine schema_th_time fait une boucle sur le schéma numérique de l'équation thermique et calcul ensuite le reste des variables.
!! La boucle s'arrêtera quand Q+-Q- atteindra une valeur de e-17.          

!! La subroutine schema_first_branch fait une boucle en appelant le schema thermique et le schema implicit de S puis recalcul le reste des variables
!! La boucle s'arrêtera quand on arrivera à m_dot égal à 1 dans tout le disque.

!---------------------------------------------------------------------------------------------------
SUBROUTINE SCHEMA_TH_TIME()

    IMPLICIT NONE
    
    REAL(KIND=XP) :: SWITCH      !! valeur d'arrêt de boucle pour Q+ - Q-
    INTEGER :: I
<<<<<<< HEAD
=======
    SWITCH = 1.0e-17_xp
    DELTA_T_TH_AD = FRACTION_DT_TH / MAXVAL(OMEGA_AD)
>>>>>>> be894d54ca4db4cee59a1fd914bcc9afd96028c9
    
    SWITCH = 1.0e-17_xp
    DELTA_T_TH = FRACTION_DT_TH / MAXVAL(OMEGA_AD)       
    
    !! affichage des variables d'entrée de boucle
    
    WRITE(*,"('Pas de temps thermique               DELTA_T_TH = ',1pE12.4)") DELTA_T_TH
    WRITE(*,"('Q+ - Q- = ',1pe12.4,'           Temperature AD = ',1pE12.4)") &
    & MAXVAL(ABS(Q_PLUS_AD - Q_MOINS_AD)) , &
    & TEMP_AD(50)
    
    !! lancement de la boucle qui tournera tant que Q+ - Q- est > switch
    I=0
    DO WHILE(MAXVAL(ABS(Q_PLUS_AD - Q_MOINS_AD)) > SWITCH)
              
              CALL ITERATION_TEMP_AD()   !! on appel le schéma de l'équation de T
              CALL COMPUTE_EQS()         !! on calcul le reste des variables
              
              
              !! afficha pour observer l'évolution du système ( q+-q- et m_dot)
              IF (MODULO(I,10000)==1) THEN
              WRITE (*,"('Q+-Q- = ',1pE12.4,'  M_DOT = ',1pE12.4)")&
              & MAXVAL(ABS(Q_PLUS_AD - Q_MOINS_AD)), &
              & ABS(MINVAL(M_DOT_AD-1.0_xp))
              ENDIF
              
              TIME_AD = TIME_AD + DELTA_T_TH
              
              !! réécriture en dimensionné
              CALL ADIM_TO_PHYSIQUE()
              
              CALL ECRITURE_DIM()
              CALL ECRITURE_ADIM()
              
              I=I+1
              
    ENDDO
    
    NB_IT_TH = I
       
END SUBROUTINE SCHEMA_TH_TIME
!---------------------------------------------------------------------------------------------------
SUBROUTINE SCHEMA_FIRST_BRANCH()

    IMPLICIT NONE
     
    INTEGER :: ITE,I
    DELTA_T_VISQ = FRACTION_DT_VISQ * MAXVAL( X_AD ** 4.0_xp / NU_AD ) 
    
    !! génération des grilles de calcul pour le schema implicit de S
    CALL CREER_LAMBDA()
     
    WRITE(*,"(48('-'))")
    WRITE(*,"(48('-'))")
    
    WRITE(*,"('PAS DE TEMPS VISQUEUX             DELTA_T_VISQ = ',1pE12.4)") DELTA_T_VISQ
    
    WRITE(*,"(48('-'))")
    WRITE(*,"(48('-'))")
    
    ! lancement boucle pour arriver à m_dot = 1
    ITE=1
    DO WHILE(ABS(MINVAL(M_DOT_AD-1.0_xp))>=0.01_xp)
            
            WRITE(*,"(48('-'))")
            WRITE(*,"(I0,'e iteration de temps thermique ')") ITE 
            
            CALL SCHEMA_TH_TIME()
            
            CALL SCHEMA_IMPLICITE_S(NU_AD)
	    
	    WRITE(*,"('S_AD(50) = ',1pE12.4)") S_AD(50)
	     
	    CALL COMPUTE_EQS()
	     
	    WRITE (11,"(2(1pE12.4,2x))") TEMP_AD(30),S_AD(30)
	    
	    TIME_AD = TIME_AD + DELTA_T_VISQ - NB_IT_TH * DELTA_T_TH
	    
	    ITE=ITE+1
             
    ENDDO
     
END SUBROUTINE SCHEMA_FIRST_BRANCH
!---------------------------------------------------------------------------------------------------

!---------------------------------------------------------------------------------------------------
                          END MODULE MODULE_BOUCLE
!---------------------------------------------------------------------------------------------------
