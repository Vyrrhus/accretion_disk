!===================================================================================================
            MODULE MODULE_SCHEMAS_SIGMA
!===================================================================================================
!> Ce module contient :
!> Le schéma implicite pour le calcul de S
!===================================================================================================
USE module_declarations

IMPLICIT NONE

!!REELS
REAL(KIND=xp) :: DELTA_T_visq=10 ! pas de temps visqueux

!!TABLEAUX
REAL(KIND=xp), DIMENSION(Nx) :: LAMBDA             !! Tableaux des lambda_i

!!MATRICE BANDES 
REAL(KIND=xp), DIMENSION(Nx-1) :: MATRICE_DL  !! sous diagonale
REAL(KIND=xp), DIMENSION(Nx) :: MATRICE_D !! diagonale centrale
REAL(KIND=xp), DIMENSION(Nx-1) :: MATRICE_DU !! sur diagonale
REAL(KIND=xp), DIMENSION(Nx-2) :: MATRICE_DUU !! sur sur diagonale  (pour lapack)


!===================================================================================================
            CONTAINS 
!===================================================================================================

SUBROUTINE SCHEMA_IMPLICITE_S(NU_AD_bis)
!---------------------------------------------------------------------------------------------------
!>    Cette routine prend en entrée les valeurs de NU_AD (viscosité) pour calculer au pas de temps
!>    visqueux suivant la densité surfacique S_AD
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE

    !!variables d'entrée
    REAL(KIND=xp), DIMENSION(Nx), INTENT(in) :: NU_AD_bis


    REAL(KIND=xp), DIMENSION(Nx) :: IPIV !! indices de pivots (pour lapack)
    INTEGER :: INFO !pour LAPACK


    CALL CREER_MATRICE_TRIDIAGONALE(-1.0_xp*LAMBDA, NU_AD_bis) !création de la matrice du schéma

    
    !!On calcule le membre de gauche du schéma implicite : il faut ajouter le terme de bord au dernier indice
    S_AD(Nx) = S_AD(Nx) + DX*LAMBDA(Nx)

    !!On doit inverser le systeme pour obtenir S_AD au temps suivant:

    !! factorisation LU
    CALL DGTTRF(Nx, MATRICE_DL, MATRICE_D, MATRICE_DU, MATRICE_DUU, IPIV, INFO)

    
    !!resolution
    CALL DGTTRS('N', Nx, 1, MATRICE_DL, MATRICE_D, MATRICE_DU, MATRICE_DUU, IPIV, S_AD, NX, INFO)

    
    !!S_AD est maintenant au temps suivant


!---------------------------------------------------------------------------------------------------
END SUBROUTINE SCHEMA_IMPLICITE_S
!---------------------------------------------------------------------------------------------------



SUBROUTINE CREER_LAMBDA()
!---------------------------------------------------------------------------------------------------
!>    Cette routine créer le tableau LAMBDA (une fois pour toutes) à partir de X_AD et du pas de temps DELTA_T
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE

    !!variables internes
    INTEGER :: iter

    DO iter=1, Nx
        LAMBDA(iter) = DELTA_T_visq / (DX**2 * X_AD(iter)**2 )
    END DO

!---------------------------------------------------------------------------------------------------
END SUBROUTINE CREER_LAMBDA
!---------------------------------------------------------------------------------------------------

SUBROUTINE CREER_MATRICE_TRIDIAGONALE(pmLAMBDA, NU_AD_bis)
!---------------------------------------------------------------------------------------------------
!>    Cette routine créer la matrice bande qui apparait dans les schémas:
!>    pmLambda est positif (egal a +LAMBDA) pour la matrice explicite, négatif (égal a -LAMBDA) pour
!>    la matrice implicite
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE

    REAL(KIND=xp), DIMENSION(Nx), INTENT(in) :: pmLAMBDA
    REAL(KIND=xp), DIMENSION(Nx), INTENT(in) :: NU_AD_bis

    INTEGER :: iter

    !! MISE EN PLACE DE LA SOUS DIAGONALE DL

    MATRICE_DL(1)=0
    DO iter=2, Nx-2
        MATRICE_DL(iter)=pmLAMBDA(iter+1)*NU_AD_bis(iter)
    END DO
    MATRICE_DL(Nx-1)=2.0_xp*pmLAMBDA(Nx)*NU_AD_bis(Nx-1)

    !! MISE EN PLACE DE LA DIAGONALE D
    
    MATRICE_D(1)=1
    DO iter=2, Nx
        MATRICE_D(iter)=1-2*pmLAMBDA(iter)*NU_AD_bis(iter)
    END DO
    
    !! MISE EN PLACE DE LA SUR DIAGONALE DU
    
    MATRICE_DU(1)=0
    DO iter=2, Nx-1
        MATRICE_DU(iter)=pmLAMBDA(iter)*NU_AD_bis(iter+1)
    END DO

    !! MISE EN PLACE DE LA SUR SUR DIAGONALE DUU (utilisée par Lapack dans la décompo LU)

    MATRICE_DUU=0.0_xp

!---------------------------------------------------------------------------------------------------
END SUBROUTINE CREER_MATRICE_TRIDIAGONALE
!---------------------------------------------------------------------------------------------------


!===================================================================================================
END MODULE MODULE_SCHEMAS_SIGMA
!===================================================================================================

