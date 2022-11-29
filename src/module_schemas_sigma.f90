!===================================================================================================
            MODULE MODULE_SCHEMAS_SIGMA
!===================================================================================================
!> Ce module contient :
!> Le schéma implicite pour le calcul de S
!===================================================================================================
USE module_declarations

IMPLICIT NONE

!!REELS
REAL(KIND=xp) :: DELTA_T_visq=1 ! pas de temps visqueux

!!TABLEAUX
REAL(KIND=xp), DIMENSION(Nx) :: LAMBDA             !! Tableaux des lambda_i

!!MATRICE BANDES 
!matrice A pour le schéma implicite (aussi utilisée par CN):
REAL(KIND=xp), DIMENSION(Nx-1) :: MATRICE_A_DL  !! sous diagonale
REAL(KIND=xp), DIMENSION(Nx) :: MATRICE_A_D !! diagonale centrale
REAL(KIND=xp), DIMENSION(Nx-1) :: MATRICE_A_DU !! sur diagonale
REAL(KIND=xp), DIMENSION(Nx-2) :: MATRICE_A_DUU !! sur sur diagonale  (pour lapack)

!matrice B pour Crank-Nicholson
REAL(KIND=xp), DIMENSION(Nx-1) :: MATRICE_B_DL  !! sous diagonale
REAL(KIND=xp), DIMENSION(Nx) :: MATRICE_B_D !! diagonale centrale
REAL(KIND=xp), DIMENSION(Nx-1) :: MATRICE_B_DU !! sur diagonale
REAL(KIND=xp), DIMENSION(Nx-2) :: MATRICE_B_DUU !! sur sur diagonale  (pour lapack)
REAL(KIND=xp), DIMENSION(3, Nx) :: MATRICE_B_BLAS !!format de B pour BLAS
!===================================================================================================
            CONTAINS 
!===================================================================================================

SUBROUTINE SCHEMA_IMPLICITE_S(NU_AD_bis)
!---------------------------------------------------------------------------------------------------
!>    Cette routine prend en entrée les valeurs de NU_AD_bis (viscosité) pour calculer au pas de temps
!>    visqueux suivant la densité surfacique S_AD, avec un schéma implicite
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE

    !!variables d'entrée
    REAL(KIND=xp), DIMENSION(Nx), INTENT(in) :: NU_AD_bis


    REAL(KIND=xp), DIMENSION(Nx) :: IPIV !! indices de pivots (pour lapack)
    INTEGER :: INFO !pour LAPACK


    !création de la matrice du schéma
    CALL CREER_MATRICE_TRIDIAGONALE(MATRICE_A_DL, MATRICE_A_D, MATRICE_A_DU, MATRICE_A_DUU, -1.0_xp*LAMBDA, NU_AD_bis)

    
    !!On calcule le membre de gauche du schéma implicite : il faut ajouter le terme de bord au dernier indice
    S_AD(Nx) = S_AD(Nx) + DX*LAMBDA(Nx)

    !!On doit inverser le systeme pour obtenir S_AD au temps suivant:

    !! factorisation LU
    CALL DGTTRF(Nx, MATRICE_A_DL, MATRICE_A_D, MATRICE_A_DU, MATRICE_A_DUU, IPIV, INFO)

    
    !!resolution
    CALL DGTTRS('N', Nx, 1, MATRICE_A_DL, MATRICE_A_D, MATRICE_A_DU, MATRICE_A_DUU, IPIV, S_AD, NX, INFO)

    
    !!S_AD est maintenant au temps suivant


!---------------------------------------------------------------------------------------------------
END SUBROUTINE SCHEMA_IMPLICITE_S
!---------------------------------------------------------------------------------------------------


SUBROUTINE SCHEMA_CN_S(NU_AD_bis, PARAM_CN)
!---------------------------------------------------------------------------------------------------
!>    Cette routine prend en entrée les valeurs de NU_AD_bis (viscosité) pour calculer au pas de temps
!>    visqueux suivant la densité surfacique S_AD, avec un Crank-Nicholson de parametre PARAM_CN
!>    entre 0 et 1.
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE

    !!variables d'entrée
    REAL(KIND=xp), DIMENSION(Nx), INTENT(in) :: NU_AD_bis
    REAL(KIND=xp), INTENT(in) :: PARAM_CN

    REAL(KIND=xp), DIMENSION(Nx):: S_AD_temp

    REAL(KIND=xp), DIMENSION(Nx) :: IPIV !! indices de pivots (pour lapack)
    INTEGER :: INFO !pour LAPACK

    !création des matrices A et B
    CALL CREER_MATRICE_TRIDIAGONALE(MATRICE_A_DL, MATRICE_A_D, MATRICE_A_DU, MATRICE_A_DUU, -1.0_xp*LAMBDA*PARAM_CN, NU_AD_bis)
    
    CALL CREER_MATRICE_TRIDIAGONALE(MATRICE_B_DL, MATRICE_B_D, MATRICE_B_DU, MATRICE_B_DUU, LAMBDA*(1.0_xp-PARAM_CN), NU_AD_bis)
    
    !mise en forme de B pour BLAS:
    MATRICE_B_BLAS(1, 2:Nx)=MATRICE_B_DU
    MATRICE_B_BLAS(2, 1:Nx)=MATRICE_B_D
    MATRICE_B_BLAS(3, 1:Nx-1)=MATRICE_B_DL

    !!On calcule le membre de gauche du schéma de CN : il faut multipler par B le S_AD du temps précédent
    
    CALL DGBMV('N', NX, NX, 1, 1, 1.0_xp, MATRICE_B_BLAS, 3, S_AD, 1, 0.0_xp, S_AD_temp, 1)

    S_AD=S_AD_temp !S_AD_temp contient le résultat de B * S_AD
    
    !!puis ajouter le terme de condition initiale

    S_AD(Nx) = S_AD(Nx) + DX*LAMBDA(Nx)

    

    !!On doit inverser la matrice A pour obtenir S_AD au temps suivant:

    !! factorisation LU
    CALL DGTTRF(Nx, MATRICE_A_DL, MATRICE_A_D, MATRICE_A_DU, MATRICE_A_DUU, IPIV, INFO)

    
    !!resolution
    CALL DGTTRS('N', Nx, 1, MATRICE_A_DL, MATRICE_A_D, MATRICE_A_DU, MATRICE_A_DUU, IPIV, S_AD, NX, INFO)

    
    !!S_AD est maintenant au temps suivant


!---------------------------------------------------------------------------------------------------
END SUBROUTINE SCHEMA_CN_S
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

SUBROUTINE CREER_MATRICE_TRIDIAGONALE(MATRICE_DL, MATRICE_D, MATRICE_DU, MATRICE_DUU, pmLAMBDA, NU_AD_bis)
!---------------------------------------------------------------------------------------------------
!>    Cette routine créer la matrice bande qui apparait dans les schémas:
!>    pmLambda est positif (egal a +LAMBDA) pour la matrice explicite, négatif (égal a -LAMBDA) pour
!>    la matrice implicite, égal à +-PARAM_CN*LAMBDA pour Crank-Nicholson.
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE

    !MATRICE
    REAL(KIND=xp), DIMENSION(Nx-1) :: MATRICE_DL  !! sous diagonale
    REAL(KIND=xp), DIMENSION(Nx) :: MATRICE_D !! diagonale centrale
    REAL(KIND=xp), DIMENSION(Nx-1) :: MATRICE_DU !! sur diagonale
    REAL(KIND=xp), DIMENSION(Nx-2) :: MATRICE_DUU !! sur sur diagonale

    REAL(KIND=xp), DIMENSION(Nx), INTENT(in) :: pmLAMBDA
    REAL(KIND=xp), DIMENSION(Nx), INTENT(in) :: NU_AD_bis

    INTEGER :: iter

    !! MISE EN PLACE DE LA SOUS DIAGONALE DL

    DO iter=1, Nx-2
        MATRICE_DL(iter)=pmLAMBDA(iter+1)*NU_AD_bis(iter)
    END DO
    MATRICE_DL(Nx-1)=2.0_xp*pmLAMBDA(Nx)*NU_AD_bis(Nx-1)

    !! MISE EN PLACE DE LA DIAGONALE D
    
    DO iter=1, Nx
        MATRICE_D(iter)=1-2*pmLAMBDA(iter)*NU_AD_bis(iter)
    END DO
    
    !! MISE EN PLACE DE LA SUR DIAGONALE DU
    
    MATRICE_DU(1)=0
    DO iter=1, Nx-1
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

