!===================================================================================================
            MODULE MODULE_SCHEMAS_SIGMA
!===================================================================================================
!> Ce module contient :
!> Le schéma implicite pour le calcul de S
!===================================================================================================
USE module_declarations

IMPLICIT NONE

!!REELS
REAL(KIND=xp) :: DELTA_T=5.0_xp

!!TABLEAUX
REAL(KIND=xp), DIMENSION(Nx) :: LAMBDA             !! Tableaux des lambda_i

!!MATRICES 
REAL(KIND=xp), DIMENSION(Nx-1) :: MATRICE_DL  !! Matrice apparaissant dans le schéma, diagonale basse
REAL(KIND=xp), DIMENSION(Nx) :: MATRICE_D !! diagonale centrale
REAL(KIND=xp), DIMENSION(Nx-1) :: MATRICE_DU !! diagonale haute

external DGTSV
!===================================================================================================
            CONTAINS 
!===================================================================================================

SUBROUTINE SCHEMA_IMPLICITE_S(S_AD, NU_AD)
!---------------------------------------------------------------------------------------------------
!>    Cette routine prend en entrée les valeurs de S_AD et de NU_AD pour calculer au pas de temps
!>    visqueux suivant S_AD
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE

    !!variables d'entrée
    REAL(KIND=xp), DIMENSION(Nx), INTENT(inout) :: S_AD
    REAL(KIND=xp), DIMENSION(Nx), INTENT(in) :: NU_AD

    !!variables internes au module
    REAL(KIND=xp) :: DELTA_X !pas

    REAL(KIND=xp), DIMENSION(Nx-2) :: MATRICE_DUU !! diagonale sur - haute (pour lapack)
    REAL(KIND=xp), DIMENSION(Nx) :: IPIV !! indices de pivots (pour lapack)
    INTEGER :: INFO !pour LAPACK



    DELTA_X = X_AD(2)-X_AD(1)

    CALL CREER_MATRICE_TRIDIAGONALE(MATRICE_DL, MATRICE_D, MATRICE_DU, -1.0_xp*LAMBDA, NU_AD)

    !!On calcule le membre de gauche du schéma implicite : il faut ajouter le terme de bord au dernier indice
    S_AD(Nx) = S_AD(Nx) + DELTA_X*LAMBDA(Nx)

    !!On doit inverser le systeme pour obtenir S_AD au temps suivant:

    !! factorisation LU
    CALL DGTTRF(Nx, MATRICE_DL, MATRICE_D, MATRICE_DU, MATRICE_DUU, IPIV, INFO)
    !!resolution
    CALL DGTTRS('N', Nx, 1, MATRICE_DL, MATRICE_D, MATRICE_DU, MATRICE_DUU, IPIV, S_AD, NX, INFO)

    !!S_AD est maintenant au temps suivant


!---------------------------------------------------------------------------------------------------
END SUBROUTINE SCHEMA_IMPLICITE_S
!---------------------------------------------------------------------------------------------------

SUBROUTINE CREER_LAMBDA(LAMBDA)
!---------------------------------------------------------------------------------------------------
!>    Cette routine créer le tableau Lambda (une fois pour toutes) à partir de X_AD et du pas de temps DELTA_T
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE

    !!variables d'entrée
    REAL(KIND=xp), DIMENSION(Nx), INTENT(inout) :: LAMBDA

    !!variables internes
    INTEGER :: iter
    REAL(KIND=xp) :: DELTA_X

    DELTA_X = X_AD(2)-X_AD(1)

    DO iter=1, Nx
        LAMBDA(iter) = DELTA_T / (DELTA_X**2 * X_AD(iter)**2 )
    END DO

!---------------------------------------------------------------------------------------------------
END SUBROUTINE CREER_LAMBDA
!---------------------------------------------------------------------------------------------------

SUBROUTINE CREER_MATRICE_TRIDIAGONALE(MATRICE_DL, MATRICE_D, MATRICE_DU, pmLAMBDA, NU_AD)
!---------------------------------------------------------------------------------------------------
!>    Cette routine créer la matrice bande qui apparait dans les schémas:
!>    Lambda est positif pour la matrice explicite, négatif pour la matrice implicite
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE

    REAL(KIND=xp), DIMENSION(Nx), INTENT(inout) :: MATRICE_D
    REAL(KIND=xp), DIMENSION(Nx-1), INTENT(inout) :: MATRICE_DL
    REAL(KIND=xp), DIMENSION(Nx-1), INTENT(inout) :: MATRICE_DU
    REAL(KIND=xp), DIMENSION(Nx), INTENT(in) :: pmLAMBDA
    REAL(KIND=xp), DIMENSION(Nx), INTENT(in) :: NU_AD

    INTEGER :: iter

    !! MISE EN PLACE DE LA SOUS DIAGONALE DL

    MATRICE_DL(1)=0
    DO iter=2, Nx-2
        MATRICE_DL(iter)=pmLAMBDA(iter+1)*NU_AD(iter)
    END DO
    MATRICE_DL(Nx-1)=2.0_xp*pmLAMBDA(Nx)*NU_AD(Nx-1)

    !! MISE EN PLACE DE LA DIAGONALE D
    
    MATRICE_D(1)=1
    DO iter=2, Nx
        MATRICE_D(iter)=1-2*pmLAMBDA(iter)*NU_AD(iter)
    END DO
    
    !! MISE EN PLACE DE LA DIAGONALE DU
    
    MATRICE_D(1)=0
    DO iter=2, Nx-1
        MATRICE_DU(iter)=pmLAMBDA(iter)*NU_AD(iter+1)
    END DO

!---------------------------------------------------------------------------------------------------
END SUBROUTINE CREER_MATRICE_TRIDIAGONALE
!---------------------------------------------------------------------------------------------------

SUBROUTINE TEST()
!---------------------------------------------------------------------------------------------------
!>    ROUTINE TEST du makefile et program_schema
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE
    PRINT *, 'hello', X_AD

!---------------------------------------------------------------------------------------------------
END SUBROUTINE TEST
!---------------------------------------------------------------------------------------------------



!===================================================================================================
END MODULE MODULE_SCHEMAS_SIGMA
!===================================================================================================

