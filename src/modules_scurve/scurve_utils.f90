!===================================================================================================
            MODULE SCURVE_UTILS
!===================================================================================================
!> Pour le calcul des courbes en S, la fonction à annuler par dichotomie pour chaque branche
!> épaisse ou mince est de la forme :
!> alpha * (Pgaz + Prad) + beta * third_terme = 0
!> Ce module permet de calculer les différents termes selon les méthodes de dichotomie
!===================================================================================================

USE DECLARATIONS
IMPLICIT NONE
    
!===================================================================================================
            CONTAINS 
!===================================================================================================

SUBROUTINE calc_H(TEMP_AD_, X_AD_, OMEGA_AD_, S_AD_, H_AD_)
!---------------------------------------------------------------------------------------------------
!> Calcul de H_AD en résolvant le trinôme :
!> H_AD**2 - B_0 * B_AD - C_0 * C_AD = 0
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE
    REAL(KIND=xp), INTENT(IN)   :: TEMP_AD_, OMEGA_AD_, S_AD_, X_AD_
    REAL(KIND=xp), INTENT(INOUT):: H_AD_
    REAL(KIND=xp)               :: TEMP_, OMEGA_, S_
    REAL(KIND=xp)               :: A2, B2, C2, DELTA    !! Coefficients & déterminant trinôme

    ! Variables redimensionnées
    TEMP_  = TEMP_AD_ * TEMP_0
    OMEGA_ = OMEGA_AD_ * OMEGA_MAX
    S_     = S_AD_ * S_0 / X_AD_

    ! Calcul des coefficients
    A2 = 0.5_xp * OMEGA_**2._xp * S_
    B2 = - (1._xp / 3._xp) * A_RADIATION * TEMP_**4._xp 
    C2 = - R_BOLTZ * TEMP_ * S_ / (2._xp * MU)

    ! Déterminant trinôme
    DELTA = B2**2._xp - 4._xp * A2 * C2

    ! Racine positive du trinôme
    H_AD_ = -0.5_xp * (B2 + SIGN(SQRT(DELTA),B2)) / A2

    ! Adimensionnement
    H_AD_ = H_AD_ / R_S

!---------------------------------------------------------------------------------------------------
END SUBROUTINE calc_H
!---------------------------------------------------------------------------------------------------

SUBROUTINE AddToList(list, element)
!---------------------------------------------------------------------------------------------------
!> Append des éléments sur une liste
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: element
    INTEGER             :: i, isize
    INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: list
    INTEGER, DIMENSION(:), ALLOCATABLE                :: clist

    IF ( allocated( list ) ) THEN
        isize = size(list)
        ALLOCATE( clist(isize+1) )

        DO i=1,isize   
            clist(i) = list(i)
        END DO

        clist(isize+1) = element

        DEALLOCATE(list)
        CALL move_alloc( clist, list)

    ELSE
        ALLOCATE(list(1))
        list(1) = element
    END IF

!---------------------------------------------------------------------------------------------------
END SUBROUTINE AddToList
!---------------------------------------------------------------------------------------------------

SUBROUTINE POINTS_CRITIQUES()
!---------------------------------------------------------------------------------------------------
!> Donne les points critiques de la branche épaisse des courbes en S correspondant à chaque rayon du disque
!> n_lines donne le nombre de lignes du fichier csv utilisé
!> n_per_radius donne le nombre de lignes par rayon du fichier CSV
!> n_radii donne le nombre de rayons différents
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE
    REAL, ALLOCATABLE    :: S_DATA(:,:,:)
    INTEGER, ALLOCATABLE :: POS_TAB(:), GAP_TAB(:)
    REAL, ALLOCATABLE    :: PC_TAB(:,:)
    INTEGER              :: i, j, k, rc, n_lines, n_per_radius, n_radii, GAP_MAX, GAP, j_max
    REAL                 :: junk1, junk2, junk3, old_radius, new_radius, max_sigma

    !--------------------------------------------------------------------------------------------------------------------------------
    ! On détecte le nombre de lignes dans le fichier csv
    !--------------------------------------------------------------------------------------------------------------------------------
    OPEN (UNIT = 1, FILE = "./output/scurve/epais.out", IOSTAT = rc)
    n_lines = 0

    DO
        READ(1, *, IOSTAT = rc)
        IF (rc /= 0) EXIT

        n_lines = n_lines + 1
    END DO

    CLOSE(1)

    !--------------------------------------------------------------------------------------------------------------------------------
    ! On détecte la position de changement de rayon dans le fichier que l'on stocke dans l'array POS_TAB
    !--------------------------------------------------------------------------------------------------------------------------------
    OPEN (UNIT = 10, FILE = "./output/scurve/epais.out")
    READ(10 ,*) junk1, junk2, junk3, old_radius
    CLOSE(10)

    OPEN (UNIT = 1, FILE = "./output/scurve/epais.out")
    i=1
    n_per_radius = 0

    DO
        READ(1, *, IOSTAT = rc) junk1, junk2, junk3, new_radius
        IF (rc/=0) EXIT

        IF (new_radius .NE. old_radius) THEN
            CALL AddToList(POS_TAB, i)
            old_radius = new_radius
        END IF

        i = i + 1
        
    END DO

    CLOSE(1)

    !--------------------------------------------------------------------------------------------------------------------------------
    ! A partir de cet array, on détecte le nombre d'éléments par rayon (dans GAP_TAB) et le nombre d'éléments maximal (dans GAP_MAX)
    !--------------------------------------------------------------------------------------------------------------------------------
    i = 2
    GAP_MAX = POS_TAB(1)
    ALLOCATE(GAP_TAB(1))
    GAP_TAB(1) = POS_TAB(1) - 1
    
    DO WHILE(i <= SIZE(POS_TAB)) 
        
        GAP = POS_TAB(i) - POS_TAB(i-1)
        CALL AddToList(GAP_TAB, GAP)

        IF (GAP > GAP_MAX) THEN
            GAP_MAX = GAP
        END IF
        i = i + 1

    END DO

    ! Traitement du dernier point
    CALL AddToList(GAP_TAB, n_lines-POS_TAB(SIZE(POS_TAB)))

    !--------------------------------------------------------------------------------------------------------------------------------
    ! On construit d'abord le tableau correspondant au fichier en sortie de la subroutine s_curve
    ! Ce tableau est en 3D, avec:
    ! 1ère dimension T, Sigma, x_ad, r, dimension 4
    ! 2ème dimension la courbe en S pour un rayon, dimension GAP_MAX
    ! 3ème dimension le rayon, dimension n_radii
    !--------------------------------------------------------------------------------------------------------------------------------
    OPEN (UNIT = 1, FILE = "./output/scurve/epais.out")

    n_radii = SIZE(POS_TAB) + 1  ! +1 pour compter le dernier point

    ALLOCATE(S_DATA(4, GAP_MAX, n_radii))
    i = 1
    j = 1
    
    DO WHILE (j <= n_radii)

        k = 1

        DO WHILE (k <= GAP_MAX)

            IF (k <= GAP_TAB(j)) THEN
                READ(1, *) S_DATA(1, k, j ), S_DATA(2, k, j), S_DATA(3, k, j), S_DATA(4, k, j)

            !------------------------------------------------------------------------------------------------------------------------
            ! Pour les courbes en S avec moins de données, on remplit chaque sous-tableau par des 0 jusqu'à GAP_MAX
            !------------------------------------------------------------------------------------------------------------------------

            ELSE IF (k > GAP_TAB(j)) THEN
                S_DATA(1, k, j ) = 0
                S_DATA(2, k, j ) = 0
                S_DATA(3, k, j ) = 0
                S_DATA(4, k, j ) = 0

            END IF

            k = k + 1

        END DO

        j = j + 1

    END DO

    CLOSE (1)

    !--------------------------------------------------------------------------------------------------------------------------------
    ! On construit ensuite un autre array PC_TAB avec la position des points critiques pour chaque rayon 
    ! en calculant le max en sigma de chaque courbe en S
    !--------------------------------------------------------------------------------------------------------------------------------
    ALLOCATE(PC_TAB(4, n_radii))
    i=1

    DO WHILE (i <= n_radii)
        j = 1
        MAX_SIGMA = S_DATA(2, 1, i)
        j_max = 1

        DO WHILE (j < GAP_MAX)

            IF (S_DATA(2, j, i) > MAX_SIGMA) THEN

                MAX_SIGMA = S_DATA(2, j, i)
                j_max = j

            END IF
        
            j = j + 1
        
        END DO
        PC_TAB(:, i) = S_DATA(:, j_max, i)

        i = i + 1

    END DO

    !--------------------------------------------------------------------------------------------------------------------------------
    ! On écrit dans un fichier spécifique les coordonnées du point d'inflexion de la branche épaisse des courbes en S.
    ! Cela correspond à un array de dimension (n_radii, 4)
    !--------------------------------------------------------------------------------------------------------------------------------
    OPEN (UNIT = 5, FILE = "./output/scurve/coord_turning_points.out")

    DO i=1, n_radii

        WRITE(5, *) PC_TAB(:, i)

    END DO

    CLOSE(5)

!---------------------------------------------------------------------------------------------------
END SUBROUTINE POINTS_CRITIQUES
!---------------------------------------------------------------------------------------------------

SUBROUTINE LECTURE_POINTS_CRITIQUES()
!---------------------------------------------------------------------------------------------------
!> Lit le fichier généré par POINTS_CRITIQUES() et stocke les valeurs associées à chaque rayon
!> dans les variables globales TEMP_CRITIQUE et SIGMA_CRITIQUE de dimension NX
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE
    INTEGER :: I, UNTC
    LOGICAL :: EXI

    ! On vérifie que le fichier existe. Sinon, on le crée.
    INQUIRE(FILE="output/scurve/coord_turning_points.out", EXIST=EXI)
    IF (.not.EXI) CALL POINTS_CRITIQUES()

    ! Ouvre le fichier
    OPEN(newunit=UNTC, file='output/scurve/coord_turning_points.out', status='old', action='read')

    ! Lecture points critiques
    DO I=1,NX
        READ(UNTC, *) TEMP_CRITIQUE(I), SIGMA_CRITIQUE(I)
    ENDDO

!---------------------------------------------------------------------------------------------------
END SUBROUTINE LECTURE_POINTS_CRITIQUES
!---------------------------------------------------------------------------------------------------

SUBROUTINE calc_QpmQm (Temp_S_AD, S_S_AD, ipos, QpmQm)
!---------------------------------------------------------------------------------------------------
!> Subroutine qui calcule Q+ - Q- pour un triplet (Temp, Sigma, ipos) donné pour la branche épaisse
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE
    REAL(KIND=xp),  INTENT(in)  :: Temp_S_AD
    REAL(KIND=xp),  INTENT(in)  :: S_S_AD
    INTEGER,        INTENT(in)  :: ipos
    REAL(KIND=xp),  INTENT(out) :: QpmQm

    REAL(KIND=xp) :: H_S_AD
    REAL(KIND=xp) :: rho_S_AD
    REAL(KIND=xp) :: Kff_S
    REAL(KIND=xp) :: Tau_S_eff
    REAL(KIND=xp) :: nu_S_AD
    REAL(KIND=xp) :: Fz_S
    REAL(KIND=xp) :: Q_plus_S
    REAL(KIND=xp) :: Q_moins_S

    ! Calculs pour obtenir Q+ et Q-, branche épaisse
    CALL calc_H(Temp_S_AD, X_AD(ipos), OMEGA_AD(ipos), S_S_AD, H_S_AD)
    rho_S_AD  = S_S_AD / ( X_AD(ipos) * H_S_AD )
    nu_S_AD   = 0.5_xp * OMEGA_AD(ipos) * H_S_AD**2._xp
    Kff_S     = 6.13E18 * rho_S_AD * Temp_S_AD**(-7._xp/2._xp) * rho_0 * TEMP_0 **(-7._xp/2._xp)
    Tau_S_eff = 0.5_xp * S_S_AD / X_AD(ipos) * S_0 * SQRT(KAPPA_E * Kff_S)
    Fz_S      = F_Z_DIFF_0 * X_AD(ipos) * Temp_S_AD**4._xp / ((KAPPA_E + Kff_S) * S_S_AD)

    Q_plus_S  = nu_S_AD * OMEGA_AD(ipos)**2._xp * Q_PLUS_0
    Q_moins_S = 2._xp * X_AD(ipos) * Fz_S / (S_S_AD * S_0)
    QpmQm     = Q_plus_S - Q_moins_S

!---------------------------------------------------------------------------------------------------
END SUBROUTINE calc_QpmQm
!---------------------------------------------------------------------------------------------------

SUBROUTINE map_QpmQm (T_min_map_AD, T_max_map_AD, S_min_map_AD, S_max_map_AD, n_map)
!---------------------------------------------------------------------------------------------------
!> Subroutine qui calcule la carte des valeurs de Q+ - Q- à tous les rayons dans l'espace Temp-Sigma
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE
    REAL(KIND=xp), INTENT(in)                          :: T_min_map_AD, T_max_map_AD   !! Températures minimales et maximales où calculer Q+-Q-
    REAL(KIND=xp), INTENT(in)                          :: S_min_map_AD, S_max_map_AD   !! Densités de surface minimales et maximales où calculer Q+-Q-
    INTEGER,       INTENT(in)                          :: n_map                        !! Nombre de températures et de densités de surface où calculer Q+-Q-

    REAL(KIND=xp), DIMENSION(n_map)                    :: QpmQm_map                    !! Tableau de Q+-Q- à une température fixe
    REAL(KIND=xp)                                      :: T_map_AD                     !! Température où Q+-Q- est calculé
    REAL(KIND=xp)                                      :: S_map_AD                     !! Densité de surface où Q+-Q- est calculé
    INTEGER                                            :: j, k                         !! Indices de boucles
    INTEGER                                            :: ipos                         !! Indice pour la position

    OPEN (unit=10, file="./output/scurve/map.out", status="unknown")

    ! On parcourt les positions dans le disque
    DO ipos=1,NX

        ! On initialise T et S
        T_map_AD = T_min_map_AD
        S_map_AD = S_min_map_AD

        CALL calc_QpmQm(T_map_AD, S_map_AD, ipos, QpmQm_map(1))

        ! On parcourt les densités de surface à T fixé et on calcule Q+-Q-
        DO j=2,n_map
            
            S_map_AD = S_map_AD + ( S_max_map_AD - S_min_map_AD ) / n_map

            CALL calc_QpmQm(T_map_AD, S_map_AD, ipos, QpmQm_map(j))

        ENDDO
        
        ! On écrit les Q+-Q- à une température fixée
        WRITE(10,*) QpmQm_map

        ! On parcourt les températures
        DO k=2,n_map
            
            ! On incrémente T et on réinitialise S
            T_map_AD = T_map_AD + ( T_max_map_AD - T_min_map_AD ) / n_map
            S_map_AD = S_min_map_AD

            CALL calc_QpmQm(T_map_AD, S_map_AD, ipos, QpmQm_map(1))

            ! On parcourt les densités de surface à T fixé et on calcule Q+-Q-
            DO j=2,n_map
            
                S_map_AD = S_map_AD + ( S_max_map_AD - S_min_map_AD ) / n_map

                CALL calc_QpmQm(T_map_AD, S_map_AD, ipos, QpmQm_map(j))

            ENDDO

            ! On écrit les Q+-Q- à une température fixée
            WRITE(10,*) QpmQm_map

        ENDDO

    ENDDO

    CLOSE(10)

!---------------------------------------------------------------------------------------------------
END SUBROUTINE map_QpmQm
!---------------------------------------------------------------------------------------------------

SUBROUTINE coupe_QpmQm (T_min_map_AD, T_max_map_AD, S_coupe_AD, n_map)
!---------------------------------------------------------------------------------------------------
!> Subroutine qui calcule une coupe à Sigma fixé des valeurs de Q+ - Q- à tous les rayons dans l'espace Temp-Sigma
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE
    REAL(KIND=xp), INTENT(in)                          :: T_min_map_AD, T_max_map_AD   !! Températures minimales et maximales où calculer Q+-Q-
    REAL(KIND=xp), INTENT(in)                          :: S_coupe_AD                   !! Densité de surface où Q+-Q- est calculé
    INTEGER,       INTENT(in)                          :: n_map                        !! Nombre de températures où calculer Q+-Q-

    REAL(KIND=xp)                                      :: QpmQm_coupe                  !! Valeur de Q+-Q- à (T,S)
    REAL(KIND=xp)                                      :: T_map_AD                     !! Température où Q+-Q- est calculé
    INTEGER                                            :: j                            !! Indice de boucle
    INTEGER                                            :: ipos                         !! Indice pour la position

    OPEN (unit=10, file="./output/scurve/coupe.out", status="unknown")

    ! On parcourt les positions dans le disque
    DO ipos=1,NX

        ! On initialise T
        T_map_AD = T_min_map_AD

        CALL calc_QpmQm(T_map_AD, S_coupe_AD, ipos, QpmQm_coupe)

        ! On écrit dans le fichier de sortie T, S Q+-Q-, X_AD et r en [SI]
        WRITE(10,*) T_map_AD*TEMP_0, S_coupe_AD * S_0 / X_AD(ipos), QpmQm_coupe, X_AD(ipos), X_AD(ipos)**2._xp * R_S

        ! On parcourt les températures
        DO j=2,n_map
            
            ! On incrémente T et on réinitialise S
            T_map_AD = T_map_AD + ( T_max_map_AD - T_min_map_AD ) / n_map

            CALL calc_QpmQm(T_map_AD, S_coupe_AD, ipos, QpmQm_coupe)

            ! On écrit dans le fichier de sortie T, S Q+-Q-, X_AD et r en [SI]
            WRITE(10,*) T_map_AD*TEMP_0, S_coupe_AD * S_0 / X_AD(ipos), QpmQm_coupe, X_AD(ipos), X_AD(ipos)**2._xp * R_S

        ENDDO

    ENDDO

    CLOSE(10)

!---------------------------------------------------------------------------------------------------
END SUBROUTINE coupe_QpmQm
!---------------------------------------------------------------------------------------------------

!===================================================================================================
END MODULE SCURVE_UTILS
!===================================================================================================