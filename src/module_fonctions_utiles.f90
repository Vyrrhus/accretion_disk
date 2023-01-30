MODULE module_fonctions_utiles

    USE module_declarations
    
    IMPLICIT NONE
    
    ! Pour chaque branche épaisse ou mince, la fonction à annuler par dichotomie 
    ! est de la forme alpha*(Prad+Pgaz)+beta*third_term=0
    ! Ce module permet donc de calculer les différents termes correspondant à des propositions H1, H2 à T fixé 
    ! ou T1, T2 à H fixé selon les méthodes de dichotomie...
    
    CONTAINS
    
    !--------------------------------------------------------------------------------------------------------------------------
    ! Dans un premier temps, on va calculer les grandeurs d'intérêt H, P_gaz, P_rad, rho en fonction de Temp et S
    !--------------------------------------------------------------------------------------------------------------------------

    REAL(kind=xp) FUNCTION trinome(a, b, c)

    ! -------------------------------------------------------------------------------------------------------------------------
    ! Fonction trinome calculant la racine positive d'un trinôme du second degré
    ! a*x**2+b*x+cc=0. La fonction retourne une erreur lorsque le déterminant est négatif
    ! -------------------------------------------------------------------------------------------------------------------------
    
        REAL(kind=xp), INTENT(IN) :: a,b,c
        REAL(kind=xp)             :: Delta
    
        Delta = b**2._xp - 4._xp * a * c
    
        IF (Delta<0) THEN
            PRINT *,'Déterminant négatif' ! Message d'erreur
        ENDIF
    
        trinome = (-b + (SQRT(Delta) ) / (2._xp*a))
    
    END FUNCTION
    
    SUBROUTINE calc_H(TEMP_AD, X_AD, OMEGA_AD, S_AD, H_AD)
    ! --------------------------------------------------------------------------------------------------------------------------------------
    ! Calcul de H_AD en résolvant le trinôme H_AD**2-B_0*B_AD-C_AD*c_star=0
    ! C_AD est défini dans le module "module_declarations
    ! ------------------------------------------------------------------------------------------------------------------------------------------
        REAL(kind=xp) , INTENT(IN)      :: TEMP_AD, X_AD, OMEGA_AD, S_AD
        REAL(kind=xp)                   :: B_AD, C_AD
        REAL(kind=xp) , INTENT(OUT)     :: H_AD
    
        B_AD = (TEMP_AD**4._xp * X_AD) / (OMEGA_AD**2._xp * S_AD)
    
        C_AD = TEMP_AD / OMEGA_AD**2._xp

    
        H_AD=trinome(1._xp, -B_0 * B_AD, -C_0 * C_AD)
    
    END SUBROUTINE

    SUBROUTINE calc_H2 (TEMP_AD, X_AD, OMEGA_AD, S_AD, H_AD)
       REAL(KIND=xp), INTENT(IN)  :: TEMP_AD, OMEGA_AD, S_AD, X_AD
       REAL(KIND=xp), INTENT(INOUT):: H_AD
       REAL(KIND=xp)               :: A2, B2, C2, DELTA
       REAL(KIND=xp)               :: TEMP, OMEGA, S

       TEMP = TEMP_AD*TEMP_0
       OMEGA = OMEGA_AD*OMEGA_MAX
       S=S_AD*S_0/X_AD

       A2= 0.5_xp * OMEGA**2._xp * S
       B2= -(1._xp/3._xp) * A_RADIATION * TEMP**4._xp 
       C2= - R_BOLTZ * TEMP * S   / (2._xp * MU)

       DELTA= B2**2._xp - 4._xp * A2 * C2

       H_AD=-0.5_xp * (B2 + SIGN(SQRT(DELTA),B2))/A2
       H_AD= H_AD/R_S

    END SUBROUTINE


    SUBROUTINE signeQ(Temp,S,X_AD)
    !-------------------------------------------------------------------------------------------------------------
    !Calcul du signe de Q+-Q-
    !---------------------------------------------------------------------------------------------------------------------
       REAL(KIND=xp), INTENT(IN)           :: Temp, S, X_AD

    END SUBROUTINE

!------------------------------------------------------------------------------------------------------------------------------------
!---------------------------------------------------Subroutine append----------------------------------------------------------------
!------------------------------------------------------------------------------------------------------------------------------------

    SUBROUTINE AddToList(list, element)

        IMPLICIT NONE

        INTEGER :: i, isize
        INTEGER, INTENT(IN) :: element
        INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: list
        INTEGER, DIMENSION(:), ALLOCATABLE :: clist


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


    end subroutine AddToList

    SUBROUTINE POINTS_CRITIQUES()

        IMPLICIT NONE
    
    !------------------------------------------------------------------------------------------------------------------------------------
    !-- Donne les points critiques de la branche épaisse des courbes en S correspondant à chaque rayon du disque
    !-- n_lines donne le nombre de lignes du fichier csv utilisé
    !-- n_per_radius donne le nombre de lignes par rayon du fichier CSV
    !-- n_radii donne le nombre de rayons différents
    !------------------------------------------------------------------------------------------------------------------------------------
        
        REAL , ALLOCATABLE   :: S_DATA(:,:,:)
        INTEGER, ALLOCATABLE :: POS_TAB(:), GAP_TAB(:)
        REAL, ALLOCATABLE    :: PC_TAB(:,:)
        INTEGER              :: i, j, k, rc, n_lines, n_per_radius, n_radii, GAP_MAX, GAP, j_max
        REAL                 :: junk1, junk2, junk3, old_radius, new_radius, max_sigma
    
        OPEN (UNIT = 1, FILE = "./output/results_epais.out", IOSTAT = rc)
    
        !--------------------------------------------------------------------------------------------------------------------------------
        ! On détecte le nombre de lignes dans le fichier csv
        !--------------------------------------------------------------------------------------------------------------------------------
    
        n_lines = 0
    
        DO
            READ(1, *, IOSTAT = rc)
    
            IF (rc/=0) EXIT
            n_lines = n_lines + 1
        END DO
    
        CLOSE(1)
    
    
        OPEN (UNIT = 10, FILE = "./output/results_epais.out")
    
        !--------------------------------------------------------------------------------------------------------------------------------
        ! On détecte la position de changement de rayon dans le fichier que l'on stocke dans l'array POS_TAB
        !--------------------------------------------------------------------------------------------------------------------------------
        
        READ(10 ,*) junk1, junk2, junk3, old_radius
    
        CLOSE(10)
    
        OPEN (UNIT = 1, FILE = "./output/results_epais.out")
    
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
        GAP_TAB(1) = POS_TAB(1)-1
        
        DO WHILE(i < SIZE(POS_TAB)) 
            
            GAP=POS_TAB(i)-POS_TAB(i-1)
            CALL AddToList(GAP_TAB, GAP)
    
            IF (GAP > GAP_MAX) THEN
                GAP_MAX=GAP
            END IF
            i = i + 1
    
        END DO
    
        !--------------------------------------------------------------------------------------------------------------------------------
        ! On construit d'abord le tableau correspondant au fichier en sortie de la subroutine s_curve
        ! Ce tableau est en 3D, avec:
        ! 1ère dimension T, Sigma, x_ad, r, dimension 4
        ! 2ème dimension la courbe en S pour un rayon, dimension GAP_MAX
        ! 3ème dimension le rayon, dimension n_radii
        !--------------------------------------------------------------------------------------------------------------------------------
    
        OPEN (UNIT = 1, FILE = "./output/results_epais.out")
    
        n_radii = SIZE(POS_TAB)
    
        ALLOCATE(S_DATA(4, GAP_MAX, n_radii))
        i=1
        j = 1
        
        DO WHILE (j < n_radii)
    
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
    
        DO WHILE (i<n_radii)
    
            j = 1
            MAX_SIGMA = S_DATA(2, 1, i)
            j_max = 1
    
            DO WHILE (j<GAP_MAX)
    
                IF (S_DATA(2, j, i) > MAX_SIGMA) THEN
    
                    MAX_SIGMA = S_DATA(2, j, i)
                    j_max = j
    
                END IF
            
                j = j + 1
            
            END DO
            PC_TAB(:, i) = S_DATA(:, j_max, i)
    
            i = i + 1
    
        END DO
    
    !------------------------------------------------------------------------------------------------------------------------------------
    ! On écrit dans un fichier spécifique les coordonnées du point d'inflexion de la branche épaisse des courbes en S.
    ! Cela correspond à un array de dimension (n_radii, 4)
    !------------------------------------------------------------------------------------------------------------------------------------
        
        OPEN (UNIT = 5, FILE = "./output/coord_turning_points.out")
    
        DO i=1, n_radii
    
            WRITE(5, *) PC_TAB(:, i)
    
        END DO
    
        CLOSE(5)
    
    END SUBROUTINE POINTS_CRITIQUES

    !SUBROUTINE calc_rho(S_AD, X_AD, H_AD, RHO_AD)

    ! --------------------------------------------------------------------------------------------------------------------------------------
    ! Calcule la densité adimensionnée correspondant à un couple (H_AD, S_AD) pour X_AD donné
    ! --------------------------------------------------------------------------------------------------------------------------------------
    
       ! REAL(kind=xp)  , INTENT(IN)     :: S_AD, X_AD, H_AD
      !  REAL(kind=xp)  , INTENT(OUT)    :: RHO_AD
    
     !   RHO_AD = S_AD/(X_AD*H_AD)
    
    !END SUBROUTINE
    
    ! --------------------------------------------------------------------------------------------------------------------------------------
    ! La fonction à annuler est de la forme 1/(4*3**(1.5))*(P_RAD_AD+P_GAZ_AD)+THIRD_TERM=0
    ! Le troisième terme étant dépendant de la branche sur laquelle on se place.
    ! On va calculer dans les prochaines subroutines ces différents termes.
    ! --------------------------------------------------------------------------------------------------------------------------------------

    !SUBROUTINE calc_P_rad(TEMP_AD, P_RAD_AD)

    ! --------------------------------------------------------------------------------------------------------------------------------------
    ! Calcule la pression de radiation adimensionnée à partir de la température adimensionnée
    ! --------------------------------------------------------------------------------------------------------------------------------------
    
       ! REAL(kind=xp)  , INTENT(IN)     :: TEMP_AD
      !  REAL(kind=xp)  , INTENT(OUT)    :: P_RAD_AD
    
     !   P_RAD_AD = (P_rad_0 / P_0) * TEMP_AD**4._xp
    
    !END SUBROUTINE
    
    !SUBROUTINE calc_P_gaz(TEMP_AD, RHO_AD, P_GAZ_AD)

    ! --------------------------------------------------------------------------------------------------------------------------------------
    ! Calcule la pression de gaz adimensionnée à partir de la température et de la densité adimensionnées
    ! --------------------------------------------------------------------------------------------------------------------------------------
    
       ! REAL(kind=xp)  , INTENT(IN)     :: TEMP_AD, RHO_AD
      !  REAL(kind=xp)  , INTENT(OUT)    :: P_GAZ_AD
    
     !   P_GAZ_AD = (P_gaz_0 / P_0) * TEMP_AD * RHO_AD
    
    !END SUBROUTINE
    
    !SUBROUTINE calc_third_term_mince(TEMP_AD, RHO_AD, THIRD_TERM_AD)

    ! --------------------------------------------------------------------------------------------------------------------------------------
    ! Calcul du troisième terme dans le cas optiquement mince, en prenant une proposition de température et de densité
    ! --------------------------------------------------------------------------------------------------------------------------------------

     !   REAL(kind=xp)  , INTENT(IN)     :: TEMP_AD, RHO_AD
      !  REAL(kind=xp)  , INTENT(OUT)    :: THIRD_TERM_AD
    
       ! THIRD_TERM_AD = F_Z_RAD_0 *RHO_AD**2._xp * TEMP_AD**0.5_xp
    
    !END SUBROUTINE calc_third_term_mince
    
    !SUBROUTINE calc_third_term_epais(TEMP_AD, RHO_AD, H_AD, THIRD_TERM_AD)

    ! --------------------------------------------------------------------------------------------------------------------------------------
    ! Calcul du troisième terme dans le cas optiquement épais, en prenant une proposition de température, de densité et de H
    ! --------------------------------------------------------------------------------------------------------------------------------------
    !
    !    REAL(kind=xp)  , INTENT(IN)     :: TEMP_AD, RHO_AD, H_AD
    !    REAL(kind=xp)                   :: NUMERATOR, DENOMINATOR
    !    REAL(kind=xp)  , INTENT(OUT)    :: THIRD_TERM_AD
    !
    !    numerator=-F_Z_DIFF_0*(2._xp*T_AD**4._xp)
    !    denominator=(KAPPA_E + 6.13E18 * RHO_AD * T_AD**(-7._xp/2._xp) * rho_0 * T_0**(-7._xp/2._xp)) * S_0 &
    !    * RHO_AD**2 *H_AD**2
    !    THIRD_TERM_AD=numerator/denominator
    
    !END SUBROUTINE calc_third_term_epais
    
END MODULE module_fonctions_utiles
