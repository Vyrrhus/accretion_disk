MODULE module_fonctions_utiles

    USE module_declarations
    
    IMPLICIT NONE
    
    ! Pour chaque branche épaisse ou mince, la fonction à annuler par dichotomie 
    ! est de la forme alpha*(Prad+Pgaz)+beta*third_term=0
    ! Ce module permet donc de calculer les différents termes correspondant à des propositions H1, H2 à T fixé 
    ! ou T1, T2 à H fixé selon les méthodes de dichotomie...
    
    CONTAINS
    
    REAL(kind=xp) FUNCTION trinome(a, b, c)
    ! -------------------------------------------------------------------------------------------------------------------------
    ! Fonction trinome calculant la racine positive d'un trinôme du second degré
    ! a*x**2+b*x+cc=0. La fonction retourne une erreur lorsque le déterminant est négatif
    ! -------------------------------------------------------------------------------------------------------------------------
    
        REAL(kind=xp), INTENT(IN) :: a,b,c
        REAL(kind=xp)             :: Delta
    
        Delta = b**2._xp-4._xp*a*c
    
        IF (Delta<0) THEN
            print *,'Déterminant négatif'
        ENDIF
    
        trinome=(-b+(Delta**2._xp))/(2._xp*a)
    
    END FUNCTION
    
    SUBROUTINE calc_H(T_AD, X_AD, OMEGA_AD, S_AD, H_AD)
    ! --------------------------------------------------------------------------------------------------------------------------------------
    ! Calcul de H_AD en résolvant le trinôme H_AD**2-B_0*b_star-C_AD*c_star=0
    ! C_AD est défini dans le module "module_declarations
    ! ------------------------------------------------------------------------------------------------------------------------------------------
    
        REAL(kind=xp) , INTENT(IN)      :: T_AD, X_AD, OMEGA_AD, S_AD
        REAL(kind=xp)                   :: B_AD, C_AD
        REAL(kind=xp) , INTENT(OUT)     :: H_AD
    
        B_AD = (T_AD**4._xp*X_AD)/(OMEGA_AD**2._xp*S_AD)
    
        C_AD = T_AD/OMEGA_AD**2._xp
    
        H_AD=trinome(1._xp, -B_0*B_AD, -C_0*C_AD)
    
    END SUBROUTINE
    
    SUBROUTINE calc_rho(S_AD, X_AD, H_AD, RHO_AD)
    ! --------------------------------------------------------------------------------------------------------------------------------------
    ! Calcule la densité adimensionnée correspondant à un couple (H_AD, S_AD) pour X_AD donné
    ! --------------------------------------------------------------------------------------------------------------------------------------
    
        REAL(kind=xp)  , INTENT(IN)     :: S_AD, X_AD, H_AD
        REAL(kind=xp)  , INTENT(OUT)    :: RHO_AD
    
        RHO_AD = S_AD/(X_AD*H_AD)
    
    END SUBROUTINE
    
    SUBROUTINE calc_P_rad(T_AD, P_RAD_AD)
    ! --------------------------------------------------------------------------------------------------------------------------------------
    ! Calcule la pression de radiation adimensionnée à partir de la température adimensionnée
    ! --------------------------------------------------------------------------------------------------------------------------------------
    
        REAL(kind=xp)  , INTENT(IN)     :: T_AD
        REAL(kind=xp)  , INTENT(OUT)    :: P_RAD_AD
    
        P_RAD_AD=(P_rad_0/P_0)*T_AD**4._xp
    
    END SUBROUTINE
    
    SUBROUTINE calc_P_gaz(T_AD, RHO_AD, P_GAZ_AD)
    ! --------------------------------------------------------------------------------------------------------------------------------------
    ! Calcule la pression de gaz adimensionnée à partir de la température et de la densité adimensionnées
    ! --------------------------------------------------------------------------------------------------------------------------------------
    
        REAL(kind=xp)  , INTENT(IN)     :: T_AD, RHO_AD
        REAL(kind=xp)  , INTENT(OUT)    :: P_GAZ_AD
    
        P_GAZ_AD=(P_gaz_0/P_0)*T_AD*RHO_AD
    
    END SUBROUTINE
    
    SUBROUTINE calc_third_term_mince(T_AD, RHO_AD, THIRD_TERM_AD)
    ! --------------------------------------------------------------------------------------------------------------------------------------
    ! Calcul du troisième terme dans le cas optiquement mince, en prenant une proposition de température et de densité
    ! --------------------------------------------------------------------------------------------------------------------------------------

        REAL(kind=xp)  , INTENT(IN)     :: T_AD, RHO_AD
        REAL(kind=xp)  , INTENT(OUT)    :: THIRD_TERM_AD
    
        THIRD_TERM_AD=F_Z_RAD_0*RHO_AD**2._xp*T_AD**0.5_xp
    
    END SUBROUTINE calc_third_term_mince
    
    SUBROUTINE calc_third_term_epais(T_AD, RHO_AD, H_AD, THIRD_TERM_AD)
    ! --------------------------------------------------------------------------------------------------------------------------------------
    ! Calcul du troisième terme dans le cas optiquement épais, en prenant une proposition de température, de densité et de H
    ! --------------------------------------------------------------------------------------------------------------------------------------
    
        REAL(kind=xp)  , INTENT(IN)     :: T_AD, RHO_AD, H_AD
        REAL(kind=xp)                   :: numerator, denominator
        REAL(kind=xp)  , INTENT(OUT)    :: THIRD_TERM_AD
    
        numerator=-F_Z_DIFF_0*(2._xp*T_AD**4._xp)
        denominator=(KAPPA_E+6.13_xpE21*RHO_AD*T_AD**(-7._xp/2._xp)*rho_0*T_0**(-7._xp/2._xp))*S_0*RHO_AD**2*H_AD**2
        THIRD_TERM_AD=numerator/denominator
    
    END SUBROUTINE calc_third_term_epais
    
    ! REAL(kINd=xp) function F_to_dicho(P_RAD_AD, P_GAZ_AD, THIRD_TERM_AD)
    
    !     REAL(kINd=xp)  , INTENT(IN)     :: P_RAD_AD, P_GAZ_AD, THIRD_TERM_AD
    
    !     F_to_dicho=1/(4*3**(1.5))*(P_RAD_AD+P_GAZ_AD)-F_Z_RAD_0*THIRD_TERM_AD
        
    ! END function F_to_dicho
    
END MODULE module_fonctions_utiles