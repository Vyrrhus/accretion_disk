MODULE MODULE_FONCTIONS_UTILES

    USE MODULE_DECLARATIONS
    
    IMPLICIT NONE
    
    ! Pour chaque branche épaisse ou mince, la fonction à annuler par dichotomie 
    ! est de la forme alpha*(Prad+Pgaz)+beta*third_term=0
    ! Ce module permet donc de calculer les différents termes correspondant à des propositions H1, H2 à T fixé 
    ! ou T1, T2 à H fixé selon les méthodes de dichotomie...
    
    CONTAINS
    
    REAL(kind=xp) FUNCTION trinome(a, b, c)
    
    ! Fonction trinome calculant la racine positive d'un trinôme du second degré
    ! a*x**2+b*x+cc=0. La fonction retourne une erreur lorsque le déterminant est négatif
    
        REAL(kind=xp), INTENT(IN) :: a,b,c
        REAL(kind=xp)             :: Delta
    
        Delta = b**2-4*a*c
    
        IF (Delta<0) THEN
            print *,'Déterminant négatif'
        ENDIF
    
        trinome=(-b+(Delta**2))/(2*a)
    
    END FUNCTION
    
    SUBROUTINE calc_H(T_star, x_ad, Omega_star, s_star, H_star)
    
    ! Calcul de H_star en résolvant le trinôme H_star**2-b_0*b_star-c_0*c_star=0
    ! c_0 est défini dans le module "module_declarations
    
        REAL(kind=xp) , INTENT(IN)      :: T_star, x_ad, omega_star, S_star
        REAL(kind=xp)                   :: b_star, c_star
        REAL(kind=xp) , INTENT(OUT)     :: H_star
    
        b_star = (T_star**4*x_ad)/(omega_star**2*S_star)
    
        c_star = T_star/omega_star**2
    
        H_star=trinome(1._xp, -b_0*b_star, -c_0*c_star)
    
    END SUBROUTINE
    
    SUBROUTINE calc_rho(S_star, x_ad, H_star, rho_star)
    
    ! Calcule la densité adimensionnée correspondant à un couple (H_star, S_star) pour x_ad donné
    
        REAL(kind=xp)  , INTENT(IN)     :: S_star, x_ad, H_star
        REAL(kind=xp)  , INTENT(OUT)    :: rho_star
    
        rho_star = S_star/(x_ad*H_star)
    
    END SUBROUTINE
    
    SUBROUTINE calc_P_rad(T_star, P_rad_star)
    
    ! Calcule la pression de radiation adimensionnée à partir de la température adimensionnée
    
        REAL(kind=xp)  , INTENT(IN)     :: T_star
        REAL(kind=xp)  , INTENT(OUT)    :: P_rad_star
    
        P_rad_star=(P_rad_0/P_0)*T_star**4
    
    END SUBROUTINE
    
    SUBROUTINE calc_P_gaz(T_star, rho_star, P_gaz_star)
    
    ! Calcule la pression de gaz adimensionnée à partir de la température et de la densité adimensionnées
    
        REAL(kind=xp)  , INTENT(IN)     :: T_star, rho_star
        REAL(kind=xp)  , INTENT(OUT)    :: P_gaz_star
    
        P_gaz_star=(P_gaz_0/P_0)*T_star*rho_star
    
    END SUBROUTINE
    
    SUBROUTINE calc_third_term_mince(T_star, rho_star, third_term_star)
    
    ! Calcul du troisième terme dans le cas optiquement mince, en prenant une proposition de température et de densité
    
        REAL(kind=xp)  , INTENT(IN)     :: T_star, rho_star
        REAL(kind=xp)  , INTENT(OUT)    :: third_term_star
    
        third_term_star=F_Z_RAD_0*rho_star**2*T_star**0.5
    
    END SUBROUTINE calc_third_term_mince
    
    SUBROUTINE calc_third_term_epais(T_star, rho_star, H_star, third_term_star)
    
    ! Calcul du troisième terme dans le cas optiquement épais, en prenant une proposition de température, de densité et de H
    
        REAL(kind=xp)  , INTENT(IN)     :: T_star, rho_star, H_star
        REAL(kind=xp)                   :: numerator, denominator
        REAL(kind=xp)  , INTENT(OUT)    :: third_term_star
    
        numerator=-F_Z_DIFF_0*(2*T_star**4)
        denominator=(KAPPA_E+6.13_xp*10._xp**21*rho_star*T_star**(-7/2)*rho_0*T_0**(-7/2))*S_0*rho_star**2*H_star**2
        third_term_star=numerator/denominator
    
    END SUBROUTINE calc_third_term_epais
    
    ! REAL(kINd=xp) function F_to_dicho(P_rad_star, P_gaz_star, third_term_star)
    
    !     REAL(kINd=xp)  , INTENT(IN)     :: P_rad_star, P_gaz_star, third_term_star
    
    !     F_to_dicho=1/(4*3**(1.5))*(P_rad_star+P_gaz_star)-F_Z_RAD_0*third_term_star
        
    ! END function F_to_dicho
    
END MODULE MODULE_FONCTIONS_UTILES
