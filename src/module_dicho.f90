MODULE module_dicho
USE module_declarations
USE module_branche_epais
USE module_fonctions_utiles
IMPLICIT NONE

CONTAINS

SUBROUTINE dichotomie(T, Sa, Sb, p, mince, Sc)
! --------------------------------------------------------------------------------------------------------------------------------------
!Calcul du zéro de la fonction Q+=Q- pour les deux branches.
! --------------------------------------------------------------------------------------------------------------------------------------


   REAL(KIND=xp), INTENT(in)   :: T                                               !! Température
   REAL(KIND=xp), INTENT(inout):: Sa, Sb                                          !! Points de départ de la dichotomie
   LOGICAL,       INTENT(in)   :: mince                                           !! Booléen pour savoir dans quelle branche on est
   REAL(KIND=xp), INTENT(out)  :: Sc                                              !! Point milieu de la dichotomie
   INTEGER,       INTENT(in)   :: p                                               !! Indice de la position
 
   REAL(KIND=xp)               :: prec=0.01_xp                                    !! Précision de la dichotomie
   REAL(KIND=xp)               :: eps=10_xp
   REAL(KIND=xp)               :: Ha, Hb, Hc                                      !! H aux points a, b et c
   REAL(KIND=xp)               :: rho_a, rho_b, rho_c                             !! rho aux points a, b et c
   REAL(KIND=xp)               :: Prad                                            !! Pression radiative
   REAL(KIND=xp)               :: Pgaz_a, Pgaz_b, Pgaz_c                          !! Pression du gaz aux points a, b et c
   REAL(KIND=xp)               :: third_term_a, third_term_b, third_term_c        !! troisième terme de la fonction à annuler en a, b et c
   REAL(KIND=xp)               :: Fa, Fb, Fc                                      !! Valeurs de la fonction à annuler aux point a, b et c



   
   CALL calc_P_rad(T,Prad)                                                                               !Calcul de Prad
   CALL calc_H(T,x_ad(p),OMEGA_AD(p),Sa,Ha)                                                                    !calcul de H au point a
   CALL calc_H(T,x_ad(p),OMEGA_AD(p),Sb,Hb)                                                                    !calcul de H au point b
   CALL calc_rho(Sa,x_ad(p),Ha,rho_a)                                                                       !calcul de rho au point a
   CALL calc_rho(Sb,x_ad(p),Hb,rho_b)                                                                       !calcul de rho au point b
   CALL calc_P_gaz(T,rho_a,Pgaz_a)                                                                       !calcul de Pgaz au point a
   CALL calc_P_gaz(T,rho_b,Pgaz_b)                                                                       !calcul de Pgaz au point b

   IF (mince .eqv. .true.) THEN                                                                               !Calculs pour la branche mince

      CALL calc_third_term_mince(T,rho_a,third_term_a)                                                   !calcul du troisième terme mince au point a
      CALL calc_third_term_mince(T,rho_b,third_term_b)                                                   !calcul du troisième terme mince au point b
      CALL calc_F(Prad,Pgaz_a,third_term_a,Fa)
      CALL calc_F(Prad,Pgaz_b,third_term_b,Fb)

      DO WHILE(eps>prec)

         Sc=(Sa+Sb)/2_xp
         CALL calc_H(T,x_ad(p),OMEGA_AD(p),Sc,Hc)                                                              !calcul de H au point c
         CALL calc_rho(Sc,x_ad(p),Hc,rho_c)                                                                 !calcul de rho au point c
         CALL calc_P_gaz(T,rho_c,Pgaz_c)                                                                 !calcul de Pgaz au point c
         CALL calc_third_term_mince(T,rho_c,third_term_c)                                                !calcul du troisième terme au point c
         CALL calc_F(Prad,Pgaz_c,third_term_c,Fc)

         IF ((Fa*Fc)<0.0_xp) THEN                                                                         !Si f(a)*f(c)<0

            Sb=Sc
            Hb=Hc
            rho_b=rho_c                                                                                  !Le point c devient le nouveau point b
            Pgaz_b=Pgaz_c
            third_term_b=third_term_c
            Fb=Fc

         ELSE                                                                                            !Si f(b)*f(c)<=0

            Sa=Sc
            Ha=Hc                                                                                        !Le point c devient le nouveau point a
            rho_a=rho_c
            Pgaz_a=Pgaz_c
            third_term_a=third_term_c
            Fa=Fc

         ENDIF

         eps=ABS(Fc)                                                      !Calcul de l'erreur

      ENDDO
   ELSE                                                                                                  !Calculs pour la branche épais

      CALL calc_third_term_epais(T,rho_a,Ha,third_term_a)
      CALL calc_third_term_epais(T,rho_b,Hb,third_term_b)
      CALL calc_F(Prad,Pgaz_a,third_term_a,Fa)
      CALL calc_F(Prad,Pgaz_b,third_term_b,Fb)

      DO WHILE(eps>prec)

         Sc=(Sa+Sb)/2_xp
         CALL calc_H(T,x_ad(p),OMEGA_AD(p),Sc,Hc)
         CALL calc_rho(Sc,x_ad(p),Hc,rho_c)
         CALL calc_P_gaz(T,rho_c,Pgaz_c)
         CALL calc_third_term_epais(T,rho_c,Hc,third_term_c)
         CALL calc_F(Prad,Pgaz_c,third_term_c,Fc)

         IF ((Fa*Fc)<0.0_xp) THEN

            Sb=Sc
            Hb=Hc
            rho_b=rho_c
            Pgaz_b=Pgaz_c
            third_term_b=third_term_c
            Fb=Fc

         ELSE 

            Sa=Sc
            Ha=Hc
            rho_a=rho_c
            Pgaz_a=Pgaz_c
            third_term_a=third_term_c
            Fa=Fc

         ENDIF

         eps=ABS(Fc)

      ENDDO

   ENDIF

END SUBROUTINE

END MODULE module_dicho