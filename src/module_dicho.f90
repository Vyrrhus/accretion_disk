MODULE module_dicho
USE module_declarations
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
 
   REAL(KIND=xp)               :: prec=1E-15                                       !! Précision de la dichotomie
   REAL(KIND=xp)               :: eps
   REAL(KIND=xp)               :: Ha, Hb, Hc                                      !! H aux points a, b et c
   REAL(KIND=xp)               :: rho_a, rho_b, rho_c                             !! rho aux points a, b et c
   REAL(KIND=xp)               :: Prad                                            !! Pression radiative
   REAL(KIND=xp)               :: Pgaz_a, Pgaz_b, Pgaz_c                          !! Pression du gaz aux points a, b et c
   REAL(KIND=xp)               :: Pa, Pb, Pc                                      !! Pression totale aux points a, b et c
   REAL(KIND=xp)               :: Omega                                           !! Valeur de Omega
   REAL(KIND=xp)               :: nua, nub, nuc                                   !! Nu aux points a, b et c
   REAL(KIND=xp)               :: Fza, Fzb, Fzc                                   !! Fz aux point a, b et c 
   REAL(KIND=xp)               :: Fa, Fb, Fc                                      !! Fonction à annuler aux points a, b et c
   REAL(KIND=xp)               :: Kffa,Kffb, Kffc                                 !! Kff aux points a, b et c
   LOGICAL                     :: Aff


   eps=10._xp
   
   PRINT*, "x= ", X_AD(p)
   Prad = ( P_rad_0 / P_0 ) * T ** 4._xp                                                                               !Calcul de Prad
   PRINT*, "Prad= ", Prad
   Omega = 3._xp ** (3._xp/2._xp) * X_AD(p) ** (-3._xp)
   PRINT*, "Omega= ", Omega
   Aff=.true.
   CALL calc_H(T,x_ad(p),Omega,Sa,Aff,Ha)                                                                    !calcul de H au point a
   Aff=.false.
   PRINT*, "H= ", Ha
   CALL calc_H(T,x_ad(p),Omega,Sb,Aff,Hb)                                                                    !calcul de H au point b
   rho_a = Sa / ( x_ad(p) * Ha )                                                                      !calcul de rho au point a
   PRINT*, "rho= ", rho_a
   rho_b = Sb / ( x_ad(p) * Hb )                                                                       !calcul de rho au point b
   Pgaz_a = ( P_gaz_0 / P_0 ) * T * rho_a                                                                       !calcul de Pgaz au point a
   PRINT*, "Pgaz= ", Pgaz_a
   Pa=Pgaz_a+Prad
   PRINT*, "Ptot= ", Pa
   Pgaz_b = ( P_gaz_0 / P_0 ) * T * rho_b                                                                     !calcul de Pgaz au point b
   Pb=Pgaz_b+Prad
   nua = 0.5_xp * SQRT(Pa / rho_a) * Ha
   PRINT*, "nu= ", nua
   nub = 0.5_xp * SQRT(Pb / rho_b) * Hb

   IF (mince .eqv. .true.) THEN                                                                               !Calculs pour la branche mince

      Fza=F_Z_RAD_0 * rho_a**2._xp * SQRT(T) * Ha
      PRINT*, "Fz= ", Fza
      Fzb=F_Z_RAD_0 * rho_b**2._xp * SQRT(T) * Hb
      Fa = nua * Omega ** 2._xp * Q_PLUS_0 - 2._xp *x_ad(p) * Fza /(Sa * S_0)
      Fb = nub * Omega ** 2._xp * Q_PLUS_0 - 2._xp *x_ad(p) * Fzb /(Sb * S_0)
      PRINT*, "Q+= ", nua*Omega**2._xp * Q_PLUS_0
      PRINT*, "Q- mince= ", 2._xp *x_ad(p) * Fza /(Sa * S_0)
      

      DO WHILE(eps>prec)

         Sc=(Sa+Sb)/2._xp
         CALL calc_H(T,x_ad(p),Omega,Sc,Aff,Hc)                                                              !calcul de H au point c
         rho_c = Sc / ( x_ad(p) * Hc )                                                                             !calcul de rho au point c
         Pgaz_c = ( P_gaz_0 / P_0 ) * T * rho_c                                                                 !calcul de Pgaz au point c
         Pc=Prad+Pgaz_c
         nuc = 0.5_xp * SQRT(Pc / rho_c) * Hc
         Fzc=F_Z_RAD_0 * rho_c**2._xp * SQRT(T) * Hc
         Fc = nuc * Omega ** 2._xp - 2._xp *x_ad(p) * Fzc /(Sc * S_0)

         IF ((Fa*Fc)<0.0_xp) THEN                                                                         !Si f(a)*f(c)<0

            Sb=Sc
            Hb=Hc
            rho_b=rho_c                                                                                  !Le point c devient le nouveau point b
            Pgaz_b=Pgaz_c
            Pb=Pc
            nub=nuc
            Fzb=Fzc
            Fb=Fc

         ELSE                                                                                            !Si f(b)*f(c)<=0

            Sa=Sc
            Ha=Hc                                                                                        !Le point c devient le nouveau point a
            rho_a=rho_c
            Pgaz_a=Pgaz_c
            Pa=Pc
            nua=nuc
            Fza=Fzc
            Fa=Fc

         ENDIF

         eps=ABS(Fc)                                                      !Calcul de l'erreur
         PRINT*, "erreur= ", eps

      ENDDO
   ELSE                                                                                                  !Calculs pour la branche épais

      Kffa = 6.13E18 *rho_a * T ** (-7._xp/2._xp) *rho_0 * T_0 ** (-7._xp/2._xp)
      PRINT*, "Kff= ", Kffa
      Kffb = 6.13E18 *rho_b * T ** (-7._xp/2._xp) *rho_0 * T_0 ** (-7._xp/2._xp)
      Fza = F_Z_DIFF_0 * X_AD(p) * T**4._xp /( (KAPPA_E + Kffa) *Sa )
      PRINT*, "Fz= ", Fza
      Fzb = F_Z_DIFF_0 * X_AD(p) * T**4._xp /( (KAPPA_E + Kffb) *Sb ) 
      Fa = nua * Omega ** 2._xp * Q_PLUS_0 - 2._xp *x_ad(p) * Fza /(Sa * S_0)
      Fb = nub * Omega ** 2._xp * Q_PLUS_0 - 2._xp *x_ad(p) * Fzb /(Sb * S_0)
      PRINT*, "Q+= ", nua*Omega**2._xp * Q_PLUS_0
      PRINT*, "Q- epais=", 2._xp *x_ad(p) * Fza /(Sa * S_0)

      DO WHILE(eps>prec)

         Sc=(Sa+Sb)/2_xp
         CALL calc_H(T,x_ad(p),Omega,Sc,Aff,Hc)
         rho_c = Sc / ( x_ad(p) * Hc )                                                                 !calcul de rho au point c
         Pgaz_c = ( P_gaz_0 / P_0 ) * T * rho_c
         Pc=Prad+Pgaz_c
         nuc = 0.5_xp * SQRT(Pc / rho_c) * Hc
         Kffc = 6.13E18 *rho_c * T ** (-7._xp/2._xp) *rho_0 * T_0 ** (-7._xp/2._xp)
         Fzc = F_Z_DIFF_0 * X_AD(p) * T**4._xp /( (KAPPA_E + Kffc) *Sc )
         Fc = nuc * Omega ** 2._xp - 2._xp *x_ad(p) * Fzc /(Sc * S_0)

         IF ((Fa*Fc)<0.0_xp) THEN

            Sb=Sc
            Hb=Hc
            rho_b=rho_c                                                                                  !Le point c devient le nouveau point b
            Pgaz_b=Pgaz_c
            Pb=Pc
            nub=nuc
            Fzb=Fzc
            Fb=Fc

         ELSE 

            Sa=Sc
            Ha=Hc                                                                                        !Le point c devient le nouveau point a
            rho_a=rho_c
            Pgaz_a=Pgaz_c
            Pa=Pc
            nua=nuc
            Fza=Fzc
            Fa=Fc

         ENDIF

         eps=ABS(Fc)
         !PRINT*, "erreur= ", eps

      ENDDO

   ENDIF

END SUBROUTINE

END MODULE module_dicho