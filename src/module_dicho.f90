MODULE module_dicho
USE module_declarations
USE module_fonctions_utiles
IMPLICIT NONE

CONTAINS

SUBROUTINE dichotomie(Temp_S_AD, Sa_AD, Sb_AD, ipos, mince, Sc_AD, ecriture)
! --------------------------------------------------------------------------------------------------------------------------------------
!Calcul du zéro de la fonction Q+=Q- pour les deux branches.
! --------------------------------------------------------------------------------------------------------------------------------------


   REAL(KIND=xp), INTENT(in)    :: Temp_S_AD                                               !! Température
   REAL(KIND=xp), INTENT(inout) :: Sa_AD, Sb_AD                                          !! Points de départ de la dichotomie
   LOGICAL,       INTENT(in)    :: mince                                           !! Booléen pour savoir dans quelle branche on est
   REAL(KIND=xp), INTENT(out)   :: Sc_AD                                              !! Point milieu de la dichotomie
   INTEGER,       INTENT(in)    :: ipos                                               !! Indice de la position
   LOGICAL,       INTENT(inout) :: ecriture 
 
   REAL(KIND=xp)               :: prec=1E-2_xp                                     !! Précision de la dichotomie
   REAL(KIND=xp)               :: eps
   REAL(KIND=xp)               :: Ha_AD, Hb_AD, Hc_AD                                      !! H aux points a, b et c
   REAL(KIND=xp)               :: rho_a_AD, rho_b_AD, rho_c_AD                             !! rho aux points a, b et c
   REAL(KIND=xp)               :: Prad_AD                                            !! Pression radiative
   REAL(KIND=xp)               :: Pgaz_a_AD, Pgaz_b_AD, Pgaz_c_AD                          !! Pression du gaz aux points a, b et c
   REAL(KIND=xp)               :: Pa, Pb, Pc                                      !! Pression totale aux points a, b et c
   REAL(KIND=xp)               :: Omega_S_AD                                           !! Valeur de Omega_S
   REAL(KIND=xp)               :: Q_plus_a, Q_plus_b, Q_plus_c
   REAL(KIND=xp)               :: Q_moins_a, Q_moins_b, Q_moins_c 
   REAL(KIND=xp)               :: nua_AD, nub_AD, nuc_AD                                   !! Nu aux points a, b et c
   REAL(KIND=xp)               :: Fza, Fzb, Fzc                                   !! Fz aux point a, b et c 
   REAL(KIND=xp)               :: Fa, Fb, Fc                                      !! Fonction à annuler aux points a, b et c
   REAL(KIND=xp)               :: Kffa,Kffb, Kffc                                 !! Kff aux points a, b et c
   INTEGER                     :: counter
   INTEGER                     :: limit = 1000


   eps=300._xp

   counter=0

   Prad_AD =  Temp_S_AD ** 4._xp

   Omega_S_AD = 3._xp ** (3._xp/2._xp) * X_AD(ipos) ** (-3._xp)

   CALL calc_H2( Temp_S_AD, x_ad(ipos), Omega_S_AD, Sa_AD, Ha_AD )                                                                    !calcul de H au point a
   
   CALL calc_H2( Temp_S_AD, x_ad(ipos), Omega_S_AD, Sb_AD, Hb_AD )                                                                    !calcul de H au point b

   rho_a_AD = Sa_AD / ( x_ad(ipos) * Ha_AD )                                                                      !calcul de rho au point a

   rho_b_AD = Sb_AD / ( x_ad(ipos) * Hb_AD )                                                                       !calcul de rho au point b

   Pgaz_a_AD = Temp_S_AD * rho_a_AD                                                                             !calcul de Pgaz au point a

   Pa = ( P_gaz_0 / P_0 ) * Pgaz_a_AD + ( P_rad_0 / P_0 ) * Prad_AD

   Pgaz_b_AD = Temp_S_AD * rho_b_AD                                                                            !calcul de Pgaz au point b

   Pb=( P_gaz_0 / P_0 ) * Pgaz_b_AD + ( P_rad_0 / P_0 ) * Prad_AD

   nua_AD = 0.5_xp * Omega_S_AD * Ha_AD * Ha_AD

   nub_AD = 0.5_xp * Omega_S_AD * Hb_AD * Hb_AD

   IF (mince .eqv. .true.) THEN                                                                               !Calculs pour la branche mince

      Fza = F_Z_RAD_0 * rho_a_AD**2._xp * SQRT(Temp_S_AD) * Ha_AD

      Fzb = F_Z_RAD_0 * rho_b_AD**2._xp * SQRT(Temp_S_AD) * Hb_AD

      Q_plus_a = nua_AD * Omega_S_AD**2._xp * Q_PLUS_0
      Q_plus_b = nub_AD * Omega_S_AD**2._xp * Q_PLUS_0

      Q_moins_a = 2._xp * x_ad(ipos) * Fza / ( Sa_AD * S_0 )
      Q_moins_b = 2._xp * x_ad(ipos) * Fzb / ( Sb_AD * S_0 )

      Fa = Q_plus_a - Q_moins_a

      Fb = Q_plus_b - Q_moins_b

      DO WHILE(eps>prec .and. counter<limit)

         Sc_AD = Sa_AD + (Sb_AD - Sa_AD) / 2._xp

         CALL calc_H2( Temp_S_AD, x_ad(ipos), Omega_S_AD, Sc_AD, Hc_AD )                                                              !calcul de H au point c

         rho_c_AD = Sc_AD / ( x_ad(ipos) * Hc_AD )                                                                             !calcul de rho au point c

         Pgaz_c_AD = Temp_S_AD * rho_c_AD                                                                 !calcul de Pgaz au point c

         Pc=( P_gaz_0 / P_0 ) * Pgaz_c_AD + ( P_rad_0 / P_0 ) * Prad_AD

         nuc_AD = 0.5_xp * Omega_S_AD * Hc_AD * Hc_AD

         Fzc = F_Z_RAD_0 * rho_c_AD**2._xp * SQRT(Temp_S_AD) * Hc_AD

         Q_plus_c = nuc_AD * Omega_S_AD**2._xp * Q_PLUS_0

         Q_moins_c = 2._xp * x_ad(ipos) * Fzc / ( Sc_AD * S_0 )

         Fc = Q_plus_c - Q_moins_c 

         IF ((Fa*Fc)<0.0_xp) THEN                                                                         !Si f(a)*f(c)<0

            Sb_AD = Sc_AD
            Hb_AD = Hc_AD
            rho_b_AD = rho_c_AD                                                                                  !Le point c devient le nouveau point b
            Pgaz_b_AD = Pgaz_c_AD
            Pb = Pc
            nub_AD = nuc_AD
            Fzb = Fzc
            Q_plus_b = Q_plus_c
            Q_moins_b = Q_moins_c
            Fb = Fc

         ELSE                                                                                            !Si f(b)*f(c)<=0

            Sa_AD = Sc_AD
            Ha_AD = Hc_AD                                                                                        !Le point c devient le nouveau point a
            rho_a_AD = rho_c_AD
            Pgaz_a_AD = Pgaz_c_AD
            Pa = Pc
            nua_AD = nuc_AD
            Fza = Fzc
            Q_plus_b = Q_plus_c
            Q_moins_b = Q_moins_c
            Fa = Fc

         ENDIF

         eps = ABS(Fc)                                                                                     !Calcul de l'erreur
         counter = counter + 1

      ENDDO
   ELSE                                                                                                  !Calculs pour la branche épais

      Kffa = 6.13E18 *rho_a_AD * Temp_S_AD ** (-7._xp/2._xp) *rho_0 * TEMP_0 ** (-7._xp/2._xp)

      Kffb = 6.13E18 *rho_b_AD * Temp_S_AD ** (-7._xp/2._xp) *rho_0 * TEMP_0 ** (-7._xp/2._xp)

      Fza = F_Z_DIFF_0 * X_AD(ipos) * Temp_S_AD**4._xp /( (KAPPA_E + Kffa) * Sa_AD )

      Fzb = F_Z_DIFF_0 * X_AD(ipos) * Temp_S_AD**4._xp /( (KAPPA_E + Kffb) * Sb_AD ) 

      Q_plus_a = nua_AD * Omega_S_AD**2._xp *Q_PLUS_0
      Q_plus_b = nub_AD * Omega_S_AD**2._xp *Q_PLUS_0

      Q_moins_a = 2._xp * x_ad(ipos) * Fza / ( Sa_AD * S_0 )
      Q_moins_b = 2._xp * x_ad(ipos) * Fzb / ( Sb_AD * S_0 )

      Fa = Q_plus_a - Q_moins_a
      Fb = Q_plus_b - Q_moins_b

      DO WHILE(eps>prec .and. counter<limit)

         Sc_AD = Sa_AD + ( Sb_AD - Sa_AD ) / 2._xp
         
         CALL calc_H2( Temp_S_AD, x_ad(ipos), Omega_S_AD, Sc_AD, Hc_AD )

         rho_c_AD = Sc_AD / ( x_ad(ipos) * Hc_AD )                                                                 !calcul de rho au point c

         Pgaz_c_AD = Temp_S_AD * rho_c_AD

         Pc = ( P_gaz_0 / P_0 ) * Pgaz_c_AD + ( P_rad_0 / P_0 ) * Prad_AD

         nuc_AD = 0.5_xp * Omega_S_AD * Hc_AD * Hc_AD

         Kffc = 6.13E18 *rho_c_AD * Temp_S_AD ** (-7._xp/2._xp) *rho_0 * TEMP_0 ** (-7._xp/2._xp)

         Fzc = F_Z_DIFF_0 * X_AD(ipos) * Temp_S_AD**4._xp /( (KAPPA_E + Kffc) * Sc_AD )

         Q_plus_c = nuc_AD * Omega_S_AD**2._xp * Q_PLUS_0
         
         Q_moins_c = 2._xp * x_ad(ipos) * Fzc / ( Sc_AD * S_0 )
         
         Fc = Q_plus_c - Q_moins_c

         IF ((Fa*Fc)<0.0_xp) THEN

            Sb_AD = Sc_AD
            Hb_AD = Hc_AD
            rho_b_AD = rho_c_AD                                                                                  !Le point c devient le nouveau point b
            Pgaz_b_AD = Pgaz_c_AD
            Pb = Pc
            nub_AD = nuc_AD
            Kffb = Kffc
            Fzb = Fzc
            Q_plus_b = Q_plus_c
            Q_moins_b = Q_moins_c
            Fb = Fc

         ELSE 

            Sa_AD = Sc_AD
            Ha_AD = Hc_AD                                                                                        !Le point c devient le nouveau point a
            rho_a_AD = rho_c_AD
            Pgaz_a_AD = Pgaz_c_AD
            Pa = Pc
            nua_AD = nuc_AD
            Kffa = Kffc
            Fza = Fzc
            Q_plus_b = Q_plus_c
            Q_moins_b = Q_moins_c
            Fa = Fc

         ENDIF

         eps = ABS(Fc)
         counter = counter + 1

      ENDDO
 
   ENDIF

   IF (counter < limit) THEN
      ecriture = .true.
   ELSE
      ecriture = .false.
   ENDIF


END SUBROUTINE

END MODULE module_dicho