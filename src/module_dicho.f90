MODULE module_dicho
USE module_declarations
USE module_fonctions_utiles
IMPLICIT NONE

CONTAINS

SUBROUTINE dichotomie(Temp_S, Sa, Sb, ipos, mince, Sc, ecriture)
! --------------------------------------------------------------------------------------------------------------------------------------
!Calcul du zéro de la fonction Q+=Q- pour les deux branches.
! --------------------------------------------------------------------------------------------------------------------------------------


   REAL(KIND=xp), INTENT(in)    :: Temp_S                                               !! Température
   REAL(KIND=xp), INTENT(inout) :: Sa, Sb                                          !! Points de départ de la dichotomie
   LOGICAL,       INTENT(in)    :: mince                                           !! Booléen pour savoir dans quelle branche on est
   REAL(KIND=xp), INTENT(out)   :: Sc                                              !! Point milieu de la dichotomie
   INTEGER,       INTENT(in)    :: ipos                                               !! Indice de la position
   LOGICAL,       INTENT(inout) :: ecriture 
 
   REAL(KIND=xp)               :: prec=1._xp                                     !! Précision de la dichotomie
   REAL(KIND=xp)               :: eps
   REAL(KIND=xp)               :: Ha, Hb, Hc                                      !! H aux points a, b et c
   REAL(KIND=xp)               :: rho_a, rho_b, rho_c                             !! rho aux points a, b et c
   REAL(KIND=xp)               :: Prad                                            !! Pression radiative
   REAL(KIND=xp)               :: Pgaz_a, Pgaz_b, Pgaz_c                          !! Pression du gaz aux points a, b et c
   REAL(KIND=xp)               :: Pa, Pb, Pc                                      !! Pression totale aux points a, b et c
   REAL(KIND=xp)               :: Omega_S                                           !! Valeur de Omega_S
   REAL(KIND=xp)               :: Q_plus_a, Q_plus_b, Q_plus_c
   REAL(KIND=xp)               :: Q_moins_a, Q_moins_b, Q_moins_c 
   REAL(KIND=xp)               :: nua, nub, nuc                                   !! Nu aux points a, b et c
   REAL(KIND=xp)               :: Fza, Fzb, Fzc                                   !! Fz aux point a, b et c 
   REAL(KIND=xp)               :: Fa, Fb, Fc                                      !! Fonction à annuler aux points a, b et c
   REAL(KIND=xp)               :: Kffa,Kffb, Kffc                                 !! Kff aux points a, b et c
   INTEGER                     :: counter
   INTEGER                     :: limit = 1000


   eps=300._xp

   counter=0
   
   !PRINT*, "x= ", X_AD(ipos)

   Prad =  Temp_S ** 4._xp                                                                               !Calcul de Prad
   !PRINT*, "Prad= ", Prad

   Omega_S = 3._xp ** (3._xp/2._xp) * X_AD(ipos) ** (-3._xp)
   !PRINT*, "Omega= ", Omega_S * OMEGA_MAX
   !PRINT*, "x_ad = ", X_AD(ipos)

   CALL calc_H2(Temp_S,x_ad(ipos),Omega_S,Sa,Ha)                                                                    !calcul de H au point a
   !PRINT*, "H= ", Ha * R_S
   
   CALL calc_H2(Temp_S,x_ad(ipos),Omega_S,Sb,Hb)                                                                    !calcul de H au point b

   rho_a = Sa / ( x_ad(ipos) * Ha )                                                                      !calcul de rho au point a
   !PRINT*, "rho= ", rho_a * rho_0

   rho_b = Sb / ( x_ad(ipos) * Hb )                                                                       !calcul de rho au point b

   Pgaz_a = Temp_S * rho_a                                                                             !calcul de Pgaz au point a
   !PRINT*, "Pgaz= ", Pgaz_a

   Pa=( P_gaz_0 / P_0 ) * Pgaz_a+ ( P_rad_0 / P_0 ) * Prad
   !PRINT*, "Ptot= ", Pa * P_0

   Pgaz_b = Temp_S * rho_b                                                                            !calcul de Pgaz au point b

   Pb=( P_gaz_0 / P_0 ) * Pgaz_b+ ( P_rad_0 / P_0 ) * Prad

   nua = 0.5_xp * Omega_S * Ha * Ha
   !PRINT*, "nu= ", nua * NU_0

   nub = 0.5_xp *Omega_S * Hb * Hb

   IF (mince .eqv. .true.) THEN                                                                               !Calculs pour la branche mince

      Fza=F_Z_RAD_0 * rho_a**2._xp * SQRT(Temp_S) * Ha
      !PRINT*, "Fza= ", Fza

      Fzb=F_Z_RAD_0 * rho_b**2._xp * SQRT(Temp_S) * Hb
      !PRINT*, "Fzb= ", Fzb

      Q_plus_a = nua * Omega_S**2._xp *Q_PLUS_0
      Q_plus_b = nub * Omega_S**2._xp *Q_PLUS_0

      Q_moins_a = 2._xp * x_ad(ipos) * Fza / (Sa*S_0)
      Q_moins_b = 2._xp * x_ad(ipos) * Fzb / (Sb*S_0)

      Fa = Q_plus_a - Q_moins_a
      !PRINT*, "Fa= ", Fa

      Fb = Q_plus_b - Q_moins_b

      !PRINT*, "Q+= ", Q_plus_a
      !PRINT*, "Q- mince= ", Q_moins_a
      

      DO WHILE(eps>prec .and. counter<limit)

         Sc=Sa+(Sb-Sa)/2._xp
         CALL calc_H2(Temp_S,x_ad(ipos),Omega_S,Sc,Hc)                                                              !calcul de H au point c

         rho_c = Sc / ( x_ad(ipos) * Hc )                                                                             !calcul de rho au point c

         Pgaz_c = Temp_S * rho_c                                                                 !calcul de Pgaz au point c

         Pc=( P_gaz_0 / P_0 ) * Pgaz_c+ ( P_rad_0 / P_0 ) * Prad

         nuc = 0.5_xp * Omega_S * Hc * Hc

         Fzc=F_Z_RAD_0 * rho_c**2._xp * SQRT(Temp_S) * Hc

         Q_plus_c = nuc * Omega_S**2._xp *Q_PLUS_0

         Q_moins_c = 2._xp * x_ad(ipos) * Fzc / (Sc*S_0)

         Fc = Q_plus_c - Q_moins_c 

         IF ((Fa*Fc)<0.0_xp) THEN                                                                         !Si f(a)*f(c)<0

            Sb=Sc
            Hb=Hc
            rho_b=rho_c                                                                                  !Le point c devient le nouveau point b
            Pgaz_b=Pgaz_c
            Pb=Pc
            nub=nuc
            Fzb=Fzc
            Q_plus_b=Q_plus_c
            Q_moins_b=Q_moins_c
            Fb=Fc

         ELSE                                                                                            !Si f(b)*f(c)<=0

            Sa=Sc
            Ha=Hc                                                                                        !Le point c devient le nouveau point a
            rho_a=rho_c
            Pgaz_a=Pgaz_c
            Pa=Pc
            nua=nuc
            Fza=Fzc
            Q_plus_b=Q_plus_c
            Q_moins_b=Q_moins_c
            Fa=Fc

         ENDIF

         eps=ABS(Fc)                                                                                     !Calcul de l'erreur
         counter = counter + 1

      ENDDO
   ELSE                                                                                                  !Calculs pour la branche épais

      Kffa = 6.13E18 *rho_a * Temp_S ** (-7._xp/2._xp) *rho_0 * TEMP_0 ** (-7._xp/2._xp)
      !PRINT*, "Kff= ", Kffa

      Kffb = 6.13E18 *rho_b * Temp_S ** (-7._xp/2._xp) *rho_0 * TEMP_0 ** (-7._xp/2._xp)

      Fza = F_Z_DIFF_0 * X_AD(ipos) * Temp_S**4._xp /( (KAPPA_E + Kffa) *Sa )
      !PRINT*, "Fz= ", Fza

      Fzb = F_Z_DIFF_0 * X_AD(ipos) * Temp_S**4._xp /( (KAPPA_E + Kffb) *Sb ) 

      Q_plus_a = nua * Omega_S**2._xp *Q_PLUS_0
      Q_plus_b = nub * Omega_S**2._xp *Q_PLUS_0

      Q_moins_a = 2._xp * x_ad(ipos) * Fza / (Sa*S_0)
      Q_moins_b = 2._xp * x_ad(ipos) * Fzb / (Sb*S_0)

      Fa = Q_plus_a - Q_moins_a
      Fb = Q_plus_b - Q_moins_b

      !PRINT*, "Q+a= ", Q_plus_a
      !PRINT*, "Q-a epais=", Q_moins_a
      !PRINT*, "Q+b= ", Q_plus_b
      !PRINT*, "Q-b= ", Q_moins_b
      !PRINT*, "Q+ - Q-= ", Fa

      DO WHILE(eps>prec .and. counter<limit)

         Sc=Sa+(Sb-Sa)/2._xp
         
         CALL calc_H2(Temp_S,x_ad(ipos),Omega_S,Sc,Hc)

         rho_c = Sc / ( x_ad(ipos) * Hc )                                                                 !calcul de rho au point c

         Pgaz_c = Temp_S * rho_c

         Pc=( P_gaz_0 / P_0 ) * Pgaz_c+ ( P_rad_0 / P_0 ) * Prad

         nuc = 0.5_xp * Omega_S * Hc * Hc

         Kffc = 6.13E18 *rho_c * Temp_S ** (-7._xp/2._xp) *rho_0 * TEMP_0 ** (-7._xp/2._xp)

         Fzc = F_Z_DIFF_0 * X_AD(ipos) * Temp_S**4._xp /( (KAPPA_E + Kffc) *Sc )

         Q_plus_c = nuc * Omega_S**2._xp *Q_PLUS_0
         
         Q_moins_c = 2._xp * x_ad(ipos) * Fzc / (Sc*S_0)
         
         Fc = Q_plus_c - Q_moins_c

         !PRINT*, "T= ", Temp_S, "Sa = ", Sa*S_0/X_AD(ipos), "Sc = ", Sc*S_0/X_AD(ipos), "Sb = ", Sb*S_0/X_AD(ipos)
         !PRINT*, "Q+ - Q-", Fa, Fc, Fb

         IF ((Fa*Fc)<0.0_xp) THEN

            Sb=Sc
            Hb=Hc
            rho_b=rho_c                                                                                  !Le point c devient le nouveau point b
            Pgaz_b=Pgaz_c
            Pb=Pc
            nub=nuc
            Kffb=Kffc
            Fzb=Fzc
            Q_plus_b=Q_plus_c
            Q_moins_b=Q_moins_c
            Fb=Fc

         ELSE 

            Sa=Sc
            Ha=Hc                                                                                        !Le point c devient le nouveau point a
            rho_a=rho_c
            Pgaz_a=Pgaz_c
            Pa=Pc
            nua=nuc
            Kffa=Kffc
            Fza=Fzc
            Q_plus_b=Q_plus_c
            Q_moins_b=Q_moins_c
            Fa=Fc

         ENDIF

         eps=ABS(Fc)
         counter = counter + 1

      ENDDO
 
   ENDIF

   !PRINT*, "Sc= ", Sc
   !PRINT*, "Q+ = ", Q_plus_c
   !PRINT*, "Q- = ", Q_moins_c
   !PRINT*, "Q+ - Q- = ", Fc

   IF (counter < limit) THEN
      ecriture = .true.
   ELSE
      ecriture = .false.
   ENDIF


END SUBROUTINE

END MODULE module_dicho