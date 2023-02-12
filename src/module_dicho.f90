!===================================================================================================
            MODULE MODULE_DICHO
!===================================================================================================
!>   Ce module permet de calculer les courbes en S
!===================================================================================================

USE MODULE_DECLARATIONS
USE MODULE_FONCTIONS_UTILES
IMPLICIT NONE

REAL(KIND=xp), PRIVATE, PARAMETER    :: PREC_DICHO  = 1e-12_xp      !! Précision sur la dichotomie
INTEGER, PRIVATE, PARAMETER          :: LIMIT_DICHO = 1000          !! Nombre max d'itérations

!===================================================================================================
            CONTAINS 
!===================================================================================================

SUBROUTINE DICHOTOMIE_S(TEMP_S_AD, Sa_AD, Sb_AD, ipos, mince, Sc_AD, ecriture)
!---------------------------------------------------------------------------------------------------
!> Calcul du zéro de la fonction Q+ = Q- pour les deux branches par dichotomie sur S
!---------------------------------------------------------------------------------------------------
    IMPLICIT NONE
    REAL(KIND=xp), INTENT(in)    :: TEMP_S_AD      !! Température
    REAL(KIND=xp), INTENT(inout) :: Sa_AD, Sb_AD   !! Points de départ de la dichotomie
    LOGICAL,       INTENT(in)    :: mince          !! Booléen pour savoir dans quelle branche on est
    REAL(KIND=xp), INTENT(out)   :: Sc_AD          !! Point milieu de la dichotomie
    INTEGER,       INTENT(in)    :: ipos           !! Indice de la position
    LOGICAL,       INTENT(inout) :: ecriture 

    REAL(KIND=xp)  :: eps                              !! erreur
    REAL(KIND=xp)  :: Ha_AD, Hb_AD, Hc_AD              !! H aux points a, b et c
    REAL(KIND=xp)  :: rho_a_AD, rho_b_AD, rho_c_AD     !! rho aux points a, b et c
    REAL(KIND=xp)  :: Prad_AD                          !! Pression radiative
    REAL(KIND=xp)  :: Pgaz_a_AD, Pgaz_b_AD, Pgaz_c_AD  !! Pression du gaz aux points a, b et c
    REAL(KIND=xp)  :: Pa, Pb, Pc                       !! Pression totale aux points a, b et c
    REAL(KIND=xp)  :: Q_plus_a, Q_plus_b, Q_plus_c     !! Q+ aux points a, b et c
    REAL(KIND=xp)  :: Q_moins_a, Q_moins_b, Q_moins_c  !! Q- aux points a, b et c
    REAL(KIND=xp)  :: nua_AD, nub_AD, nuc_AD           !! Nu aux points a, b et c
    REAL(KIND=xp)  :: Fza, Fzb, Fzc                    !! Fz aux point a, b et c 
    REAL(KIND=xp)  :: Fa, Fb, Fc                       !! Fonction à annuler aux points a, b et c
    REAL(KIND=xp)  :: Kffa,Kffb, Kffc                  !! Kff aux points a, b et c
    INTEGER        :: counter

    ! Initialisation erreur et compteur
    eps = 300._xp
    counter = 0

    ! Calcul P_rad
    Prad_AD = Temp_S_AD ** 4._xp

    ! Calcul de H aux points A et B
    CALL calc_H(TEMP_S_AD, X_AD(ipos), OMEGA_AD(ipos), Sa_AD, Ha_AD)
    CALL calc_H(TEMP_S_AD, X_AD(ipos), OMEGA_AD(ipos), Sb_AD, Hb_AD)

    ! Calcul de rho aux points A et B
    rho_a_AD = Sa_AD / (X_AD(ipos) * Ha_AD)
    rho_b_AD = Sb_AD / (X_AD(ipos) * Hb_AD)

    ! Pressions gazeuses & totales aux points A et B
    Pgaz_a_AD = Temp_S_AD * rho_a_AD
    Pgaz_b_AD = Temp_S_AD * rho_b_AD

    Pa = (P_gaz_0 / P_0) * Pgaz_a_AD + (P_rad_0 / P_0) * Prad_AD
    Pb = (P_gaz_0 / P_0) * Pgaz_b_AD + (P_rad_0 / P_0) * Prad_AD

    ! Viscosités aux points A et B
    nua_AD = 0.5_xp * OMEGA_AD(ipos) * Ha_AD**2._xp
    nub_AD = 0.5_xp * OMEGA_AD(ipos) * Hb_AD**2._xp

    ! Calculs pour la branche mince
    IF (mince .eqv. .true.) THEN

        ! Fz mince
        Fza = F_Z_RAD_0 * rho_a_AD**2._xp * SQRT(TEMP_S_AD) * Ha_AD
        Fzb = F_Z_RAD_0 * rho_b_AD**2._xp * SQRT(TEMP_S_AD) * Hb_AD

        ! Q+ aux points A et B
        Q_plus_a = nua_AD * OMEGA_AD(ipos)**2._xp * Q_PLUS_0
        Q_plus_b = nub_AD * OMEGA_AD(ipos)**2._xp * Q_PLUS_0

        ! Q- aux points A et B
        Q_moins_a = 2._xp * X_AD(ipos) * Fza / (Sa_AD * S_0)
        Q_moins_b = 2._xp * X_AD(ipos) * Fzb / (Sb_AD * S_0)

        ! Q+ - Q-
        Fa = Q_plus_a - Q_moins_a
        Fb = Q_plus_b - Q_moins_b
        
        ! Dichotomie
        DO WHILE(eps > PREC_DICHO .and. counter < LIMIT_DICHO)

            ! Sigma moyen entre les points A et B
            Sc_AD = Sa_AD + (Sb_AD - Sa_AD) / 2._xp

            ! On réitère les calculs pour ce nouveau point C
            CALL calc_H(TEMP_S_AD, X_AD(ipos), OMEGA_AD(ipos), Sc_AD, Hc_AD)
            rho_c_AD  = Sc_AD / (X_AD(ipos) * Hc_AD)
            Pgaz_c_AD = TEMP_S_AD * rho_c_AD
            Pc        = (P_gaz_0 / P_0) * Pgaz_c_AD + (P_rad_0 / P_0) * Prad_AD
            nuc_AD    = 0.5_xp * OMEGA_AD(ipos) * Hc_AD**2._xp
            Fzc       = F_Z_RAD_0 * rho_c_AD**2._xp * SQRT(TEMP_S_AD) * Hc_AD

            Q_plus_c  = nuc_AD * OMEGA_AD(ipos)**2._xp * Q_PLUS_0
            Q_moins_c = 2._xp * X_AD(ipos) * Fzc / (Sc_AD * S_0)
            Fc        = Q_plus_c - Q_moins_c

            ! Si f(a) * f(c) < 0 (ie signes différents), le point C devient le nouveau point B
            IF ((Fa*Fc) < 0.0_xp) THEN
                Sb_AD     = Sc_AD
                Hb_AD     = Hc_AD
                rho_b_AD  = rho_c_AD
                Pgaz_b_AD = Pgaz_c_AD
                Pb        = Pc
                nub_AD    = nuc_AD
                Fzb       = Fzc
                Q_plus_b  = Q_plus_c
                Q_moins_b = Q_moins_c
                Fb        = Fc

            ! Sinon, le point C devient le nouveau point A
            ELSE
                Sa_AD     = Sc_AD
                Ha_AD     = Hc_AD
                rho_a_AD  = rho_c_AD
                Pgaz_a_AD = Pgaz_c_AD
                Pa        = Pc
                nua_AD    = nuc_AD
                Fza       = Fzc
                Q_plus_b  = Q_plus_c
                Q_moins_b = Q_moins_c
                Fa        = Fc

            ENDIF

            ! Calcul de l'erreur
            eps = ABS(Fc)
            counter = counter + 1

        ENDDO

    ! Calculs pour la branche épaisse
    ELSE

        ! Kappa_FF
        Kffa = 6.13E18_xp * rho_a_AD * TEMP_S_AD**(-7._xp/2._xp) * rho_0 * TEMP_0**(-7._xp/2._xp)
        Kffb = 6.13E18_xp * rho_b_AD * TEMP_S_AD**(-7._xp/2._xp) * rho_0 * TEMP_0**(-7._xp/2._xp)

        ! Fz épais
        Fza = F_Z_DIFF_0 * X_AD(ipos) * TEMP_S_AD**4._xp / ((KAPPA_E + Kffa) * Sa_AD)
        Fzb = F_Z_DIFF_0 * X_AD(ipos) * TEMP_S_AD**4._xp / ((KAPPA_E + Kffb) * Sb_AD) 

        ! Q+
        Q_plus_a = nua_AD * OMEGA_AD(ipos)**2._xp * Q_PLUS_0
        Q_plus_b = nub_AD * OMEGA_AD(ipos)**2._xp *Q_PLUS_0

        ! Q-
        Q_moins_a = 2._xp * X_AD(ipos) * Fza / ( Sa_AD * S_0 )
        Q_moins_b = 2._xp * X_AD(ipos) * Fzb / ( Sb_AD * S_0 )

        ! Q+ - Q-
        Fa = Q_plus_a - Q_moins_a
        Fb = Q_plus_b - Q_moins_b

        ! Dichotomie
        DO WHILE(eps > PREC_DICHO .and. counter < LIMIT_DICHO)

            ! Sigma moyen entre les points A et B
            Sc_AD = Sa_AD + (Sb_AD - Sa_AD) / 2._xp

            ! On réitère les calculs pour ce nouveau point C
            CALL calc_H(TEMP_S_AD, X_AD(ipos), OMEGA_AD(ipos), Sc_AD, Hc_AD)
            rho_c_AD  = Sc_AD / (X_AD(ipos) * Hc_AD)
            Pgaz_c_AD = TEMP_S_AD * rho_c_AD
            Pc        = (P_gaz_0 / P_0) * Pgaz_c_AD + (P_rad_0 / P_0) * Prad_AD
            nuc_AD    = 0.5_xp * OMEGA_AD(ipos) * Hc_AD**2._xp
            Kffc      = 6.13E18 * rho_c_AD * TEMP_S_AD**(-7._xp/2._xp) * rho_0 * TEMP_0**(-7._xp/2._xp)
            Fzc       = F_Z_DIFF_0 * X_AD(ipos) * TEMP_S_AD**4._xp / ((KAPPA_E + Kffc) * Sc_AD)

            Q_plus_c  = nuc_AD * OMEGA_AD(ipos)**2._xp * Q_PLUS_0
            Q_moins_c = 2._xp * X_AD(ipos) * Fzc / (Sc_AD * S_0)
            Fc        = Q_plus_c - Q_moins_c

            ! Si f(a) * f(c) < 0 (ie signes différents), le point C devient le nouveau point B
            IF ((Fa*Fc) < 0.0_xp) THEN
                Sb_AD     = Sc_AD
                Hb_AD     = Hc_AD
                rho_b_AD  = rho_c_AD
                Pgaz_b_AD = Pgaz_c_AD
                Pb        = Pc
                nub_AD    = nuc_AD
                Kffb      = Kffc
                Fzb       = Fzc
                Q_plus_b  = Q_plus_c
                Q_moins_b = Q_moins_c
                Fb        = Fc

            ! Sinon, le point C devient le nouveau point A
            ELSE 
                Sa_AD     = Sc_AD
                Ha_AD     = Hc_AD
                rho_a_AD  = rho_c_AD
                Pgaz_a_AD = Pgaz_c_AD
                Pa        = Pc
                nua_AD    = nuc_AD
                Kffa      = Kffc
                Fza       = Fzc
                Q_plus_b  = Q_plus_c
                Q_moins_b = Q_moins_c
                Fa        = Fc

            ENDIF

            ! Calcul de l'erreur
            eps = ABS(Fc)
            counter = counter + 1

        ENDDO

    ENDIF

    ! Ecriture uniquement si on a convergé à la précision souhaitée
    IF (counter < LIMIT_DICHO) THEN
        ecriture = .true.
    ELSE
        ecriture = .false.
    ENDIF

!---------------------------------------------------------------------------------------------------
END SUBROUTINE DICHOTOMIE_S
!---------------------------------------------------------------------------------------------------

SUBROUTINE DICHOTOMIE_TEMP( S_S_AD, Temp_a_AD, Temp_b_AD, ipos, Temp_c_AD, ecriture)
!---------------------------------------------------------------------------------------------------
!> Calcul du zéro de la fonction Q+ = Q- pour les deux branches par dichotomie sur T
!---------------------------------------------------------------------------------------------------
   REAL(KIND=xp), INTENT(in)    :: S_S_AD                 !! Densité de surface
   REAL(KIND=xp), INTENT(inout) :: Temp_a_AD, Temp_b_AD   !! Points de départ de la dichotomie
   REAL(KIND=xp), INTENT(out)   :: Temp_c_AD              !! Point milieu de la dichotomie
   INTEGER,       INTENT(in)    :: ipos                   !! Indice de la position
   LOGICAL,       INTENT(inout) :: ecriture 

   REAL(KIND=xp) :: eps                                 !! Erreur
   REAL(KIND=xp) :: Ha_AD, Hb_AD, Hc_AD                 !! H aux points a, b et c
   REAL(KIND=xp) :: rho_a_AD, rho_b_AD, rho_c_AD        !! rho aux points a, b et c
   REAL(KIND=xp) :: Q_plus_a, Q_plus_b, Q_plus_c        !! Q+ aux points a, b et c
   REAL(KIND=xp) :: Q_moins_a, Q_moins_b, Q_moins_c     !! Q- aux points a, b et c
   REAL(KIND=xp) :: nua_AD, nub_AD, nuc_AD              !! Nu aux points a, b et c
   REAL(KIND=xp) :: Fza, Fzb, Fzc                       !! Fz aux point a, b et c 
   REAL(KIND=xp) :: Fa, Fb, Fc                          !! Fonction à annuler aux points a, b et c
   REAL(KIND=xp) :: Kffa,Kffb, Kffc                     !! Kff aux points a, b et c
   INTEGER       :: counter

   ! Initialisation erreur et compteur
   eps=300._xp
   counter=0

   ! Calcul de H aux points A et B
   CALL calc_H( Temp_a_AD, X_AD(ipos), OMEGA_AD(ipos), S_S_AD, Ha_AD )
   CALL calc_H( Temp_b_AD, X_AD(ipos), OMEGA_AD(ipos), S_S_AD, Hb_AD )

   ! Calcul de rho aux point A et B
   rho_a_AD = S_S_AD / ( X_AD(ipos) * Ha_AD )
   rho_b_AD = S_S_AD / ( X_AD(ipos) * Hb_AD )

   ! Viscosités aux points A et B
   nua_AD = 0.5_xp * OMEGA_AD(ipos) * Ha_AD**2._xp
   nub_AD = 0.5_xp * OMEGA_AD(ipos) * Hb_AD**2._xp

   ! Kappa_FF aux point A et B
   Kffa = 6.13E18 * rho_a_AD * Temp_a_AD**(-7._xp/2._xp) * rho_0 * TEMP_0**(-7._xp/2._xp)
   Kffb = 6.13E18 * rho_b_AD * Temp_b_AD**(-7._xp/2._xp) * rho_0 * TEMP_0**(-7._xp/2._xp)

   ! Fz_epais aux point A et B
   Fza = F_Z_DIFF_0 * X_AD(ipos) * Temp_a_AD**4._xp /( (KAPPA_E + Kffa) * S_S_AD )
   Fzb = F_Z_DIFF_0 * X_AD(ipos) * Temp_b_AD**4._xp /( (KAPPA_E + Kffb) * S_S_AD ) 

   ! Q+ aux point A et B
   Q_plus_a = nua_AD * OMEGA_AD(ipos)**2._xp * Q_PLUS_0
   Q_plus_b = nub_AD * OMEGA_AD(ipos)**2._xp * Q_PLUS_0

   ! Q- aux point A et B
   Q_moins_a = 2._xp * X_AD(ipos) * Fza / ( S_S_AD * S_0 )
   Q_moins_b = 2._xp * X_AD(ipos) * Fzb / ( S_S_AD * S_0 )

   ! Q+-Q- aux point A et B
   Fa = Q_plus_a - Q_moins_a
   Fb = Q_plus_b - Q_moins_b

    DO WHILE(eps > PREC_DICHO .and. counter < LIMIT_DICHO)
        ! Température moyenne entre les points A et B
        Temp_c_AD = Temp_a_AD + ( Temp_b_AD - Temp_a_AD ) / 2._xp
         
        ! On réitère les calculs pour le nouveau point C
        CALL calc_H( Temp_c_AD, X_AD(ipos), OMEGA_AD(ipos), S_S_AD, Hc_AD )
        rho_c_AD = S_S_AD / ( X_AD(ipos) * Hc_AD )
        nuc_AD   = 0.5_xp * OMEGA_AD(ipos) * Hc_AD**2._xp
        Kffc     = 6.13E18 * rho_c_AD * Temp_c_AD**(-7._xp/2._xp) * rho_0 * TEMP_0**(-7._xp/2._xp)
        Fzc      = F_Z_DIFF_0 * X_AD(ipos) * Temp_c_AD**4._xp /( (KAPPA_E + Kffc) * S_S_AD )

        Q_plus_c  = nuc_AD * OMEGA_AD(ipos)**2._xp * Q_PLUS_0
        Q_moins_c = 2._xp * X_AD(ipos) * Fzc / ( S_S_AD * S_0 )
        Fc        = Q_plus_c - Q_moins_c

        ! Si f(a) * f(c) < 0 (ie signes différents), le point C devient le nouveau point B
        IF ((Fa*Fc) < 0.0_xp) THEN

          Temp_b_AD = Temp_c_AD
          Hb_AD     = Hc_AD
          rho_b_AD  = rho_c_AD                                                                                  
          nub_AD    = nuc_AD
          Kffb      = Kffc
          Fzb       = Fzc
          Q_plus_b  = Q_plus_c
          Q_moins_b = Q_moins_c
          Fb        = Fc

        ! Sinon, le point C devient le nouveau point A
        ELSE 

           Temp_a_AD = Temp_c_AD
           Ha_AD     = Hc_AD                                                                                        
           rho_a_AD  = rho_c_AD
           nua_AD    = nuc_AD
           Kffa      = Kffc
           Fza       = Fzc
           Q_plus_a  = Q_plus_c
           Q_moins_a = Q_moins_c
           Fa        = Fc

        ENDIF

        ! Calcul de l'erreur
        eps = ABS(Fc)
        counter = counter + 1

    ENDDO

   ! Ecriture uniquement si on a convergé à la précision souhaitée
   IF (counter < LIMIT_DICHO) THEN
        ecriture = .true.
    ELSE
        ecriture = .false.
    ENDIF

!---------------------------------------------------------------------------------------------------
END SUBROUTINE DICHOTOMIE_TEMP
!---------------------------------------------------------------------------------------------------

!===================================================================================================
END MODULE MODULE_DICHO
!===================================================================================================