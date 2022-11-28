!===================================================================================================
MODULE MODULE_CONDITIONS_INITIALES
!===================================================================================================
!> Ce module contient :
!> - Les conditions initiales sur T_AD, S_AD et H_AD.
!===================================================================================================

    USE MODULE_DECLARATIONS 
    IMPLICIT NONE

    REAL(kind=xp), dimension(NX)   :: TEMP_AD_INI, S_AD_INI, H_AD_INI     !! Tableaux initiaux de TEMP_AD, S_AD et H_AD
    REAL(kind=xp)                  :: M_0_DOT_FRAC = 0.01_XP              !! Fraction du M_0_DOT utilisé pour calculer les conditions initiales 
    REAL(kind=xp), dimension(NX)   :: F_RG                                !! Facteur correctif de relativité générale
    
    CONTAINS

    SUBROUTINE CREATION_CONDITIONS_INITIALES()

    !---------------------------------------------------------------------------------------------------
    !> Cette subroutine initialise T_AD, S_AD et H_AD
    !---------------------------------------------------------------------------------------------------
        IMPLICIT NONE 
        
        F_RG = 1.0_XP - (X_AD**2.0_XP/3.0_XP)**(-1.0_XP/2.0_XP)

        TEMP_AD_INI = 1.4E4_XP / T_0 * ALPHA**(-1.0_XP/5._XP)  * F_RG**(3.0_XP/10.0_XP)                   &
                    * (M_0_DOT * M_0_DOT_FRAC / (1E13_XP))**(3.0_XP/10.0_XP) * (M/M_O)**(1.0_XP/4.0_XP)   &
                    * (R_S/1E8_XP)**(-3._XP/4._XP) * X_AD**(-3.0_XP/2.0_XP) 

        S_AD_INI    = 52.0_XP / S_0 * ALPHA**(-4.0_XP/5.0_XP) * F_RG**(7.0_XP/10.0_XP)                    &
                    * (M_0_DOT * M_0_DOT_FRAC / (1E13_XP))**(7.0_XP/10.0_XP) * (M/M_O)**(1.0_XP/4.0_XP)  &
                    * (R_S / 1E8)**(-3.0_XP/4.0_XP) * X_AD**(-1.0_XP/2.0_XP)

        H_AD_INI    = 1.7E-2_XP   * ALPHA**(-1.0_XP/10.0_XP) * F_RG**(3.0_XP/5.0_XP)                   &
                    * (M_0_DOT*M_0_DOT_FRAC/(1E13_XP))**(3.0_XP/20.0_XP) * (M/M_O)**(-3.0_XP/5.0_XP)  &
                    * (R_S/1E8_XP)**(1.0_XP/8.0_XP)  * X_AD**(9.0_XP/4.0_XP)
        
        WRITE(*, "('--------------CONDITIONS INITIALES--------------')")
        WRITE(*, "('------------------------------------------------')")
        WRITE(*, "('FRACTION DE M_0_DOT CHOISIE                    = ', 1pE12.2)") M_0_DOT_FRAC
        WRITE(*, "('T_AD MOYEN INITIALE                            = ', 1pE12.2)") SUM(TEMP_AD_INI)/NX
        WRITE(*, "('T MOYEN INITIALE                               = ', 1pE12.2)") SUM(TEMP_AD_INI)/NX*T_0
        WRITE(*, "('S_AD MOYEN INITIALE                            = ', 1pE12.2)") SUM(S_AD_INI)/NX
        WRITE(*, "('SIGMA MOYEN INITIALE                           = ', 1pE12.2)") SUM(S_AD_INI/X_AD)/NX*SIGMA_0
        WRITE(*, "('H_AD MOYEN INITIALE                            = ', 1pE12.2)") SUM(H_AD_INI)/NX

    END SUBROUTINE

END MODULE MODULE_CONDITIONS_INITIALES