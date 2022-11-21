!===================================================================================================
MODULE CONDITIONS_INITIALES
!===================================================================================================
!> Ce module contient :
!> - Les conditions initiales sur T_AD, S_AD et H_AD.
!===================================================================================================

    USE MODULE_DECLARATIONS 
    IMPLICIT NONE

    REAL(kind=xp), dimension(NX)   :: T_AD_INI, S_AD_INI, H_AD_INI     !! Tableaux initiaux de T_AD, S_AD et H_AD
    REAL(kind=xp)                  :: M_0_DOT_FRAC                     !! Fraction du M_0_DOT utilisé pour calculer les conditions initiales 
    REAL(kind=xp), dimension(NX)   :: F_RG                             !! Facteur correctif de relativité générale
    
    CONTAINS

    SUBROUTINE CREATION_CONDITIONS_INITIALES()

        F_RG = 1-(X_AD**2 /3)**(-1._XP/2._XP)

        T_AD_INI = 1.4*10**4._XP/T_0 * ALPHA**(-1._XP/5._XP)  * F_RG**(3._XP/10._XP) * &
                   & (M_0_DOT*M_0_DOT_FRAC/(10**13._XP))**(3._XP/10._XP) * (M/M_O)**(1._XP/4._XP) * &
                   & (R_S/10**8._XP)**(-3._XP/4._XP) * X_AD**(-3._XP/2._XP) 

        S_AD_INI = 52._XP/S_0        * ALPHA**(-4._XP/5._XP)  * F_RG**(7._XP/10._XP) * &
                   &(M_0_DOT*M_0_DOT_FRAC/(10**13._XP))**(7._XP/10._XP) * (M/M_O)**(1._XP/4._XP)  * &
                   &(R_S/10**8._XP)**(-3._XP/4._XP) * X_AD**(-1._XP/2._XP)

        H_AD_INI = 1.7_XP*10**(-2)   * ALPHA**(-1._XP/10._XP) * F_RG**(3._XP/5._XP)  * &
                   &(M_0_DOT*M_0_DOT_FRAC/(10**13._XP))**(3._XP/20._XP) * (M/M_O)**(-3._XP/5._XP) * &
                   &(R_S/10**8._XP)**(1._XP/8._XP)  * X_AD**(9._XP/4._XP)

    END SUBROUTINE

END MODULE CONDITIONS_INITIALES