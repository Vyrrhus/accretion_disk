!-------------------------------------------------------------
                 MODULE MODULE_FUNCTION 
!-------------------------------------------------------------

USE MODULE_DECLARATIONS

IMPLICIT NONE

                         CONTAINS 
!-------------------------------------------------------------

SUBROUTINE EQU_ALGEBRIQUES(TEMP_AD,S_AD,X_AD,COND_LIM_NU,COND_LIM_S,DIFF_FINIE,FONCTION_H)

     IMPLICIT NONE
     REAL(KIND=XP),INTENT(IN),DIMENSION(NX) :: TEMP_AD
     REAL(KIND=XP),INTENT(IN),DIMENSION(NX) :: S_AD
     REAL(KIND=XP),INTENT(IN),DIMENSION(NX) :: X_AD
     REAL(KIND=XP) :: COND_LIM_NU,COND_LIM_S
     
     REAL(KIND=XP),DIMENSION(NX) :: NU_AD
     REAL(KIND=XP),DIMENSION(NX) :: V_AD
     REAL(KIND=XP),DIMENSION(NX) :: OMEGA_AD
     REAL(KIND=XP),DIMENSION(NX) :: RHO_AD
     REAL(KIND=XP),DIMENSION(NX) :: C_S_AD
     REAL(KIND=XP),DIMENSION(NX) :: P_RAD_AD
     REAL(KIND=XP),DIMENSION(NX) :: Q_PLUS_AD
     REAL(KIND=XP),DIMENSION(NX) :: M_DOT_AD
     REAL(KIND=XP),DIMENSION(NX) :: P_GAZ_AD
     REAL(KIND=XP),DIMENSION(NX) :: P_AD
     REAL(KIND=XP),DIMENSION(NX) :: BETA
     REAL(KIND=XP),DIMENSION(NX) :: C_V_AD
     REAL(KIND=XP),DIMENSION(NX) :: KAPPA_FF
     REAL(KIND=XP),DIMENSION(NX) :: H_AD
     
     INTERFACE 
     	FUNCTION DIFF_FINIE(VEC,COND_LIM,NX)
     	IMPLICIT NONE
     	INTEGER,PARAMETER :: XP = SELECTED_REAL_KIND(15)
     	INTEGER :: NX
     	REAL(KIND=XP) :: COND_LIM
     	REAL(KIND=XP) :: VEC(NX)
     	REAL(KIND=XP) :: DIFF_FINIE(NX)
     	END FUNCTION DIFF_FINIE
     END INTERFACE
     
     INTERFACE 
     	FUNCTION FONCTION_H(NX)
     	IMPLICIT NONE
     	INTEGER,PARAMETER :: XP = SELECTED_REAL_KIND(15)
     	INTEGER :: NX
     	REAL(KIND=XP) :: FONCTION_H(NX)
     	END FUNCTION FONCTION_H
     END INTERFACE
     
     
     
     OMEGA_AD = 3.0_XP**(1.5_XP)*X_AD**(-3.0_XP)
     
     P_RAD_AD = TEMP_AD ** 4.0_XP
     
     ! IMPLEMENTER EQUATION DE H
     H_AD = 0.0_XP
     C_S_AD = OMEGA_AD * H_AD
     RHO_AD = S_AD / ( X_AD * H_AD )
     
     NU_AD = 0.5_XP * ALPHA * C_S_AD * H_AD
     Q_PLUS_AD = NU_AD * OMEGA_AD ** 2.0_XP
     
     ! IMPLEMENTER EQUATION VITESSE
     !--------
     V_AD = - 2.0_XP / ( X_AD * S_AD ) * ( S_AD * DIFF_FINIE(NU_AD,COND_LIM_NU,NX) + NU_AD * DIFF_FINIE(S_AD,COND_LIM_S,NX) )
     !--------
     M_DOT_AD = - V_AD * S_AD * X_AD
     
     P_GAZ_AD = RHO_AD * TEMP_AD
     P_AD = P_RAD_0 / P_0 * P_RAD_AD + P_GAZ_0 / P_0 * P_GAZ_AD
     BETA = P_GAZ_0 * P_GAZ_AD / ( P_0 * P_AD )
     
     C_V_AD = ( 12.0_XP * (GAMMA_G-1.0_XP)*(1.0_XP-BETA) + BETA ) / ( ( GAMMA_G - 1.0_XP ) * BETA )
     
     ! IMPLEMENTER GAMMA 3 ET Q_ADV
     
     KAPPA_FF = RHO_AD * TEMP_AD ** (-3.5_XP) * ( RHO_0 * T_0 **(-3.5_XP) * 6.13E18_XP)
     
     
END SUBROUTINE EQU_ALGEBRIQUES

!-------------------------------------------------------------
!-------------------------------------------------------------
! FUNCTION POUR CALCUL DERIVEE ET AUTRE
!-------------------------------------------------------------
!-------------------------------------------------------------
FUNCTION DIFF_FINIE(VEC,COND_LIM) RESULT(DERIV)
	
     IMPLICIT NONE
     REAL(KIND=XP) :: VEC(NX)
     REAL(KIND=XP) :: DERIV(NX)
     REAL(KIND=XP) :: DX,COND_LIM
     DX = ( R_MAX - R_MIN ) / NX 
     DERIV(1:NX-1) = ( VEC(1:NX-1)-VEC(2:NX) ) / DX
     DERIV(NX) = COND_LIM 
	
END FUNCTION DIFF_FINIE
!-------------------------------------------------------------
!-------------------------------------------------------------


! FONCTIONS ALGEBRIQUES 

!-------------------------------------------------------------
!-------------------------------------------------------------
FUNCTION EQU_ROTATION(X_AD) RESULT(OMEGA_AD)

     IMPLICIT NONE
     REAL(KIND=XP) :: X_AD(NX)
     REAL(KIND=XP) :: OMEGA_AD(NX)
     OMEGA_AD = 3.0_XP**(1.5_XP)*X_AD**(-3.0_XP)

END FUNCTION EQU_ROTATION
!-------------------------------------------------------------
!-------------------------------------------------------------
FUNCTION EQU_P_GAZ(RHO_AD,TEMP_AD) RESULT(P_GAZ_AD)

     IMPLICIT NONE
     REAL(KIND=XP) :: RHO_AD(NX),TEMP_AD(NX)
     REAL(KIND=XP) :: P_GAZ_AD(NX)
     P_GAZ_AD = RHO_AD * TEMP_AD

END FUNCTION EQU_P_GAZ
!-------------------------------------------------------------
!-------------------------------------------------------------
FUNCTION EQU_P_RAD(TEMP_AD) RESULT(P_RAD_AD)

     IMPLICIT NONE
     REAL(KIND=XP) :: TEMP_AD(NX)
     REAL(KIND=XP) :: P_RAD_AD(NX)
     P_RAD_AD = TEMP_AD**4.0_XP

END FUNCTION EQU_P_RAD
!-------------------------------------------------------------
!-------------------------------------------------------------
FUNCTION EQU_P(P_RAD_AD,P_GAZ_AD) RESULT(P_AD)

     IMPLICIT NONE
     REAL(KIND=XP),DIMENSION(NX) :: P_RAD_AD,P_GAZ_AD
     REAL(KIND=XP) :: P_AD(NX)
     P_AD = P_RAD_0 / P_0 * P_RAD_AD + P_GAZ_0 / P_0 * P_GAZ_AD

END FUNCTION EQU_P
!-------------------------------------------------------------
!-------------------------------------------------------------
FUNCTION EQU_INDIC_PRESSION(P_GAZ_AD,P_AD) RESULT(BETA)

     IMPLICIT NONE
     REAL(KIND=XP) :: P_GAZ_AD(NX),P_AD(NX)
     REAL(KIND=XP) :: BETA(NX)
     BETA = P_GAZ_0 * P_GAZ_AD / ( P_0 * P_AD )

END FUNCTION EQU_INDIC_PRESSION
!-------------------------------------------------------------
!-------------------------------------------------------------
FUNCTION EQU_VITESSE_SON(P_AD,RHO_AD) RESULT(C_S_AD)

     IMPLICIT NONE
     REAL(KIND=XP) :: P_AD(NX),RHO_AD(NX)
     REAL(KIND=XP) :: C_S_AD(NX)
     C_S_AD = ( P_AD / RHO_AD ) ** (0.5_XP)

END FUNCTION EQU_VITESSE_SON
!-------------------------------------------------------------
!-------------------------------------------------------------
FUNCTION EQU_HAUTEUR_DISQUE(C_S_AD,OMEGA_AD) RESULT(H_AD)

     IMPLICIT NONE
     REAL(KIND=XP) :: C_S_AD(NX),OMEGA_AD(NX)
     REAL(KIND=XP) :: H_AD(NX)
     H_AD = C_S_AD / OMEGA_AD
	
END FUNCTION EQU_HAUTEUR_DISQUE
!-------------------------------------------------------------
!-------------------------------------------------------------
FUNCTION EQU_RHO(X_AD,S_AD,H_AD) RESULT(RHO_AD)

     IMPLICIT NONE
     REAL(KIND=XP) :: X_AD(NX),S_AD(NX),H_AD(NX)
     REAL(KIND=XP) :: RHO_AD(NX)
     RHO_AD = S_AD / ( X_AD * H_AD )
	
END FUNCTION EQU_RHO
!-------------------------------------------------------------
!-------------------------------------------------------------
FUNCTION EQU_VISCOSITE(C_S_AD,H_AD) RESULT(NU_AD)

     IMPLICIT NONE
     REAL(KIND=XP) :: C_S_AD(NX),H_AD(NX)
     REAL(KIND=XP) :: NU_AD(NX)
     NU_AD = 0.5_XP * ALPHA * C_S_AD * H_AD

END FUNCTION EQU_VISCOSITE
!-------------------------------------------------------------
!-------------------------------------------------------------
FUNCTION EQU_VITESSE_ACCRETION(DIFF_FINIE,X_AD,S_AD,NU_AD,COND_LIM_NU,COND_LIM_S) RESULT(V_AD)

     IMPLICIT NONE
     REAL(KIND=XP) :: COND_LIM_NU,COND_LIM_S
     REAL(KIND=XP) :: NU_AD(NX),S_AD(NX),X_AD(NX)
     REAL(KIND=XP) :: V_AD(NX)
     
     INTERFACE 
     	FUNCTION DIFF_FINIE(VEC,COND_LIM,NX)
     	IMPLICIT NONE
     	INTEGER,PARAMETER :: XP = SELECTED_REAL_KIND(15)
     	INTEGER :: NX
     	REAL(KIND=XP) :: COND_LIM
     	REAL(KIND=XP) :: VEC(NX)
     	REAL(KIND=XP) :: DIFF_FINIE(NX)
     	END FUNCTION DIFF_FINIE
     END INTERFACE
     
     V_AD = - 2.0_XP / ( X_AD * S_AD ) * ( S_AD * DIFF_FINIE(NU_AD,COND_LIM_NU,NX) + NU_AD * DIFF_FINIE(S_AD,COND_LIM_S,NX) )
     
END FUNCTION EQU_VITESSE_ACCRETION
!-------------------------------------------------------------
!-------------------------------------------------------------
FUNCTION EQU_TAU_ACCRETION(V_AD,S_AD,X_AD) RESULT(M_DOT_AD)

     IMPLICIT NONE
     REAL(KIND=XP) :: V_AD(NX),S_AD(NX),X_AD(NX)
     REAL(KIND=XP) :: M_DOT_AD(NX)
     M_DOT_AD = - V_AD * S_AD * X_AD

END FUNCTION EQU_TAU_ACCRETION
!-------------------------------------------------------------
!-------------------------------------------------------------
FUNCTION EQU_Q_PLUS(NU_AD,OMEGA_AD) RESULT(Q_PLUS_AD)

     IMPLICIT NONE
     REAL(KIND=XP), INTENT(IN) :: NU_AD(NX),OMEGA_AD(NX)
     REAL(KIND=XP) :: Q_PLUS_AD(NX)
     Q_PLUS_AD = NU_AD * OMEGA_AD * 2.0_XP

END FUNCTION EQU_Q_PLUS
!-------------------------------------------------------------
!-------------------------------------------------------------
 FUNCTION EQU_Q_MOINS(X_AD,S_AD,F_Z) RESULT(Q_MOINS)

     IMPLICIT NONE
     REAL(KIND=XP), INTENT(IN) :: X_AD(NX),S_AD(NX),F_Z(NX)
     REAL(KIND=XP) :: Q_MOINS(NX)
     Q_MOINS = 2.0_XP * X_AD * F_Z / ( S_AD * S_0 )

END FUNCTION EQU_Q_MOINS
!-------------------------------------------------------------
!-------------------------------------------------------------
FUNCTION EQU_C_V(BETA) RESULT(C_V_AD)
     
     IMPLICIT NONE
     REAL(KIND=XP) :: BETA
     REAL(KIND=XP) :: C_V_AD(NX)
     
     C_V_AD = ( 12.0_XP * (GAMMA_G-1.0_XP)*(1.0_XP-BETA) + BETA ) / ( ( GAMMA_G - 1.0_XP ) * BETA )

END FUNCTION EQU_C_V
!-------------------------------------------------------------
!-------------------------------------------------------------
FUNCTION EQU_C_V_GAMMA(BETA) RESULT(C_V_GAMMA)

     IMPLICIT NONE
     REAL(KIND=XP) :: BETA
     REAL(KIND=XP) :: C_V_GAMMA
     C_V_GAMMA = ( 4.0_XP - 3.0_XP * BETA ) / BETA
    
END FUNCTION EQU_C_V_GAMMA
!-------------------------------------------------------------
!-------------------------------------------------------------
FUNCTION EQU_FLUX_DIFF(X_AD,KAPPA_FF,S_AD) RESULT(F_Z_AD)

     IMPLICIT NONE
     REAL(KIND=XP) :: X_AD(NX),KAPPA_FF(nx),S_AD(NX)
     REAL(KIND=XP) :: F_Z_AD(NX)
     F_Z_AD = F_Z_DIFF_0 * X_AD **4.0_XP / ((KAPPA_FF + KAPPA_E) * S_AD)

END FUNCTION EQU_FLUX_DIFF
!-------------------------------------------------------------
!-------------------------------------------------------------
FUNCTION EQU_FLUX_RAD(RHO_AD,TEMP_AD,H_AD) RESULT(F_Z_AD)

     IMPLICIT NONE
     REAL(KIND=XP) :: RHO_AD(NX),TEMP_AD(NX),H_AD(NX)
     REAL(KIND=XP) :: F_Z_AD(NX)
     F_Z_AD = F_Z_RAD_0 * RHO_AD**2.0_XP * TEMP_AD ** (0.5_XP) * H_AD

END FUNCTION EQU_FLUX_RAD
!------------------------------------------------------------
!------------------------------------------------------------
FUNCTION EQU_TAU_EFF(S_AD,X_AD,KAPPA_FF) RESULT(TAU_EFF)

     IMPLICIT NONE
     REAL(KIND=XP) :: S_AD(NX),X_AD(NX),KAPPA_FF(NX)
     REAL(KIND=XP) :: TAU_EFF(NX)
     TAU_EFF = 0.5_XP * KAPPA_FF * KAPPA_E * S_0 / X_AD

END FUNCTION EQU_TAU_EFF
!------------------------------------------------------------
!------------------------------------------------------------
FUNCTION EQU_KAPPA_FF(RHO_AD,TEMP_AD) RESULT(KAPPA_FF)

     IMPLICIT NONE
     REAL(KIND=XP) :: RHO_AD(NX),TEMP_AD(NX)
     REAL(KIND=XP) :: KAPPA_FF(NX)
     KAPPA_FF = RHO_AD * TEMP_AD ** (-3.5_XP) * ( RHO_0 * T_0 **(-3.5_XP) * 6.13E18_XP)

END FUNCTION EQU_KAPPA_FF
!------------------------------------------------------------
!------------------------------------------------------------
FUNCTION EQU_EPSILON_FF(RHO_AD,TEMP_AD) RESULT(EPSILON_FF)
     IMPLICIT NONE
     REAL(KIND=XP) :: RHO_AD(NX),TEMP_AD(NX)
     REAL(KIND=XP) :: EPSILON_FF(NX)
     EPSILON_FF = RHO_AD**2.0_XP * TEMP_AD**0.5_XP * RHO_0**2.0_XP * T_0**0.5_XP * 6.22E13
END FUNCTION EQU_EPSILON_FF
!------------------------------------------------------------
!------------------------------------------------------------

!---------------------------------------------------------------------------
                          END MODULE MODULE_FUNCTION
!---------------------------------------------------------------------------