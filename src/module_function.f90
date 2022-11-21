!===================================================================================================
          MODULE MODULE_FUNCTION 
!===================================================================================================
!>   Ce module permet de calculer les variables adimensionnées dans l'ordre adéquat
!===================================================================================================

USE MODULE_DECLARATIONS
IMPLICIT NONE

!===================================================================================================
          CONTAINS 
!===================================================================================================

SUBROUTINE EQU_ALGEBRIQUES(TEMP_AD,S_AD,X_AD,NU_AD,V_AD,OMEGA_AD,RHO_AD,C_S_AD&
&,M_DOT_AD,P_AD,BETA,H_AD,F_Z,Q_PLUS_AD,Q_MOINS,TAU_EFF,KAPPA_FF,DIFF_FINIE)
!---------------------------------------------------------------------------------------------------
!>    
!---------------------------------------------------------------------------------------------------
     IMPLICIT NONE
     
     REAL(KIND=XP),INTENT(IN),DIMENSION(NX) :: TEMP_AD
     REAL(KIND=XP),INTENT(IN),DIMENSION(NX) :: S_AD
     REAL(KIND=XP),INTENT(IN),DIMENSION(NX) :: X_AD
     
     REAL(KIND=XP),DIMENSION(NX),INTENT(OUT) :: NU_AD
     REAL(KIND=XP),DIMENSION(NX),INTENT(OUT) :: V_AD
     REAL(KIND=XP),DIMENSION(NX),INTENT(OUT) :: OMEGA_AD
     REAL(KIND=XP),DIMENSION(NX),INTENT(OUT) :: RHO_AD
     REAL(KIND=XP),DIMENSION(NX),INTENT(OUT) :: C_S_AD
     REAL(KIND=XP),DIMENSION(NX),INTENT(OUT) :: M_DOT_AD
     REAL(KIND=XP),DIMENSION(NX),INTENT(OUT) :: P_AD
     REAL(KIND=XP),DIMENSION(NX),INTENT(OUT) :: BETA
     REAL(KIND=XP),DIMENSION(NX),INTENT(OUT) :: H_AD
     REAL(KIND=XP),DIMENSION(NX),INTENT(OUT) :: F_Z
     
     REAL(KIND=XP),DIMENSION(NX),INTENT(OUT) :: Q_PLUS_AD
     REAL(KIND=XP),DIMENSION(NX),INTENT(OUT) :: Q_MOINS
     !REAL(KIND=XP),DIMENSION(NX),INTENT(OUT) :: Q_ADV_AD
     
     REAL(KIND=XP),DIMENSION(NX) :: P_GAZ_AD
     REAL(KIND=XP),DIMENSION(NX) :: GAMMA_3
     REAL(KIND=XP),DIMENSION(NX) :: C_V_AD
     REAL(KIND=XP),DIMENSION(NX),INTENT(OUT) :: KAPPA_FF
     REAL(KIND=XP),DIMENSION(NX),INTENT(OUT) :: TAU_EFF
     REAL(KIND=XP),DIMENSION(NX) :: P_RAD_AD
     
     REAL(KIND=XP),DIMENSION(NX) :: B_AD
     REAL(KIND=XP),DIMENSION(NX) :: C_AD
     
     REAL(KIND=XP) :: COND_LIM_V
     
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
     
     ! Vitesse de rotation
     OMEGA_AD = 3.0_XP**(1.5_XP) * X_AD**(-3.0_XP)

     ! Pression de radiation
     P_RAD_AD = TEMP_AD**4.0_XP

     ! Demi-hauteur du disque
     B_AD = (TEMP_AD**4._XP * X_AD) / (OMEGA_AD**2._XP * S_AD)
     C_AD = TEMP_AD / (OMEGA_AD**2._XP)
     H_AD = (B_0 * B_AD + sqrt((B_0 * B_AD)**2._XP + 4._XP * C_0 * C_AD)) / 2._XP

     ! Vitesse du son
     C_S_AD = OMEGA_AD * H_AD

     ! Densité volumique
     RHO_AD = S_AD / ( X_AD * H_AD )
     
     ! Viscosité
     NU_AD = 0.5_XP * ALPHA * C_S_AD * H_AD

     ! Chaleur apportée
     Q_PLUS_AD = NU_AD * OMEGA_AD**2.0_XP

     ! Vitesse d'accrétion
     
     COND_LIM_V = -1.0_XP / (X_MAX * S_AD(NX) ) 
     V_AD = - 2.0_XP / ( X_AD * S_AD ) *  DIFF_FINIE(NU_AD*S_AD,COND_LIM_V,NX) 
     
     
     M_DOT_AD = - V_AD * S_AD * X_AD
     
     P_GAZ_AD = RHO_AD * TEMP_AD
     P_AD = P_RAD_0 / P_0 * P_RAD_AD + P_GAZ_0 / P_0 * P_GAZ_AD
     BETA = P_GAZ_0 * P_GAZ_AD / ( P_0 * P_AD )
     
     C_V_AD = ( 12.0_XP * (GAMMA_G-1.0_XP)*(1.0_XP-BETA) + BETA ) / ( ( GAMMA_G - 1.0_XP ) * BETA )
     
     ! IMPLEMENTER Q_ADV
     
     
     ! ---------
     
     KAPPA_FF = RHO_AD * TEMP_AD ** (-3.5_XP) * ( RHO_0 * T_0 **(-3.5_XP) * 6.13E18_XP)
     
     GAMMA_3 = ( 4.0_XP - 3.0_XP*BETA ) / ( BETA * C_V_AD ) +1.0_XP
     
     TAU_EFF = 0.5_XP * S_AD / X_AD * KAPPA_FF * KAPPA_E * S_0
     
     DO I=1,NX
         IF (TAU_EFF(I)>=1.0_XP) THEN
             F_Z(I) = F_Z_DIFF_0 * X_AD(I) **4.0_XP / ((KAPPA_FF(I) + KAPPA_E) * S_AD(I))
         ELSEIF (TAU_EFF(I)<1.0_XP) THEN
             F_Z(I) = F_Z_RAD_0 * RHO_AD(I)**2.0_XP * TEMP_AD(I) ** (0.5_XP) * H_AD(I)
         ENDIF
     ENDDO
     
     Q_MOINS = 2.0_XP * X_AD * F_Z / (S_AD * S_0 )
     
      
!---------------------------------------------------------------------------------------------------
END SUBROUTINE EQU_ALGEBRIQUES
!---------------------------------------------------------------------------------------------------

! MODULE QUI CALCUL LES VARIABLES À PARTIR DES VARIABLES ADIMENSIONNÉES


     

FUNCTION DIFF_FINIE(VEC,COND_LIM) RESULT(DERIV)
!---------------------------------------------------------------------------------------------------
!> Fonction pour calcul de dérivée et autre
!---------------------------------------------------------------------------------------------------
	
     IMPLICIT NONE
     REAL(KIND=XP) :: VEC(NX)
     REAL(KIND=XP) :: DERIV(NX)
     REAL(KIND=XP) :: COND_LIM

     DERIV(1:NX-1) = (VEC(1:NX-1)-VEC(2:NX) ) / DX
     DERIV(NX) = COND_LIM 

!---------------------------------------------------------------------------------------------------
END FUNCTION DIFF_FINIE
!---------------------------------------------------------------------------------------------------


! FONCTIONS ALGEBRIQUES 

!-------------------------------------------------------------
!-------------------------------------------------------------
FUNCTION EQU_C_V_GAMMA(BETA) RESULT(C_V_GAMMA)

     IMPLICIT NONE
     REAL(KIND=XP) :: BETA
     REAL(KIND=XP) :: C_V_GAMMA
     C_V_GAMMA = ( 4.0_XP - 3.0_XP * BETA ) / BETA
    
END FUNCTION EQU_C_V_GAMMA
!------------------------------------------------------------
!------------------------------------------------------------

!---------------------------------------------------------------------------
                          END MODULE MODULE_FUNCTION
!---------------------------------------------------------------------------
