MODULE FONCTIONS_UTILES
USE module_declarations

CONTAINS

real(kind=xp) function trinome(a, b, cc)

    implicit none

    real(kind=xp), intent(in) :: a,b,cc
    real(kind=xp)             :: Delta

    Delta = b**2-4*a*cc
    if (Delta<0) Then
        print *,'Déterminant négatif'
    endif

    trinome=(-b+(Delta**2))/(2*a)

end function

subroutine calc_H(T_star, x_ad, Omega_star, s_star, H_star)

    USE DECLARATIONS

    real(kind=xp) , intent(in)  :: T_star, x_ad, omega_star, S_star
    real(kind=xp)               :: b_star, c_star
    real(kind=xp) , intent(out)  :: H_star

    b_star = (T_star**4*x_ad)/(omega_star**2*S_star)

    c_star = T_star/omega_star**2

    H_star=trinome(1._xp, -b_0*b_star, -c_0*c_star)

end subroutine

subroutine calc_rho(S_star, x_ad, H_star, rho_star)

    USE DECLARATIONS

    real(kind=xp)  , intent(in)     :: S_star, x_ad, H_star
    real(kind=xp)  , intent(out)    :: rho_star

    rho_star = S_star/(x_ad*H_star)

end subroutine

subroutine calc_P_rad(T_star, P_rad_star)

    USE DECLARATIONS

    real(kind=xp)  , intent(in)     :: T_star
    real(kind=xp)  , intent(out)    :: P_rad_star

    P_rad_star=(P_rad_0/P_0)*T_star**4

end subroutine

subroutine calc_P_gaz(T_star, rho_star, P_gaz_star)

    USE DECLARATIONS

    real(kind=xp)  , intent(in)     :: T_star, rho_star
    real(kind=xp)  , intent(out)    :: P_gaz_star

    P_gaz_star=(P_gaz_0/P_0)*T_star*rho_star

end subroutine

! subroutine calc_third_term(T_star, rho_star, third_term_star)

!     USE DECLARATIONS

!     real(kind=xp)  , intent(in)     :: T_star, rho_star
!     real(kind=xp)  , intent(out)    :: third_term_star

!     third_term_star=rho_star**2*T_star**0.5

! end subroutine calc_third_term

! real(kind=xp) function F_to_dicho(P_rad_star, P_gaz_star, third_term_star)

!     real(kind=xp)  , intent(in)     :: P_rad_star, P_gaz_star, third_term_star

!     F_to_dicho=1/(4*3**(1.5))*(P_rad_star+P_gaz_star)-F_Z_RAD_0*third_term_star
    
! end function F_to_dicho

end MODULE



