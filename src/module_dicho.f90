MODULE module_dicho
USE module_declarations
USE module_fonctions_utiles
IMPLICIT NONE

CONTAINS

subroutine dichotomie(T,a,b,mince)
!Calcul du zéro d'une fonction définie préalablement.

   REAL(KIND=xp), INTENT(in)   :: T,a,b
   REAL(KIND=xp)               :: prec=0.01_xp     !!Précision de la dichotomie
   REAL(KIND=xp)               :: eps=10_xp
   REAL(KIND=xp), INTENT(out)  :: c
   LOGICAL, INTENT(in)         :: mince            !!Booléen pour savoir dans quelle branche on est
   
   IF (mince==.true.) THEN
      DO WHILE(eps>prec)
         c=(a+b)/2_xp
         IF (F_mince(calc_P_rad(T),calc_P_gaz(T),third_term)*F_mince(calc_P_rad(T),calc_P_gaz(T),third_term)<0.0_xp) THEN
            b=c
         ELSE 
            a=c
         ENDIF
         eps=F_mince(calc_P_rad(T),calc_P_gaz(T),third_term)
      ENDDO
   ELSE 
      DO WHILE(eps>prec)
         c=(a+b)/2_xp
         IF (F_epais(calc_P_rad(T),calc_P_gaz(T),third_term)*F_epais(calc_P_rad(T),calc_P_gaz(T),third_term)<0.0_xp) THEN
            b=c
         ELSE 
            a=c
         ENDIF
         eps=F_epais(calc_P_rad(T),calc_P_gaz(T),third_term)
      ENDDO
   
end subroutine

end module module_dicho
