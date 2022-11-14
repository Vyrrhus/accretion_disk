module module_dicho
use module_declarations
implicit none

CONTAINS

subroutine dichotomie(T,a,b,mince)
!Calcul du zéro d'une fonction définie préalablement.

   real(kind=xp), INTENT(in)   :: T,a,b
   real(kind=xp)               :: prec=0.01_xp
   real(kind=xp)               :: eps=10_xp
   real(kind=xp), INTENT(out)  :: c
   LOGICAL, INTENT(in)         :: mince
   
   IF (mince==.true.) THEN
      DO WHILE(eps>prec)
         c=(a+b)/2_xp
         IF (F_mince(Prad(T),Pgaz(T,rho),third_term)*F_mince(Prad(T),Pgaz(T,rho),third_term)<0) THEN
            b=c
         ELSE 
            a=c
         ENDIF
         eps=F_mince(Prad(T),Pgaz(T,rho),third_term)
      ENDDO
   ELSE 
      DO WHILE(eps>prec)
         c=(a+b)/2_xp
         IF (F_epais(Prad(T),Pgaz(T,rho),third_term)*F_epais(Prad(T),Pgaz(T,rho),third_term)<0) THEN
            b=c
         ELSE 
            a=c
         ENDIF
         eps=F_epais(Prad(T),Pgaz(T,rho),third_term)
      ENDDO
   
end subroutine

end module module_dicho
