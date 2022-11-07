module module_dicho
use module_constantes
implicit none

CONTAINS

subroutine dichotomie(T,a,b,mince)
!Calcul du zéro d'une fonction définie préalablement.

   real(kind=xp), INTENT(in) :: T,a,b
   real(kind=xp) :: prec=0.01_xp
   real(kind=xp) :: eps=10_xp
   real(kind=xp), INTENT(out) :: c
   LOGICAL, INTENT(in) :: mince
   
   IF (mince==.true.) THEN
      DO WHILE(eps>prec)
         c=(a+b)/2_xp
         IF (F_mince(T,a)*F_mince(T,c)<0) THEN
            b=c
         ELSE 
            a=c
         ENDIF
         eps=F_mince(T,c)
      ENDDO
   ELSE 
      DO WHILE(eps>prec)
         c=(a+b)/2_xp
         IF (F_epais(T,a)*F_epais(T,c)<0) THEN
            b=c
         ELSE 
            a=c
         ENDIF
         eps=F_epais(T,c)
      ENDDO
   
end subroutine

end module module_dicho
