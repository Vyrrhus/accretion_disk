module courbe_S
USE module_branche_epais
USE module_constantes
USE MODULE_DECLARATIONS
USE module_fonctions_utiles
IMPLICIT NONE

real(kind=xp), DIMENSION(n)   :: T_epais
real(kind=xp), DIMENSION(n)   :: T_mince
real(kind=xp), DIMENSION(n)   :: S_epais
real(kind=xp), DIMENSION(n)   :: S_mince
real(kind=xp)                 :: Sd
real(kind=xp)                 :: Sg
real(kind=xp)                 :: H
integer                       :: i

T_epais(1)=0.1
DO i=2,n
   T_epais(i)=T_epais(i-1)+0.1   
ENDDO

T_mince(1)=0.1
DO i=2,n
   T_mince(i)=T_mince(i-1)+0.1
ENDDO


end module courbe_S