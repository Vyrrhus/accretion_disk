PROGRAM courbe_S
USE module_declarations
USE module_fonctions_utiles
USE module_branche_epais
IMPLICIT NONE

!Déclarations

REAL(KIND=xp), DIMENSION(n,nx)   :: T_epais
REAL(KIND=xp), DIMENSION(n,nx)   :: T_mince
REAL(KIND=xp), DIMENSION(n,nx)   :: S_epais
REAL(KIND=xp), DIMENSION(n,nx)   :: S_mince
REAL(KIND=xp)                    :: Sd
REAL(KIND=xp)                    :: Sg
REAL(KIND=xp)                    :: H
INTEGER                          :: i,j

DO j=1,nx

   T_epais(1,j)=0.1
   DO i=2,n
      T_epais(i,j)=T_epais(i-1,j)+0.1   
   ENDDO

   T_mince(1,j)=0.1
   DO i=2,n
      T_mince(i,j)=T_mince(i-1,j)+0.1
   ENDDO

   DO i=1,n
      
   ENDDO
ENDDO


END PROGRAM courbe_S