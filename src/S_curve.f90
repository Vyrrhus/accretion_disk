PROGRAM courbe_S
USE module_declarations
USE module_fonctions_utiles
USE module_dicho
IMPLICIT NONE

!Déclarations

INTEGER,       PARAMETER      :: n=1
REAL(KIND=xp), DIMENSION(n)   :: T_epais
REAL(KIND=xp), DIMENSION(n)   :: T_mince
REAL(KIND=xp), DIMENSION(n)   :: S_epais
REAL(KIND=xp), DIMENSION(n)   :: S_mince
REAL(KIND=xp)                 :: Sd
REAL(KIND=xp)                 :: Sg
INTEGER                       :: j
REAL(KIND=xp)                 :: Tmin, Tmax
REAL(KIND=xp)                 :: Sa, Sb, Sc 
LOGICAL                       :: mince=.false.

Tmin=1E-2
Tmax=1._xp
Sa=1E2
Sb=1E3
j=1

T_epais(1)=Tmin
CALL dichotomie(T_epais(1),Sa,Sb,j,mince,Sc)
S_epais(1)=Sc

!DO i=2,n
!   Sa=1E2
!   Sb=1E3
!   T_epais(i)=T_epais(i-1)+(Tmax-Tmin)/n
!   CALL dichotomie(T_epais(i),Sa,Sb,j,mince,Sc)
!   S_epais(i)=Sc
!ENDDO


PRINT*, T_epais
PRINT*, S_epais

END PROGRAM courbe_S