PROGRAM courbe_S
USE module_declarations
USE module_fonctions_utiles
USE module_dicho
IMPLICIT NONE

!Déclarations

INTEGER,       PARAMETER      :: n=100
REAL(KIND=xp), DIMENSION(n)   :: T_epais
REAL(KIND=xp), DIMENSION(n)   :: T_mince
REAL(KIND=xp), DIMENSION(n)   :: S_epais
REAL(KIND=xp), DIMENSION(n)   :: S_mince
INTEGER                       :: j, i
REAL(KIND=xp)                 :: Tmin, Tmax
REAL(KIND=xp)                 :: Sa, Sb, Sc 
LOGICAL                       :: mince=.false.


CALL APPEL_PARAM_INPUT()
CALL CALCUL_CONSTANTES()
OPEN (unit=10,file="results.out",status="unknown")

Tmin=1.32_xp
Tmax=8.37_xp
Sa=1E1
Sb=1E4
j=30

T_epais(1)=Tmin
PRINT*, "T= ", T_epais(1)
CALL dichotomie(T_epais(1),Sa,Sb,j,mince,Sc)
WRITE(10,*) T_epais(1), Sc
S_epais(1)=Sc

DO i=2,n
   Sa=1E1
   Sb=1E4
   T_epais(i)=T_epais(i-1)+(Tmax-Tmin)/n
   PRINT*, "T= ", T_epais(i)
   CALL dichotomie(T_epais(i),Sa,Sb,j,mince,Sc)
   WRITE(10,*) T_epais(i), Sc
   S_epais(i)=Sc
ENDDO


PRINT*, T_epais
PRINT*, S_epais


CLOSE(10)

END PROGRAM courbe_S