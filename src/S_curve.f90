PROGRAM courbe_S
USE module_declarations
USE module_fonctions_utiles
USE module_dicho
IMPLICIT NONE

!Déclarations

INTEGER,       PARAMETER      :: n=100                                        !! Nombre de points pour la courbe en S
REAL(KIND=xp), DIMENSION(n)   :: Temp_epais                                   !! Tableau des températures pour la branche épaise
REAL(KIND=xp), DIMENSION(n)   :: Temp_mince                                   !! Tableau des températures pour la branche mince
REAL(KIND=xp), DIMENSION(n)   :: S_epais                                      !! Tableau des densités pour la branche épaise
REAL(KIND=xp), DIMENSION(n)   :: S_mince                                      !! Tableau des densités pour la branche mince
INTEGER                       :: j, i
REAL(KIND=xp)                 :: Temp_min, Temp_max                           !! Températures maximales et minimales pour la dicho
REAL(KIND=xp)                 :: Sa, Sb, Sc                                   !! Trois densités pour la dicho
LOGICAL                       :: mince                                        !! Booléen pour changer de branche


CALL APPEL_PARAM_INPUT()
CALL CALCUL_CONSTANTES()
OPEN (unit=10,file="results_epais.out",status="unknown")
OPEN (unit=11,file="results_mince.out",status="unknown")

PRINT*, "Branche épaisse"

Temp_min=0.6_xp
Temp_max=1.82_xp
Sa=1E0
Sb=1E4
j=53
mince=.false.

Temp_epais(1)=Temp_min
PRINT*, "T= ", Temp_epais(1)
CALL dichotomie(Temp_epais(1),Sa,Sb,j,mince,Sc)
WRITE(10,*) Temp_epais(1)*Temp_0, Sc*S_0/x_ad(j)*1E-1
S_epais(1)=Sc

DO i=2,n
   Sa=1E1
   Sb=1E4
   Temp_epais(i)=Temp_epais(i-1)+(Temp_max-Temp_min)/n
   PRINT*, "T= ", Temp_epais(i)
   CALL dichotomie(Temp_epais(i),Sa,Sb,j,mince,Sc)
   WRITE(10,*) Temp_epais(i)*Temp_0, Sc*S_0/x_ad(j)*1E-1
   S_epais(i)=Sc
ENDDO

PRINT*, Temp_epais
PRINT*, S_epais*1E-1

CLOSE(10)

PRINT*, "Branche mince"

Temp_min=1.7_xp
Temp_max=2.5_xp
Sa=1E0
Sb=1E4
mince=.true.

Temp_mince(1)=Temp_min
PRINT*, "T= ", Temp_mince(1)
CALL dichotomie(Temp_mince(1),Sa,Sb,j,mince,Sc)
WRITE(11,*) Temp_mince(1)*Temp_0, Sc*S_0/x_ad(j)*1E-1
S_mince(1)=Sc

DO i=2,n
   Sa=1E0
   Sb=1E4
   Temp_mince(i)=Temp_mince(i-1)+(Temp_max-Temp_min)/n
   PRINT*, "T= ", Temp_mince(i)
   CALL dichotomie(Temp_mince(i),Sa,Sb,j,mince,Sc)
   WRITE(11,*) Temp_mince(i)*Temp_0, Sc*S_0/x_ad(j)*1E-1
   S_mince(i)=Sc
ENDDO


PRINT*, Temp_mince
PRINT*, S_mince*1E-1



CLOSE(11)

END PROGRAM courbe_S