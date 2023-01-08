PROGRAM courbe_S
USE module_declarations
USE module_fonctions_utiles
USE module_dicho
IMPLICIT NONE

!Déclarations

INTEGER,       PARAMETER      :: n=1000                                        !! Nombre de points pour la courbe en S
REAL(KIND=xp), DIMENSION(n)   :: Temp_epais                                   !! Tableau des températures pour la branche épaise
REAL(KIND=xp), DIMENSION(n)   :: Temp_mince                                   !! Tableau des températures pour la branche mince
REAL(KIND=xp), DIMENSION(n)   :: S_epais                                      !! Tableau des densités pour la branche épaise
REAL(KIND=xp), DIMENSION(n)   :: S_mince                                      !! Tableau des densités pour la branche mince
INTEGER                       :: j, i
REAL(KIND=xp)                 :: Temp_min, Temp_max                           !! Températures maximales et minimales pour la dicho
REAL(KIND=xp)                 :: Sa, Sb, Sc                                   !! Trois densités pour la dicho
LOGICAL                       :: mince                                        !! Booléen pour changer de branche
LOGICAL                       :: ecriture
CHARACTER(LEN=20)             :: message = "no_sig"


CALL APPEL_PARAM_INPUT()
CALL CALCUL_CONSTANTES()

OPEN (unit=10,file="results_epais.out",status="unknown")
OPEN (unit=11,file="results_mince.out",status="unknown")

DO j=1,NX

   PRINT*, "Position = ", X_AD(j)
   PRINT*, "j = ", j

   PRINT*, "Branche épaisse"

   Temp_min=1E-1_xp
   Temp_max=5E1_xp
   Sa=1E0
   Sb=1E4
   mince=.false.

   Temp_epais(1)=Temp_min
   CALL dichotomie(Temp_epais(1),Sa,Sb,j,mince,Sc,ecriture)
   IF (ecriture .eqv. .true.) THEN
      WRITE(10,*) Temp_epais(1)*Temp_0, Sc*S_0/x_ad(j)*1E-1, X_AD(j)
   ELSE
         !WRITE(10,*) Temp_epais(i)*Temp_0, message, X_AD(j)
   ENDIF
   S_epais(1)=Sc

   DO i=2,n
      Sa=1E0
      Sb=1E4
      Temp_epais(i)=Temp_epais(i-1)+(Temp_max-Temp_min)/n
      CALL dichotomie(Temp_epais(i),Sa,Sb,j,mince,Sc,ecriture)
      IF (ecriture .eqv. .true.) THEN
         WRITE(10,*) Temp_epais(i)*Temp_0, Sc*S_0/x_ad(j)*1E-1, X_AD(j)
      ELSE
         !WRITE(10,*) Temp_epais(i)*Temp_0, message, X_AD(j)
      ENDIF
      S_epais(i)=Sc
   ENDDO

!   PRINT*, "Branche mince"
!
!   Sa=1E0
!   Sb=1E4
!   mince=.true.

!   Temp_mince(1)=Temp_min
!   CALL dichotomie(Temp_mince(1),Sa,Sb,j,mince,Sc,ecriture)
!   IF (ecriture .eqv. .true.) THEN
!      WRITE(11,*) Temp_mince(1)*Temp_0, Sc*S_0/x_ad(j)*1E-1, X_AD(j)
!   ELSE
!      !WRITE(11,*) Temp_mince(1)*Temp_0, message, X_AD(j)
!   ENDIF
!   S_mince(1)=Sc

!   DO i=2,n
!      Sa=1E0
!      Sb=1E4
!      Temp_mince(i)=Temp_mince(i-1)+(Temp_max-Temp_min)/n
!      CALL dichotomie(Temp_mince(1),Sa,Sb,j,mince,Sc,ecriture)
!      IF (ecriture .eqv. .true.) THEN
!         WRITE(11,*) Temp_mince(i)*Temp_0, Sc*S_0/x_ad(j)*1E-1, X_AD(j)
!      ELSE
!         !WRITE(11,*) Temp_mince(i)*Temp_0, message, X_AD(j)
!      ENDIF
!      S_mince(i)=Sc
!   ENDDO
!
ENDDO

CLOSE(10)
CLOSE(11)

!DO i=1,NX
!   PRINT*, X_AD(i)
!ENDDO

!j=5
!Temp_min=1E7/Temp_0
!Sa=845.456*10_xp/S_0*X_AD(j)
!Sa=1E0
!Sb=1E5
!mince=.false.


!PRINT*, "Position =", X_AD(j)
!PRINT*, "T = ", Temp_min*Temp_0
!PRINT*, "S = ", Sa*S_0/X_AD(j)

!CALL dichotomie(Temp_min, Sa, Sb,j,  mince,Sc,ecriture)

!PRINT*, "Sc = ", Sc*S_0/X_AD(j)

END PROGRAM courbe_S