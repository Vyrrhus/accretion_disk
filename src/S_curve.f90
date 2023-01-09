PROGRAM courbe_S
USE module_declarations
USE module_fonctions_utiles
USE module_dicho
IMPLICIT NONE

!Déclarations

INTEGER,       PARAMETER      :: n=5000                                       !! Nombre de points pour la courbe en S
REAL(KIND=xp), DIMENSION(n)   :: Temp_epais_AD                                !! Tableau des températures pour la branche épaise
REAL(KIND=xp), DIMENSION(n)   :: Temp_mince_AD                                !! Tableau des températures pour la branche mince
INTEGER                       :: j, i
REAL(KIND=xp)                 :: Temp_min_AD, Temp_max_AD                     !! Températures maximales et minimales pour la dicho
REAL(KIND=xp)                 :: Sa, Sb, Sc                                   !! Trois densités pour la dicho
REAL(KINd=xp)                 :: Tau_ff                                       
LOGICAL                       :: mince                                        !! Booléen pour changer de branche
LOGICAL                       :: ecriture                                     !! Booléen pour écrire ou non dans le fichier
CHARACTER(LEN=20)             :: message = "no_sig"


CALL APPEL_PARAM_INPUT()
CALL CALCUL_CONSTANTES()

OPEN (unit=10, file="results_epais.out", status="unknown")

PRINT*, "Branche épaisse"

!----------------------------------------------------------------------------------------
!Calcul pour la branche épaisse
!------------------------------------------------------------------------------------------

mince=.false.

Temp_min_AD = 5E-2_xp
Temp_max_AD = 5E1_xp

!Bornes pour la dichotomie
Sa = 1E0_xp
Sb = 1E6_xp

DO j=1,NX

   PRINT*, "Position = ", X_AD(j)
   PRINT*, "j = ", j
   
   !--------------------------------------------------------------------------------------------------
   !Calcul pour la première température
   !--------------------------------------------------------------------------------------------------

   Temp_epais_AD(1)=Temp_min_AD

   CALL dichotomie( Temp_epais_AD(1), Sa, Sb, j, mince, Sc, ecriture, Tau_ff)

   IF (ecriture .eqv. .true.) THEN
      WRITE(10,*) Temp_epais_AD(1) * Temp_0, Sc * S_0 / x_ad(j) * 1E-1, X_AD(j)      !Si la dichotomie a trouvé un zéro écrit dans le fichier en cgs
   ELSE
         !WRITE(10,*) Temp_epais(i)*Temp_0, message, X_AD(j)
   ENDIF

   !---------------------------------------------------------------------------------------------------------------
   !Boucle pour les autres températures
   !--------------------------------------------------------------------------------------------------------------

   DO i=2,n

      Sa=1E0_xp                                                                               !On refixe les bornes car elles ont changé dans la dichotomie précédente
      Sb=1E6_xp
   
      Temp_epais_AD(i) = Temp_epais_AD(i-1) + (Temp_max_AD - Temp_min_AD) / n                 !On calcule la température suivante

      CALL dichotomie( Temp_epais_AD(i), Sa, Sb, j, mince, Sc, ecriture, Tau_ff)

      IF (ecriture .eqv. .true.) THEN
         WRITE(10,*) Temp_epais_AD(i) * Temp_0, Sc * S_0 / x_ad(j) * 1E-1, X_AD(j)                       !Si la dichotomie a trouvé un zéro écrit dans le fichier en cgs
      ELSE
         !WRITE(10,*) Temp_epais(i)*Temp_0, message, X_AD(j)
      ENDIF

   ENDDO
ENDDO

CLOSE(10)


OPEN (unit=11,file="results_mince.out",status="unknown")

PRINT*, "Branche mince"

!------------------------------------------------------------------------------------------------------------------------------------
!Calcul pour la branche mince
!-----------------------------------------------------------------------------------------------------------------------------------
mince=.true.

Temp_min_AD=5E-2_xp
Temp_max_AD=1E2_xp

!Bornes pour la dichotomie
Sa=1E0_xp
Sb=1E6_xp

DO j=1,NX

   PRINT*, "Position = ", X_AD(j)
   PRINT*, "j = ", j

   !--------------------------------------------------------------------------------------------------
   !Calcul pour la première température
   !--------------------------------------------------------------------------------------------------

   Temp_mince_AD(1)=Temp_min_AD

   CALL dichotomie( Temp_mince_AD(1), Sa, Sb, j, mince, Sc, ecriture, Tau_ff)

   IF (ecriture .eqv. .true.) THEN
      WRITE(11,*) Temp_mince_AD(1)*Temp_0, Sc*S_0/x_ad(j)*1E-1, X_AD(j)                                !Si la dichotomie a trouvé un zéro écrit dans le fichier en cgs
   ELSE
      !WRITE(11,*) Temp_mince(1)*Temp_0, message, X_AD(j)
   ENDIF

   !---------------------------------------------------------------------------------------------------------------
   !Boucle pour les autres températures
   !--------------------------------------------------------------------------------------------------------------

   DO i=2,n

      Sa=1E0_xp
      Sb=1E6_xp

      Temp_mince_AD(i)=Temp_mince_AD(i-1) + ( Temp_max_AD - Temp_min_AD ) / n

      CALL dichotomie( Temp_mince_AD(i), Sa, Sb, j, mince, Sc, ecriture, Tau_ff)

      IF (ecriture .eqv. .true.) THEN
         WRITE(11,*) Temp_mince_AD(i) * Temp_0, Sc * S_0 / x_ad(j)*1E-1, X_AD(j)                     !Si la dichotomie a trouvé un zéro écrit dans le fichier en cgs
      ELSE
         !WRITE(11,*) Temp_mince(i)*Temp_0, message, X_AD(j)
      ENDIF
   ENDDO

ENDDO

CLOSE(11)

END PROGRAM courbe_S