MODULE MODULE_S_CURVE
USE module_declarations
USE module_fonctions_utiles
USE module_dicho
IMPLICIT NONE

CONTAINS

SUBROUTINE S_curve(Temp_min_AD, Temp_max_AD, Sg_AD, Sd_AD, n_s)
!---------------------------------------------------------------------------------------------------------------------
! Calcul des courbes en S
!------------------------------------------------------------------------------------------------------------------------------

INTEGER,       INTENT(in)     :: n_s                                            !! Nombre de points pour la courbe en S
REAL(KIND=xp), DIMENSION(n_s)   :: Temp_epais_AD                                !! Tableau des températures pour la branche épaise
REAL(KIND=xp), DIMENSION(n_s)   :: Temp_mince_AD                                !! Tableau des températures pour la branche mince
INTEGER                       :: j, i
REAL(KIND=xp), INTENT(in)     :: Temp_min_AD, Temp_max_AD                     !! Températures maximales et minimales pour la dicho
REAL(KIND=xp), INTENT(in)     :: Sg_AD, Sd_AD                                 !! Bornes initiales pour la dicho
REAL(KIND=xp)                 :: Sa_AD, Sb_AD                                 !! Points de gauche et de droite pour la dicho
REAL(KIND=xp)                 :: Sc_AD                                        !! Point milieu de la dichotomie
LOGICAL                       :: mince                                        !! Booléen pour changer de branche
LOGICAL                       :: ecriture                                     !! Booléen pour écrire ou non dans le fichier

OPEN (unit=10, file="./output/results_epais.out", status="unknown")

!----------------------------------------------------------------------------------------
!Calcul pour la branche épaisse
!------------------------------------------------------------------------------------------

mince=.false.

DO j=1,NX
   
   !--------------------------------------------------------------------------------------------------
   !Calcul pour la première température
   !--------------------------------------------------------------------------------------------------

   Temp_epais_AD(1)=Temp_min_AD

   Sa_AD=Sg_AD
   Sb_AD=Sd_AD

   CALL dichotomie( Temp_epais_AD(1), Sa_AD, Sb_AD, j, mince, Sc_AD, ecriture)

   IF (ecriture .eqv. .true.) THEN
      WRITE(10,*) Temp_epais_AD(1) * Temp_0, Sc_AD * S_0 / x_ad(j) * 1E-1, X_AD(j)**2._xp * R_S, X_AD(j)      !Si la dichotomie a trouvé un zéro écrit dans le fichier en cgs
   ELSE
         !WRITE(10,*) Temp_epais(i)*Temp_0, message, X_AD(j)
   ENDIF

   !---------------------------------------------------------------------------------------------------------------
   !Boucle pour les autres températures
   !--------------------------------------------------------------------------------------------------------------

   DO i=2,n_s

      Sa_AD=Sg_AD                                                                               !On refixe les bornes car elles ont changé dans la dichotomie précédente
      Sb_AD=Sd_AD
   
      Temp_epais_AD(i) = Temp_epais_AD(i-1) + (Temp_max_AD - Temp_min_AD) / n_s                 !On calcule la température suivante

      CALL dichotomie( Temp_epais_AD(i), Sa_AD, Sb_AD, j, mince, Sc_AD, ecriture)

      IF (ecriture .eqv. .true.) THEN
         WRITE(10,*) Temp_epais_AD(i) * Temp_0, Sc_AD * S_0 / x_ad(j) * 1E-1, X_AD(j)**2._xp * R_S, X_AD(j)                       !Si la dichotomie a trouvé un zéro écrit dans le fichier en cgs
      ELSE
         !WRITE(10,*) Temp_epais(i)*Temp_0, message, X_AD(j)
      ENDIF

   ENDDO
ENDDO

CLOSE(10)


OPEN (unit=11,file="./output/results_mince.out",status="unknown")

!------------------------------------------------------------------------------------------------------------------------------------
!Calcul pour la branche mince
!-----------------------------------------------------------------------------------------------------------------------------------
mince=.true.

!Bornes pour la dichotomie
Sa_AD=Sg_AD
Sb_AD=Sd_AD

DO j=1,NX

   !--------------------------------------------------------------------------------------------------
   !Calcul pour la première température
   !--------------------------------------------------------------------------------------------------

   Temp_mince_AD(1)=Temp_min_AD

   CALL dichotomie( Temp_mince_AD(1), Sa_AD, Sb_AD, j, mince, Sc_AD, ecriture)

   IF (ecriture .eqv. .true.) THEN
      WRITE(11,*) Temp_mince_AD(1)*Temp_0, Sc_AD * S_0 / x_ad(j) * 1E-1, X_AD(j)**2._xp * R_S, X_AD(j)                                !Si la dichotomie a trouvé un zéro écrit dans le fichier en cgs
   ELSE
      !WRITE(11,*) Temp_mince(1)*Temp_0, message, X_AD(j)
   ENDIF

   !---------------------------------------------------------------------------------------------------------------
   !Boucle pour les autres températures
   !--------------------------------------------------------------------------------------------------------------

   DO i=2,n_s

      Sa_AD = Sg_AD
      Sb_AD = Sd_AD

      Temp_mince_AD(i)=Temp_mince_AD(i-1) + ( Temp_max_AD - Temp_min_AD ) / n_s

      CALL dichotomie( Temp_mince_AD(i), Sa_AD, Sb_AD, j, mince, Sc_AD, ecriture)

      IF (ecriture .eqv. .true.) THEN
         WRITE(11,*) Temp_mince_AD(i) * Temp_0, Sc_AD * S_0 / x_ad(j)*1E-1, X_AD(j)**2._xp * R_S, X_AD(j)                     !Si la dichotomie a trouvé un zéro écrit dans le fichier en cgs
      ELSE
         !WRITE(11,*) Temp_mince(i)*Temp_0, message, X_AD(j)
      ENDIF
   ENDDO

ENDDO

CLOSE(11)

END SUBROUTINE 

END MODULE MODULE_S_CURVE
