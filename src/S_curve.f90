PROGRAM courbe_S
USE module_declarations
USE module_branche_epais
USE module_fonctions_utiles
IMPLICIT NONE

!Déclarations

REAL(KIND=xp), DIMENSION(nx)   :: T_epais
REAL(KIND=xp), DIMENSION(nx)   :: T_mince
REAL(KIND=xp), DIMENSION(nx)   :: S_epais
REAL(KIND=xp), DIMENSION(nx)   :: S_mince
REAL(KIND=xp)                    :: Sd
REAL(KIND=xp)                    :: Sg
INTEGER                          :: i,j


END PROGRAM courbe_S