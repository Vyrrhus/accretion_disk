                                     MODULE MODULE_ECRITURE
     USE MODULE_DECLARATIONS
                               
     IMPLICIT NONE
                     
CONTAINS

SUBROUTINE ECRITURE_AD(NB_VALUES)

     IMPLICIT NONE
     
     INTEGER,INTENT(IN) :: NB_VALUES
     INTEGER :: PAS,I
     INTEGER,PARAMETER :: NB_VARIABLES = 14
     INTEGER :: ID_WANTED_VARIABLES 
     INTEGER :: LIST(NB_VARIABLES)
     CHARACTER(LEN=1024),PARAMETER :: FILE_NAME = 'data.txt'  
     CHARACTER(LEN=3) :: VALUES_LINE
     
     
     NAMELIST /WANTED_VARIABLES/ LIST
     OPEN(NEWUNIT=ID_WANTED_VARIABLES,FILE='config/wanted_variables.txt',ACTION='read',STATUS='old')
     READ(ID_WANTED_VARIABLES,WANTED_VARIABLES)
     CLOSE(ID_WANTED_VARIABLES)
     
     
     PAS = NX/NB_VALUES+1
     2 FORMAT(I3)
     WRITE(VALUES_LINE,2) NB_VALUES
     
     WRITE(*,"('Écriture des données de sortie dans le fichier data.txt')")
     OPEN(11,FILE=FILE_NAME,STATUS='UNKNOWN')!,POSITION='APPEND')
     
     WRITE(11,"("//VALUES_LINE//"(1pE10.4,2X))") X_AD(::PAS)
     
     IF (LIST(1)==1) THEN
     WRITE(11,"("//VALUES_LINE//"(1pE10.4,2X))") OMEGA_AD(::PAS)
     ENDIF
     
     IF (LIST(2)==1) THEN
     WRITE(11,"("//VALUES_LINE//"(1pE10.4,2X))") P_AD(::PAS)
     ENDIF
     
     IF (LIST(3)==1) THEN
     WRITE(11,"("//VALUES_LINE//"(1pE10.4,2X))") BETA(::PAS)
     ENDIF
     
     IF (LIST(4)==1) THEN
     WRITE(11,"("//VALUES_LINE//"(1pE10.4,2X))") C_S_AD(::PAS)
     ENDIF
     
     IF (LIST(5)==1) THEN
     WRITE(11,"("//VALUES_LINE//"(1pE10.4,2X))") H_AD(::PAS)
     ENDIF
     
     IF (LIST(6)==1) THEN
     WRITE(11,"("//VALUES_LINE//"(1pE10.4,2X))") RHO_AD(::PAS)
     ENDIF
     
     IF (LIST(7)==1) THEN
     WRITE(11,"("//VALUES_LINE//"(1pE10.4,2X))") NU_AD(::PAS)
     ENDIF
     
     !IF (LIST(8)==1) THEN
     !WRITE(11,"("//VALUES_LINE//"(1pE10.4,2X))") SIGMA_AD(::PAS)
     !ENDIF
     
     IF (LIST(9)==1) THEN
     WRITE(11,"("//VALUES_LINE//"(1pE10.4,2X))") V_AD(::PAS)
     ENDIF
     
     IF (LIST(10)==1) THEN
     WRITE(11,"("//VALUES_LINE//"(1pE10.4,2X))") TEMP_AD(::PAS)
     ENDIF
     
     !IF (LIST(11)==1) THEN
     !WRITE(11,"("//VALUES_LINE//"(1pE10.4,2X))") M_DOT_AD(::PAS)
     !ENDIF
     
     IF (LIST(12)==1) THEN
     WRITE(11,"("//VALUES_LINE//"(1pE10.4,2X))") F_Z(::PAS)
     ENDIF
     
     IF(LIST(13)==1) THEN
     WRITE(11,"("//VALUES_LINE//"(1pE10.4,2X))") P_GAZ_AD(::PAS)
     ENDIF
     
     IF(LIST(14)==1) THEN
     WRITE(11,"("//VALUES_LINE//"(1pE10.4,2X))") P_RAD_AD(::PAS)
     ENDIF
     
      
END SUBROUTINE ECRITURE_AD


                                   END MODULE MODULE_ECRITURE                 
