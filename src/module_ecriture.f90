!--------------------------------------------------------------------------------------------------
                                  MODULE MODULE_ECRITURE
!--------------------------------------------------------------------------------------------------
! Ce module écrit dans des fichiers .dat les tableaux de variables qu'on veut pouvoir étudier
! Mettre en argument le nombres de valeurs qu'on veut à chaque pas de temps (finissant par 01) et le temps t
     ! >>>> pour choisir les variables à écrire :
     
          ! >>> /config/wanted_variables 
             ! >>>> mettre 1 pour écrire une variables et 0 pour l'inverse
             
     USE MODULE_DECLARATIONS
                               
     IMPLICIT NONE
             
                 
CONTAINS

!---------------------------------------------------
! PARTIE DU MODULE POUR LES VARIABLES ADIMENSIONNÉES
!---------------------------------------------------

SUBROUTINE ECRITURE_AD_1(NB_VALUES)

     IMPLICIT NONE
     
     ! cette routine écrit les variables en lignes : chaque pas de temps correspond à N
     ! (variables) lignes avec la variations spatiale de chaque variables sur une ligne
     
     INTEGER,INTENT(IN) :: NB_VALUES
     INTEGER :: PAS,I
     INTEGER,PARAMETER :: NB_VARIABLES = 20
     INTEGER :: ID_WANTED_VARIABLES 
     INTEGER :: LIST(NB_VARIABLES)
     REAL(KIND=XP),INTENT(IN) :: T_CUR
     CHARACTER(LEN=1024),PARAMETER :: FILE_NAME = 'data_ad.dat'  
     CHARACTER(LEN=3) :: VALUES_LINE
     
     
     NAMELIST /WANTED_VARIABLES/ LIST
     OPEN(NEWUNIT=ID_WANTED_VARIABLES,FILE='config/wanted_variables.txt',ACTION='read',STATUS='old')
     READ(ID_WANTED_VARIABLES,WANTED_VARIABLES)
     CLOSE(ID_WANTED_VARIABLES)
     
     IF (NX==NB_VALUES) THEN
          PAS = NX/NB_VALUES
     ELSE 
          PAS = NX/NB_VALUES +1
     ENDIF
     
     2 FORMAT(I3)
     WRITE(VALUES_LINE,2) NB_VALUES
     
     OPEN(11,FILE=FILE_NAME,STATUS='UNKNOWN',POSITION='APPEND')
     
     WRITE(11,"('# T = ',1PE12.3)") T_CUR
     
     WRITE(11,"('X      ',"//VALUES_LINE//"(1pE11.4,2X))") X_AD(::PAS)
     
     IF (LIST(1)==1) THEN
     WRITE(11,"('OMEGA  ',"//VALUES_LINE//"(1pE11.4,2X))") OMEGA_AD(::PAS)
     ENDIF
     
     IF (LIST(2)==1) THEN
     WRITE(11,"('P      ',"//VALUES_LINE//"(1pE11.4,2X))") P_AD(::PAS)
     ENDIF
     
     IF (LIST(3)==1) THEN
     WRITE(11,"('BETA   ',"//VALUES_LINE//"(1pE11.4,2X))") BETA(::PAS)
     ENDIF  
     
     IF (LIST(4)==1) THEN
     WRITE(11,"('C_S    ',"//VALUES_LINE//"(1pE11.4,2X))") C_S_AD(::PAS)
     ENDIF
     
     IF (LIST(5)==1) THEN
     WRITE(11,"('H      ',"//VALUES_LINE//"(1pE11.4,2X))") H_AD(::PAS)
     ENDIF
     
     IF (LIST(6)==1) THEN
     WRITE(11,"('RHO    ',"//VALUES_LINE//"(1pE11.4,2X))") RHO_AD(::PAS)
     ENDIF
     
     IF (LIST(7)==1) THEN
     WRITE(11,"('NU     ',"//VALUES_LINE//"(1pE11.4,2X))") NU_AD(::PAS)
     ENDIF
     
     IF (LIST(8)==1) THEN
     WRITE(11,"('S      ',"//VALUES_LINE//"(1pE11.4,2X))") S_AD(::PAS)
     ENDIF
     
     IF (LIST(9)==1) THEN
     WRITE(11,"('V      ',"//VALUES_LINE//"(1pE11.4,2X))") V_AD(::PAS)
     ENDIF
     
     IF (LIST(10)==1) THEN
     WRITE(11,"('TEMP   ',"//VALUES_LINE//"(1pE11.4,2X))") TEMP_AD(::PAS)
     ENDIF
     
     IF (LIST(11)==1) THEN
     WRITE(11,"('M_DOT  ',"//VALUES_LINE//"(1pE11.4,2X))") M_DOT_AD(::PAS)
     ENDIF
     
     IF (LIST(12)==1) THEN
     WRITE(11,"('F_Z    ',"//VALUES_LINE//"(1pE11.4,2X))") F_Z(::PAS)
     ENDIF
     
     IF(LIST(13)==1) THEN
     WRITE(11,"('P_GAZ  ',"//VALUES_LINE//"(1pE11.4,2X))") P_GAZ_AD(::PAS)
     ENDIF
     
     IF(LIST(14)==1) THEN
     WRITE(11,"('P_RAD  ',"//VALUES_LINE//"(1pE11.4,2X))") P_RAD_AD(::PAS)
     ENDIF
     
     IF(LIST(15)==1) THEN
     WRITE(11,"('Q_PLUS ',"//VALUES_LINE//"(1pE11.4,2X))") Q_PLUS_AD(::PAS)
     ENDIF
     
     IF(LIST(16)==1) THEN
     WRITE(11,"('Q_ADV  ',"//VALUES_LINE//"(1pE11.4,2X))") Q_ADV_AD(::PAS)
     ENDIF
     
     IF(LIST(17)==1) THEN
     WRITE(11,"('Q_MOINS ',"//VALUES_LINE//"(1pE11.4,2X))") Q_MOINS(::PAS)
     ENDIF
     
     IF(LIST(18)==1) THEN
     WRITE(11,"('TAU_EFF ',"//VALUES_LINE//"(1pE11.4,2X))") TAU_EFF(::PAS)
     ENDIF
     
     IF(LIST(19)==1) THEN
     WRITE(11,"('K_FF   ',"//VALUES_LINE//"(1pE11.4,2X))") KAPPA_FF(::PAS)
     ENDIF
     
     IF(LIST(20)==1) THEN
     WRITE(11,"('E_FF   ',"//VALUES_LINE//"(1pE11.4,2X))") EPSILON_FF(::PAS)
     ENDIF
     
     CLOSE(11)
     
END SUBROUTINE ECRITURE_AD_1

!------------------------------------------------------------------------------

SUBROUTINE ECRITURE_AD_2(NB_VALUES,T_CUR)

     IMPLICIT NONE
     
     ! cette routine écrit les variables en colonne
     ! variations spatiale d'une variable en colonne
     
     INTEGER,INTENT(IN) :: NB_VALUES
     REAL(KIND=XP),INTENT(IN) :: T_CUR
     INTEGER :: PAS,I
     INTEGER,PARAMETER :: NB_VARIABLES = 20
     INTEGER :: ID_WANTED_VARIABLES 
     INTEGER :: LIST(NB_VARIABLES)
     CHARACTER(LEN=1024),PARAMETER :: FILE_NAME = 'data_ad_1.dat' 
     CHARACTER(LEN=3) :: VALUES_LINE
     
     
     NAMELIST /WANTED_VARIABLES/ LIST
     OPEN(NEWUNIT=ID_WANTED_VARIABLES,FILE='config/wanted_variables.txt',ACTION='read',STATUS='old')
     READ(ID_WANTED_VARIABLES,WANTED_VARIABLES)
     CLOSE(ID_WANTED_VARIABLES)
     
     IF (NX==NB_VALUES) THEN
          PAS = NX/NB_VALUES
     ELSE 
          PAS = NX/NB_VALUES +1
     ENDIF
     
     OPEN(11,FILE=FILE_NAME,STATUS='UNKNOWN',POSITION='APPEND')
     
     WRITE(11,"('# T = ',1PE12.3)") T_CUR
     
     WRITE(11,"('#     X      ')",ADVANCE="NO")
     
     IF (LIST(1)==1) THEN
     WRITE(11,"('    OMEGA    ')",ADVANCE="NO")
     ENDIF
     
     IF (LIST(2)==1) THEN
     WRITE(11,"('      P      ')",ADVANCE="NO")
     ENDIF
     
     IF (LIST(3)==1) THEN
     WRITE(11,"('     BETA    ')",ADVANCE="NO")
     ENDIF
     
     IF (LIST(4)==1) THEN
     WRITE(11,"('     C_S     ')",ADVANCE="NO")
     ENDIF
     
     IF (LIST(5)==1) THEN
     WRITE(11,"('      H      ')",ADVANCE="NO")
     ENDIF 
     
     IF (LIST(6)==1) THEN
     WRITE(11,"('     RHO     ')",ADVANCE="NO")
     ENDIF
     
     IF (LIST(7)==1) THEN
     WRITE(11,"('     NU      ')",ADVANCE="NO")
     ENDIF 
     
     IF (LIST(8)==1) THEN
     WRITE(11,"('    S_AD     ')",ADVANCE="NO")
     ENDIF
     
     IF (LIST(9)==1) THEN
     WRITE(11,"('      V      ')",ADVANCE="NO")
     ENDIF
     
     IF (LIST(10)==1) THEN
     WRITE(11,"('      T      ')",ADVANCE="NO")
     ENDIF
     
     IF (LIST(11)==1) THEN
     WRITE(11,"('    M_DOT    ')",ADVANCE="NO")
     ENDIF
     
     IF (LIST(12)==1) THEN
     WRITE(11,"('     F_Z     ')",ADVANCE="NO")
     ENDIF
     
     IF (LIST(13)==1) THEN
     WRITE(11,"('    P_GAZ    ')",ADVANCE="NO")
     ENDIF
     
     IF (LIST(14)==1) THEN
     WRITE(11,"('    P_RAD    ')",ADVANCE="NO")
     ENDIF
     
     IF (LIST(15)==1) THEN
     WRITE(11,"('    Q_PLUS   ')",ADVANCE="NO")
     ENDIF
     
     IF (LIST(16)==1) THEN
     WRITE(11,"('    Q_ADV    ')",ADVANCE="NO")
     ENDIF
     
     IF (LIST(17)==1) THEN
     WRITE(11,"('   Q_MOINS   ')",ADVANCE="NO")
     ENDIF
     
     IF (LIST(18)==1) THEN
     WRITE(11,"('   TAU_EFF   ')",ADVANCE="NO")
     ENDIF
     
     IF (LIST(19)==1) THEN
     WRITE(11,"('   KAPPA_FF  ')",ADVANCE="NO")
     ENDIF
     
     IF (LIST(20)==1) THEN
     WRITE(11,"('  EPSILON_FF ')",ADVANCE="NO")
     ENDIF
     
     WRITE(11,*)
     WRITE(11,*)
     
     DO I=1,NX,PAS
          
          WRITE(11,"(1pE11.4,2X)",ADVANCE="NO") X_AD(I)
          
          IF (LIST(1)==1) THEN
          WRITE(11,"(1pE11.4,2X)",ADVANCE="NO") OMEGA_AD(I)
          ENDIF
          
          IF (LIST(2)==1) THEN
          WRITE(11,"(1pE11.4,2X)",ADVANCE="NO") P_AD(I)
          ENDIF
          
          IF (LIST(3)==1) THEN
          WRITE(11,"(1pE11.4,2X)",ADVANCE="NO") BETA(I)
          ENDIF
          
          IF (LIST(4)==1) THEN
          WRITE(11,"(1pE11.4,2X)",ADVANCE="NO") C_S_AD(I)
          ENDIF
          
          IF (LIST(5)==1) THEN
          WRITE(11,"(1pE11.4,2X)",ADVANCE="NO") H_AD(I)
          ENDIF
          
          IF (LIST(6)==1) THEN
          WRITE(11,"(1pE11.4,2X)",ADVANCE="NO") RHO_AD(I)
          ENDIF
          
          IF (LIST(7)==1) THEN
          WRITE(11,"(1pE11.4,2X)",ADVANCE="NO") NU_AD(I)
          ENDIF
          
          IF (LIST(8)==1) THEN
          WRITE(11,"(1pE11.4,2X)",ADVANCE="NO") S_AD(I)
          ENDIF
          
          IF (LIST(9)==1) THEN
          WRITE(11,"(1pE11.4,2X)",ADVANCE="NO") V_AD(I)
          ENDIF
          
          IF (LIST(10)==1) THEN
          WRITE(11,"(1pE11.4,2X)",ADVANCE="NO") TEMP_AD(I)
          ENDIF
          
          IF (LIST(11)==1) THEN
          WRITE(11,"(1pE11.4,2X)",ADVANCE="NO") M_DOT_AD(I)
          ENDIF
          
          IF (LIST(12)==1) THEN
          WRITE(11,"(1pE11.4,2X)",ADVANCE="NO") F_Z(I)
          ENDIF
          
          IF(LIST(13)==1) THEN
          WRITE(11,"(1pE11.4,2X)",ADVANCE="NO") P_GAZ_AD(I)
          ENDIF
          
          IF(LIST(14)==1) THEN
          WRITE(11,"(1pE11.4,2X)",ADVANCE="NO") P_RAD_AD(I)
          ENDIF
          
          IF(LIST(15)==1) THEN
          WRITE(11,"(1pE11.4,2X)",ADVANCE="NO") Q_PLUS_AD(I)
          ENDIF
          
          IF(LIST(16)==1) THEN
          WRITE(11,"(1pE11.4,2X)",ADVANCE="NO") Q_ADV_AD(I)
          ENDIF
          
          IF(LIST(17)==1) THEN
          WRITE(11,"(1pE11.4,2X)",ADVANCE="NO") Q_MOINS(I)
          ENDIF
          
          IF(LIST(18)==1) THEN
          WRITE(11,"(1pE11.4,2X)",ADVANCE="NO") TAU_EFF(I)
          ENDIF
          
          IF(LIST(19)==1) THEN
          WRITE(11,"(1pE11.4,2X)",ADVANCE="NO") KAPPA_FF(I)
          ENDIF
          
          IF(LIST(20)==1) THEN
          WRITE(11,"(1pE11.4,2X)",ADVANCE="NO") EPSILON_FF(I)
          ENDIF
          
          WRITE(11,*)
          
     ENDDO
     WRITE(11,*)
     CLOSE(11)
          
END SUBROUTINE ECRITURE_AD_2

!--------------------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------------------

!--------------------------------------------------
! PARTIE DU MODULE POUR LES VARIABLES DIMENSIONNÉES
!--------------------------------------------------
SUBROUTINE ECRITURE_1(NB_VALUES,T_CUR)

     IMPLICIT NONE
     
     ! cette routine écrit les variables en lignes : chaque pas de temps correspond à N
     ! (variables) lignes avec la variations spatiale de chaque variables sur une ligne
     
     INTEGER,INTENT(IN) :: NB_VALUES
     INTEGER :: PAS,I
     REAL(KIND=XP),INTENT(IN) :: T_CUR
     INTEGER,PARAMETER :: NB_VARIABLES = 20
     INTEGER :: ID_WANTED_VARIABLES 
     INTEGER :: LIST(NB_VARIABLES)
     CHARACTER(LEN=1024),PARAMETER :: FILE_NAME = 'data.dat'  
     CHARACTER(LEN=3) :: VALUES_LINE
     
     
     NAMELIST /WANTED_VARIABLES/ LIST
     OPEN(NEWUNIT=ID_WANTED_VARIABLES,FILE='config/wanted_variables.txt',ACTION='read',STATUS='old')
     READ(ID_WANTED_VARIABLES,WANTED_VARIABLES)
     CLOSE(ID_WANTED_VARIABLES)
     
     IF (NX==NB_VALUES) THEN
          PAS = NX/NB_VALUES
     ELSE 
          PAS = NX/NB_VALUES +1
     ENDIF
     
     2 FORMAT(I3)
     WRITE(VALUES_LINE,2) NB_VALUES
     
     OPEN(11,FILE=FILE_NAME,STATUS='UNKNOWN',POSITION='APPEND')
     
     WRITE(11,"('# T = ',1PE12.3)") T_CUR
     
     WRITE(11,"('RAYON  ',"//VALUES_LINE//"(1pE11.4,2X))") RAYON(::PAS)
     
     IF (LIST(1)==1) THEN
     WRITE(11,"('OMEGA  ',"//VALUES_LINE//"(1pE11.4,2X))") OMEGA(::PAS)
     ENDIF
     
     IF (LIST(2)==1) THEN
     WRITE(11,"('P      ',"//VALUES_LINE//"(1pE11.4,2X))") P(::PAS)
     ENDIF
     
     IF (LIST(3)==1) THEN
     WRITE(11,"('BETA   ',"//VALUES_LINE//"(1pE11.4,2X))") BETA(::PAS)
     ENDIF
     
     IF (LIST(4)==1) THEN
     WRITE(11,"('C_S    ',"//VALUES_LINE//"(1pE11.4,2X))") C_S(::PAS)
     ENDIF
     
     IF (LIST(5)==1) THEN
     WRITE(11,"('H      ',"//VALUES_LINE//"(1pE11.4,2X))") H(::PAS)
     ENDIF
     
     IF (LIST(6)==1) THEN
     WRITE(11,"('RHO    ',"//VALUES_LINE//"(1pE11.4,2X))") RHO(::PAS)
     ENDIF
     
     IF (LIST(7)==1) THEN
     WRITE(11,"('NU     ',"//VALUES_LINE//"(1pE11.4,2X))") NU(::PAS)
     ENDIF
     
     IF (LIST(8)==1) THEN
     WRITE(11,"('SIGMA  ',"//VALUES_LINE//"(1pE11.4,2X))") SIGMA(::PAS)
     ENDIF
     
     IF (LIST(9)==1) THEN
     WRITE(11,"('V      ',"//VALUES_LINE//"(1pE11.4,2X))") V(::PAS)
     ENDIF
     
     IF (LIST(10)==1) THEN
     WRITE(11,"('TEMP   ',"//VALUES_LINE//"(1pE11.4,2X))") TEMP(::PAS)
     ENDIF
     
     IF (LIST(11)==1) THEN
     WRITE(11,"('M_DOT  ',"//VALUES_LINE//"(1pE11.4,2X))") M_DOT(::PAS)
     ENDIF
     
     IF (LIST(12)==1) THEN
     WRITE(11,"('F_Z    ',"//VALUES_LINE//"(1pE11.4,2X))") F_Z(::PAS)
     ENDIF
     
     IF(LIST(13)==1) THEN
     WRITE(11,"('P_GAZ  ',"//VALUES_LINE//"(1pE11.4,2X))") P_GAZ(::PAS)
     ENDIF
     
     IF(LIST(14)==1) THEN
     WRITE(11,"('P_RAD  ',"//VALUES_LINE//"(1pE11.4,2X))") P_RAD(::PAS)
     ENDIF
     
     IF(LIST(15)==1) THEN
     WRITE(11,"('Q_PLUS ',"//VALUES_LINE//"(1pE11.4,2X))") Q_PLUS(::PAS)
     ENDIF
     
     IF(LIST(16)==1) THEN
     WRITE(11,"('Q_ADV  ',"//VALUES_LINE//"(1pE11.4,2X))") Q_ADV(::PAS)
     ENDIF
     
     IF(LIST(17)==1) THEN
     WRITE(11,"('Q_MOINS ',"//VALUES_LINE//"(1pE11.4,2X))") Q_MOINS(::PAS)
     ENDIF
     
     IF(LIST(18)==1) THEN
     WRITE(11,"('TAU_EFF ',"//VALUES_LINE//"(1pE11.4,2X))") TAU_EFF(::PAS)
     ENDIF
     
     IF(LIST(19)==1) THEN
     WRITE(11,"('K_FF   ',"//VALUES_LINE//"(1pE11.4,2X))") KAPPA_FF(::PAS)
     ENDIF
     
     IF(LIST(20)==1) THEN
     WRITE(11,"('E_FF   ',"//VALUES_LINE//"(1pE11.4,2X))") EPSILON_FF(::PAS)
     ENDIF
     
     CLOSE(11)
     
END SUBROUTINE ECRITURE_1

SUBROUTINE ECRITURE_2(NB_VALUES,T_CUR)

     IMPLICIT NONE
     
     ! cette routine écrit les variables en colonne
     ! variations spatiale d'une variable en colonne
     
     INTEGER,INTENT(IN) :: NB_VALUES
     REAL(KIND=XP),INTENT(IN) :: T_CUR
     INTEGER :: PAS,I
     INTEGER,PARAMETER :: NB_VARIABLES = 20
     INTEGER :: ID_WANTED_VARIABLES 
     INTEGER :: LIST(NB_VARIABLES)
     CHARACTER(LEN=1024),PARAMETER :: FILE_NAME = 'data1.dat' 
     CHARACTER(LEN=3) :: VALUES_LINE
     
     
     NAMELIST /WANTED_VARIABLES/ LIST
     OPEN(NEWUNIT=ID_WANTED_VARIABLES,FILE='config/wanted_variables.txt',ACTION='read',STATUS='old')
     READ(ID_WANTED_VARIABLES,WANTED_VARIABLES)
     CLOSE(ID_WANTED_VARIABLES)
     
     IF (NX==NB_VALUES) THEN
          PAS = NX/NB_VALUES
     ELSE 
          PAS = NX/NB_VALUES +1
     ENDIF
     
     OPEN(11,FILE=FILE_NAME,STATUS='UNKNOWN',POSITION='APPEND')
     
     WRITE(11,"('# T = ',1PE12.3)") T_CUR
     
     WRITE(11,"('#   RAYON    ')",ADVANCE="NO")
     
     IF (LIST(1)==1) THEN
     WRITE(11,"('    OMEGA    ')",ADVANCE="NO")
     ENDIF
     
     IF (LIST(2)==1) THEN
     WRITE(11,"('      P      ')",ADVANCE="NO")
     ENDIF

     IF (LIST(3)==1) THEN
     WRITE(11,"('     BETA    ')",ADVANCE="NO")
     ENDIF      
     
     IF (LIST(4)==1) THEN
     WRITE(11,"('     C_S     ')",ADVANCE="NO")
     ENDIF 
     
     IF (LIST(5)==1) THEN
     WRITE(11,"('      H      ')",ADVANCE="NO")
     ENDIF 
     
     IF (LIST(6)==1) THEN
     WRITE(11,"('     RHO     ')",ADVANCE="NO")
     ENDIF 
     
     IF (LIST(7)==1) THEN
     WRITE(11,"('     NU      ')",ADVANCE="NO")
     ENDIF 
     
     IF (LIST(8)==1) THEN
     WRITE(11,"('    SIGMA    ')",ADVANCE="NO")
     ENDIF 
     
     IF (LIST(9)==1) THEN
     WRITE(11,"('      V      ')",ADVANCE="NO")
     ENDIF 
     
     IF (LIST(10)==1) THEN
     WRITE(11,"('      T      ')",ADVANCE="NO")
     ENDIF
     
     IF (LIST(11)==1) THEN
     WRITE(11,"('    M_DOT    ')",ADVANCE="NO")
     ENDIF
     
     IF (LIST(12)==1) THEN
     WRITE(11,"('     F_Z     ')",ADVANCE="NO")
     ENDIF
     
     IF (LIST(13)==1) THEN
     WRITE(11,"('    P_GAZ    ')",ADVANCE="NO")
     ENDIF
     
     IF (LIST(14)==1) THEN
     WRITE(11,"('    P_RAD    ')",ADVANCE="NO")
     ENDIF
     
     IF (LIST(15)==1) THEN
     WRITE(11,"('    Q_PLUS   ')",ADVANCE="NO")
     ENDIF
     
     IF (LIST(16)==1) THEN
     WRITE(11,"('    Q_ADV    ')",ADVANCE="NO")
     ENDIF
     
     IF (LIST(17)==1) THEN
     WRITE(11,"('   Q_MOINS   ')",ADVANCE="NO")
     ENDIF
     
     IF (LIST(18)==1) THEN
     WRITE(11,"('   TAU_EFF   ')",ADVANCE="NO")
     ENDIF
     
     IF (LIST(19)==1) THEN
     WRITE(11,"('   KAPPA_FF  ')",ADVANCE="NO")
     ENDIF
     
     IF (LIST(20)==1) THEN
     WRITE(11,"('  EPSILON_FF ')",ADVANCE="NO")
     ENDIF
     
     WRITE(11,*)
     WRITE(11,*)
     
     DO I=1,NX,PAS
          
          WRITE(11,"(1pE11.4,2X)",ADVANCE="NO") RAYON(I)
          
          IF (LIST(1)==1) THEN
          WRITE(11,"(1pE11.4,2X)",ADVANCE="NO") OMEGA(I)
          ENDIF
          
          IF (LIST(2)==1) THEN
          WRITE(11,"(1pE11.4,2X)",ADVANCE="NO") P(I)
          ENDIF
          
          IF (LIST(3)==1) THEN
          WRITE(11,"(1pE11.4,2X)",ADVANCE="NO") BETA(I)
          ENDIF
          
          IF (LIST(4)==1) THEN
          WRITE(11,"(1pE11.4,2X)",ADVANCE="NO") C_S(I)
          ENDIF
          
          IF (LIST(5)==1) THEN
          WRITE(11,"(1pE11.4,2X)",ADVANCE="NO") H(I)
          ENDIF
          
          IF (LIST(6)==1) THEN
          WRITE(11,"(1pE11.4,2X)",ADVANCE="NO") RHO(I)
          ENDIF
          
          IF (LIST(7)==1) THEN
          WRITE(11,"(1pE11.4,2X)",ADVANCE="NO") NU(I)
          ENDIF
          
          IF (LIST(8)==1) THEN
          WRITE(11,"(1pE11.4,2X)",ADVANCE="NO") SIGMA(I)
          ENDIF
          
          IF (LIST(9)==1) THEN
          WRITE(11,"(1pE11.4,2X)",ADVANCE="NO") V(I)
          ENDIF
          
          IF (LIST(10)==1) THEN
          WRITE(11,"(1pE11.4,2X)",ADVANCE="NO") TEMP(I)
          ENDIF
          
          IF (LIST(11)==1) THEN
          WRITE(11,"(1pE11.4,2X)",ADVANCE="NO") M_DOT(I)
          ENDIF
          
          IF (LIST(12)==1) THEN
          WRITE(11,"(1pE11.4,2X)",ADVANCE="NO") F_Z(I)
          ENDIF
          
          IF(LIST(13)==1) THEN
          WRITE(11,"(1pE11.4,2X)",ADVANCE="NO") P_GAZ(I)
          ENDIF
          
          IF(LIST(14)==1) THEN
          WRITE(11,"(1pE11.4,2X)",ADVANCE="NO") P_RAD(I)
          ENDIF
          
          IF(LIST(15)==1) THEN
          WRITE(11,"(1pE11.4,2X)",ADVANCE="NO") Q_PLUS(I)
          ENDIF
          
          IF(LIST(16)==1) THEN
          WRITE(11,"(1pE11.4,2X)",ADVANCE="NO") Q_ADV(I)
          ENDIF
          
          IF(LIST(17)==1) THEN
          WRITE(11,"(1pE11.4,2X)",ADVANCE="NO") Q_MOINS(I)
          ENDIF
          
          IF(LIST(18)==1) THEN
          WRITE(11,"(1pE11.4,2X)",ADVANCE="NO") TAU_EFF(I)
          ENDIF
          
          IF(LIST(19)==1) THEN
          WRITE(11,"(1pE11.4,2X)",ADVANCE="NO") KAPPA_FF(I)
          ENDIF
          
          IF(LIST(20)==1) THEN
          WRITE(11,"(1pE11.4,2X)",ADVANCE="NO") EPSILON_FF(I)
          ENDIF
          
          WRITE(11,*)
          
     ENDDO
     WRITE(11,*)
     CLOSE(11)
          
END SUBROUTINE ECRITURE_2

!--------------------------------------------------------------------------------------------------
                                  END MODULE MODULE_ECRITURE
!--------------------------------------------------------------------------------------------------
