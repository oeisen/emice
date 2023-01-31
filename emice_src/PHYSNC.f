C********************************************************
C*                                                      *
C* SUBROUTINE zum oeffnen der NetCDF-Datei und          *
C* 		Definition der dim und var           	*	
C*                                                      *
C********************************************************

C*********************** RCS-KEYWORDS ******************************
C $Source: /pf/a/a270012/fdem_src/RCS/PHYSNC.f,v $
C $Author: a270012 $
C $Date: 2002/06/26 13:39:26 $
C $RCSfile: PHYSNC.f,v $
C $Revision: 5.0 $
C $State: Exp $
C 	Epsilon und Sigma werden als 2D (x,z) Variablen 
C       behandelt, wegen der Symmetrie. 
C       Erweiterung auf 3D durch entfernen der Kommentare C3D
C       leicht moeglich.
C*********************** RCS-KEYWORDS ******************************

      SUBROUTINE PHYSNC
      USE dynfelder_mod
      USE NETCDF
C     INCLUDE 'netcdf.inc'

      INTEGER PH_nc			! File ID
      INTEGER XDimPH,YDimPH,ZDimPH
      INTEGER XDimID,YDimID,ZDimID	! Dimension ID
      INTEGER X_ID,Y_ID,Z_ID 		! Coord.Var. ID
      INTEGER EPS_ID,SIG_ID 		! Variable ID
      REAL XCOPH(:),YCOPH(:),ZCOPH(:),EPStmp(:,:)

      ALLOCATABLE :: XCOPH,YCOPH,ZCOPH,EPStmp

C     PRINT*,' PHYSICS NETCDF-File'

C     PRINT*, ' ALLOCATE COORDINATE AND PRIMARY VARIABLES'
      ALLOCATE(XCOPH(0:XDIM),YCOPH(0:YDIM),ZCOPH(0:ZDIM),
     &         EPStmp(0:XDIM,0:ZDIM))


C**********************************************************************
C     RECORD VARIABLES (E,H)_xyz : NAME and TITLE

C     NAME AND DESCRIPTION
      EM_NAME(1)="EX"
      EM_NAME(2)="EY"
      EM_NAME(3)="EZ"
      EM_NAME(4)="HX"
      EM_NAME(5)="HY"
      EM_NAME(6)="HZ"
      EM_DESC(1)="X-component E-field"
      EM_DESC(2)="Y-component_E-field"
      EM_DESC(3)="Z-component_E-field"
      EM_DESC(4)="X-component_H-field"
      EM_DESC(5)="Y-component_H-field"
      EM_DESC(6)="Z-component_H-field"

C     PRINT*, ' ASSIGN VALUES TO COORDINATE AND PRIMARY VARIABLES'
      DO i=0,XDIM
        EPStmp(i,:)=epsilon
        XCOPH(i)=FDXMIN+(i-ISX)*DELTA
      END DO
C3D   DO j=0,YDIM
C3D     YCOPH(j)=FDYMIN+(j-ISY)*DELTA
C3D   END DO
      DO k=0,ZDIM
        ZCOPH(k)=FDZMIN+(k-ISIGMA)*DELTA
      END DO
      DO i=0,XDIM
        DO k=0,ZDIM
        END DO
      END DO
      
C     PRINT*, ' DIMENSIONEN DER AUSGABEFELDER'
      XDimPH = XDIM+1
C3D   YDimPH = YDIM+1
      ZDimPH = ZDIM+1

C     PRINT*, ' DEFINE NCPH-File'
      IF (NF90CLOB.EQ.1) THEN
        CALL chk_nf(NF90_CREATE(NCPH,NF90_CLOBBER,PH_nc))
      ELSE
        CALL chk_nf(NF90_CREATE(NCPH,NF90_NOCLOBBER,PH_nc))
      ENDIF

C     PRINT*, ' DEFINE DIMENSIONS'
      CALL chk_nf(NF90_DEF_Dim(PH_nc, "X", XDimPH, XDimID))
C3D   CALL chk_nf(NF90_DEF_Dim(PH_nc, "Y", YDimPH, YDimID))
      CALL chk_nf(NF90_DEF_Dim(PH_nc, "Z", ZDimPH, ZDimID))

C     PRINT*, ' DEFINE VARIABLES (COORD. AND VARIABLES)'
      CALL chk_nf(NF90_DEF_VAR(PH_nc,"X",nf90_real,(/ XDimID /),X_ID))
C3D   CALL chk_nf(NF90_DEF_VAR(PH_nc,"Y",nf90_real,(/ YDimID /),Y_ID))
      CALL chk_nf(NF90_DEF_VAR(PH_nc,"Z",nf90_real,(/ ZDimID /),Z_ID))
      CALL chk_nf(NF90_DEF_VAR(PH_nc,"epsilon",nf90_real, 
     &     (/ XDimID, ZDimID /), EPS_ID))
C3D  &     (/ XDimID, YDimID, ZDimID /), EPS_ID))
      CALL chk_nf(NF90_DEF_VAR(PH_nc,"sigma",nf90_real, 
     &     (/ XDimID, ZDimID /), SIG_ID))
C3D  &     (/ XDimID, YDimID, ZDimID /), SIG_ID))

C     PRINT*, '  DEFINE ATTRIBUTES'
      CALL chk_nf(NF90_PUT_ATT(PH_nc,X_ID,"units","m" ))
C3D   CALL chk_nf(NF90_PUT_ATT(PH_nc,Y_ID,"units","m" ))
      CALL chk_nf(NF90_PUT_ATT(PH_nc,Z_ID,"units","m" ))
      CALL chk_nf(NF90_PUT_ATT(PH_nc,EPS_ID,"title",
     &                             "ordinary relative permittivity"))
      CALL chk_nf(NF90_PUT_ATT(PH_nc,SIG_ID,"title","conductivity" ))
      CALL chk_nf(NF90_PUT_ATT(PH_nc,SIG_ID,"units","S m^-1"))
      CALL chk_nf(NF90_PUT_ATT(PH_nc,NF90_GLOBAL, "IABSOR", IABSOR ))
      CALL chk_nf(NF90_PUT_ATT(PH_nc,NF90_GLOBAL, "history_start",
     &            fdate_beg ) )
      CALL chk_nf(NF90_PUT_ATT(PH_nc,NF90_GLOBAL, "history",
     &            comline(1:ilenc-1) ) )
      CALL chk_nf(NF90_PUT_ATT(PH_nc,NF90_GLOBAL, "des2fd",
     &            DES2FD ) )

C     PRINT*, '  LEAVE DEFINE MODE'
      CALL chk_nf(NF90_ENDDEF(PH_nc))

C     PRINT*, '  WRITE COORDINATE VARIABLE VALUES'
      CALL chk_nf(NF90_PUT_VAR(PH_nc,X_ID,XCOPH))
C3D   CALL chk_nf(NF90_PUT_VAR(PH_nc,Y_ID,YCOPH))
      CALL chk_nf(NF90_PUT_VAR(PH_nc,Z_ID,ZCOPH))

C     PRINT*, '  WRITE PHYSICAL VARIABLE VALUES'
      CALL chk_nf(NF90_PUT_VAR(PH_nc,EPS_ID,EPStmp))
      CALL chk_nf(NF90_PUT_VAR(PH_nc,SIG_ID,
     &   SIGMA(0:XDIM,YDIM/2,0:ZDIM)))

      DEALLOCATE(XCOPH,YCOPH,ZCOPH,EPStmp)

      CALL chk_nf(NF90_CLOSE(PH_nc))
 
C     PRINT*, ' DONE WITH PHYSNC'
      END !	SUBROUTINE PHYSNC
