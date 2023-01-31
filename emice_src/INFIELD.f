C********************************************************
C*                                                      *
C* SUBROUTINE zur Speichern der Spur am Ort des RX	*
C*                                                      *
C********************************************************

C*********************** RCS-KEYWORDS ******************************
C $Source: /pf/a/a270012/fdem_src/RCS/INITNC.f,v $
C $Author: a270012 $
C $Date: 2002/06/26 13:39:26 $
C $RCSfile: INITNC.f,v $
C $Revision: 5.0 $
C $State: Exp $
C*********************** RCS-KEYWORDS ******************************

      SUBROUTINE INFIELD
      USE dynfelder_mod
      USE netcdf
C     INCLUDE 'netcdf.inc'

      INTEGER EMIF_nc			! NETCDF input file ID
      INTEGER EMIF_ID(6)		! NETCDF input variable ID
      INTEGER X_ID,Y_ID,Z_ID,T_ID	! Coord.Var. ID
      INTEGER XdimMOD,YdimMOD,ZdimMOD	! Modelldimensionen
      INTEGER SXMOD, SYMOD, SZMOD       ! Start Modellindizes
      INTEGER EXMOD, EYMOD, EZMOD       ! Ende Modellindizes
      INTEGER SXIN, SYIN, SZIN       	! Start Eingabeindizes
      INTEGER EXIN, EYIN, EZIN       	! Ende Eingabeindizes
      REAL FDXIN(1:2),FDYIN(1:2)	! Phys. Eingabeausdehnung
      REAL FDZIN(1:2),FDTIN(1:2)
      REAL DeltaXIN,DeltaYIN,DeltaZIN,DeltaTIN
      INTEGER start(4),stride(4),count(4)

C******************************************************************
C     Option -I : Einlesen des gesamten Feldes fuer ersten Zeitschritt
C     Keine raeumlichen und zeitliche Rasterung

C     PRINT*,'SUBROUTINE INFIELD' 

**********************************************************************
C     print*, 'Define NETCDF EM-field input file (NCEMINF)'

      CALL chk_nf(NF90_OPEN(NCEMINF,NF90_NOWRITE,EMIF_nc))


**********************************************************************
C     print*, 'Input Variable IDs and Attributes'

      IF (TEMODE.NE.1) THEN   ! Erlaubte Feldkomponenten einlesen
           IINEM=1
      ELSE
           IINEM(2)=1
           IINEM(4)=1
           IINEM(6)=1
      ENDIF
C     print*,IINEM
C     print*,EM_NAME

C     Inquire EM field variable IDs
C       IF (IINEM(1).EQ.1) CALL chk_nf(NF90_INQ_VARID(
C    &		EMIF_nc,"EX",EMIF_ID(1)) )
C       IF (IINEM(1).EQ.1) CALL chk_nf(NF90_INQ_VARID(
C    &		EMIF_nc,"EY",EMIF_ID(2)) )
C       IF (IINEM(1).EQ.1) CALL chk_nf(NF90_INQ_VARID(
C    &		EMIF_nc,"EZ",EMIF_ID(3)) )
C       IF (IINEM(1).EQ.1) CALL chk_nf(NF90_INQ_VARID(
C    &		EMIF_nc,"HX",EMIF_ID(4)) )
C       IF (IINEM(1).EQ.1) CALL chk_nf(NF90_INQ_VARID(
C    &		EMIF_nc,"HY",EMIF_ID(5)) )
C       IF (IINEM(1).EQ.1) CALL chk_nf(NF90_INQ_VARID(
C    &		EMIF_nc,"HZ",EMIF_ID(6)) )

      EMIF_ID(1) = 0
      EMIF_ID(2) =  5
      EMIF_ID(3) =  0
      EMIF_ID(4) =  6
      EMIF_ID(5) =  0
      EMIF_ID(6) =  7
      print*,'ID:',EMIF_ID

C     Inquire coordinate attribute IDs
      CALL chk_nf(NF90_INQ_VARID(EMIF_nc,"X",X_ID) )
      CALL chk_nf(NF90_INQ_VARID(EMIF_nc,"Y",Y_ID) )
      CALL chk_nf(NF90_INQ_VARID(EMIF_nc,"Z",Z_ID) )
      CALL chk_nf(NF90_INQ_VARID(EMIF_nc,"Time",T_ID) )

C     Read att. values of input variables
      CALL chk_nf(NF90_GET_ATT(EMIF_nc,X_ID,"delta_x",DeltaXIN ))
      CALL chk_nf(NF90_GET_ATT(EMIF_nc,Y_ID,"delta_y",DeltaYIN ))
      CALL chk_nf(NF90_GET_ATT(EMIF_nc,Z_ID,"delta_z",DeltaZIN ))
      CALL chk_nf(NF90_GET_ATT(EMIF_nc,T_ID,"delta_t",DeltaTIN ))
C     Valid Range: jeweils Min und Max als Feld
      CALL chk_nf(NF90_GET_ATT(EMIF_nc,X_ID,"valid_range",FDXIN))
      CALL chk_nf(NF90_GET_ATT(EMIF_nc,Y_ID,"valid_range",FDYIN))
      CALL chk_nf(NF90_GET_ATT(EMIF_nc,Z_ID,"valid_range",FDZIN))
      CALL chk_nf(NF90_GET_ATT(EMIF_nc,T_ID,"valid_range",FDTIN))
C     PRINT*,'X:',DeltaXIN,FDXIN
C     PRINT*,'Y:',DeltaYIN,FDYIN
C     PRINT*,'Z:',DeltaZIN,FDZIN
C     PRINT*,'T:',DeltaTIN,FDTIN

      IF (DeltaTIN.NE.DeltaT) CALL HANDLE_ERR(-2)

C**********************************************************************
C     Einlese-Dimension und Index-Intervalle der Modellvariablen

C     Neuer Startzeitpunkt fuer FD-Berechnung:
C     beginnt einen Schritt nach Zeitende (Max) der Eingabedatei
      IF (FDTIN(2).NE.FDTMIN) CALL HANDLE_ERR(-3)
      STMOD=INT(FDTIN(2)/FDDT)+1	
      TDIM=ETMOD-STMOD+1
      IF (STMOD.GT.ETMOD) CALL HANDLE_ERR(8)

C     Auch Rand wird belegt:

      SXMOD=0
      SYMOD=0		!YDIM/2
      SZMOD=0
      EXMOD=XDIM
      EYMOD=YDIM	!/2
      EZMOD=(FDZIN(2)-FDZmin)/Delta	! Notwendig, da ZDimMOD > ZDimIN
C     print*,'MOD',SXMOD,SYMOD,SZMOD,EXMOD,EYMOD,EZMOD

      XdimMOD=EXMOD-SXMOD+1
      YdimMOD=EYMOD-SYMOD+1
      ZdimMOD=EZMOD-SZMOD+1
C     print*,'Dim:',XDimMOD,YDimMOD,ZDimMOD,'1'

C     Rand wird auf 0 belassen, nur eigentlichen Modellraum belegen:
C     SXMOD=ISX
C     SYMOD=YDIM/2
C     SZMOD=ISIGMA
C     EXMOD=XDIM-ISX
C     EYMOD=SYMOD
C     EZMOD=(FDZIN(2)-FDZmin)/Delta-ISIGMA	! Notwendig, da ZDimMOD > ZDimIN
C     XdimMOD=XDIM-2*ISX
C     YdimMOD=YDIM-2*ISY
C     ZdimMOD=ZDIM-2*ISIGMA

C**********************************************************************
C     print*, 'Index-Intervalle der Eingabevariablen'
C     (im diskreten System der Eingabedatei: Endung IN !)

      SXIN=(SXMOD*Delta+FDXmin-FDXIN(1))/DeltaXIN+1	! +1 notwendig, da
      SYIN=(SYMOD*Delta+FDYmin-FDYIN(1))/DeltaYIN+1	! netCDF mit C-Konvention
      SZIN=(SZMOD*Delta+FDZmin-FDZIN(1))/DeltaZIN+1	! zaehlt, d.h. Feld-
      EXIN=(EXMOD*Delta+FDXmin-FDXIN(1))/DeltaXIN+1	! intervall beginnt mit
      EYIN=(EYMOD*Delta+FDYmin-FDYIN(1))/DeltaYIN+1	! 1 und endet mit n+1
      EZIN=(EZMOD*Delta+FDZmin-FDZIN(1))/DeltaZIN+1	! bei n Elementen.
C     print*,' IN:',SXIN,SYIN,SZIN,EXIN,EYIN,EZIN

C**********************************************************************
C     print*,'Input of all record variables'

      start  = 1
      stride = 1
      count  = (/ XDimMOD,YDimMOD,ZDimMOD, 1 /)

C     'Read EX component'
      IF (IINEM(1).EQ.1) THEN
C     print*,'Read EX component'
        CALL chk_nf(NF90_GET_VAR(EMIF_nc,EMIF_ID(1),
     &      EX(SXMOD:EXMOD:1,
     &	       SYMOD:EYMOD:1,
     & 	       SZMOD:EZMOD:1,0),
     &         start  = (/ SXIN,SYIN,SZIN,1 /),
     &         stride = (/ 1,1,1,1 /),
     &         count  = (/ XDimMOD,YDimMOD,ZDimMOD, 1 /) 
     &         ))
      ENDIF

C     'Read EY component'
      IF (IINEM(2).EQ.1) THEN
C     print*,'Read EY component'
        CALL chk_nf(NF90_GET_VAR(EMIF_nc,EMIF_ID(2),
     &      EY(SXMOD:EXMOD:1,
     &	       SYMOD:EYMOD:1,
     & 	       SZMOD:EZMOD:1,0),
     &         start  = (/ SXIN,SYIN,SZIN,1 /),
     &         stride  = (/ 1,1,1,1 /),
     &         count = (/ XDimMOD,YDimMOD,ZDimMOD, 1 /) 
     &         ))
      ENDIF

C     'Read EZ component'
      IF (IINEM(3).EQ.1) THEN
C     print*,'Read EZ component'
        CALL chk_nf(NF90_GET_VAR(EMIF_nc,EMIF_ID(3),
     &      EZ(SXMOD:EXMOD:1,
     &	       SYMOD:EYMOD:1,
     & 	       SZMOD:EZMOD:1,0),
     &         start  = (/ SXIN,SYIN,SZIN,1 /),
     &         stride  = (/ 1,1,1,1 /),
     &         count = (/ XDimMOD,YDimMOD,ZDimMOD, 1 /) 
     &         ))
      ENDIF

C     'Read HX component'
      IF (IINEM(4).EQ.1) THEN
C     print*,'Read HX component'
        CALL chk_nf(NF90_GET_VAR(EMIF_nc,EMIF_ID(4),
     &      HX(SXMOD:EXMOD:1,
     &	       SYMOD:EYMOD:1,
     & 	       SZMOD:EZMOD:1,0),
     &         start  = (/ SXIN,SYIN,SZIN,1 /),
     &         stride  = (/ 1,1,1,1 /),
     &         count = (/ XDimMOD,YDimMOD,ZDimMOD, 1 /) 
     &         ))
      ENDIF

C     'Read HY component'
      IF (IINEM(5).EQ.1) THEN
C     print*,'Read HY component'
        CALL chk_nf(NF90_GET_VAR(EMIF_nc,EMIF_ID(5),
     &      HY(SXMOD:EXMOD:1,
     &	       SYMOD:EYMOD:1,
     & 	       SZMOD:EZMOD:1,0),
     &         start  = (/ SXIN,SYIN,SZIN,1 /),
     &         stride  = (/ 1,1,1,1 /),
     &         count = (/ XDimMOD,YDimMOD,ZDimMOD, 1 /) 
     &         ))
      ENDIF

C     'Read HZ component'
      IF (IINEM(6).EQ.1) THEN
C     print*,'Read HZ component'
        CALL chk_nf(NF90_GET_VAR(EMIF_nc,EMIF_ID(6),
     &      HZ(SXMOD:EXMOD:1,
     &	       SYMOD:EYMOD:1,
     & 	       SZMOD:EZMOD:1,0),
     &         start  = (/ SXIN,SYIN,SZIN,1 /),
     &         stride  = (/ 1,1,1,1 /),
     &         count = (/ XDimMOD,YDimMOD,ZDimMOD, 1 /) 
     &         ))
      ENDIF

C**********************************************************************
C     Ausgabe der in der FD-Berechnung wegen neuem SXMOD nicht mehr 
C     beruecksichtigten Zeitschritte in Zeitschleife:
C     print*,'CALL OUTTRACE'
C     DO l=1,STMOD-1
C       CALL OUTTRACE(l) ! Speichern der E/H-Werte (Format: IOUTFORM) 
C     END DO

C     PRINT*,'DONE WITH SUBROUTINE INFIELD' 
      END !	SUBROUTINE INFIELD
