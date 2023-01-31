C********************************************************
C*                                                      *
C* SUBROUTINE zum oeffnen der NetCDF-Datei und          *
C* 		Definition der dim und var           	*	
C*                                                      *
C********************************************************

C*********************** RCS-KEYWORDS ******************************
C $Source: /pf/a/a270012/fdem_src/RCS/OUTFIELD.f,v $
C $Author: a270012 $
C $Date: 2002/06/26 13:39:26 $
C $RCSfile: OUTFIELD.f,v $
C $Revision: 5.0 $
C $State: Exp $
C*********************** RCS-KEYWORDS ******************************

      SUBROUTINE OUTFIELD
      USE dynfelder_mod
      USE NETCDF
C     INCLUDE 'netcdf.inc'

      INTEGER XDimOF,YDimOF,ZDimOF  		! Ausgabedimensionen
      INTEGER SXOF,SYOF,SZOF,STOF  		! Start der Ausgabeindizes
      INTEGER EXOF,EYOF,EZOF,ETOF   		! Ende der Ausgabeindizes
      INTEGER i,j,k,l,v
      INTEGER EMOF_nc				! NETCDF ID field output
      INTEGER EMOF_ID(6)			! NETCDF output variable ID
      INTEGER XDimID,YDimID,ZDimID,TDimID	! Dim-id
      INTEGER X_ID,Y_ID,Z_ID,T_ID               ! Coord.Var.-id
      REAL XCOEM(:),YCOEM(:),ZCOEM(:),TCOEM(:)

      ALLOCATABLE :: XCOEM,YCOEM,ZCOEM,TCOEM

C******************************************************************
C     Option -O : Ausgabe gesamtes Feld fuer letzten Zeitschritt
C     Keine raeumlichen und zeitliche Rasterung 

C     PRINT*,' OUTFIELD to NETCDF-File'

C**********************************************************************
C     Index-Intervalle der Ausgabevariablen und -koordinaten

      IF (TEMODE.NE.1) THEN   	! Erlaubte Feldkomponenten ausgeben
        IOUTEM=1  
      ELSE
        IOUTEM(2)=1
        IOUTEM(4)=1
        IOUTEM(6)=1
      ENDIF

C     Ausgabe Modellfeld incl. Rand (entspricht IOUTTYPE 0,2)
      SXOF = 0
      SYOF = 0 		!YDIM/2  ! ACHTUNG 3D
      SZOF = 0
      STOF = STMOD
      EXOF = XDIM
      EYOF = YDIM	!/2  ! ACHTUNG 3D
      EZOF = ZDIM
      ETOF = ETMOD

C     Dimensionen der Ausgabefelder
      XDIMOF=EXOF-SXOF+1	! +1 notwendig, da alle schleifen von
      YDIMOF=EYOF-SYOF+1	! 0 bis [XYZ]DIM zaehlen
      ZDIMOF=EZOF-SZOF+1	! 

C**********************************************************************
C     ALLOCATE COORDINATE VARIABLES AND ASSIGN VALUES

      ALLOCATE( 
     &     XCOEM(0:XDIM), 
     &     YCOEM(0:YDIM),
     &     ZCOEM(0:ZDIM),
     &     TCOEM(ETOF:ETOF))

      DO i=0,XDIM
        XCOEM(i)=FDXMIN+(i-ISX)*DELTA
      END DO
      DO j=0,YDIM
        YCOEM(j)=FDYMIN+(j-ISY)*DELTA
      END DO
      DO k=0,ZDIM
        ZCOEM(k)=FDZMIN+(k-ISIGMA)*DELTA
      END DO
      TCOEM(ETOF)=(ETOF-1)*DELTAT*1e9	! in ns

C**********************************************************************
C     DEFINE NETCDF EM-FIELD OUTPUT FILE NCEMOUTF

      CALL chk_nf(NF90_CREATE(NCEMOUTF,NF90_CLOBBER,EMOF_nc))

C**********************************************************************
C     COORDINATE VARIABLES AND GLOBAL ATTRIBUTES

C     PRINT*, 'DEFINE DIMENSIONS'
      CALL chk_nf(NF90_DEF_Dim(EMOF_nc, "X", XDimOF, XDimID))
      CALL chk_nf(NF90_DEF_Dim(EMOF_nc, "Y", YDimOF, YDimID))
      CALL chk_nf(NF90_DEF_Dim(EMOF_nc, "Z", ZDimOF, ZDimID))
      CALL chk_nf(NF90_DEF_Dim(EMOF_nc,"Time",nf90_unlimited,TDimId))

C     PRINT*, '  DEFINE COORDINATE VARIABLES'
      CALL chk_nf(NF90_DEF_VAR(EMOF_nc,"X",nf90_real,(/ XDimID /),X_ID))
      CALL chk_nf(NF90_DEF_VAR(EMOF_nc,"Y",nf90_real,(/ YDimID /),Y_ID))
      CALL chk_nf(NF90_DEF_VAR(EMOF_nc,"Z",nf90_real,(/ ZDimID /),Z_ID))
      CALL chk_nf(NF90_DEF_VAR(EMOF_nc,"Time",nf90_real,
     &     (/ TDimID /),T_ID ) )

C     PRINT*, '  DEFINE GLOBAL ATTRIBUTES'

C     X-ATTRIBUTES
      CALL chk_nf(NF90_PUT_ATT(EMOF_nc,X_ID,"units",DistanceDimension ))
      CALL chk_nf(NF90_PUT_ATT(EMOF_nc,X_ID,"valid_range",
     &                         (/ FDXMIN, FDXMAX /) ))
      CALL chk_nf(NF90_PUT_ATT(EMOF_nc,X_ID,"delta_x",DELTA ))

C     Y-ATTRIBUTES
      CALL chk_nf(NF90_PUT_ATT(EMOF_nc,Y_ID,"units",DistanceDimension ))
      CALL chk_nf(NF90_PUT_ATT(EMOF_nc,Y_ID,"valid_range",
     &                         (/ FDYMIN, FDYMAX /) ))
      CALL chk_nf(NF90_PUT_ATT(EMOF_nc,Y_ID,"delta_y",DELTA ))

C     Z-ATTRIBUTES
      CALL chk_nf(NF90_PUT_ATT(EMOF_nc,Z_ID,"units",DistanceDimension ))
      CALL chk_nf(NF90_PUT_ATT(EMOF_nc,Z_ID,"valid_range",
     &                         (/ FDZMIN, FDZMAX /) ))
      CALL chk_nf(NF90_PUT_ATT(EMOF_nc,Z_ID,"delta_z",DELTA ))

C     T-ATTRIBUTES
      CALL chk_nf(NF90_PUT_ATT(EMOF_nc,T_ID,"units",TimeDimension ))
      CALL chk_nf(NF90_PUT_ATT(EMOF_nc,T_ID,"valid_range",
     &	(/ FDTMAX, FDTMAX /) ))	! FELDINITIALISIERUNG: Nur letzter Zeitpunkt
      CALL chk_nf(NF90_PUT_ATT(EMOF_nc,T_ID,"delta_t",DELTAT ))

C    HISTORY ATTRIBUTES
      CALL chk_nf(NF90_PUT_ATT(EMOF_nc,NF90_GLOBAL, "history_start", 
     &            fdate_beg ) )
      CALL chk_nf(NF90_PUT_ATT(EMOF_nc,NF90_GLOBAL, "history", 
     &            comline(1:ilenc-1) ) )
      CALL chk_nf(NF90_PUT_ATT(EMOF_nc,NF90_GLOBAL, "des2fd", 
     &            DES2FD ) )

C     FD ATTRIBUTES
      CALL chk_nf(NF90_PUT_ATT(
     &   EMOF_nc,NF90_GLOBAL, "output-type", MeasureArt ))
      CALL chk_nf(NF90_PUT_ATT(EMOF_nc,NF90_GLOBAL,"ITRACES",ITRACES ))
      CALL chk_nf(NF90_PUT_ATT(EMOF_nc,NF90_GLOBAL,"ITIMES",ITIMES ))
      CALL chk_nf(NF90_PUT_ATT(EMOF_nc,NF90_GLOBAL,"IABSOR",IABSOR ))
      CALL chk_nf(NF90_PUT_ATT(EMOF_nc,NF90_GLOBAL,"ISURVAC",ISURVAC ))
      CALL chk_nf(NF90_PUT_ATT(EMOF_nc,NF90_GLOBAL,"IWAVE",IWAVE ))
      CALL chk_nf(NF90_PUT_ATT(EMOF_nc,NF90_GLOBAL,"IFD",IFD ))
      CALL chk_nf(NF90_PUT_ATT(EMOF_nc,NF90_GLOBAL,"FD-SCHEME",scheme))

C     SOURCE ATTRIBUTES
      CALL chk_nf(NF90_PUT_ATT(EMOF_nc,NF90_GLOBAL, "TX-POS", 
     &                         (/ SOURCEX,SOURCEY,SOURCEZ /) ))
      CALL chk_nf(NF90_PUT_ATT(EMOF_nc,NF90_GLOBAL,"TSIGNAL",TSIGNAL ))
      CALL chk_nf(NF90_PUT_ATT(EMOF_nc,NF90_GLOBAL,"ISIGNAL",ISIGNAL ))
      CALL chk_nf(NF90_PUT_ATT(EMOF_nc,NF90_GLOBAL,"ISOURCE",ISOURCE ))
      CALL chk_nf(NF90_PUT_ATT(EMOF_nc,NF90_GLOBAL,"TX-E-COMP", 
     &                         (/ ISOURCEEX,ISOURCEEY,ISOURCEEZ /) ))
      CALL chk_nf(NF90_PUT_ATT(EMOF_nc,NF90_GLOBAL, "TX-H-COMP", 
     &                         (/ ISOURCEHX,ISOURCEHY,ISOURCEHZ /) ))

C     RECEIVER ATTRIBUTES
      CALL chk_nf(NF90_PUT_ATT(EMOF_nc,NF90_GLOBAL, "RX-POS_S", 
     &                         (/ RECXS,RECYS,RECZS /) ))
      CALL chk_nf(NF90_PUT_ATT(EMOF_nc,NF90_GLOBAL, "RX-POS_E", 
     &                         (/ RECXE,RECYE,RECZE /) ))
      CALL chk_nf(NF90_PUT_ATT(EMOF_nc,NF90_GLOBAL,"IOUTTYPE",IOUTTYPE))
      CALL chk_nf(NF90_PUT_ATT(EMOF_nc,NF90_GLOBAL,"IOUTFORM",IOUTFORM))
      CALL chk_nf(NF90_PUT_ATT(
     &   EMOF_nc,NF90_GLOBAL,"OUTSTRIDE", 1 ) )
      CALL chk_nf(NF90_PUT_ATT(
     &   EMOF_nc,NF90_GLOBAL, "OUTSTEP", 1 ) )
      CALL chk_nf(NF90_PUT_ATT(EMOF_nc,NF90_GLOBAL, "RX-E-COMP", 
     &                         (/ IOUTEM(1),IOUTEM(2),IOUTEM(3) /) ))
      CALL chk_nf(NF90_PUT_ATT(EMOF_nc,NF90_GLOBAL, "RX-H-COMP", 
     &                         (/ IOUTEM(4),IOUTEM(5),IOUTEM(6) /) ))

C**********************************************************************
C     RECORD VARIABLES (E,H)_xyz : DEFINE AND ATTRIBUTES

      DO v=1,6	! record variables
        IF (IOUTEM(v).EQ.1) THEN
C         DEFINE RECORD VARIABLE
          CALL chk_nf(NF90_DEF_VAR(EMOF_nc,EM_NAME(v),nf90_real, 
     &        (/ XDimID, YDimID, ZDimID, TDimID /), EMOF_ID(v) ))
C         ATTRIBUTES FOR EM-FIELD COMPONENTS
          CALL chk_nf(NF90_PUT_ATT(
     &   		EMOF_nc,EMOF_ID(v),"title",EM_DESC(v) ))
        ENDIF
      END DO	! v record variables 
      print*, EM_NAME
      print*, EMOF_ID


C     LEAVE DEFINE MODE
      CALL chk_nf(NF90_ENDDEF(EMOF_nc))

C**********************************************************************
C     PRINT*, '  WRITE COORDINATE VARIABLE VALUES'

      CALL chk_nf(NF90_PUT_VAR(EMOF_nc,X_ID,
     &            XCOEM(SXOF:EXOF:1) ) )
      CALL chk_nf(NF90_PUT_VAR(EMOF_nc,Y_ID,
     &            YCOEM(SYOF:EYOF:1)))
      CALL chk_nf(NF90_PUT_VAR(EMOF_nc,Z_ID,
     &            ZCOEM(SZOF:EZOF:1)))
      CALL chk_nf(NF90_PUT_VAR(EMOF_nc,T_ID,
     &            TCOEM(ETOF:ETOF:1)))

      DEALLOCATE(XCOEM,YCOEM,ZCOEM,TCOEM)


C**********************************************************************
C     WRITE EM components to file

C     print*,'Write EX component'
      IF (IOUTEM(1).EQ.1)
     &        CALL chk_nf(NF90_PUT_VAR(EMOF_nc,EMOF_ID(1),
     &        EX(SXOF:EXOF:1,
     &         SYOF:EYOF:1,
     &         SZOF:EZOF:1,0),
     &         start  = (/ 1,1,1,1 /),
     &         stride = (/ 1,1,1,1 /),
     &         count  = (/ XDimOF,YDimOF,ZDimOF, 1 /)
     &         ))

C       print*,'Write EY component'
      IF (IOUTEM(2).EQ.1)
     &        CALL chk_nf(NF90_PUT_VAR(EMOF_nc,EMOF_ID(2),
     &        EY(SXOF:EXOF:1,
     &         SYOF:EYOF:1,
     &         SZOF:EZOF:1,0),
     &         start  = (/ 1,1,1,1 /),
     &         stride = (/ 1,1,1,1 /),
     &         count  = (/ XDimOF,YDimOF,ZDimOF, 1 /)
     &         ))

C       print*,'Write EZ component'
      IF (IOUTEM(3).EQ.1)
     &        CALL chk_nf(NF90_PUT_VAR(EMOF_nc,EMOF_ID(3),
     &        EZ(SXOF:EXOF:1,
     &           SYOF:EYOF:1,
     &           SZOF:EZOF:1,0),
     &         start  = (/ 1,1,1,1 /),
     &         stride = (/ 1,1,1,1 /),
     &         count  = (/ XDimOF,YDimOF,ZDimOF, 1 /)
     &         ))

C       print*,'Write HX component'
      IF (IOUTEM(4).EQ.1)
     &        CALL chk_nf(NF90_PUT_VAR(EMOF_nc,EMOF_ID(4),
     &        HX(SXOF:EXOF:1,
     &         SYOF:EYOF:1,
     &         SZOF:EZOF:1,0),
     &         start  = (/ 1,1,1,1 /),
     &         stride = (/ 1,1,1,1 /),
     &         count  = (/ XDimOF,YDimOF,ZDimOF, 1 /)
     &         ))

C       print*,'Write HY component'
      IF (IOUTEM(5).EQ.1)
     &        CALL chk_nf(NF90_PUT_VAR(EMOF_nc,EMOF_ID(5),
     &        HY(SXOF:EXOF:1,
     &         SYOF:EYOF:1,
     &         SZOF:EZOF:1,0),
     &         start  = (/ 1,1,1,1 /),
     &         stride = (/ 1,1,1,1 /),
     &         count  = (/ XDimOF,YDimOF,ZDimOF, 1 /)
     &         ))

C       print*,'Write HZ component'
      IF (IOUTEM(6).EQ.1)
     &        CALL chk_nf(NF90_PUT_VAR(EMOF_nc,EMOF_ID(6),
     &        HZ(SXOF:EXOF:1,
     &         SYOF:EYOF:1,
     &         SZOF:EZOF:1,0),
     &         start  = (/ 1,1,1,1 /),
     &         stride = (/ 1,1,1,1 /),
     &         count  = (/ XDimOF,YDimOF,ZDimOF, 1 /)
     &         ))

      CALL chk_nf(NF90_CLOSE(EMOF_nc))

C     PRINT*,'DONE WITH OUTFIELD NETCDF-File'
      END !	SUBROUTINE OUTFIELD
