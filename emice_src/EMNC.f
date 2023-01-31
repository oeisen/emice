C********************************************************
C*                                                      *
C* SUBROUTINE zum oeffnen der NetCDF-Datei und          *
C* 		Definition der dim und var           	*	
C*                                                      *
C********************************************************

C*********************** RCS-KEYWORDS ******************************
C $Source: /pf/a/a270012/fdem_src/RCS/EMNC.f,v $
C $Author: a270012 $
C $Date: 2002/06/26 13:39:26 $
C $RCSfile: EMNC.f,v $
C $Revision: 5.0 $
C $State: Exp $
C*********************** RCS-KEYWORDS ******************************

      SUBROUTINE EMNC
      USE dynfelder_mod
      USE netcdf
C     INCLUDE 'netcdf.inc'

      INTEGER i,j,k,l,v
      INTEGER XDimID,YDimID,ZDimID,TDimID	! Dim-id
      INTEGER X_ID,Y_ID,Z_ID,T_ID               ! Coord.Var.-id
      INTEGER STOUT,ETOUT			! Ausgabeindizes Time
      REAL XCOEM(:),YCOEM(:),ZCOEM(:),TCOEM(:)

      ALLOCATABLE :: XCOEM,YCOEM,ZCOEM,TCOEM

C     PRINT*,' EM-WAVE NETCDF-File'

C**********************************************************************
C     Index-Intervalle der Ausgabevariablen und -koordinaten
      IF (IOUTTYPE.EQ.0.OR.IOUTTYPE.EQ.2) THEN	!Modellfeld incl. Rand
        SXOUT = 0
        SYOUT = YDIM/2	! ACHTUNG 3D
        SZOUT = 0
        EXOUT = XDIM
        EYOUT = YDIM/2	! ACHTUNG 3D
        EZOUT = ZDIM
      ELSE IF(IOUTTYPE.EQ.1.OR.IOUTTYPE.EQ.3) THEN !Modellfeld ohne Rand
        SXOUT = ISX
        SYOUT = YDIM/2	! ACHTUNG 3D
        SZOUT = ISIGMA
        EXOUT = XDIM-ISX
        EYOUT = SYOUT	! ACHTUNG 3D
        EZOUT = ZDIM-ISIGMA
      ELSE IF(IOUTTYPE.EQ.4) THEN	! xz-subregion RECX*, RECZ*
        SXOUT = MXREC1 
        SYOUT = YDIM/2
        SZOUT = NZREC1
        EXOUT = MXREC2
        EYOUT = SYOUT
        EZOUT = NZREC2
      ELSE IF(IOUTTYPE.EQ.5) THEN	! 2D-slice RECXS,z ohne Rand
        SXOUT = MXREC1 
        SYOUT = YDIM/2
        SZOUT = ISIGMA
        EXOUT = SXOUT
        EYOUT = SYOUT
        EZOUT = ZDIM-ISIGMA
      ELSE IF(IOUTTYPE.EQ.6) THEN	! 1D-line RECXS,RECZS ohne Rand
        SXOUT = MXREC1 
        SYOUT = YDIM/2
        SZOUT = NZREC1
        EXOUT = SXOUT
        EYOUT = SYOUT
        EZOUT = SZOUT
      ENDIF

      STOUT = STMOD
      ETOUT = ETMOD
      
      IF (IOUTTYPE.LE.1) THEN 	! Keine raeumlichen und zeitliche Rasterung 
        OUTSTRIDE = 1
        OUTSTEP = 1
      ENDIF

C     Dimensionen der Ausgabefelder
      XDIMOUT=EXOUT-SXOUT+1	! +1 notwendig, da alle schleifen von
      YDIMOUT=EYOUT-SYOUT+1	! 0 bis [XYZ]DIM zaehlen
      ZDIMOUT=EZOUT-SZOUT+1	! 

      IF (XDIMOUT.EQ.1) THEN	! Keine x-Dimension 
 	  XDimOUT=1
      ELSE IF (MOD(XDIMOUT,OUTSTRIDE).GT.0) THEN
          XDimOUT=INT((XDIMOUT)/OUTSTRIDE)+1		
      ELSE
          XDimOUT=INT((XDIMOUT)/OUTSTRIDE)
      ENDIF
      IF (YDIMOUT.EQ.1) THEN	! Keine y-Dimension
 	  YDimOUT=1
      ELSE IF (MOD(YDIMOUT,OUTSTRIDE).GT.0) THEN
          YDimOUT=INT((YDIMOUT)/OUTSTRIDE)+1		
      ELSE
          YDimOUT=INT((YDIMOUT)/OUTSTRIDE)
      ENDIF
      IF (ZDIMOUT.EQ.1) THEN	! Keine z-Dimension
 	  ZDimOUT=1
      ELSE IF (MOD(ZDIMOUT,OUTSTRIDE).GT.0) THEN
          ZDimOUT=INT((ZDIMOUT)/OUTSTRIDE)+1		
      ELSE
          ZDimOUT=INT((ZDIMOUT)/OUTSTRIDE)
      ENDIF			

C**********************************************************************
C     ALLOCATE COORDINATE VARIABLES AND ASSIGN VALUES

      ALLOCATE( 
     &     XCOEM(0:XDIM), 
     &     YCOEM(0:YDIM),
     &     ZCOEM(0:ZDIM),
     &     TCOEM(STOUT:ETOUT))

      DO i=0,XDIM
        XCOEM(i)=FDXMIN+(i-ISX)*DELTA
      END DO
      DO j=0,YDIM
        YCOEM(j)=FDYMIN+(j-ISY)*DELTA
      END DO
      DO k=0,ZDIM
        ZCOEM(k)=FDZMIN+(k-ISIGMA)*DELTA
      END DO
      DO l=STOUT,ETOUT
        TCOEM(l)=(l-1)*DELTAT*1e9	! in ns
      END DO

C**********************************************************************
C     DEFINE NETCDF EM-FIELD OUTPUT FILE NCEMOUT

      IF (NF90CLOB.EQ.1) THEN
        CALL chk_nf(NF90_CREATE(NCEMOUT,NF90_CLOBBER,EM_nc))
      ELSE
        CALL chk_nf(NF90_CREATE(NCEMOUT,NF90_NOCLOBBER,EM_nc))
      ENDIF

C**********************************************************************
C     COORDINATE VARIABLES AND GLOBAL ATTRIBUTES

C     PRINT*, 'DEFINE DIMENSIONS'
      CALL chk_nf(NF90_DEF_Dim(EM_nc, "X", XDimOUT, XDimID))
      CALL chk_nf(NF90_DEF_Dim(EM_nc, "Y", YDimOUT, YDimID))
      CALL chk_nf(NF90_DEF_Dim(EM_nc, "Z", ZDimOUT, ZDimID))
      CALL chk_nf(NF90_DEF_Dim(EM_nc,"Time",nf90_unlimited,TDimId))

C     PRINT*, '  DEFINE COORDINATE VARIABLES'
      CALL chk_nf(NF90_DEF_VAR(EM_nc,"X",nf90_real,(/ XDimID /),X_ID))
      CALL chk_nf(NF90_DEF_VAR(EM_nc,"Y",nf90_real,(/ YDimID /),Y_ID))
      CALL chk_nf(NF90_DEF_VAR(EM_nc,"Z",nf90_real,(/ ZDimID /),Z_ID))
      CALL chk_nf(NF90_DEF_VAR(EM_nc,"Time",nf90_real,
     &     (/ TDimID /),T_ID ) )

C     PRINT*, '  DEFINE GLOBAL ATTRIBUTES'

C     X-ATTRIBUTES
      CALL chk_nf(NF90_PUT_ATT(EM_nc,X_ID,"units",DistanceDimension ))
      CALL chk_nf(NF90_PUT_ATT(EM_nc,X_ID,"valid_range",
     &                         (/ FDXMIN, FDXMAX /) ))
      CALL chk_nf(NF90_PUT_ATT(EM_nc,X_ID,"delta_x",DELTA ))

C     Y-ATTRIBUTES
      CALL chk_nf(NF90_PUT_ATT(EM_nc,Y_ID,"units",DistanceDimension ))
      CALL chk_nf(NF90_PUT_ATT(EM_nc,Y_ID,"valid_range",
     &                         (/ FDYMIN, FDYMAX /) ))
      CALL chk_nf(NF90_PUT_ATT(EM_nc,Y_ID,"delta_y",DELTA ))

C     Z-ATTRIBUTES
      CALL chk_nf(NF90_PUT_ATT(EM_nc,Z_ID,"units",DistanceDimension ))
      CALL chk_nf(NF90_PUT_ATT(EM_nc,Z_ID,"valid_range",
     &                         (/ FDZMIN, FDZMAX /) ))
      CALL chk_nf(NF90_PUT_ATT(EM_nc,Z_ID,"delta_z",DELTA ))

C     T-ATTRIBUTES
      CALL chk_nf(NF90_PUT_ATT(EM_nc,T_ID,"units",TimeDimension ))
      CALL chk_nf(NF90_PUT_ATT(EM_nc,T_ID,"valid_range",
     &                         (/ FDTMIN, FDTMAX /) ))
      CALL chk_nf(NF90_PUT_ATT(EM_nc,T_ID,"delta_t",DELTAT ))

C    HISTORY ATTRIBUTES
      CALL chk_nf(NF90_PUT_ATT(EM_nc,NF90_GLOBAL, "history_start", 
     &            fdate_beg ) )
      CALL chk_nf(NF90_PUT_ATT(EM_nc,NF90_GLOBAL, "history", 
     &            comline(1:ilenc-1) ) )
      CALL chk_nf(NF90_PUT_ATT(EM_nc,NF90_GLOBAL, "des2fd", 
     &            DES2FD ) )

C     FD ATTRIBUTES
      CALL chk_nf(NF90_PUT_ATT(
     &   EM_nc,NF90_GLOBAL, "output-type", MeasureArt ) )
      CALL chk_nf(NF90_PUT_ATT(EM_nc,NF90_GLOBAL, "ITRACES", ITRACES ))
      CALL chk_nf(NF90_PUT_ATT(EM_nc,NF90_GLOBAL, "ITIMES", ITIMES ))
      CALL chk_nf(NF90_PUT_ATT(EM_nc,NF90_GLOBAL, "IABSOR", IABSOR ))
      CALL chk_nf(NF90_PUT_ATT(EM_nc,NF90_GLOBAL, "ISURVAC", ISURVAC ))
      CALL chk_nf(NF90_PUT_ATT(EM_nc,NF90_GLOBAL, "IWAVE", IWAVE ))
      CALL chk_nf(NF90_PUT_ATT(EM_nc,NF90_GLOBAL, "IFD", IFD ))
      CALL chk_nf(NF90_PUT_ATT(EM_nc,NF90_GLOBAL, "FD-SCHEME",scheme))

C     SOURCE ATTRIBUTES
      CALL chk_nf(NF90_PUT_ATT(EM_nc,NF90_GLOBAL, "TX-POS", 
     &                         (/ SOURCEX,SOURCEY,SOURCEZ /) ))
      CALL chk_nf(NF90_PUT_ATT(EM_nc,NF90_GLOBAL, "TSIGNAL", TSIGNAL ))
      CALL chk_nf(NF90_PUT_ATT(EM_nc,NF90_GLOBAL, "ISIGNAL", ISIGNAL ))
      CALL chk_nf(NF90_PUT_ATT(EM_nc,NF90_GLOBAL, "ISOURCE", ISOURCE ))
      CALL chk_nf(NF90_PUT_ATT(EM_nc,NF90_GLOBAL, "TX-E-COMP", 
     &                         (/ ISOURCEEX,ISOURCEEY,ISOURCEEZ /) ))
      CALL chk_nf(NF90_PUT_ATT(EM_nc,NF90_GLOBAL, "TX-H-COMP", 
     &                         (/ ISOURCEHX,ISOURCEHY,ISOURCEHZ /) ))

C     RECEIVER ATTRIBUTES
      CALL chk_nf(NF90_PUT_ATT(EM_nc,NF90_GLOBAL, "RX-POS_S", 
     &                         (/ RECXS,RECYS,RECZS /) ))
      CALL chk_nf(NF90_PUT_ATT(EM_nc,NF90_GLOBAL, "RX-POS_E", 
     &                         (/ RECXE,RECYE,RECZE /) ))
      CALL chk_nf(NF90_PUT_ATT(EM_nc,NF90_GLOBAL,"IOUTTYPE",IOUTTYPE))
      CALL chk_nf(NF90_PUT_ATT(EM_nc,NF90_GLOBAL,"IOUTFORM",IOUTFORM))
      CALL chk_nf(NF90_PUT_ATT(
     &   EM_nc,NF90_GLOBAL,"OUTSTRIDE", OUTSTRIDE ) )
      CALL chk_nf(NF90_PUT_ATT(
     &   EM_nc,NF90_GLOBAL, "OUTSTEP", OUTSTEP ) )
      CALL chk_nf(NF90_PUT_ATT(EM_nc,NF90_GLOBAL, "RX-E-COMP", 
     &                         (/ IOUTEM(1),IOUTEM(2),IOUTEM(3) /) ))
      CALL chk_nf(NF90_PUT_ATT(EM_nc,NF90_GLOBAL, "RX-H-COMP", 
     &                         (/ IOUTEM(4),IOUTEM(5),IOUTEM(6) /) ))

C**********************************************************************
C     RECORD VARIABLES (E,H)_xyz : DEFINE AND ATTRIBUTES

      DO v=1,6	! record variables
        IF (IOUTEM(v).EQ.1) THEN
C         DEFINE RECORD VARIABLE
          CALL chk_nf(NF90_DEF_VAR(EM_nc,EM_NAME(v),nf90_real, 
     &        (/ XDimID, YDimID, ZDimID, TDimID /), EM_ID(v) ))
C         ATTRIBUTES FOR EM-FIELD COMPONENTS
          CALL chk_nf(NF90_PUT_ATT(EM_nc,EM_ID(v),"title",EM_DESC(v) ))
        ENDIF
      END DO	! v record variables 
      print*, EM_NAME
      print*, EM_ID

C     LEAVE DEFINE MODE
      CALL chk_nf(NF90_ENDDEF(EM_nc))

C**********************************************************************
C     PRINT*, '  WRITE COORDINATE VARIABLE VALUES'

      CALL chk_nf(NF90_PUT_VAR(EM_nc,X_ID,
     &            XCOEM(SXOUT:EXOUT:OUTSTRIDE) ) )
      CALL chk_nf(NF90_PUT_VAR(EM_nc,Y_ID,
     &            YCOEM(SYOUT:EYOUT:OUTSTRIDE)))
      CALL chk_nf(NF90_PUT_VAR(EM_nc,Z_ID,
     &            ZCOEM(SZOUT:EZOUT:OUTSTRIDE)))
      CALL chk_nf(NF90_PUT_VAR(EM_nc,T_ID,
     &            TCOEM(STOUT:ETOUT:OUTSTEP)))

      DEALLOCATE(XCOEM,YCOEM,ZCOEM,TCOEM)

C     PRINT*,'DONE WITH EM-WAVE NETCDF-File'
      END !	SUBROUTINE EMNC
