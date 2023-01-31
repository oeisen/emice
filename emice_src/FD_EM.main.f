C************************* RCS-KEYWORDS ********************************
C $Source: /pf/a/a270012/fdem_src/RCS/FD_EM.main.f,v $
C $Author: a270012 $
C $Date: 2002/06/26 13:39:26 $
C $RCSfile: FD_EM.main.f,v $
C $Revision: 5.0 $
C $State: Exp $
C************************* RCS-KEYWORDS ********************************
C
C***********************************************************************
C
C_1-6_L_________________ 7-71 _________________________________________L________
C
C***********************************************************************

C     VORSICHT: bei dieser Version sind keine Liao-Randbedingungen
C     mehr verwirklicht. Man mu· unbedingt noch pruefen, ob sich
C     dadurch Schwierigkeiten beim Einlesen der Parameterdatei ergeben!

C
C     Diese Version enthÑlt O(2/2)-Fall und O(2/4)- Fall fuer
C     absorbierende RÑnder in 3-D und 2-D

C     FD-EM: Simulation der Ausbreitung elektromagnetischer Wellen
C
C     Es wird ein staggered Grid angewandt:
C     E-Felder sind immer in einer Ortskoordinate um 0,5 gegnÅber dem
C     Feld der Materialkonstanten verschoben, H-Felder sogar in zwei
C     Ortskoordinaten. Bei der Berechnung der Felder wird dies nicht be-
C     rÅcksichtigt, sondern jedes Feld wird von seinem eigenen 
C     Nullpunkt aus in gleichabstÑndigen Intervallen betrachtet.
C     Auch bei der Berechnung der H-Felder, die gegenÅber den 
C     E-Feldern um 0,5 verschoben sind, wird nach der eben beschriebenen
C     Methode verfahren.
C
C     Bei den nachfolgend definierten Feldern bin ich davon ausgegangen,
C     dass die FELDER VON 0 BIS *DIM LAUFEN. 
C     (Wichtig fÅr die Dimension der Schleifen!)

C********************************************************************************
C     			CALL-Struktur der SUBROUTINEN
C
C	FD_EM.main
C 	|
C	|_MAINEXE:
C 	  | 
C	  |_PARINPUT/
C 	  | |
C	  | |_IOOPT/
C 	  |  
C	  |_PHYSNC/
C 	  |  
C	  |_EMNC/
C 	  |  
C	  |_CalculateHelpFieldsFor3D/
C 	  |  
C	  |_CalculateHelpFieldsFor2D/         
C 	  |  
C	  |_CalcTimeIndepFieldsForO22andO24/  
C 	  |  
C	  |_INFIELD/
C 	  | |
C	  | |_OUTTRACE/
C 	  |  
C	  |_TXSIG/
C 	  |  
C	  |_CalculateForO22AbsRange3D/        
C 	  | |
C	  | |_QUELLE
C 	  | |
C	  | |_OUTRACE
C 	  |  
C	  |_CalculateForO24AbsRange2DTEMode/  
C 	  | |
C	  | |_QUELLE
C 	  | |
C	  | |_OUTRACE
C 	  |  
C	  |_CalculateForO24AbsRange3D/        
C 	  | |
C	  | |_QUELLE
C 	  | |
C	  | |_OUTRACE
C	  |
C	  |_CalculateForO24AbsRange1DTEMode/  
C 	  | |
C	  | |_QUELLE
C 	  | |
C	  | |_OUTRACE
C	  |
C	  |_OUTFIELD
C	  |
C	  HANDLE_ERR: nach Bedarf
C	
C
C********************************************************************************

C********************************************************************************
C*										*
C*    Variablendefinition:                         				*
C*    Alle variablen Felder werden ueber das Modul dynfelder_mod definiert	*
C*										*
C********************************************************************************

      MODULE dynfelder_mod

C  Modellwerte
C  fct(x,y,z)
      REAL,SAVE :: SIGMA(:,:,:)	! SIMGA = 3D wegen der Daempfung am Rand!!!
C  fct(z)
      REAL,SAVE :: EPSILON(:)
C  fct=const. 
      REAL,SAVE :: MUE
C  (E,H)_xyz
      REAL,SAVE :: EX(:,:,:,:),EY(:,:,:,:),EZ(:,:,:,:),
     &     HX(:,:,:,:),HY(:,:,:,:),HZ(:,:,:,:)
C  Pointer auf (E,H)_xyz
      POINTER :: EM(:)
C  Hilfsfelder
      REAL,SAVE :: muehx,muehy,muehz
      REAL,SAVE :: sigmahx(:,:,:),sigmahy(:,:,:),sigmahz(:,:,:)
      REAL,SAVE :: epsilonhx(:,:,:),epsilonhy(:,:,:),epsilonhz(:,:,:)
      REAL SourceQ(:,:,:),SourcePhi(:,:,:)

      REAL EMOUTEX(:),EMOUTEY(:),EMOUTEZ(:)
      REAL EMOUTHX(:),EMOUTHY(:),EMOUTHZ(:)
      REAL ST(:),ST1(:)
      INTEGER NDTIME(:),TOPGRID(:)
      INTEGER IBALLA(0:4096)

C Dimension
      REAL,SAVE :: FDDX,FDDT,
     &     DELTA,DELTAT,
     &     DeltaX,DeltaY,DeltaZ,
     &     DeltaX24,DeltaY24,DeltaZ24

C Parameter
      PARAMETER(MAXXDIM=1000,MAXYDIM=1000,MAXZDIM=400,MAXTDIM1=3001,
     &RMUENULL=1.2566370614E-6,EPSILONNULL=8.854187817E-12)
      INTEGER OutVar_TDim	! E,H-Feldvariablen Zeit-Dimensionen (0:OutVar_TDim)
      INTEGER TwoOrThree,TwoOrFour,XDIM,YDIM,ZDIM,
     &TDIM,STMOD,ETMOD,FDScheme,
     &lla,lle
C	i,j,k,l,ll,m, 	GLOBAL DEFINIEREN!
C                     	BEI CALL INNERHALB SCHLEIFE EXPLICIT UEBERGEBEN!

C     Dateinamen, Eingabe, Ausgabe
      CHARACTER*80 INFDR,INFDP,INFDS,INFDD,INFDQ,INFDQP,INDES
      CHARACTER*80 OUTEX,OUTEY,OUTEZ,OUTHX,OUTHY,OUTHZ, OUTSRC
      CHARACTER*80 PAREX,PAREY,PAREZ,PARHX,PARHY,PARHZ
      CHARACTER*80 argv,inroot,outroot,run,INWL
      CHARACTER*80 DES2FD	! String fuer Befehlszeile des2fd ...
      CHARACTER*120 comline
      INTEGER narg		! Number of arguments on com.line
      INTEGER IINITIN		! Initialisierunglauf: Eingabe
      INTEGER IINITOUT		! Initialisierunglauf: Ausgabe
      INTEGER ILEN(3),ILENC

      REAL FDXMIN,FDXMAX,FDZMIN,FDZMAX,FDYMIN,FDYMAX
      REAL FDTMIN,FDTMAX
      REAL SOURCEX,SOURCEZ,SOURCEY,RECXS,RECZS,RECYS,RECXE,RECZE,RECYE
      INTEGER ITRACES,ITIMES,ISURFAC
      INTEGER IOUTTYPE		! Ausgaberegion fuer NETCDF
 		! 0: Ausgabe Modellraum incl. Rand ohne Abtastung
 		! 1: Ausgabe Modellraum ohne Abtastung   
 		! 2: Ausgabe Modellraum incl. Rand (Abtastung: OUTSTRIDE,OUTSTEP)
 		! 3: Ausgabe Modellraum (-"-)    
 		! 4: Ausgabe xz-subregion RECXS-RECXE, RECZS-RECZE (-"-)
 		! 5: Ausgabe 2D-slice (z,t) RECXS,z (-"-)
 		! 6: Ausgabe 1D-line (t) RECXS,RECZS (-"-)
      INTEGER IOUTFORM		! (Datei-)Format der Ausgabe
 		! 0: NETCDF
 		! 2: REFLEX Snapshots (total wavefield)
 		! 3: REFLEX REC 2D-line (single line)
 		! 4: REC xz-plane (2D-slice (z,t))
 		!10: NO OUTPUT DURING TIME LOOP
      INTEGER IABSOR	! ABC gerade:exponentiell ungerade:linear 
      INTEGER IWAVE
      INTEGER IFD	!
		! 0	2D O22 Yee	
		! 1	2D O24 Yee	
		! 2	2D O22 E-Feld
		! 3	2D O24 E-Feld
		! 10	3D O22 Yee	
		! 11	3D O24 Yee	
		! 12	3D O22 E-Feld
		! 13	3D O24 E-Feld
		! 21	1D O24 yee

C     Quellsignal, -position und Anregung:
      REAL TSIGNAL,SIGNAL(:)
      INTEGER ISOURCE	! Art der Quelle
		! 0: point source
		! 1: plane wave
		! 2: exploding reflector
      INTEGER LSIGNAL,ISIGNAL
      INTEGER ISOURCEEX,ISOURCEEY,ISOURCEEZ
      INTEGER ISOURCEHX,ISOURCEHY,ISOURCEHZ
      INTEGER IA,IE,JA,JE

      INTEGER IINEM(6),IOUTEM(6)
      INTEGER ISIGMA,ISX,ISY,ISOURCEZ,ORDER
      REAL PI2
      INTEGER TEMODE,NENT,IOUTS,IRESAM
      INTEGER MXREC1,MXREC2,MYREC1,MYREC2,NZREC1,NZREC2
      INTEGER IEND,IBEGIN,NGEOPH
      REAL TSCALE
      INTEGER*4 TraceNo,NoOfSamples,IKomp,EnsembleNo,CDPNo,ShotNo,
     &GeophoneNo,TraceMarker
      INTEGER*4 TraceTime
      INTEGER*4 TotalTraces
      REAL*4 ShotElevation,TimeDel,Distance
      REAL*4 ShotOrt1,ShotOrt2,GeophoneOrt1,GeophoneOrt2,CDPOrt1,
     &CDPOrt2,GeophoneElevation
      REAL*4 TraceDataSingle(1:32000)

      REAL*4 TimeIncrement,TraceIncrement,TimeBegin,TimeMeasured
      REAL*4 ProfileIncrement,rhelp
      REAL*4 XCoordBegin,XCoordEnd,YCoordBegin,YCoordEnd
      REAL*4 ShotPosition,ShotOffset,ShotReceiverDistance
      Integer*4 ScansMeasured,LongHelp,LTOUT,IXOUT,KZOUT
      Integer*4 MeasureSamples,FormatCode,ProfileArt,ihelp
      CHARACTER*21 DistanceDimension,TimeDimension
      CHARACTER*21 ProfileDirection,ProfileConstant
      CHARACTER*35 MeasureArt
      CHARACTER*35 OutFormat
      CHARACTER*10 scheme

C	VARIABLEN FUER RECHENZEIT, DATUM, CPUs:
      REAL seca,sece
      CHARACTER*24 fdate_beg,fdate_end
#ifndef NEC
      REAL timea,timee					! CRAY
      INTEGER threads
#else
      CHARACTER*8 fdate_date,fdate_time			! sx6
#endif

C     GLOBALE VARIABLEN FUER NETCDF:
      INTEGER NF90CLOB			! clobbing of NETCDF-files (1/0)
      INTEGER EM_nc	 		! NETCDF file ID for trace output
      INTEGER EM_ID(6) 			! Record Variable ID (E,H)_xyz
      CHARACTER*3 EM_NAME(6)		! Record Variable Name
      CHARACTER*30 EM_DESC(6)		! Description of EM record variables
      INTEGER OUTSTRIDE, OUTSTEP   	! space, time sampling
      INTEGER XDimOUT,YDimOUT,ZDimOUT	! Ausgabedimensionen			
      INTEGER SXOUT,SYOUT,SZOUT 	! Start der Ausgabeindizes
      INTEGER EXOUT,EYOUT,EZOUT 	! Ende der Ausgabeindizes
      CHARACTER*80 NCEMOUT		! EM output file name
      CHARACTER*80 NCEMOUTF		! EM output file name for -O
      CHARACTER*80 NCEMINF		! EM input file name for -I
      CHARACTER*80 NCPH			! Medium output file name


      ALLOCATABLE :: EX,EY,EZ,HX,HY,HZ,SIGMA,EPSILON,
     &    sigmahx,sigmahy,sigmahz,epsilonhx,
     &    epsilonhy,epsilonhz,
     &    SourceQ,SourcePhi,
     &    EMOUTEX,EMOUTEY,EMOUTEZ,EMOUTHX,EMOUTHY,EMOUTHZ,
     &    ST,ST1,NDTIME,TOPGRID,
     &    SIGNAL

C********************************************************************************

      END MODULE dynfelder_mod

C********************************************************************************


C********************************************************************************
C*              					                        *
C* 			BEGINN DES HAUPTPROGRAMMS 				*
C*                                      					*
C********************************************************************************

      PROGRAM MAIN

      PRINT*,'          '
      PRINT*,'*******************************************************'
      PRINT*,'                  FD-Program '
      PRINT*,'for solving the electromagnetic and seismic wave equation'
      PRINT*,'Copyright 1996/98 by K.J. Sandmeier, Karlsruhe, Germany'
      PRINT*,'Copyright 2000-02 by O. Eisen, AWI, Germany'
      PRINT*,''

C Endkopplung von dynfelder_mod und Hauptprogramm fuer schnellere Compilierung

      CALL MAINEXE	! Hauptprogramm. 

      PRINT*,'END OF PROGRAM'
      PRINT*,'*********************************************************'
      PRINT*,'          '

C********************************************************************************
C********************************************************************************
C*                                   						*
C* 			Ende des Hauptprogramms 				*
C*                                  					 	*
C********************************************************************************
C********************************************************************************

      END
