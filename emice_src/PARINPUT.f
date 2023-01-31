C************************************************************************
C*                                                                      *
C*    SUBROUTINE ZUM EINLESEN DER RUN-PARAMETER  			*
C*                                                                      *
C************************************************************************

C*********************** RCS-KEYWORDS ******************************
C $Source:
C $Author:
C $Date:
C $RCSfile:
C $Revision:
C $State:
C*********************** RCS-KEYWORDS ******************************


      SUBROUTINE PARINPUT
      USE dynfelder_mod
      
C     WERTE INITIALISIEREN
      run="STD"
      inroot="./"
      outroot="./"
      ILEN=0
      PI4=ATAN(1.)
      PI=4.*PI4
      PI2=2.*PI
      FDDX=0
      FDDT=0
      XDIM=0
      YDIM=0
      ZDIM=0
      lla=1
      lle=50
      TotalTraces=0
      EnsembleNo=0
      TwoOrThree=2
      TwoOrFour=2
      FDScheme=0
      ISIGMA=0
      ISY=0

C     EINLESEFORMATE
 980  FORMAT(I10)
 981  FORMAT(F10.4)
 982  FORMAT(F12.6)
 983  FORMAT(A80)
 100  FORMAT(10F8.2)
 101  FORMAT(F8.2)
 102  FORMAT(10F8.5)

C     Ein/Ausgabedateien und -pfade
      CALL IOOPT

C     Werte werden eingelesen
      PRINT*,'RASTERFILE:', INFDR
      OPEN(98,FILE=INFDR,STATUS='OLD')
      READ(98,983) DES2FD
      READ(98,981) FDXMIN
      READ(98,981) FDXMAX
      READ(98,981) FDZMIN
      READ(98,981) FDZMAX
      READ(98,981) FDYMIN
      READ(98,981) FDYMAX
      READ(98,982) FDDX
      READ(98,*) XDIM
      READ(98,*) ZDIM
      READ(98,*) YDIM
      READ(98,981) SOURCEX
      READ(98,981) SOURCEZ
      READ(98,981) SOURCEY
      READ(98,981) RECXS
      READ(98,981) RECZS
      READ(98,981) RECYS
      READ(98,981) RECXE
      READ(98,981) RECZE
      READ(98,981) RECYE
      READ(98,982) FDTMIN
      READ(98,982) FDTMAX
      READ(98,982) FDDT
      READ(98,982) TSIGNAL
      READ(98,980) ITRACES
      READ(98,980) ITIMES
      IF(ITRACES.LE.0) ITRACES=1
      IF(ITIMES.LE.0) ITIMES=1
      READ(98,980) ISOURCE
      READ(98,980) ISIGNAL
      READ(98,980) IABSOR
      READ(98,980) ISURFAC
      READ(98,980) IWAVE
      READ(98,980) IFD        !
      READ(98,980) IOUTTYPE
      READ(98,980) IOUTFORM
      READ(98,980) OUTSTRIDE
      READ(98,980) OUTSTEP
      READ(98,980) ISOURCEEX
      READ(98,980) ISOURCEEY
      READ(98,980) ISOURCEEZ
      READ(98,980) ISOURCEHX
      READ(98,980) ISOURCEHY
      READ(98,980) ISOURCEHZ
      READ(98,980) IOUTEM(1)
      READ(98,980) IOUTEM(2)
      READ(98,980) IOUTEM(3)
      READ(98,980) IOUTEM(4)
      READ(98,980) IOUTEM(5)
      READ(98,980) IOUTEM(6)
      CLOSE(98,STATUS='KEEP')

C     Dimensionen der Zeit (wird von -I geaendert --> INFIELD)
      STMOD=FDTMIN/FDDT+1 	! Zeitintervall [1:*]
      ETMOD=FDTMAX/FDDT+1
      TDIM=ETMOD-STMOD+1
C     print*,'PARINPUT:'
C     print*,'eingelesen Zeitparameter:', 
C    &	FDTMIN,FDTMAX,STMOD,ETMOD,TDIM

C     Override ISIGNAL if -s is specified:
      IF (INWL(1:1).NE.' '.AND.ISIGNAL.LT.10) THEN 
	 ISIGNAL=0 
      ELSE IF (INWL(1:1).NE.' '.AND.ISIGNAL.GE.10) THEN
	 ISIGNAL=10 
      ENDIF

C     AUSGABEDATEIEN


      OUTSRC=outroot(1:ILEN(3)-1)//"/"//run(1:ILEN(1)-1)//".src"
      IF(IOUTFORM.EQ.0) THEN
        NCEMOUT=outroot(1:ILEN(3)-1)//"/"//run(1:ILEN(1)-1)//"EM.nc"
        NCPH=outroot(1:ILEN(3)-1)//"/"//run(1:ILEN(1)-1)//"PHYS.nc"
        OutFormat="NETCDF"
      ELSE IF(IOUTFORM.GT.0.AND.IOUTFORM.LT.10)THEN
        OUTEX=outroot(1:ILEN(3)-1)//"/"//run(1:ILEN(1)-1)//"EX.dat"
        OUTEY=outroot(1:ILEN(3)-1)//"/"//run(1:ILEN(1)-1)//"EY.dat"
        OUTEZ=outroot(1:ILEN(3)-1)//"/"//run(1:ILEN(1)-1)//"EZ.dat"
        OUTHX=outroot(1:ILEN(3)-1)//"/"//run(1:ILEN(1)-1)//"HX.dat"
        OUTHY=outroot(1:ILEN(3)-1)//"/"//run(1:ILEN(1)-1)//"HY.dat"
        OUTHZ=outroot(1:ILEN(3)-1)//"/"//run(1:ILEN(1)-1)//"HZ.dat"
        IF (IOUTFORM.EQ.2) THEN
           OutFormat="REFLEX Snapshots (total wavefield)"
        ELSE IF (IOUTFORM.EQ.3) THEN
           OutFormat="REFLEX RX 2D-line (single line)"
           ITRACES=1
        ELSE IF (IOUTFORM.EQ.4) THEN
           OutFormat="RX xz-plane"
        ENDIF
      ELSE IF(IOUTFORM.EQ.10)THEN
           OutFormat="NO OUTPUT DURING TIME LOOP"
      ELSE
        PRINT*,'ERROR: Output format not supported  - Abort Program'
        STOP
      ENDIF

C******************************************************************
C*                                                                *
C*     Festlegung, welche Art von Berechnung durchgefuehrt wird:  *
C*                                                                *
C******************************************************************

      IF(IFD.EQ.0) then
         TwoOrThree=2           ! 2-d
         TwoOrFour=2            ! O(2/2)
         FDScheme=0             ! Yee
         scheme='O22 2D Yee'
      ELSEIF(IFD.EQ.1) then
         TwoOrThree=2           ! 2-d
         TwoOrFour=4            ! O(2/4)
         FDScheme=0             ! Yee
         scheme='O24 2D Yee'
      ELSEIF(IFD.EQ.2) then
         TwoOrThree=2           ! 2-d
         TwoOrFour=2            ! O(2/4)
         FDScheme=1             ! E-Feldberechnung
         scheme='O24 2D E'
      ELSEIF(IFD.EQ.3) then
         TwoOrThree=2           ! 2-d
         TwoOrFour=4            ! O(2/4)
         FDScheme=1             ! E-Feldberechnung
         scheme='O24 2D E'
      ELSEIF(IFD.EQ.10) then
         TwoOrThree=3           ! 3-d
         TwoOrFour=2            ! O(2/2)
         FDScheme=0             ! Yee
         scheme='O22 3D Yee'
      ELSEIF(IFD.EQ.11) then
         TwoOrThree=3           ! 3-d
         TwoOrFour=4            ! O(2/4)
         FDScheme=0             ! Yee
         scheme='O24 3D Yee'
      ELSEIF(IFD.EQ.12) then
         TwoOrThree=3           ! 3-d
         TwoOrFour=2            ! O(2/2)
         FDScheme=1             ! E-Feldberechnung
         scheme='O22 3D E'
      ELSEIF(IFD.EQ.13) then
         TwoOrThree=3           ! 3-d
         TwoOrFour=4            ! O(2/4)
         FDScheme=1             ! E-Feldberechnung
         scheme='O24 3D E'
      ELSEIF(IFD.EQ.21) then
         TwoOrThree=1           ! 1-d
         TwoOrFour=4            ! O(2/4)
         FDScheme=0             ! Yee
         scheme='O24 1D Yee'
      ENDIF
      if (TwoOrThree.EQ.2.AND.
     &    ISOURCEEY.EQ.1.AND.ISOURCEEX.NE.1.AND.ISOURCEEZ.NE.1.AND.
     &    ISOURCEHY.NE.1.AND.
     &    IOUTEM(1).NE.1.AND.IOUTEM(3).NE.1.AND.IOUTEM(5).NE.1) THEN
         TEMODE = 1
      ELSE
         TEMODE = 0
      ENDIF

C NOCH ZU MACHEN:	if (TwoOrThree.EQ.1)	KANN NUR FUER ISOURCEEY = 1 funktionieren.

C******************************************************************
C     OUTPUT DESCRIPTION STRINGS

C     UNITS AND MARKER STRINGS
      DistanceDimension="m"
      IF(IWAVE.LT.10) then
         TimeDimension="ns"
      ELSE
         TimeDimension="ms"
      ENDIF
      ProfileDirection='X'
      ProfileConstant='Y'

C     STRING FUER AUSGABEREGION
      IF(IOUTTYPE.EQ.0) THEN
         MeasureArt = 'Modellraum incl. Rand ohne Abtastung'
      ELSE IF (IOUTTYPE.EQ.1) THEN
         MeasureArt = 'Modellraum ohne Abtastung'
      ELSE IF (IOUTTYPE.EQ.2) THEN
         MeasureArt = 'Modellraum incl. Rand'
      ELSE IF (IOUTTYPE.EQ.3) THEN
         MeasureArt = 'Modellraum'
      ELSE IF (IOUTTYPE.EQ.4) THEN
         MeasureArt = 'subregion (x,z,t) RECXS-RECXE,RECZS-RECZE '
      ELSE IF (IOUTTYPE.EQ.5) THEN
         MeasureArt = '2D-slice (z,t) RECXS,z'
      ELSE IF (IOUTTYPE.EQ.6) THEN
         MeasureArt = '1D-line (t) RECXS,RECZS'
      ENDIF

      END ! SUBROUTINE PARINPUT
