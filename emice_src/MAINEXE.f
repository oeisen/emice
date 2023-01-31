C************************* RCS-KEYWORDS ********************************
C $Source: /pf/a/a270012/fdem_src/RCS/FD_EM.main.f,v $
C $Author: a270012 $
C $Date: 2002/06/26 13:39:26 $
C $RCSfile: FD_EM.main.f,v $
C $Revision: 5.0 $
C $State: Exp $
C************************* RCS-KEYWORDS ********************************
C
C
C***********************************************************************
C
C_1-6_L_________________ 7-71 _________________________________________L________
C
C***********************************************************************
      

C Endkopplung von dynfelder_mod und Hauptprogramm fuer schnellere Compilierung

      SUBROUTINE MAINEXE	
      USE dynfelder_mod
      USE netcdf
C     INCLUDE 'netcdf.inc'


#ifndef NEC
      CHARACTER*24 fdate			! SGI
#endif

C***************************************************
C*                                                 *
C*     Groessenfestlegung und Einlesen der Felder  *
C*                                                 *
C***************************************************

C     Berechnung Start- & Rechenzeit:
#ifndef NEC
      seca=second()				! SGI
      timea=TIMEF()				! SGI
      fdate_beg = fdate()			! SGI
#else
      CALL CPU_TIME(seca)			! SX-6
      CALL DATIM(fdate_date,fdate_time,4)	! SX-6
      fdate_beg = fdate_date//" "//fdate_time	! SX-6
#endif
      WRITE(*,*) 'STARTED AT ',fdate_beg
      PRINT*,''
 
C*******************************************************************************
C     READ RUN-PARAMETER AND DETERMINE CALCULATION TYPE

      CALL PARINPUT

C*******************************************************************************

C     QUELLE
      if(ISOURCE.EQ.2) then
         TSIGNAL=0.5*TSIGNAL
C         DELTAT=0.5*DELTAT
         FDTMAX=0.5*FDTMAX
      ENDIF
      IF(IWAVE.LT.10) then
        TSIGNAL=TSIGNAL*1.0E-9
        DeltaT=FDDT*1.0E-9	! Eingabeparameter in ns
      ENDIF
      DELTA=FDDX		! Eingabeparameter in m
      DELTAX=DELTA
      DELTAY=DELTA
      DELTAZ=DELTA

      OMEGA=0
      IF(TSIGNAL.GT.0) OMEGA=PI2/TSIGNAL

C	Y (X,Y)-Dimension(en) bei 2D (1D) beschraenken:
      IF(TwoOrThree.EQ.1) THEN
	XDIM=0
	YDIM=0
      ENDIF
      IF(TwoOrThree.EQ.2) YDIM=0

C	Error MAX TIME STEPS:
      IF(IOUTFORM.EQ.1.AND.ETMOD.GT.4096) CALL HANDLE_ERR(4)
C	JETZT IN TXSIG :     LSIGNAL=TSIGNAL/DELTAT
      if(ISOURCE.EQ.2) then
         TSCALE=2
      ELSE
         TSCALE=1
      ENDIF
      IF ((IABSOR.EQ.1.OR.IABSOR.EQ.2).AND.
     &    (IWAVE.LT.10)) THEN
        ISIGMA=50
      ELSEIF ((IABSOR.EQ.3.OR.IABSOR.EQ.4).AND.
     &        (IWAVE.LT.10)) THEN
        ISIGMA=100
      ELSEIF ((IABSOR.EQ.5.OR.IABSOR.EQ.6).AND.
     &        (IWAVE.LT.10)) THEN
        ISIGMA=25
      ELSE	! Rand: 5 m, unabh. von DELTA
        ISIGMA=5/DELTA
      ENDIF

      PRINT*,'model size in points: X:',XDIM,'Y:',YDIM,'Z:',ZDIM
      PRINT*,'size of absorbing range: ', ISIGMA
      if(TwoOrThree.GT.1) THEN
	ISX=ISIGMA			! x-Rand existiert
        if(TwoOrThree.EQ.3) ISY=ISIGMA	! y-Rand existiert
      ELSE 				! nur z-Rand existiert
	ISX=0
  	ISY=0
      ENDIF
      IXOUT=0
      DO i=ISX,XDIM+ISX
         I1=MOD(I,ITRACES)
         IF(I1.EQ.0) THEN
            IXOUT=IXOUT+1
         ENDIF
      ENDDO
      KZOUT=0
      DO k=ISIGMA,ZDIM+ISIGMA
         K1=MOD(K,ITRACES)
         IF(K1.EQ.0) THEN
            KZOUT=KZOUT+1
         ENDIF
      ENDDO
      LTOUT=0
      DO L=STMOD,ETMOD,ITIMES
         L1=MOD(L-1,ITIMES)
         IF(L1.EQ.0) THEN
            LTOUT=LTOUT+1
         ENDIF
      ENDDO

C********************************************************
C*                                                      *
C*	Dateien zum Rausschreiben der Daten oeffnen       *
C*                                                      *
C********************************************************

      IF (IWAVE.EQ.10) CALL HANDLE_ERR(IWAVE)
      IF (IWAVE.EQ.11) CALL HANDLE_ERR(IWAVE)
      IF (IWAVE.EQ.12) CALL HANDLE_ERR(IWAVE)
      WRITE(*,'(X,A,I4,A,$)') 'OUTPUT MODE:',IOUTFORM,':'
      WRITE(*,'(X,A)') OutFormat
      WRITE(*,'(X,A,I4,A,$)') 'OUTPUT TYPE:',IOUTTYPE,':'
      WRITE(*,'(X,A)') MeasureArt
      WRITE(*,'(X,A)') 'OUTPUT FILES:'
      OPEN(40,FILE=OUTSRC,STATUS='UNKNOWN')
      WRITE(*,403) '    OUTSRC:',OUTSRC
      IF(IOUTFORM.EQ.0) THEN
        WRITE(*,403) '    NCVAR:',NCEMOUT
        WRITE(*,403) '    NCPHY:',NCPH
      ELSE IF (MOD(IOUTFORM,10).NE.0) THEN
        IF(IOUTEM(1).EQ.1) THEN
         PRINT*,OutEX
         OPEN(20,FILE=OUTEX,STATUS='UNKNOWN')
        ENDIF
        IF(IOUTEM(2).EQ.1) THEN
         PRINT*,OutEY
         OPEN(21,FILE=OUTEY,STATUS='UNKNOWN')
        ENDIF
        IF(IOUTEM(3).EQ.1) THEN
         PRINT*,OutEZ
         OPEN(22,FILE=OUTEZ,STATUS='UNKNOWN')
        ENDIF
        IF(IOUTEM(4).EQ.1) THEN
         PRINT*,OutHX
         OPEN(23,FILE=OUTHX,STATUS='UNKNOWN')
        ENDIF
        IF(IOUTEM(5).EQ.1) THEN
         PRINT*,OutHY
         OPEN(24,FILE=OUTHY,STATUS='UNKNOWN')
        ENDIF
        IF(IOUTEM(6).EQ.1) THEN
         PRINT*,OutHZ
         OPEN(25,FILE=OUTHZ,STATUS='UNKNOWN')
        ENDIF
      ENDIF ! IOUTFORM

 200  FORMAT(3I10)
 201  FORMAT(7F12.6)
 202  FORMAT(I10,3F12.6)

C*******************************
C*                             *
C*	TX/RX Positionen:      *
C*                             *
C*******************************
      IF(DELTAX.GT.0) THEN
         MXREC1=NINT((RECXS-FDXMIN)/DeltaX)+ISX
         MXREC2=NINT((RECXE-FDXMIN)/DeltaX)+ISX
         MYREC1=NINT((RECYS-FDYMIN)/DeltaX)+ISY
         MYREC2=NINT((RECYE-FDYMIN)/DeltaX)+ISY
         NZREC1=NINT((RECZS-FDZMIN)/DeltaX)+ISIGMA
         NZREC2=NINT((RECZE-FDZMIN)/DeltaX)+ISIGMA
C         PRINT*,'IF:',IOUTFORM,MXREC1,MXREC2,
C    &MYREC1,MYREC2,NZREC1,NZREC2
        IF(ABS(NZREC2-NZREC1).GT.ABS(MXREC1-MXREC2)) THEN
          ProfileDirection='Y'
          ProfileConstant='X'
        ENDIF
      ENDIF
      ISOURCEZ=(SOURCEZ-FDZMIN)/DELTAX+ISIGMA

C********************************************************
C		ABC SETZEN

      IF (IABSOR.GT.0) THEN
        IF(MOD(IABSOR,2).NE.0) THEN	! IABSOR ungerade
           PRINT*,'absorbing linear range of ',ISIGMA,' points'
        ELSE				! IABSOR gerade
           PRINT*,'Absorbing exponential range of ',ISIGMA,' points'
        ENDIF
        ALLOCATE(
     &   SIGMA(-ISX:XDIM+ISX,-ISY:YDIM+ISY,-ISIGMA:ZDIM+ISIGMA),
     &   EPSILON(-ISIGMA:ZDIM+ISIGMA),
     &   TOPGRID(0:XDIM))
         IF(ISOURCE.EQ.2) then
           ALLOCATE (SourceQ(-ISX:XDIM+ISX,-ISY:YDIM+ISY,
     &                       -ISIGMA:ZDIM+ISIGMA),
     &            SourcePhi(-ISX:XDIM+ISX,-ISY:YDIM+ISY,
     &                      -ISIGMA:ZDIM+ISIGMA))
         ENDIF
      ELSE
         ISIGMA=0
         ISX=0
         ISY=0
         ALLOCATE (
     &            SIGMA(0:XDIM,0:YDIM,0:ZDIM),
     &            EPSILON(0:ZDIM),
     &            TOPGRID(0:XDIM))
         IF(ISOURCE.EQ.2) then
           ALLOCATE (SourceQ(0:XDIM,0:YDIM,0:ZDIM),
     &            SourcePhi(0:XDIM,0:YDIM,0:ZDIM))
         ENDIF
      ENDIF	! IABSOR.GT.0

      OPEN(9,FILE=INDES,STATUS='OLD')
C      OPEN(10,FILE=INFDP,STATUS='OLD')
C      OPEN(11,FILE=INFDS,STATUS='OLD')
C      OPEN(12,FILE=INFDD,STATUS='OLD')
      IF(ISOURCE.EQ.2) then
C         OPEN(13,FILE=INFDQ,STATUS='OLD')
C         OPEN(14,FILE=INFDQP,STATUS='OLD')
      ENDIF


C***********************************************************************
C*                                                                     *
C*    SUBROUTINE ZUM EINLESEN DER PARAMETER EPSILON und SIGMA          *
C*    FORMAT F12.5 fuer EPSILON   F12.5 fuer SIGMA(Siemens/M)          *
C*    SPALTENWEISES EINLESEN, FUER JEDEN X-WERT SAEMTLICHE Z-WERTE     *
C*                                                                     *
C***********************************************************************

C	Einlesen der EPSILON,SIGMA-Werte fuer x=0, y=0.
C	Da lateral homogen werden die Werte auf die anderen
C	Punkte uebertragen.

  99  FORMAT(F12.5,F12.5)
 103  FORMAT(10F8.3)
      i=0
      j=0
      TOPGRID(i)=0
      DO K=0,ZDIM,1
	       READ(9,99) EPSILON(k),SIGMA(i,j,k)
               IF(EPSILON(k).LE.0.00001) THEN
                  TOPGRID(i)=K+1
                  EPSILON(k)=1.0
               ENDIF
C      PRINT*, 'EPSILON: ', EPSILON(k), 'SIGMA: ', SIGMA(i,j,k)
      END DO

C      READ(12,102) (SIGMA(i,j,k),k=0,ZDIM)
c      Do k=0,ZDIM
c        IF(SIGMA(i,j,k).LE.0) SIGMA(i,j,k)=0.000001
c      ENDDO
      IF(ISOURCE.EQ.2) then	! Exploding reflector
        READ(13,103) (SourceQ(i,j,k),k=0,ZDIM)
        READ(14,103) (SourcePhi(i,j,k),k=0,ZDIM)
        Do k=0,ZDIM
          SourceQ(i,j,k)=SourceQ(i,j,k)-COS(SourcePHI(i,j,k))*
     &    SourceQ(i,j,k)*SourceQ(i,j,k)
          IF(EPSILON(k).GT.0) THEN
            SourceQ(i,j,k)=SourceQ(i,j,k)/Sqrt(EPSILON(k))
          END IF
        END DO
      ENDIF

      DO j=0,YDIM
         DO i=0,XDIM
	    DO k=0,ZDIM
	      SIGMA(i,j,k) = SIGMA(0,0,k)
	    END DO
         END DO
      END DO
      CLOSE(9,STATUS='KEEP')
C      CLOSE(10,STATUS='KEEP')
C      CLOSE(11,STATUS='KEEP')
C      CLOSE(12,STATUS='KEEP')
      IF(ISOURCE.EQ.2) then
         CLOSE(13,STATUS='KEEP')
         CLOSE(14,STATUS='KEEP')
      ENDIF

C***********************************************************************
C*                                                                      *
C*    ZUWEISUNG DER DAEMPFUNGSZONE
C*                                                                      *
C************************************************************************
c      if (TwoOrThree.EQ.2) then

      PRINT*,'Initialize absorbing range'
      IF(MOD(IABSOR,2).NE.0) THEN
         FAC1=REAL(ISIGMA/50)
      ELSEIF(MOD(IABSOR,2).EQ.0) THEN
         FAC1=REAL(ISIGMA/20)
      ENDIF
       DO k=-1,-ISIGMA,-1
        DO j=0,YDIM
        DO i=0,XDIM
         IF(ISOURCE.EQ.2) then
           SourceQ(i,j,k)=SourceQ(i,j,0)
           SourcePhi(i,j,k)=SourcePhi(i,j,0)
         ENDIF
         SIGMAEND=FAC1*OMEGA*EPSILON(0)*EPSILONNULL
c         IF(SourceQ(I,J,K).NE.0.AND.ISOURCE.EQ.2) SIGMAEND=0
         IF(SIGMAEND.GT.SIGMA(i,j,0)) THEN
           IF(MOD(IABSOR,2).NE.0) THEN
             SIGMA(i,j,k)=SIGMA(i,j,0)+(SIGMAEND-SIGMA(i,j,0))*
     &       ABS(k)/ISIGMA
           ELSEIF(MOD(IABSOR,2).EQ.0) THEN
             IF(SIGMA(i,j,0).LE.0) SIGMA(i,j,0)=0.001*SIGMAEND
             SIGMA(i,j,k)=SIGMA(i,j,0)*EXP(LOG(SIGMAEND/SIGMA(i,j,0))*
     &       ABS(k)/ISIGMA)
           ELSE
             SIGMA(i,j,k)=SIGMA(i,j,0)
           ENDIF
         ELSE
           SIGMA(i,j,k)=SIGMA(i,j,0)
         ENDIF
        ENDDO
        ENDDO
        EPSILON(k)=EPSILON(0)
       ENDDO

       DO k=ZDIM+1,ZDIM+ISIGMA,1
        DO j=0,YDIM
        DO i=0,XDIM
         IF(ISOURCE.EQ.2) then
           SourceQ(i,j,k)=SourceQ(i,j,ZDIM)
           SourcePhi(i,j,k)=SourcePhi(i,j,ZDIM)
         ENDIF
         SIGMAEND=FAC1*OMEGA*EPSILON(ZDIM)*EPSILONNULL
c         IF(SourceQ(I,J,K).NE.0.AND.ISOURCE.EQ.2) SIGMAEND=0
         IF(SIGMAEND.GT.SIGMA(i,j,ZDIM)) THEN
           IF(MOD(IABSOR,2).NE.0) THEN
             SIGMA(i,j,k)=SIGMA(i,j,ZDIM)+(SIGMAEND-SIGMA(i,j,ZDIM))*
     &       ABS(k-ZDIM)/ISIGMA
           ELSEIF(MOD(IABSOR,2).EQ.0) THEN
             IF(SIGMA(i,j,ZDIM).LE.0) SIGMA(i,j,ZDIM)=0.001*SIGMAEND
             SIGMA(i,j,k)=SIGMA(i,j,ZDIM)*EXP(LOG(SIGMAEND/
     &       SIGMA(i,j,ZDIM))*ABS(k-ZDIM)/ISIGMA)
           ELSE
             SIGMA(i,j,k)=SIGMA(i,j,ZDIM)
           ENDIF
         ELSE
           SIGMA(i,j,k)=SIGMA(i,j,ZDIM)
         ENDIF
        ENDDO
        ENDDO
        EPSILON(k)=EPSILON(ZDIM)
       ENDDO

       DO k=-ISIGMA,ZDIM+ISIGMA
        DO j=0,YDIM
        DO i=-1,-ISX,-1
         IF(ISOURCE.EQ.2) then
           SourceQ(i,j,k)=SourceQ(0,j,k)
           SourcePhi(i,j,k)=SourcePhi(0,j,k)
         ENDIF
         SIGMAEND=FAC1*OMEGA*EPSILON(k)*EPSILONNULL
c         if(ISOURCE.EQ.2) SIGMAEND=0
         IF(SIGMAEND.GT.SIGMA(0,j,k)) THEN
           IF(MOD(IABSOR,2).NE.0) THEN
             SIGMA(i,j,k)=SIGMA(0,j,k)+(SIGMAEND-SIGMA(0,j,k))*
     &       ABS(i)/ISIGMA
           ELSEIF(MOD(IABSOR,2).EQ.0) THEN
             IF(SIGMA(0,j,k).LE.0) SIGMA(0,j,k)=0.001*SIGMAEND
             SIGMA(i,j,k)=SIGMA(0,j,k)*EXP(LOG(SIGMAEND/
     &       SIGMA(0,j,k))*ABS(i)/ISIGMA)
           ELSE
             SIGMA(i,j,k)=SIGMA(0,j,k)
           ENDIF
         ELSE
           SIGMA(i,j,k)=SIGMA(0,j,k)
         ENDIF
        ENDDO
        ENDDO
       ENDDO

       DO k=-ISIGMA,ZDIM+ISIGMA
        DO j=0,YDIM
        DO i=XDIM+1,XDIM+ISX
         IF(ISOURCE.EQ.2) then
           SourceQ(i,j,k)=SourceQ(XDIM,j,k)
           SourcePhi(i,j,k)=SourcePhi(XDIM,j,k)
         ENDIF
         SIGMAEND=FAC1*OMEGA*EPSILON(k)*EPSILONNULL
c         IF(SourceQ(I,J,K).NE.0.AND.ISOURCE.EQ.2) SIGMAEND=0
         IF(SIGMAEND.GT.SIGMA(XDIM,j,k)) THEN
           IF(MOD(IABSOR,2).NE.0) THEN
             SIGMA(i,j,k)=SIGMA(XDIM,j,k)+(SIGMAEND-SIGMA(XDIM,j,k))*
     &       ABS(i-XDIM)/ISIGMA
           ELSEIF(MOD(IABSOR,2).EQ.0) THEN
             IF(SIGMA(XDIM,j,k).LE.0) SIGMA(XDIM,j,k)=0.001*SIGMAEND
             SIGMA(i,j,k)=SIGMA(XDIM,j,k)*EXP(LOG(SIGMAEND/
     &       SIGMA(XDIM,j,k))*ABS(i-XDIM)/ISIGMA)
           ELSE
             SIGMA(i,j,k)=SIGMA(XDIM,j,k)
           ENDIF
         ELSE
           SIGMA(i,j,k)=SIGMA(XDIM,j,k)
         ENDIF
        ENDDO
        ENDDO
       ENDDO

       DO k=-ISIGMA,ZDIM+ISIGMA
        DO i=-ISX,XDIM+ISX
        DO j=-1,-ISY,-1
         IF(ISOURCE.EQ.2) then
           SourceQ(i,j,k)=SourceQ(i,0,k)
           SourcePhi(i,j,k)=SourcePhi(i,0,k)
         ENDIF
         SIGMAEND=FAC1*OMEGA*EPSILON(k)*EPSILONNULL
c         IF(SourceQ(I,J,K).NE.0.AND.ISOURCE.EQ.2) SIGMAEND=0
         IF(SIGMAEND.GT.SIGMA(i,0,k)) THEN
           IF(MOD(IABSOR,2).NE.0) THEN
             SIGMA(i,j,k)=SIGMA(i,0,k)+(SIGMAEND-SIGMA(i,0,k))*
     &       ABS(j-0)/ISIGMA
           ELSEIF(MOD(IABSOR,2).EQ.0) THEN
             IF(SIGMA(i,0,k).LE.0) SIGMA(i,0,k)=0.001*SIGMAEND
             SIGMA(i,j,k)=SIGMA(i,0,k)*EXP(LOG(SIGMAEND/
     &       SIGMA(i,0,k))*ABS(j-0)/ISIGMA)
           ELSE
             SIGMA(i,j,k)=SIGMA(i,0,k)
           ENDIF
         ELSE
           SIGMA(i,j,k)=SIGMA(i,0,k)
         ENDIF
        ENDDO
        ENDDO
       ENDDO

       DO k=-ISIGMA,ZDIM+ISIGMA
        DO i=-ISX,XDIM+ISX
        DO j=YDim+1,YDim+ISY,1
         IF(ISOURCE.EQ.2) then
           SourceQ(i,j,k)=SourceQ(i,YDim,k)
           SourcePhi(i,j,k)=SourcePhi(i,YDim,k)
         ENDIF
         SIGMAEND=FAC1*OMEGA*EPSILON(k)*EPSILONNULL
c         IF(SourceQ(I,J,K).NE.0.AND.ISOURCE.EQ.2) SIGMAEND=0
         IF(SIGMAEND.GT.SIGMA(i,YDim,k)) THEN
           IF(MOD(IABSOR,2).NE.0) THEN
             SIGMA(i,j,k)=SIGMA(i,YDim,k)+(SIGMAEND-SIGMA(i,YDim,k))*
     &       ABS(j-YDim)/ISIGMA
           ELSEIF(MOD(IABSOR,2).EQ.0) THEN
             IF(SIGMA(i,YDim,k).LE.0) SIGMA(i,YDim,k)=0.001*SIGMAEND
             SIGMA(i,j,k)=SIGMA(i,YDim,k)*EXP(LOG(SIGMAEND/
     &       SIGMA(i,YDim,k))*ABS(j-YDim)/ISIGMA)
           ELSE
             SIGMA(i,j,k)=SIGMA(i,YDim,k)
           ENDIF
         ELSE
           SIGMA(i,j,k)=SIGMA(i,YDim,k)
         ENDIF
        ENDDO
        ENDDO
       ENDDO

       XDIM=XDIM+2*ISX
       YDIM=YDIM+2*ISY
       ZDIM=ZDIM+2*ISIGMA

       ALLOCATE (
     & sigmahx(0:XDIM,0:YDIM,0:ZDIM),
     & epsilonhx(0:XDIM,0:YDIM,0:ZDIM))
       DO k=0,ZDIM
         epsilonhx(0,0,k)=EPSILON(k-ISIGMA)
         DO j=0,YDIM
           DO i=0,XDIM
             SIGMAHX(i,j,k)=SIGMA(i-ISX,j-ISY,k-ISIGMA)
           ENDDO
         ENDDO
       ENDDO
       DEALLOCATE(EPSILON,SIGMA)
       ALLOCATE (
     &            SIGMA(0:XDIM,0:YDIM,0:ZDIM),
     &            EPSILON(0:ZDIM))
       DO k=0,ZDIM
         EPSILON(k)=epsilonhx(0,0,k)
         DO j=0,YDIM
           DO i=0,XDIM
             SIGMA(i,j,k)=SIGMAHX(i,j,k)
           ENDDO
         ENDDO
       ENDDO
       DEALLOCATE(epsilonhx,SIGMAHX)
c      ENDIF

      PRINT*,'Reallocate memory and set to 0'

      ISOURCEZ=(SOURCEZ-FDZMIN)/DELTAX+ISIGMA
      IF(ISOURCEZ.LT.ISIGMA) ISOURCEZ=ISIGMA
      IF(ISOURCEZ.GT.ZDIM-ISIGMA) ISOURCEZ=ZDIM-ISIGMA

C****************************************************************
C*                                                              *
C      SPEICHERBELEGUNG: Unterscheidung TEMODE 1/0		* 
C*     Nullsetzen der E- und H-Felder und der Felder		*
C*     sigmahx,sigmahy,sigmahz,					*
C*     muehx,muehy,muehz,       				* 
C*     epsilonhx,epsilonhy,epsilonhz                            *
C*                                                              *
C****************************************************************

C     E,H-Feldvariablen 2 Zeit-Dimensionen (0:1)
C	Nur notwendig, solange nicht alle Calc-Routinen umgestellt sind
C	Laut Ketelsen ist es (in einigen oder allen?) nicht notwendig
C	zwei Zeitfelder (0:1) zu haben, da z.B. EY(r,0) nur von EY(r,0) 
C	abhaengt, etc, also keine Ueberschneidung zustande kommt.
      IF(TwoOrThree.EQ.3) THEN
        OutVar_TDim=0     
      ELSE
        OutVar_TDim=0     
      ENDIF

      muehx=0
      muehy=0
      muehz=0

      ALLOCATE (sigmahx(0:XDIM,0:YDIM,0:ZDIM),
     &          sigmahy(0:XDIM,0:YDIM,0:ZDIM),
     &          sigmahz(0:XDIM,0:YDIM,0:ZDIM),
     &          epsilonhx(0:XDIM,0:YDIM,0:ZDIM),
     &          epsilonhy(0:XDIM,0:YDIM,0:ZDIM),
     &          epsilonhz(0:XDIM,0:YDIM,0:ZDIM))

      epsilonhx=0
      epsilonhy=0
      epsilonhz=0
      sigmahx=0
      sigmahy=0
      sigmahz=0

      IF(TwoOrThree.EQ.2.AND.TEMODE.EQ.1) THEN
        ALLOCATE (EY(0:XDIM,0:YDIM,0:ZDIM,0:OutVar_TDim),
     &            HX(0:XDIM,0:YDIM,0:ZDIM,0:OutVar_TDim),
     &            HZ(0:XDIM,0:YDIM,0:ZDIM,0:OutVar_TDim))
        DO k=0,ZDIM
         DO i=0,XDIM
          DO j=0,YDIM
           DO l=0,OutVar_TDim
             EY(i,j,k,0)=0
             HX(i,j,k,0)=0
             HZ(i,j,k,0)=0
           END DO
          END DO
         END DO
        END DO
      ELSE IF(TwoOrThree.EQ.1) THEN
        ALLOCATE (EY(0:XDIM,0:YDIM,0:ZDIM,0:OutVar_TDim),
     &            HX(0:XDIM,0:YDIM,0:ZDIM,0:OutVar_TDim))
        DO k=0,ZDIM
            EY(0,0,k,0)=0
            HX(0,0,k,0)=0
        END DO
      ELSE
        ALLOCATE (EX(0:XDIM,0:YDIM,0:ZDIM,0:OutVar_TDim),
     &            EY(0:XDIM,0:YDIM,0:ZDIM,0:OutVar_TDim),
     &            EZ(0:XDIM,0:YDIM,0:ZDIM,0:OutVar_TDim),
     &            HX(0:XDIM,0:YDIM,0:ZDIM,0:OutVar_TDim),
     &            HY(0:XDIM,0:YDIM,0:ZDIM,0:OutVar_TDim),
     &            HZ(0:XDIM,0:YDIM,0:ZDIM,0:OutVar_TDim))
        DO k=0,ZDIM
         DO i=0,XDIM
          DO j=0,YDIM
           DO l=0,OutVar_TDim
            EX(i,j,k,0)=0
            EY(i,j,k,0)=0
            EZ(i,j,k,0)=0
            HX(i,j,k,0)=0
            HY(i,j,k,0)=0
            HZ(i,j,k,0)=0
           END DO
          END DO
         END DO
        END DO
      ENDIF


C****************************************************************
C* NETCDF-PHYSIK-DATEI DEFINIEREN UND KOORDINATEN SCHREIBEN	*
        IF(IOUTFORM.EQ.0) THEN
          CALL PHYSNC
        ENDIF

C****************************************************************
C*                                                              *
C*     Umrechnung der Modellparameter in MKSA-System            *
C*                                                              *
C****************************************************************

      MUE=1.0
      MUE=MUE*RMUENULL

      DO k=0,ZDIM
         EPSILON(k)=EPSILON(k)*EPSILONNULL
      END DO

C********************************************************************
C*                                                                  *
C*  Berechnung von Hilfsfeldern, je nachdem, welche Dimension oder  *
C*  Randbedingung gewaehlt wird                                     *
C*                                                                  *
C********************************************************************

      PRINT*,'Calculate HELPFIELDS:'
      if (TwoOrThree.EQ.3) THEN
	 WRITE(*,401) 'CalculateHelpFieldsFor3D'
         CALL CalculateHelpFieldsFor3D
      ELSEif (TwoOrThree.EQ.2) THEN
	 WRITE(*,401) 'CalculateHelpFieldsFor2D'
         CALL CalculateHelpFieldsFor2D
      ELSEif (TwoOrThree.EQ.1) THEN
	 WRITE(*,401) 'CalculateHelpFieldsFor1D'
         CALL CalculateHelpFieldsFor2D	! Use same as for 2D until later
      ELSE
	 CALL HANDLE_ERR(9)
      ENDIF
      WRITE(*,'(A)') ' . . . done'

C********************************************************
C*                                                   	*
C*     DEALLOCATE von EPSILON und SIGMA,        	*
C*     die nicht mehr gebraucht werden               	*
C*     (Parameter ueber epsilonh* und sigmah*		*
C*                                                   	*
C********************************************************
      DEALLOCATE(EPSILON,SIGMA)

C*****************************************************************
C*                                                               *
C*    Berechnung einzelner Terme zur Vereinfachung der spaeteren *
C*    Berechnung der E.-M.-Felder in einer Zeitschleife          *
C*                                                               *
C*     GELTEN FUER ALLE BERECHNUNGEN; EGAL WELCHE DIMENSION;     *
C*     WELCHE ABLEITUNG UND WELCHE RANDBEDINGUNGEN               *
C*                                                               *
C*****************************************************************

      deltax=1/deltax
      deltax24=deltax/24
      deltay=1/deltay
      deltay24=deltay/24
      deltaz=1/deltaz
      deltaz24=deltaz/24

C*****************************************************************
C*                                                               *
C*     Hier beginnt die eigentliche Berechnung der e.-m. Felder  *
C*                                                               *
C*****************************************************************

      WRITE(*,'(T3,A,$)') 'CalcTimeIndepFieldsForO22andO24'
      CALL CalcTimeIndepFieldsForO22andO24 ! Zeitunabhaengige Felder
      WRITE(*,'(A)') ' . . . done'

C****************************************************************
C*     NETCDF-EINGABE LESEN, FALLS OPTION -I <NCEMINF>		*
C*     sonst 							*
C*     BERECHNUNG QUELLSIGNAL					*
C****************************************************************

      IF(IINITIN.EQ.1) THEN
        WRITE(*,401) 'Read EM field from netCDF file'
        CALL INFIELD 
        WRITE(*,'(A)') ' . . . done'
        WRITE(*,403) '    NCEMINF:',NCEMINF
      ELSE
        WRITE(*,'(T3,A,I4,A,$)') 
     &       'Calculate source signal: TXSIG (ISIGNAL =',ISIGNAL,')'
        CALL TXSIG ! Quellsignal
        WRITE(*,'(A)') ' . . . done'
        WRITE(*,403) '    Wavelet:',INWL
      ENDIF

C****************************************************************
C* NETCDF-EM-FELD-DATEI DEFINIEREN UND KOORDINATEN SCHREIBEN	*
        IF(IOUTFORM.EQ.0) THEN
          CALL EMNC
        ENDIF

C
C*    allocate memory for storing traces if outform=1||3
C
      IF(IOUTFORM.EQ.1.OR.IOUTFORM.EQ.3) THEN
         IF(IOUTEM(1).EQ.1) ALLOCATE (EMOUTEX(STMOD:ETMOD))
         IF(IOUTEM(2).EQ.1) ALLOCATE (EMOUTEY(STMOD:ETMOD))
         IF(IOUTEM(3).EQ.1) ALLOCATE (EMOUTEZ(STMOD:ETMOD))
         IF(IOUTEM(4).EQ.1) ALLOCATE (EMOUTHX(STMOD:ETMOD))
         IF(IOUTEM(5).EQ.1) ALLOCATE (EMOUTHY(STMOD:ETMOD))
         IF(IOUTEM(6).EQ.1) ALLOCATE (EMOUTHZ(STMOD:ETMOD))
         DO L=STMOD,ETMOD,1
           IF(IOUTEM(1).EQ.1) EMOUTEX(L)=0
           IF(IOUTEM(2).EQ.1) EMOUTEY(L)=0
           IF(IOUTEM(3).EQ.1) EMOUTEZ(L)=0
           IF(IOUTEM(4).EQ.1) EMOUTHX(L)=0
           IF(IOUTEM(5).EQ.1) EMOUTHY(L)=0
           IF(IOUTEM(6).EQ.1) EMOUTHZ(L)=0
         ENDDO
      ENDIF

C*******************************************************************************
C     Berechnung der Maxwell-Gleichungen in Zeitschleifen (l=STMOD,ETMOD)
      WRITE(*,'(A,A,I1,1X,I1,A,I2,A)') '0','FD-SCHEMA: O2',TwoOrFour,
     &		TwoOrThree,'D  (IFD=',IFD,')'


      IF ((TwoOrFour.EQ.2).and.(TwoOrThree.EQ.2)) THEN
         IF(TEMODE.EQ.1) THEN
 	   WRITE(*,402) 'CalculateForO22AbsRange2DTEMode'
C          CALL CalculateForO22AbsRange2DTEMode  ! O(2/2), Absorbing Range, 2-d
         ELSE
 	   WRITE(*,402) 'CalculateForO22AbsRange2D'
C          CALL CalculateForO22AbsRange2D  ! O(2/2), Absorbing Range, 2-d
         END IF
      ELSE IF ((TwoOrFour.EQ.4).and.(TwoOrThree.EQ.2)) THEN
         IF(TEMODE.EQ.1) THEN
 	   WRITE(*,402) 'CalculateForO24AbsRange2DTEMode'
           WRITE(*,402) '. . .'	! Leerzeile nach Kontrollausgabe '+'-time step
           CALL CalculateForO24AbsRange2DTEMode  ! O(2/4), Absorbing Range, 2-d
         ELSE
 	   WRITE(*,402) 'CalculateForO24AbsRange2D'
C          CALL CalculateForO24AbsRange2D  ! O(2/4), Absorbing Range, 2-d
         END IF
      ELSE IF ((TwoOrFour.EQ.2).and.(TwoOrThree.EQ.3)) THEN
	 WRITE(*,402) 'CalculateForO22AbsRange3D'
         WRITE(*,402) '. . .'	! Leerzeile nach Kontrollausgaben '+'-time step
         CALL CalculateForO22AbsRange3D  ! O(2/2), Absorbing Range, 3-d
      ELSE IF ((TwoOrFour.EQ.4).and.(TwoOrThree.EQ.3)) THEN
	 WRITE(*,402) 'CalculateForO24AbsRange3D'
         WRITE(*,402) '. . .'	! Leerzeile nach Kontrollausgaben '+'-time step
         CALL CalculateForO24AbsRange3D  ! O(2/4), Absorbing Range, 3-d
      ELSE IF ((TwoOrFour.EQ.4).and.(TwoOrThree.EQ.1)) THEN
	 WRITE(*,402) 'CalculateForO24AbsRange1DTEMode'
         WRITE(*,402) '. . .'	! Leerzeile nach Kontrollausgaben '+'-time step
         CALL CalculateForO24AbsRange1DTEMode  ! O(2/4), Absorbing Range, 1-d
      ENDIF

      WRITE(*,402) 'Finished FD calculations'

C**************************************************************
C*    Ausgabe des Feldes in Datei NCEMOUTF fuer Option -O

      IF (IINITOUT.EQ.1) THEN
        WRITE(*,401) 'Write EM field to netCDF file '
        CALL OUTFIELD
        WRITE(*,'(A)') ' . . . done'
        WRITE(*,403) '    NCEMOUTF:',NCEMOUTF
      ENDIF

C**************************************************************
C*
C*	Ausgabe der Spur in Datei
C*
C**************************************************************

C      IF(IOUTEM(1).EQ.1) THEN
C	DO L=STMOD,ETMOD
C	  WRITE(20,300) L*DELTAT, EMOUTEX(L)
C	ENDDO
C      ENDIF
C      IF(IOUTEM(2).EQ.1) THEN
C	DO L=STMOD,ETMOD
C	  !!! WRITE(21,300) L*DELTAT, EMOUTEY(L)
C	ENDDO
C      ENDIF
C      IF(IOUTEM(3).EQ.1) THEN
C	DO L=STMOD,ETMOD
C	  WRITE(22,300) L*DELTAT, EMOUTEZ(L)
C	ENDDO
C      ENDIF
C      IF(IOUTEM(4).EQ.1) THEN
C	DO L=STMOD,ETMOD
C	  WRITE(23,300) L*DELTAT, EMOUTHX(L)
C	ENDDO
C      ENDIF
C      IF(IOUTEM(5).EQ.1) THEN
C	DO L=STMOD,ETMOD
C	  WRITE(24,300) L*DELTAT, EMOUTHY(L)
C	ENDDO
C      ENDIF
C      IF(IOUTEM(6).EQ.1) THEN
C	DO L=STMOD,ETMOD
C	  WRITE(25,300) L*DELTAT, EMOUTHZ(L)
C	ENDDO
C      ENDIF
  300 FORMAT(E12.6,1X,E12.6)

C*******************************************************************************
C     BERECHNUNG UND AUSGABE RECHENZEIT
#ifndef NEC
      fdate_end = fdate()			! SGI
      sece=second()				! SGI
      timee=secondr()				! SGI
      WRITE(*,*) 'USER TIME',timea,timee	! SGI
#else
      CALL cpu_time(sece)			! SX-6
      CALL DATIM(fdate_date,fdate_time,4)	! SX-6
      fdate_end = fdate_date//' '//fdate_time	! SX-6
#endif

      WRITE(*,*) 'DONE WITH RUN ',INFDR  
      WRITE(*,*) 'FINISHED AT ',fdate_end
      WRITE(*,'(A,I2,A,I2,A,I2)') ' CPU TIME: ',
     &INT((sece-seca)/3600),':',
     &INT((sece-seca)/60)-INT((sece-seca)/3600)*60,':',
     &INT(sece-seca)-INT((sece-seca)/60)*60

C*******************************************************************************
C ENDE DER AUSGABE: ALLE DATEIEN SCHLIESSEN
      IF (IOUTFORM.EQ.0) THEN
       CALL chk_nf(NF90_REDEF(EM_nc))
       CALL chk_nf(NF90_PUT_ATT(EM_nc,NF90_GLOBAL, "history_end",
     &                          fdate_end ) )
       CALL chk_nf(NF90_ENDDEF(EM_nc))
       CALL chk_nf(NF90_CLOSE(EM_nc))

      ELSE IF (MOD(IOUTFORM,10).NE.0) THEN
       IF(IOUTEM(1).EQ.1) CLOSE(20,STATUS='KEEP')
       IF(IOUTEM(2).EQ.1) CLOSE(21,STATUS='KEEP')
       IF(IOUTEM(3).EQ.1) CLOSE(22,STATUS='KEEP')
       IF(IOUTEM(4).EQ.1) CLOSE(23,STATUS='KEEP')
       IF(IOUTEM(5).EQ.1) CLOSE(24,STATUS='KEEP')
       IF(IOUTEM(6).EQ.1) CLOSE(25,STATUS='KEEP')
       IF(IOUTFORM.EQ.3) THEN
        IF(IOUTEM(1).EQ.1) CLOSE(30,STATUS='KEEP')
        IF(IOUTEM(2).EQ.1) CLOSE(31,STATUS='KEEP')
        IF(IOUTEM(3).EQ.1) CLOSE(32,STATUS='KEEP')
        IF(IOUTEM(4).EQ.1) CLOSE(33,STATUS='KEEP')
        IF(IOUTEM(5).EQ.1) CLOSE(34,STATUS='KEEP')
        IF(IOUTEM(6).EQ.1) CLOSE(35,STATUS='KEEP')
       ENDIF
      ENDIF
      CLOSE(40,STATUS='KEEP')

C*******************************************************************************

  401 FORMAT(T3,A,$)	! 3 Abstand, String, kein Zeilenumbruch
  402 FORMAT(' ',T3,A)	! 4 Abstand, String, Zeilenumbruch
  403 FORMAT(T3,A,A)	! 3 Abstand, 2 String, Zeilenumbruch

 5001 FORMAT(2F15.6,I5/,I10,5X,E15.4)
 5002 FORMAT(16I5)
 5003 FORMAT(F15.5)

      END SUBROUTINE MAINEXE
