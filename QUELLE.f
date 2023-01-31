
C*******************************************************************
C*                                                                 *
C* SUBROUTINE, in der die QUELLE realisiert wird                   *
C*                                                                 *
C*******************************************************************

C*********************** RCS-KEYWORDS ******************************
C $Source: /pf/a/a270012/fdem_src/RCS/QUELLE.f,v $
C $Author: a270012 $
C $Date: 2002/06/26 13:39:26 $
C $RCSfile: QUELLE.f,v $
C $Revision: 5.0 $
C $State: Exp $
C*********************** RCS-KEYWORDS ******************************


      SUBROUTINE QUELLE (l)
      USE dynfelder_mod

      INTEGER l
      
C     PRINT*,'SUBROUTINE QUELLE'

C		#ifndef NEC
C***************************************************
C     ANREGUNG FELD DURCH QUELLSIGNAL
C***************************************************
      IF(l.LE.LSIGNAL) THEN
C      PRINT*,'Quellinitialisierung ISIGNAL =',ISIGNAL,' l =',L

C*******************************************************************
C     Eigentliche Anregung des EM-Feldes durch Quelle
C*******************************************************************

	   WRITE(40,311) EY(IA,YDIM/2,ISOURCEZ,0)
	   WRITE(40,313) HX(IA,YDIM/2,ISOURCEZ,0)
	   WRITE(40,315) HZ(IA,YDIM/2,ISOURCEZ,0)
	   IF (TEMODE.NE.1) THEN
	    WRITE(40,310) EX(IA,YDIM/2,ISOURCEZ,0)
	    WRITE(40,312) EZ(IA,YDIM/2,ISOURCEZ,0)
	    WRITE(40,314) HY(IA,YDIM/2,ISOURCEZ,0)
	   ENDIF	! TEMODE
	   WRITE(40,330) SIGNAL(l)

       DO j=JA,JE	! Quellposition x
        DO i=IA,IE	! Quellposition y
         k=ISOURCEZ
C ???         IF(K.LT.TOPGRID(I-ISIGMA)+ISIGMA) 
C ???   &             K=TOPGRID(I-ISIGMA)+ISIGMA
         DO i1=0,0	! ???
          DO k1=0,0	! ???
           Fac=0.5
           IF(I1.EQ.-1.AND.K1.EQ.-1) Fac = 0.17
           IF(I1.EQ.1.AND.K1.EQ.1) Fac = 0.17
           IF(I1.EQ.1.AND.K1.EQ.-1) Fac = 0.17
           IF(I1.EQ.-1.AND.K1.EQ.1) Fac = 0.17
           IF(I1.EQ.0.AND.K1.EQ.0) Fac = 1
C	   PRINT*,'quellgeo:        ',IA,IE,JA,JE,i,j,k

           IF(ISIGNAL.GE.10) THEN	! Signal ueberschreibt bestehende Werte
             IF(ISOURCEEY.EQ.1) EY(i+i1,j,k+k1,0)=SIGNAL(l)*Fac
             IF(ISOURCEEX.EQ.1) EX(i+i1,j,k+k1,0)=SIGNAL(l)*Fac
             IF(ISOURCEEZ.EQ.1) EZ(i+i1,j,k+k1,0)=SIGNAL(l)*Fac
             IF(ISOURCEHY.EQ.1) HY(i+i1,j,k+k1,0)=SIGNAL(l)*Fac
             IF(ISOURCEHX.EQ.1) HX(i+i1,j,k+k1,0)=SIGNAL(l)*Fac
             IF(ISOURCEHZ.EQ.1) HZ(i+i1,j,k+k1,0)=SIGNAL(l)*Fac
	   ELSE			! Signal zu bestehenden Werten addieren (phys. korrekt!)
            IF(ISOURCEEY.EQ.1) EY(i+i1,j,k+k1,0)=EY(i+i1,j,k+k1,0)
     &            +SIGNAL(l)*Fac
            IF(ISOURCEEX.EQ.1) EX(i+i1,j,k+k1,0)=EX(i+i1,j,k+k1,0)
     &            +SIGNAL(l)*Fac
            IF(ISOURCEEZ.EQ.1) EZ(i+i1,j,k+k1,0)=EZ(i+i1,j,k+k1,0)
     &            +SIGNAL(l)*Fac
            IF(ISOURCEHY.EQ.1) HY(i+i1,j,k+k1,0)=HY(i+i1,j,k+k1,0)
     &            +SIGNAL(l)*Fac
            IF(ISOURCEHX.EQ.1) HX(i+i1,j,k+k1,0)=HX(i+i1,j,k+k1,0)
     &            +SIGNAL(l)*Fac
            IF(ISOURCEHZ.EQ.1) HZ(i+i1,j,k+k1,0)=HZ(i+i1,j,k+k1,0)
     &            +SIGNAL(l)*Fac
	   ENDIF	! ISIGNAL
          END DO  ! k1	! ???
         END DO	  ! i1	! ???

C	Werte auf 0 zur Unterdrueckung des Gleichstromanteils (nach Wavlet)
C	IF(l.EQ.LSIGNAL.AND.TwoOrThree.EQ.4) THEN
C	PRINT*,'UNTERDRUECKUNG DC'
C         DO i1=-6,6	
C          DO j1=-6,6	
C           DO k1=-6,6
C             EY(i+i1,j+j1,k+k1,0)=0
C             EX(i+i1,j+j1,k+k1,0)=0
C             EZ(i+i1,j+j1,k+k1,0)=0
C             HY(i+i1,j+j1,k+k1,0)=0
C             HX(i+i1,j+j1,k+k1,0)=0
C             HZ(i+i1,j+j1,k+k1,0)=0
C           END DO ! k1	
C          END DO  ! j1	
C         END DO	  ! i1
C	ENDIF

C        IF(ISOURCEEY.EQ.1) THEN
C         EY(i,j,k,1)=EY(i,j,k,1)+SIGNAL(l)
C         EY(i-1,j,k-1,1)=EY(i-1,j,k-1,1)+SIGNAL(l)*0.17
C         EY(i-1,j,k,1)=EY(i-1,j,k,1)+SIGNAL(l)*0.5
C         EY(i-1,j,k+1,1)=EY(i-1,j,k+1,1)+SIGNAL(l)*0.17
C         EY(i,j,k-1,1)=EY(i,j,k-1,1)+SIGNAL(l)*0.5
C         EY(i,j,k,1)=EY(i,j,k,1)+SIGNAL(l)*1.0
C         EY(i,j,k+1,1)=EY(i,j,k+1,1)+SIGNAL(l)*0.5
C         EY(i+1,j,k-1,1)=EY(i+1,j,k-1,1)+SIGNAL(l)*0.17
C         EY(i+1,j,k,1)=EY(i+1,j,k,1)+SIGNAL(l)*0.5
C         EY(i+1,j,k+1,1)=EY(i+1,j,k+1,1)+SIGNAL(l)*0.17
C        ENDIF	! ISOURCEEY
        END DO	! i Quellposition x
       END DO	! j Quellposition y
	   WRITE(40,321) EY(IA,YDIM/2,ISOURCEZ,0)
	   WRITE(40,323) HX(IA,YDIM/2,ISOURCEZ,0)
	   WRITE(40,325) HZ(IA,YDIM/2,ISOURCEZ,0)
	   IF (TEMODE.NE.1) THEN
	    WRITE(40,320) EX(IA,YDIM/2,ISOURCEZ,0)
	    WRITE(40,322) EZ(IA,YDIM/2,ISOURCEZ,0)
	    WRITE(40,324) HY(IA,YDIM/2,ISOURCEZ,0)
           ENDIF	! TEMODE


      ENDIF 	! l < LSIGNAL


  310 FORMAT('        EX  vor:',E12.6)
  311 FORMAT('        EY  vor:',E12.6)
  312 FORMAT('        EZ  vor:',E12.6)
  313 FORMAT('        HX  vor:',E12.6)
  314 FORMAT('        HY  vor:',E12.6)
  315 FORMAT('        HZ  vor:',E12.6)
  
  320 FORMAT('        EX nach:',E12.6)
  321 FORMAT('        EY nach:',E12.6)
  322 FORMAT('        EZ nach:',E12.6)
  323 FORMAT('        HX nach:',E12.6)
  324 FORMAT('        HY nach:',E12.6)
  325 FORMAT('        HZ nach:',E12.6)
  330 FORMAT('         SIGNAL:',E12.6)

C		#endif
      END SUBROUTINE QUELLE


C*******************************************************************
C*                                                                 *
C* SUBROUTINE BERECHNUNG QUELLSIGNAL UND QUELLGEOMETRIE		   *
C*                                                                 *
C*******************************************************************

      SUBROUTINE TXSIG
      USE dynfelder_mod

      INTEGER FACTOR
      REAL FACTOR1,TSIG,ActPhase,TACT
      REAL WL(:),WLTIME(:),WLlen,LTIME
      INTEGER pp,WLno	! Stuetzstellen des Wavelets
 
      ALLOCATABLE :: WL,WLTIME 

C***********************************************************
C* BERECHNUNG QUELLSIGNAL 				   *
C***********************************************************

      IF(ISOURCE.EQ.1.OR.ISOURCE.EQ.2) THEN 
         FACTOR=2048
      ELSE
         FACTOR=8192
      ENDIF

      IF(MOD(ISIGNAL,10).EQ.1) THEN
        LSIGNAL=TSIGNAL/DELTAT
      ELSE IF(MOD(ISIGNAL,10).EQ.2) THEN
        LSIGNAL=TSIGNAL/DELTAT
      ELSE IF(MOD(ISIGNAL,10).EQ.3) THEN
        LSIGNAL=TSIGNAL/DELTAT
      ELSE IF(MOD(ISIGNAL,10).EQ.4) THEN
        INWL='./wavelets/cmp5dintA.wl' 
C        print*,' '
C        print*,' WAVELET:', INWL
        OPEN(30,FILE=INWL,STATUS='OLD')
	READ(30,501) WLno,WLlen
        LSIGNAL=WLlen/DELTAT
        ALLOCATE(WL(1:WLno),WLTIME(1:WLno))
        READ(30,504) (WLTIME(pp),WL(pp),pp=1,WLno)
        CLOSE(30,STATUS='KEEP')
      ELSE 	! 0 oder andere Werte = default
C        print*,' '
C        print*,' WAVELET:', INWL
        OPEN(30,FILE=INWL,STATUS='OLD')
	READ(30,501) WLno,WLlen
        LSIGNAL=WLlen/DELTAT
        ALLOCATE(WL(1:WLno),WLTIME(1:WLno))
        READ(30,504) (WLTIME(pp),WL(pp),pp=1,WLno)
        CLOSE(30,STATUS='KEEP')
      ENDIF

      ALLOCATE(SIGNAL(1:LSIGNAL))

      DO l=1,LSIGNAL
       IF(MOD(ISIGNAL,10).EQ.1) THEN
C       ***  SINUS MIT ABLEITUNGEN = 0 ZU BEGINN UND ENDE: ***
        SIGNAL(l)=(FACTOR)*(SIN(PI2*FLOAT(L)*DeltaT/TSIGNAL)-
     &         0.5*SIN(2.*PI2*FLOAT(L)*DeltaT/TSIGNAL))
C       *** REINER SINUS ***
C        SIGNAL(l)=(FACTOR)*SIN(PI2*FLOAT(l)*DeltaT/TSIGNAL)
       ELSE IF(MOD(ISIGNAL,10).EQ.2) THEN
C       *** RICKER-WAVELET ***
        TSIG=0.25*TSIGNAL
        TSIG=TSIG*TSIG
        FACTOR1=SQRT(TSIG*1.35914)
        TACT=FLOAT(L-(LSIGNAL / 2))*DELTAT
        SIGNAL(l)=-2*FACTOR*FACTOR1*TACT/TSIG
        SIGNAL(l)=SIGNAL(l)*EXP(-TACT*TACT/TSIG)
       ELSE IF(MOD(ISIGNAL,10).EQ.3) THEN
C       *** FUCHS-MUELLER - pi4 PHASENVERSCHIEBUNG ***
c       ActPhase=-3.14*0.25
c       SIGNAL(l)=(FACTOR)*(SIN(ActPhase+
c    &        PI2*FLOAT(L)*DeltaT/TSIGNAL)-
c    &        0.5*SIN(ActPhase+2.*PI2*FLOAT(L)*DeltaT/TSIGNAL))
             CALL HANDLE_ERR(7) 
       ELSE 	! ISIGNAL.NE.1,2,3 oder -s ... : default
  		! EINLESEN DES EXTERNEN WAVELETS UND 
 		! INTERPOLATION AUF ZEITSCHRITT DELTAT
         LTIME=(l-1)*DELTAT
         IF(LTIME.GT.WLTIME(WLno)) 	THEN
		SIGNAL(l)=0
         ELSE IF(LTIME.EQ.WLTIME(WLno))	THEN
		SIGNAL(l)=WL(WLno)
         ELSE IF(LTIME.LT.WLTIME(WLno)) THEN
          DO m=1,WLno-1
           IF(LTIME.GE.WLTIME(m).AND.LTIME.LT.WLTIME(m+1))
     &       SIGNAL(l)=(WL(m+1)-WL(m))/(WLTIME(m+1)-WLTIME(m))
     &                *(LTIME-WLTIME(m))+WL(m)
          END DO	! m
         ELSE 
             CALL HANDLE_ERR(6) 
         ENDIF
	 SIGNAL(L)=FACTOR*SIGNAL(L)
       ENDIF 	! ISIGNAL
C      print*,l,(l-1)*DELTAT,SIGNAL(L)
      END DO ! l

  501 FORMAT(12X,I8,8x,E12.5)
  504 FORMAT(9X,E12.5,1X,E12.5)

C***************************************************
C     BERECHNUNG QUELLGEOMETRIE
C     IA,IE: X-Komponenten 
C     JA,JE: Y-Komponenten
C***************************************************

      IF(ISOURCE.EQ.1) THEN  	 ! plane wave
         IA=0
         IE=XDIM
         JA=0
         JE=YDIM
      ELSE IF(ISOURCE.EQ.2) THEN ! exploding reflector
         IA=ISX
         IE=XDIM-ISX
         JA=ISY
         JE=YDIM-ISY
      ELSE IF(ISOURCE.EQ.0) THEN ! point source

C	 Quellgeometrie X-Richtung:
         IA=(SOURCEX-FDXMIN)*DELTAX+ISX
         IE=IA
C	 Ausdehnung ueber 7 Modellpunkte:
	 IF ( XDIM.NE.0 ) THEN
C          IA=IA-3
C          IE=IE+3
         ENDIF
C			Abfrage, ob X-Quelle innerhalb Rand:
         IF(IA.LT.ISX) IA=ISX
         IF(IA.GT.XDIM-ISX) IA=XDIM-ISX
         IF(IE.LT.ISX) IE=ISX
         IF(IE.GT.XDIM-ISX) IE=XDIM-ISX

C	 Quellgeometrie Y-Richtung:
         JA=YDIM / 2
         JE=JA
C	 Ausdehnung ueber 7 Modellpunkte:
	 IF ( YDIM.NE.0 ) THEN
C          JA=JA-3
C          JE=JE+3
	 ENDIF
C			Abfrage, ob Y-Quelle innerhalb Rand:
         IF(JA.LT.ISY) JA=ISY
         IF(JA.GT.YDIM-ISY) JA=YDIM-ISY
         IF(JE.LT.ISY) JE=ISY
         IF(JE.GT.YDIM-ISY) JE=YDIM-ISY
      ENDIF 	! IF ISOURCE (QUELLGEOMETRIE)

      END SUBROUTINE TXSIG
