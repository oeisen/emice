C*******************************************************************
C*                                                                 *
C* SUBROUTINE zur Berechnung des O(2/4)-Falles in 1D mit Abs Range *
C*                                                                 *
C*******************************************************************

C*********************** RCS-KEYWORDS ******************************
C $Source: /pf/a/a270012/fdem_src/RCS/CalculateForO24AbsRange1DTEMode.f,v $
C $Author: a270012 $
C $Date: 2002/06/26 13:39:26 $
C $RCSfile: CalculateForO24AbsRange1DTEMode.f,v $
C $Revision: 5.0 $
C $State: Exp $
C*********************** RCS-KEYWORDS ******************************

      SUBROUTINE CalculateForO24AbsRange1DTEMode
      USE dynfelder_mod

C     1-dimensionale Berechnung des O(2/4)-Falles mit
C     absorbierenden R„ndern (d/dx,d/dy=0)
C     Nur HX=f(HX,EY) und EY=f(HX,EY) bleiben uebrig
C     (Hier abgeleitet vom 2D O24-Fall.)

      DO L=STMOD,ETMOD ! Zeitschleife

#ifndef OPERATION
#ifndef NEC
      sec0=second()						! SIG
#else
      CALL CPU_TIME(sec0)					! SX-6
#endif
#endif

      CALL QUELLE(l) ! Quellterme werden beim l-ten Zeitschritt beruecksichtigt

C******************************************************************
C     BEGINN DER H-FELD BERECHNUNGEN 
C     Aufteilung in verschiedene Schleifen wegen Unterscheidung 
C     O(2,2) und O(2,4) in Randnaehe.
C     Indizes: i=0,j=0,z=0,ZDIM
C
C     Fuer x und y gilt immer:

      j=YDIM ! j=0
      i=XDIM ! i=0

C     Berechnung der H-Felder fuer k=1,(ZDIM-2)
C     In diesem Bereich kann O(2/4)-Berechnung fuer alle Koordinaten
C     durchgefuehrt werden

!$OMP PARALLEL DO private(k)
      DO k=1,(ZDIM-2)

         HX(i,j,k+1,0)=HX(i,j,k+1,0)+muehx*
     &   (EY(i,j,k-1,0)-27*EY(i,j,k,0)+27*EY(i,j,k+1,0)-
     &    EY(i,j,k+2,0))*deltaz24

      END DO
 
C     Berechnung der H-Felder fuer k=0
C     Es wird wie beim O(2/2)-Fall gerechnet, da keine Koordinaten
C     k-1 existieren

       k=0

         HX(i,j,k+1,0)=HX(i,j,k+1,0)+muehx*
     &              (EY(i,j,k+1,0)-EY(i,j,k,0))*deltaz
 
C     Berechnung der H-Felder fuer k=ZDIM-1
C     Es wird wie beim O(2/2)-Fall gerechnet, da keine Koordinaten
C     i+2 oder k+2 existieren

       k=ZDIM-1

         HX(i,j,k+1,0)=HX(i,j,k+1,0)+muehx*
     &              (EY(i,j,k+1,0)-EY(i,j,k,0))*deltaz


C     Berechnung der H-Felder fuer k=ZDIM
C     Es wird wie beim O(2/2)-Fall gerechnet, da keine Koordinaten
C     k+2 existieren

      k=ZDIM

C        HX(i,j,k+1,0)=HX(i,j,k+1,0)+muehx*
C     &              (EY(i,j,k+1,0)-EY(i,j,k,0))*deltaz

C     ENDE DER H-FELD BERECHNUNGEN 
C******************************************************************
C     BEGINN DER E-FELD BERECHNUNGEN 
C
C     Fuer x und y gilt immer:

      j=YDIM ! j=0
      i=XDIM ! i=0

C     Berechnung der E-Felder fuer k=1,(ZDIM-2)
C
C     In diesem Bereich kann O(2/4)-Berechnung fuer alle Koordinaten
C     durchgefuehrt werden
C
!$OMP PARALLEL DO private(k)
       DO k=1,(ZDIM-2)
C
C        EY wird berechnet:
C
         EY(i,j,k,0)=epsilonhy(i,j,k)*EY(i,j,k,0)+
     &              sigmahy(i,j,k)*
     &                 ((HX(i,j,k-1,0)-27*HX(i,j,k,0)+
     &                27*HX(i,j,k+1,0)-HX(i,j,k+2,0))*deltaz24)
       END DO
C
C     Berechnung der E-Felder fuer k=0
C
C     Es wird wie beim O(2/2)-Fall gerechnet, da keine Koordinaten
C     i-1 oder k-1 existieren
C
       k=0

         EY(i,j,k,0)=epsilonhy(i,j,k)*EY(i,j,k,0)+
     &              sigmahy(i,j,k)*
     &                 ((HX(i,j,k+1,0)-HX(i,j,k,0))*deltaz)

C     Berechnung der E-Felder fuer k=ZDIM-1
C     Es wird wie beim O(2/2)-Fall gerechnet, da keine Koordinaten
C     i+2 oder k+2 existieren

       k=ZDIM-1
C
C        EY wird berechnet:
C
         EY(i,j,k,0)=epsilonhy(i,j,k)*EY(i,j,k,0)+
     &              sigmahy(i,j,k)*
     &                 ((HX(i,j,k+1,0)-HX(i,j,k,0))*deltaz)

C     Berechnung der E-Felder fuer k=ZDIM
C     Es wird wie beim O(2/2)-Fall gerechnet, da keine Koordinaten
C     i+2 oder k+2 existieren
C     Es werden allerdings einseitige Ableitungen gebildet, da k=ZDIM

       k=ZDIM

         EY(i,j,k,0)=epsilonhy(i,j,k)*EY(i,j,k,0)+
     &              sigmahy(i,j,k)*
     &                 ((HX(i,j,k,0)-HX(i,j,k-1,0))*deltaz)

C     ENDE DER E-FELD BERECHNUNGEN 
C
C******************************************************************
C     UEBERGABE DER FELDER FUER NEUEN ZEITSCHRITT: 
C           E,H(*,*,*,1) ==> E,H(*,*,*,0)
C******************************************************************

C     j=YDIM ! j=0
C      DO k=1,ZDIM
C       DO i=1,XDIM-2
C 	 HX(i,j,k,0)=HX(i,j,k,1)
C	 HZ(i,j,k,0)=HZ(i,j,k,1)
C	 EY(i,j,k,0)=EY(i,j,k,1)
C       END DO
C      END DO

C     Speichern der E/H-Felder (Format je nach IOUTFORM)
      CALL OUTTRACE(l)

#ifndef OPERATION
C     Rechenzeit fuer letzten Zeitschritt:
#ifndef NEC
      sec=second()						! SGI
#else
      CALL CPU_TIME(sec)					! SX-6
#endif
      PRINT 398, l-STMOD+1,TDIM,sec-sec0
#endif

      END DO ! Der Zeitschleife

  398 FORMAT ( "+ time step ",I6,' of ',I6,' finished within ',F5.2,'s')

C     2-dimensionale Berechnung des O(2/4)-Falles mit absorbierenden R„ndern
      END ! SUBROUTINE CalculateForO24AbsRange1DTEMode
