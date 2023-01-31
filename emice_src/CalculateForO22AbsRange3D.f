C*******************************************************************
C*                                                                 *
C* SUBROUTINE zur Berechnung des O(2/2)-Falles in 3D mit Abs Range *
C*                                                                 *
C*******************************************************************

C*********************** RCS-KEYWORDS ******************************
C $Source: /pf/a/a270012/fdem_src/RCS/CalculateForO22AbsRange3D.f,v $
C $Author: a270012 $
C $Date: 2002/06/26 13:39:26 $
C $RCSfile: CalculateForO22AbsRange3D.f,v $
C $Revision: 5.0 $
C $State: Exp $
C*********************** RCS-KEYWORDS ******************************


      SUBROUTINE CalculateForO22AbsRange3D
      USE dynfelder_mod

C     Berechnung des 3-dimensionalen O(2,2)-Falles mit absorbierenden
C     Raendern
C     Im Gegensatz zum 2-D-Fall wird die Berechnung im 3-D-Fall nur
C     in dem Bereich durchgefuehrt, in dem man keine einseitigen
C     Ableitungen oder aehnlich Kunstgriffe durchfuehren muss, um die
C     Berechnung zu erm�glichen. Deshalb gilt i=0,(XDIM-1), j=0,(YDIM-1)
C     und k=0,(ZDIM-1). Es wird also im absorbierenden Rand teilweise
C     nicht bis zu einer Stelle 2*lamda weitergerechnet, sondern eine
C     Stelle vorher aufgehoert. Das macht aber nichts, da man sich im
C     absorbierenden Rand befindet und nicht im eigentlichen Modellgebiet.

      PRINT*,'Model dimensions in points: X:',
     &             XDIM,'Y:',YDIM,'Z:',ZDIM,'T:',TDIM

C$    threads=OMP_GET_NUM_THREADS()			
C$    PRINT*,'OMP_NUM_THREADS:',threads		
C$    PRINT*,' '

C$OMP PARALLEL PRIVATE (sec , sec0)


      DO l=STMOD,ETMOD
#ifndef NEC
      sec0=second()					! SGI
#else
      CALL CPU_TIME(sec0)				! SX-6
#endif
C$OMP SINGLE
      CALL QUELLE(l) ! Quellterme werden beim l-ten Zeitschritt 
C                     < TSIGNAL beruecksichtigt

C$OMP END SINGLE
C$OMP SECTIONS
C$OMP SECTION
C
C     HX wird berechnet:
C
       DO k=0,(ZDIM-1)
        DO j=0,(YDIM-1)
         DO i=0,(XDIM-1)
           HX(i,j+1,k+1,1)=HX(i,j+1,k+1,0)+muehx*
     &              ((EY(i,j+1,k+1,0)-EY(i,j+1,k,0))*deltaz-
     &               (EZ(i,j+1,k+1,0)-EZ(i,j,k+1,0))*deltay)
           HX(i,j,k,0)=HX(i,j,k,1)
         END DO
        END DO
       END DO
C
C     HY wird berechnet:
C
C$OMP SECTION
       DO k=0,(ZDIM-1)
        DO j=0,(YDIM-1)
         DO i=0,(XDIM-1)
           HY(i+1,j,k+1,1)=HY(i+1,j,k+1,0)+muehy*
     &              ((EZ(i+1,j,k+1,0)-EZ(i,j,k+1,0))*deltax-
     &               (EX(i+1,j,k+1,0)-EX(i+1,j,k,0))*deltaz)
           HY(i,j,k,0)=HY(i,j,k,1)
         END DO
        END DO
       END DO
C
C     HZ wird berechnet:
C
C$OMP SECTION
       DO k=0,(ZDIM-1)
        DO j=0,(YDIM-1)
         DO i=0,(XDIM-1)
           HZ(i+1,j+1,k,1)=HZ(i+1,j+1,k,0)+muehz*
     &              ((EX(i+1,j+1,k,0)-EX(i+1,j,k,0))*deltay-
     &               (EY(i+1,j+1,k,0)-EY(i,j+1,k,0))*deltax)
           HZ(i,j,k,0)=HZ(i,j,k,1)
         END DO
        END DO
       END DO
C$OMP END SECTIONS

C barrier, such that Hy,Hz is ready for Ex in subseq. part
C$OMP BARRIER

C$OMP SECTIONS
C
C     EX wird berechnet:
C
C$OMP SECTION
       DO k=0,(ZDIM-1)
        DO j=0,(YDIM-1)
         DO i=0,(XDIM-1)
           EX(i+1,j,k,1)=epsilonhx(i+1,j,k)*EX(i+1,j,k,0)+
     &              sigmahx(i+1,j,k)*
     &                 ((HZ(i+1,j+1,k,1)-HZ(i+1,j,k,1))*deltay-
     &                  (HY(i+1,j,k+1,1)-HY(i+1,j,k,1))*deltaz)
           EX(i,j,k,0)=EX(i,j,k,1)
         END DO
        END DO
       END DO
C
C     EY wird berechnet:
C
C$OMP SECTION
       DO k=0,(ZDIM-1)
        DO j=0,(YDIM-1)
         DO i=0,(XDIM-1)
           EY(i,j+1,k,1)=epsilonhy(i,j+1,k)*EY(i,j+1,k,0)+
     &              sigmahy(i,j+1,k)*
     &                 ((HX(i,j+1,k+1,1)-HX(i,j+1,k,1))*deltaz-
     &                  (HZ(i+1,j+1,k,1)-HZ(i,j+1,k,1))*deltax)
           EY(i,j,k,0)=EY(i,j,k,1)
         END DO
        END DO
       END DO
C
C     EZ wird berechnet:
C
C$OMP SECTION
       DO k=0,(ZDIM-1)
        DO j=0,(YDIM-1)
         DO i=0,(XDIM-1)
           EZ(i,j,k+1,1)=epsilonhz(i,j,k+1)*EZ(i,j,k+1,0)+
     &              sigmahz(i,j,k+1)*
     &                 ((HY(i+1,j,k+1,1)-HY(i,j,k+1,1))*deltax-
     &                  (HX(i,j+1,k+1,1)-HX(i,j,k+1,1))*deltay)
           EZ(i,j,k,0)=EZ(i,j,k,1)
         END DO
        END DO
       END DO

C$OMP END SECTIONS
C$OMP BARRIER
C$OMP SINGLE

C      Ausgabe auf Profillinie falls ioutform auf 1 gesetzt
C      IF(IOUTFORM.EQ.1.OR.IOUTFORM.EQ.3) THEN
C         CALL OUTLINE
C      ENDIF

C     Speichern der E/H-Werte am Ort des RX:
      CALL OUTTRACE(L)

#ifndef NEC
      sec=second()					! SGI
#else
      CALL CPU_TIME(sec)				! SX-6
#endif
      PRINT 398, l,TDIM,sec-sec0

C$OMP END SINGLE
      END DO ! Der Zeitschleife
C$OMP END PARALLEL

  398 FORMAT ( "+ time step ",I6,' of ',I6,' finished within ',F5.2,'s')

C     Ende der SUBROUTINE CalculateForO22AbsRange3D
C     3-dimensionale Berechnung des O(2/2)-Falles mit absorbierenden R�ndern
      END