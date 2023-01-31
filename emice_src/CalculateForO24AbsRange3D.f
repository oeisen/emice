
C*******************************************************************
C*                                                                 *
C* SUBROUTINE zur Berechnung des O(2/4)-Falles in 3D mit Abs Range *
C*                                                                 *
C*******************************************************************

C*********************** RCS-KEYWORDS ******************************
C $Source: /pf/a/a270012/fdem_src/RCS/CalculateForO24AbsRange3D.f,v $
C $Author: a270012 $
C $Date: 2002/06/26 13:39:26 $
C $RCSfile: CalculateForO24AbsRange3D.f,v $
C $Revision: 5.0 $
C $State: Exp $
C*********************** RCS-KEYWORDS ******************************


      SUBROUTINE CalculateForO24AbsRange3D
      USE dynfelder_mod
 
 
C     Berechnung des 3-dimensionalen O(2,4)-Falles mit absorbierenden
C     Raendern
C     Im Gegensatz zum 2-D-Fall wird die Berechnung im 3-D-Fall nur
C     in dem Bereich durchgefuehrt, in dem man keine einseitigen
C     Ableitungen oder aehnlich Kunstgriffe durchfuehren muss, um die
C     Berechnung zu erm”glichen. Deshalb gilt i=1,(XDIM-2), j=1,(YDIM-2)
C     und k=1,(ZDIM-2). Es wird also im absorbierenden Rand teilweise
C     nicht bis zu einer Stelle 2*lamda weitergerechnet, sondern an
C     entsprechend frueher kommenden Stellen aufgehoert. Das macht aber
C     nichts, da man sich im absorbierenden Rand befindet und nicht im
C     eigentlichen Modellgebiet.

C     PRINT*,' CalculateForO24AbsRange3D:'
      PRINT*,'Model dimensions in points: X:',
     &             XDIM,'Y:',YDIM,'Z:',ZDIM,'T:',TDIM
      PRINT*,' '	

CC$    threads = OMP_GET_NUM_THREADS()				
CC$    PRINT*,'OMP_NUM_THREADS:',threads			
CC$    PRINT*,' '	


      DO l=STMOD,ETMOD
#ifndef OPERATION
#ifndef NEC      
      sec0=second()						! SGI
#else
      CALL CPU_TIME(sec0)                                	! SX-6
#endif
#endif

      CALL QUELLE(l) ! Quellterme werden beim l-ten Zeitschritt
C                     < TSIGNAL beruecksichtigt

C	print*,'     H wird berechnet:'

!$OMP PARALLEL DO private(k,j,i)
       DO k=1,(ZDIM-2)
C	print*,'+    HX wird berechnet:',k	
        DO j=1,(YDIM-2)
         DO i=1,(XDIM-2)
           HX(i,j+1,k+1,0)=HX(i,j+1,k+1,0)+muehx*
     &       ((EY(i,j+1,k-1,0)-27*EY(i,j+1,k,0)+27*EY(i,j+1,k+1,0)-
     &         EY(i,j+1,k+2,0))*deltaz24-
     &        (EZ(i,j-1,k+1,0)-27*EZ(i,j,k+1,0)+27*EZ(i,j+1,k+1,0)-
     &         EZ(i,j+2,k+1,0))*deltay24)
         END DO
        END DO

C	print*,'+    HY wird berechnet:',k
        DO j=1,(YDIM-2)
         DO i=1,(XDIM-2)
           HY(i+1,j,k+1,0)=HY(i+1,j,k+1,0)+muehy*
     &       ((EZ(i-1,j,k+1,0)-27*EZ(i,j,k+1,0)+27*EZ(i+1,j,k+1,0)-
     &         EZ(i+2,j,k+1,0))*deltax24-
     &        (EX(i+1,j,k-1,0)-27*EX(i+1,j,k,0)+27*EX(i+1,j,k+1,0)-
     &         EX(i+1,j,k+2,0))*deltaz24)
         END DO
        END DO
 
C	print*,'+    HZ wird berechnet:',k
        DO j=1,(YDIM-2)
         DO i=1,(XDIM-2)
           HZ(i+1,j+1,k,0)=HZ(i+1,j+1,k,0)+muehz*
     &       ((EX(i+1,j-1,k,0)-27*EX(i+1,j,k,0)+27*EX(i+1,j+1,k,0)-
     &         EX(i+1,j+2,k,0))*deltay24-
     &        (EY(i-1,j+1,k,0)-27*EY(i,j+1,k,0)+27*EY(i+1,j+1,k,0)-
     &         EY(i+2,j+1,k,0))*deltax24)
         END DO
        END DO
       END DO	! k

C       print*,'     E wird berechnet:'

!$OMP PARALLEL DO private(k,j,i)
       DO k=1,(ZDIM-2)
C       print*,'+    EX wird berechnet:',k
        DO j=1,(YDIM-2)
         DO i=1,(XDIM-2)
           EX(i+1,j,k,0)=epsilonhx(i+1,j,k)*EX(i+1,j,k,0)+
     &              sigmahx(i+1,j,k)*
     &                 ((HZ(i+1,j-1,k,0)-27*HZ(i+1,j,k,0)+
     &                27*HZ(i+1,j+1,k,0)-HZ(i+1,j+2,k,0))*deltay24-
     &                  (HY(i+1,j,k-1,0)-27*HY(i+1,j,k,0)+
     &                27*HY(i+1,j,k+1,0)-HY(i+1,j,k+2,0))*deltaz24)
         END DO
        END DO

C       print*,'+    EY wird berechnet:',k
        DO j=1,(YDIM-2)
         DO i=1,(XDIM-2)
           EY(i,j+1,k,0)=epsilonhy(i,j+1,k)*EY(i,j+1,k,0)+
     &              sigmahy(i,j+1,k)*
     &                 ((HX(i,j+1,k-1,0)-27*HX(i,j+1,k,0)+
     &                27*HX(i,j+1,k+1,0)-HX(i,j+1,k+2,0))*deltaz24-
     &                  (HZ(i-1,j+1,k,0)-27*HZ(i,j+1,k,0)+
     &                27*HZ(i+1,j+1,k,0)-HZ(i+2,j+1,k,0))*deltax24)
         END DO
        END DO

C       print*,'+    EZ wird berechnet:',k
        DO j=1,(YDIM-2)
         DO i=1,(XDIM-2)
           EZ(i,j,k+1,0)=epsilonhz(i,j,k+1)*EZ(i,j,k+1,0)+
     &              sigmahz(i,j,k+1)*
     &                 ((HY(i-1,j,k+1,0)-27*HY(i,j,k+1,0)+
     &                27*HY(i+1,j,k+1,0)-HY(i+2,j,k+1,0))*deltax24-
     &                  (HX(i,j-1,k+1,0)-27*HX(i,j,k+1,0)+
     &                27*HX(i,j+1,k+1,0)-HX(i,j+2,k+1,0))*deltay24)
         END DO
        END DO
       END DO	! k

C       Ausgabe auf Profillinie falls ioutform auf 1 gesetzt
C       IF(IOUTFORM.EQ.1.OR.IOUTFORM.EQ.3) THEN
C          CALL OUTLINE
C       ENDIF

C     Speichern der E/H-Werte am Ort des RX:
      CALL OUTTRACE(L)

C     Kontrolle der Rechenzeit:
#ifndef OPERATION
#ifndef NEC
      sec=second()						! SGI
#else
      CALL CPU_TIME(sec)                                	! SX-6
#endif
      PRINT 398, l,ETMOD,sec-sec0
#endif

      END DO ! Der Zeitschleife

  398 FORMAT ( "+ time step ",I6,' of ',I6,' finished within ',F5.2,'s')

C     END !       3-dimensionale Berechnung des O(2/4)-Falles mit
C                   absorbierenden R„ndern
C      Ende der SUBROUTINE CalculateForO24AbsRange3D
      END
