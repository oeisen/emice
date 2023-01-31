C*********************************************************************
C*                                                                   *
C* SUBROUTINE zur Berechnung der zeitunabhaengigen Felder 2D und 3D  *
C*                                                                   *
C*********************************************************************

C*********************** RCS-KEYWORDS ******************************
C $Source: /pf/a/a270012/fdem_src/RCS/CalcTimeIndepFieldsForO22andO24.f,v $
C $Author: a270012 $
C $Date: 2002/06/26 13:39:26 $
C $RCSfile: CalcTimeIndepFieldsForO22andO24.f,v $
C $Revision: 5.0 $
C $State: Exp $
C*********************** RCS-KEYWORDS ******************************

      SUBROUTINE CalcTimeIndepFieldsForO22andO24

      USE dynfelder_mod

C     Berechnung von Zeitunabhaengigen Feldern fuer O(2/2)- und
C     O(2/4)-Fall. Berechnung erfolgt sowohl dreidimensional als auch
C     zweidimensional.
C
C     ACHTUNG: Die Felder HX,EY,HZ werden hier nur als Hilfsfelder benutzt!
C     Sie stehen nach dieser Operation wieder zur Verfuegung
C
C************************************
C     Dreidimensionale Berechnung:  *
C************************************

      IF (TwoOrThree.EQ.3) THEN

      muehx=deltat/muehx
      muehy=deltat/muehy
      muehz=deltat/muehz

       DO k=0,(ZDIM-1)
         DO j=0,(YDIM-1)
           DO i=0,(XDIM-1)


            HX(i+1,j,k,0)=2*deltat/(2*epsilonhx(i+1,j,k)+
     &                                     sigmahx(i+1,j,k)*deltat)
            EY(i,j+1,k,0)=2*deltat/(2*epsilonhy(i,j+1,k)+
     &                                     sigmahy(i,j+1,k)*deltat)
            HZ(i,j,k+1,0)=2*deltat/(2*epsilonhz(i,j,k+1)+
     &                                     sigmahz(i,j,k+1)*deltat)

            epsilonhx(i+1,j,k)=(2*epsilonhx(i+1,j,k)-sigmahx(i+1,j,k)*
     &                         deltat)/
     &                        (2*epsilonhx(i+1,j,k)+sigmahx(i+1,j,k)*
     &                         deltat)
            epsilonhy(i,j+1,k)=(2*epsilonhy(i,j+1,k)-sigmahy(i,j+1,k)*
     &                         deltat)/
     &                        (2*epsilonhy(i,j+1,k)+sigmahy(i,j+1,k)*
     &                         deltat)
            epsilonhz(i,j,k+1)=(2*epsilonhz(i,j,k+1)-sigmahz(i,j,k+1)*
     &                         deltat)/
     &                        (2*epsilonhz(i,j,k+1)+sigmahz(i,j,k+1)*
     &                         deltat)

            sigmahx(i+1,j,k)=HX(i+1,j,k,0)
            sigmahy(i,j+1,k)=EY(i,j+1,k,0)
            sigmahz(i,j,k+1)=HZ(i,j,k+1,0)

            HX(i+1,j,k,0)=0
            EY(i,j+1,k,0)=0
            HZ(i,j,k+1,0)=0

           END DO
         END DO
       END DO

        k=ZDIM
         DO j=0,(YDIM-1)
           DO i=0,(XDIM-1)

            HX(i+1,j,k,0)=2*deltat/(2*epsilonhx(i+1,j,k)+
     &                                     sigmahx(i+1,j,k)*deltat)
            EY(i,j+1,k,0)=2*deltat/(2*epsilonhy(i,j+1,k)+
     &                                     sigmahy(i,j+1,k)*deltat)

            epsilonhx(i+1,j,k)=(2*epsilonhx(i+1,j,k)-sigmahx(i+1,j,k)*
     &                         deltat)/
     &                        (2*epsilonhx(i+1,j,k)+sigmahx(i+1,j,k)*
     &                         deltat)
            epsilonhy(i,j+1,k)=(2*epsilonhy(i,j+1,k)-sigmahy(i,j+1,k)*
     &                         deltat)/
     &                        (2*epsilonhy(i,j+1,k)+sigmahy(i,j+1,k)*
     &                         deltat)

            sigmahx(i+1,j,k)=HX(i+1,j,k,0)
            sigmahy(i,j+1,k)=EY(i,j+1,k,0)

            HX(i+1,j,k,0)=0
            EY(i,j+1,k,0)=0


           END DO
         END DO

      i=XDIM
        DO k=0,(ZDIM-1)
         DO j=0,(YDIM-1)

            EY(i,j+1,k,0)=2*deltat/(2*epsilonhy(i,j+1,k)+
     &                                     sigmahy(i,j+1,k)*deltat)
            HZ(i,j,k+1,0)=2*deltat/(2*epsilonhz(i,j,k+1)+
     &                                     sigmahz(i,j,k+1)*deltat)

            epsilonhy(i,j+1,k)=(2*epsilonhy(i,j+1,k)-sigmahy(i,j+1,k)*
     &                         deltat)/
     &                        (2*epsilonhy(i,j+1,k)+sigmahy(i,j+1,k)*
     &                         deltat)
            epsilonhz(i,j,k+1)=(2*epsilonhz(i,j,k+1)-sigmahz(i,j,k+1)*
     &                         deltat)/
     &                        (2*epsilonhz(i,j,k+1)+sigmahz(i,j,k+1)*
     &                         deltat)

            sigmahy(i,j+1,k)=EY(i,j+1,k,0)
            sigmahz(i,j,k+1)=HZ(i,j,k+1,0)

            EY(i,j+1,k,0)=0
            HZ(i,j,k+1,0)=0


           END DO
         END DO

      j=YDIM
        DO k=0,(ZDIM-1)
           DO i=0,(XDIM-1)

            HX(i+1,j,k,0)=2*deltat/(2*epsilonhx(i+1,j,k)+
     &                                     sigmahx(i+1,j,k)*deltat)
            HZ(i,j,k+1,0)=2*deltat/(2*epsilonhz(i,j,k+1)+
     &                                     sigmahz(i,j,k+1)*deltat)

            epsilonhx(i+1,j,k)=(2*epsilonhx(i+1,j,k)-sigmahx(i+1,j,k)*
     &                         deltat)/
     &                        (2*epsilonhx(i+1,j,k)+sigmahx(i+1,j,k)*
     &                         deltat)
            epsilonhz(i,j,k+1)=(2*epsilonhz(i,j,k+1)-sigmahz(i,j,k+1)*
     &                         deltat)/
     &                        (2*epsilonhz(i,j,k+1)+sigmahz(i,j,k+1)*
     &                         deltat)

            sigmahx(i+1,j,k)=HX(i+1,j,k,0)
            sigmahz(i,j,k+1)=HZ(i,j,k+1,0)

            HX(i+1,j,k,0)=0
            HZ(i,j,k+1,0)=0


           END DO
         END DO

        k=ZDIM
        j=YDIM
          DO i=0,(XDIM-1)

            HX(i+1,j,k,0)=2*deltat/(2*epsilonhx(i+1,j,k)+
     &                                     sigmahx(i+1,j,k)*deltat)

            epsilonhx(i+1,j,k)=(2*epsilonhx(i+1,j,k)-sigmahx(i+1,j,k)*
     &                         deltat)/
     &                        (2*epsilonhx(i+1,j,k)+sigmahx(i+1,j,k)*
     &                         deltat)

            sigmahx(i+1,j,k)=HX(i+1,j,k,0)

            HX(i+1,j,k,0)=0

          END DO

        k=ZDIM
        i=XDIM
          DO j=0,(YDIM-1)

            EY(i,j+1,k,0)=2*deltat/(2*epsilonhy(i,j+1,k)+
     &                                     sigmahy(i,j+1,k)*deltat)

            epsilonhy(i,j+1,k)=(2*epsilonhy(i,j+1,k)-sigmahy(i,j+1,k)*
     &                         deltat)/
     &                        (2*epsilonhy(i,j+1,k)+sigmahy(i,j+1,k)*
     &                         deltat)

            sigmahy(i,j+1,k)=EY(i,j+1,k,0)

            EY(i,j+1,k,0)=0


          END DO

        j=YDIM
        i=XDIM
          DO k=0,(ZDIM-1)

            HZ(i,j,k+1,0)=2*deltat/(2*epsilonhz(i,j,k+1)+
     &                                     sigmahz(i,j,k+1)*deltat)

            epsilonhz(i,j,k+1)=(2*epsilonhz(i,j,k+1)-sigmahz(i,j,k+1)*
     &                         deltat)/
     &                        (2*epsilonhz(i,j,k+1)+sigmahz(i,j,k+1)*
     &                         deltat)

            sigmahz(i,j,k+1)=HZ(i,j,k+1,0)

            HZ(i,j,k+1,0)=0


          END DO

        k=ZDIM
        j=YDIM     ! Hierfuer gibt es keinen Wert, der innerhalb des
        i=XDIM     ! Modells liegen wuerde


C************************************
C     Zweidimensionale Berechnung:  *
C************************************


      ELSEIF (TwoOrThree.EQ.2) THEN

      muehx=deltat/muehx
      muehy=deltat/muehy
      muehz=deltat/muehz

      j=YDIM ! YDIM=0

       DO k=0,(ZDIM-1)
         DO i=0,(XDIM-1)

            HX(i+1,j,k,0)=2*deltat/(2*epsilonhx(i+1,j,k)+
     &                                     sigmahx(i+1,j,k)*deltat)
            EY(i,j,k,0)=2*deltat/(2*epsilonhy(i,j,k)+
     &                                     sigmahy(i,j,k)*deltat)
            HZ(i,j,k+1,0)=2*deltat/(2*epsilonhz(i,j,k+1)+
     &                                     sigmahz(i,j,k+1)*deltat)

            epsilonhx(i+1,j,k)=(2*epsilonhx(i+1,j,k)-sigmahx(i+1,j,k)*
     &                         deltat)/
     &                        (2*epsilonhx(i+1,j,k)+sigmahx(i+1,j,k)*
     &                         deltat)
            epsilonhy(i,j,k)=(2*epsilonhy(i,j,k)-sigmahy(i,j,k)*
     &                         deltat)/
     &                        (2*epsilonhy(i,j,k)+sigmahy(i,j,k)*
     &                         deltat)
            epsilonhz(i,j,k+1)=(2*epsilonhz(i,j,k+1)-sigmahz(i,j,k+1)*
     &                         deltat)/
     &                        (2*epsilonhz(i,j,k+1)+sigmahz(i,j,k+1)*
     &                         deltat)

            sigmahx(i+1,j,k)=HX(i+1,j,k,0)
            sigmahy(i,j,k)=EY(i,j,k,0)
            sigmahz(i,j,k+1)=HZ(i,j,k+1,0)

            HX(i+1,j,k,0)=0
            EY(i,j,k,0)=0
            HZ(i,j,k+1,0)=0


         END DO
       END DO

       k=ZDIM
         DO i=0,(XDIM-1)

            HX(i+1,j,k,0)=2*deltat/(2*epsilonhx(i+1,j,k)+
     &                                     sigmahx(i+1,j,k)*deltat)
            EY(i,j,k,0)=2*deltat/(2*epsilonhy(i,j,k)+
     &                                     sigmahy(i,j,k)*deltat)

            epsilonhx(i+1,j,k)=(2*epsilonhx(i+1,j,k)-sigmahx(i+1,j,k)*
     &                         deltat)/
     &                        (2*epsilonhx(i+1,j,k)+sigmahx(i+1,j,k)*
     &                         deltat)
            epsilonhy(i,j,k)=(2*epsilonhy(i,j,k)-sigmahy(i,j,k)*
     &                         deltat)/
     &                        (2*epsilonhy(i,j,k)+sigmahy(i,j,k)*
     &                         deltat)

            sigmahx(i+1,j,k)=HX(i+1,j,k,0)
            sigmahy(i,j,k)=EY(i,j,k,0)

            HX(i+1,j,k,0)=0
            EY(i,j,k,0)=0


         END DO

      i=XDIM
        DO k=0,(ZDIM-1)

            EY(i,j,k,0)=2*deltat/(2*epsilonhy(i,j,k)+
     &                                     sigmahy(i,j,k)*deltat)
            HZ(i,j,k+1,0)=2*deltat/(2*epsilonhz(i,j,k+1)+
     &                                     sigmahz(i,j,k+1)*deltat)

            epsilonhy(i,j,k)=(2*epsilonhy(i,j,k)-sigmahy(i,j,k)*
     &                         deltat)/
     &                        (2*epsilonhy(i,j,k)+sigmahy(i,j,k)*
     &                         deltat)
            epsilonhz(i,j,k+1)=(2*epsilonhz(i,j,k+1)-sigmahz(i,j,k+1)*
     &                         deltat)/
     &                        (2*epsilonhz(i,j,k+1)+sigmahz(i,j,k+1)*
     &                         deltat)

            sigmahy(i,j,k)=EY(i,j,k,0)
            sigmahz(i,j,k+1)=HZ(i,j,k+1,0)

            EY(i,j,k,0)=0
            HZ(i,j,k+1,0)=0

        END DO

        i=XDIM
        k=ZDIM

            EY(i,j,k,0)=2*deltat/(2*epsilonhy(i,j,k)+
     &                                     sigmahy(i,j,k)*deltat)

            epsilonhy(i,j,k)=(2*epsilonhy(i,j,k)-sigmahy(i,j,k)*
     &                         deltat)/
     &                        (2*epsilonhy(i,j,k)+sigmahy(i,j,k)*
     &                         deltat)

            sigmahy(i,j,k)=EY(i,j,k,0)

            EY(i,j,k,0)=0


C************************************
C     Eindimensionale Berechnung:   *
C************************************


      ELSEIF (TwoOrThree.EQ.1) THEN

      muehx=deltat/muehx

      i=XDIM ! XDIM=0
      j=YDIM ! YDIM=0

       DO k=0,ZDIM

            EY(i,j,k,0)=2*deltat/(2*epsilonhy(i,j,k)+
     &                                     sigmahy(i,j,k)*deltat)

            epsilonhy(i,j,k)=(2*epsilonhy(i,j,k)-sigmahy(i,j,k)*
     &                         deltat)/
     &                        (2*epsilonhy(i,j,k)+sigmahy(i,j,k)*
     &                         deltat)

            sigmahy(i,j,k)=EY(i,j,k,0)

            EY(i,j,k,0)=0

       END DO


      END IF ! Ende der Berechnung der zeitunabhaengigen Felder fuer
C              O(2/2)- und O(2/4)-Fall

C     Ende der SUBROUTINE CalcTimeIndepFieldsForO22andO24
      END
