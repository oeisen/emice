
C*******************************************************************
C*                                                                 *
C* SUBROUTINE zur Berechnung der Hilfsfelder fÅr den 3-D-Fall      *
C*                                                                 *
C*******************************************************************

C*********************** RCS-KEYWORDS ******************************
C $Source: /pf/a/a270012/fdem_src/RCS/CalculateHelpFieldsFor3D.f,v $
C $Author: a270012 $
C $Date: 2002/06/26 13:39:26 $
C $RCSfile: CalculateHelpFieldsFor3D.f,v $
C $Revision: 5.0 $
C $State: Exp $
C*********************** RCS-KEYWORDS ******************************

      SUBROUTINE CalculateHelpFieldsFor3D
      USE dynfelder_mod
C
C
C
C     Berechnung der Hilfsfelder mueh, sigmah, epsilonh fÅr
C     3-dimensionale Berechnung (also falls TwoOrThree = 3)
C
C

      muehx=(MUE+MUE+MUE+MUE)/4
      muehy=(MUE+MUE+MUE+MUE)/4
      muehz=(MUE+MUE+MUE+MUE)/4

        DO k=0,(ZDIM-1)
         DO j=0,(YDIM-1)
          DO i=0,(XDIM-1)

            sigmahx(i+1,j,k)=(SIGMA(i,j,k)+SIGMA(i+1,j,k))/2
            sigmahy(i,j+1,k)=(SIGMA(i,j,k)+SIGMA(i,j+1,k))/2
            sigmahz(i,j,k+1)=(SIGMA(i,j,k)+SIGMA(i,j,k+1))/2

            epsilonhx(i+1,j,k)=(EPSILON(k)+EPSILON(k))/2
            epsilonhy(i,j+1,k)=(EPSILON(k)+EPSILON(k))/2
            epsilonhz(i,j,k+1)=(EPSILON(k)+EPSILON(k+1))/2
          END DO
         END DO
        END DO
C
C      Gesonderte Berechnung von sigmah, epsilonh und mueh fuer
C      i=XDIM,j=YDIM und k=ZDIM
C
        i=XDIM
          DO k=0,(ZDIM-1)
           DO j=0,(YDIM-1)


            sigmahy(i,j+1,k)=(SIGMA(i,j,k)+SIGMA(i,j+1,k))/2
            sigmahz(i,j,k+1)=(SIGMA(i,j,k)+SIGMA(i,j,k+1))/2

            epsilonhy(i,j+1,k)=(EPSILON(k)+EPSILON(k))/2
            epsilonhz(i,j,k+1)=(EPSILON(k)+EPSILON(k+1))/2

            END DO
           END DO

        j=YDIM
          DO k=0,(ZDIM-1)
           DO i=0,(XDIM-1)


            sigmahx(i+1,j,k)=(SIGMA(i,j,k)+SIGMA(i+1,j,k))/2
            sigmahz(i,j,k+1)=(SIGMA(i,j,k)+SIGMA(i,j,k+1))/2

            epsilonhx(i+1,j,k)=(EPSILON(k)+EPSILON(k))/2
            epsilonhz(i,j,k+1)=(EPSILON(k)+EPSILON(k+1))/2

           END DO
          END DO

          k=ZDIM
          DO j=0,(YDIM-1)
           DO i=0,(XDIM-1)


            sigmahx(i+1,j,k)=(SIGMA(i,j,k)+SIGMA(i+1,j,k))/2
            sigmahy(i,j+1,k)=(SIGMA(i,j,k)+SIGMA(i,j+1,k))/2

            epsilonhx(i+1,j,k)=(EPSILON(k)+EPSILON(k))/2
            epsilonhy(i,j+1,k)=(EPSILON(k)+EPSILON(k))/2

           END DO
          END DO


        i=XDIM
        j=YDIM
        DO k=0,(ZDIM-1)

            sigmahz(i,j,k+1)=(SIGMA(i,j,k)+SIGMA(i,j,k+1))/2

            epsilonhz(i,j,k+1)=(EPSILON(k)+EPSILON(k+1))/2

        END DO

        i=XDIM
        k=ZDIM
        DO j=0,(YDIM-1)

            sigmahy(i,j+1,k)=(SIGMA(i,j,k)+SIGMA(i,j+1,k))/2

            epsilonhy(i,j+1,k)=(EPSILON(k)+EPSILON(k))/2

        END DO

        j=YDIM
        k=ZDIM
        DO i=0,(XDIM-1)

            sigmahx(i+1,j,k)=(SIGMA(i,j,k)+SIGMA(i+1,j,k))/2

            epsilonhx(i+1,j,k)=(EPSILON(k)+EPSILON(k))/2

        END DO

        i=XDIM
        j=YDIM  !Fuer diese Kombination gibt es keinen Punkt,
        k=ZDIM  !der noch innerhalb des Modellraumes liegt

C     END ! Ende der Berechnung der Hilfsfelder fÅr 3-D-Modell
C     Ende der SUBROUTINE CalculateHelpFieldsFor3D
      END
