
C*******************************************************************
C*                                                                 *
C* SUBROUTINE zur Berechnung der Hilfsfelder fÅr den 2-D-Fall      *
C*                                                                 *
C*******************************************************************

C*********************** RCS-KEYWORDS ******************************
C $Source: /pf/a/a270012/fdem_src/RCS/CalculateHelpFieldsFor2D.f,v $
C $Author: a270012 $
C $Date: 2002/06/26 13:39:26 $
C $RCSfile: CalculateHelpFieldsFor2D.f,v $
C $Revision: 5.0 $
C $State: Exp $
C*********************** RCS-KEYWORDS ******************************

      SUBROUTINE CalculateHelpFieldsFor2D
      USE dynfelder_mod
C
C
C
C     Berechnung der Hilfsfelder mueh, sigmah, epsilonh fÅr
C     2-dimensionales Modell (also falls TwoOrThree = 2)
C     Das bedeutet, dass j=0=YDIM und alle Y-Koordinaten zu
C     0 werden.
C
C


      muehx=(MUE+MUE)/2
      muehy=(MUE+MUE+MUE+MUE)/4
      muehz=(MUE+MUE)/2

      j=YDIM !YDIM=0

       DO k=0,(ZDIM-1)
        DO i=0,(XDIM-1)

            sigmahx(i+1,j,k)=(SIGMA(i,j,k)+SIGMA(i+1,j,k))/2
            sigmahy(i,j,k)=SIGMA(i,j,k)
            sigmahz(i,j,k+1)=(SIGMA(i,j,k)+SIGMA(i,j,k+1))/2

            epsilonhx(i+1,j,k)=(EPSILON(k)+EPSILON(k))/2
            epsilonhy(i,j,k)=EPSILON(k)
            epsilonhz(i,j,k+1)=(EPSILON(k)+EPSILON(k+1))/2
         END DO
        END DO
C
C      Gesonderte Berechnung von sigmah, epsilonh und mueh fuer
C      i=XDIM und k=ZDIM
C

        i=XDIM
          DO k=0,(ZDIM-1)

            sigmahy(i,j,k)=SIGMA(i,j,k)
            sigmahz(i,j,k+1)=(SIGMA(i,j,k)+SIGMA(i,j,k+1))/2

            epsilonhy(i,j,k)=EPSILON(k)
            epsilonhz(i,j,k+1)=(EPSILON(k)+EPSILON(k+1))/2

          END DO

        k=ZDIM
          DO i=0,(XDIM-1)

            sigmahx(i+1,j,k)=(SIGMA(i,j,k)+SIGMA(i+1,j,k))/2
            sigmahy(i,j,k)=SIGMA(i,j,k)

            epsilonhx(i+1,j,k)=(EPSILON(k)+EPSILON(k))/2
            epsilonhy(i,j,k)=EPSILON(k)

          END DO


        i=XDIM
        k=ZDIM

            sigmahy(i,j,k)=SIGMA(i,j,k)
            epsilonhy(i,j,k)=EPSILON(k)

C     END ! Ende der Berechnung der Hilfsfelder fÅr 2-D-Modell
C     Ende der SUBROUTINE CalculateHelpFieldsFor2D
      END
