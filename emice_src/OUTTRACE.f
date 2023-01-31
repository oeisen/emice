C********************************************************
C*                                                      *
C* SUBROUTINE zur Speichern der Spur am Ort des RX	*
C*                                                      *
C********************************************************

C*********************** RCS-KEYWORDS ******************************
C $Source: /pf/a/a270012/fdem_src/RCS/OUTTRACE.f,v $
C $Author: a270012 $
C $Date: 2002/06/26 13:39:26 $
C $RCSfile: OUTTRACE.f,v $
C $Revision: 5.0 $
C $State: Exp $
C*********************** RCS-KEYWORDS ******************************

      SUBROUTINE OUTTRACE(l)
      USE dynfelder_mod
      USE netcdf
C     INCLUDE 'netcdf.inc'

      INTEGER l
      INTEGER tindex	! Zeitindex fuer NETCDF (STMOD beruecksichtigen)
      INTEGER start(4),stride(4),count(4)

C      PRINT*,'SUBROUTINE OUTTRACE'


      IF(IOUTFORM.EQ.0) THEN	! NETCDF

        IF (MOD(l,OUTSTEP).EQ.0) THEN

          start  = (/ 1,1,1,l/OUTSTEP /)
          stride = (/ 1,1,1,1 /)
          count  = (/ XDimOUT,YDimOUT,ZDimOUT, 1 /)

        tindex=(l-STMOD+1)/OUTSTEP	

C       print*,'Write EX component'
          IF (IOUTEM(1).EQ.1) 
     &        CALL chk_nf(NF90_PUT_VAR(EM_nc,EM_ID(1),
     &        EX(SXOUT:EXOUT:OUTSTRIDE,
     &	       SYOUT:EYOUT:OUTSTRIDE,
     & 	       SZOUT:EZOUT:OUTSTRIDE,0),
     &         start  = (/ 1,1,1,tindex /),
     &         stride = (/ 1,1,1,1 /),
     &         count  = (/ XDimOUT,YDimOUT,ZDimOUT, 1 /)
     &         ))
C    &         start,stride,count ))

C       print*,'Write EY component'
          IF (IOUTEM(2).EQ.1) 
     &        CALL chk_nf(NF90_PUT_VAR(EM_nc,EM_ID(2),
     &        EY(SXOUT:EXOUT:OUTSTRIDE,
     &	       SYOUT:EYOUT:OUTSTRIDE,
     & 	       SZOUT:EZOUT:OUTSTRIDE,0),
     &         start  = (/ 1,1,1,tindex /),
     &         stride = (/ 1,1,1,1 /),
     &         count  = (/ XDimOUT,YDimOUT,ZDimOUT, 1 /)
     &         ))
C    &         start,stride,count ))

C       print*,'Write EZ component'
          IF (IOUTEM(3).EQ.1) 
     &        CALL chk_nf(NF90_PUT_VAR(EM_nc,EM_ID(3),
     &        EZ(SXOUT:EXOUT:OUTSTRIDE,
     &	       SYOUT:EYOUT:OUTSTRIDE,
     & 	       SZOUT:EZOUT:OUTSTRIDE,0),
     &         start  = (/ 1,1,1,tindex /),
     &         stride = (/ 1,1,1,1 /),
     &         count  = (/ XDimOUT,YDimOUT,ZDimOUT, 1 /)
     &         ))
C    &         start,stride,count ))

C       print*,'Write HX component'
          IF (IOUTEM(4).EQ.1) 
     &        CALL chk_nf(NF90_PUT_VAR(EM_nc,EM_ID(4),
     &        HX(SXOUT:EXOUT:OUTSTRIDE,
     &	       SYOUT:EYOUT:OUTSTRIDE,
     & 	       SZOUT:EZOUT:OUTSTRIDE,0),
     &         start  = (/ 1,1,1,tindex /),
     &         stride = (/ 1,1,1,1 /),
     &         count  = (/ XDimOUT,YDimOUT,ZDimOUT, 1 /)
     &         ))
C    &         start,stride,count ))

C       print*,'Write HY component'
          IF (IOUTEM(5).EQ.1) 
     &        CALL chk_nf(NF90_PUT_VAR(EM_nc,EM_ID(5),
     &        HY(SXOUT:EXOUT:OUTSTRIDE,
     &	       SYOUT:EYOUT:OUTSTRIDE,
     & 	       SZOUT:EZOUT:OUTSTRIDE,0),
     &         start  = (/ 1,1,1,tindex /),
     &         stride = (/ 1,1,1,1 /),
     &         count  = (/ XDimOUT,YDimOUT,ZDimOUT, 1 /)
     &         ))
C    &         start,stride,count ))

C       print*,'Write HZ component'
          IF (IOUTEM(6).EQ.1) 
     &        CALL chk_nf(NF90_PUT_VAR(EM_nc,EM_ID(6),
     &        HZ(SXOUT:EXOUT:OUTSTRIDE,
     &	       SYOUT:EYOUT:OUTSTRIDE,
     & 	       SZOUT:EZOUT:OUTSTRIDE,0),
     &         start  = (/ 1,1,1,tindex /),
     &         stride = (/ 1,1,1,1 /),
     &         count  = (/ XDimOUT,YDimOUT,ZDimOUT, 1 /)
     &         ))
C    &         start,stride,count ))

        ENDIF 	! MOD(l,OUTSTEP).EQ.0 NETCDF

      ELSE IF(IOUTFORM.EQ.3) THEN ! REFLEX REC.S 2D-line

        IF(IOUTEM(1).EQ.1) EMOUTEX(l)=EX(MXREC1,MYREC1,NZREC1,0)
        IF(IOUTEM(2).EQ.1) EMOUTEY(l)=EY(MXREC1,MYREC1,NZREC1,0)
        IF(IOUTEM(3).EQ.1) EMOUTEZ(l)=EZ(MXREC1,MYREC1,NZREC1,0)
        IF(IOUTEM(4).EQ.1) EMOUTHX(l)=HX(MXREC1,MYREC1,NZREC1,0)
        IF(IOUTEM(5).EQ.1) EMOUTHY(l)=HY(MXREC1,MYREC1,NZREC1,0)
        IF(IOUTEM(6).EQ.1) EMOUTHZ(l)=HZ(MXREC1,MYREC1,NZREC1,0)

C       Schnellausgabe der letzten 50 Werte RX-Startposition
C       aus Variablen ..OUT(:)
        IF(l.EQ.lle) THEN
          PRINT*,'lla:',lla,'lle:',lle
          DO ll=lla,lle
            IF(IOUTEM(1).EQ.1) WRITE(20,301) ll*DELTAT, EMOUTEX(ll)
            IF(IOUTEM(2).EQ.1) WRITE(21,301) ll*DELTAT, EMOUTEY(ll)
            IF(IOUTEM(3).EQ.1) WRITE(22,301) ll*DELTAT, EMOUTEZ(ll)
            IF(IOUTEM(4).EQ.1) WRITE(23,301) ll*DELTAT, EMOUTHX(ll)
            IF(IOUTEM(5).EQ.1) WRITE(24,301) ll*DELTAT, EMOUTHY(ll)
            IF(IOUTEM(6).EQ.1) WRITE(25,301) ll*DELTAT, EMOUTHZ(ll)
          ENDDO
          lla=l+1
          lle=l+50
        ENDIF

      ELSE IF(IOUTFORM.EQ.4) THEN ! REC.S-REC.E: xz-plane

       DO i=MXREC1,MXREC2
	DO j=MYREC1,MYREC2
	 DO k=NZREC1,NZREC2
	  IF(IOUTEM(1).EQ.1) WRITE(20,302) 
     &      l,i-ISX,j-ISY,k-ISIGMA,EX(i,j,k,0)
	  IF(IOUTEM(2).EQ.1) WRITE(21,302) 
     &      l,i-ISX,j-ISY,k-ISIGMA,EY(i,j,k,0)
	  IF(IOUTEM(3).EQ.1) WRITE(22,302) 
     &      l,i-ISX,j-ISY,k-ISIGMA,EZ(i,j,k,0)
	  IF(IOUTEM(4).EQ.1) WRITE(23,302) 
     &      l,i-ISX,j-ISY,k-ISIGMA,HX(i,j,k,0)
	  IF(IOUTEM(5).EQ.1) WRITE(24,302) 
     &      l,i-ISX,j-ISY,k-ISIGMA,HY(i,j,k,0)
	  IF(IOUTEM(6).EQ.1) WRITE(25,302) 
     &      l,i-ISX,j-ISY,k-ISIGMA,HZ(i,j,k,0)
	 END DO
	END DO
       END DO

      ENDIF

  301 FORMAT(E12.6,1X,E12.6)
  302 FORMAT(4(I8,1X),E12.6)

      END !	SUBROUTINE OUTTRACE
