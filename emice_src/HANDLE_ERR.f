
C*******************************************************************
C*                                                                 *
C* SUBROUTINE zur Ausgabe der Felder H(x,y,z,0) etc.               *
C*                                                                 *
C*******************************************************************

C*********************** RCS-KEYWORDS ******************************
C $Source: /pf/a/a270012/fdem_src/RCS/HANDLE_ERR.f,v $
C $Author: a270012 $
C $Date: 2002/06/26 13:39:26 $
C $RCSfile: HANDLE_ERR.f,v $
C $Revision: 5.0 $
C $State: Exp $
C*********************** RCS-KEYWORDS ******************************


      SUBROUTINE HANDLE_ERR(status)

C     Ausgabe Fehlermeldungen, Hilfeseite, etc in Abhaengigkeit von status
C     status >= 0 : Fehler --> STOP
C     status = -1 : Hilfeseite --> STOP
C     status < -1 : Warnung --> NO STOP

C     PRINT*,'SUBROUTINE HANDLE_ERR'

      INTEGER, intent ( in) :: status

C     *****************************************************************
      IF (status.GT.0) THEN				! ERRORS
        WRITE(*,'(A,I2,A,$)') '0ERROR ',status,' -- '
        IF (status.EQ.10) THEN
          print*, 'IWAVE=10: elastic wave 
     &           propogation not supported'
        ELSE IF (status.EQ.11) THEN
          print*, 'IWAVE=11: acoustic wave 
     &           propogation not supported'
        ELSE IF (status.EQ.12) THEN
          print*, 'IWAVE=12: SH wave 
     &           propogation not supported'
        ELSE IF (status.EQ.4) THEN
          print*, 'MAX.TIME STEPS IS 4096'
        ELSE IF (status.EQ.5) THEN
          print*, 'WRONG NUMBER OF ARGUMENTS'
          print*, 'use -h for more information'
        ELSE IF (status.EQ.6) THEN
          print*, 'CANNOT INTERPOLATE WAVELET'
        ELSE IF (status.EQ.7) THEN
          print*, 'MODULE NOT IMPLEMENTED!'
        ELSE IF (status.EQ.8) THEN
          print*, 'Reading file for initialization:'
          print*, 'Time for model start (from -I <NCEMINF>)'
          print*, 'larger than final time'
          print*, 'STMOD > ETMOD'
        ELSE IF (status.EQ.9) THEN
          print*, 'TwoOrThree has wrong value'
          print*, 'Choose 1, 2, or 3'
        ELSE IF (status.EQ.99) THEN
          print*, 'SCHEDULED EXIT!'
        ENDIF
        PRINT*,'ABORT PROGRAM'
        stop 
C     *****************************************************************
      ELSE IF (status.EQ.-1) THEN			! HELP PAGE
        print*,' '
        print*, 'USAGE: FD_EM.e -r <run> -i <in> -o <out>', 
     &           '[ -s <wl> -I <eminf> -O <emoutf> -f -h ]'
        print*,'   -r: use <run> as root for io-filenames'
        print*,'   -i: input path <in>'
        print*,'   -o: output path <out>'
        print*,'   -s: use wavelet <wl> for source signal'
        print*,'   -f: force clobbing of output files (NETCDF only)'
        print*,'   -h: print this help'
        print*,'   -I: read EM field from file <eminf>'
        print*,'   -O: write EM field of last time step for 
     & 		       later initialization to file <emoutf>'
        print*,' '
        print*,'   If -s <wl> is specified, actual value of ISIGNAL'
        print*,'   is overridden. ISIGNAL is then only used to '
        print*,'   determine the type of source implementation'
        print*,'   (ISGINAL  < 10: Feld am Ort TX wird ueberschrieben'
        print*,'    ISIGNAL >= 10: Feld am Ort TX wird ersetzt)'
        print*,'Important flags:'
        print*,' IOUTTYPE: Umfang der Ausgabe:'
        print*,'   0: Modellraum incl. Rand ohne Abtastung'
        print*,'   1: Modellraum ohne Abtastung'
        print*,'   2: Modellraum incl. Rand', 
     &                '(Abtastung: OUTSTRIDE,OUTSTEP)'
        print*,'   3: Modellraum (-"-)'
        print*,'   4: xz-subregion RECXS-RECXE,',
     &                'RECZS-RECZE (-"-)'
        print*,'   5: 2D-slice (z,t) RECXS,z (-"-)'
        print*,'   6: 1D-line (t) RECXS,RECZS (-"-)'
        print*,' '
        print*,' IOUTFORM:'
        print*,'   0: NETCDF'
        print*,'   2: REFLEX FD-Snapshots (total wavefield)'
        print*,'   3: REFLEX FD-2D-line (single line)'
        print*,' '
        print*,'    G E N E R A L   I N F O R M A T I O N'
        print*,' '
        print*,' FD_EM.e reads the effective permittivity e (epsilon)'
        print*,' and the effective conductivity s (sigma) from the'
        print*,' file <>.des. It does not perform any calculations'
        print*,' like averaging, frequency scaling, whatsoever with'
        print*,' these values.'
	print*,' As the conductivity and permittivity of a mixture'
        print*,' depend on frequency, all necessary calculations have'
        print*,' to be done before supplying e and s to FD_EM.e.'
        print*,' '
        print*,' EXAMPLE:'
        print*,' From DEP-measurements e and s are available at'
        print*,' 250 kHz. To use them for modelling at 100 MHz, '
        print*,' apply the complex-valued Looyenga-mixing (COMP) '
        print*,' at the EMR frequency. In case the values are '
        print*,' provided as Re{e} and Im{e}, transform Im{e}'
        print*,' to s by s=Im{e}*2*pi*f*e_0.'
        print*,' '
        print*,' '
        print*,' '
        print*,' '
        stop
C     *****************************************************************
      ELSE IF (status.LT.-1) THEN			! WARNINGS
        WRITE(*,'(A,I2,A,$)') '0WARNING ',status,' -- '
        IF (status.EQ.-2) THEN
          print*, 'Time increments are not equal:'
          print*, 'DT != DT_in (from -I <NCEMINF>)' 
        ELSE IF (status.EQ.-3) THEN
          print*, 'OPTION -I <NCEMINF>: '
          print*, 'Read EM field for initalization:' 
          print*, 'starting time STMOD and dimension TDIM '
          print*, 'of FD calculations changed.'
        ENDIF
      ENDIF
C     *****************************************************************

      END !   SUBROUTINE HANDLE_ERR

      SUBROUTINE chk_nf(status)
      USE NETCDF
C     INCLUDE 'netcdf.inc'
C       Internal subroutine - checks error status after each netcdf,
C       prints out text message each time an error code is returned.
      INTEGER, intent ( in) :: status
      IF(status /= nf90_noerr) then
        WRITE(*,'(A,$)') '0ERROR -- '
C       print *, trim(nf90_strerror(status))
        print *, nf90_strerror(status)
        print*, 'ABORT PROGRAM'
        stop
      ENDIF
      END SUBROUTINE chk_nf
