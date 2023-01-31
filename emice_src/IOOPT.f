
C*******************************************************************
C*                                                                 *
C* SUBROUTINE zum Bestimmen der Ein/Ausgabedateien und -pfade      *
C*                                                                 *
C*******************************************************************

C*********************** RCS-KEYWORDS ******************************
C $Source: /pf/a/a270012/fdem_src/RCS/IOOPT.f,v $
C $Author: a270012 $
C $Date: 2002/06/26 13:39:26 $
C $RCSfile: IOOPT.f,v $
C $Revision: 5.0 $
C $State: Exp $
C*********************** RCS-KEYWORDS ******************************


      SUBROUTINE IOOPT 
      USE dynfelder_mod

C     PRINT*,'SUBROUTINE IOOPT'

C*******************************************************************************
C     Einlesen der Argumente 

      narg = iargc()
      IF (narg.EQ.0) THEN 
         CALL HANDLE_ERR(5)
      ELSE
       DO i=0,narg
         CALL getarg( i, argv )
         ilenc=0
         DO k=1,80
           IF(comline(K:K).EQ.' '.AND.ilenc.EQ.0) ilenc=k
         END DO
         comline=comline(1:ilenc-1)//"_"//argv
C        print*,'ROOT:', argv
         IF(argv.EQ.'-r') then
           CALL getarg( i+1, argv )
           run=argv
         ELSE IF(argv.EQ.'-i') then
           CALL getarg( i+1, argv )
           inroot=argv
         ELSE IF(argv.EQ.'-o') then
           CALL getarg( i+1, argv )
           outroot=argv
         ELSE IF(argv.EQ.'-s') then
           CALL getarg( i+1, argv )
           inwl=argv
         ELSE IF(argv.EQ.'-f') then
	   NF90CLOB=1
         ELSE IF(argv.EQ.'-O') then
           IINITOUT=1
           CALL getarg( i+1, argv )
           NCEMOUTF=argv
         ELSE IF(argv.EQ.'-I') then
           IINITIN=1
           CALL getarg( i+1, argv )
           NCEMINF=argv
         ELSE IF (argv.EQ.'-h') THEN 
           CALL HANDLE_ERR(-1)
         ENDIF
       END DO
       ilenc=0
       DO k=1,80
          IF(run(K:K).EQ.' '.AND.ILEN(1).EQ.0) ILEN(1)=K
          IF(inroot(K:K).EQ.' '.AND.ILEN(2).EQ.0) ILEN(2)=K
          IF(outroot(K:K).EQ.' '.AND.ILEN(3).EQ.0) ILEN(3)=K
          IF(comline(K:K).EQ.' '.AND.ilenc.EQ.0) ilenc=k
          IF(comline(K:K).EQ.'_') comline(k:k) = ' '
       END DO
C      print*, 'IOOPT: KOMMANDOZEILE', comline

C      EINGABEDATEIEN
       INFDR=inroot(1:ILEN(2)-1)//"/"//run(1:ILEN(1)-1)//".fdr"
       INDES=inroot(1:ILEN(2)-1)//"/"//run(1:ILEN(1)-1)//".des"

      ENDIF

      END !   SUBROUTINE IOOPT
