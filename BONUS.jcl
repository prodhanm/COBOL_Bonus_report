//BONUS JOB 30000000, 'BONUS  ', MSGLEVEL=(1,1),
//  CLASS=A,MSGCLASS=Q,NOTIFY=&SYSUID,REGION=0M
//*
//COMP1     EXEC IGYWCL,PARM=(OFFSET,NOLIST,ADV),
//  PGMLIB='&&GOSET'.GOPGM=DATETIME
//COBOL.SYSIN   DD  DSN=COBOL.REPORT(DATETIME),DISP=SHR
//COBOL.SYSLIB  DD  DSN=COBOL.ONLINE.LOADLIB,DISP=SHR
//*
//COMP2     EXEC IGYWCL,PARM=(OFFSET,NOLIST,ADV),
//  PGMLIB='&&GOSET'.GOPGM=BONUS
//COBOL.SYSIN   DD  DSN=COBOL.REPORT(BONUS),DISP=SHR
//COBOL.SYSLIB  DD  DSN=COBOL.ONLINE.LOADLIB,DISP=SHR
//COMP3     EXEC IGYWCL,PARM=(OFFSET,NOLIST,ADV),
//     PGMLIB='&&GOSET',GOPGM=BONUS
//  PGMLIB='&&GOSET'.GOPGM=BONUS
//COBOL.SYSIN   DD  DSN=COBOL.REPORT(DATETIME),DISP=SHR
//COBOL.SYSIN`  DD  *
    ENTRY BONUS
    INCLUDE     SYSLMOD(DATETIME)
    INCLUDE     SYSLMOD(BONUS)
//*
//STEP4     EXEC PGM=BONUS
//STEPLIB   DD  DSN=&&GOSET, DISP=(OLD,PASS)
//INBONUS   DD  DSN=REPORT.DATA,DISP=SHR
//INSTATE   DD  DUMMY
//OUTREPT   DD  SYSOUT=*
//SYSOUT    DD  SYSOUT=*
...........................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................