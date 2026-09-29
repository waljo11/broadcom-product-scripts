/*   rexx    */
/* Perform various Package actions in REXX                          */
/*                                                                  */
/* A   COBOL exit CALLS this REXX and provides values for           */
/* REXX variables, including these.                                 */
/* Find documentation on these in the TechDocs documentation        */
/* where each underscore appears as a dash in the documentation.    */
/* For example, PECB_PACKAGE_ID is documented as                    */
/*              PECB-PACKAGE-ID                                     */
/*                                                                  */
/* PECB_PACKAGE_ID               PAPP_GROUP_NAME                    */
/* PECB_FUNCTION_LITERAL         PAPP_ENVIRONMENT                   */
/* PECB_SUBFUNC_LITERAL          PAPP_QUORUM_COUNT                  */
/* PECB_BEF_AFTER_LITERAL        PAPP_APPROVER_FLAG                 */
/* PECB_USER_BATCH_JOBNAME       PAPP_APPR_GRP_TYPE                 */
/* PREQ_PKG_CAST_COMPVAL         PAPP_APPR_GRP_DISQ                 */
/* PHDR_PKG_SHR_OPTION           PAPP_SEQUENCE_NUMBER               */
/* PHDR_PKG_ENV                                                     */
/* PHDR_PKG_STGID                                                   */
/*     Address fields are provided for fields that may be           */
/*     modified by the REXX.                                        */
/* Address_PECB_MESSAGE          Address_MYSMTP_SUBJECT             */
/* Address_MYSMTP_MESSAGE        Address_MYSMTP_TEXT                */
/* Address_MYSMTP_USERID         Address_MYSMTP_URL                 */
/* Address_MYSMTP_FROM           Address_MYSMTP_EMAIL_IDS           */
/* MYSMTP_EMAIL_IDS              MYSMTP_EMAIL_ID_SIZE               */
/*                                                                  */
   /* If wanting to limit the use of this exit, uncomment...        */
/*
   If USERID() /= 'IBMUSER' &,
      USERID() /= 'JW61868' &,
      USERID() /= 'JW618685' then Say USERID()
*/
   /* In case these are not already allocated, these are attempted  */
   STRING = "ALLOC DD(SYSTSPRT) SYSOUT(A) "
   CALL BPXWDYN STRING;
   STRING = "ALLOC DD(SYSTSIN) DUMMY"
   CALL BPXWDYN STRING;
   /* If C1UEXTR7 is allocated to anything, turn on Trace  */
   WhatDDName = 'C1UEXTR7'
   CALL BPXWDYN "INFO FI("WhatDDName")",
              "INRTDSN(DSNVAR) INRDSNT(myDSNT)"
   if RESULT = 0 then TraceRQ = 'Y'
   /* Initialize variables....                             */
   Message = ''
   MessageCode = '    '
   MyRc = 0
   /* Parms are REXX statements passed from COBOL exit              */
   Arg Parms
   Parms = Strip(Parms)
   sa= 'Parms len=' Length(Parms)
   If TraceRQ = 'Y' then,
      Say 'C1UEXTR7 is called again:'
   /* Parms from C1UEXT07 is a string of REXX statements   */
   /*  Validate and interpret if validation is OK   */
   myRC = EvaluateParms()
   If myRC > 4 then Exit(12)
   /* Interpret Parms */
   If Substr(PHDR_PKG_NOTE5,1,5) = 'TRACE' then TraceRQ = 'Y'
   If TraceRQ = 'Y' then,
      If PECB_MODE = 'B' then Trace r
      Else                    Trace ?r
   where = 'C1UEXTR7'
   what = 'C1UEXTR7-' PECB_FUNCTION_LITERAL,
                      PECB_BEF_AFTER_LITERAL,
                      PHDR_PACKAGE_STATUS
   /* If being called Before the CAST, just get out              */
   If PECB_BEF_AFTER_LITERAL = 'BEFORE'   &,
      PECB_FUNCTION_LITERAL = 'CAST'      then,
       Exit
   /* If the package status just became IN-APPROVAL, send emails */
   /*  to request approval(s).                                   */
   IF PHDR_PACKAGE_STATUS = 'IN-APPROVAL' &,
      PECB_BEF_AFTER_LITERAL = 'AFTER'    &,
      PECB_FUNCTION_LITERAL = 'CAST'      &,
      Substr(CALL_REASON,1,16) = 'APPROVER GROUP #' then,
       Do
       /* Find SENDMAIL on GitHub in the folder-                 */
       /* endevor/Field-Developed-Programs/...                   */
       /*   Email-For-External-Approver-Groups                   */
       Call SENDMAIL PAPP_GROUP_NAME PECB_PACKAGE_ID,
          'Needs-Approval' PAPP_APPROVAL_IDS
       Exit
       End
   /* Find GTUNIQUE on GitHub in the folder-                   */
   /* endevor/Field-Developed-Programs/Miscellaneous-items     */
   Unique_Name = GTUNIQUE()
   IF PHDR_PACKAGE_STATUS = 'APPROVED' &,
      PECB_BEF_AFTER_LITERAL = 'AFTER' &,
      (PECB_FUNCTION_LITERAL = 'CAST' |,
       PECB_FUNCTION_LITERAL = 'REVIEW') Then,
       Do
       GoExecute = 'Y'
       Call CheckExecutionWindow
       If GoExecute = 'Y' then,
         DO
         Call Get_Site_Shipping_Variables
         PKGEXECT_Parm = Copies(' ',055)
         PKGEXECT_Parm = Overlay(PECB_PACKAGE_ID     ,PKGEXECT_Parm,001)
         PKGEXECT_Parm = Overlay(PHDR_PKG_ENV        ,PKGEXECT_Parm,018)
         PKGEXECT_Parm = Overlay(PHDR_PKG_STGID      ,PKGEXECT_Parm,026)
         PKGEXECT_Parm = Overlay(REXX_EXEC_MODE      ,PKGEXECT_Parm,028)
         PKGEXECT_Parm = Overlay(PHDR_PKG_CREATE_USER,PKGEXECT_Parm,029)
         PKGEXECT_Parm = Overlay(PHDR_PKG_UPDATE_USER,PKGEXECT_Parm,037)
         PKGEXECT_Parm = Overlay(PHDR_PKG_CAST_USER  ,PKGEXECT_Parm,045)
         /* Find PKGEXECT on GitHub in the folder-               */
         /* endevor/Field-Developed-Programs/Package-Automation  */
         Call PKGEXECT PKGEXECT_Parm
         End  /* If GoExecute = 'Y' */
       Exit
       End
   /* If a package is executed, examine for package shipments */
   /* Examine NOTES to determine whether the Package NOTES    */
   /* contain Shipping instructions...                        */
   /* If a package is executed, examine for package shipments */
      /* You can limit this action to packages with Approvals  */
      /* by including    the next line....                     */
      /* PECB_ACT_REC_EXIST_FLAG = 'Y' &,                      */
   IF PECB_BEF_AFTER_LITERAL = 'AFTER' &,
      (Substr(PECB_FUNCTION_LITERAL,1,4) = 'EXEC' |,
       Substr(PECB_FUNCTION_LITERAL,1,4) = 'BACK') then,
       Do
       If TraceRQ = 'Y' then Say 'C1UEXTR7 is exiting @160 '
       /* Find PKGESHIP on GitHub in the folder-                 */
       /* endevor/Field-Developed-Programs/Package-Automation    */
       PKGESHIP_Parm = Copies(' ',055)
       PKGESHIP_Parm = Overlay(PECB_PACKAGE_ID     ,PKGESHIP_Parm,001)
       PKGESHIP_Parm = Overlay(PHDR_PKG_ENV        ,PKGESHIP_Parm,018)
       PKGESHIP_Parm = Overlay(PHDR_PKG_STGID      ,PKGESHIP_Parm,027)
       PKGESHIP_Parm = Overlay(REXX_EXEC_MODE      ,PKGESHIP_Parm,028)
       PKGESHIP_Parm = Overlay(PHDR_PKG_CREATE_USER,PKGESHIP_Parm,029)
       PKGESHIP_Parm = Overlay(PHDR_PKG_UPDATE_USER,PKGESHIP_Parm,037)
       PKGESHIP_Parm = Overlay(PHDR_PKG_CAST_USER  ,PKGESHIP_Parm,045)
       PKGESHIP_Parm = Overlay(PHDR_PKG_NOTE1      ,PKGESHIP_Parm,054)
       PKGESHIP_Parm = Overlay(PHDR_PKG_NOTE2      ,PKGESHIP_Parm,114)
       PKGESHIP_Parm = Overlay(PHDR_PKG_NOTE3      ,PKGESHIP_Parm,174)
       PKGESHIP_Parm = Overlay(PHDR_PKG_NOTE4      ,PKGESHIP_Parm,234)
       PKGESHIP_Parm = Overlay(PHDR_PKG_NOTE5      ,PKGESHIP_Parm,294)
       PKGESHIP_Parm = Overlay(PHDR_PKG_NOTE6      ,PKGESHIP_Parm,354)
       PKGESHIP_Parm = Overlay(PHDR_PKG_NOTE7      ,PKGESHIP_Parm,414)
       PKGESHIP_Parm = Overlay(PHDR_PKG_NOTE8      ,PKGESHIP_Parm,474)
       If Substr(PECB_FUNCTION_LITERAL,1,4) = 'BACK' then,
          PKGESHIP_Parm = Overlay('BAK'            ,PKGESHIP_Parm,584)
       Else,
          PKGESHIP_Parm = Overlay('OUT'            ,PKGESHIP_Parm,584)
       /* Find PKGESHIP on GitHub in the folder-                 */
       /* endevor/Field-Developed-Programs/Package-Automation    */
       Call PKGESHIP PKGESHIP_Parm
       If TraceRQ = 'Y' then Say 'C1UEXTR7 is exiting @183 '
       Exit
       End
   If MyRc > 0 then Call SetExitReturnInfo
   /* Another way to Determine if Trace is wanted...  */
   If Substr(PHDR_PKG_NOTE5,1,5) = 'TRACE' then TraceRQ = 'Y'
   If TraceRQ = 'Y' then,
      Do
      Sa= 'CALL_REASON           = '    CALL_REASON
      Sa= 'PECB_FUNCTION_LITERAL = '    PECB_FUNCTION_LITERAL
      Sa= 'PECB_SUBFUNC_LITERAL  = '    PECB_SUBFUNC_LITERAL
      Sa= 'PECB_BEF_AFTER_LITERAL= '    PECB_BEF_AFTER_LITERAL
      Sa= 'PECB_PACKAGE_ID       = '    PECB_PACKAGE_ID
      Sa= 'MYSMTP_EMAIL_ID_SIZE  = '    MYSMTP_EMAIL_ID_SIZE
      End
   /* Early outs          ....                             */
   If PECB_FUNCTION_LITERAL = 'SETUP' then Exit
   /* Enforce packages to be Backout Enabled              */
   IF PREQ_BACKOUT_ENABLED /= 'Y' then,
      Do
      Message = 'C1UEXTR7 - Package made to be Backout enabled'
      MyRc        = 4
      hexAddress = D2X(Address_PREQ_BACKOUT_ENABLED)
      storrep = STORAGE(hexAddress,,'Y')
      Call SetExitReturnInfo
      Exit
      End;
   If TraceRQ = 'Y' then Say 'C1UEXTR7 is exiting @280 '
   EXIT
EvaluateParms:
 $numbers   = '0123456789.'   /* chars for numeric values   */
 RemainingParms = Strip(Parms)
 Do Until Words(Remainingparms) < 1
    Parse Var RemainingParms $keyword '=' RemainingParms
    $keyword = Strip($keyword)
    RemainingParms = Strip(RemainingParms,'L')
    $firstchar = Substr(RemainingParms,1,1)
    If $firstchar = '"' then $NumericValue = 0
    Else,
       Do
       $firstNonNumeric =,
          VERIFY(RemainingParms,$numbers || ' ')
       $NumericValue =,
          Substr(RemainingParms,$firstNonNumeric,1) = ';'
       End
    /* Value must be numeric, or be double quoted */
    If words($keyword) /= 1 |,
       DATATYPE($keyword,SYMBOL) /= 1 |,
       ($NumericValue = 0 & $firstchar /= '"') then,
       Do
       Parse var RemainingParms dropit ';' RemainingParms
       Say "Invalid syntax-" $keyword '=' dropit
       myAcct  = GETACCTC()
       myJobnr = GETJOBNR()
       parm="Invalid syntax-" command '=' dropit
       parm='Usr=' || USERID() 'Acct='myAcct dropit
       parm= parm  || ' jobnumber=' myJobnr
       parm=Left(parm,70)
       Address LINKMVS "WTO#MSG parm"
       Return 12
       End
    Else       /* double quoted value */
    If VERIFY($firstchar,$numbers) > 0 then,
       Do
       Parse var RemainingParms '"' $value '"' blanks ";" RemainingParms
       command  = $keyword '=' '"' || Strip($value) || '"'
       End
    Else       /* numeric value */
       Do
       Parse var RemainingParms $value ';' RemainingParms
       command  = $keyword '=' Strip($value)
       End
    RemainingParms = strip(RemainingParms)
    If TraceRQ = 'Y' then say command
    interpret command
 End; /* Do rexx# = 1 to Words(RemainingParms) */
 Return 0
Get_Site_Shipping_Variables:
  /* Get the related site-level options */
  WhereIam =  Strip(Left("@"MVSVAR(SYSNAME),8)) ;
  /* ShipSchedulingMethod can be set by C1System */
  interpret 'Call' WhereIam,
           "'ShipSchedulingMethod_"PackageSystem"'"
  ShipSchedulingMethod = Result
  If Wordpos(ShipSchedulingMethod,'Rules Notes One None') = 0 then,
    Do
    interpret 'Call' WhereIam "'ShipSchedulingMethod'"
    ShipSchedulingMethod = Result
    End
  Return
CheckExecutionWindow:
  /* Check the Execution window for immediate package Execution */
  curntTimestamp = Substr(DATE('S'),3) || '@'|| Substr(TIME(),1,5)
  startTimestamp = ConvertDate(PREQ_EXEC_START_DATE) ||'@'||,
                               PREQ_EXEC_START_TIME
  sa= curntTimestamp startTimestamp PREQ_EXEC_START_DATE
  If curntTimestamp < startTimestamp then,
     Do
     GoExecute = 'N'
     Return
     End
  ENDTimestamp = ConvertDate(PREQ_EXEC_END_DATE) ||'@'||,
                             PREQ_EXEC_END_TIME
  sa= curntTimestamp ENDTimestamp PREQ_EXEC_END_DATE
  If curntTimestamp > ENDTimestamp then GoExecute = 'N'
  Return
ConvertDate:
  alphaMonths = 'JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC'
  /* convert date from 24FEB26 format to a 260224 format */
  Arg dateToConvert
  ConvertedDay = Substr(dateToConvert,1,2)
  ConvertedMon = Substr(dateToConvert,3,3)
  ConvertedMon = WordPos(ConvertedMon,alphaMonths)
  ConvertedMon = Right(ConvertedMon,2,'0')
  ConvertedYear= Substr(dateToConvert,6,2)
  sa= thisYear ConvertedYear
  Converted_Date = Convertedyear || ConvertedMon || ConvertedDay
  return Converted_Date
SetExitReturnInfo:
   If TraceRQ = 'Y'             then Trace ?R
   whereami = 'SetExitReturnInfo'
   If TraceRQ = 'Y' then Say 'SetExitReturnInfo:    '
   hexAddress = D2X(Address_PECB_MESSAGE)
   storrep = STORAGE(hexAddress,,Message)
   hexAddress = D2X(Address_PECB_ERROR_MESS_LENGTH)
   storrep = STORAGE(hexAddress,,'0084'X)
   hexAddress = D2X(Address_PECB_MODS_MADE_TO_PREQ)
   storrep = STORAGE(hexAddress,,'Y')
   If MessageCode /= '    ' then,
      Do
      hexAddress = D2X(Address_PECB_MESSAGE_ID)
      storrep = STORAGE(hexAddress,,MessageCode)
      End
/* Set the return code for the exit                */
/*  for PECB-NDVR-EXIT-RC                          */
   hexAddress = D2X(Address_PECB_NDVR_EXIT_RC)
   If MyRc = 4 then,
      storrep = STORAGE(hexAddress,,'00000004'X)
   Else,
      storrep = STORAGE(hexAddress,,'00000008'X)
   RETURN ;
