/*  REXX  */
/*                                                                   */
/*      https://github.com/BroadcomMFD/broadcom-product-scripts      */
/*                                                                   */
/* This rexx is called by a package exit program, receives a         */
/* package name, compares the package content to SHIPRULE,           */
/* and may submit zero or more jobs to ship the package.             */
/* Subroutines BILDTGGR and PULLTGGR do a lot of the work.           */
/*                                                                   */
/* If uncommented, the exit Runs only 4 IBMUSER                      */
/* If USERID() /= 'IBMUSER' then Exit                                */
/*                                                                   */
/* THESE ROUTINES ARE DISTRIBUTED BY THE CA TECHNOLOGIES STAFF
   "AS IS".  NO WARRANTY, EITHER EXPRESSED OR IMPLIED, IS MADE
   FOR THEM.  CA TECHNOLOGIES CANNOT GUARANTEE THAT THE ROUTINES
   ARE ERROR FREE, OR THAT IF ERRORS ARE FOUND, THEY WILL BE
   CORRECTED.
*/
/*
   This REXX submits automatically 0 or more package shipping
   jobs.
*/
/* Runs only 4 userid = IBMUSER     (if uncommented)  */
/*
   If USERID() /= 'IBMUSER' then Exit
*/
   STRING = "ALLOC DD(SYSTSPRT) SYSOUT(A) "
   CALL BPXWDYN STRING;
   /* If a DDNAME of PKGESHIP is allocated, then Trace */
   WhatDDName = 'PKGESHIP'
   CALL BPXWDYN "INFO FI("WhatDDName")",
              "INRTDSN(DSNVAR) INRDSNT(myDSNT)"
   if Substr(DSNVAR,1,1) /= ' ' then TraceRc = 1;
   IF TraceRc = 1 then Trace R
   STRING = "ALLOC DD(SYSPRINT) SYSOUT(A) "
   CALL BPXWDYN STRING;
/* Variable settings for each site --->           */
   WhereIam =  WHERE@M1()
   RunUnderAltid  = 'Y' ;   /* Y/N  */
   /* If just shipping to one destination, then use          */
   /* ShipSchedulingMethod = 'None '                         */
   /* if there only a few  destinations (in notes), then use */
   /* ShipSchedulingMethod = 'Notes'                         */
   /* if using a RULES file and Trigger, then use            */
   /* ShipSchedulingMethod = 'Rules'                         */
   interpret 'Call' WhereIam "'ShipSchedulingMethod'"
   ShipSchedulingMethod = Result
   If ShipSchedulingMethod = 'One'   then,
      Do
      interpret 'Call' WhereIam "'Destination'"
      Destination          = Result
      interpret 'Call' WhereIam "'Hostprefix'"
      Hostprefix           = Result
      interpret 'Call' WhereIam "'Rmteprefix'"
      Rmteprefix           = Result
      interpret 'Call' WhereIam "'ModelMember'"
      ModelMember          = Result
      End
   interpret 'Call' WhereIam "'SHLQ'"
   SHLQ = Result
   interpret 'Call' WhereIam "'MyAUTHLibrary'"
   MyAUTHLibrary = Result
   interpret 'Call' WhereIam "'MyAUTULibrary'"
   MyAUTULibrary = Result
   interpret 'Call' WhereIam "'MyLOADLibrary'"
   MyLOADLibrary = Result
   interpret 'Call' WhereIam "'MySEN2Library'"
   MySEN2Library = Result
   interpret 'Call' WhereIam "'MySENULibrary'"
   MySENULibrary = Result
   interpret 'Call' WhereIam "'MyOPT2Library'"
   MyOPT2Library = Result
   interpret 'Call' WhereIam "'MyOPTNLibrary'"
   MyOPTNLibrary = Result
   interpret 'Call' WhereIam "'MyCLS0Library'"
   HSYSEXEC  = Result
   MyCLS0Library = HSYSEXEC
   interpret 'Call' WhereIam "'MyCLS2Library'"
   HSYSEXEC  = Result
   MyCLS2Library = HSYSEXEC
   interpret 'Call' WhereIam "'AltIDAcctCode'"
   AltIDAcctCode= Result
   interpret 'Call' WhereIam "'AltIDJobClass'"
   AltIDJobClass= Result
   interpret 'Call' WhereIam "'AltIDMsgClass'"
   AltIDMsgClass= Result
   interpret 'Call' WhereIam "'TransmissionModels'"
   TransmissionModels = Result
   MyHomeAddress  = 'node2.lvn.yoursite.net'
/* <---- Variable settings for each site          */
   ARG Parms ;
   If USERID() = '???????' then Trace ?R
   Notes.7  = Substr(PARMS,414,60) ;
   If Substr(Notes.7,1,5) = 'TRACE' then Trace r
   Package = Substr(PARMS,1,16) ;
   Environ = Substr(PARMS,18,08) ;
   Stage   = Substr(PARMS,27,01) ;
   CREATE_USER = Substr(PARMS,29,08) ;
   UPDATE_USER = Substr(PARMS,37,08) ;
   CAST_USER = Substr(PARMS,45,08) ;
   Notes.1  = Substr(PARMS,054,60) ;
   Notes.2  = Substr(PARMS,114,60) ;
   Notes.3  = Substr(PARMS,174,60) ;
   Notes.4  = Substr(PARMS,234,60) ;
   Notes.5  = Substr(PARMS,294,60) ;
   Notes.6  = Substr(PARMS,354,60) ;
   Notes.7  = Substr(PARMS,414,60) ;
   Notes.8  = Substr(PARMS,474,60) ;
   ShipOutput = Substr(PARMS,584,03) ;
   Trace off
   If ShipOutput = 'BAK' & TraceRc = 1 then Trace ?R
   If ShipOutput /='BAK' & TraceRc = 1 then Trace r
   TYPRUN = ' '
   If ShipOutput /= 'BAK' | TraceRc = 1 then,
      SAY 'Running PKGESHIP;'
/*                                                                    */
/* This Rexx participates in the submission of Endevor Package        */
/* Shipment jobs. It is called by the Endevor exit program C1UEXT07   */
/*                                                                    */
/* First. See if the package has any package backouts ...             */
/*                                                                    */
   Call CSV_to_List_Package_Id
   If BACKOUT_RCD_EXIST = 'N' then Exit
/* Allocate and prepare files for updating the shipment transaction   */
/* file, and submitting package shipments for those scheduled for     */
/* immediate submission.                                              */
/*                                                                    */
   Userid = USERID()
   Date8  = DATE('S')
   Date6  = substr(Date8,3);
   Temp   = TIME('L')
   Time8  = Substr(Temp,1,2) ||,
            Substr(Temp,4,2) ||,
            Substr(Temp,7,2) ||,
            Substr(Temp,10,2) ;
   Time6  = Substr(Temp,1,2) ||,
            Substr(Temp,4,2) ||,
            Substr(Temp,7,2) ;
   TodaysDate = DATE('S') ;
   NOW  = TIME(L);
   HOUR = SUBSTR(NOW,1,2) ;
   IF HOUR = '00' THEN HOUR = '0'
   MINUTE = SUBSTR(NOW,4,2) ;
   CurrentTime= HOUR || MINUTE ;
   SENDNODE =  MVSVAR(SYSNAME)
/* ShipOutput = 'OUT' or 'BAK' */
   VDDRSPFX   = 'IBMUSER.NDVR'
   $All_VARIABLES = "Jobname Destination Userid ",
         "PkgExecJobname MyOPTNLibrary MyAUTULibrary ",
         "MyAUTHLibrary MyLOADLibrary MyCLS0Library ",
         "MyHomeAddress ",
         "Package Date8 Date6 Time8 Time6 Notify ",
         "Typrun SENDNODE $delimiter ShipOutput  ",
         "SHLQ MySEN2Library MySENULibrary MyOPT2Library ",
         "MyCLS2Library AltIDAcctCode AltIDJobClass ",
         "AltIDMsgClass VDDHSPFX VDDRSPFX HSYSEXEC OUT"
   /* If Scheduling Package Shipments, then BILDTGGR             */
   /* determines sites and updates the Trigger file  .           */
   /* If any Sites can receive Shipments immediately, then       */
   /* BILDTGGR indicates so in its Return code.                  */
   If ShipSchedulingMethod = 'Rules' then,
      Do
      Call BILDTGGR Parms;
      BildRC = RESULT ;
      /* The immediate shipments are queued by BILDTGGR in a Table  */
      If BildRC = 1 then,
         Do
         PULLTGGRParms = Userid'.PULLTGGR' MySEN2Library
         Call PULLTGGR PULLTGGRParms ;
         End
      End /*  SchedulingPackageShipBundle */
/*                                                                    */
/* All Done                                                           */
/*                                                                    */
   Exit
AllocateModel:
/*                                                                    */
/* Submit immediate package shipment jobs                             */
/*                                                                    */
/*    Allocate files                                                  */
/*                                                                    */
/*    A Model is used for building Package Shipment JCL               */
/*                                                                    */
   STRING = 'ALLOC DD(MODEL) ',
            "DA('"MySEN2Library"("ModelMember")') SHR REUSE "
   CALL BPXWDYN STRING;
   Return;
GetDestinationInfo:
   /* Set values for Hostprefix and Rmteprefix */
   /*     From the site definition             */
   /*  Call API to Get Destination information  */
   ADDRESS TSO
   "ALLOC F(BSTAPI) DA(*) REUSE "
   "ALLOC F(BSTERR) DA(*) REUSE "
   STRING = "ALLOC DD(APIMSGS) LRECL(133) BLKSIZE(13300) ",
              " DSORG(PS) ",
              " SPACE(1,1) RECFM(F,B) TRACKS ",
              " NEW UNCATALOG REUSE ";
   CALL BPXWDYN STRING;
   STRING = "ALLOC DD(APILIST) LRECL(2048) BLKSIZE(22800) ",
              " DSORG(PS) ",
              " SPACE(1,1) RECFM(V,B) TRACKS ",
              " NEW UNCATALOG REUSE ";
   CALL BPXWDYN STRING;
   /*  Call API to Get Destination information  */
   parm =    Left(Destination'*',7)
   ADDRESS TSO "CALL *(APIALDST) '"||parm||"'"
   RETURN_RC = RC ;
   If RETURN_RC > 0 then,
      DO
      SA= 'CANNOT GET INFORMATION FROM ENDEVOR' ;
      EXIT
      END ;
   "EXECIO * DISKR APILIST ( Stem apiDestinations. FINIS"
   STRING = "FREE  DD(APILIST)"
   CALL BPXWDYN STRING;
   STRING = "FREE  DD(APIMSGS)"
   CALL BPXWDYN STRING;
   STRING = "FREE  DD(BSTAPI) "
   CALL BPXWDYN STRING;
   STRING = "FREE  DD(BSTERR) "
   CALL BPXWDYN STRING;
   Return ;
CSV_to_List_Package_Id:
  /* To Search the package shipping table, we need the Entry       */
  /*    Environment information as a search criteria.              */
  /*    Get it from the USER-DATA field on the 1st element.        */
   STRING = "ALLOC DD(C1MSGS1) DUMMY "
   CALL BPXWDYN STRING;
   STRING = "ALLOC DD(BSTERR) DUMMY "
   CALL BPXWDYN STRING;
   STRING = "ALLOC DD(BSTAPI) DUMMY "
   CALL BPXWDYN STRING;
   STRING = "ALLOC DD(EXTRACTM) LRECL(4000) BLKSIZE(32000) ",
              " DSORG(PS) ",
              " SPACE(1,5) RECFM(F,B) TRACKS ",
              " NEW UNCATALOG REUSE ";
   CALL BPXWDYN STRING;
   STRING = "ALLOC DD(BSTIPT01) LRECL(80) BLKSIZE(800) ",
              " DSORG(PS) ",
              " SPACE(1,5) RECFM(F,B) TRACKS ",
              " NEW UNCATALOG REUSE ";
   CALL BPXWDYN STRING;
   QUEUE "LIST PACKAGE ID '"Package"'"
   QUEUE "     TO DDNAME 'EXTRACTM' "
   QUEUE "     ."
   "EXECIO" QUEUED() "DISKW BSTIPT01 (FINIS ";
   CALL BPXWDYN "INFO FI(CONLIB) INRTDSN(DSNVAR) INRDSNT(myDSNT)"
   if RESULT = 0 then,
      Do
      CSVParm = 'DDN:CONLIB,BC1PCSV0'
      ADDRESS LINKMVS 'CONCALL' "CSVParm"
      End
   Else,
      ADDRESS LINK 'BC1PCSV0'   ;  /* load from authlib */
/* ADDRESS TSO  'ISRDDN' */
   call_rc = rc ;
  "EXECIO * DISKR EXTRACTM (STEM API. finis"
   STRING = "FREE DD(EXTRACTM)" ;
   CALL BPXWDYN STRING;
   STRING = "FREE DD(BSTIPT01)" ;
   CALL BPXWDYN STRING;
   STRING = "FREE DD(C1MSGS1)" ;
   CALL BPXWDYN STRING;
   STRING = "FREE DD(BSTERR)" ;
   CALL BPXWDYN STRING;
   STRING = "FREE DD(BSTAPI)" ;
   CALL BPXWDYN STRING;
  IF API.0 < 2 THEN RETURN;
  $table_variables= Strip(API.1,'T')
  $table_variables = translate($table_variables,"_"," ") ;
  $table_variables = translate($table_variables," ",',"') ;
  $table_variables = translate($table_variables,"@","/") ;
  $table_variables = translate($table_variables,"@",")") ;
  $table_variables = translate($table_variables,"@","(") ;
  $table_variables = translate($table_variables,"_","-") ;
  Do rec# = 2 to API.0
     $detail = API.rec#
     /* Parse the Detail record until done */
     Do $column =  1 to Words($table_variables)
        Call ParseDetailCSVline
     End
  End; /* Do rec# = 1 to API.0 */
  RETURN ;
ParseDetailCSVline:
  /* Find the data for the current $column */
  $dlmchar = Substr($detail,1,1);
  If $dlmchar = "'" then,
     Do
     SA= 'parsing with single quote '
     PARSE VAR $detail "'" $temp_value "'" $detail ;
     If Substr($detail,1,1) = ',' then,
        $detail = Strip(Substr($detail,2),'L')
     End
  Else,
  If $dlmchar = '"' then,
     Do
     SA= 'parsing with double quote '
     PARSE VAR $detail '"' $temp_value '"' $detail ;
     If Substr($detail,1,1) = ',' then,
        $detail = Strip(Substr($detail,2),'L')
     End
  Else,
  If $dlmchar = ',' then,
     Do
     SA= 'parsing with comma        '
     PARSE VAR $detail ',' $temp_value ',' $detail ;
     If Substr($detail,1,1)/= ',' then,
        $detail = "," || $detail
        $detail = Strip(Substr($detail,2),'L')   */
     End
  Else,
  If Words($detail) = 0 then,
     $temp_value = ' '
  Else,
     Do
     SA= 'parsing with comma        '
     PARSE VAR $detail $temp_value ',' $detail ;
     Sa= '$temp_value=>' $temp_value '<'
     End
  $temp_value = STRIP($temp_value) ;
  $rslt = $temp_value
  $rslt = Strip($rslt,'B','"')
  $rslt = Strip($rslt,'B',"'")
  if Length($rslt) < 1 then $rslt = ' '
  variable_name = WORD($table_variables,$column)
  If variable_name = 'BACKOUT_RCD_EXIST' then,
     BACKOUT_RCD_EXIST = $rslt
  RETURN ;
