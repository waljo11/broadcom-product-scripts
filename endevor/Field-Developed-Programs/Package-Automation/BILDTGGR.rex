/*  REXX  */
/*                                                                   */
/*      https://github.com/BroadcomMFD/broadcom-product-scripts      */
/*                                                                   */
/* This rexx is a called subroutine.                                 */
/* From data in SHIPRULE, BILDTGGR updates the Trigger file          */
/* for each expected shipment. PULLTGGR submits package ship jobs.   */
/*                                                                   */
   /* If a DDNAME of BILDTGGR is allocated, then Trace */
   WhatDDName = 'BILDTGGR'
   CALL BPXWDYN "INFO FI("WhatDDName")",
              "INRTDSN(DSNVAR) INRDSNT(myDSNT)"
   if Substr(DSNVAR,1,1) /= ' ' then TraceRc = 1;
   IF TraceRc = 1 then Trace R
/* Variable settings for each site --->           */
   /* Determine the DSORG for TriggerFileNAme     */
   /* x = LISTDSI("'"TriggerFileName"'" RECALL ); */
   TriggerFileDsorg = 'PS' ;     /* VS / PS .... */
   WhereIam =  WHERE@M1()
   interpret 'Call' WhereIam "'MyCLS2Library'"
   MyCLS2Library = Result
   Say 'Running BILDTGGR in' MyCLS2Library
   interpret 'Call' WhereIam "'TriggerFileName'"
   TriggerFileName = Result
   interpret 'Call' WhereIam "'MyDATALibrary'"
   MyDATALibrary = Result
   ShipRules       = MyDATALibrary"(SHIPRULE)"
   interpret 'Call' WhereIam "'MySEN2Library'"
   MySEN2Library = Result
/* <---- Variable settings for each site          */
/*                                                                    */
/* This Rexx creates Trigger entrires for a package just executed.    */
/* It is called by the Endevor exit program C1UEXT07   */
/*                                                                    */
/*                                                                    */
   STRING = "ALLOC DD(SYSTSPRT) SYSOUT(A) "
   CALL BPXWDYN STRING;
   /* Do not allow the Trace to start until after the allocation
      above for SYSTSPRT.
   */
   ARG Parms ;
/* if called by zowe, then this  is  the Argument ....   */
   If Length(Parms) < 18 then,
      Do
      Package = Translate(Parms,' ',"'")
      Package = Strip(Package)
      Say Length(Package) Package
      End /* If Length(Parms) < 18 */
   Else
      Do
      /* if called by exit, then these are the Arguments.... */
      Notes.7  = Substr(PARMS,414,60) ;
      If Substr(Notes.7,1,5) = 'TRACE' then Trace r
      Package = Substr(PARMS,1,16) ;
/*  No longer attempting to leverage promotion packages
      Environ = Substr(PARMS,18,08) ;
     pkgStage = Substr(PARMS,27,01) ; */
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
      End /*  Else  */
   Say "@082 Package =" Package
   Value. = ''
   Userid    = CAST_USER  ;
   MyRC = 0 ;
   /* Intitialize a list of shipments for this package */
   Shipment_List = ''
   /*   Install Date 06 JUN 2013 15:22   */
   /*   ----+----1----+----2----+----3-- */
   rulSt  = '_'
   InstallDate = Word(Notes.8,5) ;
   If InstallDate = '' | Word(Notes.8,1) /= 'SHIP' then,
      Do
      InstallDate = DATE('S')
      InstallTime = '0000'
      rulSt  = 'R' ; /* This Trigger file entry to be Reviewed */
      End
   Else,
      Do
      InstallTime = Word(Notes.8,6) ;
      InstallTime = Substr(Installtime,1,2) || Substr(Installtime,4)
      ListMonths =,
        "JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC" ;
      mon = Substr(InstallDate,3,3) ;
      mon#= Right(Wordpos(mon,ListMonths),2,'0') ;
      year = Substr(InstallDate,6)
      Century = '20'    ;
      day  = Substr(InstallDate,1,2) ;
      InstallDate = Century || year || mon# || day
      End
   Call DateConvert ;
   /* Capture Content of Rules into Rexx Stem array format */
   Call AllocateandReadRules;
   if TraceRc = 1 then Trace r
   $rulHeading_Variable_count = Words($heading.SHIPRULE)
/* Examine Package actions and                           */
/* compare each against the Rules stem array data        */
/* make an entry into the ImmediateTrigger stem array    */
   Call CSV_to_List_Package_Actions ;
   sa = pkgEnvironment pkgStage
   If Words(Shipment_List) > 0 then,
      Do
      Call FreeTriggerFile ;
      Say 'BILDTGGR: Found these destinations -' Shipment_List
      Exit(1)
      End
/*                                                                    */
/* All Done                                                           */
/*                                                                    */
   Exit(0)
AllocateandReadRules:
   Say "AllocateandReadRules   "
   If TraceRc = 1 then Say 'AllocateandReadRules+             '
   /* Call utility to get SHIPRULE details */
   /* Save ShipRule in stem array data         */
   STRING = "ALLOC DD(TABLE) DA('"ShipRules"') SHR REUSE"
   CALL BPXWDYN STRING;
   "EXECIO * DISKR TABLE (STEM $tableShip. FINIS"
   CALL BPXWDYN "FREE DD(TABLE)"
   If Substr(HeadingChar,1,1) /= ' ' then,
      $TableHeadingChar = HeadingChar ;
   $TableCommentPrefix = "**";  /* For TABLE   comments   */
   $TableHeadingChar  = "*" ;   /* For TABLE   heading    */
   /* Find Heading for Table */
   DO $tbl = 1 TO $tableShip.0     /* Look for header or CSV format*/
      /* If not finding a heading, just get out */
      If Substr($tableShip.$tbl,1,2) = $TableCommentPrefix then Iterate ;
      If Substr($tableShip.$tbl,1,1) /= $TableHeadingChar then Iterate ;
      Leave
   End
   If Substr($tableShip.$tbl,1,1) /= $TableHeadingChar then Exit(8) ;
   /* A Heading was found, get column info for Table */
   Call ShipRules_Process_Heading
   Call ShipRules_CaptureTableContent
   Return
/* Process the table heading           */
ShipRules_Process_Heading :
   Say "ShipRules_Process_Heading   "
   $LastWord = Word($tableShip.$tbl,Words($tableShip.$tbl));
   If DATATYPE($LastWord) = 'NUM' then,
      Do
      Say 'Please remove sequence numbers from the Table'
      Exit(12)
      End
   $tmprec = Substr($tableShip.$tbl,2) ;
   $PositionSpclChar = POS('-',$tmprec) ;
   If $PositionSpclChar = 0 then,
      $PositionSpclChar = POS('*',$tmprec) ;
   $tmpreplaces = '-,.'$TableHeadingChar ;
   $tmprec = TRANSLATE($tmprec,' ',$tmpreplaces);
   $table_variables = strip($tmprec);
   $Heading_Variable_count = WORDS($table_variables) ;
   If $PositionSpclChar = 0 then return
   If $Heading_Variable_count /=,
      Words(Substr($tableShip.$tbl,2)) then,
      Do
      Say 'Invalid table Heading:' $tableShip.$tbl
      exit(12)
      End
   $heading = Overlay(' ',$tableShip.$tbl,1); /* Space leading * */
   Do $pos = 1 to $Heading_Variable_count
      $headingVariable = Word($table_variables,$pos) ;
      If $headingVariable = "$my_rc" then,
         $headingVariable = "$Temp_RC"
      $tmp = Wordindex($heading,$pos) ;
      $ShipRule_Starting_$pos.$headingVariable = $tmp
      $tmp = $tmp + Length(Word($heading,$pos)) -1 ;
      $ShipRule_$Ending_$pos.$headingVariable = $tmp
      Sa=y "@212" $headingVariable,
          $ShipRule_Starting_$pos.$headingVariable $tmp
   end; /* DO $pos = 1 to $Heading_Variable_count */
   Return ;
ShipRules_CaptureTableContent:
   Say "ShipRules_CaptureTableContent "
   DO $tbl = 1 TO $tableShip.0     /* Capture content of deail recs*/
      $headingVariable = 'FirstChar'
      Value.$headingVariable.$tbl = Substr($tableShip.$tbl,1,1)
      If Substr($tableShip.$tbl,1,2) = $TableCommentPrefix then Iterate ;
      If Substr($tableShip.$tbl,1,1) = $TableHeadingChar then Iterate ;
      Do $pos = 1 to $Heading_Variable_count
         $headingVariable = Word($table_variables,$pos) ;
         Starts=$ShipRule_Starting_$pos.$headingVariable
         Ends  =$ShipRule_$Ending_$pos.$headingVariable
         len = Ends - Starts +1
         $temp_value = Strip(Substr($tableShip.$tbl,Starts,len))
         Value.$headingVariable.$tbl = $temp_value
         Say '@231 Rules:' $tbl $headingVariable '=' $temp_value
      End; /* DO $pos = 1 to $Heading_Variable_count */
   End /* DO $tbl = 1 TO $tableShip.0 */
   LastShiprec# = $tableShip.0
   Return ;
/*                                                                    */
/* Convert Date formats                                               */
/*                                                                    */
AllocateTriggerForMod:
   Say "AllocateTriggerForMod         "
   STRING = "ALLOC DD(TRIGGER)",
              " DA('"TriggerFileName"') MOD REUSE"
   seconds = '000005' /* Number of Seconds to wait if needed */
   Do Forever  /* or at least until the file is available */
      CALL BPXWDYN STRING;
      MyResult = RESULT ;
      If MyResult = 0 then Leave
      Say 'BILDTGGR is waiting for' TriggerFileName
      Call WaitAwhile
   End /* Do Forever */
   Return ;
CreateNewTriggerEntry:
   Say "CreateNewTriggerEntry         "
   If TraceRc = 1 then Say 'CreateNewTriggerEntry+            '
   If TraceRc = 1 then Trace r
   Jobnumb   = '  ';
   Time = rulTime ;
   Say "@271 Package =" Package
   Say 'My_BILDTGGR Creating new Trigger file Entry'
   rulAdjustDate = Strip(rulAdjustDate,'L','+') ;
   rulAdjustDate = Strip(rulAdjustDate)
   #days = DATE('B')
   #days = DATE('B',InstallDate,'S') ;
   if rulAdjustDate < '0' then rulAdjustDate = '0'
   #days = #days + rulAdjustDate ;
   TriggerDate = DATE('S',#days,'B') ;
   Date = DATE('S',#days,'B') ;                                        X
   Trigger = Copies(' ',400) ;
   $Heading_TriggerVar_count = WORDS($trigger_variables) ;
   Do $pos = 1 to $Heading_TriggerVar_count
      $HeadingVariable = Word($trigger_variables,$pos) ;
      StartPos = $TGGR_Starting_$pos.$HeadingVariable
      /* These values come from SHIPRULES  */
      /* Build ...pos variables and values */
      If $HeadingVariable = 'Package' then tmpValue = Package
      Else,
      If $HeadingVariable = 'Date'    then tmpValue = TriggerDate
      Else,
      If $HeadingVariable = 'Time'    then tmpValue = Time
      Else tmpValue = Value.$HeadingVariable.rul#
      If length(tmpValue) > 0 then,
         Trigger = Overlay(tmpValue,Trigger,StartPos)
   End; /* DO $pos = 1 to $Heading_TriggerVar_count */
   Sa= Trigger
   Push Trigger
   "EXECIO 1 DISKW TRIGGER (FINIS"
   Return ;
FreeTriggerFile:
   STRING = "FREE DD(TRIGGER)"
   CALL BPXWDYN STRING;
   Return ;
/*                                                                    */
/* Convert Date formats                                               */
/*                                                                    */
DateConvert:
  /* Date may be in this format  6/15/2013 */
  tmpdate = Translate(InstallDate,' ','/') ;
  If Wordlength(tmpdate,1) < '4' then do
  InstallDatenumeric = Word(tmpdate,3)    ||,
               Right(Word(tmpdate,1),2,'0') ||,
                     Word(tmpdate,2)       ;
  end
  else do
  InstallDatenumeric = Word(tmpdate,1) || ,
                       Word(tmpdate,2) ||,
                       Word(tmpdate,3)   ;
  end
  Return
  if Substr(InstallDate,1,1) = '0' then
     InstallDate = Substr(InstallDate,2)
  MONTHSLISTUPPER = 'JAN FEB MAR APR MAY JUN JUL AUG SEP NOV DEC'
  monthslistlower = 'Jan Feb Mar Apr May Jun Jul Aub Sep Nov Dec'
  MONTHUPPER = Word(InstallDate,2) ;
  pos = Wordpos(MONTHUPPER,MONTHSLISTUPPER)
  monthlower = Word(monthslistlower,pos) ;
  InstallDate = Word(InstallDate,1) monthlower Word(InstallDate,3)
  InstallDatenumeric = DATE(S,InstallDate)
  Sa= InstallDate  InstallDatenumeric
  Return
WaitAwhile:
  /*                                                               */
  /* A resource is unavailable. Wait awhile and try                */
  /*   accessing the resource again.                               */
  /*                                                               */
  /*   The length of the wait is designated in the parameter       */
  /*   value which specifies a number of seconds.                  */
  /*   A parameter value of '000003' causes a wait for 3 seconds.  */
  /*                                                               */
  seconds = Abs(seconds)
  seconds = Trunc(seconds,0)
  Say "Waiting for" seconds "seconds at " DATE(S) TIME()
  /* AOPBATCH and BPXWDYN are IBM programs */
  CALL BPXWDYN  "ALLOC DD(STDOUT) DUMMY SHR REUSE"
  CALL BPXWDYN  "ALLOC DD(STDERR) DUMMY SHR REUSE"
  CALL BPXWDYN  "ALLOC DD(STDIN) DUMMY SHR REUSE"
  /* AOPBATCH and BPXWDYN are IBM programs */
  parm = "sleep "seconds
  Address LINKMVS "AOPBATCH parm"
  Return
Process_Trigger_Heading :
   Say "Process_Trigger_Heading       "
   "EXECIO 1 DISKR TRIGGER (Stem $tableTggr. FINIS"
/* Get layout of TRIGGER file from heading */
   $tbl = 1 ;
   $TableHeadingChar = '*'
   $LastWord = Word($tableTggr.$tbl,Words($tableTggr.$tbl));
   If DATATYPE($LastWord) = 'NUM' then,
      Do
      Say 'Please remove sequence numbers from the Table'
      Exit(12)
      End
   $tmprec = Substr($tableTggr.$tbl,2) ;
   $PositionSpclChar = POS('-',$tmprec) ;
   If $PositionSpclChar = 0 then,
      $PositionSpclChar = POS('*',$tmprec) ;
   $tmpreplaces = '-,.'$TableHeadingChar ;
   $tmprec = TRANSLATE($tmprec,' ',$tmpreplaces);
   TGGR_variables = strip($tmprec);
   TGGR_Variable_count = WORDS(TGGR_variables) ;
   If TGGR_Variable_count /=,
      Words(Substr($tableTggr.$tbl,2)) then,
      Do
      Say 'Invalid table Heading:' $tableTggr.$tbl
      exit(12)
      End
   $TGGR_heading = Overlay(' ',$tableTggr.$tbl,1); /* Space leading */
   Do $pos = 1 to TGGR_Variable_count
      $TGGR_headingVariable = Word(TGGR_variables,$pos) ;
      $tmp = Wordindex($TGGR_heading,$pos) ;
      $TGGR_Starting_$pos.$TGGR_headingVariable = $tmp
      $tmp = $tmp + Length(Word($TGGR_heading,$pos)) -1 ;
      $TGGR_Ending_$pos.$TGGR_headingVariable = $tmp
      Say $TGGR_headingVariable,
          $TGGR_Starting_$pos.$TGGR_headingVariable $tmp
   end; /* DO $pos = 1 to TGGR_Variable_count */
   $TGGR_heading = Translate($TGGR_heading,' ','-*')
   $trigger_variables = $TGGR_heading
   Return ;
CSV_to_List_Package_Actions:
   Say "CSV_to_List_Package_Actions   "
  /* To Search the package shipping table, we need the Entry       */
  /*    Environment information as a search criteria.              */
  /*    Get it from the USER-DATA field on the 1st element.        */
   Say "@453 Package =" Package
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
   QUEUE "LIST PACKAGE ACTION FROM PACKAGE '"Package"'"
   QUEUE "     TO DDNAME 'EXTRACTM' "
   QUEUE "     ."
   "EXECIO" QUEUED() "DISKW BSTIPT01 (FINIS ";
   ADDRESS LINK 'BC1PCSV0'   ;  /* load from authlib */
/* ADDRESS TSO  'ISRDDN' */
   call_rc = rc ;
  "EXECIO * DISKR EXTRACTM (STEM CSV. finis"
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
  /* To Search the package action data in CSV format.              */
  /* Identify matches with Rules file, determining Ship Dests      */
  IF CSV.0 < 2 THEN RETURN;
  /* CSV data heading - showing CSV variables */
  $table_variables= Strip(CSV.1,'T')
  $table_variables = translate($table_variables,"_"," ") ;
  $table_variables = translate($table_variables," ",',"') ;
  $table_variables = translate($table_variables,"@","/") ;
  $table_variables = translate($table_variables,"@",")") ;
  $table_variables = translate($table_variables,"@","(") ;
  Do rec# = 2 to CSV.0
     $detail = CSV.rec#
     Drop SBS_NAME_@T@
     /* Parse the Detail record until done */
     Do $column =  1 to Words($table_variables)
        Call ParseDetailCSVline
     End
     IF Substr(ENV_NAME_@T@,1,1) = ' ' then,
        Do
        ENV_NAME_@T@ = ENV_NAME_@S@ ;
        STG_#_@T@    = STG_#_@S@  ;
        STG_NAME_@T@ = STG_NAME_@S@ ;
        STG_ID_@T@   = STG_ID_@S@   ;
        SYS_NAME_@T@ = SYS_NAME_@S@ ;
        SBS_NAME_@T@ = SBS_NAME_@S@ ;
        TYPE_NAME_@T@ = TYPE_NAME_@S@
        End
     IF Substr(SBS_NAME_@T@,1,1) = ' ' then,
        SBS_NAME_@T@ = SBS_NAME_@S@ ;
     pkgStage = STG_ID_@T@
     If TraceRc /= 1 then,
        Do
        Say '@530'
        Say 'ELM_ACT'        ELM_ACT
        Say 'ENV_NAME_@S@'   ENV_NAME_@S@
        Say 'SYS_NAME_@S@'   SYS_NAME_@S@
        Say 'SBS_NAME_@S@'   SBS_NAME_@S@
        Say 'TYPE_NAME_@S@'  TYPE_NAME_@S@
        Say 'STG_ID_@S@'     STG_ID_@S@
        Say 'ENV_NAME_@T@'   ENV_NAME_@T@
        Say 'SYS_NAME_@T@'   SYS_NAME_@T@
        Say 'SBS_NAME_@T@'   SBS_NAME_@T@
        Say 'TYPE_NAME_@T@'  TYPE_NAME_@T@
        Say 'STG_ID_@T@'     STG_ID_@T@
        End
     If TraceRc = 1 then Trace r
     Sa= 'Messages from BILDTGGR:'
     pkgElement = ELM_@S@
     pkgEnvironment = ENV_NAME_@T@
     pkgStage       = STG_ID_@T@
     pkgSystem      = SYS_NAME_@T@
     pkgSubsys      = SBS_NAME_@T@
     pkgType        = TYPE_NAME_@T@
     RuleMatch  = 'N'
     Call DoesPkgMatchRule;
  End; /* Do rec# = 1 to CSV.0 */
  RETURN ;
DoesPkgMatchRule:
  Say "DoesPkgMatchRule              "
  If TraceRc = 1 then Say 'DoesPkgMatchRule+                 '
  Do rul# = 2 to LastShiprec#
     $headingVariable = 'FirstChar'
     FirstChar = Value.$headingVariable.rul#
     If FirstChar = '*' then Iterate;
     If TraceRc = 1 then TRACE R
     Sa= 'BILDTGGR: Comparing pkg with rul#' rul# pkgElement,
               pkgEnvironment pkgStage pkgSystem
     $headingVariable = 'Environment'
     rulEnvironment = Value.$headingVariable.rul#
     If TestMatch(pkgEnvironment,rulEnvironment) /= 1 then Iterate;
     Environment = pkgEnvironment
     $headingVariable = 'Stage'
     rulStage = Value.$headingVariable.rul#
     If TestMatch(pkgStage,rulStage) /= 1 then Iterate;
     Stage  = pkgStage
     $headingVariable = 'System'
     rulSystem = Value.$headingVariable.rul#
     If TestMatch(pkgSystem,rulSystem) /= 1 then Iterate;
     System = pkgSystem
     $headingVariable = 'Subsys'
     rulSubsys = Value.$headingVariable.rul#
     If TestMatch(pkgSubsys,rulSubsys) /= 1 then Iterate;
     Subsys = pkgSubsys
     $headingVariable = 'Destination'
     newDestination= Value.$headingVariable.rul#
     If TraceRc = 1 then,
        Say 'found newDestination=' newDestination
     Say '@599' Environment Stage rulSystem System,
         rulSubsys Subsys newDestination
     if Length(newDestination) > 7 |,
        Pos('.',newDestination) > 0 |,
        Wordpos(newDestination,Shipment_List) > 0 then,
        Iterate ;
     Shipment_List = newDestination Shipment_List
     If TraceRc = 1 then,
        Say 'Shipment_List =' Shipment_List
     /* If finding first destination for shipping     */
     /* then, allocate Trigger file for DISP=MOD      */
     If Words(Shipment_List) = 1 then,
        Do
        Call AllocateTriggerForMod
        Call Process_Trigger_Heading
        End
     Destination = newDestination
     $headingVariable = 'Date'
     rulAdjustDate = Value.$headingVariable.rul#
     If TraceRc = 1 then,
        Say 'found rulAdjustDate =' rulAdjustDate
     $headingVariable = 'Time'
     rulTime       = Value.$headingVariable.rul#
     If TraceRc = 1 then,
        Say 'found rulTime       =' rulTime
     /* Capture remaining values from Rules file ..    */
     AlreadyAssigned =,
        'Environment Stage System Subsys ',
        'Type Destination Element Date Time '
     If TraceRc = 1 then Trace r
     /* If the package shipment can be done now....    */
     /* Set the return code to 1               ....    */
     Drop Date
     rulDate    = Value.Date.rul#
     if rulDate = ' ' | Length(rulDate) > 5 then,
        rulDate = '+0'
     if rulTime = ' ' | Length(rulTime) > 5 then,
        rulTime = '0000'
     If rulDate = '+0' & rulTime = '0000' then,
        MyRC = 1 ;
     /* Variables are now available either from        */
     /*  the Endevor package or from the Rules File    */
     Call CreateNewTriggerEntry
  End; /* Do rul# = 2 to LastShiprec# */
  RETURN ;
TestMatch:
  Arg thisString,Mask ;
      Say '@660' thisString Mask
      If Mask  = '*' then Return(1)
      Mask     = Strip(Mask,'T',"*")
      lenMask  = Length(Mask)
      Return ABBREV(thisString,Mask)
/*                                                                    */
ParseDetailCSVline:
  Sa= "ParseDetailCSVline            "
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
  $keyword = WORD($table_variables,$column)
  Select
    When $keyword = 'ELM_ACT'       then ELM_ACT      = $rslt
    When $keyword = 'ENV_NAME_@S@'  then ENV_NAME_@S@ = $rslt
    When $keyword = 'SYS_NAME_@S@'  then SYS_NAME_@S@ = $rslt
    When $keyword = 'SBS_NAME_@S@'  then SBS_NAME_@S@ = $rslt
    When $keyword = 'TYPE_NAME_@S@' then TYPE_NAME_@S@= $rslt
    When $keyword = 'STG_ID_@S@'    then STG_ID_@S@   = $rslt
    When $keyword = 'ENV_NAME_@T@'  then ENV_NAME_@T@ = $rslt
    When $keyword = 'SYS_NAME_@T@'  then SYS_NAME_@T@ = $rslt
    When $keyword = 'SBS_NAME_@T@'  then SBS_NAME_@T@ = $rslt
    When $keyword = 'TYPE_NAME_@T@' then TYPE_NAME_@T@= $rslt
    When $keyword = 'STG_ID_@T@'    then STG_ID_@T@   = $rslt
    Otherwise  NOP
  End
  RETURN ;
