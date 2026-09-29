/*  REXX                                                             */
/*                                                                   */
/*      https://github.com/BroadcomMFD/broadcom-product-scripts      */
/*                                                                   */
/* This rexx is a called subroutine.                                 */
/* It can be called by PKGESHIP during exit processing, or           */
/* by a package Sweep job. (See SWEEPJOB) .                          */
/*                                                                   */
/* From data in SHIPRULE, BILDTGGR updates the Trigger file          */
/* for each expected shipment. PULLTGGR submits package ship jobs.   */
/*                                                                   */
   /* If a DDNAME of PULLTGGR is allocated, then Trace */
   CALL BPXWDYN "INFO FI(PULLTGGR) INRTDSN(DSNVAR) INRDSNT(myDSNT)"
   if RESULT = 0 then TraceRc = 1;
   If TraceRc = 1 then Trace r
   Value. = ''
   LocalVariables = ''
/* PkgExecJobname = MVSVAR('SYMDEF',JOBNAME )   Returns JOBNAME */
/* Variable settings for each site --->           */
   WhereIam =  WHERE@M1()
   $headingVariable = 'MyCLS0Library'
   interpret 'Call' WhereIam $headingVariable
   Value.$headingVariable = Result
   MyCLS0Library = Result
   LocalVariables = LocalVariables $headingVariable
   $headingVariable = 'MyCLS2Library'
   interpret 'Call' WhereIam $headingVariable
   Value.$headingVariable = Result
   MyCLS2Library = Result
   LocalVariables = LocalVariables $headingVariable
   $headingVariable = 'TriggerFileName'
   interpret 'Call' WhereIam $headingVariable
   Value.$headingVariable = Result
   TriggerFileName = Result
   LocalVariables = LocalVariables $headingVariable
   $headingVariable = 'MyAUTULibrary'
   interpret 'Call' WhereIam $headingVariable
   Value.$headingVariable = Result
   MyAUTULibrary = Result
   LocalVariables = LocalVariables $headingVariable
   $headingVariable = 'MyHomeAddress'
   interpret 'Call' WhereIam $headingVariable
   Value.$headingVariable = Result
   MyHomeAddress = Result
   LocalVariables = LocalVariables $headingVariable
   $headingVariable = 'MyAUTHLibrary'
   interpret 'Call' WhereIam $headingVariable
   Value.$headingVariable = Result
   MyAUTHLibrary = Result
   LocalVariables = LocalVariables $headingVariable
   $headingVariable = 'MyLOADLibrary'
   interpret 'Call' WhereIam $headingVariable
   Value.$headingVariable = Result
   MyLOADLibrary = Result
   LocalVariables = LocalVariables $headingVariable
   $headingVariable = 'MyDATALibrary'
   interpret 'Call' WhereIam $headingVariable
   Value.$headingVariable = Result
   MyDATALibrary = Result
   LocalVariables = LocalVariables $headingVariable
   $headingVariable = 'MyOPT2Library'
   interpret 'Call' WhereIam $headingVariable
   Value.$headingVariable = Result
   MyOPT2Library = Result
   LocalVariables = LocalVariables $headingVariable
   $headingVariable = 'MyOPTNLibrary'
   interpret 'Call' WhereIam $headingVariable
   Value.$headingVariable = Result
   MyOPTNLibrary = Result
   LocalVariables = LocalVariables $headingVariable
   $headingVariable = 'MySENULibrary'
   interpret 'Call' WhereIam $headingVariable
   Value.$headingVariable = Result
   MySENULibrary = Result
   LocalVariables = LocalVariables $headingVariable
   $headingVariable = 'MySEN2Library'
   interpret 'Call' WhereIam $headingVariable
   Value.$headingVariable = Result
   MySEN2Library = Result
   LocalVariables = LocalVariables $headingVariable
   $headingVariable = 'AltIDOrderfile'
   interpret 'Call' WhereIam $headingVariable
   Value.$headingVariable = Result
   AltIDOrderfile = Result
   LocalVariables = LocalVariables $headingVariable
   $headingVariable = 'AltIDAcctCode'
   interpret 'Call' WhereIam $headingVariable
   Value.$headingVariable = Result
   AltIDAcctCode = Result
   LocalVariables = LocalVariables $headingVariable
   $headingVariable = 'AltIDJobClass'
   interpret 'Call' WhereIam $headingVariable
   Value.$headingVariable = Result
   AltIDJobClass = Result
   LocalVariables = LocalVariables $headingVariable
   $headingVariable = 'TransmissionMethods'
   interpret 'Call' WhereIam $headingVariable
   Value.$headingVariable = Result
   TransmissionMethods = Result
   LocalVariables = LocalVariables $headingVariable
   $headingVariable = 'TransmissionModels'
   interpret 'Call' WhereIam $headingVariable
   Value.$headingVariable = Result
   TransmissionModels = Result
   LocalVariables = LocalVariables $headingVariable
   $headingVariable = 'SHLQ'
   interpret 'Call' WhereIam $headingVariable
   Value.$headingVariable = Result
   SHLQ = Result
   LocalVariables = LocalVariables $headingVariable
/* <---- Variable settings for each site          */
/* Arg DSN_Prefix ModelDSN . ;                                        */
   MySEN2Library = MySEN2Library
   MySEN2Library = Strip(MySEN2Library,'B',',') ;
   MySEN2Library = Strip(MySEN2Library)
   Sa= "MySEN2Library =" MySEN2Library
   Jobnbr = '   '
/*                                                                    */
/* This Rexx participates in the submission of Endevor Package        */
/* Shipment jobs. It is called by the Endevor sweep job.              */
/*                                                                    */
   Submit_RC = 0  ;
   Last_Submit_RC = 0  ;
   TodaysDate = DATE('S') ;
   NOW  = TIME(L);
   HOUR = SUBSTR(NOW,1,2) ;
   IF HOUR = '00' THEN HOUR = '0'
   MINUTE = SUBSTR(NOW,4,2) ;
   CurrentTime= HOUR || MINUTE ;
   HSYSEXEC = MyCLS2Library
   Userid = USERID()
   Call AllocateTriggerForUpdate ;
   "EXECIO * DISKR TRIGGER (STEM $tablerec. FINIS" ;
   /* Build all the ...pos variables from heading */
   Call ProcessTriggerHeading;
/*                                                                    */
   seconds = '000001' /* Wait 1 second between submitting jobs  */
   Do trg# = 1 to $tablerec.0
      status      = Substr($tablerec.trg#,Stpos,1) ;
      If status /= "_" & status /= " " &,
         status /= "B"                 then iterate;
      Package     = Substr($tablerec.trg#,Packagepos,16) ;
      System      = Strip(Substr($tablerec.trg#,Systempos,08));
      Destination = Strip(Substr($tablerec.trg#,Destinationpos,08));
      Date        = Substr($tablerec.trg#,Datepos,08) ;
      IF Date > TodaysDate then iterate ;
      Time        = Substr($tablerec.trg#,Timepos,04) ;
      IF Date = TodaysDate &,
         Time > CurrentTime then iterate ;
      Call  GetDestinationInfoViaCSV;
      If Hostprefix  = "?" then Iterate
      Jobname     = Strip(Substr($tablerec.trg#,Jobnamepos,08)) ;
      If Jobname  = 'useridX' then Jobname = USERID() || 'X'
      $headingVariable = 'PkgExecJobname'
      Value.$headingVariable = Jobname
      LocalVariables = LocalVariables $headingVariable
      /* Support more variables */
      SENDNODE =  MVSVAR(SYSNAME)
      $headingVariable = 'SENDNODE'
      Value.$headingVariable = SENDNODE
      LocalVariables = LocalVariables $headingVariable
      $headingVariable = 'Notify'
      Value.$headingVariable = USERID()
      LocalVariables = LocalVariables $headingVariable
      $headingVariable = 'Userid'
      Value.$headingVariable = USERID()
      LocalVariables = LocalVariables $headingVariable
      ShipOutput = 'OUT'
      If status  = "B" then ShipOutput = 'BAC' ;
      $headingVariable = 'ShipOutput'
      Value.$headingVariable = ShipOutput
      LocalVariables = LocalVariables $headingVariable
      Date8  = DATE('S')
      $headingVariable = 'Date8'
      Value.$headingVariable = Date8
      LocalVariables = LocalVariables $headingVariable
      Date6  = substr(Date8,3);
      $headingVariable = 'Date6'
      Value.$headingVariable = Date6
      LocalVariables = LocalVariables $headingVariable
      Temp   = TIME('L')
      Time8  = Substr(Temp,1,2) ||,
               Substr(Temp,4,2) ||,
               Substr(Temp,7,2) ||,
               Substr(Temp,10,2) ;
      $headingVariable = 'Time8'
      Value.$headingVariable = Time8
      LocalVariables = LocalVariables $headingVariable
      Time6  = Substr(Temp,1,2) ||,
               Substr(Temp,4,2) ||,
               Substr(Temp,7,2) ;
      $headingVariable = 'Time6'
      Value.$headingVariable = Time6
      LocalVariables = LocalVariables $headingVariable
      NewStatus = 's' ;
      Call UPDATE_MODEL_FROM_VARIABLES ; /* Submits Shipment job */
      /* Update Trigger to show s for job submitted */
      $TGGR_$headingVariable = 'St'
      pos= $TGGR_Starting_$pos.$TGGR_$headingVariable
      if Last_Submit_RC = 0 then,
         Do
         $tablerec.trg# = Overlay(NewStatus,$tablerec.trg#,Stpos) ;
         $tablerec.trg# = ,
            Overlay(CurrentTime,$tablerec.trg#,Timepos) ;
         pos= $TGGR_Starting_$pos.$TGGR_$headingVariable
         If Substr(Jobnbr,1,1) > ' ' then,
            $tablerec.trg# = ,
               Overlay(Jobnbr,$tablerec.trg#,Jobnumbpos);
         If trg# < $tablerec.0 then,
            Call WaitAwhile ;
         End
      Else,
         $tablerec.trg# = Overlay("?",$tablerec.trg#,Stpos) ;
      Last_Submit_RC = 0  ;
   End ;  /* Do trg# = 1 to $tablerec.0 */
   "EXECIO * DISKW TRIGGER (STEM $tablerec. FINIS" ;
   Call FreeTriggerFile ;
   if TraceRc = 1 then Say "PULLTGGR- exiting....  "
   Exit(Submit_RC) ;
/*                                                                    */
/* Substitute Variables in the MODEL                                  */
/*                                                                    */
UPDATE_MODEL_FROM_VARIABLES:
   if TraceRc = 1 then Say "UPDATE_MODEL_FROM_VARIABLES:      "
   Sa= "UPDATE_MODEL_FROM_VARIABLES:       "
   /* Determine Shipment JCL Model */
   Method# = Wordpos(Transmissn,TransmissionMethods) ;
   If Method# = 0 then Exit
   $headingVariable = 'TransmissionModels'
   TransmissionModels = Value.$headingVariable
   ShipModel = Word(TransmissionModels,Method#);
   STRING = "ALLOC DD(MODEL) ",
              " DA('"MySEN2Library"("ShipModel")')",
              " SHR REUSE ";
   sa= 'Destination' Destination 'is' ShipModel
   CALL BPXWDYN STRING;
   MyResult = RESULT ;
   If MyResult > 0 then,
      Do
      Say 'PULLTGGR- Cannot find Shipment Model' ShipModel
      Return ;
      End;
   "EXECIO * DISKR "MODEL "(STEM $Model. FINIS" ;
   $delimiter = "|" ;
   STRING = "FREE DD(MODEL) "
   CALL BPXWDYN STRING;
   DO $LINE = 1 TO $Model.0
      $PLACE_VARIABLE = 1;
      CALL EVALUATE_SYMBOLICS ;
   END; /* DO $LINE = 1 TO $Model.0 */
   IF TraceRc = 1 then Trace R
   CALL BPXWDYN ,
    "ALLOC DD(SYSUT1) LRECL(80) BLKSIZE(27920) SPACE(5,5) ",
           " RECFM(F,B) TRACKS ",
           " NEW UNCATALOG REUSE ";
   "EXECIO * DISKW SYSUT1 (STEM $Model. FINIS" ;
   Call Submit_Job ;
   Drop $Model. ;
   RETURN;
EVALUATE_SYMBOLICS:
   If TraceRc = 1 then Say "EVALUATE_SYMBOLICS:               "
   DO FOREVER;
      $PLACE_VARIABLE = POS('&',$Model.$LINE,$PLACE_VARIABLE)
      IF $PLACE_VARIABLE = 0 THEN LEAVE;
      $temp_$LINE = TRANSLATE($Model.$LINE,' ',',.()"/\+-*|');
      $temp_$LINE = TRANSLATE($temp_$LINE,' ',"'"$delimiter);
      $table_word = WORD(SUBSTR($temp_$LINE,($PLACE_VARIABLE+1)),1);
      $table_word = TRANSLATE($table_word,'_','-') ;
      $varlen = LENGTH($table_word) + 1 ;
      if WORDPOS($table_word,LocalVariables) > 0 then,
         Do
         $headingVariable = $table_word
         SYMBVALUE  = Value.$headingVariable
         End;
      Else,
      if WORDPOS($table_word,TGGR_variables) > 0 then,
         Do
         $TGGR_$headingVariable = $table_word
         pos= $TGGR_Starting_$pos.$TGGR_$headingVariable
         SYMBVALUE  = Word(Substr($tablerec.trg#,pos),1)
         If $table_word = 'Typrun' &,
            Length(SYMBVALUE) > 0 then,
               SYMBVALUE = ',TYPRUN='SYMBVALUE
         End
      Else,
         Do
         $PLACE_VARIABLE = $PLACE_VARIABLE + 1 ;
         iterate;
         End
      SA= 'SYMBVALUE  = ' SYMBVALUE ;
      $tail = SUBSTR($Model.$LINE,($PLACE_VARIABLE+$varlen)) ;
      if Substr($tail,1,1) = $delimiter then,
         $tail = SUBSTR($tail,2) ;
      IF $PLACE_VARIABLE > 1 THEN,
         $Model.$LINE = ,
            SUBSTR($Model.$LINE,1,($PLACE_VARIABLE-1)) ||,
            SYMBVALUE || $tail ;
      else,
         $Model.$LINE = ,
            SYMBVALUE || $tail ;
      END; /* DO FOREVER */
   RETURN;
Submit_Job:
   if TraceRc = 1 then Say "Submit_Job:                       "
   CALL BPXWDYN "ALLOC DD(SHOWJCL) SYSOUT(A) "
   "Execio * DISKR SYSUT1   ( Stem jcl. finis"
   "Execio * DISKW SHOWJCL  ( Stem jcl. finis"
   STRING = "ALLOC DD(SUBMIT)",
               "SYSOUT(A) WRITER(INTRDR) REUSE " ;
   CALL BPXWDYN STRING;
   "Execio * DISKW SUBMIT   ( Stem jcl. finis"
   CALL BPXWDYN "FREE DD(SHOWJCL)"
   CALL BPXWDYN "FREE DD(SUBMIT)"
   CALL BPXWDYN "FREE DD(SYSUT1)"
   RETURN;
AllocateTriggerForUpdate:
   if TraceRc = 1 then Say "AllocateTriggerForUpdate:         "
   STRING = "ALLOC DD(TRIGGER)",
              " DA('"TriggerFileName"') OLD REUSE"
   seconds = '000001' /* Number of Seconds to wait if needed */
   Do Forever  /* or at least until the file is available */
      CALL BPXWDYN STRING;
      MyRC = RC
      MyResult = RESULT ;
      If MyResult = 0 then Leave
      Call WaitAwhile
   End /* Do Forever */
   Return ;
FreeTriggerFile:
   if TraceRc = 1 then Say "AllocateTriggerForUpdate:         "
   STRING = "FREE DD(TRIGGER)"
   CALL BPXWDYN STRING  ;
   Return ;
/*                                                                    */
/* Convert Date formats                                               */
/*                                                                    */
WaitAwhile:
   if TraceRc = 1 then Say "WaitAwhile: " seconds
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
  If TraceRc = 1 then,
     Say "PULLTGGR- Waiting for" seconds "seconds at " DATE(S) TIME()
  /* AOPBATCH and BPXWDYN are IBM programs */
  CALL BPXWDYN  "ALLOC DD(STDOUT) DUMMY SHR REUSE"
  CALL BPXWDYN  "ALLOC DD(STDERR) DUMMY SHR REUSE"
  CALL BPXWDYN  "ALLOC DD(STDIN) DUMMY SHR REUSE"
  /* AOPBATCH and BPXWDYN are IBM programs */
  parm = "sleep "seconds
  Address LINKMVS "AOPBATCH parm"
  Return
ProcessTriggerHeading :
   if TraceRc = 1 then Say "ProcessTriggerHeading : "
   $tbl = 1 ;
   $TableHeadingChar = '*'
   $LastWord = Word($tablerec.$tbl,Words($tablerec.$tbl));
   If DATATYPE($LastWord) = 'NUM' then,
      Do
      Say 'PULLTGGR- Please remove sequence numbers from the Table'
      Exit(12)
      End
   $tmprec = Substr($tablerec.$tbl,2) ;
   $PositionSpclChar = POS('-',$tmprec) ;
   If $PositionSpclChar = 0 then,
      $PositionSpclChar = POS('*',$tmprec) ;
   $tmpreplaces = '-,.'$TableHeadingChar ;
   $tmprec = TRANSLATE($tmprec,' ',$tmpreplaces);
   TGGR_variables = strip($tmprec);
   $TGGR_Variable_count = WORDS(TGGR_variables) ;
   If $TGGR_Variable_count /=,
      Words(Substr($tablerec.$tbl,2)) then,
      Do
      Say 'PULLTGGR- Invalid table Heading:' $tablerec.$tbl
      exit(12)
      End
   $TGGR_heading = Overlay(' ',$tablerec.$tbl,1); /* Space leading * */
   Do $pos = 1 to $TGGR_Variable_count
      $TGGR_$headingVariable = Word(TGGR_variables,$pos) ;
      $tmp = Wordindex($TGGR_heading,$pos) ;
      $TGGR_Starting_$pos.$TGGR_$headingVariable = $tmp
      If $TGGR_$headingVariable = 'St' then Stpos = $tmp
      If $TGGR_$headingVariable = 'Package' then Packagepos =$tmp
      If $TGGR_$headingVariable = 'System' then Systempos   =$tmp
      If $TGGR_$headingVariable = 'Destination' then,
         Destinationpos =$tmp
      If $TGGR_$headingVariable = 'Date'    then Datepos    =$tmp
      If $TGGR_$headingVariable = 'Time'    then Timepos    =$tmp
      If $TGGR_$headingVariable = 'Jobname' then Jobnamepos =$tmp
      If $TGGR_$headingVariable = 'Typrun' then TYPRUNpos   =$tmp
      $tmp = $tmp + Length(Word($TGGR_heading,$pos)) -1 ;
      $TGGR_$Ending_$pos.$TGGR_$headingVariable = $tmp
      Say $TGGR_$headingVariable,
          $TGGR_Starting_$pos.$TGGR_$headingVariable $tmp
   end; /* DO $pos = 1 to TGGR_Variable_count */
   $TGGR_heading = Translate($TGGR_heading,' ','-*')
   Return ;
GetDestinationInfoViaCSV:
   if TraceRc = 1 then Say "GetDestinationInfoViaCSV:   "
   Hostprefix  = "?"; Rmteprefix  = "?";
   Transmissn  = "?"; TARGnodeix  = "?";
   /* Set values for Hostprefix and Rmteprefix */
   /*     From the site definition             */
   /*  Call CSV to Get Destination information  */
   SiteVariables = GTDESTIN(Destination)
   If Words(SiteVariables) < 4 then Return
   $headingVariable = 'Hostprefix'
   Hostprefix              = Word(SiteVariables,1)
   Value.$headingVariable = Hostprefix
   LocalVariables = LocalVariables $headingVariable
   $headingVariable = 'Rmteprefix'
   Rmteprefix              = Word(SiteVariables,2)
   Value.$headingVariable = Rmteprefix
   LocalVariables = LocalVariables $headingVariable
   $headingVariable = 'Transmissn'
   Transmissn              = Word(SiteVariables,3)
   Value.$headingVariable = Transmissn
   LocalVariables = LocalVariables $headingVariable
   $headingVariable = 'TARGnode'
   TARGnode                = Word(SiteVariables,4)
   Value.$headingVariable = TARGnode
   LocalVariables = LocalVariables $headingVariable
   Return
