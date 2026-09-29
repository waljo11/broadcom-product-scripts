/*   REXX  */
/* Find on the BroadCom product Scripts GitHub:                */
/* https://github.com/BroadcomMFD/broadcom-product-scripts     */
/* Receive tha Package name to use for finding Approver Groups */
   isItThere = ,
     BPXWDYN("INFO FI(GETAPPRV) INRTDSN(DSNVAR) INRDSNT(myDSNT)")
   If isItThere = 0 then Trace ?r
   Arg Package ;
   /* Prepare and run a CSV call to fetch Approvals for Package */
   /* Return the highest Quorum Count across Approver Groups    */
   Highest_QUORUM_CNT = 0
   Call CSV_to_List_Package_Approvals
   Call Parse_CSV_for_Package_Approvals
   Return Highest_QUORUM_CNT
CSV_to_List_Package_Approvals:
  /* Get Package Approver group information       */
   STRING = "ALLOC DD(EXTRACTM) LRECL(4000) BLKSIZE(32000) ",
              " DSORG(PS) ",
              " SPACE(1,5) RECFM(F,B) TRACKS ",
              " NEW UNCATALOG REUSE ";
   CALL BPXWDYN STRING;
   STRING = "ALLOC DD(C1MSGS1) SYSOUT(A)"
   CALL BPXWDYN STRING;
   STRING = "ALLOC DD(BSTERR) SYSOUT(A)"
   CALL BPXWDYN STRING;
   STRING = "ALLOC DD(BSTIPT01) LRECL(80) BLKSIZE(800) ",
              " DSORG(PS) ",
              " SPACE(1,5) RECFM(F,B) TRACKS ",
              " NEW UNCATALOG REUSE ";
   CALL BPXWDYN STRING;
   QUEUE "LIST PACKAGE APPROVER GROUP",
         "FROM PACKAGE '"Package"'"
   QUEUE "     TO DDNAME 'EXTRACTM' "
   QUEUE "     ."
   "EXECIO 3 DISKW BSTIPT01 (FINIS ";
   CALL BPXWDYN "INFO FI(CONLIB) INRTDSN(DSNVAR) INRDSNT(myDSNT)"
   if RESULT = 0 then,
      Do
      CSVParm = 'DDN:CONLIB,BC1PCSV0'
      ADDRESS LINKMVS 'CONCALL' "CSVParm"
      End
   Else,
      ADDRESS LINK 'BC1PCSV0'   ;  /* load from authlib */
   call_rc = rc ;
   CALL BPXWDYN "FREE DD(BSTIPT01)" ;
   CALL BPXWDYN "FREE DD(C1MSGS1)" ;
   CALL BPXWDYN "FREE DD(BSTERR)" ;
   If call_rc > 4 then Return
   "EXECIO * DISKR EXTRACTM (STEM CSV. finis"
   CALL BPXWDYN "FREE DD(EXTRACTM)" ;
   Return
Parse_CSV_for_Package_Approvals:
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
     /* Parse CSV fields in the Detail record until done */
     Do $column =  1 to Words($table_variables)
        Call ParseDetailCSVline
     End
     If QUORUM_CNT > Highest_QUORUM_CNT then,
        Highest_QUORUM_CNT = QUORUM_CNT
     If SEQ# = 1 then,
        Sa= APPR_GRP_NAME OVERALL_APPR_STATUS QUORUM_CNT
  End; /* Do rec# = 1 to CSV.0 */
  sa= "Highest_QUORUM_CNT="   Highest_QUORUM_CNT
  RETURN ;
ParseDetailCSVline:
  /* Find the data for the current $column */
  $dlmchar = Substr($detail,1,1);
  If $dlmchar = "'" then,
     Do
     PARSE VAR $detail "'" $temp_value "'" $detail ;
     If Substr($detail,1,1) = ',' then,
        $detail = Strip(Substr($detail,2),'L')
     End
  Else,
  If $dlmchar = '"' then,
     Do
     PARSE VAR $detail '"' $temp_value '"' $detail ;
     If Substr($detail,1,1) = ',' then,
        $detail = Strip(Substr($detail,2),'L')
     End
  Else,
  If $dlmchar = ',' then,
     Do
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
     PARSE VAR $detail $temp_value ',' $detail ;
     End
  $temp_value = STRIP($temp_value) ;
  $rslt = $temp_value
  $rslt = Strip($rslt,'B','"')                             ;
  $rslt = Strip($rslt,'B',"'")                             ;
  if Length($rslt) < 1 then $rslt = ' '
  $keyword     = WORD($table_variables,$column)
  Select
    When $keyword = 'APPR_GRP_NAME'       then APPR_GRP_NAME = $rslt
    When $keyword = 'OVERALL_APPR_STATUS' then,
       OVERALL_APPR_STATUS= $rslt
    When $keyword = 'QUORUM_CNT'          then QUORUM_CNT    = $rslt
    When $keyword = 'SEQ#'                then SEQ#          = $rslt
    Otherwise  NOP
  End
  RETURN ;
