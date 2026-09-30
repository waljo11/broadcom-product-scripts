

   /* Validate Package prefix with ServiceNow              */
   If PECB_FUNCTION_LITERAL  ='CREATE'   &,
      PECB_BEF_AFTER_LITERAL ='BEFORE'   &,
      (Substr(PECB_PACKAGE_ID,1,3) = 'PRB' |,
       Substr(PECB_PACKAGE_ID,1,3) = 'CHG' )         then,
      Do
      PackageSnowRef = Substr(PECB_PACKAGE_ID,1,10)
      Message = SERVINOW('C1UEXTR7' PackageSnowRef PECB-MODE )
      If POS('**NOT**', Message) > 0 then,
         Do
         MyRc        = 8
         Call SetExitReturnInfo
         Exit
         End;  /* If POS('**NOT**', Message) > 0 */
      End;  /* If PECB_FUNCTION_LITERAL  ='CREATE' ... */
   
   