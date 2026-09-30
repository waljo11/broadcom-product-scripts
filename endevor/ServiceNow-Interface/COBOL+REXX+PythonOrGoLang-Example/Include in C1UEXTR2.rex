
   If Substr(REQ_CCID,1,3) = 'PRB' |,
      Substr(REQ_CCID,1,3) = 'CHG' &,
      REQ_CCID /= Former_CCID      Then,
         Do
         Message = SERVINOW('C1UEXTR2' REQ_CCID ECB_TSO_BATCH_MODE)
         If POS('**NOT**', Message) > 0 then,
            Do
            MessageCode = 'U012'
            MyRc        = 8
            End;  /* If POS('**NOT**', Message) > 0 */
         End;  /* If Substr(REQ_CCID,1,3) = 'PRB' ... 'CHG' */
   