

           IF  SETUP-EXIT-OPTIONS
               MOVE ZERO TO PECB-UEXIT-HOLD-FIELD
*********    to enforce package create rules
               MOVE 'Y'   TO PECB-BEFORE-CREATE-BLD
               MOVE 'Y'   TO PECB-BEFORE-CREATE-COPY
               MOVE 'Y'   TO PECB-BEFORE-CREATE-EDIT
               MOVE 'Y'   TO PECB-BEFORE-CREATE-IMPT
               MOVE 'Y'   TO PECB-BEFORE-REV-APPR

