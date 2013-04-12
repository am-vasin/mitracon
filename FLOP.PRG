********************************
PROCEDURE flop
********************************
PARAMETERS flop
PRIVATE flop
!fdd_stat &flop
status_fl=inkey()
IF status_fl=0
   RETURN .T.
 ELSE  
   RETURN .F.
ENDIF
