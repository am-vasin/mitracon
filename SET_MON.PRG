SET DELE ON
SET EXCLU OFF
SET TALK OFF

pth_old = "z:\store\OLD\base\"
pth_new = "z:\store\base\"
use (pth_new+"INCMONEY") ALIAS NEW
SELECT 0
use (pth_old+"INCMONEY") ALIAS OLD

cnt = 54346
CLEAR
? RECCOUNT()
go 54346

SCAN rest
  scatter memvar
  IF m.doc_type = "2"
    m.doc_type = " "
    m.firm = "C00"
  ELSE
    m.firm = "CMP"
  ENDIF
  SELECT NEW
  APPEND BLANK
  GATHER MEMVAR
  SELECT OLD
  if mod(recno(),100) = 0
    @ 10, 10 SAY str(recno())
  endif
ENDSCAN

CLOSE DATA

    