*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                     Проверка и коррекция долга.                       ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Rem_Due

DO Wt_Mess WITH "Подбор данных"

DELETE FILE (tmpo_path+"REM_DUE.DBF")
DELETE FILE (tmpo_path+"REM_DUE.CDX")
CREATE DBF (tmpo_path+"REM_DUE")  ;
   ( CODE    N( 7), ;
     PREFIX  C( 4), ;
     NAME    C(35), ;
     L_NUM   C( 6), ;
     QNT_DUE N( 6), ;
     QNT_STO N( 6), ;
     ARM_C   C( 3), ;
     ARM_N   C(16), ;
     REASON  C(16)  )
INDEX ON ARM_C+L_NUM+NAME+PREFIX TAG ORD_DUE

SELECT 0
USE STOCK ORDER TAG NAME ALIAS STOCK

SELECT 0
USE LIST_DET ORDER TAG CODE
GO TOP

*
*    Сращиваем позиции, которые почему-то не срослись!
*

SCAN WHILE CODE = 0

  IF .NOT. SEEK(NAME+PREFIX,"STOCK")
    LOOP
  ENDIF
  SELECT REM_DUE
  APPEND BLANK
  REPLACE CODE    WITH STOCK.CODE,   ;
          PREFIX  WITH STOCK.PREFIX, ;
          NAME    WITH STOCK.NAME,   ;
          L_NUM   WITH LIST_DET.LIST_NUM, ;
          QNT_DUE WITH LIST_DET.QNT_DUE, ;
          QNT_STO WITH STOCK.QNT,    ;
          REASON  WITH "Прив. по имени"
  SELECT LIST_DET
  REPLACE CODE WITH STOCK.CODE

ENDSCAN

*
*    Ищем долг и склад!
*

SELECT STOCK
SET ORDER TO TAG CODE

SELECT LIST_DET
SET ORDER TO TAG DUE_CODE
GO TOP
SCAN

  IF .NOT. SEEK(CODE,"STOCK")
    LOOP
  ENDIF
  IF STOCK.QNT <= 0
    LOOP
  ENDIF
  SELECT REM_DUE
  APPEND BLANK
  REPLACE CODE    WITH STOCK.CODE,   ;
          PREFIX  WITH STOCK.PREFIX, ;
          NAME    WITH STOCK.NAME,   ;
          L_NUM   WITH LIST_DET.LIST_NUM, ;
          QNT_DUE WITH LIST_DET.QNT_DUE, ;
          QNT_STO WITH STOCK.QNT,    ;
          REASON  WITH "Долг и наличие"
  SELECT LIST_DET

ENDSCAN

USE LIST_TIT ORDER TAG LST_NUM
SELECT STOCK
USE ARM_LST ORDER TAG ARM_C

SELECT REM_DUE
IF RECCOUNT() = 0
  DO Wt_Mess
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Не обнаружено никакого криминала..."
  mss(3) = ""
  DO Out_Mess WITH 5, "mss"
  CLOSE DATABASES

  DELETE FILE (tmpo_path+"REM_DUE.DBF")
  DELETE FILE (tmpo_path+"REM_DUE.CDX")
  RETURN
  
ENDIF
  
  
GO TOP
PRIVATE l_name, a_name, l_w, w
l_name = ""
a_name = ""
l_w    = "******"
DO Wt_Mess

DO Ini_Prn WITH "Обработка долга", 0, 0, 1, 0, 1

SCAN

  IF l_w # L_NUM
    IF SEEK(L_NUM,"LIST_TIT")
      l_name = ALLTRIM(LIST_TIT.LST_NAME)
      IF LIST_TIT.LST_ARM = "***"
        a_name = "Общаяя область"
      ELSE
        a_name = IIF(SEEK(LIST_TIT.LST_ARM,"ARM_LST"), ARM_LST.ARM_N, "************")
      ENDIF
    ELSE
      l_name = "************"
      a_name = "************"
    ENDIF
    w = "Список N "+ALLTRIM(L_NUM)+", "+l_name+". Раб. место: "+a_name
    IF l_w = "******"
      @ PROW(), PCOL() SAY w
    ELSE
      @ PROW()+3, 0 SAY w
    ENDIF
    @ PROW()+1, 0 SAY   ;
    "    Позиция                      Долг  Наличие Дефект"
    @ PROW()+1, 0 SAY   ;
    "───────────────────────────────────────────────────────────────────────"
    l_w = L_NUM
  ENDIF
  @ PROW()+1, 0 SAY PREFIX+" "+NAME+STR(QNT_DUE,7)+STR(QNT_STO,7)+" "+REASON

ENDSCAN

DO Term_Prn WITH ""

CLOSE DATABASES

DELETE FILE (tmpo_path+"REM_DUE.DBF")
DELETE FILE (tmpo_path+"REM_DUE.CDX")

RETURN
