*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Prep_Inv     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║       Формирование/коррекция дополнительного списка на доставку.       ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 31.03.2000 ══╝
PROCEDURE Prep_Inv
PARAMETERS lst_n, c_code, c_name, a_code

PRIVATE s_sav, mss, sw, l_n

l_n = lst_n
s_sav = SELECT()
SELECT 0
DELETE FILE (tmpo_path+"INV_DLV.DBF")
DELETE FILE (tmpo_path+"INV_DLV.CDX")
CREATE DBF (tmpo_path+"INV_DLV.DBF") ;
  ( LINK   N(10),  ;
    CODE   N( 7),  ;
    NAME   C(30),  ;
    PRICE0 N(12,5), ;
    PRICE  N(12,5)  )
INDEX ON CODE TAG CODE
INDEX ON NAME TAG NAME ADDITIVE
SET ORDER TO TAG CODE
SELECT 0
USE (base_path+"DLV_STO") ALIAS DS_0403 ORDER TAG NAME
SCAN
  SELECT INV_DLV
  APPEND BLANK
  REPLACE CODE WITH DS_0403.CODE, NAME WITH DS_0403.NAME
  SELECT DS_0403
ENDSCAN
USE
IF RECCOUNT("INV_DLV") = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Номенклатура доставки пуста..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SELECT INV_DLV
  USE
  DELETE FILE (tmpo_path+"INV_DLV.DBF")
  DELETE FILE (tmpo_path+"INV_DLV.CDX")
  SELECT (s_sav)
  RETURN
ENDIF
    
IF .NOT. EMPTY(lst_n)
  sw = .T.
  USE (base_path+"LIST_TIT") ORDER TAG LST_NUM ALIAS LST0331 AGAIN
  IF .NOT. SEEK(lst_n)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Дополнительный список уже удален!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    sw = .F.
  ENDIF
  IF sw
    IF .NOT. EMPTY(SAL_NUM)
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"Дополнительный список продан"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      sw = .F.
    ENDIF
  ENDIF
  USE
  IF sw
    USE (base_path+"LIST_DET") ORDER TAG LIST ALIAS LST0331 AGAIN
    SEEK STR(lst_n,6)
    SCAN REST WHILE LIST_NUM = STR(lst_n,6)
      IF .NOT. SEEK(CODE,"INV_DLV")
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"Дополнительный список содержит не только позиции доставки."
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        sw = .F.
        EXIT
      ENDIF
      REPLACE INV_DLV.LINK   WITH RECNO(),       ;
              INV_DLV.PRICE0 WITH QNT*SAL_PRICE, ;
              INV_DLV.PRICE  WITH QNT*SAL_PRICE
    ENDSCAN
    USE
  ENDIF
  SELECT INV_DLV
  IF .NOT. sw
    USE
    DELETE FILE (tmpo_path+"INV_DLV.DBF")
    DELETE FILE (tmpo_path+"INV_DLV.CDX")
    SELECT (s_sav)
    RETURN
  ENDIF
ENDIF

SELECT INV_DLV
IF RECCOUNT() = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Не определена номенклатура доставки!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  DELETE FILE (tmpo_path+"INV_DLV.DBF")
  DELETE FILE (tmpo_path+"INV_DLV.CDX")
  SELECT (s_sav)
  RETURN
ENDIF

IF Modi_Inv()
  IF EMPTY(lst_n)
    DO New_List
  ELSE
    DO Modi_List
  ENDIF
  IF USED("LT_0405")
    DO To_Ord
  ENDIF
ENDIF
USE
DELETE FILE (tmpo_path+"INV_DLV.DBF")
DELETE FILE (tmpo_path+"INV_DLV.CDX")

SELECT (s_sav)
IF .NOT. EMPTY(l_n)
  REPLACE NEW_LIST WITH l_n
ENDIF

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Modi_Inv     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                          Расходы на доставку.                          │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03.04.2000 ──┘
PROCEDURE Modi_Inv

*
*  Переменные состояния для навигации
*

PRIVATE t_sum
PRIVATE stat_type     && Тип навигации: 0 - нестандартный;
                                        1 - бланк;
                                        2 - BROWSE - таблица;
                                        3 - BROWSE - список.
PRIVATE what_do       && Имя режима.
PRIVATE menu_name     && Имя асинхронного меню.
PRIVATE last_mouse    && Время последнего нажатия звериной кнопки.
PRIVATE win_name      && Имя окна ( окон для BROWSE ).
PRIVATE s_sav         && Номер рабочей области для сохранения и восстановления!

*
*   Заполняем значениями переменные состояния...
*
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
s_sav = SELECT()

SELECT INV_DLV

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-15   && Количество видимых строк BROWSE
wd = FSIZE("NAME")+13

SUM ROUND(PRICE,2) TO t_sum
DO D_Wins WITH ln, wd, "Расходы на доставку", 1, 1
@ 1, 1 SAY LEFT(CUS_0329.CUS_NAME, WCOLS()-2)
@ WROWS()-2, WCOLS()-20 SAY "Итого "+STR(t_sum, 12, 5)
what_do = "List"
GO TOP
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1

*
*    BROWSE - меню
*
    BROWSE FIELDS NAME:H="", ;
                  PRICE:H="":P="999999.99999":V=Draw_Foot() ;
           FREEZE PRICE ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Exit")
    ENDIF

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   Аккуратненько (аккуратненько!) закрываем DBF-файлы.
*
SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Draw_Foot    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                   Рисуем дополнительную информацию.                    │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.10.1998 ──┘
PROCEDURE Draw_Foot

PRIVATE r_sav, sum_t, mss

IF PRICE < 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Возврат денег в данном случае запрещен!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

REPLACE PRICE WITH ROUND_6(PRICE)
r_sav = RECNO()
SUM ROUND(PRICE,2) TO t_sum
ACTIVATE WINDOW (win_name+"_M") SAME
*
*   Вот здесь, рисуем...
*
@ WROWS()-2, WCOLS()-14 SAY STR(t_sum, 12, 5)

ACTIVATE WINDOW (ALIAS()) SAME

GO r_sav

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура New_List     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Создание списка.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 05.04.2000 ──┘
PROCEDURE New_List
PRIVATE s_sav

s_sav = SELECT()

SELECT 0
USE (base_path+"LIST_TIT") ALIAS LT_0405 AGAIN
SELECT 0
USE (base_path+"LIST_DET") ALIAS LD_0405 AGAIN
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS ST_0405 AGAIN
SELECT INV_DLV
SCAN FOR .NOT. EMPTY(PRICE)
  = SEEK(CODE, "ST_0405")
  IF l_n <= 0
    l_n = Doc_Num("LISTS", "", a_code, DATE())
    SELECT LT_0405
    APPEND BLANK
    REPLACE LST_ARM   WITH a_code, ;
            FIRM      WITH LSTS_0329.FIRM, ;
            LST_NUM   WITH l_n,    ;
            CR_DAT    WITH DATE(), ;
            LST_NAME  WITH "Дост. "+c_name, ;
            CLI_CODE  WITH c_code, ;
            WHO_LET   WITH user,   ;
            ORD_QUEUE WITH queuetype,;
            IS_SPEC   WITH "*",    ;
            WHO_CR    WITH user,   ;
            WHERE_CR  WITH arm
  ENDIF
  SELECT LD_0405
  APPEND BLANK
  REPLACE LIST_NUM  WITH STR(l_n,6),          ;
          CODE      WITH ST_0405.CODE,        ;
          PREFIX    WITH ST_0405.PREFIX,      ;
          NAME      WITH ST_0405.NAME,        ;
          PRO_CODE  WITH ST_0405.PRO_CODE,    ;
          PRODUCER  WITH ST_0405.PRODUCER,    ;
          QNT       WITH SIGN(INV_DLV.PRICE), ;
          IS_SPEC   WITH "*",                 ;
          SAL_PRICE WITH ABS(INV_DLV.PRICE)
  SELECT INV_DLV
ENDSCAN

SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Modi_List    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Коррекция списка.                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 05.04.2000 ──┘
PROCEDURE Modi_List
PRIVATE s_sav, mss

s_sav = SELECT()

SELECT 0
USE (base_path+"LIST_TIT") ORDER TAG LST_NUM ALIAS LT_0405 AGAIN
IF .NOT. SEEK(l_n)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Пока Вы корректировали список, его уже удалили..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  RETURN
ENDIF
IF .NOT. EMPTY(SAL_NUM)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Пока Вы корректировали список, его уже продали..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  RETURN
ENDIF
SELECT 0
USE (base_path+"LIST_DET") ALIAS LD_0405 AGAIN
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS ST_0405 AGAIN
SELECT INV_DLV
SCAN FOR .NOT. EMPTY(PRICE)
  = SEEK(CODE, "ST_0405")
  SELECT LD_0405
  IF EMPTY(INV_DLV.LINK)
    APPEND BLANK
  ELSE
    GO INV_DLV.LINK
  ENDIF
  REPLACE LIST_NUM  WITH STR(l_n,6),          ;
          CODE      WITH ST_0405.CODE,        ;
          PREFIX    WITH ST_0405.PREFIX,      ;
          NAME      WITH ST_0405.NAME,        ;
          PRO_CODE  WITH ST_0405.PRO_CODE,    ;
          PRODUCER  WITH ST_0405.PRODUCER,    ;
          QNT       WITH SIGN(INV_DLV.PRICE), ;
          IS_SPEC   WITH "*",                 ;
          SAL_PRICE WITH ABS(INV_DLV.PRICE)
  SELECT INV_DLV
ENDSCAN

SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура To_Ord       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                     Формирование заявки на склад.                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 06.04.2000 ──┘
PROCEDURE To_Ord

PRIVATE s_sav, s0, o_n, np, nn
s_sav = SELECT()
o_n = 0
nn = 0
np = 0

SELECT 0
USE (base_path+"ORD_D") ALIAS OD_0405 AGAIN
SELECT LD_0405
SET ORDER TO TAG LIST
s0 = 0
SEEK STR(l_n, 6)
SCAN REST WHILE LIST_NUM = STR(l_n, 6)
  s0 = s0+SAL_PRICE
  IF QNT # QNT_REQ
    = SEEK(CODE, "ST_0405")
    IF o_n = 0
      o_n = LT_0405.ORD_NUM+1
      REPLACE LT_0405.ORD_NUM WITH LT_0405.ORD_NUM+1
    ENDIF
    SELECT OD_0405
    APPEND BLANK
    REPLACE LIST_NUM WITH STR(l_n, 6), ;
            ORD_NUM  WITH o_n,         ;
            CODE     WITH LD_0405.CODE,;
            QNT      WITH LD_0405.QNT-LD_0405.QNT_REQ
    IF QNT > 0
      np = np+QNT
    ELSE
      nn = nn-QNT
    ENDIF
    SELECT ST_0405
    REPLACE QNT      WITH QNT-OD_0405.QNT, ;
            QNT_SAVE WITH QNT_SAVE+OD_0405.QNT
    IF QNT < 0
      PRIVATE mss
      DIMENSION mss(4)
      mss(1) = ""
      mss(2) = CHR(0)+"Внимание! "+ALLTRIM(PREFIX)+" "+ALLTRIM(NAME)+ ;
               IIF(EMPTY(PRODUCER), "", " /"+PRODUCER+"/")
      mss(3) = CHR(0)+"закончилась на складе! Ее обязательно надо оприходовать!"
      mss(4) = ""
      DO Out_Mess WITH 7, "mss"
    ENDIF
    SELECT LD_0405
    REPLACE QNT_REQ WITH QNT
  ENDIF
ENDSCAN

SELECT OD_0405
IF o_n > 0
  USE (base_path+"ORD_T") ALIAS OT_0405 AGAIN
  APPEND BLANK
  REPLACE LIST_NUM  WITH STR(l_n, 6), ;
          ORD_NUM   WITH o_n,         ;
          LIST_NAME WITH LT_0405.LST_NAME, ;
          ORD_DATE  WITH DATE(),      ;
          ORD_TIME  WITH TIME(),      ;
          STO_MARK  WITH user,        ;
          WHO       WITH user,        ;
          WHERE     WITH arm,         ;
          SIZE_POS  WITH np,          ;
          SIZE_NEG  WITH nn
ENDIF
USE
SELECT LT_0405
REPLACE R_TOTAL WITH s0,   ;
        WHO_LET WITH user, ;
        IS_SPEC WITH "*"
USE
SELECT LD_0405
USE
SELECT ST_0405
USE
SELECT (s_sav)

RETURN
