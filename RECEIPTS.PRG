*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Receipts     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                       Выручка на рабочем месте.                        ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 18.04.2000 ══╝
PROCEDURE Receipts
PARAMETER prmArm

PRIVATE dt

dt = Get_Date({})
IF EMPTY(dt)
  RETURN
ENDIF

DO Prep_Tmp

DO V_Arms WITH prmArm

SELECT TMP_ARM
USE
DELETE FILE (tmpo_path+"TMP_ARM.DBF")
DELETE FILE (tmpo_path+"TMP_ARM.CDX")

SELECT TMP_MOVE
USE
DELETE FILE (tmpo_path+"TMP_MOVE.DBF")
DELETE FILE (tmpo_path+"TMP_MOVE.CDX")
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Prep_Tmp     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Подготовка временного файла.                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 18.04.2000 ──┘
PROCEDURE Prep_Tmp

DELETE FILE (tmpo_path+"TMP_ARM.DBF")
DELETE FILE (tmpo_path+"TMP_ARM.CDX")
CREATE DBF (tmpo_path+"TMP_ARM") ;
  ( ARM_C  N( 4),  ;
    ARM_N  C(16),  ;
    SUM_S1 N(12,2),;
    SUM_M1 N(12,2),;
    SUM_M2 N(12,2) )
INDEX ON SYS(15, lwr, ARM_N)TAG ARM_N
INDEX ON ARM_C TAG ARM_C ADDITIVE

SELECT 0

DELETE FILE (tmpo_path+"TMP_MOVE.DBF")
DELETE FILE (tmpo_path+"TMP_MOVE.CDX")
CREATE DBF (tmpo_path+"TMP_MOVE") ;
  ( REST      N(12,2),;
    ARM_C     N( 4),  ;
    FIRM      C( 3),  ;
    DOC_TIME  N( 5),  ;
    DOC_NUM   C(10),  ;
    DOC_TYPE  C( 1),  ;
    DOC_V     C(11),  ;
    SALE_NUM  C(10),  ;
    SALE_V    C(11),  ;
    SALE_DATE D,      ;
    SUM       N(12,2),;
    WHO       N( 5),  ;
    SAL_NAME  C(30)   )

SELECT 0
USE (base_path+"KASSA") ORDER TAG HRON DESCENDING ALIAS TMP_0418 AGAIN
SCAN WHILE DOC_DATE >= dt
  IF VALUTA = "$"
    LOOP
  ENDIF
  SELECT TMP_ARM
  IF .NOT. SEEK(VAL(TMP_0418.WHERE))
    APPEND BLANK
    REPLACE ARM_C  WITH VAL(TMP_0418.WHERE)
  ENDIF
  REPLACE SUM_S1 WITH SUM_S1+TMP_0418.TOTAL
  SELECT TMP_0418
ENDSCAN

USE (base_path+"INCMONEY") ORDER TAG HRON DESCENDING ALIAS TMP_0418 AGAIN
SCAN WHILE DOC_DATE >= dt
  IF .NOT. DOC_TYPE $ " Ч"
    LOOP
  ENDIF
  SELECT TMP_ARM
  IF .NOT. SEEK(VAL(TMP_0418.WHERE))
    APPEND BLANK
    REPLACE ARM_C WITH VAL(TMP_0418.WHERE)
  ENDIF
  REPLACE SUM_S1 WITH SUM_S1-TMP_0418.MONEY_R-TMP_0418.N_OF_SALE
  IF TMP_0418.DOC_DATE = dt
    IF TMP_0418.DOC_TYPE = " "
      REPLACE SUM_M1 WITH SUM_M1+TMP_0418.MONEY_R+TMP_0418.N_OF_SALE
    ELSE
      REPLACE SUM_M2 WITH SUM_M2+TMP_0418.MONEY_R+TMP_0418.N_OF_SALE
    ENDIF
    SELECT TMP_MOVE
    APPEND BLANK
    REPLACE FIRM      WITH TMP_0418.FIRM,      ;
            ARM_C     WITH VAL(TMP_0418.WHERE),;
            DOC_NUM   WITH TMP_0418.DOC_NUM,   ;
            DOC_TYPE  WITH TMP_0418.DOC_TYPE,  ;
            DOC_TIME  WITH TMP_0418.DOC_TIME,  ;
            DOC_V     WITH LEFT(TMP_0418.DOC_NUM,1)+ALLTRIM(SUBSTR(TMP_0418.DOC_NUM,2)), ;
            SALE_NUM  WITH TMP_0418.SALE_NUM,  ;
            SALE_DATE WITH TMP_0418.SALE_DATE, ;
            SALE_V    WITH IIF(EMPTY(TMP_0418.SALE_NUM), "",  ;
                       LEFT(TMP_0418.SALE_NUM,4)+"-"+ALLTRIM(SUBSTR(TMP_0418.SALE_NUM,5))), ;
            SUM       WITH TMP_0418.MONEY_R+TMP_0418.N_OF_SALE, ;
            WHO       WITH TMP_0418.WHO
  ENDIF
  SELECT TMP_0418
ENDSCAN

USE

USE (base_path+"ARM_LST") ORDER TAG ARM_CODE ALIAS TMP_0418 AGAIN
SCAN && FOR .NOT. EMPTY(KASSA)
  IF .NOT. SEEK(ARM_CODE, "TMP_ARM")
    IF EMPTY(KASSA)
      LOOP
    ENDIF
    SELECT TMP_ARM
    APPEND BLANK
    REPLACE ARM_C WITH TMP_0418.ARM_CODE
    SELECT TMP_0418
  ENDIF
  REPLACE TMP_ARM.ARM_N  WITH TMP_0418.ARM_N, ;
          TMP_ARM.SUM_S1 WITH TMP_ARM.SUM_S1+TMP_0418.KASSA
ENDSCAN
USE
SELECT TMP_MOVE
INDEX ON STR(ARM_C)+DOC_TYPE+STR(DOC_TIME) TAG ARM_C
PRIVATE keyVal, tmpRest, tmpArm
GO TOP
keyVal = STR(ARM_C)+DOC_TYPE
= SEEK(ARM_C, "TMP_ARM")
tmpRest = IIF(DOC_TYPE = " ", TMP_ARM.SUM_S1, 0)
SCAN
  IF keyVal # STR(ARM_C)+DOC_TYPE
    keyVal = STR(ARM_C)+DOC_TYPE
    = SEEK(ARM_C, "TMP_ARM")
    tmpRest = IIF(DOC_TYPE = " ", TMP_ARM.SUM_S1, 0)
  ENDIF
  tmpRest = tmpRest+SUM
  REPLACE REST WITH tmpRest
ENDSCAN

SELECT TMP_ARM
SET ORDER TO TAG ARM_N
GO TOP

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура V_Arms       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                          Список рабочих мест.                          │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.04.2000 ──┘
PROCEDURE V_Arms
PARAMETER prmArm
PRIVATE tmpArm

*
*  Переменные состояния для навигации
*
IF TYPE("prmArm") = "C"
  tmpArm = ASC(prmArm)
ELSE
  IF TYPE("prmArm") = "N"
    tmpArm = prmArm
  ENDIF
ENDIF

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

SELECT 0
DO Use_Dummy
SELECT TMP_ARM
IF TYPE("tmpArm") = "N"
  SET ORDER TO TAG ARM_C
ENDIF

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Orders OF (menu_name) PROMPT "Приходные ордера" KEY Ctrl-A, "Enter"
ON SELECTION PAD Orders OF (menu_name) DO Sw_Mode WITH "Orders"

DEFINE PAD Checks OF (menu_name) PROMPT "Чеки" KEY Ctrl-A, "Ctrl+Enter"
ON SELECTION PAD Checks OF (menu_name) DO Sw_Mode WITH "Checks"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = IIF(TYPE("tmpArm") = "N", 1, WROWS("")-16)  && Количество видимых строк BROWSE
wd = 55

DO D_Wins WITH ln, wd, ;
     IIF(TYPE("tmpArm") = "N", "Состояние касс на ", "Состояние кассы на ")+DTOC(dt), 0, 0
what_do = "List"
*┌─Раб. место────────Остаток────Прих.ордера──────Чеки────┐
*│....:....!....:. 999999999.99 999999999.99 999999999.99│
*└───────────────────────────────────────────────────────┘
@ 1, 2 SAY "─Раб. место────────Остаток────Прих.ордера──────Чеки"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter      KEYBOARD CHR(23)
    ON KEY LABEL Ctrl+Enter DO Sw_Mode WITH "Checks"

*
*    BROWSE - меню
*
    IF TYPE("tmpArm") = "N"
      BROWSE KEY tmpArm ;
             FIELDS ARM_N:16:H="",     ;
                    SUM_S1:H="":P="999999999.99", ;
                    SUM_M1:H="":P="999999999.99", ;
                    SUM_M2:H="":P="999999999.99", ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             NOMODIFY   ;
             NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ELSE
      BROWSE FIELDS ARM_N:16:H="",     ;
                    SUM_S1:H="":P="999999999.99", ;
                    SUM_M1:H="":P="999999999.99", ;
                    SUM_M2:H="":P="999999999.99", ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             NOMODIFY   ;
             NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ENDIF
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Orders")
    ENDIF

  CASE what_do = "Checks"    && Просмотр чеков

    DO V_Check
    what_do = "List"

  CASE what_do = "Orders"    && Приходные ордера

    DO V_Ord
    what_do = "List"

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
*│   Процедура V_Check      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Просмотр чеков.                             │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.04.2000 ──┘
PROCEDURE V_Check

*
*  Переменные состояния для навигации
*

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

SELECT TMP_MOVE
GO TOP

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD P_Check OF (menu_name) PROMPT "Печать чека" KEY Ctrl-A, "Enter"
ON SELECTION PAD P_Check OF (menu_name) DO Sw_Mode WITH "P_Check"

DEFINE PAD P_Inv OF (menu_name) PROMPT "Печать тов. чека" KEY Ctrl-A, "Ctrl_Enter"
ON SELECTION PAD P_Inv OF (menu_name) DO Sw_Mode WITH "P_Inv"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 60

DO D_Wins WITH ln, wd, ALLTRIM(TMP_ARM.ARM_N)+" (Чеки)", 0, 0
*┌─Чек N──────Тов. чек N────Дата─────────Сумма──────Остаток───┐
*│DDDDDDDDDDD SSSSSSSSSSS ДД.ММ.ГГГГ 999999999.99 999999999.99│
*└────────────────────────────────────────────────────────────┘
@ 1, 2 SAY "─Чек N──────Тов. чек N────Дата─────────Сумма──────Остаток"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter      KEYBOARD CHR(23)
    ON KEY LABEL Ctrl+Enter DO Sw_Mode WITH "P_Inv"

*
*    BROWSE - меню
*
    BROWSE KEY STR(TMP_ARM.ARM_C)+"Ч" ;
    FIELDS DOC_V    :H="",  ;
           SALE_V   :H="",  ;
           SALE_DATE:H="",  ;
           SUM      :H="":P="999999999.99", ;
           REST     :H="":P="999999999.99", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "P_Check")
    ENDIF

  CASE what_do = "P_Check"    && Печать чека

    DO P_Money WITH FIRM, DOC_TYPE, DOC_NUM, dt
    what_do = "List"

  CASE what_do = "P_Inv"      && Печать тов. чека

    DO Prn_Inv WITH FIRM, SALE_NUM, SALE_DATE
    what_do = "List"

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
*│   Процедура V_Ord        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                     Просмотр приходных ордеров.                        │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.04.2000 ──┘
PROCEDURE V_Ord

*
*  Переменные состояния для навигации
*

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

SELECT TMP_MOVE
GO TOP

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD P_Check OF (menu_name) PROMPT "Печать прих ордера" KEY Ctrl-A, "Enter"
ON SELECTION PAD P_Check OF (menu_name) DO Sw_Mode WITH "P_Check"

DEFINE PAD P_Inv OF (menu_name) PROMPT "Печать накладной" KEY Ctrl-A, "Ctrl_Enter"
ON SELECTION PAD P_Inv OF (menu_name) DO Sw_Mode WITH "P_Inv"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 60

DO D_Wins WITH ln, wd, ALLTRIM(TMP_ARM.ARM_N)+" (Прих. ордера)", 0, 0
*┌Прих.ордер───Накладная────Дата─────────Сумма──────Остаток───┐
*│DDDDDDDDDDD SSSSSSSSSSS ДД.ММ.ГГГГ 999999999.99 999999999.99│
*└────────────────────────────────────────────────────────────┘
@ 1, 2 SAY "Прих.ордер───Накладная────Дата─────────Сумма──────Остаток"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter      KEYBOARD CHR(23)
    ON KEY LABEL Ctrl+Enter DO Sw_Mode WITH "P_Inv"

*
*    BROWSE - меню
*
    BROWSE KEY STR(TMP_ARM.ARM_C)+" " ;
    FIELDS DOC_V    :H="",  ;
           SALE_V   :H="",  ;
           SALE_DATE:H="",  ;
           SUM      :H="":P="999999999.99", ;
           REST     :H="":P="999999999.99", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "P_Check")
    ENDIF

  CASE what_do = "P_Check"    && Печать чека


    DO P_Money WITH FIRM, DOC_TYPE, DOC_NUM, dt
    what_do = "List"

  CASE what_do = "P_Inv"      && Печать тов. чека

    DO Prn_Inv WITH FIRM, SALE_NUM, SALE_DATE
    what_do = "List"

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
