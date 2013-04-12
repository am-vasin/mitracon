*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Ord_Exe1     Разработчик Андрей Васин           12.03.98 15:34:32 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                      Обработка заявок на складе.                       ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Ord_Exe1
PARAMETER q_type

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

*
*   Заполняем значениями переменные состояния...
*
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()
PRIVATE flt, cur_zone, nam_zone, mark_lst, mark_ord, tmpQueue, ord
mark_lst = 0
mark_ord = ""
flt = .T.
ord = .F.
cur_zone = ""
nam_zone = "Все зоны"
IF TYPE("q_type") = "C"
  tmpQueue = q_type
ELSE
  tmpQueue = Queue(" ")
ENDIF

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
USE (base_path+"BOXES") ORDER TAG BOX_NO
SELECT 0

USE (base_path+"BOXCONT") ORDER TAG LIST_IN
SET RELATION TO BOX_NO INTO BOXES
SELECT 0

USE (base_path+"LIST_TIT") ORDER TAG LST_NUM
SELECT 0

USE (base_path+"PERSONS") ORDER TAG CODE ALIAS STOCKMAN
SELECT 0

USE (base_path+"PERSONS") ORDER TAG CODE ALIAS SALEMAN AGAIN
SELECT 0

USE (base_path+"PERSONS") ORDER TAG CODE ALIAS WORKER AGAIN
SELECT 0

USE (base_path+"ORD_T") ORDER TAG DOC_NUM_1
SET RELATION TO WHO INTO SALEMAN
SET RELATION TO STO_MARK INTO STOCKMAN ADDITIVE
SET RELATION TO VAL(ALLTRIM(CHECK_S)) INTO WORKER ADDITIVE
SET RELATION TO VAL(ALLTRIM(LIST_NUM)) INTO LIST_TIT ADDITIVE
GO BOTTOM
DO Use_Dummy
SELECT ORD_T

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Print OF (menu_name) PROMPT "Печать"
ON PAD Print OF (menu_name) ACTIVATE POPUP Print

  DEFINE POPUP Print MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Print PROMPT "Формат заявки" KEY Ctrl-A, "F1"
  ON SELECTION BAR 1 OF Print DO Sw_Mode WITH "Print_1"

  DEFINE BAR  2 OF Print PROMPT "Формат списка" KEY Ctrl-A, "F2"
  ON SELECTION BAR 2 OF Print DO Sw_Mode WITH "Print_2"

  DEFINE BAR  3 OF Print PROMPT "Список" KEY Ctrl-A, "F4"
  ON SELECTION BAR 3 OF Print DO Sw_Mode WITH "Print_L"

  DEFINE BAR  4 OF Print PROMPT "Бронь" KEY Ctrl-A, "F5"
  ON SELECTION BAR 4 OF Print DO Sw_Mode WITH "Print_B"

  DEFINE BAR  5 OF Print PROMPT "Суммарная заявка" KEY Ctrl-A, "F7"
  ON SELECTION BAR 5 OF Print DO Sw_Mode WITH "Print_S"

DEFINE PAD Work OF (menu_name) PROMPT "Обработка"
ON PAD Work OF (menu_name) ACTIVATE POPUP Work

  DEFINE POPUP Work MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Work PROMPT "В наборе" KEY Ctrl-A, "Tab"
  ON SELECTION BAR 1 OF Work DO Sw_Mode WITH "Prepare"

  DEFINE BAR  2 OF Work PROMPT "Утверждение" KEY Ctrl-A, "Enter"
  ON SELECTION BAR 2 OF Work DO Sw_Mode WITH "Terminate"

  DEFINE BAR  3 OF Work PROMPT "Установка адресов" KEY Ctrl-A, "Ins"
  ON SELECTION BAR 3 OF Work DO Sw_Mode WITH "Address"

  DEFINE BAR  4 OF Work PROMPT "Просмотр адресов" KEY Ctrl-A, "Home"
  ON SELECTION BAR 4 OF Work DO Sw_Mode WITH "ViewAddr"

  DEFINE BAR  5 OF Work PROMPT "Отметить" KEY Ctrl-A, "Shift_Tab"
  ON SELECTION BAR 5 OF Work DO Sw_Mode WITH "Mark"

DEFINE PAD Options OF (menu_name) PROMPT "Настройки"
ON PAD Options OF (menu_name) ACTIVATE POPUP Options

  DEFINE POPUP Options MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Options PROMPT "Время работы склада"
  ON SELECTION BAR 1 OF Options DO Sw_Mode WITH "Work Time"

  DEFINE BAR  2 OF Options PROMPT "MIN время подборки"
  ON SELECTION BAR 2 OF Options DO Sw_Mode WITH "Min Time"

  DEFINE BAR  3 OF Options PROMPT "Сотрудники склада"
  ON SELECTION BAR 3 OF Options DO Sw_Mode WITH "Workers"

DEFINE PAD View OF (menu_name) PROMPT "Вид"
ON PAD View OF (menu_name) ACTIVATE POPUP View

  DEFINE POPUP View MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF View PROMPT "Фильтр" KEY Ctrl-A, "F6"
  ON SELECTION BAR 1 OF View DO Sw_Mode WITH "Filter"

  DEFINE BAR 2 OF View PROMPT "Порядок" KEY Ctrl-A, "F9"
  ON SELECTION Bar 2 OF View DO Sw_Mode WITH "H_Ord"

  DEFINE BAR  3 OF View PROMPT "Зона" KEY Ctrl-A, "F3"
  ON SELECTION BAR 3 OF View DO Sw_Mode WITH "Stat"

  DEFINE BAR  4 OF View PROMPT "Поиск" KEY Ctrl-A, "F8"
  ON SELECTION BAR 4 OF View DO Sw_Mode WITH "Seek"

*  DEFINE BAR  5 OF View PROMPT "Состояние" KEY Ctrl-A, "F9"
*  ON SELECTION BAR 5 OF View DO Sw_Mode WITH "Stat"

  DEFINE BAR  6 OF View PROMPT "Гистограммы" KEY Ctrl-A, "Ctrl+F9"
  ON SELECTION BAR 6 OF View DO Sw_Mode WITH "Hist"

  IF TYPE("q_type") # "C"
    DEFINE BAR  7 OF View PROMPT "Тип очереди"
    ON SELECTION BAR 7 OF View DO Sw_Mode WITH "Queue"
  ENDIF

  DEFINE BAR  8 OF View PROMPT "Только возвратные" KEY Ctrl-A, "Ctrl+F6" BEFORE 2
  ON SELECTION BAR 8 OF View DO Sw_Mode WITH "Reverse"

  DEFINE BAR  9 OF View PROMPT "Автор списка" KEY Ctrl-A, "Ctrl+F2"
  ON SELECTION BAR 9 OF View DO Sw_Mode WITH "Who_Lst"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd, rev
ln = 100   && Количество видимых строк BROWSE - во весь экран!
wd = 75
rev = .F.

DO D_Wins WITH ln, wd, "Обработка заявок", 1, 4
what_do = "List"
*┌─N заявки────Готовность к────Имя списка──────────────────────С──З─Дл.+─Дл.-┐
*│1234567890 ДД.ММ.ГГГГ ЧЧ:ММ 12345678901234567890123456789012 С ЗЗ 9999 9999│
*└───────────────────────────────────────────────────────────────────────────┘
*┌Время подачи заявки─Примечание─────────────────────Адрес───────────────────┐
*│ДД.ММ.ГГГГ ЧЧ:ММ:СС ....:....!....:....!....:....! ....:....!....:....!....│
*└───────────────────────────────────────────────────────────────────────────┘
@ 2, 2 SAY ;
"─N заявки────Готовность к────Имя списка──────────────────────С──З─Дл.+─Дл.-"
@ WROWS()-6, 2 SAY "Время подачи заявки"
@ WROWS()-6, 22 SAY "Примечание"
@ WROWS()-6, 53 SAY "Адрес"
DO WHILE .T.

  DO P_Mode

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter    KEYBOARD CHR(23)
    ON KEY LABEL Tab      DO Sw_Mode WITH "Prepare"
    ON KEY LABEL BackTab  DO Mark_O WITH .T.
    ON KEY LABEL Ins      DO Sw_Mode WITH "Address"
    ON KEY LABEL Home     DO Sw_Mode WITH "ViewAddr"
    ON KEY LABEL F1       DO Sw_Mode WITH "Print_1"
    ON KEY LABEL F2       DO Sw_Mode WITH "Print_2"
    ON KEY LABEL F3       DO Sw_Mode WITH "Stat"
    ON KEY LABEL F4       DO Sw_Mode WITH "Print_L"
    ON KEY LABEL F5       DO Sw_Mode WITH "Print_B"
    ON KEY LABEL F6       DO Sw_Mode WITH "Filter"
    ON KEY LABEL F7       DO Sw_Mode WITH "Print_S"
    ON KEY LABEL F8       DO Sw_Mode WITH "Seek"
    ON KEY LABEL F9       DO Sw_Mode WITH "H_Ord"
    ON KEY LABEL Ctrl+F9  DO Sw_Mode WITH "Hist"
    ON KEY LABEL Ctrl+F6  DO Sw_Mode WITH "Reverse"
    ON KEY LABEL Ctrl+F2  DO Sw_Mode WITH "Who_Lst"

*
*    BROWSE - меню
*
*    BROWSE KEY tmpQueue+cur_zone
    BROWSE FIELDS F000 = LIST_NUM+"-"+ALLTRIM(STR(ORD_NUM)):10:H="",   ;
                  TIME000 = Time000():16:H="", ;
                  LIST_NAME:32:H="", ;
                  F002 = IIF(.NOT. EMPTY(STO_MARK), "У",    ;
                          IIF(.NOT. EMPTY(CHECK_S), "Н", " ")):1:H="", ;
                  ZONE:H="":2,       ;
                  SIZE_POS:H="":P="@Z 9999", ;
                  SIZE_NEG:H="":P="@Z 9999", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Show_Who() ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Terminate")
    ENDIF

  CASE what_do = "Prepare"    && Заявка в наборе

*    IF .NOT. BOF() .AND. .NOT. EOF()
*      what_do = Prepare()
*      what_do = IIF(EMPTY(what_do), "List", what_do)
*    ELSE
      what_do = "List"
*    ENDIF

  CASE what_do = "Mark"       && Отметить

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO Mark_O
    ENDIF
    what_do = "List"

  CASE what_do = "Address"    && Адрес

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO Address
      GO RECNO()
      = Show_Who()
    ENDIF
    what_do = "List"

  CASE what_do = "ViewAddr"   && Адрес

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO ViewAddr WITH LIST_TIT.LST_NUM
    ENDIF
    what_do = "List"

  CASE what_do = "Stat"       && Состояние

    DO Stat
    what_do = "List"

  CASE what_do = "Hist"       && Гистограммы

    ACTIVATE SCREEN
    @ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
    DO Ord_Hist
    what_do = "List"

  CASE what_do = "Terminate"    && Утверждение заявки

    IF .F.	&&.NOT. BOF() .AND. .NOT. EOF()
      DO CASE
      CASE sale_mode = "Резервный режим"
        DO Ord_Rez WITH ORD_T.LIST_NUM, ORD_T.ORD_NUM
      CASE sale_mode = "Mitracon" .OR. sale_mode = "Mitin"
        DO Ord_Mtk WITH ORD_T.LIST_NUM, ORD_T.ORD_NUM
      OTHERWISE
        DO Corr_Ord WITH ORD_T.LIST_NUM, ORD_T.ORD_NUM
      ENDCASE
      GO RECNO()
    ENDIF
    what_do = "List"

  CASE what_do = "Reverse"    && Только возвратные

    IF rev
      rev = .F.
      SET FILTER TO
      DEFINE BAR  8 OF View PROMPT "Только возвратные" KEY Ctrl-A, "Ctrl+F6" BEFORE 2
    ELSE
      rev = .T.
      SET FILTER TO SIZE_NEG # 0
      DEFINE BAR  8 OF View PROMPT "√Только возвратные" KEY Ctrl-A, "Ctrl+F6" BEFORE 2
    ENDIF
    what_do = "List"

  CASE what_do = "Print_1"    && Печать заявки

    IF new_ord
      DO Prn_OrdN WITH VAL(ALLTRIM(ORD_T.LIST_NUM)), ORD_T.ORD_NUM
    ELSE
      DO Prn_Ord WITH VAL(ALLTRIM(ORD_T.LIST_NUM)), ORD_T.ORD_NUM
    ENDIF
    what_do = "List"

  CASE what_do = "Print_2"    && Печать списка

    DO Prn_Ord1 WITH VAL(ALLTRIM(ORD_T.LIST_NUM)), ORD_T.ORD_NUM
    what_do = "List"

  CASE what_do = "Print_L"    && Печать списка

    DO P_List WITH VAL(ALLTRIM(ORD_T.LIST_NUM))
    what_do = "List"

  CASE what_do = "Print_B"    && Печать брони

    DO P_Bron WITH VAL(ALLTRIM(ORD_T.LIST_NUM))
    what_do = "List"

  CASE what_do = "Print_S"    && Печать суммарной заявки

    DO Prn_OrdS WITH VAL(ALLTRIM(ORD_T.LIST_NUM))
    ACTIVATE WINDOW (win_name+"_M") SAME
    mark_ord = ""
    mark_lst = 0
    @ 1, 39 CLEAR TO 1, WCOLS()
    what_do = "List"

  CASE what_do = "H_Ord"        && Порядок по хронологии/номеру заявки

    ord = .NOT. ord
    DO P_Mode
    what_do = "List"

  CASE what_do = "Min Time"     && MIN время подборки

    DO Sto_Time
    what_do = "List"

  CASE what_do = "Work Time"    && Рабочее время склада

    DO Work_Time
    what_do = "List"

  CASE what_do = "Workers"      && Сотрудники склада

    DO Sto_Men
    what_do = "List"

  CASE what_do = "Zone"         && Зона

    DO Ch_Zone
    what_do = "List"

  CASE what_do = "Filter"       && Установка/снятие фильтра

    flt = .NOT. flt
    what_do = "List"

  CASE what_do = "Queue"        && Тип очереди

    tmpQueue = Queue(tmpQueue)
    what_do = "List"

  CASE what_do = "Seek"         && Поиск заявок для списка

    DO Seek_L
    what_do = "List"

  CASE what_do = "Who_Lst"      && Автор списка

    DO S_Author WITH LIST_TIT.WHO_CR
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
CLOSE DATABASES
RELEASE MENU (menu_name) EXTENDED

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Time000      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                        Время готовности списка.                        │
*│                                                                        │
*└────────────────────────────────────────────────────────── 12.01.2000 ──┘
PROCEDURE Time000

DO CASE
CASE .NOT. FOUND("LIST_TIT")
  RETURN " Список удален  "
CASE .NOT. EMPTY(LIST_TIT.SAL_NUM)
  RETURN " Список продан  "
CASE EMPTY(REQ_DATE)
  RETURN "  Не собирать!"
ENDCASE
RETURN DTOC(REQ_DATE)+" "+LEFT(REQ_TIME,5)

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║             Визуализация и установка текущего режима.                  ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 12.03.98 16:16:52 ═╝
PROCEDURE P_Mode

DO CASE
CASE flt && .AND. ord
  SET ORDER TO TAG DOC_NUM_1
CASE .NOT. flt && .AND. ord
  SET ORDER TO TAG DOC_NUM
CASE flt .AND. LEN(cur_zone) = 0
  SET ORDER TO TAG REQUIRE_1
CASE .NOT. flt .AND. LEN(cur_zone) = 0
  SET ORDER TO TAG REQUIRE
CASE flt
  SET ORDER TO TAG ZONE_1
CASE .NOT. flt
  SET ORDER TO TAG ZONE
ENDCASE

ACTIVATE WINDOW (win_name+"_M") SAME
@ 1, 3 SAY "Зона: "+nam_zone+ ;
   "  Фильтр "+IIF(flt, "Неутв.", "Все   ")+ ;
   "  Отм."

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                                Footer!                                 ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 12.03.98 16:55:20 ═╝
PROCEDURE Show_Who
PRIVATE n_sal, n_sto, n_work, ln, s_sav

@ WROWS()-5,  2 SAY DTOC(ORD_T.ORD_DATE)+" "+ORD_T.ORD_TIME
@ WROWS()-5, 22 SAY ORD_T.NOTE

ln = FSIZE("FAMILY","SALEMAN")+24
IF WHO = 0
  n_sal = ""
ELSE
  IF FOUND("SALEMAN")
    n_sal = ALLTRIM(SALEMAN.FAMILY)+" "+LEFT(SALEMAN.NAME,1)+". "+ ;
            LEFT(SALEMAN.S_NAME,1)+"."
  ELSE
    n_sal = "?"
  ENDIF
ENDIF
n_sal = PADR(n_sal, ln)

IF STO_MARK = 0
  n_sto = ""
ELSE
  IF FOUND("STOCKMAN")
    n_sto = ALLTRIM(STOCKMAN.FAMILY)+" "+LEFT(STOCKMAN.NAME,1)+". "+ ;
            LEFT(STOCKMAN.S_NAME,1)+"."
  ELSE
    n_sto = "?"
  ENDIF
  n_sto = n_sto+" "+DTOC(TERM_DATE)+" "+TERM_TIME
ENDIF
n_sto = PADR(n_sto, ln)

IF CHECK_S = "   "
  n_work = ""
ELSE
  IF FOUND("WORKER")
    n_work = ALLTRIM(WORKER.FAMILY)+" "+LEFT(WORKER.NAME,1)+". "+ ;
            LEFT(WORKER.S_NAME,1)+". c "+DTOC(START_DATE)+" "+START_TIME
  ELSE
    n_work = ""
  ENDIF
ENDIF
n_work = PADR(n_work, ln)

ACTIVATE WINDOW (win_name+"_M") SAME
@ WROWS()-4, 3 SAY "Продавец  "+n_sal
@ WROWS()-3, 3 SAY "Собирает  "+n_work
@ WROWS()-2, 3 SAY "Кладовщик "+n_sto
@ WROWS()-4,48 SAY "Старый адрес "+LIST_TIT.ADDRESS
n_sto = SPACE(20)
*IF FOUND("LIST_TIT")
*  IF .NOT. EMPTY(LIST_TIT.ADDRESS)
*    n_sto = LIST_TIT.ADDRESS
*  ELSE
*    n_sto = SPACE(FSIZE("ADDRESS","LIST_TIT"))
*  ENDIF
*ENDIF
*s_sav = SELECT()
*SELECT BOXCONT
*SEEK ORD_T.LIST_NUM
*IF FOUND()
*  n_sto = BOXES.BOXADDRESS
*  SKIP
*  IF .NOT. EOF()
*    IF LIST_NUM == ORD_T.LIST_NUM
*      n_sto = n_sto+" ..."
*    ELSE
*      n_sto = n_sto+"    "
*    ENDIF
*  ELSE
*    n_sto = n_sto+"    "
*  ENDIF
*ENDIF
*SELECT (s_sav)
n_sto = ListAddr(ORD_T.LIST_NUM, 24)

@ WROWS()-5, 53 SAY n_sto
IF WEXIST(ALIAS())
  ACTIVATE WINDOW (ALIAS())
ENDIF

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                    Коррекция и утверждение заявки.                     ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 12.03.98 18:29:40 ═╝
PROCEDURE Corr_Ord
PARAMETERS lst_n, ord_n

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

*
*   Заполняем значениями переменные состояния...
*
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

PRIVATE s_sav
s_sav = SELECT()

PRIVATE lst_n_w, mode, is_name, ord_lst
lst_n_w = IIF(TYPE("lst_n") = "C", lst_n, STR(lst_n,6))
ord_lst = GetOrds(lst_n_w, ord_n)
IF EMPTY(ord_lst)
  RETURN
ENDIF
is_name = .T.
SELECT 0
USE (base_path+"ORD_T") ORDER TAG DOC_NUM ALIAS OT_000 AGAIN

mode = .T.

SELECT 0
USE (base_path+"LIST_TIT") ORDER TAG LST_NUM ALIAS LS_000 AGAIN
IF .NOT. SEEK(VAL(ALLTRIM(lst_n_w)))
  PRIVATE mss
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"Списка "+ALLTRIM(lst_n)+" не существует!"
  mss(3) = CHR(0)+"Коррекция заявки недоступна."
  mss(4) = ""
  DO Out_Mess WITH 7, "mss"
  mode = .F.
ELSE
  IF .NOT. EMPTY(LS_000.SAL_NUM)
    PRIVATE mss
    DIMENSION mss(4)
    mss(1) = ""
    mss(2) = CHR(0)+"Список "+ALLTRIM(lst_n)+" уже продан!"
    mss(3) = CHR(0)+"Коррекция заявки недоступна."
    mss(4) = ""
    DO Out_Mess WITH 7, "mss"
     mode = .F.
  ENDIF
ENDIF
USE

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS ST_000 AGAIN

DELETE FILE(tmpo_path+"O_TMP.DBF")
DELETE FILE(tmpo_path+"O_TMP.CDX")
CREATE DBF (tmpo_path+"O_TMP")  ;
  ( CODE     N( 7), ;
    ADDRESS  C( 8), ;
    PREFIX   C( 4), ;
    NAME     C(35), ;
    PRODUCER C( 8), ;
    QNT_ORD  N( 7), ;
    QNT_RES  N( 7), ;
    LET_CORR L,     ;
    DUMMY    C( 1)  )
USE (tmpo_path+"O_TMP") EXCLU
INDEX ON ADDRESS TAG ADDRESS
INDEX ON NAME+PREFIX+PRODUCER TAG NAME

SELECT 0
USE (base_path+"ORD_D") ORDER TAG DOC_NUM ALIAS ORD_000 AGAIN
SET RELATION TO CODE INTO ST_000

PRIVATE tmpOrds
tmpOrds = ord_lst
DO WHILE LEN(tmpOrds) # 0
  SEEK lst_n_w+LEFT(tmpOrds,3)
  SCAN REST WHILE lst_n_w = LIST_NUM .AND. VAL(ALLTRIM(LEFT(tmpOrds,3))) = ORD_NUM
    IF QNT = 0
      LOOP
    ENDIF
    SELECT O_TMP
    APPEND BLANK
    REPLACE CODE     WITH ORD_000.CODE,   ;
            ADDRESS  WITH ST_000.ADDRESS, ;
            PREFIX   WITH ST_000.PREFIX,  ;
            NAME     WITH ST_000.NAME,    ;
            PRODUCER WITH ST_000.PRODUCER,;
            QNT_ORD  WITH ORD_000.QNT,    ;
            QNT_RES  WITH ORD_000.QNT
    SELECT ORD_000
  ENDSCAN
  tmpOrds = SUBSTR(tmpOrds,4)
ENDDO

USE
SELECT O_TMP

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Order OF (menu_name) PROMPT "По адресам" KEY Ctrl-A, "F3"
ON SELECTION PAD Order OF (menu_name) DO Sw_Mode WITH "Order"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd, pic_w
pic_w = Addr_Pic()
ln = MAX(1, RECCOUNT())   && Количество видимых строк BROWSE
wd = 64

DO D_Wins WITH ln, wd, "Заявка N "+ALLTRIM(lst_n)+"/"+ALLTRIM(STR(ord_n)), 0, 0
@ 1, 2 SAY ;
"──Адрес────────Наименование───────────────────────Заказ─Отпуск──"
what_do = "List"

GO TOP
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL F3 DO Sw_Mode WITH "Order"

*
*    BROWSE - меню
*
    IF mode
      BROWSE FIELDS ADDRESS:H="":P=pic_w, PREFIX:H="", NAME:H="",   ;
                    PRODUCER:H="", QNT_ORD:H="", QNT_RES:H="",   ;
                    F000 = IIF(QNT_ORD=QNT_RES, " ", "√"):1:H="", ;
                    DUMMY:H="" FREEZE QNT_RES ;
             NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ELSE
      BROWSE FIELDS ADDRESS:H="":P=pic_w, PREFIX:H="", NAME:H="",   ;
                    PRODUCER:H="", QNT_ORD:H="", QNT_RES:H="",   ;
                    F000 = IIF(QNT_ORD=QNT_RES, " ", "√"):1:H="", ;
                    DUMMY:H="" FREEZE DUMMY ;
             NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ENDIF
    ON KEY
    IF what_do = "List"
      what_do = "Exit"
    ENDIF

  CASE what_do = "Order"    && Переупорядочение

    is_name = .NOT. is_name
    IF is_name
      SET ORDER TO TAG NAME
      DEFINE PAD Order OF (menu_name) PROMPT "По адресам"   ;
            BEFOR Exit KEY Ctrl-A, "F3"
    ELSE
      SET ORDER TO TAG ADDRESS
      DEFINE PAD Order OF (menu_name) PROMPT "По именам"   ;
            BEFOR Exit KEY Ctrl-A, "F3"
    ENDIF    
    what_do = "List"

  OTHERWISE

*    IF Terminate()
      EXIT
*    ELSE
*      what_do = "List"
*    ENDIF

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
USE
DELETE FILE(tmpo_path+"O_TMP.DBF")
DELETE FILE(tmpo_path+"O_TMP.CDX")
SELECT OT_000
USE
SELECT ST_000
USE

SELECT (s_sav)

RETURN

**------------------------------------------------
*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                 Утверждение заявки ( резервный режим ).                ║
*║                                                                        ║
*╚═══════════════════════════════════════════════════════════ 06.03.2001 ═╝
PROCEDURE Ord_Rez
PARAMETERS lst_n, ord_n

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

*
*   Заполняем значениями переменные состояния...
*
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
PRIVATE s_sav
s_sav = SELECT()

PRIVATE lst_n_w, ord_lst, who_0, mss
lst_n_w = IIF(TYPE("lst_n") = "C", lst_n, STR(lst_n,6))
ord_lst = ord_n
who_0 = 0

IF EMPTY(ord_lst)
  RETURN
ENDIF


SELECT 0
USE (base_path+"STO_MEN") ORDER TAG WORK ALIAS SM_0631 AGAIN
COUNT TO r01 FOR .NOT.EMPTY(CODE)

DO CASE
  CASE r01 > 1
    who_0 = Ch_SMen(who_0, "Кто утверждает заявку")

  CASE r01 = 1
  LOCATE FOR .NOT. EMPTY(CODE)
   who_0 = CODE

  OTHERWISE

    DIMENSION mss(3) 
    mss(1) = ""
    mss(2) = CHR(0)+"Неизвестно, кто должен утвердить заявку! Заявка не утверждена ! "
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    USE  

    SELECT (s_sav)
    RETURN
  ENDCASE
USE
 
SELECT 0
USE (base_path+"ORD_T") ORDER TAG DOC_NUM ALIAS OT_0631 AGAIN
SEEK (lst_n_w+STR(ord_lst,3))

IF .NOT.EMPTY(STO_MARK)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+" Вы действительно отказываетесь от утверждения? [ Да ] [ Нет ]"
  mss(3) = ""
  IF Out_Mess(7,"mss") = 1
    REPLACE OT_0631.STO_MARK WITH 0
  ENDIF
ELSE  
  REPLACE OT_0631.STO_MARK WITH who_0
ENDIF
USE

SELECT (s_sav)
POP KEY
RELEASE WINDOW (win_name)

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                    Утверждение заявки ( Митракон ).                    ║
*║                                                                        ║
*╚═══════════════════════════════════════════════════════════ 06.03.2001 ═╝
PROCEDURE Ord_Mtk
PARAMETERS lst_n, ord_n

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

*
*   Заполняем значениями переменные состояния...
*
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
PRIVATE s_sav
s_sav = SELECT()

PRIVATE lst_n_w, ord_lst, who_0, mss
lst_n_w = IIF(TYPE("lst_n") = "C", lst_n, STR(lst_n,6))
ord_lst = ord_n
who_0 = 0

IF EMPTY(ord_lst)
  RETURN
ENDIF

who_0 = USER
 
SELECT 0
USE (base_path+"ORD_T") ORDER TAG DOC_NUM ALIAS OT_0631 AGAIN
SEEK (lst_n_w+STR(ord_lst,3))

IF .NOT.EMPTY(STO_MARK)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+" Вы действительно отказываетесь от утверждения? [ Да ] [ Нет ]"
  mss(3) = ""
  IF Out_Mess(7,"mss") = 1
    REPLACE OT_0631.STO_MARK WITH 0
  ENDIF
ELSE  
  REPLACE OT_0631.STO_MARK WITH who_0
ENDIF
USE

SELECT (s_sav)
POP KEY
RELEASE WINDOW (win_name)
RETURN
**------------------------------------------------

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                      Завершение работы с заявкой.                      ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 13.03.98 09:53:59 ═╝
PROCEDURE Terminate

*
*  Переменные состояния для навигации
*
PRIVATE who_0, mss, tmpCnt, cnt_pos, cnt_neg
*IF OT_000.STO_MARK # 0
*  RETURN
*ENDIF

who_0 = 0
DO WHILE .T.
  who_0 = Ch_SMen(who_0, "Кто утверждает заявку")
  IF EMPTY(who_0)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+" Вы действительно отказываетесь от утверждения? [ Да ] [ Нет ]"
    mss(3) = ""
    IF Out_Mess(7,"mss") = 1
      EXIT
    ENDIF
  ELSE
    = SEEK(who_0, "WORKER")
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"  Заявку утвердил "+  ;
             ALLTRIM(WORKER.FAMILY)+" "+   ;
             LEFT(WORKER.NAME,1)+" "+  ;
             LEFT(WORKER.S_NAME,1)+"? [ Да ] [ Нет ]"
    mss(3) = ""
    IF Out_Mess(5,"mss") = 1
      EXIT
    ENDIF
    who_0 = 0
  ENDIF
ENDDO
IF who_0 # 0
  PRIVATE s_sav, cnt, o_n, r_sav
  s_sav = SELECT()
  SELECT OT_000
  r_sav = RECNO()
  SELECT 0
  USE (base_path+"LIST_TIT") ORDER TAG LST_NUM ALIAS OLT_000 AGAIN
  SELECT 0
  USE (base_path+"LIST_DET") ORDER TAG L_CODE ALIAS OLD_000 AGAIN
  SELECT 0
  USE (base_path+"ORD_D") ALIAS OD_000 AGAIN
  *
  *   Выставляем пометку об утверждении
  *
  cnt = .T.
  *  Ищем скорректированную позицию
  SELECT O_TMP
  LOCATE FOR QNT_ORD # QNT_RES
  IF .NOT. SEEK(lst_n_w, "OLT_000")
    cnt = .F.    && Нет списка - не формируем заявку!
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Список уже удален..."
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
  ELSE
    IF .NOT. EMPTY(OLT_000.SAL_NUM)    && Список продан - не формируем заявку!
      cnt = .F.
      PRIVATE mss
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"Список уже продан..."
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
    ENDIF
  ENDIF
  IF FOUND() .AND. cnt
    && Нашли, начинаем формировать заявку и корректировать список
    &&     и заявку все же надо сформировать...
    o_n = OLT_000.ORD_NUM+1
    REPLACE OLT_000.ORD_NUM WITH o_n      && Номер заявки
    SELECT OT_000
    APPEND BLANK             && Заголовок заявки...
    REPLACE LIST_NUM  WITH STR(OLT_000.LST_NUM,6), ;
            LIST_NAME WITH OLT_000.LST_NAME, ;
            ORD_NUM   WITH o_n,        ;
            ORD_DATE  WITH DATE(),     ;
            ORD_TIME  WITH TIME(),     ;
            REQ_DATE  WITH OLT_000.DATE_REQ, ;
            REQ_TIME  WITH OLT_000.TIME_REQ, ;
            STO_MARK  WITH who_0,      ;
            CHECK_S   WITH ORD_T.CHECK_S,;
            WHO       WITH IIF(RIGHT(OLT_000.LST_NAME,1) = CHR(0), who_0, 0), ;
            WHERE     WITH arm
    SELECT O_TMP
    *
    *  ...и содержимое заявки...
    tmpCnt = 0
    cnt_pos = 0
    cnt_neg = 0
    SCAN REST FOR QNT_ORD # QNT_RES
      tmpCnt = tmpCnt+1
      SELECT OD_000
      APPEND BLANK
      REPLACE LIST_NUM WITH OT_000.LIST_NUM, ;
              ORD_NUM  WITH OT_000.ORD_NUM,  ;
              CODE     WITH O_TMP.CODE,      ;
              QNT      WITH O_TMP.QNT_RES-O_TMP.QNT_ORD
      IF QNT < 0
        cnt_neg = cnt_neg+1
      ELSE
        cnt_pos = cnt_pos+1
      ENDIF
      *
      *   Количества на складе...
      = SEEK(CODE, "ST_000")
      REPLACE ST_000.QNT WITH ST_000.QNT-QNT, ;
              ST_000.QNT_SAVE WITH ST_000.QNT_SAVE+QNT
      *
      *   Количества в списке...
      = SEEK(LIST_NUM+STR(CODE,7), "OLD_000")
      REPLACE OLD_000.QNT     WITH OLD_000.QNT+QNT, ;
              OLD_000.QNT_REQ WITH OLD_000.QNT_REQ+QNT, ;
              OLD_000.QNT_DUE WITH OLD_000.QNT_DUE - ;
                   IIF(RIGHT(OLT_000.LST_NAME,1) = CHR(0), QNT, 0)
      SELECT O_TMP
      IF OD_000.QNT < 0 .AND. File_O(base_path+"ABSENT.DBF")
        DO Absent WITH OD_000.CODE
      ENDIF
    ENDSCAN
    SELECT OT_000
    REPLACE OT_000.SIZE_POS WITH cnt_pos, SIZE_NEG WITH cnt_neg
  ENDIF
  SELECT OT_000
  IF BETWEEN(r_sav,1,RECCOUNT())
    GO r_sav
  ENDIF
  PRIVATE tmpNum, tmpOrds, tmpOrd
  SELECT OT_000
  tmpOrd = TAG()
  SET ORDER TO DOC_NUM
  tmpOrds = ord_lst
  DO WHILE LEN(tmpOrds) # 0
    SEEK lst_n_w+LEFT(tmpOrds,3)
    REPLACE OT_000.STO_MARK  WITH who_0,  ;
            OT_000.TERM_DATE WITH DATE(), ;
            OT_000.TERM_TIME WITH TIME()
    tmpOrds = SUBSTR(tmpOrds, 4)
  ENDDO
  SET ORDER TO (tmpOrd)
  IF FOUND("OLT_000") .AND. EMPTY(OLT_000.SAL_NUM)
    *
    *
    *  Корректируем заголовок списка в части:
    * суммы, наличия дефицита, наличия долга, наличия скидки,
    * наличия спец. цен, неутв. заявок.
    
    PRIVATE ww_0, ww_1, ww_2, ww_3, ww_4, ww_5, ww_6
    STORE " " TO ww_1, ww_2, ww_3, ww_4, ww_5, ww_6
    ww_0 = 0
    SELECT OLD_000
    SEEK STR(OLT_000.LST_NUM,6)
    SCAN REST WHILE LIST_NUM = STR(OLT_000.LST_NUM,6)
      ww_0 = ww_0+QNT*IIF(OLT_000.IS_USD, USD_PRICE, SAL_PRICE)    && Сумма
      *
      *  Дефицит
      = SEEK(CODE,"ST_000")
      IF MAX(QNT-QNT_REQ, 0) > MAX(ST_000.QNT,0)
        ww_1 = "*"
      ENDIF 
      *
      *  Долг
      IF QNT_DUE # 0
        ww_2 = "*"
      ENDIF 
      *
      *  Скидка или спец. цена
      IF IIF(OLT_000.IS_USD, USD_DEF, DEF_PRICE) # IIF(OLT_000.IS_USD, USD_PRICE, SAL_PRICE)
        IF EMPTY(IS_SPEC)
          ww_3 = "*"    && Скидка
        ELSE
          ww_4 = "*"    && Спец.цена
        ENDIF
      ENDIF
    ENDSCAN
    SELECT OT_000
    SEEK STR(OLT_000.LST_NUM,6)
    SCAN REST WHILE LIST_NUM = STR(OLT_000.LST_NUM,6)
      IF EMPTY(STO_MARK)  && Не утверждена на складе
        ww_5 = "*"
      ENDIF
      IF EMPTY(WHO)       && Не утверждена на продаже
        ww_6 = "*"
      ENDIF
    ENDSCAN
    SELECT OLT_000
    REPLACE OLT_000.R_TOTAL  WITH ww_0, ;
            OLT_000.IS_DEF   WITH ww_1, ;
            OLT_000.IS_DUE   WITH ww_2, ;
            OLT_000.IS_DIS   WITH ww_3, ;
            OLT_000.IS_SPEC  WITH ww_4, ;
            OLT_000.NOTE_STO WITH ww_5, ;
            OLT_000.NOTE_SAL WITH ww_6
  ENDIF
  SELECT OT_000
  IF BETWEEN(r_sav,1,RECCOUNT())
    GO r_sav
  ENDIF
  SELECT OLT_000
  USE
  SELECT OLD_000
  USE
  SELECT OD_000
  USE
  SELECT (s_sav)

ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Prepare      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Заявка в наборе...                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 11.01.2000 ──┘
PROCEDURE Prepare

PRIVATE who_0, mss, rc_sav
rc_sav = RECNO()

IF EMPTY(mark_lst)
  who_0 = VAL(ALLTRIM(ORD_T.CHECK_S))
  IF .NOT. EMPTY(who_0)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"  Заявка уже в наборе! Вы хотите скорректировать пометку? [ Да ] [ Нет ]"
    mss(3) = ""
    IF Out_Mess(7, "mss") = 2
      RETURN ""
    ENDIF
  ENDIF
  who_0 = Ch_SMen(who_0, "Кто собирает заявку")
  IF EMPTY(who_0)
    IF .NOT. EMPTY(ORD_T.CHECK_S)
      IF .NOT. EMPTY(STO_MARK)
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"Заявка уже утверждена, отметить набор нельзя!"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        RETURN ""
      ENDIF
      REPLACE ORD_T.CHECK_S    WITH "", ;
              ORD_T.START_DATE WITH {}, ;
              ORD_T.START_TIME WITH ""
    ENDIF
    RETURN ""
  ENDIF
  REPLACE ORD_T.START_DATE WITH IIF(EMPTY(ORD_T.CHECK_S), DATE(), ORD_T.START_DATE), ;
          ORD_T.START_TIME WITH IIF(EMPTY(ORD_T.CHECK_S), TIME(), ORD_T.START_TIME), ;
          ORD_T.CHECK_S    WITH STR(who_0,3)
  RETURN "Print_1"
ELSE
  who_0 = 0
  PRIVATE ordList, n_ords, tmpLst, o_sav, j, is_0, is_1
  o_sav = ORDER()
  SET ORDER TO TAG DOC_NUM
  tmpLst = mark_ord
  n_ords = 0
  DO WHILE .NOT. EMPTY(tmpLst)
    j = AT(",",tmpLst)
    n_ords = n_ords+1
    DIMENSION ordList(n_ords)
    ordList(n_ords) = PADL(ALLTRIM(LEFT(tmpLst,j-1)),3)
    tmpLst = SUBSTR(tmpLst,j+1)
  ENDDO
  STORE .F. TO is_0, is_1
  FOR j = 1 TO n_ords
    SEEK STR(mark_lst,6)+ordList(j)
    IF .NOT. EMPTY(ORD_T.CHECK_S)
      is_0 = .T.
    ENDIF
    IF .NOT. EMPTY(ORD_T.STO_MARK)
      is_1 = .T.
    ENDIF
  ENDFOR
  IF is_1
    PRIVATE mss
    DIMENSION mss(4)
    mss(1) = ""
    mss(2) = CHR(0)+"Среди отмеченных заявок есть утвержденные."
    mss(3) = CHR(0)+'Установка отметки "В работе" запрещена!'
    mss(4) = ""
    DO Out_Mess WITH 7, "mss"
  ELSE
    IF is_0
      PRIVATE mss
      DIMENSION mss(4)
      mss(1) = ""
      mss(2) = CHR(0)+"Среди отмеченных заявок есть уже подбираемые."
      mss(3) = CHR(0)+"Обновим пометку? [ Да ] [ Нет ]"
      mss(4) = ""
      IF Out_Mess(7, "mss") = 2
        is_1 = .T.
      ENDIF
    ENDIF
  ENDIF
  IF is_1
    SET ORDER TO (o_sav)
    IF BETWEEN(rc_sav, 1, RECCOUNT())
      GO rc_sav
    ENDIF
    RETURN ""
  ENDIF
  who_0 = Ch_SMen(who_0, "Кто собирает заявки")
  FOR j = 1 TO n_ords
    SEEK STR(mark_lst,6)+ordList(j)
    IF EMPTY(who_0)
      REPLACE ORD_T.CHECK_S    WITH "", ;
              ORD_T.START_DATE WITH {}, ;
              ORD_T.START_TIME WITH ""
    ELSE
      REPLACE ORD_T.START_DATE WITH IIF(EMPTY(ORD_T.CHECK_S), DATE(), ORD_T.START_DATE), ;
              ORD_T.START_TIME WITH IIF(EMPTY(ORD_T.CHECK_S), TIME(), ORD_T.START_TIME), ;
              ORD_T.CHECK_S    WITH STR(who_0,3)
    ENDIF
  ENDFOR
  SET ORDER TO (o_sav)
  IF BETWEEN(rc_sav, 1, RECCOUNT())
    GO rc_sav
  ENDIF
  RETURN IIF(EMPTY(who_0), "", "Print_S")
ENDIF

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Address      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                                 Адрес                                  │
*│                                                                        │
*└────────────────────────────────────────────────────────── 11.01.2000 ──┘
PROCEDURE Address

PRIVATE mss, viewOnly

viewOnly = .F.
IF .NOT. FOUND("LIST_TIT")
  viewOnly = .T.
ENDIF

IF .NOT. EMPTY(LIST_TIT.SAL_NUM)
  viewOnly = .T.
ENDIF
IF viewOnly
  DO ViewAddr WITH LIST_TIT.LST_NUM
ELSE
  DO To_Box WITH LIST_TIT.LST_NUM, LIST_TIT.CLI_CODE, LIST_TIT.LST_NAME
ENDIF

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Min_Time     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                   Минимальное время подбора заявки.                    │
*│                                                                        │
*└────────────────────────────────────────────────────────── 13.01.2000 ──┘
PROCEDURE Min_Time

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

*
*   Заполняем значениями переменные состояния...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    Содержательная часть программы:
*
*MIN время подбора заявки
*┌────────────────────────────────┐
*│      Время в минутах 9999      │
*│                                │
*│< OK Ctrl-W > < Отказаться Esc >│
*└────────────────────────────────┘

PRIVATE ex, tmpTime
tmpTime = Q_TIME
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 6, 36, "MIN время подбора заявки"

*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ 2, 8 SAY "Время в минутах" GET tmpTime PICTURE "9999"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID T_Time()

IF ex = 1
  *
  * Отрабатываем бланк
  REPLACE Q_TIME WITH tmpTime
  t_stock = tmpTime
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура T_Time       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                   Проверка корректности MIN времени.                   │
*│                                                                        │
*└────────────────────────────────────────────────────────── 13.01.2000 ──┘
PROCEDURE T_Time

PRIVATE mss

IF ex = 2
  RETURN .T.
ENDIF

IF tmpTime < 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Время не может быть отрицательным!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Work_Time    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                         Рабочее время склада.                          │
*│                                                                        │
*└────────────────────────────────────────────────────────── 13.01.2000 ──┘
PROCEDURE Work_Time

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

*
*   Заполняем значениями переменные состояния...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    Содержательная часть программы:
*
*  Рабочее время склада
*┌──────────────────────────────────┐
*│   С  99 ч  99 м  по  99 ч 99 м   │
*│                                  │
*│ < OK Ctrl-W > < Отказаться Esc > │
*└──────────────────────────────────┘

PRIVATE ex, h0, m0, h1, m1
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 6, 38, "Рабочее время склада"
h0 = VAL(ALLTRIM(LEFT(b_stock,2)))
m0 = VAL(ALLTRIM(SUBSTR(b_stock,4,2)))
h1 = VAL(ALLTRIM(LEFT(e_stock,2)))
m1 = VAL(ALLTRIM(SUBSTR(e_stock,4,2)))
*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ 2,  5 SAY "С  99 ч  99 м  по  99 ч 99 м"

@ 2,  8 GET h0 RANGE 0, 23 PICTURE "99"
@ 2, 14 GET m0 RANGE 0, 59 PICTURE "99"
@ 2, 24 GET h1 RANGE 0, 23 PICTURE "99"
@ 2, 29 GET m1 RANGE 0, 59 PICTURE "99"

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID T_Work()

IF ex = 1
  *
  * Отрабатываем бланк
  b_stock = RIGHT(STR(100+h0),2)+":"+RIGHT(STR(100+m0),2)
  e_stock = RIGHT(STR(100+h1),2)+":"+RIGHT(STR(100+m1),2)
  DO Put_Sto
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура T_Work       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│              Проверка корректности времени работы склада               │
*│                                                                        │
*└────────────────────────────────────────────────────────── 13.01.2000 ──┘
PROCEDURE T_Work

PRIVATE mss

IF ex = 2
  RETURN .T.
ENDIF

IF 60*h0+m0 >= 60*h1+m1
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы некорректно задали интервал!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Ch_Zone      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                               Выбор зоны                               │
*│                                                                        │
*└────────────────────────────────────────────────────────── 13.01.2000 ──┘
PROCEDURE Ch_Zone

PRIVATE z_list, z_char, z_count, s_sav, p

p = 1
s_sav = SELECT()
SELECT 0
USE (base_path+"ZONES") ORDER TAG ZONE
z_count = 3
DIMENSION z_list(z_count), z_char(z_count)
z_char(1) = ""
z_list(1) = PADR("Все зоны", FSIZE("NAME"))
IF cur_zone == ""
  p = 1
ENDIF  
z_list(2) = PADR("Без зоны", FSIZE("NAME"))
z_char(3) = "**"
IF cur_zone == "  "
  p = 2
ENDIF  
z_list(3) = PADR("Прочие (*)", FSIZE("NAME"))
z_char(2) = " "
IF cur_zone = "**"
  p = 3
ENDIF  

SCAN
  z_count = z_count+1
  IF cur_zone == CHAR
    p = z_count
  ENDIF
  DIMENSION z_list(z_count), z_char(z_count)
  z_list(z_count) = NAME
  z_char(z_count) = CHAR
ENDSCAN
USE
SELECT (s_sav)
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

*
*   Заполняем значениями переменные состояния...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    Содержательная часть программы:
*

*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH MIN(z_count+3, WROWS("")-7), LEN(z_list(1))+4, "Зоны склада"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
DO WHILE .T.
  p = MAX(p, 1)
  p = MIN(p, z_count)
  @ 1, 1 MENU z_list, z_count, WROWS()-3
  READ MENU TO p
  IF READKEY() % 256 = 12
    EXIT
  ENDIF
  IF READKEY() % 256 = 15
    cur_zone = z_char(p)
    nam_zone = z_list(p)
    EXIT
  ENDIF
ENDDO
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Seek_L       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                             Поиск заявки.                              │
*│                                                                        │
*└────────────────────────────────────────────────────────── 13.01.2000 ──┘
PROCEDURE Seek_L
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

*
*   Заполняем значениями переменные состояния...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    Содержательная часть программы:
*
* Поиск заявок для списка
*┌──────────────────────────────────┐
*│       Номер списка 999999        │
*│                                  │
*│ < OK Ctrl-W > < Отказаться Esc > │
*└──────────────────────────────────┘

PRIVATE ex, r_sav, tmpList
ex = 1
tmpList = SPACE(FSIZE("LIST_NUM"))
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 6, 38, "Поиск заявок для списка"

*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ 2, 9 SAY "Номер списка" GET tmpList
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "
READ CYCLE

IF ex = 1 .AND. .NOT. EMPTY(tmpList)
  tmpList = PADL(ALLTRIM(tmpList), FSIZE("LIST_NUM"))
  r_sav = RECNO()
  SET ORDER TO TAG DOC_NUM
  IF .NOT. SEEK(tmpList)
    PRIVATE mss01
    DIMENSION mss01(3)
    mss01(1) = ""
    mss01(2) = CHR(0)+"Для списка "+ALLTRIM(tmpList)+" не найдено ни одной заявки"
    mss01(3) = ""
    DO Out_Mess WITH 7, "mss01"
    IF BETWEEN(r_sav, 1, RECCOUNT())
      GO r_sav
    ENDIF
  ELSE
    DO Ch_Ord
  ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Ch_Ord       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Просмотр найденных заявок.                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 13.01.2000 ──┘
PROCEDURE Ch_Ord

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

*
*   Заполняем значениями переменные состояния...
*
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Выбор" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd, mss00
ln = 10   && Количество видимых строк BROWSE - во весь экран!
wd = 75

DO D_Wins WITH ln, wd, "Заявки к списку "+ALLTRIM(tmpList), 0, 4
what_do = "List"
*┌─N заявки────Готовность к────Имя списка──────────────────────С──З─Дл.+─Дл.-┐
*│1234567890 ДД.ММ.ГГГГ ЧЧ:ММ 12345678901234567890123456789012 С ЗЗ 9999 9999│
*└───────────────────────────────────────────────────────────────────────────┘
*┌Время подачи заявки─Примечание─────────────────────Адрес───────────────────┐
*│ДД.ММ.ГГГГ ЧЧ:ММ:СС ....:....!....:....!....:....! ....:....!....:.        │
*└───────────────────────────────────────────────────────────────────────────┘
*┌─N заявки────Готовность к────Имя списка──────────────────────С──З─Длина┐
*│1234567890 ДД.ММ.ГГГГ ЧЧ:ММ 12345678901234567890123456789012 С ЗЗ 99999│
*└───────────────────────────────────────────────────────────────────────┘
@ 1, 2 SAY ;
"─N заявки────Готовность к────Имя списка──────────────────────С──З─Дл.+─Дл.-"
@ WROWS()-6, 2 SAY "Время подачи заявки"
@ WROWS()-6, 22 SAY "Примечание"
@ WROWS()-6, 53 SAY "Адрес"
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE KEY tmpList    ;
           FIELDS F000 = ALLTRIM(LIST_NUM)+"-"+ALLTRIM(STR(ORD_NUM)):10:H="",   ;
                  TIME000 = Time000():16:H="", ;
                  LIST_NAME:32:H="", ;
                  F002 = IIF(.NOT. EMPTY(STO_MARK), "У",    ;
                          IIF(.NOT. EMPTY(CHECK_S), "Н", " ")):1:H="", ;
                  ZONE:H="":2,       ;
                  SIZE_POS:H="":P="@Z 9999", ;
                  SIZE_NEG:H="":P="@Z 9999", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Show_Who() ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Выбор

    IF EMPTY(REQ_DATE)
      DIMENSION mss00(3)
      mss00(1) = ""
      mss00(2) = CHR(0)+'  На заявке пометка "не собирать", как поступим? [ Вернемся ] [ Выйдем ]'
      mss00(3) = ""
      IF Out_Mess(7, "mss00") = 1
        what_do = "List"
      ELSE
        IF BETWEEN(r_sav, 1, RECCOUNT())
          GO r_sav
        ENDIF
        EXIT
      ENDIF
    ELSE
      IF .NOT. ZONE = cur_zone .OR. .NOT. EMPTY(STO_MARK) .AND. flt
        DIMENSION mss00(4)
        mss00(1) = ""
        mss00(2) = CHR(0)+'Заявка вне установленного фильтра, как поступим?'
        mss00(3) = CHR(0)+"  [ Снимем фильтр ] [ Вернемся ] [ Выйдем ]"
        mss00(4) = ""
        wd = Out_Mess(7, "mss00")
        DO CASE
        CASE wd = 1
          flt = .F.
          cur_zone = ""
          nam_zone = "Все зоны"
          EXIT
        CASE wd = 2
          what_do = "List"
        OTHERWISE
          IF BETWEEN(r_sav, 1, RECCOUNT())
            GO r_sav
          ENDIF
          EXIT
        ENDCASE
      ELSE
        EXIT
      ENDIF
    ENDIF

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Stat         Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                               Состояние.                               │
*│                                                                        │
*└────────────────────────────────────────────────────────── 17.01.2000 ──┘
PROCEDURE Stat

DELETE FILE (tmpo_path+"STAT_TMP.DBF")
DELETE FILE (tmpo_path+"STAT_TMP.CDX")
PRIVATE s_sav
s_sav = SELECT()
CREATE DBF (tmpo_path+"STAT_TMP")  ;
  ( Z_CODE  C( 2), ;
    Z_NAME  C(24), ;
    ORDS_H  N( 5), ;
    POS_H   N( 6), ;
    ORDS_W  N( 5), ;
    POS_W   N( 6)  )
INDEX ON Z_CODE TAG CODE
SELECT 0
USE (base_path+"ZONES") ORDER TAG ZONE ALIAS Z_0117 AGAIN
SCAN
  SELECT STAT_TMP
  APPEND BLANK
  REPLACE Z_CODE WITH Z_0117.CHAR, ;
          Z_NAME WITH Z_0117.NAME
  SELECT Z_0117
ENDSCAN
SELECT STAT_TMP
APPEND BLANK
REPLACE Z_CODE WITH "**", ;
        Z_NAME WITH "Прочие"
APPEND BLANK
REPLACE Z_CODE WITH "  ", ;
        Z_NAME WITH "Без зоны"
SELECT Z_0117
USE (base_path+"ORD_T") ORDER TAG ZONE_1 ALIAS OT_0117 AGAIN
SCAN
  SELECT STAT_TMP
  IF .NOT. SEEK(OT_0117.ZONE)
    SEEK "**"
  ENDIF
  REPLACE ORDS_H WITH ORDS_H+1, ;
          POS_H  WITH POS_H+OT_0117.SIZE_POS+OT_0117.SIZE_NEG
  IF .NOT. EMPTY(OT_0117.CHECK_S)
    REPLACE ORDS_W WITH ORDS_W+1, ;
            POS_W  WITH POS_W+OT_0117.SIZE_POS+OT_0117.SIZE_NEG
  ENDIF
  SELECT OT_0117
ENDSCAN

USE
DO Use_Dummy
SELECT STAT_TMP
SET ORDER TO
PRIVATE s0, s1, s2, s3
STORE 0 TO s0, s1, s2, s3
SCAN
  s0 = s0+ORDS_H
  s1 = s1+POS_H
  s2 = s2+ORDS_W
  s3 = s3+POS_W
ENDSCAN

GO TOP

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

*
*   Заполняем значениями переменные состояния...
*
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()
*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Выбрать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD All OF (menu_name) PROMPT "Все зоны" KEY Ctrl-A, "Tab"
ON SELECTION PAD All OF (menu_name) DO Sw_Mode WITH "All"

DEFINE PAD Exit OF (menu_name) PROMPT "Отказаться" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = RECCOUNT()   && Количество видимых строк BROWSE
wd = 50
*                           В очереди    В наборе
*┌─Зона───────────────────Заявок─Строк─Заявок──Строк┐
*│....:....!....:....!.... 99999 999999 99999 999999│
*└──────────────────────────────────────────────────┘
*┌──────────────────────────────────────────────────┐
*│Всего                    99999 999999 99999 999999│
*└──────────────────────────────────────────────────┘
DO D_Wins WITH ln, wd, "Состояние очереди заявок", 1, 1
@ 1, 28 SAY "В очереди    В наборе"
@ 2,  2 SAY "─Зона───────────────────Заявок─Строк─Заявок──Строк"
@ WROWS()-2, 2 SAY "Всего                   "+   ;
                    STR(s0,6)+STR(s1,7)+STR(s2,6)+STR(s3,7)

what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1

*
*    BROWSE - меню
*
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Tab   DO Sw_Mode WITH "All"

    BROWSE FIELDS Z_NAME:H="":24,        ;
                  ORDS_H:H="":P="99999", ;
                  POS_H:H="":P="999999", ;
                  ORDS_W:H="":P="99999", ;
                  POS_W:H="":P="999999", ;
           DUMMY.F:H="" FREEZE DUMMY.F   ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"

    cur_zone = Z_CODE
    nam_zone = Z_NAME
    EXIT

  CASE what_do = "All"

    cur_zone = ""
    nam_zone = PADR("Все зоны", FSIZE("Z_NAME"))
    EXIT

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
USE
DELETE FILE (tmpo_path+"STAT_TMP.DBF")
DELETE FILE (tmpo_path+"STAT_TMP.CDX")
SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Mark_O       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                                Отметка.                                │
*│                                                                        │
*└────────────────────────────────────────────────────────── 31.01.2000 ──┘
PROCEDURE Mark_O
PARAMETER is_br
PRIVATE tmpVal

tmpVal = VAL(ALLTRIM(LIST_NUM))
IF EMPTY(mark_lst)
  mark_ord = ""
ENDIF

IF mark_lst # tmpVal
  ?? CHR(7)
  mark_ord = ""
ENDIF
mark_lst = tmpVal

PRIVATE j, s
s = ALLTRIM(STR(ORD_NUM))
j = AT(" "+s+",", mark_ord)
IF j # 0
  mark_ord = STUFF(mark_ord, j, LEN(s)+2, "")
ELSE
  IF .NOT. EMPTY(STO_MARK)
    WAIT WINDOW "ВНИМАНИЕ! Заявка уже собрана!" NOWAIT
  ELSE
    IF .NOT. EMPTY(CHECK_S)
      WAIT WINDOW "ВНИМАНИЕ! Заявка в наборе!" NOWAIT
    ENDIF
  ENDIF
  mark_ord = mark_ord+" "+s+","
  *
  *  Упорядочим список
  PRIVATE tmpList, tmpLen
  tmpLen = 0
  DO WHILE .NOT. EMPTY(mark_ord)
    tmpLen = tmpLen+1
    DIMENSION tmpList(tmpLen)
    j = AT(",", mark_ord)
    tmpList(tmpLen) = LEFT(mark_ord, j)
    mark_ord = SUBSTR(mark_ord, j+1)
  ENDDO
  = ASORT(tmpList)
  mark_ord = ""
  FOR j = 1 TO tmpLen
    mark_ord = mark_ord+tmpList(j)
  ENDFOR 
ENDIF
IF EMPTY(mark_ord)
  mark_lst = 0
ENDIF

ACTIVATE WINDOW "Ord_Exe1_M" SAME
@ 1, 39 CLEAR TO 1, WCOLS()
IF .NOT. EMPTY(mark_lst)
  @ 1, 39 SAY ALLTRIM(STR(mark_lst))+"/"+mark_ord
ENDIF
IF .NOT. BOF() .AND. .NOT. EOF()
  SKIP
ENDIF
IF EOF()
  GO BOTT
ENDIF

IF is_br
  ACTIVATE WINDOW (ALIAS())
ENDIF

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура GetOrds      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Формирование списка заявок.                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 11/24/2000 ──┘
PROCEDURE GetOrds
PARAMETERS n_lst, n_ord

PRIVATE tmpMark, tmpIdx
tmpMark = ""

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

SELECT 0
DO Use_Dummy
SELECT 0
USE (base_path+"ORD_T") ORDER TAG DOC_NUM ALIAS ORDS_0B24 AGAIN
SEEK n_lst+STR(n_ord,3)

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Выбрать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Mark OF (menu_name) PROMPT "Отметить" KEY Ctrl-A, "Ins"
ON SELECTION PAD Mark OF (menu_name) DO Sw_Mode WITH "Mark"

DEFINE PAD Exit OF (menu_name) PROMPT "Отказаться" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 49
*┌───N──З──Примечание──────────────────────────────┐
*│M NNN ZZ nnnnNnnnnNnnnnNnnnnNnnnnNnnnnNnnnnNnnnnN│
*└─────────────────────────────────────────────────┘
*┌─Состояние───────────────────────────────────────┐
*│                                                 │
*└─────────────────────────────────────────────────┘
DO D_Wins WITH ln, wd, ALLTRIM(n_lst)+" "+ALLTRIM(LIST_NAME), 0, 1
@ 1, 2 SAY "───N──З──Примечание"
@ WROWS()-3, 3 SAY "Состояние"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins   DO Sw_Mode WITH "Mark"

*
*    BROWSE - меню
*
    BROWSE KEY n_lst  ;
           FIELDS TMPMARK = IIF(EMPTY(CHECK_S).OR.STO_MARK#0, "░",         ;
                            IIF(STR(ORD_NUM,3)$tmpMark, "√", " ")):1:H="", ;
                  ORD_NUM:H="",  ;
                  ZONE:H="",     ;
                  NOTE:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Выбрать

    IF EMPTY(tmpMark)
      IF .NOT. EMPTY(CHECK_S) .AND. STO_MARK = 0
        tmpMark = STR(ORD_NUM,3)
      ENDIF
    ENDIF
    EXIT

  CASE what_do = "Mark"      && Отметить

    IF .NOT. EMPTY(CHECK_S) .AND. STO_MARK = 0
      tmpIdx = AT(STR(ORD_NUM,3), tmpMark)
      IF tmpIdx = 0
        tmpMark = tmpMark+STR(ORD_NUM,3)
      ELSE
        tmpMark = STRTRAN(tmpMark, STR(ORD_NUM,3), "")
      ENDIF
    ENDIF
    SKIP
    what_do = "List"

  OTHERWISE

    tmpMark = ""
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
SELECT ORDS_0B24
USE
SELECT (s_sav)

IF LEN(tmpMark) > 3
  PRIVATE j, j, n, v1, v2
  n = LEN(tmpMark)/3
  FOR i = 1 TO n-1
    FOR j = i+1 TO n
      v1 = SUBSTR(tmpMark, (i-1)*3+1, 3)
      v2 = SUBSTR(tmpMark, (j-1)*3+1, 3)
      IF v2 < v1
        tmpMark = STUFF(tmpMark, (i-1)*3+1, 3, v2)
        tmpMark = STUFF(tmpMark, (j-1)*3+1, 3, v1)
      ENDIF
    ENDFOR
  ENDFOR
ENDIF

RETURN tmpMark

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Draw_Foot    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                   Рисуем дополнительную информацию.                    │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.10.1998 ──┘
PROCEDURE Draw_Foot

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   Вот здесь, рисуем...
*
DO CASE
CASE STO_MARK # 0
  @ WROWS()-2, 3 SAY "Утверждена"
CASE .NOT. EMPTY(CHECK_S)
  @ WROWS()-2, 3 SAY "В наборе  "
OTHERWISE
  @ WROWS()-2, 3 SAY "          "
ENDCASE

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Sto_Time     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                 Установка времени для типов очередей.                  │
*│                                                                        │
*└────────────────────────────────────────────────────────── 30.07.2001 ──┘
PROCEDURE Sto_Time

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

SELECT 0
DO Use_Dummy
SELECT 0
USE base_path+"QUEUE" SHARED AGAIN ALIAS ST_TIME

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Modify OF (menu_name) PROMPT "Корректировать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Modify OF (menu_name) DO Sw_Mode WITH "Modify"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 28

DO D_Wins WITH ln, wd, "MIN время гот. заявки", 0, 0
@ 1, 2 SAY ;
"─Тип очереди───────────Время"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE FIELDS Q_NAME:H="", Q_TIME:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Modify")
    ENDIF

  CASE what_do = "Modify"    && Просмотр списка

    IF .NOT. EOF() .AND. .NOT. BOF()
      DO Min_Time
    ENDIF
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
USE
SELECT (s_sav)

RETURN


*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура S_Author     Разработчик Галина Дмитриенко                 │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Установка автора списка.                         │
*│                                                                        │
*└────────────────────────────────────────────────────────── 13.05.2002 ──┘
PROCEDURE S_Author
PARAMETER f_s001
PRIVATE ord_sav, f_sal, mss

ord_sav = SELECT()

SELECT 0
USE(base_path+"PERSONS")ORDER TAG CODE ALIAS PS_2513 AGAIN

IF SEEK(f_s001, "PS_2513")
  f_sal = ALLTRIM(PS_2513.FAMILY)+" "+LEFT(PS_2513.NAME,1)+". "+ ;
          LEFT(PS_2513.S_NAME,1)+"."

ELSE
  f_sal = ""
ENDIF
USE

DIMENSION mss(3)
mss(1) = ""
IF .NOT. EMPTY(f_sal)
  mss(2) = CHR(0)+"Автор списка "+ f_sal
ELSE
  mss(2) = CHR(0)+"Автора списка установить не удалось!"
ENDIF
mss(3) = ""
DO Out_Mess WITH 5, "mss"

SELECT(ord_sav)

RETURN