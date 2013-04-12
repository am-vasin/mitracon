*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Load     Разработчик Андрей Васин                          ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                            Описание грузов.                            ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 20.01.2000 ══╝
PROCEDURE Load

PRIVATE path_as, mss

path_as = ALLTRIM(Get_Sys("PATH_AS"))
IF EMPTY(path_as)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Не определена база данных грузов."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF
mss = .F.
IF TYPE("path_comm") = "C"
  mss = .NOT. EMPTY(path_comm)
ENDIF
IF .NOT. mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Не определена общая база данных."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

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
USE (path_as+"LOAD") ORDER TAG LD_NUM ALIAS LT_0120 AGAIN
SELECT 0
USE (path_comm+"INVOICET") ORDER TAG LD_BOX ALIAS IT_0120 AGAIN
SELECT 0
USE (path_as+"BOXNO") ORDER TAG BOXNO ALIAS BX_0120 AGAIN
SELECT 0
USE (path_as+"LOAD_D") ORDER TAG LOAD_NUM ALIAS LD_0120 AGAIN
SET RELATION TO STR(YEAR(LD_DATE),4)+STR(LD_NUM,FSIZE("LD_NUM")) ;
    INTO LT_0120
SET RELATION TO ;
    STR(YEAR(LD_DATE),4)+STR(LD_NUM)+OFFICE+STR(BOX_YEAR,4)+STR(BOXNO) ;
    INTO IT_0120 ADDITIVE
SET RELATION TO ;
    OFFICE+STR(BOX_YEAR,4)+STR(BOXNO) ;
    INTO BX_0120 ADDITIVE
*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Order OF (menu_name) PROMPT "Вид"
ON PAD Order OF (menu_name) ACTIVATE POPUP Order

  DEFINE POPUP Order MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Order PROMPT "√Упорядичить по грузам" KEY Ctrl-A, "Ctrl-F1"
  ON SELECTION BAR 1 OF Order DO Sw_Mode WITH "OrdLoad"

  DEFINE BAR  2 OF Order PROMPT " Упорядичить по коробкам" KEY Ctrl-A, "F1"
  ON SELECTION BAR 2 OF Order DO Sw_Mode WITH "OrdBox"

  DEFINE BAR  3 OF Order PROMPT "\-"

  DEFINE BAR  4 OF Order PROMPT "√Все коробки" KEY Ctrl-A, "F6"
  ON SELECTION BAR 4 OF Order DO Sw_Mode WITH "AllBox"

  DEFINE BAR  5 OF Order PROMPT " Только коробки с накладными" KEY Ctrl-A, "Shift-F6"
  ON SELECTION BAR 5 OF Order DO Sw_Mode WITH "InvBox"

  DEFINE BAR  6 OF Order PROMPT " Только принятые" KEY Ctrl-A, "Ctrl-F6"
  ON SELECTION BAR 6 OF Order DO Sw_Mode WITH "GotBox"

  DEFINE BAR  7 OF Order PROMPT " Только не принятые" KEY Ctrl-A, "Alt-F6"
  ON SELECTION BAR 7 OF Order DO Sw_Mode WITH "NotGotBox"

DEFINE PAD View OF (menu_name) PROMPT "Просмотр"
ON PAD View OF (menu_name) ACTIVATE POPUP View

  DEFINE POPUP View MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF View PROMPT "Содержимого коробки" KEY Ctrl-A, "F2"
  ON SELECTION BAR 1 OF View DO Sw_Mode WITH "ViewBox"

  DEFINE BAR  2 OF View PROMPT "Приемных актов" KEY Ctrl-A, "F3"
  ON SELECTION BAR 2 OF View DO Sw_Mode WITH "ViewActs"

DEFINE PAD Seek OF (menu_name) PROMPT "Поиск"
ON PAD Seek OF (menu_name) ACTIVATE POPUP Seek

  DEFINE POPUP Seek MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Seek PROMPT "По маркировке" KEY Ctrl-A, "F7"
  ON SELECTION BAR 1 OF Seek DO Sw_Mode WITH "SeekMask"

  DEFINE BAR  2 OF Seek PROMPT "По позиции" KEY Ctrl-A, "Ctrl-F7"
  ON SELECTION BAR 2 OF Seek DO Sw_Mode WITH "SeekPos"

DEFINE PAD Weight OF (menu_name) PROMPT "Вес" KEY Ctrl-A, "Tab"
ON SELECTION PAD Weight OF (menu_name) DO Sw_Mode WITH "Weight"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-5   && Количество видимых строк BROWSE
wd = 75
*
*┌──N груза──Дата груза──Офис────N к.─Год──С──Ожидается─Накладная────Приемка─┐
*│XXXXXXXXXX ДД.ММ.ГГГГ XXXXXXXX XXXX XXXX C ДД.ММ.ГГГГ XXXXXXXXXX ДД.ММ.ГГГГ│
*└─────────Маркировка коробки XXXXXXXXXX─────────────────────────────────────┘
DO D_Wins WITH ln, wd, "Перечень коробок", 0, 1
@ 1, 2 SAY ;
"──N груза──Дата груза──Офис────N к.─Год──С──Ожидается─Накладная────Приемка"
what_do = "List"

GO BOTTOM
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Ctrl+F1  DO Sw_Mode WITH "OrdLoad"
    ON KEY LABEL F1       DO Sw_Mode WITH "OrdBox"
    ON KEY LABEL F6       DO Sw_Mode WITH "AllBox"
    ON KEY LABEL Shift+F6 DO Sw_Mode WITH "InvBox"
    ON KEY LABEL Ctrl+F6  DO Sw_Mode WITH "GotBox"
    ON KEY LABEL Alt+F6   DO Sw_Mode WITH "NotGotBox"
    ON KEY LABEL F2       DO Sw_Mode WITH "ViewBox"
    ON KEY LABEL F3       DO Sw_Mode WITH "ViewActs"
    ON KEY LABEL F7       DO Sw_Mode WITH "SeekMask"
    ON KEY LABEL Ctrl+F7  DO Sw_Mode WITH "SeekPos"
    ON KEY LABEL Tab      DO Sw_Mode WITH "Weight"

*
*    BROWSE - меню
*
    BROWSE FIELDS LD_0120.LD_NUM:H="",  ;
                  LD_0120.LD_DATE:H="", ;
                  LD_0120.OFFICE:H="",  ;
                  LD_0120.BOXNO:H="",   ;
                  LD_0120.BOX_YEAR:H="",;
                  URGENT = IIF(EMPTY(LT_0120.QUICKLY), " ", "√"):H="", ;
                  LT_0120.LEADTIME:H="",;
                  IT_0120.EXP_NUM:H="", ;
                  IT_0120.DATE_INC:H="",;
           DUMMY.F:H="" FREEZE DUMMY.F  ;
           WHEN Draw_Foot()    ;
           NOMODIFY ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")

    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Exit")
    ENDIF

  CASE what_do = "OrdLoad"    && Упорядочить по грузам

    SET ORDER TO LOAD_NUM
    DEFINE BAR  1 OF Order PROMPT "√Упорядичить по грузам" KEY Ctrl-A, "Ctrl-F1"
    DEFINE BAR  2 OF Order PROMPT " Упорядичить по коробкам" KEY Ctrl-A, "F1"
    what_do = "List"

  CASE what_do = "OrdBox"     && Упорядочить по коробкам

    SET ORDER TO BOXNO
    DEFINE BAR  1 OF Order PROMPT " Упорядичить по грузам" KEY Ctrl-A, "Ctrl-F1"
    DEFINE BAR  2 OF Order PROMPT "√Упорядичить по коробкам" KEY Ctrl-A, "F1"
    what_do = "List"

  CASE what_do = "AllBox"     && Все коробки

    DEFINE BAR  4 OF Order PROMPT "√Все коробки" KEY Ctrl-A, "F6"
    DEFINE BAR  5 OF Order PROMPT " Только коробки с накладными" KEY Ctrl-A, "Shift-F6"
    DEFINE BAR  6 OF Order PROMPT " Только принятые" KEY Ctrl-A, "Ctrl-F6"
    DEFINE BAR  7 OF Order PROMPT " Только не принятые" KEY Ctrl-A, "Alt-F6"
    SET FILTER TO
    what_do = "List"

  CASE what_do = "InvBox"     && Только коробки с накладными

    DEFINE BAR  4 OF Order PROMPT " Все коробки" KEY Ctrl-A, "F6"
    DEFINE BAR  5 OF Order PROMPT "√Только коробки с накладными" KEY Ctrl-A, "Shift-F6"
    DEFINE BAR  6 OF Order PROMPT " Только принятые" KEY Ctrl-A, "Ctrl-F6"
    DEFINE BAR  7 OF Order PROMPT " Только не принятые" KEY Ctrl-A, "Alt-F6"
    SET FILTER TO FOUND("IT_0120")
    what_do = "List"

  CASE what_do = "GotBox"     && Только принятые

    DEFINE BAR  4 OF Order PROMPT " Все коробки" KEY Ctrl-A, "F6"
    DEFINE BAR  5 OF Order PROMPT " Только коробки с накладными" KEY Ctrl-A, "Shift-F6"
    DEFINE BAR  6 OF Order PROMPT "√Только принятые" KEY Ctrl-A, "Ctrl-F6"
    DEFINE BAR  7 OF Order PROMPT " Только не принятые" KEY Ctrl-A, "Alt-F6"
    SET FILTER TO FOUND("IT_0120") .AND. .NOT. EMPTY(IT_0120.DATE_INC)
    what_do = "List"

  CASE what_do = "NotGotBox"     && Только не принятые

    DEFINE BAR  4 OF Order PROMPT " Все коробки" KEY Ctrl-A, "F6"
    DEFINE BAR  5 OF Order PROMPT " Только коробки с накладными" KEY Ctrl-A, "Shift-F6"
    DEFINE BAR  6 OF Order PROMPT " Только принятые" KEY Ctrl-A, "Ctrl-F6"
    DEFINE BAR  7 OF Order PROMPT "√Только не принятые" KEY Ctrl-A, "Alt-F6"
    SET FILTER TO FOUND("IT_0120") .AND. EMPTY(IT_0120.DATE_INC)
    what_do = "List"

  CASE what_do = "ViewBox"     && Содержимое коробки

    DO ViewBox
    what_do = "List"

  CASE what_do = "ViewActs"    && Приемные акты

    DO ViewAct
    what_do = "List"

  CASE what_do = "SeekMask"    && Поиск по маркеровке

    DO SeekMask
    what_do = "List"

  CASE what_do = "SeekPos"     && Поиск по позиции

    DO SeekPos
    what_do = "List"

  CASE what_do = "Weight"       && Вес

    IF FOUND("BX_0120")
      DO Get_Wght
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
SELECT LT_0120
USE
SELECT BX_0120
USE
SELECT IT_0120
USE
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
PRIVATE s, p
ACTIVATE WINDOW (win_name+"_M") SAME

*
*   Вот здесь, рисуем...
*
s = " Маркировка коробки: "+IIF(EMPTY(LD_0120.MASK), ;
        ALLTRIM(LD_0120.OFFICE)+ALLTRIM(STR(LD_0120.BOXNO)),ALLTRIM(LD_0120.MASK))
@ WROWS()-2, 3 SAY PADR(s, 35)
@ WROWS()-2, COL() SAY "Вес: предв. "+TRANSFORM(BX_0120.WEIGHT,"@Z 999.9")+  ;
                       "   точно "+TRANSFORM(BX_0120.WEIGHT_ST,"@Z 999.9")

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура ViewBox      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                          Содержимое коробки.                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 21.01.2000 ──┘
PROCEDURE ViewBox

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
DO Prep_Pos
IF BOF() .OR. EOF()
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Коробка пуста..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  DELETE FILE (tmpo_path+"TMP_POS.DBF")
  DELETE FILE (tmpo_path+"TMP_POS.CDX")
  SELECT (s_sav)
  RETURN
ENDIF
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
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 48

*┌─ Позиция ────────────────────────────── Кол-во─┐
*│XXXX XXXXXXXXXXXXXXXXXXXXXXXXX XXXXXXXX 99999999│
*└────────────────────────────────────────────────┘

DO D_Wins WITH ln, wd, "Содержимое коробки "+  ;
       LD_0120.OFFICE+" "+ALLTRIM(STR(LD_0120.BOXNO)), 0, 0
@ 1, 2 SAY "─ Позиция ────────────────────────────── Кол-во─"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1

*
*    BROWSE - меню
*
    BROWSE FIELDS PREFIX:H="", NAME:H="", PRODUCER:H="", QNT:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
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
USE
DELETE FILE (tmpo_path+"TMP_POS.DBF")
DELETE FILE (tmpo_path+"TMP_POS.CDX")

SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Prep_Pos     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│            Подготовка временного файла содержимого коробки.            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 21.01.2000 ──┘
PROCEDURE Prep_Pos

SELECT 0
DELETE FILE (tmpo_path+"TMP_POS.DBF")
DELETE FILE (tmpo_path+"TMP_POS.CDX")
CREATE DBF (tmpo_path+"TMP_POS") ;
   (  ID     C(11),   ;
      PREFIX C( 4),   ;
      NAME   C(35),   ;
      PRODUCER C( 8), ;
      QNT    N( 8)    )
INDEX ON ID TAG ID

SELECT 0
USE (path_comm+"PURBASE") ALIAS ST_0121 ORDER TAG IDP AGAIN

SELECT 0
USE (path_comm+"ONHAND") ALIAS CNT_0121 ORDER TAG BOXNO AGAIN
SET RELATION TO PURSYS+STR(IDP) INTO ST_0121

SEEK LD_0120.OFFICE+STR(LD_0120.BOX_YEAR,4)+STR(LD_0120.BOXNO)
SCAN REST WHILE LD_0120.OFFICE = BOXOFFIC .AND. ;
                LD_0120.BOX_YEAR = BOXYEAR .AND. ;
                LD_0120.BOXNO = BOXNO
  SELECT TMP_POS
  IF .NOT. SEEK(CNT_0121.PURSYS+STR(CNT_0121.IDP))
    APPEND BLANK
    REPLACE ID       WITH CNT_0121.PURSYS+STR(CNT_0121.IDP), ;
            PREFIX   WITH ST_0121.EQVPREF, ;
            NAME     WITH ST_0121.EQVNAME, ;
            PRODUCER WITH ST_0121.EQVPROD
  ENDIF
  REPLACE QNT WITH QNT+CNT_0121.QTY
  SELECT CNT_0121
ENDSCAN
USE
SELECT ST_0121
USE
SELECT TMP_POS
INDEX ON NAME+PREFIX+PRODUCER TAG NAME FOR QNT # 0
GO TOP

RETURN


*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура ViewAct      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                     Просмотр приемных актов.                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 21.01.2000 ──┘
PROCEDURE ViewAct

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
IF EMPTY(IT_0120.EXP_NUM) .OR. EMPTY(IT_0120.EXP_DATE)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"К коробке нет накладной..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

s_sav = SELECT()

SELECT 0
DO Use_Dummy
DO Prp_Act
IF BOF() .OR. EOF()
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет ни одного приемного акта..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  DELETE FILE (tmpo_path+"TMP_ACT.DBF")
  DELETE FILE (tmpo_path+"TMP_ACT.CDX")
  SELECT (s_sav)
  RETURN
ENDIF
*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Print OF (menu_name) PROMPT "Печать приемного акта" KEY Ctrl-A, "Enter"
ON SELECTION PAD Print OF (menu_name) DO Sw_Mode WITH "Print"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"
*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 25

*┌Фрм──N док-та─────Дата───┐
*│XXX NNNNNNNNNN ДД.ММ.ГГГГ│
*└─────────────────────────┘
GO TOP
DO D_Wins WITH ln, wd, "Приемные акты", 0, 0
@ 1, 2 SAY "Фрм──N док-та─────Дата"
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
    BROWSE FIELDS FIRM:H="", D_NUM:H="", D_DATE:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Print")
    ENDIF

  CASE what_do = "Print"    && Печать приемного акта

    DO P_Act WITH FIRM, D_NUM, D_DATE
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
DELETE FILE (tmpo_path+"TMP_ACT.DBF")
DELETE FILE (tmpo_path+"TMP_ACT.CDX")

SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Prp_Act      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│            Подготовка временного файла с приемными актами.             │
*│                                                                        │
*└────────────────────────────────────────────────────────── 21.01.2000 ──┘
PROCEDURE Prp_Act
PRIVATE p, tmpStr, tmpKey
SELECT 0
DELETE FILE (tmpo_path+"TMP_ACT.DBF")
DELETE FILE (tmpo_path+"TMP_ACT.CDX")
CREATE DBF (tmpo_path+"TMP_ACT") ;
   (  FIRM   C( 3),   ;
      D_NUM  C(10),   ;
      D_DATE D   )

INDEX ON FIRM+STR(YEAR(D_DATE),4)+D_NUM TAG D_NUM

SELECT 0
USE (base_path+"ACT_S_T") ALIAS A_0121 ORDER TAG DOC_NUM AGAIN

SELECT 0
USE (path_comm+"INVOICED") ALIAS ID_0121 ORDER TAG EXP_NUM AGAIN

SEEK STR(YEAR(IT_0120.EXP_DATE),4)+IT_0120.EXP_NUM
SCAN REST WHILE IT_0120.EXP_DATE = EXP_DATE .AND. EXP_NUM = IT_0120.EXP_NUM ;
       FOR .NOT. EMPTY(AKT_NUM)
  tmpStr = ALLTRIM(AKT_NUM)
  DO WHILE .NOT. EMPTY(tmpStr)
    p = AT(",", tmpStr)
    IF p = 0
      EXIT
    ENDIF
    tmpKey = LEFT(tmpStr,p-1)
    tmpStr = SUBSTR(tmpStr,p+1)
    tmpKey = LEFT(tmpKey,7)+PADL(SUBSTR(tmpKey,8), 10)
    IF SEEK(tmpKey, "TMP_ACT")
      LOOP
    ENDIF
    IF .NOT. SEEK(tmpKey, "A_0121")
      LOOP
    ENDIF
    SELECT TMP_ACT
    APPEND BLANK
    REPLACE FIRM   WITH A_0121.FIRM,    ;
            D_NUM  WITH A_0121.DOC_NUM, ;
            D_DATE WITH A_0121.DOC_DATE
  ENDDO
  SELECT ID_0121
ENDSCAN
SELECT TMP_ACT
SET ORDER TO
IF RECCOUNT() # 0
  SCAN WHILE .NOT. "!" $ D_NUM
    p = RECNO()
    = SEEK(FIRM+STR(YEAR(D_DATE),4)+D_NUM, "A_0121")
    SELECT A_0121
    tmpKey = FIRM+DTOS(DOC_DATE)+DOC_NUM
    SCAN REST FOR "!" $ DOC_NUM .AND. tmpKey = FIRM+DTOS(EXP_DATE)+EXP_NUM
      SELECT TMP_ACT
      APPEND BLANK
      REPLACE FIRM   WITH A_0121.FIRM,    ;
              D_NUM  WITH A_0121.DOC_NUM, ;
              D_DATE WITH A_0121.DOC_DATE
      SELECT A_0121
    ENDSCAN
    SELECT TMP_ACT
    GO p
  ENDSCAN
ENDIF
SELECT A_0121
USE
SELECT ID_0121
USE
SELECT TMP_ACT
INDEX ON DTOS(D_DATE) TAG HRON
GO TOP

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура SeekMask     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Поиск коробок по маркировке.                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 22.01.2000 ──┘
PROCEDURE SeekMask

PRIVATE shab, o_sav, r_sav, s_sav, n_rec
shab = GetMask()
IF EMPTY(shab)
  RETURN
ENDIF

s_sav = SELECT()
SELECT LD_0120
o_sav = TAG()
r_sav = RECNO()

DO Prp_Mask

n_rec = ViewMask()
SELECT LD_0120
SET ORDER TO (o_sav)
IF BETWEEN(n_rec, 1, RECCOUNT())
  GO n_rec
ELSE
  IF BETWEEN(r_sav, 1, RECCOUNT())
    GO r_sav
  ENDIF
ENDIF
SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура GetMask      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                   Ввод названия коробки для поиска.                    │
*│                                                                        │
*└────────────────────────────────────────────────────────── 22.01.2000 ──┘
PROCEDURE GetMask
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
*Поиск коробки по маркировке
*┌────────────────────────────────────────────┐
*│ Задайте шаблон для поиска 9999999999999999 │
*│                                            │
*│      < OK Ctrl-W > < Отказаться Esc >      │
*└────────────────────────────────────────────┘

PRIVATE ex, sh
ex = 1
sh = SPACE(16)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 6, 48, "Поиск коробки по маркировке"

*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ 2, 3 SAY "Задайте шаблон для поиска" GET sh
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN IIF(ex = 1, ALLTRIM(sh), "")

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Prp_Mask     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                             Поиск коробок.                             │
*│                                                                        │
*└────────────────────────────────────────────────────────── 22.01.2000 ──┘
PROCEDURE Prp_Mask

PRIVATE s

SELECT 0
DELETE FILE (tmpo_path+"LNK_0122.DBF")
CREATE DBF (tmpo_path+"LNK_0122") ( N_REF N(10))

shab = SYS(15, lwr, shab)

SELECT LD_0120
SCAN
  s = IIF(EMPTY(LD_0120.MASK),  ;
        ALLTRIM(LD_0120.OFFICE)+ALLTRIM(STR(LD_0120.BOXNO)),ALLTRIM(LD_0120.MASK))
  s = SYS(15, lwr, s)
  IF shab $ s
    SELECT LNK_0122
    APPEND BLANK
    REPLACE N_REF WITH RECNO("LD_0120")
    SELECT LD_0120
  ENDIF
ENDSCAN

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура ViewMask     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Просмотр и выбор коробок.                        │
*│                                                                        │
*└────────────────────────────────────────────────────────── 22.01.2000 ──┘
PROCEDURE ViewMask

IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Не найдено ни одной коробки..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  DELETE FILE (tmpo_path+"LNK_0122.DBF")
  RETURN
ENDIF

PRIVATE tmpRet
tmpRet = 0
  
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

SELECT LD_0120
SET ORDER TO
SELECT LNK_0122
SET RELATION TO N_REF INTO LD_0120
*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Выбор" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Cont OF (menu_name) PROMPT "Содержимое коробки" KEY Ctrl-A, "F2"
ON SELECTION PAD Cont OF (menu_name) DO Sw_Mode WITH "ViewBox"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-5   && Количество видимых строк BROWSE
wd = 75
*
*┌──N груза──Дата груза──Офис────N к.─Год──С──Ожидается─Накладная────Приемка─┐
*│XXXXXXXXXX ДД.ММ.ГГГГ XXXXXXXX XXXX XXXX C ДД.ММ.ГГГГ XXXXXXXXXX ДД.ММ.ГГГГ│
*└─────────Маркировка коробки XXXXXXXXXX─────────────────────────────────────┘
DO D_Wins WITH ln, wd, "Перечень коробок", 0, 0
@ 1, 2 SAY ;
"──N груза──Дата груза──Офис────N к.─Год──С──Ожидается─Накладная────Приемка"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F2       DO Sw_Mode WITH "ViewBox"

*
*    BROWSE - меню
*
    BROWSE FIELDS LD_0120.LD_NUM:H="",  ;
                  LD_0120.LD_DATE:H="", ;
                  LD_0120.OFFICE:H="",  ;
                  LD_0120.BOXNO:H="",   ;
                  LD_0120.BOX_YEAR:H="",;
                  LD_0120.BOXNO:H="",   ;
                  URGENT = IIF(EMPTY(LT_0120.QUICKLY), " ", "√"):H="", ;
                  LT_0120.LEADTIME:H="",;
                  IT_0120.EXP_NUM:H="", ;
                  IT_0120.DATE_INC:H="",;
           DUMMY.F:H="" FREEZE DUMMY.F  ;
           WHEN Draw_Foot()    ;
           NOMODIFY ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")

    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Выбор

    tmpRet = LNK_0122.N_REF
    EXIT

  CASE what_do = "ViewBox"     && Содержимое коробки

    SELECT LD_0120
    DO ViewBox
    SELECT LNK_0122
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
DELETE FILE (tmpo_path+"LNK_0122.DBF")

RETURN tmpRet

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура SeekPos      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Поиск по позиции.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 22.01.2000 ──┘
PROCEDURE SeekPos
PRIVATE shab, n_rec, s_sav, r_sav, o_sav

r_sav = RECNO()
o_sav = ORDER()
s_sav = SELECT()
SET ORDER TO TAG BOXNO
shab = GetPos()
IF EMPTY(shab)
  RETURN
ENDIF

DO Prp_Pos

n_rec = ViewPos()
SELECT LD_0120
IF BETWEEN(n_rec, 1, RECCOUNT())
  GO n_rec
ENDIF
SELECT (s_sav)
SET ORDER TO (o_sav)
IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура GetPos      Разработчик Андрей Васин                       │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                    Ввод текста для поиска позиции.                     │
*│                                                                        │
*└────────────────────────────────────────────────────────── 22.01.2000 ──┘
PROCEDURE GetPos
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

*Поиск позиции
*┌─────────────────────────────────────────────────────┐
*│ Задайте шаблон для поиска 9999999999999999999999999 │
*│                                                     │
*│          < OK Ctrl-W > < Отказаться Esc >           │
*└─────────────────────────────────────────────────────┘

PRIVATE ex, sh
ex = 1
sh = SPACE(25)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 6, 57, "Поиск позиции"

*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ 2, 3 SAY "Задайте шаблон для поиска" GET sh
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN IIF(ex = 1, ALLTRIM(sh), "")

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Prp_pos      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Подготовка списка позиций.                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 22.01.2000 ──┘
PROCEDURE Prp_pos

DO Wt_Mess WITH "Поиск..."
DELETE FILE (tmpo_path+"TMP_LIST.DBF")
DELETE FILE (tmpo_path+"TMP_LIST.CDX")
CREATE DBF (tmpo_path+"TMP_LIST") ;
  (  PREFIX   C( 4), ;
     NAME     C(35), ;
     PRODUCER C( 8), ;
     BOXNO    N( 7), ;
     OFFICE   C( 8), ;
     BOXYEAR  N( 4), ;
     REC_NO   N(10)  )
INDEX ON NAME+PREFIX+PRODUCER+STR(BOXNO)+OFFICE+STR(BOXYEAR) TAG TMP
cnt = 0
SELECT 0
USE (path_comm+"PURBASE") ALIAS ST_0122 AGAIN ORDER TAG IDP
SELECT 0
USE (path_comm+"ONHAND") ALIAS ONH_0122 AGAIN
SET RELATION TO PURSYS+STR(IDP) INTO ST_0122

SCAN
  cnt = cnt+1
*  @ WROW()-1, 0 SAY STR(cnt)
  IF .NOT. shab $ ST_0122.EQVPREF+ST_0122.EQVNAME+ST_0122.EQVPROD
    LOOP
  ENDIF
  IF SEEK(ST_0122.EQVNAME+ST_0122.EQVPREF+ST_0122.EQVPROD+ ;
          +STR(BOXNO)+BOXOFFIC+STR(BOXYEAR), "TMP_LIST")
    LOOP
  ENDIF
  IF .NOT. SEEK(ONH_0122.BOXOFFIC+STR(ONH_0122.BOXYEAR,4)+  ;
                STR(ONH_0122.BOXNO), "LD_0120")
    LOOP
  ENDIF
  SELECT TMP_LIST
  APPEND BLANK
  REPLACE PREFIX   WITH ST_0122.EQVPREF,  ;
          NAME     WITH ST_0122.EQVNAME,  ;
          PRODUCER WITH ST_0122.EQVPROD,  ;
          BOXNO    WITH ONH_0122.BOXNO,   ;
          OFFICE   WITH ONH_0122.BOXOFFIC,;
          BOXYEAR  WITH ONH_0122.BOXYEAR, ;
          REC_NO   WITH RECNO("LD_0120")
  SELECT ONH_0122
ENDSCAN
DO Wt_Mess
USE
SELECT ST_0122
USE
SELECT TMP_LIST

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура ViewPos      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Просмотр отобранных позиций.                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 24.01.2000 ──┘
PROCEDURE ViewPos

PRIVATE retVal, k00
retVal = 0
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Ничего не удалось найти..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  DELETE FILE (tmpo_path+"TMP_LIST.DBF")
  DELETE FILE (tmpo_path+"TMP_LIST.CDX")
  RETURN retVal
ENDIF

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
SELECT TMP_LIST
*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Выбрать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "Отказаться" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = MIN(WROWS("")-8,RECCOUNT())   && Количество видимых строк BROWSE
wd = 61
*┌─Позиция─────────────────────────────────N кор.─Офис─────Год─┐
*│XXXX xxxxxxxxxXxxxxxxxxxXxxxxx xxxxxxxx NNNNNNN XXXXXXXX ГГГГ│
*└─────────────────────────────────────────────────────────────┘

DO D_Wins WITH ln, wd, "Список отобранных позиций", 0, 0
what_do = "List"
GO TOP
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE FIELDS PREFIX:H="",   ;
                  NAME:H="",     ;
                  PRODUCER:H="", ;
                  BOXNO:H="",    ;
                  OFFICE:H="",   ;
                  BOXYEAR:H="",  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Выбор

    retVal = REC_NO
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
DELETE FILE (tmpo_path+"TMP_LIST.DBF")
DELETE FILE (tmpo_path+"TMP_LIST.CDX")

SELECT (s_sav)

RETURN retVal


*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Get_Wght     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                         Установка веса груза.                          │
*│                                                                        │
*└────────────────────────────────────────────────────────── 21.04.2000 ──┘
PROCEDURE Get_Wght

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

*┌────────────────────────────────┐
*│ ┌─ Вес ──────────────────────┐ │
*│ │ Предв. 99999  Точный 999.9 │ │
*│ └────────────────────────────┘ │
*│< OK Ctrl-W > < Отказаться Esc >│
*└────────────────────────────────┘

PRIVATE ex, tmpWeight
ex = 1
tmpWeight = BX_0120.WEIGHT_ST
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 7, 36, ALLTRIM(LD_0120.OFFICE)+ALLTRIM(STR(LD_0120.BOXNO))
DO Sun_Bord WITH  2,  3,  4, 32, " Вес "

*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ 3,  5 SAY "Предв." GET BX_0120.WEIGHT PICTURE "@Z 999.9" WHEN .F.
@ 3, 19 SAY "Точный" GET tmpWeight PICTURE "@Z 999.9"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex = 1
  *
  * Отрабатываем бланк
  REPLACE BX_0120.WEIGHT_ST WITH tmpWeight
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
