*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла GTD_Link     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                   Заполнение ГТД для списка позиций.                   ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 26.02.2000 ══╝
PROCEDURE GTD_Link
PARAMETERS view

PRIVATE s_sav, cnt_hole, cnt_cur, tmpName, p, nameWd, cnt_found, r_sav, flt_val, swFull
PRIVATE period
PRIVATE p_num, p_country, p_cntC, p_line, p_badG, p_GTDName
period = 120       &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
swFull = .F.
p_GTDName = ""
flt_val = SPACE(25)
s_sav = SELECT()

SELECT GTD_LINK
cnt_cur = 0
cnt_found = 0
cnt_hole = RECCOUNT()
DO Wt_Mess WITH "   Подбор ГТД...   "

SCAN
  p_num     = ""
  p_country = ""
  p_line    = ""
  p_badG    = ""
  p_cntC    = ""
  DO Set_Gtd WITH GTD_LINK.CODE, GTD_LINK.PREFIX, GTD_LINK.NAME, GTD_LINK.PRODUCER, ;
             p_num, p_country, p_cntC, p_line, p_badG, p_GTDName
  IF .NOT. EMPTY(p_num)
    cnt_found = cnt_found+1
    REPLACE GTD_NAME WITH IIF(p_badG, "См. класс", p_GTDName), ;
            GTD_NUM  WITH p_num,      ;
            GTD_LINE WITH p_line,     ;
            COUNTRY  WITH p_country,  ;
            COUNTRYC WITH p_cntC,     ;
            B_GTD    WITH p_badG
  ENDIF
  SELECT GTD_LINK
  cnt_cur = cnt_cur+1
  IF cnt_cur % 10 = 0
    p = ALLTRIM(STR(cnt_cur))+"/"+ALLTRIM(STR(cnt_hole))
    @ WROWS()-1, 0 SAY PADC(p, WCOLS())
  ENDIF
ENDSCAN

DO Wt_Mess

IF view
  PRIVATE mss0
  DIMENSION mss0(3)
  mss0(1) = ""
  mss0(2) = CHR(0)+"Обработано "+ALLTRIM(STR(cnt_hole))+" позиций, найдено ГТД для "+ALLTRIM(STR(cnt_found))+"."
  mss0(3) = ""
  DO Out_Mess WITH 5, "mss0"
  DO V_GTD
ENDIF

SELECT (s_sav)

RETURN cnt_found

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура V_GTD        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                   Просмотр результата привязки ГТД.                    │
*│                                                                        │
*└────────────────────────────────────────────────────────── 26.02.2000 ──┘
PROCEDURE V_GTD

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
SELECT 0
DO Use_Dummy
SELECT 0
DO Use_Link
ZAP
SELECT GTD_LINK

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Mark OF (menu_name) PROMPT "Отметки"
ON PAD Mark OF (menu_name) ACTIVATE POPUP Mark

  DEFINE POPUP Mark MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Mark PROMPT "Инвертировать" KEY Ctrl-A, "Ins"
  ON SELECTION BAR 1 OF Mark DO Sw_Mode WITH "MarkOne"

  DEFINE BAR  2 OF Mark PROMPT "Инвертировать все" KEY Ctrl-A, "*"
  ON SELECTION BAR 2 OF Mark DO Sw_Mode WITH "MarkAll"

  DEFINE BAR  3 OF Mark PROMPT "Установить остаток" KEY Ctrl-A, "+"
  ON SELECTION BAR 3 OF Mark DO Sw_Mode WITH "Set"

  DEFINE BAR  4 OF Mark PROMPT "Очистить остаток" KEY Ctrl-A, "-"
  ON SELECTION BAR 4 OF Mark DO Sw_Mode WITH "Clear"

DEFINE PAD GTD OF (menu_name) PROMPT "ГТД" KEY Ctrl-A, "Enter"
ON SELECTION PAD GTD OF (menu_name) DO Sw_Mode WITH "GTD"

DEFINE PAD Filter OF (menu_name) PROMPT "Фильтр" KEY Ctrl-A, "F7"
ON SELECTION PAD Filter OF (menu_name) DO Sw_Mode WITH "Filter"

DEFINE PAD Copy OF (menu_name) PROMPT "Копировать"
ON PAD Copy OF (menu_name) ACTIVATE POPUP Copy

  DEFINE POPUP Copy MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Copy PROMPT "Эталон" KEY Ctrl-A, "Ctrl-C"
  ON SELECTION BAR 1 OF Copy DO Sw_Mode WITH "Copy"

  DEFINE BAR  2 OF Copy PROMPT "Вставить" KEY Ctrl-A, "Ctrl-V"
  ON SELECTION BAR 2 OF Copy DO Sw_Mode WITH "PasteOne"

  DEFINE BAR  3 OF Copy PROMPT "Вставить от текущей строки"
  ON SELECTION BAR 3 OF Copy DO Sw_Mode WITH "PasteNext"

  DEFINE BAR  4 OF Copy PROMPT "Вставить в отмеченные"
  ON SELECTION BAR 4 OF Copy DO Sw_Mode WITH "PasteMark"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE g_sav, c_sav, cc_sav, l_sav
STORE 0 TO g_sav, c_sav, cc_sav, l_sav
PRIVATE tmpGTD, point1, point2, point3
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 51

DO D_Wins WITH ln, wd, "ГТД для списка позиций", 1, 2
what_do = "List"
@ WROWS()-4, 2 SAY "── Номер ГТД ────────────── Страна "
@ WROWS()-2, 3 SAY "Имя ГТД"
@ 1, 2 SAY "Фильтр:"
*┌───────────────────────────────────────┐
*│XXXX XXXXXXXXXXXXXXXXXXXXXXXXX XXXXXXXX│
*└───────────────────────────────────────┘
*┌── Номер ГТД ────────────── Страна ────┐
*│XXXXXXXXXXXXXXXXXXXXXXXXXX XXXXXXXXXXXX│
*│ Имя ГТД XXXXXXXXXXXXXXXXXXXXXXXXXXXXX │
*└───────────────────────────────────────┘

GO TOP
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Home   GO TOP
    ON KEY LABEL End    GO BOTTOM
    ON KEY LABEL Enter  KEYBOARD CHR(23)
    ON KEY LABEL Ins    DO Sw_Mode WITH "MarkOne"
    ON KEY LABEL "*"    DO Sw_Mode WITH "MarkAll"
    ON KEY LABEL "+"    DO Sw_Mode WITH "Set"
    ON KEY LABEL "-"    DO Sw_Mode WITH "Clear"
    ON KEY LABEL F7     DO Sw_Mode WITH "Filter"
    ON KEY LABEL Ctrl+C DO Get_Copy
    ON KEY LABEL Ctrl+V DO P_One

*
*    BROWSE - меню
*
    BROWSE FIELDS M000 = IIF(GTD_LINK.MARK, "√", " "):1:H="", ;
                  GTD_LINK.PREFIX:4:H="", ;
                  GTD_LINK.NAME:35:H="",  ;
                  GTD_LINK.PRODUCER:8:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "GTD")
    ENDIF

  CASE what_do = "MarkOne"    && Инвертировать

    IF .NOT. BOF() .AND. .NOT. EOF()
      REPLACE GTD_LINK.MARK WITH .NOT. GTD_LINK.MARK
      SKIP
      IF EOF()
        GO BOTTOM
      ENDIF
    ENDIF
    what_do = "List"
      
  CASE what_do = "MarkAll" && Инвертировать все

    IF .NOT. BOF() .AND. .NOT. EOF()
      r_sav = RECNO()
      REPLACE ALL GTD_LINK.MARK WITH .NOT. GTD_LINK.MARK
      GO r_sav
    ENDIF
    what_do = "List"
      
  CASE what_do = "Set"     && Установить остаток

    IF .NOT. BOF() .AND. .NOT. EOF()
      r_sav = RECNO()
      REPLACE REST GTD_LINK.MARK WITH .T.
      GO r_sav
    ENDIF
    what_do = "List"
      
  CASE what_do = "Clear"   && Очистить остаток

    IF .NOT. BOF() .AND. .NOT. EOF()
      r_sav = RECNO()
      REPLACE REST GTD_LINK.MARK WITH .F.
      GO r_sav
    ENDIF
    what_do = "List"
      
  CASE what_do = "GTD"        && Список ГТД

    IF .NOT. BOF() .AND. .NOT. EOF()
      tmpGTD = Get_GTD(GTD_LINK.PREFIX, GTD_LINK.NAME, GTD_LINK.PRODUCER)
      point1 = AT(CHR(13), tmpGTD)
      point2 = AT(CHR(13), tmpGTD, 2)
      point3 = AT(CHR(13), tmpGTD, 3)
      IF point1 # 0
        REPLACE GTD_LINK.GTD_NUM  WITH LEFT(tmpGTD,point1-1), ;
                GTD_LINK.COUNTRY  WITH SUBSTR(tmpGTD,point1+1, point2-point1-1), ;
                GTD_LINK.GTD_LINE WITH SUBSTR(tmpGTD,point2+1, point3-point2-1), ;
                GTD_LINK.COUNTRYC WITH SUBSTR(tmpGTD,point3+1)
      ENDIF
    ENDIF
    what_do = "List"

  CASE what_do = "Filter"        && Фильтр

    DO Set_Fltr
    ACTIVATE WINDOW (win_name+"_M") SAME
    @ 1, 10 SAY flt_val
    what_do = "List"

  CASE what_do = "Copy"        && Копировать

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO Get_Copy
    ENDIF
    what_do = "List"

  CASE what_do = "PasteOne"    && Вставить в текущуу строку

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO P_One
    ENDIF
    what_do = "List"

  CASE what_do = "PasteNext"    && Вставить от текущей строки

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO P_Next
    ENDIF
    what_do = "List"

  CASE what_do = "PasteMark"    && Вставить в отмеченные

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO P_Mark
    ENDIF
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

SELECT LINK
ZAP
SET RELATION TO
USE
SELECT GTD_LINK

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

RETURN

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
@ WROWS()-3, 2 SAY GTD_LINK.GTD_NUM COLOR SCHEME 13
@ WROWS()-3, COL()+1 SAY LEFT(GTD_LINK.COUNTRY,12) COLOR SCHEME 13
@ WROWS()-2, 11 SAY GTD_LINK.GTD_NAME COLOR SCHEME 13

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Set_Fltr     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Установка/очистка фильтра.                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 05.04.2000 ──┘
PROCEDURE Set_Fltr
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

*┌─────────────────────────────────────────────┐
*│      ┌─ Шаблон ──────────────────────┐      │
*│      │   ....:....!....:....!....:   │      │
*│      └───────────────────────────────┘      │
*│              [ ] Только с ГТД               │
*│< OK Ctrl-W > < Отказаться Esc > < Очистить >│
*└─────────────────────────────────────────────┘

PRIVATE ex, tmpVal, tmpSw
ex = 1
tmpVal = flt_val
tmpSw = swFull
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 8, 49, "Фильтр"
DO Sun_Bord WITH  2,  8,  4, 40, " Шаблон "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3, 12 GET tmpVal
@ 5, 16 GET tmpSw PICTURE "@*C Только с ГТД"
IF EMPTY(flt_val)
  @ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "
ELSE
  @ WROWS()-2, 2 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc ;\ Очистить "
ENDIF

READ CYCLE

IF ex # 2 .AND. EMPTY(tmpVal) .AND. .NOT. tmpSw
  ex = 3
ENDIF

DO CASE
CASE ex = 1
  *
  * Отрабатываем бланк
  tmpVal = ALLTRIM(tmpVal)
  swFull = tmpSw
  SELECT LINK
  SET RELATION TO
  ZAP
  SELECT GTD_LINK
  DO Wt_Mess WITH "Поиск..."
  SCAN
    IF .NOT. EMPTY(tmpVal)
      IF .NOT. tmpVal $ ALLTRIM(GTD_LINK.PREFIX)+ALLTRIM(GTD_LINK.NAME)+ALLTRIM(GTD_LINK.PRODUCER)
        LOOP
      ENDIF
    ENDIF
    IF swFull
      IF EMPTY(GTD_LINK.GTD_NUM)
        LOOP
      ENDIF
    ENDIF
    SELECT LINK
    APPEND BLANK
    REPLACE LNK WITH RECNO("GTD_LINK")
    SELECT GTD_LINK
  ENDSCAN
  DO Wt_Mess
  IF RECCOUNT("LINK") = 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Ничего не удалось найти. Очищаем фильтр!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    SELECT GTD_LINK
    flt_val = SPACE(LEN(flt_val))
  ELSE
    SELECT LINK
    SET RELATION TO LNK INTO GTD_LINK
    flt_val = PADR(tmpVal, LEN(flt_val))
  ENDIF
CASE ex = 3
  *
  * Отрабатываем бланк
  swFull = tmpSw
  SELECT LINK
  ZAP
  SET RELATION TO
  SELECT GTD_LINK
  flt_val = SPACE(LEN(flt_val))
  swFull = .F.
ENDCASE
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Get_Copy     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                               Копировать                               │
*│                                                                        │
*└────────────────────────────────────────────────────────── 05.04.2000 ──┘
PROCEDURE Get_Copy

IF .NOT. BOF() .AND. .NOT. EOF()
  g_sav = GTD_LINK.GTD_NUM
  c_sav = GTD_LINK.COUNTRY
  cc_sav = GTD_LINK.COUNTRYC
  l_sav = GTD_LINK.GTD_LINE
ENDIF

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура P_One        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                 Вставить из кармана в текущую строку.                  │
*│                                                                        │
*└────────────────────────────────────────────────────────── 05.04.2000 ──┘
PROCEDURE P_One

IF .NOT. BOF() .AND. .NOT. EOF()
  IF TYPE("g_sav") = "C"
    REPLACE GTD_LINK.GTD_NUM  WITH g_sav, ;
            GTD_LINK.COUNTRY  WITH c_sav, ;
            GTD_LINK.COUNTRYC WITH cc_sav,;
            GTD_LINK.GTD_LINE WITH l_sav
    DO Draw_Foot
  ENDIF
ENDIF

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура P_Next       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│              Вставить из кармана в следующие строки.                   │
*│                                                                        │
*└────────────────────────────────────────────────────────── 05.04.2000 ──┘
PROCEDURE P_Next

PRIVATE r_sav

IF .NOT. BOF() .AND. .NOT. EOF()
  IF TYPE("g_sav") = "C"
    r_sav = RECNO()
    REPLACE REST GTD_LINK.GTD_NUM  WITH g_sav, ;
                 GTD_LINK.COUNTRY  WITH c_sav, ;
                 GTD_LINK.COUNTRYC WITH cc_sav,;
                 GTD_LINK.GTD_LINE WITH l_sav
    GO r_sav
  ENDIF
ENDIF

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура P_Mark       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│             Вставить из кармана в отмеченные строки.                   │
*│                                                                        │
*└────────────────────────────────────────────────────────── 05.04.2000 ──┘
PROCEDURE P_Mark

PRIVATE r_sav

IF .NOT. BOF() .AND. .NOT. EOF()
  IF TYPE("g_sav") = "C"
    r_sav = RECNO()
    REPLACE ALL FOR GTD_LINK.MARK  ;
                 GTD_LINK.GTD_NUM  WITH g_sav, ;
                 GTD_LINK.COUNTRY  WITH c_sav, ;
                 GTD_LINK.COUNTRYC WITH cc_sav,;
                 GTD_LINK.GTD_LINE WITH l_sav
    GO r_sav
  ENDIF
ENDIF

RETURN
