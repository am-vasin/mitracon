*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Customer     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                             Выбор клиента.                             ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 04.05.2001 ══╝
PROCEDURE Customer
PARAMETERS parmCode, parmName, parmFlags, A000, B000, C000
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
PRIVATE tmpFlags
IF TYPE("parmFlags") = "C"
  tmpFlags = parmFlags
ELSE
  tmpFlags = ""
ENDIF

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
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS TMP_1504 AGAIN
IF parmCode > 0 .AND. SEEK(parmCode)
  SET ORDER TO TAG CUS_NAME
ELSE
  SET ORDER TO TAG CUS_NAME
  GO TOP
ENDIF

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Выбрать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Seek OF (menu_name) PROMPT "Поиск"
ON PAD Seek OF (menu_name) ACTIVATE POPUP Seek

  DEFINE POPUP Seek MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Seek PROMPT "По контексту" KEY Ctrl-A, "F7"
  ON SELECTION BAR 1 OF Seek DO Sw_Mode WITH "Seek_1"
  
  DEFINE BAR  2 OF Seek PROMPT "Повтор" KEY Ctrl-A, "Ctrl-F7"
  ON SELECTION BAR 2 OF Seek DO Sw_Mode WITH "Again"
  
IF "I" $ UPPER(tmpFlags)

  DEFINE PAD Info OF (menu_name) PROMPT "Информация"
  ON PAD Info OF (menu_name) ACTIVATE POPUP Info

    DEFINE POPUP Info MARGIN RELATIVE SHADOW COLOR SCHEME 4

    DEFINE BAR  1 OF Info PROMPT "История" KEY Ctrl-A, "F1"
    ON SELECTION BAR 1 OF Info DO Sw_Mode WITH "History"

    DEFINE BAR  2 OF Info PROMPT "Депозит" KEY Ctrl-A, "F2"
    ON SELECTION BAR 2 OF Info DO Sw_Mode WITH "Deposit"

    DEFINE BAR  3 OF Info PROMPT "Маркетинговая информация" KEY Ctrl-A, "Shift-F1"
    ON SELECTION BAR 3 OF Info DO Sw_Mode WITH "Market"

    DEFINE BAR  4 OF Info PROMPT "Атрибуты для счета-фактуры" KEY Ctrl-A, "Shift-F2"
    ON SELECTION BAR 4 OF Info DO Sw_Mode WITH "Cus_Bil"

    DEFINE BAR  5 OF Info PROMPT "Досье" KEY Ctrl-A, "Ctrl-F1"
    ON SELECTION BAR 5 OF Info DO Sw_Mode WITH "Dossier"

    DEFINE BAR  6 OF Info PROMPT "Паспортные данные" KEY Ctrl-A, "Ctrl-F2"
    ON SELECTION BAR 6 OF Info DO Sw_Mode WITH "Pasport"

ENDIF

IF "S" $ UPPER(tmpFlags)
  DEFINE PAD Add OF (menu_name) PROMPT "Добавить" KEY Ctrl-A, "Ins"
  ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"
ENDIF

DEFINE PAD Exit OF (menu_name) PROMPT "Отказаться" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = FSIZE("CUS_NAME")

DO D_Wins WITH ln, wd, IIF("S" $ UPPER(tmpFlags),parmName,"Клиенты"), 1, 0
@ 1, 2 SAY "Поиск "
@ 1, 8 SAY "Tab" COLOR (SCHEME(14,2))
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F7       DO Sw_Mode WITH "Seek_1"
    ON KEY LABEL Ctrl+F7  DO Sw_Mode WITH "Again"
    IF "I" $ UPPER(tmpFlags)
      ON KEY LABEL Shift+F1 DO Sw_Mode WITH "Market"
      ON KEY LABEL F2       DO Sw_Mode WITH "Deposit"
      ON KEY LABEL Shift+F2 DO Sw_Mode WITH "Cus_Bil"
      ON KEY LABEL Ctrl+F2  DO Sw_Mode WITH "Pasport"
      ON KEY LABEL F1       DO Sw_Mode WITH "History"
      ON KEY LABEL Ctrl+F1  DO Sw_Mode WITH "Dossier"
    ENDIF
    IF "S" $ UPPER(tmpFlags)
      ON KEY LABEL Ins DO Sw_Mode WITH "Add"
    ENDIF    
    ON KEY LABEL Tab      DO Locator WITH 1, 8, 30
*
*    BROWSE - меню
*
    BROWSE FIELDS CUS_NAME:H="",       ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      IF READKEY() % 256 = 12
        what_do = "Exit"
      ENDIF
      IF READKEY() % 256 = 14
        what_do = "Choose"
      ENDIF
    ENDIF

  CASE what_do = "Choose"    && Выбор

    parmCode = CUS_CODE
    IF TYPE("parmName") = "C"
      parmName = CUS_NAME
    ENDIF
    EXIT

  CASE what_do = "Seek_1"    && Контекстный поиск

    IF Seek_1()
      what_do = "Again"
    ELSE
      what_do = "List"
    ENDIF

  CASE what_do = "Again"      && Повтор

    = Ch_Found()
    what_do = "List"

  CASE what_do = "Market"     && Маркетинговая информация
   
  IF "I" $ tmpFlags
    DO Cli_Info WITH "", "I" $ tmpFlags, CUS_CODE
    what_do = "List"
  ENDIF

  CASE what_do = "History"    && История

    DO Acc_Hist WITH CUS_CODE
    what_do = "List"

  CASE what_do = "Deposit"    && Депозит

    DO V_Note WITH "I" $ tmpFlags, CUS_CODE
    what_do = "List"

  CASE what_do = "Dossier"     && Досье

    DO Dossier WITH CUS_CODE, IIF("I" $ tmpFlags, "C", "")
    what_do = "List"

  CASE what_do = "Cus_Bil"     && Атрибуты для с-ф

    DO Cus_Bil WITH CUS_CODE, CUS_NAME, "i" $ tmpFlags
    what_do = "List"

  CASE what_do = "Pasport"     && Паспортные данные

    DO Pasport WITH CUS_CODE, .F., .F., .F., .F., .F., .F., .F., ;
                    IIF("i" $ tmpFlags, "", "C")
    what_do = "List"

  CASE what_do = "Add"         && Новый клиент
  
    DO Add_Cus WITH parmName, parmCode
    SET ORDER TO TAG CUS_CODE
    IF SEEK(parmCode)
      = RECNO()
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
SELECT TMP_1504
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

*ACTIVATE WINDOW (win_name+"_M") SAME

*
*   Вот здесь, рисуем...
*

*ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Seek_1       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Контекстный поиск                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 04.05.2001 ──┘
PROCEDURE Seek_1

PRIVATE v_shab, r_sav, s_sav

v_shab = Get_Shab()
v_shab = SYS(15, lwr, v_shab)

IF EMPTY(v_shab)
  RETURN .F.
ENDIF

r_sav = RECNO()
s_sav = SELECT()
SELECT 0
DELETE FILE (tmpo_path+"LINK.DBF")
CREATE DBF (tmpo_path+"LINK") ( LNK N(10) )
USE (tmpo_path+"LINK")
SELECT TMP_1504
DO Wt_Mess WITH "Ищем..."
SCAN FOR v_shab $ SYS(15, lwr, CUS_NAME)
  SELECT LINK
  APPEND BLANK
  REPLACE LNK WITH RECNO("TMP_1504")
  SELECT TMP_1504
ENDSCAN

SELECT LINK
USE

SELECT (s_sav)
IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF

DO Wt_Mess

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Ch_Found     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                          Выбор из найденного.                          │
*│                                                                        │
*└────────────────────────────────────────────────────────── 04.05.2001 ──┘
PROCEDURE Ch_Found

PRIVATE mss, s_sav, r_sav, ret_v

ret_v = .F.

IF .NOT. File_O(tmpo_path+"LINK.DBF")
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Ничего не найдено..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

r_sav = RECNO()
s_sav = SELECT()

SELECT 0
USE (tmpo_path+"LINK")

IF RECCOUNT() = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Ничего не найдено..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  RETURN .F.
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

SELECT 0
DO Use_Dummy

SELECT TMP_1504
SET ORDER TO

SELECT LINK
SET RELATION TO LNK INTO TMP_1504
GO TOP
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
ln = WROWS("")-11   && Количество видимых строк BROWSE
ln = MIN(ln, RECCOUNT())
wd = FSIZE("CUS_NAME", "TMP_1504")

DO D_Wins WITH ln, wd, "Результат контекстного поиска", 0, 0
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
    BROWSE FIELDS TMP_1504.CUS_NAME:H="",       ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Выбор

    ret_v = .T.
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
SELECT LINK
SET RELATION TO
USE

SELECT TMP_1504
SET ORDER TO TAG CUS_NAME
IF .NOT. ret_v
  IF BETWEEN(r_sav, 1, RECCOUNT())
    GO r_sav
  ENDIF
ENDIF

SELECT (s_sav)

RETURN ret_v

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Locator      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Поиск по начальным буквам                        │
*│                                                                        │
*└────────────────────────────────────────────────────────── 04.05.2001 ──┘
PROCEDURE Locator
PARAMETERS l001, c001, ln

PRIVATE prf_flt, is_ins, c_flt, chr_type, c, c_c, c_t, v_sav, r_sav, c_sav

is_ins = INSMODE()   && Режим вставка/замена

*
*   "*" - коды, соответствующие печатным символам
*
chr_type = SPACE(31)+     REPLICATE("*",126-31 )+ ;
           SPACE(127-126)+REPLICATE("*",175-127)+ ;
           SPACE(223-175)+REPLICATE("*",241-223)+ ;
           SPACE(255-223)
*
*   "I" - коды, соответствующие символам внутренней навигации
*
chr_type = STUFF(chr_type,   1, 1, "I")  &&  Home
chr_type = STUFF(chr_type,   4, 1, "I")  &&  Стрелка вправо
chr_type = STUFF(chr_type,   6, 1, "I")  &&  End
chr_type = STUFF(chr_type,   7, 1, "I")  &&  Del
chr_type = STUFF(chr_type,  13, 1, "I")  &&  Enter
chr_type = STUFF(chr_type,  19, 1, "I")  &&  Стрелка влево
chr_type = STUFF(chr_type,  22, 1, "I")  &&  Ins
chr_type = STUFF(chr_type, 127, 1, "I")  &&  BackSpace

*
*   "E" - коды, соответствующие символам внешней навигации ( по базе )
*

chr_type = STUFF(chr_type,  3, 1, "E")  &&  PgDn
chr_type = STUFF(chr_type,  5, 1, "E")  &&  Стрелка вверх
chr_type = STUFF(chr_type, 18, 1, "E")  &&  PgUp
chr_type = STUFF(chr_type, 24, 1, "E")  &&  Стрелка вниз

prf_flt = ""    && Префикс
c_flt   = 0     && Текущая позиция курсора

DO WHILE .T.
  ACTIVATE WINDOW (win_name+"_M") SAME
  @ l001, c001 SAY PADR(prf_flt, ln, "▒")
  @ l001, c001+c_flt SAY ""
  v_sav   = prf_flt
  c_sav   = c_flt

  c_c = INKEY(0)                && Код символа
  IF c_c >= 0
    c = CHR(c_c)                && Символ
    c_t = SYS(15, chr_type, c)  && Тип символа
  ENDIF
  IF INLIST(c_c, 27, 9)
    EXIT
  ENDIF
  DO CASE
  CASE c_c < 0                  && Функциональная клавиша
  CASE c_t = "*"                && Печатный символ
    prf_flt = LEFT(prf_flt, c_flt)+c+  ;
              SUBSTR(prf_flt, c_flt+IIF(is_ins,1,2))
    prf_flt = LEFT(prf_flt, ln)
    c_flt = c_flt+1
    c_flt = MIN(c_flt, ln)
  CASE c_t = "E"                && Навигация по базе
  CASE c_t = "I"                && Внутренняя навигация
    DO CASE
    CASE c_c = 7                && Del
      prf_flt = LEFT(prf_flt, c_flt)+SUBSTR(prf_flt, c_flt+2)
    CASE c_c = 127              && BackSpace
      prf_flt = LEFT(prf_flt, c_flt-1)+SUBSTR(prf_flt, c_flt+1)
      c_flt = MAX(c_flt-1,0)
    CASE c_c = 19               && Стрелка влево
      c_flt = MAX(c_flt-1,0)
    CASE c_c = 4                && Стрелка вправо
      c_flt = MIN(c_flt+1,LEN(prf_flt))
    CASE c_c = 22               && Ins
      is_ins = .NOT. is_ins
      = INSMODE(is_ins)
    CASE c_c = 13
      EXIT
    ENDCASE
  OTHERWISE
    LOOP
  ENDCASE
  IF .NOT. v_sav == prf_flt
    r_sav   = RECNO()
    IF .NOT. SEEK(SYS(15, lwr, prf_flt))
      prf_flt = v_sav
       c_flt   = c_sav
       IF BETWEEN(r_sav, 1, RECCOUNT())
          GO r_sav
       ENDIF
    ENDIF
    SHOW WINDOW (ALIAS()) SAME REFRESH
  ENDIF

ENDDO

@ 1, 8 SAY PADR("Tab", ln) COLOR (SCHEME(14,2))

RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                        Программа создания клиента.                    ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Add_Cus
PARAMETER cc_name, cc_code
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
PRIVATE c_sav
*
*   Заполняем значениями переменные состояния...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()
c_sav = SELECT()
*------------------------------------------------------------------------
*    Содержательная часть программы:
*

PRIVATE ex, nm_w, sw_i, sw_a, cd, rc_sav, typ_cli, tmpINN, tmpKPP
                     &&   Объявляем и заполняем поля бланка
ex   = 1             &&
m.tmpINN = ""
m.tmpKPP = ""
DO Get_INN WITH 0, m.tmpINN, m.tmpKPP

IF EMPTY(cc_name)
  nm_w = SPACE(50)
ELSE
  nm_w = cc_name
ENDIF  
sw_i = .F.
sw_a = .F.
SELECT 0
USE (A000) ALIAS ACC_C AGAIN
IF EMPTY(MARK1)
  typ_cli = "0"
ELSE
  typ_cli = MARK1
ENDIF
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE SHARED ALIAS ACC_2832 AGAIN
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
*
*┌────────────────────────────────────────────────────────┐
*│ ┌─ Имя клиента ──────────────────────────────────────┐ │
*│ │ ....:....!....:....!....:....!....:....!....:....! │ │
*│ └────────────────────────────────────────────────────┘ │
*│        ИНН ....:....!..    КПП ....:....!....:.        │
*│      [ ] Карточка клиента    [ ] Атрибуты клиента      │
*│                                                        │
*│            < OK Ctrl-W > < Отказаться Esc >            │
*│                                                        │
*└────────────────────────────────────────────────────────┘

DO Prp_Nav_2
DO D_Win_N WITH 11, 60, "Клиент"
DO Sun_Bord WITH 2, 3, 4, WCOLS()-4
@ 2, 5 SAY " Имя клиента "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3,  5 GET nm_w
@ 5, 10 SAY "ИНН" GET m.tmpINN
@ 5, 30 SAY "КПП" GET m.tmpKPP
@ 6,  8 GET sw_i PICTURE "@*C Карточка клиента"
@ 6, 32 GET sw_a PICTURE "@*C Атрибуты клиента"
@ 8, 14 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_Name()

IF ex = 1
  IF sys_char = "A" .OR. sale_mode = "Mitin"   && основная площадка: добавляем запись

    cd = Doc_Num("CUS_CODE  ", "", arm, DATE())
    SELECT ACC_2832
    APPEND BLANK
    REPLACE SUBSYS   WITH sys_char,;
            CUS_CODE WITH cd,      ;
            CUS_NAME WITH nm_w,    ;
            DATE_ON  WITH DATE(),  ;
            WHO      WITH user,    ;
            WHERE    WITH arm,     ;
            MARK1    WITH typ_cli
    rc_sav = RECNO()
    DO Put_INN WITH CUS_CODE, m.tmpINN, m.tmpKPP

  ELSE     && Не основная площадка: используем заготовку

    rc_sav = RECNO()
    SET ORDER TO TAG EMP_NAME
    GO TOP
    IF EOF() .OR. BOF()
      PRIVATE mss
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"Вы исчерпали все заготовки!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      sw_i = .F.
      sw_a = .F.
    ELSE
      REPLACE SUBSYS   WITH sys_char, ;
              CUS_NAME WITH nm_w,     ;
              DATE_ON  WITH DATE(),   ;
              WHO      WITH user,     ;
              WHERE    WITH arm,      ;
              MARK1    WITH typ_cli
      rc_sav = RECNO()
    ENDIF
    SET ORDER TO TAG CUS_NAME
    IF rc_sav > 0 .AND. rc_sav <= RECCOUNT()
      GO rc_sav
    ENDIF
  ENDIF
  cc_code = CUS_CODE
  IF sw_i
    DO Cli_Inf WITH "", CUS_CODE
  ENDIF
  IF sw_a
    DO Cus_Bilm WITH ACC_2832.CUS_CODE, ACC_2832.CUS_NAME
  ENDIF
ENDIF
*--------------------------------------------------------------------------
SELECT ACC_2832
USE
SELECT ACC_C
USE
POP KEY
RELEASE WINDOW (win_name)
SELECT(c_sav)

RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                    Проверка имени клиента.                            ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Tst_Name

IF ex = 2 .OR. READKEY() % 256 = 12
  RETURN .T.
ENDIF
IF EMPTY(nm_w)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Пустое имя клиента? Так не пойдет!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN Tst_INN(0, m.tmpINN)

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                  Программа создания атрибутов клиента.                ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Cus_Bilm
PARAMETERS cd, nm
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
*┌──────────────────────────────────────────────────────────────────────────┐
*│      Покупатель ....:....!....:....!....:....!....:....!....:....!       │
*│                                                                          │
*│    Адрес ....:....!....:....!....:....!....:....!....:....!....:....!    │
*│                                                                          │
*│Тел. ....:....!....:. К/С ....:....!....:....! Р/С N ....:....!....:....! │
*│                                                                          │
*│             в банке ....:....!....:....!....:....!....:....!             │
*│                                                                          │
*│          БИК ....:....!....:....!        Город ....:....!....:.          │
*│                                                                          │
*│       ИНН ....:....!..   Код по ОКОНХ ....:   Код по ОКПО ....:...       │
*│┌─ Примечание ───────────────────────────────────────────────────────────┐│
*││....:....!....:....!....:....!....:....!....:....!....:....!....:....!..││
*││                                                                        ││
*││                   < OK Ctrl-W > < Отказаться Esc >                     ││
*│└────────────────────────────────────────────────────────────────────────┘│
*│ Кто создал                                 Дата создания                 │
*│ Кто корректировал                          Дата коррекции                │
*└──────────────────────────────────────────────────────────────────────────┘
PRIVATE f01, f02, f03, f04, f05, f06, f07, f08, f09, f10, f11, f12, ex
PRIVATE sel, us, new
PRIVATE c01, c02, c03, c04, c11, c12

sel = SELECT()
**************************
SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS P_2833 AGAIN

c01 = ""
c02 = ""
c03 = {}
c04 = {}
c11 = " "
c12 = " "

SELECT 0
USE (B000) ALIAS BILL_C AGAIN
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS ACC_2833 AGAIN
SET RELATION TO WHO_CORR INTO P_2833
SET RELATION TO WHO INTO P_2833

IF SEEK (cd)
  c01 = ACC_2833.WHO
  c02 = ACC_2833.WHO_CORR
  c03 = DATE_ON
  c04 = DATE_CORR
ENDIF
USE

SELECT P_2833
IF SEEK (c01)
  c11 = ALLTRIM(P_2833.FAMILY)+" "+LEFT(P_2833.NAME,1)+". "+ ;
              LEFT(P_2833.S_NAME,1)+"."
ENDIF
IF SEEK (c02)              
  c12 = ALLTRIM(P_2833.FAMILY)+" "+LEFT(P_2833.NAME,1)+". "+ ;
              LEFT(P_2833.S_NAME,1)+"."
ENDIF              
USE

********************************
IF USED("CUS_BIL")
  us = .T.
  SELECT CUS_BIL
ELSE
  us = .F.
  SELECT 0
  USE CUS_BIL ORDER TAG CUS_CODE
ENDIF

SELECT BILL_C
IF EMPTY(ADDRESS)
 f02 = SPACE(FSIZE("ADDRESS"))
ELSE
  f02 = ADDRESS
ENDIF

IF EMPTY(INN)
  f09 = SPACE(FSIZE("INN"))
ELSE
  f09 = INN
ENDIF
ex   = 1
IF EMPTY(nm)
  f01 = PADR(nm, FSIZE("CUS_NAME"))
ELSE
  f01 = nm  
ENDIF
f03 = SPACE(FSIZE("PHONE"))
f04 = SPACE(FSIZE("ACC_BILL"))
f05 = SPACE(FSIZE("BANK"))
f06 = SPACE(FSIZE("CORR_BILL"))
f07 = SPACE(FSIZE("BIK"))
f08 = SPACE(FSIZE("CITY"))
f10 = SPACE(FSIZE("OKOHX"))
f11 = SPACE(FSIZE("OKPO"))
f12 = SPACE(FSIZE("NOTE"))
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 22, 78, "Атрибуты клиента"

*------------------------------------------------------------------------
*      Ввод полей бланка
*

@  2,  8 SAY "Покупатель" GET f01
@  4,  6 SAY "Адрес" GET f02
@  6,  2 SAY "Тел." GET f03
@  6, 24 SAY "К/С" GET f06
@  6, 49 SAY "Р/С" GET f04
@  8, 15 SAY "в банке" GET f05
@ 10, 12 SAY "БИК" GET f07
@ 10, 44 SAY "Город" GET f08
@ 12,  9 SAY "ИНН" GET f09
@ 12, 28 SAY "Код по ОКОНХ" GET f10
@ 12, 49 SAY "Код по ОКПО" GET f11
DO Sun_Bord WITH 13, 2, 15, 75
@ 13, 4 SAY " Примечание "
@ 14,  3 GET f12
DO Sun_Bord WITH 16, 2, 19, 75
@ WROWS()-5,  3 SAY "Кто создал         "+c11
@ WROWS()-4,  3 SAY "Кто корректировал  "+c12
@ WROWS()-5, 47 SAY "Дата создания   "+DTOC(c03)
@ WROWS()-4, 47 SAY "Дата коррекции  "+DTOC(c04)
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "
READ CYCLE

IF ex = 1
  APPEND BLANK
  REPLACE CUS_CODE  WITH cd,  ;
          CUS_NAME  WITH f01, ;
          ADDRESS   WITH f02, ;
          PHONE     WITH f03, ;
          ACC_BILL  WITH f04, ;
          BANK      WITH f05, ;
          CORR_BILL WITH f06, ;
          BIK       WITH f07, ;
          CITY      WITH f08, ;
          INN       WITH f09, ;
          OKOHX     WITH f10, ;
          OKPO      WITH f11, ;
          NOTE      WITH f12, ;
          DATE_CORR  WITH DATE(),  ;
          WHO_CORR   WITH user,    ;
          WHERE_CORR WITH arm
ENDIF
*------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
IF .NOT. us
  USE
ENDIF
IF USED("CUS_BIL")
  SELECT CUS_BIL
  APPEND FROM (B000)
  USE
ENDIF
SELECT (sel)

RETURN ex = 1
*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                    Программа создания информации о клиенте.           ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Cli_Inf
PARAMETERS ext_menu, parmCode

PRIVATE ln, wd, sel_sav, mss, ex, crsr, manager_n, k_sw, k_code, k_name
PRIVATE tmpName

sel_sav = SELECT()
manager_n = ""

IF EMPTY(parmCode)
  RETURN
  SELECT (sel_sav)
ENDIF
SELECT 0

USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS ACC_2901 AGAIN
IF .NOT. SEEK(parmCode)
  USE
  SELECT (sel_sav)
  RETURN
ENDIF

tmpName = CUS_NAME
IF .NOT. EMPTY(ACC_2901.WHO_NEW)
  SELECT 0
  USE (base_path+"PERSONS") ORDER TAG CODE ALIAS PRS_2901
  SEEK ACC_2901.WHO_NEW
  manager_n = ALLTRIM(FAMILY)+" "+LEFT(NAME,1)+" "+LEFT(S_NAME,1)
  USE
ENDIF
manager_n = PADR(manager_n, 28)
IF TYPE("ext_menu") = "C"
  IF .NOT. EMPTY(ext_menu)
    DEACTIVATE MENU (ext_menu)
  ENDIF
ENDIF

DEFINE WINDOW H_Wind FROM 0, 0 TO 0, WCOLS("") NONE COLOR SCHEME 13
SHOW WINDOW H_Wind
SELECT 0
USE (C000) ALIAS INF_C AGAIN
SELECT 0
USE (base_path+"CLI_KIND") ORDER TAG CODE ALIAS KIND_2901 AGAIN
SELECT 0
USE (base_path+"CLI_INFO") ORDER TAG CUS_CODE ALIAS INFO_2901 AGAIN
k_sw   = .F.
k_code = SPACE(FSIZE("KIND_CODE", "KIND_2901"))
k_name = SPACE(FSIZE("KIND_NAME", "KIND_2901"))
 
PUSH KEY CLEAR
SCATTER MEMVAR BLANK

SELECT INF_C
m.address = ADDRESS
m.fax = FAX
ln = 24
wd = 78

DO D_W WITH "Cli_Info", ln, wd, "Покупатель: "+ALLTRIM(tmpName), 0
@ 0, 0 SAY ""
*       Покупатель: < Имя клиента >
? "┌───────────────────────────────────────────────────────────────────────────┐"
? "│    ┌──── Страна ────────── Регион ────────────────── Город ─────────┐     │"
? "│    │  ....:....1....:.  ....:....1....:....1....  ....:....1....:.  │     │"
? "│    └────────────────────────────────────────────────────────────────┘     │"
? "│┌── Адрес ────────────────────────────────────────────────────────────────┐│"
? "││....:.  ....:....1....:....1....:....1....:....1....:....1....:....1.... ││"
? "││              Телефон ....:....1....       Факс ....:....1               ││"
? "││               Эл. почта ....:....1....:....1....:....1..                ││"
? "│└─────────────────────────────────────────────────────────────────────────┘│"
? "│ Ф.И.О. ....:....1....:....1....:....1..   Должность ....:....1....:....1  │"
? "│┌─[ ] Сфера деят. ────┐┌── Специализация ─────────────────────────────────┐│"
? "││                     ││[ ] Аудио & Мультимедиа  [ ] Мед. оборудование    ││"
? "││                     ││[ ] Видео & Быт. техика  [ ] Авто-техника         ││"
? "││                     ││[ ] Средства связи       [ ] Телекоммуникация     ││"
? "││                     ││[ ] Изм. техника         [ ] Оборуд.радиовещ.и TV ││"
? "││                     ││[ ] Источники питания    [ ] Навигац.оборудование ││"
? "││                     ││[ ] Компьютерная техника     ....:....1....:.     ││"
? "││                     ││[ ] Охранные системы                              ││"
? "│└─────────────────────┘└──────────────────────────────────────────────────┘│"
? "│Примечание ....:....1....:....1....:....1....:....1....:....1....:....1....│"
? "│                   Менеджер ....:....!....:....!....:...                   │"
? "│                     < OK Ctrl-W > < Отказаться Esc >                      │"
? "└───────────────────────────────────────────────────────────────────────────┘"

ex = 1

@  3,  8 GET m.country
@  3, 26 GET m.region
@  3, 52 GET m.city

@  6,  2 GET m.index
@  6, 10 GET m.address PICTURE "@S64"
@  7, 24 GET m.phone
@  7, 50 GET m.fax
@  8, 27 GET m.eml

@ 10,  9 GET m.contact
@ 10, 54 GET m.job

@ 11, 3  SAY " Сфера деят. "
@ 12, 3 GET m.production PICTURE "@*C Производство"
@ 13, 3 GET m.work       PICTURE "@*C Разработка"
@ 14, 3 GET m.sale       PICTURE "@*C Продажа"
@ 15, 3 GET m.repair     PICTURE "@*C Ремонт"
@ 16, 7 GET m.f_other

@ 12, 25 GET m.audio      PICTURE "@*C Аудио & Мультимедиа"
@ 13, 25 GET m.video      PICTURE "@*C Видео & Бытовая тех."
@ 14, 25 GET m.connect    PICTURE "@*C Средства связи"
@ 15, 25 GET m.meastech   PICTURE "@*C Изм. техника"
@ 16, 25 GET m.power      PICTURE "@*C Источники питания"
@ 17, 25 GET m.computer   PICTURE "@*C Компьютерная техника"
@ 18, 25 GET m.quardsys   PICTURE "@*C Охранные системы"

@ 12, 50 GET m.medequip   PICTURE "@*C Мед. оборудование"
@ 13, 50 GET m.auto       PICTURE "@*C Авто-техника"
@ 14, 50 GET m.telecom    PICTURE "@*C Телекоммуникация"
@ 15, 50 GET m.radio      PICTURE "@*C Оборуд.радиовещ.и TV"
@ 16, 50 GET m.navig      PICTURE "@*C Навигац.оборудование"
@ 17, 54 GET m.p_other

@ 20, 12 GET m.note
@ 21, 20 SAY "Менеджер " GET manager_n WHEN .F.

crsr = SET("CURSOR") = "OFF"
SET CURSOR ON
@ 22, 23 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "
READ CYCLE VALID Tst_Blank()
IF ex = 1
  m.who = user
  m.where = arm
  m.cus_code = parmCode
  m.name     = tmpName
  APPEND BLANK
  m.kind_code = k_code
  GATHER MEMVAR
  REPLACE CUS_CODE WITH m.parmCode
ENDIF
IF crsr
  SET CURSOR OFF
ENDIF

USE
SELECT KIND_2901
USE
SELECT ACC_2901
USE
SELECT INFO_2901
APPEND FROM (C000)
USE
RELEASE WINDOW H_Wind
RELEASE WINDOW Cli_Info
SELECT (sel_sav)
POP KEY

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Ch_Kind      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                          Выбор типа клиента.                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.03.2001 ──┘
PROCEDURE Ch_Kind

IF .NOT. k_sw
  RETURN .T.
ENDIF

k_sw = .F.
SHOW GET k_sw

DO Cli_Kind WITH k_code, k_name

SHOW GET k_name
RETURN .T.
*┌────────────────────────────────────────────────────────────────────────┐
*│                                                                        │
*│                            Выход из бланка.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 25.03.2003 ──┘
PROCEDURE Tst_Blank
PRIVATE mss

IF EMPTY(m.country) .AND. ex = 1
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Название страны должно быть заполнено!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.
