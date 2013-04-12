*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла To_Box       Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                    Размещение заготовки по адресам                     ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 23.10.2001 ══╝
PROCEDURE To_Box
PARAMETERS tmpList, tmpCus, tmpName
PRIVATE addrList, c_name, k, tmp, j, is_filter
is_filter = .F.
addrList = ""
PRIVATE mss, mss1
DIMENSION mss(3), mss1(3)
mss(1) = ""
mss(2) = CHR(0)+"Сохранить изменения? [ Да ] [ Нет ]"
mss(3) = ""

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
USE (base_path+"ACCOUNT") AGAIN ALIAS LST_1A23 ORDER TAG CUS_CODE
SEEK tmpCus
c_name = CUS_NAME
USE (base_path+"BOXCONT") AGAIN ALIAS LST_1A23 ORDER TAG LIST_IN
SEEK STR(tmpList,6)
SCAN REST WHILE LIST_NUM = STR(tmpList,6)
  addrList = addrList+STR(BOX_NO)+"!"
ENDSCAN
SET ORDER TO FIL_BOX
SELECT 0
USE (base_path+"BOXES") AGAIN ALIAS BOX_1A23 ORDER TAG BOXADDRESS
SET RELATION TO STR(BOX_NO) INTO LST_1A23
IF LEN(addrList) > 0
  LOCATE FOR BOX_NO = VAL(ALLTRIM(LEFT(addrList,10)))
ENDIF
SET FILTER TO .NOT. FOUND("LST_1A23") .OR. LST_1A23.LIST_NUM = STR(tmpList,6)

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Mark OF (menu_name) PROMPT "Поместить/Изъять" KEY Ctrl-A, "Enter"
ON SELECTION PAD Mark OF (menu_name) DO Sw_Mode WITH "Mark"

DEFINE PAD Seek OF (menu_name) PROMPT "Поиск" KEY Ctrl-A, "F7"
ON SELECTION PAD Seek OF (menu_name) DO Sw_Mode WITH "Seek"

DEFINE PAD Filter OF (menu_name) PROMPT "Фильтр" KEY Ctrl-A, "F6"
ON SELECTION PAD Filter OF (menu_name) DO Sw_Mode WITH "Filter"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 51

DO D_Wins WITH ln, wd, "Адреса для списка № "+ALLTRIM(STR(tmpList)), 3, 0
DO Sun_Bord WITH 1, 1, 3, WCOLS()-2, " Клиент "
@ 2, 3 SAY c_name
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL F6    DO Sw_Mode WITH "Filter"
    ON KEY LABEL F7    DO Sw_Mode WITH "Seek"
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE FIELDS MARK = IIF(STR(BOX_NO)+"!" $ addrList, "√", " "):1:H="",  ;
                  BOXADDRESS:H="", ;
                  NOTE:H="",       ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Просмотр списка

    tmp = STR(BOX_NO)+"!"
    j = AT(tmp, addrList)
    IF j # 0
      addrList = STUFF(addrList, j, 11, "")
    ELSE
      addrList = addrList+tmp
    ENDIF
    what_do = "List"

  CASE what_do = "Seek"      && Поиск

    DO Seek_Line
    what_do = "List"

  CASE what_do = "Filter"    && Фильтр

    is_filter = .NOT. is_filter
    DEFINE PAD Filter OF (menu_name) PROMPT IIF(is_filter, "√", "")+"Фильтр" KEY Ctrl-A, "F6" BEFORE Exit
    IF is_filter
      SET FILTER TO STR(BOX_NO)+"!" $ addrList
    ELSE
      SET FILTER TO .NOT. FOUND("LST_1A23") .OR. LST_1A23.LIST_NUM = STR(tmpList,6)
    ENDIF
    what_do = "List"

  OTHERWISE

    IF Out_Mess(5, "mss") = 1
      SELECT LST_1A23
      SET ORDER TO FIL_BOX
      j = 1
      DO WHILE j < LEN(addrList)
        k = SUBSTR(addrList, j, 10)
        j = j+11
        IF SEEK(k)
          IF LIST_NUM # STR(tmpList, 6)
            SELECT BOX_1A23
            SET FILTER TO
            SET ORDER TO BOX_NO
            SEEK LST_1A23.BOX_NO
            mss1(1) = ""
            mss1(2) = CHR(0)+"Ячейка с адресом "+ALLTRIM(BOXADDRESS)+" уже занята!"
            mss1(3) = ""
            DO Out_Mess WITH 7, "mss1"
            SELECT LST_1A23
            SET ORDER TO TAG LIST_IN
            SELECT BOX_1A23
            SET ORDER TO TAG BOXADDRESS
            SET RELATION TO STR(BOX_NO) INTO LST_1A23
            SET FILTER TO .NOT. FOUND("LST_1A23") .OR. LST_1A23.LIST_NUM = STR(tmpList,6)
            GO TOP
            addrList = STUFF(addrList, j, 11, "")
            what_do = "List"
            EXIT
          ENDIF
        ENDIF
      ENDDO
      IF what_do # "List"
        SET ORDER TO LIST_IN
        SEEK STR(tmpList,6)
        SCAN REST WHILE LIST_NUM = STR(tmpList,6)
          IF .NOT. STR(BOX_NO)+"!" $ addrList
            REPLACE WHO_OUT  WITH user,   ;
                    DATE_OUT WITH DATE(), ;
                    TIME_OUT WITH TIME()
          ENDIF
        ENDSCAN
        SET ORDER TO FIL_BOX
        j = 1
        DO WHILE j < LEN(addrList)
          k = SUBSTR(addrList, j, 10)
          j = j+11
          IF .NOT. SEEK(k)
            k = VAL(ALLTRIM(k))
            APPEND BLANK
            REPLACE BOX_NO    WITH k,      ;
                    LIST_NUM  WITH STR(tmpList,6), ;
                    LST_NAME  WITH tmpName,;
                    CUS_CODE  WITH tmpCus, ;
                    WHO_IN    WITH user,   ;
                    DATE_IN   WITH DATE(), ;
                    TIME_IN   WITH TIME()
          ENDIF
        ENDDO
      ENDIF
    ENDIF

    IF what_do # "List"
      EXIT
    ENDIF

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   Аккуратненько (аккуратненько!) закрываем DBF-файлы.
*
SELECT BOX_1A23
USE
SELECT LST_1A23
USE
SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Seek_Line    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                              Поиск адреса                              │
*│                                                                        │
*└────────────────────────────────────────────────────────── 22.11.2001 ──┘
PROCEDURE Seek_Line
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
* Поиск адреса
*┌───────────────────────────────────────────────┐
*│ Начальные символы для поиска AAAAAAAAAAAAAAAA │
*│                                               │
*│       < OK Ctrl-W > < Отказаться Esc >        │
*└───────────────────────────────────────────────┘

PRIVATE ex, shab, r_sav
shab = SPACE(16)
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 6, 51, "Поиск адреса"

*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ 2, 3 SAY "Начальные символы для поиска" GET shab
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex = 1 .AND. .NOT. EMPTY(shab)
  *
  * Отрабатываем бланк
  shab = TRIM(shab)
  r_sav = RECNO()
  IF .NOT. SEEK(shab)
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Ничено не найдено..."
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    IF BETWEEN(r_sav, 1, RECCOUNT())
      GO r_sav
    ENDIF
  ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
