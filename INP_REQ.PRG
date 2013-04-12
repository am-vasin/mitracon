*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Inp_Req      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                   Ввод денег по заявке от продавца.                    ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 22.07.1999 ══╝
PROCEDURE Inp_Req

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
*IF File_O(base_path+"MAX_ORD.MEM")
*  RESTORE FROM (base_path+"MAX_ORD.MEM") ADDITIVE
*ENDIF
PRIVATE pl_code
pl_code = sys_char

s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE AGAIN ALIAS CS_9722

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE AGAIN ALIAS WS_9722

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE AGAIN ALIAS WK_9722

SELECT 0
USE (base_path+"BUHG_REQ") ORDER TAG DOC_NUM AGAIN
GO BOTTOM
SET RELATION TO WHO_REQ INTO WS_9722
SET RELATION TO WHO_INP INTO WK_9722 ADDITIVE
SET RELATION TO CUS_CODE INTO CS_9722 ADDITIVE

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Seek OF (menu_name) PROMPT "Поиск" KEY Ctrl-A, "F7"
ON SELECTION PAD Seek OF (menu_name) DO Sw_Mode WITH "Seek"

DEFINE PAD Inp_M OF (menu_name) PROMPT "Приход/конвертация" KEY Ctrl-A, "Enter"
ON SELECTION PAD Inp_M OF (menu_name) DO Sw_Mode WITH "Inp_M"

DEFINE PAD Print OF (menu_name) PROMPT "Печать" KEY Ctrl-A, "Tab"
ON SELECTION PAD Print OF (menu_name) DO Sw_Mode WITH "Print"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
*┌─N Требов────Дата─────Клиент─────────────────────────────────────────ПК┐
*│NNNNNNNNNN ДД.ММ.ГГГГ СССССССССССССССССССССССССССССССССССССССССССССС **│
*└───────────────────────────────────────────────────────────────────────┘
*┌─Продавец─────────────────────────────────Кассир───────────────────────┐
*│ ....:....!....:....!.... . .             ....:....!....:....!.... . . │
*└───────────────────────────────────────────────────────────────────────┘
PRIVATE ln, wd
PRIVATE mss
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 72

DO D_Wins WITH ln, wd, "Заявки от продавцов", 0, 1
@ 1, 3 SAY "N Требов────Дата─────Клиент──────────────────────────────────────────ПК"
@ WROWS()-3, 3 SAY "Продавец─────────────────────────────────Кассир"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка
    SET RELATION TO WHO_REQ INTO WS_9722
    SET RELATION TO WHO_INP INTO WK_9722 ADDITIVE
    SET RELATION TO CUS_CODE INTO CS_9722 ADDITIVE

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F7 DO Sw_Mode WITH "Seek"
    ON KEY LABEL Tab DO Sw_Mode WITH "Print"

*
*    BROWSE - меню
*
    BROWSE FIELDS DOC_NUM:10:H="", DOC_DATE:P="@D":H="", CS_9722.CUS_NAME:47:H="", ;
                  S00 = IIF(IS_INC, "√", " ")+IIF(IS_CONV, "√", " "):2:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Inp_M")
    ENDIF

  CASE what_do = "Print"    && Печать

    DO P_B_Req WITH DOC_NUM, DOC_DATE
    what_do = "List"

  CASE what_do = "Inp_M"    && Приход денег

    DO CASE
    CASE IS_CONV
      IF BOF() .OR. EOF()
        what_do = "List"
        LOOP
      ENDIF
      IF CUS_CODE = 0
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"Продавец не указал клиента!"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        what_do = "List"
        LOOP
      ENDIF
      DO Conv_dep WITH CUS_CODE
      SELECT BUHG_REQ
      what_do = "List"
    CASE IS_INC
      IF BOF() .OR. EOF()
        what_do = "List"
        LOOP
      ENDIF
      IF CUS_CODE = 0
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"Продавец не указал клиента!"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        what_do = "List"
        LOOP
      ENDIF
      DO IncMoney WITH FIRM, DOC_NUM, DOC_DATE, "Req"
      SELECT BUHG_REQ
      what_do = "List"

    OTHERWISE
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"Все операции уже выполнены!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      what_do = "List"
      LOOP
    ENDCASE

  CASE what_do = "Seek"    && Поиск

    DO Seek_Num
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
SELECT CS_9722
USE
SELECT WS_9722
USE
SELECT WK_9722
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

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   Вот здесь, рисуем...
*
@ WROWS()-2,  3 SAY WS_9722.FAMILY-(" "+LEFT(WS_9722.NAME,1))-(" "+LEFT(WS_9722.S_NAME,1))
@ WROWS()-2, 44 SAY WK_9722.FAMILY-(" "+LEFT(WK_9722.NAME,1))-(" "+LEFT(WK_9722.S_NAME,1))

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Seek_Num     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Поиск по номеру.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 22.07.1999 ──┘
PROCEDURE Seek_Num
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

*┌────────────────────────────────────┐
*│                                    │
*│ Год 9999   Номер заявки 9999999999 │
*│                                    │
*│  < OK Ctrl-W > < Отказаться Esc >  │
*└────────────────────────────────────┘

PRIVATE ex, y, n
ex = 1
y = YEAR(DATE())
n = SPACE(10)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 7, 40, "Поиск заявки"

*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ 3, 14 SAY "Номер заявки" GET n
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "
@ 3,  3 SAY "Год" GET y PICTURE "9999"

READ CYCLE

IF ex = 1
  *
  * Отрабатываем бланк
  PRIVATE r_sav
  r_sav = RECNO()
  n = PADL(ALLTRIM(n),10)
  IF .NOT. SEEK(STR(y,4)+n)
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Нет заявки с таким номером!"
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
