*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла V_B_Req      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                   Просмотр требований в бухгалтерию.                   ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 24.12.1998 ══╝
PROCEDURE V_B_Req
PARAMETERS wh

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
* BUHG_REQ (BR_8C24)
*  ├─ LIST_TIT (LT_8C24)
*  ├─ ACCOUNT  (CUS_8C24)
*  ├─ PERSONS  (S_8C24)
*  └─ PERSONS  (B_8C24)
*
s_sav = SELECT()
SELECT 0
DO Use_Dummy
SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS B_8C24 AGAIN
SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS S_8C24 AGAIN
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS CUS_8C24 AGAIN
SELECT 0
USE (base_path+"LIST_TIT") ORDER TAG LST_NUM ALIAS LT_8C24 AGAIN
SELECT 0
USE (base_path+"BUHG_REQ") ORDER TAG DOC_NUM ALIAS BR_8C24 AGAIN
IF TYPE("wh") = "C"
  SET ORDER TO TAG WHERE
ENDIF
GO BOTTOM
SET RELATION TO WHO_REQ INTO S_8C24 ADDITIVE
SET RELATION TO WHO_INP INTO B_8C24 ADDITIVE
SET RELATION TO CUS_CODE INTO CUS_8C24 ADDITIVE
SET RELATION TO LST_NUM INTO LT_8C24 ADDITIVE

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Print OF (menu_name) PROMPT "Печать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Print OF (menu_name) DO Sw_Mode WITH "Print"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   Расчет размеров окна
*
*┌─ Номер ───── Дата ──── Имя списка ──────────────────── Выполн. ─┐
*│1234567890 ДД.ММ.ГГГГ 12345678901234567890123456789012 ДД.ММ.ГГГГ│
*└─────────────────────────────────────────────────────────────────┘
*┌─ Клиент ────────────────────────────────────────────────────────┐
*│ 12345678901234567890123456789012345678901234567890              │
*└─────────────────────────────────────────────────────────────────┘
*┌─ Продавец ─────────────────────── Кассир ───────────────────────┐
*│ 123456789012345678901234 А. А.   123456789012345678901234 А. А. │
*└─────────────────────────────────────────────────────────────────┘
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 65

DO D_Wins WITH ln, wd, "Требования в бухгалтерию", 0, 4
@ 1, 2 SAY "─ Номер ───── Дата ──── Имя списка ──────────────────── Выполн. "
DO Sun_Bord WITH WROWS()-6, 1, WROWS()-4, WCOLS()-2, " Клиент "
DO Sun_Bord WITH WROWS()-3, 1, WROWS()-1, WCOLS()-2, " Продавец "
@ WROWS()-3, 36 SAY " Кассир "
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
    BROWSE FIELDS BR_8C24.DOC_NUM:H="",  ;
                  BR_8C24.DOC_DATE:H="":P="@D", ;
                  LT_8C24.LST_NAME:H="",  ;
                  TERM00 = IIF(EMPTY(BR_8C24.DATE_INP), SPACE(10), DTOC(BR_8C24.DATE_INP)):H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Print")
    ENDIF

  CASE what_do = "Print"    && Просмотр списка

    DO P_B_Req WITH BR_8C24.DOC_NUM, BR_8C24.DOC_DATE
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
SELECT B_8C24
USE
SELECT S_8C24
USE
SELECT CUS_8C24
USE
SELECT LT_8C24
USE
SELECT BR_8C24
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
PRIVATE nm_s, nm_k

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   Вот здесь, рисуем...
*
IF BR_8C24.CUS_CODE = 0
  @ WROWS()-5, 3 SAY SPACE(50)
ELSE
  @ WROWS()-5, 3 SAY CUS_8C24.CUS_NAME
ENDIF

IF BR_8C24.WHO_REQ = 0
  nm_s = "Сам!"
ELSE
  nm_s = ALLTRIM(S_8C24.FAMILY)+" "+ ;
         LEFT(S_8C24.NAME,1)+". "+LEFT(S_8C24.NAME,1)+"."
ENDIF
nm_s = PADR(nm_s,28)


IF EMPTY(BR_8C24.DATE_INP)
  nm_b = ""
ELSE
  IF BR_8C24.WHO_INP = 0
    nm_b = ""
  ELSE
    nm_b = ALLTRIM(B_8C24.FAMILY)+" "+ ;
           LEFT(B_8C24.NAME,1)+". "+LEFT(B_8C24.NAME,1)+"."
  ENDIF
ENDIF
nm_b = PADR(nm_s,28)

@ WROWS()-2, 3 SAY nm_s

@ WROWS()-2, 36 SAY nm_b

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.
