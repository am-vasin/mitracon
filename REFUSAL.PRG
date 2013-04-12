*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Refusal      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                           Справка по спросу                            ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 07/25/2001 ══╝
PROCEDURE Refusal

PRIVATE dt_s, dt_e, tmpName

dt_e = DATE()
dt_s = GOMONTH(dt_e, -1)

IF .NOT. G_Period(dt_s, dt_e)
  RETURN
ENDIF
tmpName = SYS(3)

CREATE DBF (tmpo_path+tmpName) ;
  ( PREFIX   C( 4), ;
    NAME     C(35), ;
    PRODUCER C( 8), ;
    DATE     D,     ;
    QNT      N( 6), ;
    PRICE    N(8,2),;
    PRICE_D  N(10,4) )
USE (tmpo_path+tmpName) EXCLUSIVE ALIAS TMP_REF

SELECT 0
USE base_path+"REFUSAL" ORDER TAG NAME_DATE

SCAN FOR BETWEEN(DATE, dt_s, dt_e)
  SELECT TMP_REF
  APPEND BLANK
  REPLACE PREFIX   WITH REFUSAL.PREFIX,  ;
          NAME     WITH REFUSAL.NAME,    ;
          PRODUCER WITH REFUSAL.PRODUCER,;
          DATE     WITH REFUSAL.DATE,    ;
          QNT      WITH REFUSAL.QTY,     ;
          PRICE    WITH REFUSAL.PRICE,   ;
          PRICE_D  WITH REFUSAL.PRICE_D
  SELECT REFUSAL
ENDSCAN
USE
SELECT TMP_REF
INDEX ON NAME+PREFIX+PRODUCER+DTOS(DATE) TAG TMP
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
SELECT TMP_REF

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
*
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 77

DO D_Wins WITH ln, wd, "Отказы по цене с "+DTOC(dt_s)+" по "+DTOC(dt_e), 0, 1
@ 1, 2 SAY ;
"─────Позиция───────────────────────────────Дата─────Цена(р)────Цена($)─Кол-во"
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
    BROWSE FIELDS PREFIX:H="",  ;
                  Brw_Name=Nm_Part(NAME):25:H="", ;
                  PRODUCER:H="",;
                  DATE:H="",    ;
                  PRICE:H="",   ;
                  PRICE_D:H="", ;
                  QNT:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           WHEN Draw_Foot()    ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    what_do = "Exit"
*    IF what_do = "List"
*      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
*    ENDIF

*  CASE what_do = "Choose"    && Просмотр списка
*
*    DO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*    what_do = "List"
*
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
CLOSE DATABASES
DELETE FILE (tmpo_path+tmpName+".dbf")
DELETE FILE (tmpo_path+tmpName+".cdx")
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
@ WROWS()-2, 3 SAY PREFIX COLOR SCHEME 1
@ ROW(), COL()+1 SAY NAME COLOR SCHEME 1
@ ROW(), COL()+1 SAY PRODUCER COLOR SCHEME 1

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.
