*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла ViewBig      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                      Просмотр приходных ордеров.                       ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 20.09.2000 ══╝
PROCEDURE ViewBig
PARAMETER pMax

IF pMax = 0
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

PRIVATE c_name, show
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
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS C_0920 AGAIN
SEEK BIGORD.CUS_CODE
c_name = CUS_NAME
USE
show = .F.
SELECT BIGORD
SCAN
  IF DAY_SUM > pMax
    show = .T.
    EXIT
  ENDIF
ENDSCAN
IF .NOT. show
  RETURN
ENDIF
GO TOP

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*

*┌─────Дата──────По документу──────За день────┐
*│! ДД.ММ.ГГГГ 999999999999.99 999999999999.99│
*└────────────────────────────────────────────┘
PRIVATE ln, wd
ln = MIN(RECCOUNT(), 10)   && Количество видимых строк BROWSE
wd = 44

DO D_Wins WITH ln, wd, "Приход наличных денег по дням", 1, 0
@ 1, 1 SAY LEFT(c_name, WCOLS()-2)
@ 2, 2 SAY "─────Дата──────По документу──────За день"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
*    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE FIELDS BIG = IIF(DAY_SUM > pMax, "√", " "):1:H="", ;
                  DOC_DATE:H="":V=ReCalc(),         ;
                  DOC_SUM:H="":P="999999999999.99", ;
                  DAY_SUM:H="":P="999999999999.99"  ;
           FREEZE DOC_DATE     ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Просмотр списка

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
*│   Процедура ReCalc       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Пересчет значений таблицы.                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.09.2000 ──┘
PROCEDURE ReCalc

PRIVATE r_sav, s000
r_sav = RECNO()
SELECT BIGORD
SCAN
  GO BIGORD.DOC_REF IN BIGDOC
  SELECT BIGDOC
  REPLACE DOC_DATE WITH BIGORD.DOC_DATE, ENTER_DAT WITH BIGORD.DOC_DATE
  s000 = Cus_Sum(BIGDOC.CUS_CODE, BIGDOC.DOC_DATE, BIGDOC.FIRM)
  SELECT BIGORD
  REPLACE DAY_SUM WITH s000
ENDSCAN

GO r_sav

RETURN .T.
