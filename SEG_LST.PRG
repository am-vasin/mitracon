*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Seg_Lst      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║            Формитрование списка сегментов, разделенных Cr.             ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 10/31/2007 ══╝
PROCEDURE Seg_Lst

PRIVATE retV

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
m.retV = ""
m.stat_type  = 3
m.menu_name  = PROGRAM()
m.last_mouse = 0
m.win_name   = PROGRAM()

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

m.s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT 0
USE (m.base_path+"ORD_SEG") SHARED AGAIN ORDER TAG SEGMENT ALIAS TMP_S_L

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Mrk OF (menu_name) PROMPT "Отметка" KEY Ctrl-A, "Ins"
ON SELECTION PAD Mrk OF (menu_name) DO Sw_Mode WITH "Mark"

DEFINE PAD Choose OF (menu_name) PROMPT "Выбрать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "Отказаться" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   Расчет размеров окна
*
PRIVATE ln, wd
m.ln = WROWS("")-11   && Количество видимых строк BROWSE
m.wd = FSIZE("SEGMENT", "TMP_S_L")+2

DO D_Wins WITH m.ln, m.wd, "Сегменты", 0, 0
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE m.what_do = "List"    && Просмотр списка

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins DO Sw_Mode WITH "Mark"

*
*    BROWSE - меню
*
    BROWSE FIELDS tmpMrk = IIF(TMP_S_L.SEGMENT $ m.retV, "√", " "):H="":1, ;
                  SEGMENT:H="",  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Choose"    && Выбор

    EXIT

  CASE m.what_do = "Mark"      && Отметка

    IF TMP_S_L.SEGMENT $ m.retV
      m.retV = STRTRAN(m.retV, TMP_S_L.SEGMENT+CHR(10), "")
    ELSE
      m.retV = m.retV+TMP_S_L.SEGMENT+CHR(10)
    ENDIF
    m.what_do = "List"

  OTHERWISE

    m.retV = ""
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
USE IN TMP_S_L
SELECT (m.s_sav)

RETURN m.retV
