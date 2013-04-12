*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Ch_Bank      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                              Выбор банка.                              ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 14.01.1999 ══╝
PROCEDURE Ch_Bank
PARAMETERS f_code, f_name_b, f_name_l, b_code, b_name_b, b_name_l, all_f
*
*  f_code   - Код предприятия, если не указан или пуст, то все;
*  f_name_b - Имя предприятия, заполняется программой если код пуст;
*  f_name_l - Имя предприятия, заполняется программой если код пуст;
*  b_code   - Код банка;
*  b_name_b - Короткое имя банка;
*  b_name_l - Длинное имя банка;
*  all_f    - Для всех предприятий.
*

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
PRIVATE k_w, mss
SELECT 0
DO Use_Dummy
SELECT 0
IF all_f .OR. EMPTY(f_code)
  k_w = ""
  USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS F_9114 AGAIN
  SELECT 0
  USE (base_path+"BANKS") ORDER TAG ALL_NAMES ALIAS B_9114 AGAIN
  SET FILTER TO EMPTY(DATE_OFF) .AND. EMPTY(HIDE)
  SET RELATION TO FIRM_CODE INTO F_9114
  IF .NOT. EMPTY(f_code) .AND. .NOT. EMPTY(b_code)
    SET ORDER TO TAG BANK
    IF .NOT. SEEK(f_code+b_code) .OR. .NOT. EMPTY(DATE_OFF)
      SET ORDER TO TAG ALL_NAMES
      GO TOP
    ELSE
      SET ORDER TO TAG ALL_NAMES
    ENDIF
  ENDIF
ELSE
  k_w = f_code
  USE (base_path+"BANKS") ORDER TAG NAME ALIAS B_9114 AGAIN
  IF .NOT. SEEK(k_w)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Для предпрития нет ни одного банка!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    USE
    SELECT (s_sav)
    RETURN
  ENDIF
  IF .NOT. EMPTY(b_code)
    SET ORDER TO TAG BANK
    IF .NOT. SEEK(f_code+b_code)
      SET ORDER TO TAG NAME
      SEEK k_w
    ELSE
      IF .NOT. EMPTY(DATE_OFF)
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"Этот банк удален!"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        USE
        SELECT (s_sav)
        RETURN
      ENDIF
      SET ORDER TO TAG NAME
    ENDIF
  ENDIF
ENDIF  

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
PRIVATE ln, wd, tit
ln = WROWS("")-IIF(EMPTY(k_w), 14, 11)   && Количество видимых строк BROWSE
wd = 16

DO D_Wins WITH ln, wd, "Список банков", 0, IIF(EMPTY(k_w), 1, 0)
IF EMPTY(k_w)
  @ WROWS()-3, 3 SAY "Предприятие"
ENDIF
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
    BROWSE KEY k_w FIELDS B_NAME:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Выбор

    IF EMPTY(k_w)
      f_code   = F_9114.FIRM_CODE
      f_name_b = F_9114.B_NAME
      f_name_l = F_9114.L_NAME
      b_code   = B_9114.BANK_CODE
      IF TYPE("b_name_b") = "C"
        b_name_b = B_9114.B_NAME
      ENDIF
      IF TYPE("b_name_l") = "C"
        b_name_l = B_9114.L_NAME
      ENDIF
    ELSE
      b_code   = B_9114.BANK_CODE
      IF TYPE("b_name_b") = "C"
        b_name_b = B_9114.B_NAME
      ENDIF
      IF TYPE("b_name_l") = "C"
        b_name_l = B_9114.L_NAME
      ENDIF
    ENDIF
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
IF EMPTY(k_w)
  SELECT F_9114
  USE
ENDIF
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

IF EMPTY(k_w)
  ACTIVATE WINDOW (win_name+"_M") SAME

  *
  *   Вот здесь, рисуем...
  *
  @ WROWS()-2, 2 SAY F_9114.B_NAME

  ACTIVATE WINDOW (ALIAS()) SAME
ENDIF

RETURN .T.
