*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Ch_Lst       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Выбор стандартного списка.                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 07.09.1999 ──┘
PROCEDURE Ch_Lst
PARAMETERS flgs, c_code, f00, b00, nm, usd, chc

PRIVATE s_sav, mss

flgs = SPACE(16)
s_sav = SELECT()
IF .NOT. File_O(prmo_path+"LST_TYPE.DBF")
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет ни одной заготовки!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  flgs = ""
  RETURN
ENDIF
SELECT 0
USE (prmo_path+"LST_TYPE") ALIAS L___0 AGAIN
IF RECCOUNT() = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет ни одной заготовки!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  flgs = ""
  USE
  SELECT (s_sav)
  RETURN
ENDIF
IF RECCOUNT() # 1
  SELECT 0
  DO Use_Dummy
  SELECT L___0

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
  ln = MIN(RECCOUNT(), WROWS("")-11)   && Количество видимых строк BROWSE
  wd = FSIZE("NAME")

  DO D_Wins WITH ln, wd, "Выберите заготовку", 0, 0
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
     BROWSE FIELDS NAME,     ;
            DUMMY.F:H="" FREEZE DUMMY.F ;
            NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
            WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

    CASE what_do = "Choose"    && Просмотр списка

      EXIT

    OTHERWISE

      flgs = ""
      EXIT

    ENDCASE

  ENDDO

  RELEASE WINDOW (win_name+"_I")
  RELEASE WINDOW (win_name+"_E")
  RELEASE WINDOW (win_name+"_M")
  RELEASE MENU (menu_name) EXTENDED
ENDIF
IF LEN(flgs) # 0
  flgs   = FLAGS
  c_code = CUS_CODE
  f00    = FIRM
  b00    = BANK
  nm     = VarLName(LST_NAME)  && NEW 22.12.1999 Подстановка переменных
  usd    = IS_USD
  chc    = IS_CHECK
  IF c_code # 0
    USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS L___0 AGAIN
    SEEK c_code
    nm = ALLTRIM(CUS_NAME)+" "+ALLTRIM(nm)
  ENDIF
ENDIF
*
*   Аккуратненько (аккуратненько!) закрываем DBF-файлы.
*
USE
SELECT (s_sav)

RETURN

* && NEW 22.12.1999
* Написана А. Драчевым для того, чтобы в перечне списков,
* созданных на основе щаблонов, было разнообразие.
* Таблица подстановок:
* \Ч - час, \М - минуты, \С - секунды,
* \ГОД - год (4 цифры), \Г - год (2 цифры), \м - месяц (2 цифры)
*
FUNCTION VarLName  && Подстановка текущих даты и времени в имя списка
PARAMETER LName
PRIVATE s_date, s_day, s_month, s_year4, s_year2, s_hour, s_min, s_sec, s_lname
s_dtos = DTOS(DATE())
s_day = SUBSTR(s_dtos,7,2)
s_month = SUBSTR(s_dtos,5,2)
s_year2 = SUBSTR(s_dtos,3,4)
s_year4 = SUBSTR(s_dtos,1,4)
s_Time = TIME()
s_hour = SUBSTR(s_time,1,2)
s_min = SUBSTR(s_time,4,2)
s_sec = SUBSTR(s_time,7,2)
s_Lname = STRTRAN(LName, '\Д', s_day)
s_Lname = STRTRAN(s_LName, '\м', s_month)
s_Lname = STRTRAN(s_LName, '\ГОД', s_year4)
s_Lname = STRTRAN(s_LName, '\Г', s_year2)
s_Lname = STRTRAN(s_LName, '\Ч', s_hour)
s_Lname = STRTRAN(s_LName, '\М', s_min)
s_Lname = STRTRAN(s_LName, '\С', s_sec)

RETURN s_LName
