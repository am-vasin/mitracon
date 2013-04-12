********************************************************************************
*    Файл Ctrl_Ch        Разработчик Андрей Васин                05.02.2012    *
********************************************************************************
*                                                                              *
*                         Выбор страны из справочника                          *
*                                                                              *
********************************************************************************
PROCEDURE Ctrl_Ch
PARAMETERS s_code
*
* s_code - Код для начального значения
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
PRIVATE retVal, s_shab, tmpCode, r_sav, mss

m.retVal = ""
m.s_shab = ""
m.tmpCode = 0

*
*   Заполняем значениями переменные состояния...
*
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
USE (base_path+"OKSM") SHARED AGAIN ORDER TAG B_NAME ALIAS CTRL_CH

IF TYPE("m.s_code") = "C"
  m.tmpCode = VAL(ALLTRIM(m.s_code))
ENDIF
IF TYPE("m.s_code") = "N"
  m.tmpCode = m.s_code
ENDIF
IF m.tmpCode # 0
  m.tmpCode = ALLTRIM(STR(m.tmpCode))
  m.tmpCode = PADL(m.tmpCode, 3, "0")
  SET ORDER TO TAG CODE
  IF .NOT. SEEK(m.tmpCode)
    SET ORDER TO TAG B_NAME
    GO TOP
  ELSE
    SET ORDER TO TAG B_NAME
  ENDIF
ENDIF

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Выбор" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Seek OF (menu_name) PROMPT "Поиск"
ON PAD Seek OF (menu_name) ACTIVATE POPUP Seek

  DEFINE POPUP Seek MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Seek PROMPT "...по коду" KEY Ctrl-A, "F3"
  ON SELECTION BAR 1 OF Seek DO Sw_Mode WITH "Seek_C"
  DEFINE BAR  2 OF Seek PROMPT "...по контексту" KEY Ctrl-A, "F7"
  ON SELECTION BAR 2 OF Seek DO Sw_Mode WITH "Seek_T"
  DEFINE BAR  3 OF Seek PROMPT "...продолжение" KEY Ctrl-A, "Shift-F7"
  ON SELECTION BAR 3 OF Seek DO Sw_Mode WITH "Seek_A"

DEFINE PAD Exit OF (menu_name) PROMPT "Отказаться" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   Расчет размеров окна
*
PRIVATE ln, wd
m.ln = WROWS("")-11   && Количество видимых строк BROWSE
m.wd = FSIZE("CODE")+FSIZE("B_NAME")+1

DO D_Wins WITH m.ln, m.wd, "Классфикатор стран", 0, 0

@ 1, 2 SAY "Код"
@ 1, 6 SAY "Название"
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter    KEYBOARD CHR(23)
    ON KEY LABEL F3       DO Sw_Mode WITH "Seek_C"
    ON KEY LABEL F7       DO Sw_Mode WITH "Seek_T"
    ON KEY LABEL Shift-F7 DO Sw_Mode WITH "Seek_A"

*
*    BROWSE - меню
*
    BROWSE FIELDS CODE:H="",  B_NAME:H="",   ;
           DUMMY.F:H="" FREEZE DUMMY.F       ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Choose"    && Выбор

    IF .NOT. EOF() .AND. .NOT. BOF()
      m.retVal = CODE+B_NAME
    ENDIF
    EXIT

  CASE m.what_do = "Seek_C"    && Поиск по коду

    m.tmpCode = VAL(BankName("Код страны", 4))
    IF m.tmpCode # 0
      m.r_sav = RECNO()
      m.tmpCode = PADL(ALLTRIM(STR(m.tmpCode)), 3, "0")
      SET ORDER TO CODE
      IF .NOT. SEEK(m.tmpCode)
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"Код не найден..."
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        IF BETWEEN(m.r_sav, 1, RECCOUNT())
          GO m.r_sav
        ENDIF
      ENDIF
      SET ORDER TO TAG B_NAME
    ENDIF
    
    m.what_do = "List"

  CASE m.what_do = "Seek_T"    && Поиск по контексту

    m.s_shab = BankName("Контекстный поиск", FSIZE("B_NAME"))
    IF .NOT. EMPTY(m.s_shab)
      m.s_shab = SYS(15, m.lwr, m.s_shab)
      m.r_sav = RECNO()
      LOCATE FOR m.s_shab $ SYS(15, m.lwr, B_NAME)
      IF .NOT. FOUND()
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"Ничего не найдено..."
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        IF BETWEEN(m.r_sav, 1, RECCOUNT())
          GO m.r_sav
        ENDIF
      ENDIF
    ENDIF
    m.what_do = "List"

  CASE m.what_do = "Seek_A"    && Повтор поиска

    IF .NOT. EMPTY(m.s_shab)
      m.r_sav = RECNO()
      CONTINUE
      IF .NOT. FOUND()
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"Ничего не найдено..."
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        IF BETWEEN(m.r_sav, 1, RECCOUNT())
          GO m.r_sav
        ENDIF
      ENDIF
    ENDIF
    m.what_do = "List"

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
SELECT (m.s_sav)

RETURN m.retVal
