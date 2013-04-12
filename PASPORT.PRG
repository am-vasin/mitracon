*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Pasport      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                        Ввод паспортных данных.                         ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 27.08.2000 ══╝
PROCEDURE Pasport
PARAMETERS c_code, c_name, p_face, p_no, p_ser, p_line1, p_line2, p_line3, flags
PRIVATE modify
modify = .T.
IF TYPE("flags") = "C"
  modify = "C" $ flags
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
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    Содержательная часть программы:
*
PRIVATE s_sav
s_sav = SELECT()
SELECT 0
IF c_code # 0
  USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS CUS_PASP AGAIN
  = SEEK(c_code)
  SELECT 0
  USE (base_path+"CLI_INFO") ORDER TAG CUS_CODE ALIAS TMP_INFO AGAIN
  SELECT 0
  USE (base_path+"PASPORT") ORDER TAG CUS_CODE ALIAS TMP_PASP AGAIN
ENDIF

*  Паспортные данные
*┌──────────────────────────────────────────────────────┐
*│ ┌─ Клиент ─────────────────────────────────────────┐ │
*│ │....:....!....:....!....:....!....:....!....:....!│ │
*│ └──────────────────────────────────────────────────┘ │
*│          Фамилия И О ....:....!....:....!..          │
*│ ┌─ Паспорт ────────────────────────────────────────┐ │
*│ │             Серия ....:. № ....:...              │ │
*│ │                                                  │ │
*│ │ Выдан ....:....!....:....!....:....!....:....!   │ │
*│ │       ....:....!....:....!....:....!....:....!   │ │
*│ │       ....:....!....:....!....:....!....:....!   │ │
*│ └──────────────────────────────────────────────────┘ │
*│           < OK Ctrl-W > < Отказаться Esc >           │
*└──────────────────────────────────────────────────────┘

PRIVATE ex, tmpName, tmpContact, tmpSeries, tmpNumber, tmpLine1, tmpLine2, tmpLine3
ex = 1
IF c_code # 0
  tmpName = CUS_PASP.CUS_NAME
  SELECT TMP_INFO
  IF SEEK(c_code)
    tmpContact = LEFT(CONTACT,22)
  ELSE
    tmpContact = SPACE(22)
  ENDIF
ELSE
  tmpName = c_name
  tmpContact = p_face
ENDIF

SELECT TMP_PASP
tmpSeries  = SPACE( 8)
tmpNumber  = SPACE( 6)
tmpLine1   = SPACE(40)
tmpLine2   = SPACE(40)
tmpLine3   = SPACE(40)
IF c_code # 0
  IF SEEK(c_code)
    tmpSeries  = PASP_SR
    tmpNumber  = PASP_NO
    tmpLine1   = PASP_W1
    tmpLine2   = PASP_W2
    tmpLine3   = PASP_W3
  ENDIF
ENDIF

*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 15, 58, "Паспортные данные"
DO Sun_Bord WITH  2,  3,  4, 54, " Клиент "
DO Sun_Bord WITH  6,  3, 12, 54, " Паспорт "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@  3,  4 GET tmpName WHEN c_code = 0
@  5, 12 SAY "Фамилия И О" GET tmpContact
@  7, 17 SAY "Серия" GET tmpNumber
@  7, 30 SAY "№" GET tmpSeries
@  9,  5 SAY "Выдан" GET tmpLine1
@ 10, 11 GET tmpLine2
@ 11, 11 GET tmpLine3

IF modify
  @ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "
ELSE
  CLEAR GETS
  @ WROWS()-2, FLOOR(WCOLS()/2-3) GET ex PICTURE "@*HT \ OK "
ENDIF
READ CYCLE

IF ex = 1 .AND. modify
  *
  * Отрабатываем бланк
  IF c_code # 0
    IF .NOT. FOUND("TMP_PASP")
      APPEND BLANK
      REPLACE CUS_CODE WITH c_code
    ENDIF
    REPLACE PASP_NO WITH tmpNumber,   ;
            PASP_SR WITH tmpSeries,   ;
            PASP_W1 WITH tmpLine1,    ;
            PASP_W2 WITH tmpLine2,    ;
            PASP_W3 WITH tmpLine3
    SELECT TMP_INFO
    IF .NOT. FOUND("TMP_INFO")
      APPEND BLANK
      REPLACE CUS_CODE WITH c_code
    ENDIF
    REPLACE CONTACT WITH tmpContact
  ENDIF
  IF TYPE("c_name") = "C"
    c_name = tmpName
  ENDIF
  IF TYPE("p_face") = "C"
    p_face = tmpContact
  ENDIF
  IF TYPE("p_no") = "C"
    p_no = tmpSeries
  ENDIF
  IF TYPE("p_ser") = "C"
    p_ser = tmpNumber
  ENDIF
  IF TYPE("p_line1") = "C"
    p_line1 = tmpLine1
  ENDIF
  IF TYPE("p_line2") = "C"
    p_line2 = tmpLine2
  ENDIF
  IF TYPE("p_line3") = "C"
    p_line3 = tmpLine3
  ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
SELECT TMP_INFO
USE
SELECT TMP_PASP
USE
SELECT CUS_PASP
USE
SELECT (s_sav)
RELEASE WINDOW (win_name)
RETURN
