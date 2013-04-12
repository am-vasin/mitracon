*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл New_Sup      Разработчик Андрей Васин           06.07.97 11:32:05 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                  Новая программа создания поставщика.                  ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE New_Sup

*
*  Переменные состояния для навигации
*
PRIVATE tmpType

PRIVATE stat_type     && Тип навигации: 0 - нестандартный;
                                        1 - бланк;
                                        2 - BROWSE - таблица;
                                        3 - BROWSE - список.
PRIVATE what_do       && Имя режима.
PRIVATE menu_name     && Имя асинхронного меню.
PRIVATE last_mouse    && Время последнего нажатия звериной кнопки.
PRIVATE win_name      && Имя окна ( окон для BROWSE ).
PRIVATE tmpINN, tmpKPP
m.tmpINN = ""
m.tmpKPP = ""
DO Get_INN WITH 0, m.tmpINN, m.tmpKPP

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

*┌──────────────────────────────────────────────────────┐
*│    ( ) Совсем новый   ( ) Известен как покупатель    │
*│┌─ Имя поставщика ───────────────────────────────────┐│
*││ ....:....!....:....!....:....!....:....!....:....! ││
*│└────────────────────────────────────────────────────┘│
*│       ИНН ....:....!..    КПП ....:....!....:.       │
*│           < OK Ctrl-W > < Отказаться Esc >           │
*└──────────────────────────────────────────────────────┘

USE (base_path+"CLI_INFO") ORDER TAG CUS_CODE ALIAS C_3326 SHARED AGAIN
SELECT 0
USE (base_path+"CUSTOMER") ORDER TAG CODE
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_NAME
DO Use_Dummy
DO Use_Link
PRIVATE ex, nm_w, is_new, cntr
nm_w   = SPACE(FSIZE("CUS_NAME","ACCOUNT"))
is_new = 1
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 9, 58, "Поставщик"
DO Sun_Bord WITH 3, 2, 5, 55, " Имя поставщика "
*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 2,  7 GET is_new PICTURE "@*RH Совсем новый  ;Известен как покупатель"  ;
        VALID Get_Cus()
@ 4,  4 GET nm_w WHEN is_new = 1
@ 6,  9 SAY "ИНН" GET m.tmpINN
@ 6, 29 SAY "КПП" GET m.tmpKPP
@ 7, 13 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID ex # 1 .OR. EMPTY(nm_w) .OR. ;
                 Tst_INN(IIF(is_new = 1, 0, ACCOUNT.CUS_CODE), m.tmpINN)

IF ex = 1 .AND. .NOT. EMPTY(nm_w)
  IF is_new = 1
    cntr = SPACE(16)
    SELECT ACCOUNT
    IF (sys_char = "A" .OR. sale_mode = "Mitin")
      tmpType = WhatFace()
      ex = Doc_Num("CUS_CODE", "", "", DATE())
      APPEND BLANK
      REPLACE SUBSYS   WITH sys_char, ;
              CUS_CODE WITH ex,   ;
              CUS_NAME WITH nm_w, ;
              DATE_ON  WITH DATE(),;
              MARK1    WITH tmpType,;
              WHO      WITH user, ;
              WHERE    WITH arm
    ELSE
      SET ORDER TO TAG EMP_NAME
      GO TOP
      IF EOF() .OR. BOF()
        PRIVATE mss
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"Вы исчерпали все заготовки!"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        CLOSE DATABASES
        POP KEY
        RELEASE WINDOW (win_name)
        RETURN
      ENDIF
      tmpType = WhatFace()
      REPLACE CUS_NAME WITH nm_w, MARK1 WITH tmpType
    ENDIF
    SELECT C_3326
    APPEND BLANK
    REPLACE CUS_CODE WITH ex,  ;
            NAME     WITH nm_w,;
            COUNTRY  WITH cntr,;
            WHO      WITH user,;
            WHERE    WITH arm
  ENDIF
  DO Put_Inn WITH CUS_CODE, m.tmpINN, m.tmpKPP
  SELECT CUSTOMER
  IF .NOT. SEEK(ACCOUNT.CUS_CODE, "CUSTOMER")
    APPEND BLANK
    REPLACE CODE    WITH ACCOUNT.CUS_CODE,  ;
            B_NAME  WITH ACCOUNT.CUS_NAME,  ;
            L_NAME  WITH ACCOUNT.CUS_NAME,  ;
            DATE_ON WITH ACCOUNT.DATE_ON,   ;
            WHO_ADD WITH ACCOUNT.WHO,       ;
            WHERE_ADD WITH ACCOUNT.WHERE
  ENDIF
  REPLACE SUP WITH "*"
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
CLOSE DATABASES
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                         Выбор среди клиентов.                          ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 06.07.97 11:50:35 ═╝
PROCEDURE Get_Cus
PRIVATE mss, pl

IF is_new = 1
  RETURN .T.
ENDIF
SELECT ACCOUNT
pl = sys_char
IF .NOT. Custs("", .F., .F., .F., pl)
  is_new = 1
  SHOW GET is_new
  RETURN .T.
ENDIF

IF SEEK(ACCOUNT.CUS_CODE, "CUSTOMER") .AND. CUSTOMER.SUP # " "
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Этот клиент уже помечен как поставщик!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  is_new = 1
  SHOW GET is_new
  RETURN .T.
ENDIF

DO Get_INN WITH ACCOUNT.CUS_CODE, m.tmpINN, m.tmpKPP
nm_w = ACCOUNT.CUS_NAME
SHOW GET nm_w
SHOW GET tmpINN
SHOW GET tmpKPP


RETURN .T.
