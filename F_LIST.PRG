*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                            Список филиалов.                            ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 03.06.98 17:48:46 ═╝
PROCEDURE F_List

DELETE FILE (tmpo_path+"F_TMP.DBF")
DELETE FILE (tmpo_path+"F_TMP.CDX")
USE (base_path+"CUSTOMER")
COPY FIELDS CODE, B_NAME TO (tmpo_path+"F_TMP.DBF") FOR RYN # " "
USE (tmpo_path+"F_TMP.DBF") EXCLUSIVE
INDEX ON SYS(15, lwr, B_NAME) TAG NAME
GO TOP
IF BOF() .OR. EOF()
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет ни одного филиала!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  DELETE FILE (tmpo_path+"F_TMP.DBF")
  DELETE FILE (tmpo_path+"F_TMP.CDX")
  RETURN
ENDIF
DO Use_Dummy
SELECT F_TMP

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
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exist OF (menu_name) PROMPT "Наличие"
ON PAD Exist OF (menu_name) ACTIVATE POPUP Exist

  DEFINE POPUP Exist MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Exist PROMPT "Печать" KEY Ctrl-A, "Ctrl-P"
  ON SELECTION BAR 1 OF Exist DO Sw_Mode WITH "Print"

  DEFINE BAR  2 OF Exist PROMPT "Передача" KEY Ctrl-A, "Ctrl-Q"
  ON SELECTION BAR 2 OF Exist DO Sw_Mode WITH "Send"

DEFINE PAD Move OF (menu_name) PROMPT "Накладные" KEY Ctrl-A, "Enter"
ON SELECTION PAD Move OF (menu_name) DO Sw_Mode WITH "Move"

DEFINE PAD Docs OF (menu_name) PROMPT "Движение" KEY Ctrl-A, "Ctrl-Enter"
ON SELECTION PAD Docs OF (menu_name) DO Sw_Mode WITH "Docs"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd, f_name, path_w
ln = MIN(100, RECCOUNT())   && Количество видимых строк BROWSE
wd = FSIZE("B_NAME")

DO D_Wins WITH ln, wd, "Филиалы", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1

    ON KEY LABEL Ctrl+P DO Sw_Mode WITH "Print"
    ON KEY LABEL Ctrl+Q DO Sw_Mode WITH "Send"
    ON KEY LABEL Ctrl+Enter DO Sw_Mode WITH "Docs"
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE FIELDS B_NAME:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Move")
    ENDIF

  CASE what_do = "Move"    && Накладные

    DO Doc_List WITH CODE
    what_do = "List"

  CASE what_do = "Docs"    && Движение

    DO Send_Doc WITH CODE
    what_do = "List"

  CASE what_do = "Print"   && Печать

    DO P_Sto_F WITH CODE
    what_do = "List"

  CASE what_do = "Send"    && Передача

*    f_name = S_List(CODE)
    DO Sto_S WITH CODE
    IF .F.  && TYPE("f_name") = "C"
      path_w = ""
      path_w = Int_Dir(path_w, "Укажите путь", 0)
      DELETE FILE (path_w+f_name+"_T.DBF")
      DELETE FILE (path_w+f_name+".DBF")
      DELETE FILE (path_w+f_name+".FPT")
      COPY FILE (tmpo_path+f_name+"_T.DBF") TO (path_w+"RYN_L_T.DBF")
      COPY FILE (tmpo_path+f_name+".DBF") TO (path_w+"RYN_L.DBF")
      COPY FILE (tmpo_path+f_name+".FPT") TO (path_w+"RYN_L.FPT")
      DELETE FILE (path_w+f_name+"_T.DBF")
      DELETE FILE (path_w+f_name+".DBF")
      DELETE FILE (path_w+f_name+".FPT")
    ENDIF

    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
CLOSE DATABASES
DELETE FILE (tmpo_path+"F_TMP.DBF")
DELETE FILE (tmpo_path+"F_TMP.CDX")
RELEASE MENU (menu_name) EXTENDED

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║              Накладные передачи на реализацию для филиала              ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 03.06.98 17:48:46 ═╝
PROCEDURE Doc_List
PARAMETER c_code

PRIVATE s_sav

s_sav = SELECT()
SELECT 0

DELETE FILE (tmpo_path+"F_MOVE.DBF")
DELETE FILE (tmpo_path+"F_MOVE.CDX")
USE (base_path+"RYN_M_T") ORDER TAG CUS_CODE ALIAS M04698 AGAIN
SEEK c_code
COPY REST WHILE CUS_CODE = c_code FIELDS DOC_NUM, DOC_DATE, ORD_NUM, ORD_DATE TO (tmpo_path+"F_MOVE.DBF")
USE (tmpo_path+"F_MOVE.DBF") EXCLUSIVE
INDEX ON DTOS(DOC_DATE) TAG HRON
GO TOP
IF BOF() .OR. EOF()
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет ни одной накладной для филиала!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  DELETE FILE (tmpo_path+"F_MOVE.DBF")
  DELETE FILE (tmpo_path+"F_MOVE.CDX")
  SELECT (s_sav)
  RETURN
ENDIF
DO Use_Dummy
SELECT F_MOVE

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
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Print OF (menu_name) PROMPT "Печать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Print OF (menu_name) DO Sw_Mode WITH "Print"

DEFINE PAD Send OF (menu_name) PROMPT "Передача" KEY Ctrl-A, "Ctrl-Q"
ON SELECTION PAD Send OF (menu_name) DO Sw_Mode WITH "Send"

DEFINE PAD Boxes OF (menu_name) PROMPT "По коробкам" KEY Ctrl-A, "Tab"
ON SELECTION PAD Boxes OF (menu_name) DO Sw_Mode WITH "Boxes"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd, f_name, path_w
ln = MIN(100, RECCOUNT())   && Количество видимых строк BROWSE
wd = IIF(SET("CENTURY") = "ON", 38, 34)
DO D_Wins WITH ln, wd, "Накладные", 0, 0
@ 1, 2 SAY IIF(SET("CENTURY") = "ON",  ;
        "──Док────Дата───────Заказ──────Дата", ;
        "──Док───Дата──────Заказ─────Дата")
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Ctrl+Q DO Sw_Mode WITH "Send"
    ON KEY LABEL Tab    DO Sw_Mode WITH "Boxes"
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE FIELDS DOC_NUM:5:H="",       ;
                  DOC_DATE:P="@D":H="", ;
                  ORD_NUM:10:H="",      ;
                  ORD_DATE:P="@D":H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F  ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Print")
    ENDIF

  CASE what_do = "Print"   && Печать

    DO P_Inv_F WITH DOC_NUM, DOC_DATE
    what_do = "List"

  CASE what_do = "Boxes"   && По коробкам

    DO RynBoxes WITH DOC_NUM, DOC_DATE
    what_do = "List"

  CASE what_do = "Send"    && Передача

    f_name = S_Inv(DOC_NUM, DOC_DATE)
    IF TYPE("f_name") = "C"
      path_w = ""
      path_w = Int_Dir(path_w, "Укажите путь", 0)
      DELETE FILE (path_w+f_name+"_T.DBF")
      DELETE FILE (path_w+f_name+".DBF")
      DELETE FILE (path_w+f_name+".FPT")
      COPY FILE (tmpo_path+f_name+"_T.DBF") TO (path_w+"RYN_M_T.DBF")
      COPY FILE (tmpo_path+f_name+".DBF") TO (path_w+"RYN_M.DBF")
      COPY FILE (tmpo_path+f_name+".FPT") TO (path_w+"RYN_M.FPT")
      DELETE FILE (path_w+f_name+"_T.DBF")
      DELETE FILE (path_w+f_name+".DBF")
      DELETE FILE (path_w+f_name+".FPT")
    ENDIF
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
SELECT F_MOVE
USE
DELETE FILE (tmpo_path+"F_MOVE.DBF")
DELETE FILE (tmpo_path+"F_MOVE.CDX")
RELEASE MENU (menu_name) EXTENDED
SELECT (s_sav)

RETURN
