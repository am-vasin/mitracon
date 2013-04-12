*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Mark_Ord     Разработчик Андрей Васин           15.03.98 17:11:20 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                     Утверждение заявок продавцом.                      ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Mark_Ord

PRIVATE mss, s_sav

IF .NOT. EMPTY(LIST_TIT.SAL_NUM)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Какие могут быть заявки? Список продан!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

IF EMPTY(LIST_TIT.ORD_NUM)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"По списку не было ни одной заявки!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  POP KEY
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

DEFINE PAD Print OF (menu_name) PROMPT "Печать"
ON PAD Print OF (menu_name) ACTIVATE POPUP Print

  DEFINE POPUP Print MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Print PROMPT "Формат списка" KEY Ctrl-A, "F1"
  ON SELECTION BAR 1 OF Print DO Sw_Mode WITH "Print_1"

  DEFINE BAR  2 OF Print PROMPT "Формат заявки" KEY Ctrl-A, "F2"
  ON SELECTION BAR 2 OF Print DO Sw_Mode WITH "Print_2"

DEFINE PAD Mark OF (menu_name) PROMPT "Утвердить" KEY Ctrl-A, "Enter"
ON SELECTION PAD Mark OF (menu_name) DO Sw_Mode WITH "Mark"

DEFINE PAD Note OF (menu_name) PROMPT "Примечание" KEY Ctrl-A, "Tab"
ON SELECTION PAD Note OF (menu_name) DO Sw_Mode WITH "Note"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
s_sav = SELECT()

SELECT 0
DO Use_Dummy

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS PSL_000 AGAIN

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS PST_000 AGAIN

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS PWK_000 AGAIN

SELECT 0
USE (base_path+"ORD_T") ORDER TAG DOC_NUM ALIAS ORD_999 AGAIN
SET RELATION TO WHO      INTO PSL_000
SET RELATION TO STO_MARK INTO PST_000 ADDITIVE
SET RELATION TO VAL(ALLTRIM(CHECK_S)) INTO PWK_000 ADDITIVE

*
*   Расчет размеров окна
*
PRIVATE ln, wd, tmpVal
ln = 8   && Количество видимых строк BROWSE
wd = 65

IF EMPTY(LIST_TIT.DATE_REQ)
  tmpVal = "    (не собирать)"
ELSE
  tmpVal = "    к "+DTOC(LIST_TIT.DATE_REQ)+" "+LIST_TIT.TIME_REQ
ENDIF
*┌─N────Дата───────Время─────Прод.───────В работе c───────Склад────┐
*│999 ДД.ММ.ГГГГ ЧЧ:ММ:СС ....:....! ДД.ММ.ГГГГ ЧЧ:ММ:СС ....:....!│
*└─────────────────────────────────────────────────────────────────┘
DO D_Wins WITH ln, wd, "Cписок N "+ALLTRIM(STR(LIST_TIT.LST_NUM))+tmpVal, 0, 4
what_do = "List"
@ 1, 2 SAY ;
"─N────Дата───────Время─────Прод.───────В работе c───────Склад"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    SELECT ORD_999
    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL F1    DO Sw_Mode WITH "Print_1"
    ON KEY LABEL F2    DO Sw_Mode WITH "Print_2"
    ON KEY LABEL Tab   DO Sw_Mode WITH "Note"
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE KEY STR(LIST_TIT.LST_NUM, 6) ;
           FIELDS ORD_NUM:3:H="",     ;
                  ORD_DATE:10:H="",   ;
                  ORD_TIME:8:H="",    ;
                  Sale_Stat  = IIF(EMPTY(WHO), " ", "Утверждена"):10:H="", ;
                  In_Work = IIF(EMPTY(START_DATE), " ", DTOC(START_DATE)+" "+START_TIME):19:H="", ;
                  Stock_Stat = IIF(.NOT. EMPTY(STO_MARK), "Утверждена", " "):10:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN V_Foot() ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Mark")
    ENDIF

  CASE what_do = "Mark"       && Утвердить

    DO P_D
    what_do = "List"

  CASE what_do = "Note"       && Примечание

    DO Modi_Nt
    what_do = "List"

  CASE what_do = "Print_1"    && Печать в формате списка

    DO Prn_Ord1 WITH VAL(ALLTRIM(LIST_NUM)), ORD_NUM
    what_do = "List"

  CASE what_do = "Print_2"    && Печать в формате заявки

    IF new_ord
      DO Prn_OrdN WITH VAL(ALLTRIM(LIST_NUM)), ORD_NUM
    ELSE
      DO Prn_Ord WITH VAL(ALLTRIM(LIST_NUM)), ORD_NUM
    ENDIF
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

SELECT PSL_000
USE

SELECT PST_000
USE

SELECT PWK_000
USE

SELECT ORD_999

PRIVATE m_sl, m_st
STORE .T. TO m_sl, m_st
SEEK STR(LIST_TIT.LST_NUM, 6)
SCAN REST WHILE LIST_NUM = STR(LIST_TIT.LST_NUM, 6)
  IF EMPTY(WHO)
    m_sl = .F.
  ENDIF
  IF EMPTY(STO_MARK)
    m_st = .F.
  ENDIF
ENDSCAN
USE
SELECT LIST_TIT
REPLACE LIST_TIT.NOTE_STO WITH IIF(m_st, " ", "*"), ;
        LIST_TIT.NOTE_SAL WITH IIF(m_sl, " ", "*")

SELECT (s_sav)
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                          Утверждение заявки.                           ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 15.03.98 17:31:53 ═╝
PROCEDURE P_D

PRIVATE mss
SELECT ORD_999
IF EMPTY(WHO)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"  Утверждаем? Точно? [ Да ] [ Нет ]"
  mss(3) = ""
  IF Out_Mess(5, "mss") = 1
    REPLACE WHO WITH user
    RETURN .T.
  ENDIF
ELSE
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Так заявка уже утверждена!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
ENDIF

RETURN .F.

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                    Программа вывода доп. информации.                  ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE V_Foot
PRIVATE vl1, vl2, vl3, vl4

ACTIVATE WINDOW (win_name+"_M") SAME
IF .NOT. EMPTY(WHO)
  vl1 = ALLTRIM(PSL_000.FAMILY)+" "+LEFT(PSL_000.NAME,1)+". "+LEFT(PSL_000.S_NAME,1)+"."
ELSE
  vl1 = ""
ENDIF

IF .NOT. EMPTY(CHECK_S)
  vl2 = ALLTRIM(PWK_000.FAMILY)+" "+LEFT(PWK_000.NAME,1)+". "+LEFT(PWK_000.S_NAME,1)+"."
ELSE
  vl2 = ""
ENDIF

IF .NOT. EMPTY(STO_MARK)
  vl3 = ALLTRIM(PST_000.FAMILY)+" "+LEFT(PST_000.NAME,1)+". "+LEFT(PST_000.S_NAME,1)+"."+ ;
        +"   "+DTOC(TERM_DATE)+" "+TERM_TIME
ELSE
  vl3 = ""
ENDIF

vl1 = "  Продавец   "+PADR(vl1, 50)
vl2 = "  Собирает   "+PADR(vl2, 50)
vl3 = "  Утвердил   "+PADR(vl3, 50)
vl4 = "  Примечание "+NOTE

@ WROWS()-5, 2 SAY vl1
@ WROWS()-4, 2 SAY vl2
@ WROWS()-3, 2 SAY vl3
@ WROWS()-2, 2 SAY vl4

ACTIVATE WINDOW (ALIAS())

RETURN .T.
