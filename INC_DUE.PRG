*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Inc_Due      Разработчик Андрей Васин           14.05.97 09:55:35 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                 Программа обработки долга при приходе.                 ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Inc_Due
PRIVATE sel_sav, s_q

sel_sav = SELECT()
s_q = 0                && Заготовка для суммарной брони
IF .NOT. Prep_Due()    && Подготовка долга для позиции...
  SELECT (sel_sav)
  RETURN               &&  ...а нет никакого долга!
ENDIF

SELECT 0
USE (m.base_path+"LINK_ORD") SHARED AGAIN ALIAS LO_DUE ORDER TAG CODE
SELECT INC_DUE
SET RELATION TO CODE INTO LO_DUE

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
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD View OF (menu_name) PROMPT "Заказы менеджеров" KEY Ctrl-A, "Tab"
ON SELECTION PAD View OF (menu_name) Do Sw_Mode WITH "view"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = 10  && Количество видимых строк BROWSE
wd = FSIZE("ARM_N")+FSIZE("CUS_NAME")+;
     FSIZE("EXP_LIST")+FSIZE("QNT_DUE")+FSIZE("QNT")+6

DO D_Wins WITH ln, wd, "Долг: "+ALLTRIM(STOCK.PREFIX)+" "+  ;
                       ALLTRIM(STOCK.NAME)+" "+ALLTRIM(STOCK.PRODUCER), 0, 1

@ 1, 2 SAY PADC("Раб. место───",FSIZE("ARM_N")+1,"─")+     ;
           PADC("Список",FSIZE("EXP_LIST")+1,"─")+  ;
           PADC("Клиент───────",FSIZE("CUS_NAME")+1,"─")+  ;
           PADC("Долг",FSIZE("QNT_DUE")+1,"─")+   ;
           PADC("Бронь",FSIZE("QNT"),"─")+"─М"
@ WROWS()-2,  3 SAY "Имеем:"
@ WROWS()-2, 10 SAY STR(STOCK.QNT+INC_LIST.QNT,6) COLOR SCHEME 1
@ WROWS()-2, 20 SAY "Распределили:"
@ WROWS()-2, 34 SAY STR(s_q,6) COLOR SCHEME 1

what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Tab Do Sw_Mode WITH "view"

*
*    BROWSE - меню
*
    BROWSE KEY key_v+STR(STOCK.CODE,7)  ;
           FIELDS ARM_N:H="", EXP_LIST:H="", CUS_NAME:H="", QNT_DUE:H="", QNT:H="":V=Sh_Q(),  ;
                  f000 = IIF(.NOT. EMPTY(LO_DUE.CODE), "√", " "):H= "" ;
           FREEZE QNT ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "List")
    ENDIF
    ON KEY

  CASE what_do = "view" && Заказы на поставку от менеджеров

    DO Pos_Ord2 WITH CODE
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
USE IN LO_DUE

SELECT (sel_sav)

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                     Подготовка долга для позиции.                      ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 14.05.97 09:57:44 ═╝
PROCEDURE Prep_Due

PRIVATE mss, l_nam, arr_w, rc_sav

IF .NOT. FOUND("LIST_DET")
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Так нет никакого долга по этой позиции."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

SELECT INC_DUE
SCATTER TO arr_w BLANK

SELECT 0
USE LIST_TIT ORDER TAG LST_NUM

SELECT 0
USE ARM_LST ORDER TAG ARM_C

SELECT LIST_DET
SCAN REST WHILE CODE = STOCK.CODE
  = SEEK(VAL(ALLTRIM(LIST_NUM)), "LIST_TIT")
  IF LIST_TIT.LST_ARM = "***"
    l_nam = "Общая область"
  ELSE
    = SEEK(LIST_TIT.LST_ARM, "ARM_LST")
    l_nam = ARM_LST.ARM_N
  ENDIF
  IF .NOT. SEEK(key_v+STR(CODE,7)+LIST_NUM, "INC_DUE")
    *
    *   Пытаемся захватить удаленную запись
    SET DELETED OFF
    SELECT INC_DUE
    SET ORDER TO TAG DEL
    GO TOP
    IF DELETED()
      RECALL
      GATHER FROM arr_w
    ELSE
      APPEND BLANK
    ENDIF
    SET DELETED ON
    SET ORDER TO TAG L_CODE
  ENDIF
  SELECT INC_DUE
  REPLACE INC_LIST WITH key_v,             ;
          EXP_LIST WITH LIST_DET.LIST_NUM, ;
          CUS_NAME WITH LIST_TIT.LST_NAME, ;
          ARM_N    WITH l_nam,             ;
          CODE     WITH STOCK.CODE,        ;
          QNT_DUE  WITH LIST_DET.QNT_DUE
  SELECT LIST_DET
ENDSCAN

SELECT LIST_TIT
USE

SELECT ARM_LST
USE

SELECT LIST_DET
SET ORDER TO TAG L_CODE
SELECT INC_DUE
SEEK key_v+STR(STOCK.CODE,7)
SCAN REST WHILE INC_LIST = key_v .AND. CODE = STOCK.CODE
  IF .NOT. SEEK(EXP_LIST+STR(CODE,7), "LIST_DET")
    DELETE
    LOOP
  ENDIF
  IF LIST_DET.QNT_DUE <= 0
    DELETE
    LOOP
  ENDIF
  REPLACE QNT_DUE WITH LIST_DET.QNT_DUE
  s_q = s_q+QNT
ENDSCAN
SELECT LIST_DET
SET ORDER TO TAG DUE_CODE

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                          Выход из поля брони.                          ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 14.05.97 12:48:42 ═╝
PROCEDURE Sh_Q

PRIVATE mss, rc_sav

IF QNT > QNT_DUE
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Мы ему столько не должны!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

rc_sav = RECNO()
SEEK key_v+STR(STOCK.CODE,7)
SUM REST WHILE key_v = INC_LIST .AND. STOCK.CODE = CODE QNT TO s_q
GO rc_sav
IF s_q > STOCK.QNT+INC_LIST.QNT
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"У нас столько нет!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

ACTIVATE WINDOW (win_name+"_M") SAME
@ WROWS()-2, 34 SAY STR(s_q,6) COLOR SCHEME 1
ACTIVATE WINDOW (ALIAS())

RETURN .T.
