*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Rep_Sto      Разработчик Андрей Васин           13.06.97 21:08:11 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                  Справки по позициям номенклатуры.                     ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Rep_Sto
PARAMETER modi, rep_sale, for_del
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
PRIVATE mss

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

IF modi
  DEFINE PAD Modi OF (menu_name) PROMPT "Коррекция"
  ON PAD Modi OF (menu_name) ACTIVATE POPUP Modi

    DEFINE POPUP Modi MARGIN RELATIVE SHADOW COLOR SCHEME 4

    IF for_del

      DEFINE BAR  1 OF Modi PROMPT "Восстановить" KEY Ctrl-A, "Ctrl-Enter"
      ON SELECTION BAR 1 OF Modi DO Sw_Mode WITH "Modify"

    ELSE

      DEFINE BAR  1 OF Modi PROMPT "Корректировать" KEY Ctrl-A, "Ctrl-Enter"
      ON SELECTION BAR 1 OF Modi DO Sw_Mode WITH "Modify"

      DEFINE BAR  2 OF Modi PROMPT "Корпус, сегмент..." KEY Ctrl-A, "F5"
      ON SELECTION BAR 2 OF Modi DO Sw_Mode WITH "Corp"

      DEFINE BAR  3 OF Modi PROMPT "Добавить" KEY Ctrl-A, "F4"
      ON SELECTION BAR 3 OF Modi DO Sw_Mode WITH "Add"

      DEFINE BAR  4 OF Modi PROMPT "Удалить" KEY Ctrl-A, "Del"
      ON SELECTION BAR 4 OF Modi DO Sw_Mode WITH "Del_P"

  ENDIF
ENDIF

IF rep_sale
  DEFINE PAD Prn_Docs OF (menu_name) PROMPT "Продажи" KEY Ctrl-A, "Shift+F2"
  ON SELECTION PAD Prn_Docs OF (menu_name) DO Sw_Mode WITH "V_Pos"
ENDIF

DEFINE PAD Move OF (menu_name) PROMPT "Движение" KEY Ctrl-A, "Tab"
ON SELECTION PAD Move OF (menu_name) DO Sw_Mode WITH "Move_S"

DEFINE PAD Qnt OF (menu_name) PROMPT "Наличие"
ON PAD Qnt OF (menu_name) ACTIVATE POPUP Qnt

  DEFINE POPUP Qnt MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Qnt PROMPT "Бронь" KEY Ctrl-A, "F1"
  ON SELECTION BAR 1 OF Qnt DO Sw_Mode WITH "Br&Fil0"

  DEFINE BAR  2 OF Qnt PROMPT "Реализация" KEY Ctrl-A, "Shift-F1"
  ON SELECTION BAR 2 OF Qnt DO Sw_Mode WITH "Br&Fil1"

  DEFINE BAR  3 OF Qnt PROMPT "Спецфилиалы" KEY Ctrl-A, "F2"
  ON SELECTION BAR 3 OF Qnt DO Sw_Mode WITH "Spec_F"

  DEFINE BAR  4 OF Qnt PROMPT "Партии" KEY Ctrl-A, "Ctrl-F1"
  ON SELECTION BAR 4 OF Qnt DO Sw_Mode WITH "Incs"

  IF .NOT. modi
    DEFINE BAR  5 OF Qnt PROMPT "Корпус, сегмент..." KEY Ctrl-A, "F5"
    ON SELECTION BAR 5 OF Qnt DO Sw_Mode WITH "Corp"
  ENDIF

  DEFINE BAR  6 OF Qnt PROMPT "Заказы на поставку" KEY Ctrl-A, "F9"
  ON SELECTION BAR 6 OF Qnt DO Sw_Mode WITH "Sup_Ord"

  DEFINE BAR  7 OF Qnt PROMPT "Заказы от менеджеров" KEY Ctrl-A, "Ctrl-F9"
  ON SELECTION BAR 7 OF Qnt DO Sw_Mode WITH "Man_Ord"

  DEFINE BAR  8 OF Qnt PROMPT "Отчеты перед поставщиком" KEY Ctrl-A, ""
  ON SELECTION BAR 8 OF Qnt DO Sw_Mode WITH "Prn_Rep"

  IF Is_Acc(1)
    DEFINE BAR  9 OF Qnt PROMPT "Закупочные цены"
    ON SELECTION BAR 9 OF Qnt DO Sw_Mode WITH "bPrice"
  ENDIF

  DEFINE BAR 10 OF Qnt PROMPT "Заявки на склад" KEY Ctrl-A, ""
  ON SELECTION BAR 10 OF Qnt DO Sw_Mode WITH "Ord_Sto"

  DEFINE BAR 11 OF Qnt PROMPT "Наличие в ДМС" KEY Ctrl-A, ""
  ON SELECTION BAR 11 OF Qnt DO Sw_Mode WITH "Rest_DMS"

IF .NOT. for_del
  DEFINE PAD Addr OF (menu_name) PROMPT "Адрес" KEY Ctrl-A, "F3"
  ON SELECTION PAD Addr OF (menu_name) Do Sw_Mode WITH "Addr"
ENDIF

DEFINE PAD Seek OF (menu_name) PROMPT "Поиск"
ON PAD Seek OF (menu_name) ACTIVATE POPUP Seek

  DEFINE POPUP Seek MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Seek PROMPT "Контекстный поиск" KEY Ctrl-A, "F7"
  ON SELECTION BAR 1 OF Seek DO Sw_Mode WITH "Seek_C"

  DEFINE BAR  11 OF Seek PROMPT "Поиск по числ. части" && KEY Ctrl-A, "F7"
  ON SELECTION BAR 11 OF Seek DO Sw_Mode WITH "Seek_Dg"

  DEFINE BAR  10 OF Seek PROMPT "Повтор поиска" KEY Ctrl-A, "Ctrl-F7"
  ON SELECTION BAR 10 OF Seek DO Sw_Mode WITH "Seek_R"

  DEFINE BAR  2 OF Seek PROMPT "Поиск по адресу" KEY Ctrl-A, "F8"
  ON SELECTION BAR 2 OF Seek DO Sw_Mode WITH "Seek_A"

  IF .NOT. for_del
    DEFINE BAR  3 OF Seek PROMPT "Упорядочить по адресу" KEY Ctrl-A, "Ctrl-F2"
    ON SELECTION BAR 3 OF Seek DO Sw_Mode WITH "Set_Ord"

    DEFINE BAR  4 OF Seek PROMPT "Класс"  && KEY Ctrl-A, "Shift-F7"
    ON SELECTION BAR 4 OF Seek DO Sw_Mode WITH "Class"

    DEFINE BAR  5 OF Seek PROMPT "Все классы"  && KEY Ctrl-A, "Ctrl-F8"
    ON SELECTION BAR 5 OF Seek DO Sw_Mode WITH "AllClass"
  ENDIF

IF rep_sale
  DEFINE PAD Prc OF (menu_name) PROMPT "Цены" KEY Ctrl-A, "F6"
  ON SELECTION PAD Prc OF (menu_name) DO Sw_Mode WITH "Prc"
ENDIF

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
DO Use_Dummy

SELECT 0
DO Use_DMS
SET ORDER TO TAG CODE

SELECT 0
USE (base_path+"DIVISION") ORDER TAG CODE

SELECT 0
USE (path_comm+"SUBJECT") ORDER TAG STO_CODE ALIAS SBJ_4404

SELECT 0
IF for_del
  USE (base_path+"STOCK") ORDER TAG DELETED
ELSE
  USE (base_path+"STOCK") ORDER TAG NAME_P
ENDIF
SET RELATION TO DIV_CODE INTO DIVISION, ;
                CODE     INTO SBJ_4404
*                PADR(ALLTRIM(STR(CODE)), FSIZE("CODE", "DMS")) INTO DMS

*
*   Расчет размеров окна
*
PRIVATE ln, wd, interr1, interr2, let_div, pic_w, r_sav
PRIVATE keyClass, className
className = "Все классы"
keyClass  = ""
let_div = "L"
pic_w = Addr_Pic()
DIMENSION interr1(10), interr2(10)
interr1(1) = -2
interr2(1) = "{F3}"
interr1(2) = -6
interr2(2) = "{F7}"
interr1(3) = 28
interr2(3) = "{F1}"
interr1(4) = -94		&& off
interr2(4) = "{Ctrl+F1}"
interr1(5) = 9
interr2(5) = "{Tab}"
interr1(6) = 15
interr2(6) = "{Shift+Tab}"
interr1(7) = -8
interr2(7) = "{F9}"
interr1(8) = -1
interr2(8) = "{F2}"
interr1(9) = -7
interr2(9) = "{F8}"
interr1(10) = 4
interr2(10) = "{Ctrl+D}"

IF .NOT. for_del
  DIMENSION interr1(12), interr2(12)
  interr1(10) = -101		&& off
  interr2(10) = "{Ctrl+F8}"
  interr1(11) = -95		&& off
  interr2(11) = "{Ctrl+F2}"
  interr1(12) = -90		&& off
  interr2(12) = "{Shift+F7}"

  IF modi
    DIMENSION interr1(15), interr2(15)
    interr1(13) = 10
    interr2(13) = "{Ctrl+Enter}"
    interr1(14) = -3
    interr2(14) = "{F4}"
    interr1(15) = -4
    interr2(15) = "{F5}"
  ENDIF
ENDIF

ln = MIN(100, RECCOUNT())   && Количество видимых строк BROWSE
wd = 76-IIF(for_del, 16, 0)

DO D_Wins WITH ln, wd, "Номенклатура", IIF(for_del, 1, 2), 3
@ 1, 9 SAY "Поиск"
@ 1, 15 SAY PADR("Enter",25) COLOR (SCHEME(14,2))
IF for_del
  @ 2,  3 SAY "Адрес"
  @ 2, 14 SAY "Наименование"
  @ 2, 51 SAY "Удалена"
ELSE
  @ 2,  2 SAY "Класс"
  @ 3,  3 SAY "Адрес"
  @ 3, 14 SAY "Наименование"
  @ 3, 51 SAY "Кол-во"
  @ 3, 59 SAY "Бронь"
  @ 3, 67 SAY "Фил."
  @ 3, 72 SAY "Спец."
ENDIF
@ WROWS()-5,  3 SAY "Раздел"
@ WROWS()-5, 22 SAY "Цена"
@ WROWS()-5, 34 SAY "Упак."
IF for_del
  @ WROWS()-5, 41 SAY "Доп. адрес"
ELSE
  @ WROWS()-5, 41 SAY "Реал."
  @ WROWS()-5, 47 SAY "Доп. адрес"
  @ WROWS()-5, 63 SAY "Доп. н."
  @ WROWS()-5, 71 SAY "  ДМС  "
ENDIF
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    ACTIVATE WINDOW (win_name+"_M") SAME
    IF .NOT. for_del
      @ 2, 8 SAY PADR(className, 64) COLOR SCHEME 1
    ENDIF
    DO Prp_Nav_1
    ON KEY LABEL F3    DO Sw_Mode WITH "Addr"
    ON KEY LABEL F7    DO Sw_Mode WITH "Seek_C"
    ON KEY LABEL Ctrl-F7 DO Sw_Mode WITH "Seek_R"
    ON KEY LABEL F1    DO Sw_Mode WITH "Br&Fil0"
    ON KEY LABEL Shift+F1 DO Sw_Mode WITH "Br&Fil1"
    ON KEY LABEL Ctrl+D DO Sw_Mode WITH "Rest_DMS"
    ON KEY LABEL F2    DO Sw_Mode WITH "Spec_F"
*    ON KEY LABEL F9    DO Sw_Mode WITH "For_MVS"
    ON KEY LABEL F9    DO Sw_Mode WITH "Sup_Ord"
    ON KEY LABEL Ctrl+F9 DO Sw_Mode WITH "Man_Ord"
    ON KEY LABEL Ctrl+F1 DO Sw_Mode WITH "Incs"
    ON KEY LABEL Tab   DO Sw_Mode WITH "Move_S"
    ON KEY LABEL Enter DO Seek_P
    ON KEY LABEL F8    DO Sw_Mode WITH "Seek_A"
    IF .NOT. for_del
      ON KEY LABEL Ctrl+F2 DO Sw_Mode WITH "Set_Ord"
      ON KEY LABEL Shift+F7 DO Sw_Mode WITH "Class"
      ON KEY LABEL Ctrl+F8 DO Sw_Mode WITH "AllClass"
    ENDIF
    ON KEY LABEL F5         DO Sw_Mode WITH "Corp"
    IF modi 
      ON KEY LABEL Ctrl+Enter DO Sw_Mode WITH "Modify"
      IF .NOT. for_del
        ON KEY LABEL F4         DO Sw_Mode WITH "Add"
        ON KEY LABEL Del        DO Sw_Mode WITH "Del_P"
      ENDIF
    ENDIF
    IF rep_sale
      ON KEY LABEL Shift+F2 DO Sw_Mode WITH "V_Pos"
      ON KEY LABEL F6 DO Sw_Mode WITH "Prc"
    ENDIF
*
*    BROWSE - меню
*
   IF for_del
     BROWSE FIELDS STOCK.ADDRESS:H="":P=pic_w,   ;
                    STOCK.PREFIX:H="",  ;
                    Brw_Name=Nm_Part(STOCK.NAME):25:H="",;
                    STOCK.PRODUCER:H="",  ;
                    STOCK.DATE_OFF:H="",  ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             WHEN Show_F() ;
             NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
   ELSE
     IF EMPTY(keyClass)
       BROWSE REST ;
               FIELDS STOCK.ADDRESS:H="":P=pic_w,   ;
                      STOCK.PREFIX:H="",  ;
                      Brw_Name=Nm_Part(STOCK.NAME):25:H="",;
                      STOCK.PRODUCER:H="",  ;
                      STOCK.QNT:H="":P="@Z 999999",      ;
                      BRON = STOCK.QNT_SAVE+STOCK.QNT_REZ:H="":P="@Z 999999", ;
                      STOCK.QNT_REAL:H="":P="@Z 999999", ;
                      STOCK.QNT_SPEC:H="":P="@Z 999999", ;
               DUMMY.F:H="" FREEZE DUMMY.F ;
               WHEN Show_F() ;
               NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
               WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
      ELSE
       BROWSE KEY keyClass REST ;
               FIELDS STOCK.ADDRESS:H="":P=pic_w,   ;
                      STOCK.PREFIX:H="",  ;
                      Brw_Name=Nm_Part(STOCK.NAME):25:H="",;
                      STOCK.PRODUCER:H="",  ;
                      STOCK.QNT:H="":P="@Z 999999",      ;
                      BRON = STOCK.QNT_SAVE+STOCK.QNT_REZ:H="":P="@Z 999999", ;
                      STOCK.QNT_REAL:H="":P="@Z 999999", ;
                      STOCK.QNT_SPEC:H="":P="@Z 999999", ;
               DUMMY.F:H="" FREEZE DUMMY.F ;
               WHEN Show_F() ;
               NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
               WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
      ENDIF
    ENDIF
    ON KEY
    IF what_do = "List"
      what_do = "Exit"
    ENDIF

  CASE what_do = "Addr"    && Коррекция адресов

    DO Addr
    what_do = "List"

  CASE what_do = "Ord_Sto" && Заявки на склад

    DO Ord_Sto WITH CODE
    what_do = "List"

  CASE what_do = "Rest_DMS" && Остатки ДМС

    DO Rest_DMS WITH CODE
    what_do = "List"

  CASE what_do = "V_Pos"   && Печать истории продаж

    DO V_Pos WITH CODE
    what_do = "List"

  CASE what_do = "Prn_Rep" && Отчеты перед поставщиком

    DO Prn_Rep WITH CODE
    what_do = "List"

  CASE what_do = "Seek_C"  && Контекстный поиск

    DO Seek_C
    what_do = "List"

  CASE what_do = "Seek_R"  && Повтор поиска

    DO Seek_C WITH .T.
    what_do = "List"

  CASE what_do = "Seek_A"  && Поиск по адресу

    DO Seek_A
    what_do = "List"

  CASE what_do = "Class"   && Выбор класса

    DO Ch_Class WITH keyClass, ClassName
    SET ORDER TO TAG CLASS
    GO TOP
    what_do = "List"

  CASE what_do = "AllClass"   && Все классы

    SET ORDER TO TAG NAME_P
    keyClass = ""
    className = "Все классы"
    GO TOP
    what_do = "List"

  CASE what_do = "Set_Ord" && Упорядочить по...

    IF "ADDRESS" $ SET("ORDER")
      SET ORDER TO TAG NAME_P
      DEFINE BAR  3 OF Seek PROMPT "Упорядочить по адресу" KEY Ctrl-A, "Ctrl-F2"
    ELSE
      SET ORDER TO ADDRESS
      DEFINE BAR  3 OF Seek PROMPT "Упорядочить по наименованию" KEY Ctrl-A, "Ctrl-F2"
    ENDIF
    what_do = "List"
  
  CASE what_do = "Br&Fil0"  && Наличие: бронь

    DO View_Br WITH CODE
*    DO Br_Fil WITH 0
    what_do = "List"

  CASE what_do = "Br&Fil1"  && Наличие: филиалы

    DO Br_Fil WITH 1
    what_do = "List"

  CASE what_do = "Modify"   && Коррекция позиции

    IF for_del
      REPLACE DATE_OFF WITH {""}
    ELSE
      IF CODE < 9000000
        DIMENSION mss[4]
        mss[1] = ""
        mss[2] = CHR(0)+"ВНИМАНИЕ! Ваша коррекция будет отменена при очередной сверке номенклатуры"
        mss[3] = CHR(0)+"[ Будем корректировать ] [ Откажемся ]"
        mss[4] = ""
        IF Out_Mess(5, "mss") = 2
          what_do = "List"
          LOOP
        ENDIF
      ENDIF
      DO Modi_Pos WITH CODE
    ENDIF
    what_do = "List"

  CASE what_do = "Corp"     && Корпус, сегмент, функц. назначение

    IF CODE < 9000000 .AND. modi
      DIMENSION mss[4]
      mss[1] = ""
      mss[2] = CHR(0)+"ВНИМАНИЕ! Ваша коррекция будет отменена при очередной сверке номенклатуры"
      mss[3] = CHR(0)+"[ Будем корректировать ] [ Откажемся ]"
      mss[4] = ""
      IF Out_Mess(5, "mss") = 2
        what_do = "List"
        LOOP
      ENDIF
    ENDIF
    DO Sto_Add WITH CODE, .NOT. modi .OR. for_del
    DO Show_F
    what_do = "List"

  CASE what_do = "Add"      && Добавление позиции

    DO Modi_Pos WITH 0
    what_do = "List"

  CASE what_do = "Del_P"    && Удаление позиции

    DO Del_P
    what_do = "List"

  CASE what_do = "Spec_F"   && Наличие: спецфилиалы

    DO Br_Fil WITH 2
    what_do = "List"

  CASE what_do = "Seek_Dg"  && Поиск по числ. части

    what_do = Get_Shab("", "Поиск по цифровой части")
    SELECT STOCK
    IF .NOT. EMPTY(what_do)
      what_do = Seek_Dig(what_do)
      IF .NOT. EMPTY(what_do)
        GO what_do
      ENDIF
    ENDIF
    what_do = "List"

  CASE what_do = "For_MVS" && Специально для М В Смагиной

    DO For_MVS
    what_do = "List"

  CASE what_do = "Incs"    && Наличие по партиям

    DO Incs
    what_do = "List"

  CASE what_do = "bPrice"  && Закупочные цены

    DO V_B_Prc WITH CODE
    what_do = "List"

  CASE what_do = "Prc"     && Цены оптовые, мелкооптовые, розничные ...

    DO Rep_Prc WITH CODE
    what_do = "List"

  CASE what_do = "Sup_Ord" && Заказы на поставку

    DO Pos_Ord WITH CODE
    what_do = "List"

  CASE what_do = "Man_Ord" && Заказы на поставку от менеджеров

    DO Pos_Ord2 WITH CODE
    what_do = "List"

  CASE what_do = "Move_S"  && Движение по складу

    DO Move_S WITH STOCK.CODE
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

CLEAR WINDOWS
CLOSE DATABASES
RELEASE MENU (menu_name) EXTENDED

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                           Удаление позиции.                            ║
*║                                                                        ║
*╚═══════════════════════════════════════════════════════════ 28.04.2000 ═╝
PROCEDURE Del_P
PRIVATE mss

IF QNT # 0 .OR. ;
   QNT_REP # 0 .OR. ;
   QNT_REAL # 0 .OR.;
   QNT_SAVE # 0 .OR.;
   QNT_SPEC # 0
     
   DIMENSION mss(3)
   mss(1) = ""
   mss(2) = CHR(0)+"Удаление позиции невозможно ! Позиция есть на складе ! "
   mss(3) = ""
   DO Out_Mess WITH 7, "mss"
   RETURN .F.
 ELSE
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"Позиция "+ALLTRIM(STOCK.PREFIX)+" "+ALLTRIM(STOCK.NAME)+" "+IIF(EMPTY(STOCK.PRODUCER), "", "/"+ALLTRIM(STOCK.PRODUCER)+"/")
  mss(3) = CHR(0)+"Удаляем? [ Да ] [ Нет ]"
  mss(4) = ""
  IF Out_Mess(5, "mss") = 1
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"  Удаляем позицию ? Точно ? [ Да ] [ Нет ]"
    mss(3) = ""
    IF Out_Mess(5, "mss") = 1
      REPLACE DATE_OFF WITH DATE(), ;
              WHO_DEL  WITH user
      DO Corr_Log
      SKIP
      IF EOF()
        GO BOTT
      ENDIF
    ENDIF              
  ENDIF
ENDIF
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                   Запись в журнал переименований.                      ║
*║                                                                        ║
*╚═══════════════════════════════════════════════════════════ 15.03.2002 ═╝
PROCEDURE Corr_Log
PRIVATE m_sav
m_sav = SELECT()

SELECT 0
USE (base_path+"STO_LOG") ALIAS SL_2315 AGAIN
      APPEND BLANK
      REPLACE CODE WITH STOCK.CODE,           ;
              OLD_PRF  WITH STOCK.PREFIX,  ;
              OLD_NAM  WITH STOCK.NAME,    ;
              OLD_PRO  WITH STOCK.PRO_CODE,;
              NEW_NAM  WITH "УДАЛЕНИЕ",  ;
              DAT_CORR WITH DATE(),;
              TIM_CORR WITH TIME(),;
              WHO_CORR WITH user,  ;
              WHERE_C  WITH arm
      USE
SELECT(m_sav)
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                      Поиск по начальным символам.                      ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 10.06.97 10:43:18 ═╝
PROCEDURE Seek_P
PRIVATE prf, s, ln, rc_sav, i

ACTIVATE WINDOW (win_name+"_M") SAME

prf = ""
ln = 25
DO WHILE .T.
  ACTIVATE WINDOW (win_name+"_M") SAME
  @ 1, 15 SAY REPLICATE("▒",25) COLOR SCHEME 1
  @ 1, 15 SAY prf COLOR SCHEME 1
  s = INKEY(0)
  *
  *   Исследуем на предмет наличия обработчика
  *
  i = ASCAN(interr1, s)
  IF i # 0
    KEYBOARD interr2(i)
    EXIT
  ENDIF
  IF s < 0       && Функциональная клавиша
    IF s = -9
      KEYBOARD "{F10}"
      EXIT
    ENDIF
    LOOP
  ENDIF
  IF BETWEEN(s,  32,126) .OR.   ;
     BETWEEN(s, 128,175) .OR.   ;
     BETWEEN(s, 224,241)     && Символ
    IF LEN(prf) >= 25        && Все! Больше некуда!
      LOOP
    ENDIF
    rc_sav = RECNO()
    IF SEEK(keyClass+prf+CHR(s))
      prf = prf+CHR(s)
      SHOW WINDOW (ALIAS()) REFRESH
    ELSE
      ?? CHR(7)
      GO rc_sav
    ENDIF
  ENDIF
  IF s = 127       && Back Space
    IF LEN(prf) > 0
      prf = LEFT(prf, LEN(prf)-1)
      = SEEK(keyClass+prf)
      SHOW WINDOW (ALIAS()) REFRESH
    ENDIF
  ENDIF
  IF s = 13        && Enter
    EXIT
  ENDIF
  IF s = 24        && Down Arrow
    KEYBOARD "{DnArrow}"
    EXIT
  ENDIF
  IF s = 5         && Up Arrow
    KEYBOARD "{UpArrow}"
    EXIT
  ENDIF
  IF s = 3         && Page Down
    KEYBOARD "{PgDn}"
    EXIT
  ENDIF
  IF s = 18        && Page Up
    KEYBOARD "{PgUp}"
    EXIT
  ENDIF
  IF s = 27        && Escape
    KEYBOARD "{Esc}"
    EXIT
  ENDIF
ENDDO

@ 1, 15 SAY PADR("Enter",25) COLOR (SCHEME(14,2))
  
ACTIVATE WINDOW (ALIAS()) SAME

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                      Вывод информационной строки.                      ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 14.06.97 14:49:18 ═╝
PROCEDURE Show_F
PRIVATE sup_qnt, tmp_code

ACTIVATE WINDOW (win_name+"_M") SAME

sup_qnt = STR(QNT_STORE,6)
DO CASE
CASE QNT_STORE = 0
  sup_qnt = " Нет  "
CASE QNT_STORE = -99999
  sup_qnt = " Есть "
ENDCASE

@ WROWS()-4, 3 SAY DIVISION.B_NAME COLOR SCHEME 1
@ WROWS()-4, COL()+1 SAY TRANSFORM(PRICE_R,"@Z 999999999.99") COLOR SCHEME 1
@ WROWS()-4, COL()+1 SAY TRANSFORM(QNT_ROUND,"@Z 999999") COLOR SCHEME 1
IF for_del
  @ WROWS()-4, COL()+1 SAY MEMO_ST COLOR SCHEME 1
ELSE
  @ WROWS()-4, COL()+1 SAY TRANSFORM(QNT_REP,"@Z 999999") COLOR SCHEME 1
  @ WROWS()-4, COL()+1 SAY MEMO_ST COLOR SCHEME 1
  @ WROWS()-4, COL()+1 SAY sup_qnt COLOR SCHEME 1
  tmp_code = ALLTRIM(STR(CODE))
  tmp_code = PADR(tmp_code, FSIZE("CODE", "DMS"))
  IF SEEK(tmp_code, "DMS")
    @ WROWS()-4, COL()+1 SAY TRANSFORM(MIN(DMS.QNT, 999999),"@Z 999999") COLOR SCHEME 1
  ELSE
    @ WROWS()-4, COL()+1 SAY "      " COLOR SCHEME 1
  ENDIF
ENDIF
IF FOUND("SBJ_4404")
  @ WROWS()-3, 3 SAY PADR(SBJ_4404.FUNCTION, WCOLS()-6)
ELSE
  @ WROWS()-3, 3 SAY SPACE(WCOLS()-6)
ENDIF

*@ WROWS()-2, 3 SAY STOCK.PREFIX COLOR SCHEME 1
@ WROWS()-2, 2 SAY LEFT(STOCK.NAME, WCOLS()-15) COLOR SCHEME 1
*@ WROWS()-2, COL()+1 SAY STOCK.PRODUCER COLOR SCHEME 1
*@ WROWS()-2, COL()+3 SAY "Артикул"
@ WROWS()-2, COL()+1 SAY Articul(STOCK.CODE) COLOR SCHEME 1
IF .NOT. EMPTY(STOCK.NOTE)
  m.sup_qnt = STOCK.NOTE
  WAIT WINDOW m.sup_qnt NOWAIT
ENDIF

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                           Коррекция адресов.                           ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 14.06.97 14:20:18 ═╝
PROCEDURE Addr
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

*┌─────────────────────────────────────────────────────────┐
*│ ┌─ Адреса ────────────────────────────────────────────┐ │
*│ │ Основной ▓▓▓▓▓▓▓▓?? Дополнительный ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓ │ │
*│ └─────────────────────────────────────────────────────┘ │
*│           Упаковка ▓▓▓▓▓▓     Отпуск ▓▓▓▓▓▓▓▓           │
*│            < OK Ctrl-W > < Отказаться Esc >             │
*└───────────────── F1 - История адресов ──────────────────┘

PRIVATE a1, a2, pk, pk_m, ex, nm_w

nm_w = ALLTRIM(STOCK.PREFIX+" "+STOCK.NAME+" "+STOCK.PRODUCER)
a1   = STOCK.ADDRESS
a2   = STOCK.MEMO_ST
pk   = STOCK.QNT_ROUND
pk_m = STOCK.QNT_PACK
ex   = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 8, 61, nm_w
@ WROWS()-1, 19 SAY " F1 - История адресов "
*------------------------------------------------------------------------
*      Ввод полей бланка
*
DO Sun_Bord WITH 2, 3, 4, 57, " Адреса "

@ 3,  5 SAY "Основной" GET a1 PICTURE pic_w
@ 3, 25 SAY "Дополнительный" GET a2
@ 5, 13 SAY "Упаковка" GET pk PICTURE "999999" VALID Tst_Pack(1)
@ 5, 32 SAY " Отпуск" GET pk_m PICTURE "999999" VALID Tst_Pack(2)
@ 6, 14 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

ON KEY LABEL F1 DO Addr_Hst WITH a1, a2, STOCK.ADDR_0, STOCK.MEMO_ST_0, STOCK.ADDR_1, STOCK.MEMO_ST_1
READ CYCLE
ON KEY LABEL F1

IF ex = 1
  IF STOCK.ADDRESS # a1
    REPLACE STOCK.ADDR_1  WITH STOCK.ADDR_0
    REPLACE STOCK.ADDR_0  WITH STOCK.ADDRESS
    REPLACE STOCK.ADDRESS WITH a1
  ENDIF
  IF STOCK.MEMO_ST # a2            
    REPLACE STOCK.MEMO_ST_1 WITH STOCK.MEMO_ST_0
    REPLACE STOCK.MEMO_ST_0 WITH STOCK.MEMO_ST
    REPLACE STOCK.MEMO_ST   WITH a2
  ENDIF            
  REPLACE STOCK.QNT_PACK  WITH pk_m,;
          STOCK.QNT_ROUND WITH pk
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                           Контекстный поиск.                           ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 16.06.97 08:19:37 ═╝
PROCEDURE Seek_C

PARAMETERS repeate	&& Повтор происка

PRIVATE tmpRec

m.tmpRec = Pos_Cont("", keyClass, for_del, m.repeate)
IF BETWEEN(m.tmpRec, 1, RECCOUNT("STOCK"))
  GO m.tmpRec IN STOCK
ENDIF

RETURN


*
*  Все, что далее - не работает...

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

*┌───────────────────────────────────────────────┐
*│                                               │
*│  Шаблон для поиска ░░░░▒░░░░█░░░░▒░░░░█░░░░▒  │
*│                                               │
*│       < OK Ctrl-W > < Отказаться Esc >        │
*│                                               │
*└───────────────────────────────────────────────┘

PRIVATE ex, nm_w     &&
                     &&   Объявляем и заполняем поля бланка
ex   = 1             &&
nm_w = SPACE(25)     &&
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 7, 51, "Контекстный поиск"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3, 4 SAY "Шаблон для поиска" GET nm_w
@ 5, 9 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

POP KEY
RELEASE WINDOW (win_name)

IF ex = 1
  SELECT STOCK
  DO Ch_Seek WITH ALLTRIM(nm_w)
ENDIF
*--------------------------------------------------------------------------

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║           Подборка по шаблону и интерактивный выбор позиции.           ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 16.06.97 08:25:11 ═╝
PROCEDURE Ch_Seek
PARAMETER shab

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

DEFINE PAD Choose OF (menu_name) PROMPT "Выбор" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) Do Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
PRIVATE rc_sav
rc_sav = RECNO()
DO Wt_Mess WITH "Подборка."

SELECT PREFIX, NAME, PRODUCER, RECNO() AS RC_NO  ;
       FROM STOCK INTO CURSOR TMP_C                ;
       WHERE shab $ PREFIX+" "+NAME+" "+PRODUCER AND EMPTY(DATE_OFF);
       ORDER BY NAME
SELECT TMP_C
IF RECCOUNT() = 0
  USE
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Ничего не найдено!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SELECT STOCK
  IF BETWEEN(rc_sav,1,RECCOUNT())
    GO rc_sav
  ENDIF
  RETURN
ENDIF

*
*   Расчет размеров окна
*
PRIVATE ln, wd
GO TOP
ln = MIN(100, RECCOUNT())   && Количество видимых строк BROWSE
wd = FSIZE("NAME")+FSIZE("PREFIX")+FSIZE("PRODUCER")+2

DO D_Wins WITH ln, wd, shab, 0, 0
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
    BROWSE FIELDS PREFIX:H="", NAME:H="", PRODUCER:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Выбрали

    GO TMP_C.RC_NO IN STOCK
    EXIT

  OTHERWISE

    IF BETWEEN(rc_sav,1,RECCOUNT("STOCK"))
      GO rc_sav IN STOCK
    ENDIF
    EXIT

  ENDCASE

ENDDO

USE
SELECT STOCK

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                            Поиск по адресу                             ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 16.06.97 08:19:37 ═╝
PROCEDURE Seek_A

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

*┌──────────────────────────────────┐
*│                                  │
*│    Шаблон для поиска ░░░░▒░░░    │
*│                                  │
*│ < OK Ctrl-W > < Отказаться Esc > │
*│                                  │
*└──────────────────────────────────┘

PRIVATE ex, nm_w     &&
                     &&   Объявляем и заполняем поля бланка
ex   = 1             &&
nm_w = SPACE(8)      &&
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 7, 38, "Поиск по адресу"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3, 6 SAY "Шаблон для поиска" GET nm_w
@ 5, 3 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

POP KEY
RELEASE WINDOW (win_name)

IF ex = 1
  SELECT STOCK
  DO Ch_SeekA WITH ALLTRIM(nm_w)
ENDIF
*--------------------------------------------------------------------------

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║           Подборка по шаблону и интерактивный выбор позиции.           ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 16.06.97 08:25:11 ═╝
PROCEDURE Ch_SeekA
PARAMETER shab

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
PRIVATE tmpName		  && Имя временного файла

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

DEFINE PAD Choose OF (menu_name) PROMPT "Выбор" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) Do Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
PRIVATE rc_sav
rc_sav = RECNO()
DO Wt_Mess WITH "Подборка."

SELECT 0
m.tmpName = SYS(3)
CREATE DBF (m.tmpo_path+m.tmpName)  ;
	(	ADDRESS		C(FSIZE("ADDRESS","STOCK")), ;
		PREFIX		C(FSIZE("PREFIX","STOCK")),  ;
		NAME		C(FSIZE("NAME","STOCK")),    ;
		PRODUCER	C(FSIZE("PRODUCER","STOCK")),;
		RC_NO		N(10) )
USE (m.tmpo_path+m.tmpName) ALIAS TMP_C
SELECT STOCK
SCAN FOR UPPER(shab) $ UPPER(ADDRESS)
  SELECT TMP_C
  APPEND BLANK
  REPLACE	ADDRESS		WITH STOCK.ADDRESS,  ;
  			PREFIX		WITH STOCK.PREFIX,   ;
			NAME		WITH STOCK.NAME,     ;
			PRODUCER	WITH STOCK.PRODUCER, ;
			RC_NO		WITH RECNO("STOCK")	
ENDSCAN

SELECT TMP_C
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Ничего не найдено!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE IN TMP_C
  DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
  SELECT STOCK
  IF BETWEEN(rc_sav,1,RECCOUNT())
    GO rc_sav
  ENDIF
  RETURN
ENDIF

*
*   Расчет размеров окна
*
PRIVATE ln, wd
GO TOP
ln = MIN(100, RECCOUNT())   && Количество видимых строк BROWSE
wd = FSIZE("ADDRESS")+FSIZE("NAME")+FSIZE("PREFIX")+FSIZE("PRODUCER")+3

DO D_Wins WITH ln, wd, shab, 0, 0
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
    BROWSE FIELDS ADDRESS:H="", PREFIX:H="", NAME:H="", PRODUCER:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Выбрали

    GO TMP_C.RC_NO IN STOCK
    EXIT

  OTHERWISE

    IF BETWEEN(rc_sav,1,RECCOUNT("STOCK"))
      GO rc_sav IN STOCK
    ENDIF
    EXIT

  ENDCASE

ENDDO

USE IN TMP_C
DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
SELECT STOCK

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                       Наличие: бронь и филиалы.                        ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 16.06.97 09:47:10 ═╝
PROCEDURE Br_Fil
PARAMETER mode
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
PRIVATE qnt_0, qnt_1, qnt_2
STORE 0 TO qnt_0, qnt_1, qnt_2

*
*   Заполняем значениями переменные состояния...
*
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

DO Wt_Mess WITH "Подборка."

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

PRIVATE w_where, w_who, w_cus, w_price, sum_qnt, sum_rez

DELETE FILE (tmpo_path+"BR_FIL.DBF")

SELECT 0
CREATE DBF (tmpo_path+"BR_FIL.DBF")  ;
  ( QNT      N( 6),  ;
    QNT_REZ  N( 6),  ;
    QNT_01   N( 6),  ;
    QNT_02   N( 6),  ;
    PRICE    C(10),  ;
    CUSTOMER C(50),  ;
    WHO      C(30),  ;
    WHO_1    C(30),  ;
    WHO_2    C(30),  ;
    L_NO     C( 6),  ;
    WHERE    C(16)   )

*
*    На реализации
*
SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE

SELECT 0
USE (base_path+"ARM_LST") ORDER TAG ARM_C

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE

*------------------------------------------------------------------------
sum_qnt = 0
sum_rez = 0
IF mode = 1
*
*  Филиалы
*
  SELECT 0
  USE (base_path+"RYN_L_T") ORDER TAG CUS_CODE

  SELECT 0
  USE (base_path+"RYN_L") ORDER TAG CODE

  SEEK STOCK.CODE
  SCAN REST WHILE CODE = STOCK.CODE
    = SEEK(CUS_CODE, "RYN_L_T")
    = SEEK(CUS_CODE, "ACCOUNT")
    w_cus   = ACCOUNT.CUS_NAME
    w_where = ""
    w_who   = ""
    w_price = TRANSFORM(RYN_L.PRICE_R, "@Z 999999.99")+"Р"
    IF FOUND("RYN_L_T")        && Если нашли заголовок...
      IF SEEK(RYN_L_T.WHO, "PERSONS")
        w_who = ALLTRIM(PERSONS.FAMILY)+" "+  ;
                LEFT(PERSONS.NAME,1)+". "+    ;
                LEFT(PERSONS.S_NAME,1)+"."
      ENDIF
      IF SEEK(RYN_L_T.WHERE, "ARM_LST")
        w_where = ARM_LST.ARM_N
      ENDIF
      IF RYN_L_T.VALUTA = "$"
        w_price = TRANSFORM(RYN_L.PRICE_D, "@Z 99999.9999")+"$"
      ENDIF
    ENDIF
    SELECT BR_FIL
    APPEND BLANK
    REPLACE QNT      WITH RYN_L.QNT, ;
            PRICE    WITH w_price,   ;
            CUSTOMER WITH w_cus,     ;
            WHO      WITH w_who,     ;
            WHERE    WITH w_where
    sum_qnt = sum_qnt+QNT
    SELECT RYN_L
  ENDSCAN
  SELECT RYN_L
  USE
  SELECT RYN_L_T
  USE
ENDIF

IF mode = 2
*
*  Спецфилиалы
*
  SELECT 0
  USE (base_path+"SPEC_SUM") ORDER TAG POS_CUS
  SET RELATION TO CUS_CODE INTO ACCOUNT

  SEEK STR(STOCK.CODE,7)
  SCAN REST WHILE CODE = STOCK.CODE
    w_cus   = ACCOUNT.CUS_NAME
    w_where = ""
    w_who   = ""
    w_price = ""
    SELECT BR_FIL
    APPEND BLANK
    REPLACE QNT      WITH SPEC_SUM.QNT, ;
            PRICE    WITH w_price,   ;
            CUSTOMER WITH w_cus,     ;
            WHO      WITH w_who,     ;
            WHERE    WITH w_where
    sum_qnt = sum_qnt+QNT
    SELECT SPEC_SUM
  ENDSCAN
  USE
ENDIF

IF mode = 0
  *
  *   Бронь
  *
  SELECT 0
  USE (base_path+"LIST_TIT") ORDER TAG LST_NUM

  SELECT 0
  USE (base_path+"LIST_DET") ORDER TAG CODE
  SEEK STOCK.CODE
  SCAN REST WHILE CODE = STOCK.CODE FOR QNT_REQ # 0 .OR. QNT_REZ # 0
    IF .NOT. SEEK(VAL(ALLTRIM(LIST_NUM)), "LIST_TIT")
      LOOP
    ENDIF
    IF .NOT. EMPTY(LIST_TIT.SAL_NUM)
      LOOP
    ENDIF
    IF .NOT. EMPTY(LIST_TIT.CLI_CODE) .AND.SEEK(LIST_TIT.CLI_CODE,"ACCOUNT")
      w_cus   = ACCOUNT.CUS_NAME
    ELSE
      w_cus   = LIST_TIT.LST_NAME
    ENDIF
    w_where = ""
    w_who   = ""
    w_price = TRANSFORM(SAL_PRICE, "@Z 999999.99")+"Р"
    IF SEEK(LIST_TIT.WHO_CR, "PERSONS")
      w_who = ALLTRIM(PERSONS.FAMILY)+" "+  ;
              LEFT(PERSONS.NAME,1)+". "+    ;
              LEFT(PERSONS.S_NAME,1)+"."
    ENDIF
    IF LIST_TIT.LST_ARM = "***"
      w_where = "Общая область"
    ELSE
      IF SEEK(LIST_TIT.LST_ARM, "ARM_LST")
        w_where = ARM_LST.ARM_N
      ENDIF
    ENDIF
    SELECT BR_FIL
    APPEND BLANK
    REPLACE QNT      WITH LIST_DET.QNT_REQ, ;
            QNT_REZ  WITH LIST_DET.QNT_REZ, ;
            PRICE    WITH w_price,   ;
            CUSTOMER WITH w_cus,     ;
            L_NO     WITH LIST_DET.LIST_NUM, ;
            WHO      WITH w_who,     ;
            WHERE    WITH w_where
    sum_qnt = sum_qnt+QNT
    sum_rez = sum_rez+QNT_REZ
    SELECT LIST_DET
  ENDSCAN
  USE (base_path+"ORD_T") ORDER TAG DOC_NUM AGAIN ALIAS OT_0809
  SELECT LIST_TIT
  USE (base_path+"ORD_D") ORDER TAG DOC_NUM AGAIN ALIAS OD_0809
  SET RELATION TO LIST_NUM+STR(ORD_NUM,3) INTO OT_0809
  SELECT BR_FIL
  SCAN
    SELECT OD_0809
    SEEK BR_FIL.L_NO
    SCAN REST WHILE BR_FIL.L_NO = LIST_NUM FOR CODE = STOCK.CODE
      DO CASE
      CASE .NOT. EMPTY(OT_0809.STO_MARK)
        qnt_2 = qnt_2+QNT
        SELECT BR_FIL
        REPLACE QNT_02 WITH QNT_02+OD_0809.QNT
        IF SEEK(OT_0809.STO_MARK, "PERSONS")
          REPLACE WHO_2 WITH ALLTRIM(PERSONS.FAMILY)+" "+  ;
                             LEFT(PERSONS.NAME,1)+". "+    ;
                             LEFT(PERSONS.S_NAME,1)+"."
        ENDIF
        SELECT OD_0809
      CASE .NOT. EMPTY(OT_0809.CHECK_S)
        qnt_1 = qnt_1+QNT
        SELECT BR_FIL
        REPLACE QNT_01 WITH QNT_01+OD_0809.QNT
        IF SEEK(VAL(ALLTRIM(OT_0809.CHECK_S)), "PERSONS")
          REPLACE WHO_1 WITH ALLTRIM(PERSONS.FAMILY)+" "+  ;
                             LEFT(PERSONS.NAME,1)+". "+    ;
                             LEFT(PERSONS.S_NAME,1)+"."
        ENDIF
        SELECT BR_FIL
      OTHERWISE
        qnt_0 = qnt_0+QNT
      ENDCASE
    ENDSCAN
    SELECT BR_FIL
  ENDSCAN
  SELECT OT_0809
  USE
  SELECT OD_0809
  USE
ENDIF


SELECT PERSONS
USE

SELECT ARM_LST
USE

SELECT ACCOUNT
USE

DO Wt_Mess

SELECT BR_FIL
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  DO CASE
  CASE mode = 0
    mss(2) = CHR(0)+"Ничего не забронировано!"
  CASE mode = 1
    mss(2) = CHR(0)+"В филиалах ничего нет!"
  CASE mode = 2
    mss(2) = CHR(0)+"В спецфилиалах ничего нет!"
  ENDCASE
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  DELETE FILE (tmpo_path+"BR_FIL.DBF")
  SELECT STOCK
  RETURN
ENDIF

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = MIN(100, RECCOUNT())   && Количество видимых строк BROWSE
wd = 68+IIF(mode = 0, 7, 0)
w_cus = ALLTRIM(STOCK.PREFIX+" "+STOCK.NAME+" "+STOCK.PRODUCER)

DO D_Wins WITH ln, wd, w_cus, IIF(mode = 0, 1, 0), IIF(mode = 0, 3, 1)

IF mode = 0
  @ 1, 2 SAY "Заявлено "+ALLTRIM(STR(qnt_0))+ ;
             "    В работе "+ALLTRIM(STR(qnt_1))+ ;
             "    Собрано "+ALLTRIM(STR(qnt_2))
  @ 2,  2 SAY "Кол-во"
  @ 2,  9 SAY "Резерв"
  @ 2, 16 SAY "В раб."
  @ 2, 24 SAY "Собр."
  @ 2, 34 SAY "Цена"
  @ 2, 43 SAY "Клиент"
  @ WROWS()-5,  0 CLEAR TO WROWS()-1,  WCOLS()-1

  DO Sun_Bord WITH WROWS()-5, 1, WROWS()-3, 10, "Всего"
  @ WROWS()-4,  3 SAY sum_qnt+sum_rez PICTURE "999999" COLOR SCHEME 1
  DO Sun_Bord WITH WROWS()-5, 11, WROWS()-3, WCOLS()-2
  @ WROWS()-5, 13 SAY "Раб. место"
  @ WROWS()-5, 30 SAY "Продавец"
  @ WROWS()-5, 61 SAY "Список"
  @ WROWS()-2,  2 SAY "Собирает"
  @ WROWS()-1,  2 SAY "Утвердил"

ELSE
  @ 1,  2 SAY "Кол-во"
  @ 1, 13 SAY "Цена"
  @ 1, 22 SAY "Клиент"
  @ WROWS()-3,  3 SAY "Всего"
  @ WROWS()-2,  3 SAY sum_qnt PICTURE "999999" COLOR SCHEME 1
  @ WROWS()-3, 10 SAY "┐" COLOR (SCHEME(14,2))
  @ WROWS()-2, 10 SAY "│" COLOR (SCHEME(14,2))
  @ WROWS()-1, 10 SAY "┘" COLOR (SCHEME(14,2))
  @ WROWS()-3, 11 SAY "┌" COLOR (SCHEME(13,1))
  @ WROWS()-2, 11 SAY "│" COLOR (SCHEME(13,1))
  @ WROWS()-1, 11 SAY "└" COLOR (SCHEME(13,1))

  @ WROWS()-3, 13 SAY "Раб. место"
  @ WROWS()-3, 30 SAY "Продавец"
  @ WROWS()-3, 61 SAY "Список"
ENDIF

what_do = "List"

GO TOP
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*

    IF mode = 0
      BROWSE FIELDS QNT:P="999999":H="",     ;
                    QNT_REZ:P="999999":H="", ;
                    QNT_01:P="999999":H="",  ;
                    QNT_02:P="999999":H="",  ;
                    PRICE:H="",    CUSTOMER:H="",        ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             WHEN ShowWho()              ;
             NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ELSE
      BROWSE FIELDS QNT:P="999999":H="",  ;
                    PRICE:H="",    CUSTOMER:H="",        ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             WHEN ShowWho()              ;
             NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ENDIF
    ON KEY
    what_do = "Exit"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
SELECT BR_FIL
USE
DELETE FILE (tmpo_path+"BR_FIL.DBF")
SELECT STOCK
RELEASE MENU (menu_name) EXTENDED

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                        Информационная строка.                          ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 16.06.97 12:35:10 ═╝
PROCEDURE ShowWho

ACTIVATE WINDOW (win_name+"_M") SAME

IF mode = 0
  @ WROWS()-4, 13 SAY WHERE COLOR SCHEME 1
  @ WROWS()-4, COL()+1 SAY WHO COLOR SCHEME 1
  @ WROWS()-4, COL()+1 SAY L_NO COLOR SCHEME 1
  @ WROWS()-2, 11 SAY WHO_1
  @ WROWS()-1, 11 SAY WHO_2
ELSE
  @ WROWS()-2, 13 SAY WHERE COLOR SCHEME 1
  @ WROWS()-2, COL()+1 SAY WHO COLOR SCHEME 1
  @ WROWS()-2, COL()+1 SAY L_NO COLOR SCHEME 1
ENDIF

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                          Наличие по партиям.                           ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 16.06.97 12:21:20 ═╝
PROCEDURE Incs
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
PRIVATE tmpSkip

*
*   Заполняем значениями переменные состояния...
*
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE

SELECT 0
USE (base_path+"ARM_LST") ORDER TAG ARM_C

SELECT 0
USE (base_path+"ACT_S_T") ORDER TAG DOC_NUM
*SET RELATION TO WHO_ACT INTO PERSONS, WHERE_ACT INTO ARM_LST

SELECT 0
USE (base_path+"ACT_S") ORDER TAG CODE
SET RELATION TO FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM INTO ACT_S_T

IF .NOT. SEEK(STR(STOCK.CODE,7))
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Для позиции нет ни одной партии!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT ACT_S_T
  USE
  SELECT ARM_LST
  USE
  SELECT PERSONS
  USE
  SELECT ACCOUNT
  USE
  SELECT STOCK
  RETURN
ENDIF

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Print OF (menu_name) PROMPT "Печать док-та" KEY Ctrl-A, "Enter"
ON SELECTION PAD Print OF (menu_name) DO Sw_Mode WITH "Print"

DEFINE PAD P_Rest OF (menu_name) PROMPT "Остатки"
ON PAD P_Rest OF (menu_name) ACTIVATE POPUP P_Rest

  DEFINE POPUP P_Rest MARGIN RELATIVE SHADOW COLOR SCHEME 4


  DEFINE BAR  1 OF P_Rest PROMPT "Фильтр" KEY Ctrl-A, "F3"
  ON SELECTION BAR 1 OF P_Rest DO Sw_Mode WITH "Filter"

  DEFINE BAR  2 OF P_Rest PROMPT "Продажа в первую очередь" KEY Ctrl-A, "Tab"
  ON SELECTION BAR 2 OF P_Rest DO Sw_Mode WITH "F_Sale"

IF .F. &&modi
  DEFINE PAD GTD OF (menu_name) PROMPT "ГТД" KEY Ctrl-A, "Tab"
  ON SELECTION PAD GTD OF (menu_name) DO Sw_Mode WITH "GTD"
ENDIF

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd, nm_w
ln = 12   && Количество видимых строк BROWSE
wd = 63
nm_w = ALLTRIM(STOCK.PREFIX+" "+STOCK.NAME+" "+STOCK.PRODUCER)

DO D_Wins WITH ln, wd, nm_w, 0, 3
@ 1, 2 SAY "Фрм.─"+PADC("Дата", IIF(SET("CENTURY") = "ON", 10, 8), "─")+ ;
           "──N док-та──Тип─Приход─Остат.──Реал.─"+ ;
           PADC("Продажа", IIF(SET("CENTURY") = "ON", 10, 8), "─")
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
*    IF .F. &&modi
*      ON KEY LABEL Tab DO Sw_Mode WITH "GTD"
*    ENDIF
    ON KEY LABEL Tab DO Sw_Mode WITH "F_Sale"
    ON KEY LABEL F3  DO Sw_Mode WITH "Filter"
*
*    BROWSE - меню
*
    BROWSE KEY IIF(UPPER(TAG()) = "SALE", STR(STOCK.CODE, 7), STOCK.CODE) ;
           FIELDS FIRM:H="",         ;
                  DOC_DATE:H="",     ;
                  DN000 = V_Doc(DOC_NUM):H="":10,  ;
                  DT000 = " "+INC_TYPE+" ":H="":3, ;
                  QNT_INC:H="":P="@Z 999999",      ;
                  QNT_REST:H="":P="@Z 999999",     ;
                  REST_REP:H="":P="@Z 999999",     ;
                  LAST_SALE:H="",     ;
                  F_SALE = IIF(FIRST_SALE, "√", " "):H="",;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN ShowWho1()             ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() = 12, "Exit", "Print")
    ENDIF

  CASE what_do = "GTD"

    DO New_GTD WITH STOCK.PREFIX, STOCK.NAME, STOCK.PRODUCER
    what_do = "List"

  CASE what_do = "Print"

    DO P_Act WITH FIRM, DOC_NUM, DOC_DATE
    what_do = "List"

  CASE what_do = "F_Sale"

    IF .NOT. BOF() .AND. .NOT. EOF()
      IF QNT_REST = 0
        PRIVATE mss
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"Эта партия уже пуста!"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
      ELSE
        REPLACE FIRST_SALE WITH .NOT. FIRST_SALE
      ENDIF
    ENDIF
    what_do = "List"

  CASE what_do = "Filter"

    IF UPPER(TAG()) = "SALE"
      SET ORDER TO TAG CODE
    ELSE
      m.tmpSkip = QNT_REST = 0
      SET ORDER TO TAG SALE
      IF m.tmpSkip
        SEEK(STR(STOCK.CODE, 7))
      ENDIF
    ENDIF
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
USE
SELECT ACT_S_T
USE
SELECT PERSONS
USE
SELECT ARM_LST
USE
SELECT ACCOUNT
USE
SELECT STOCK
RELEASE MENU (menu_name) EXTENDED

RETURN
*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                         Номер приемного акта.                          ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 16.06.97 12:43:22 ═╝
PROCEDURE V_Doc
PARAMETER d_n
PRIVATE p0, p1

p0 = ALLTRIM(LEFT(d_n,8))
p1 = ALLTRIM(RIGHT(d_n,2))

RETURN p0+IIF(EMPTY(p1), "", "/"+p1)

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                        Информационная строка.                          ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 16.06.97 12:35:10 ═╝
PROCEDURE ShowWho1
PRIVATE w, w1, w2

ACTIVATE WINDOW (win_name+"_M") SAME

IF SEEK(ACT_S_T.WHO_ACT, "PERSONS")
  w = PERSONS.FAMILY
ELSE
  w = SPACE(FSIZE("FAMILY","PERSONS"))
ENDIF

IF SEEK(ACT_S_T.WHERE_ACT, "ARM_LST")
  w1 = ARM_LST.ARM_N
ELSE
  w1 = SPACE(FSIZE("ARM_N","ARM_LST"))
ENDIF

IF SEEK(ACT_S_T.SUP_CODE, "ACCOUNT")
  w2 = ACCOUNT.CUS_NAME
ELSE
  w2 = SPACE(FSIZE("CUS_NAME","ACCOUNT"))
ENDIF

@ WROWS()-4, 3 SAY "ГТД N "+GTD_NUM+"/"+GTD_LINE
@ WROWS()-4, COL()+3 SAY "Страна "+COUNTRY
@ WROWS()-3, 3 SAY "Раб. место "+w1
@ ROW(), COL()+2 SAY "Принял "+w
@ WROWS()-2, 3 SAY w2
ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                      Специально для М В Смагиной!                      ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 21.07.97 15:27:09 ═╝
PROCEDURE For_MVS

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

*┌───── Интервал дат ────── Осн. склад ───────── Розн. магазин ──┐
*│ ДД.ММ.ГГГГ ДД.ММ.ГГГГ 999999 999999999.99 999999 999999999.99 │
*│                                                               │
*│                                                               │
*│                                                               │
*│                                                               │
*│                                                               │
*│                                                               │
*│                                                               │
*│                                                               │
*│                                                               │
*│                                                               │
*│                                                               │
*│                                                               │
*│                                                               │
*│                                                               │
*│                                                               │
*│                                                               │
*│ ДД.ММ.ГГГГ ДД.ММ.ГГГГ    < OK Ctrl-W > < Отказаться Esc >     │
*└───────────────────────────────────────────────────────────────┘

PRIVATE ex, d1, d2, q1, q2, s1, s2
ex   = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 21, 67, ALLTRIM(STOCK.PREFIX)+" "+ALLTRIM(STOCK.NAME)+" "+ALLTRIM(STOCK.PRODUCER)
@ 1,  7 SAY " Интервал дат "
@ 1, 27 SAY " Осн. склад "
@ 1, 48 SAY " Розн. магазин "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
d1 = {}
d2 = DATE()
SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG DOC_NUM
SELECT 0
USE (base_path+"SALE") ORDER TAG CODE
SET RELATION TO LEFT(DTOS(DOC_DATE),4)+DOC_NUM INTO SALE_TIT
DO WHILE .T.
  STORE 0 TO q1, q2, s1, s2
  @ WROWS()-2,  3 GET d1 PICTURE "@D"
  @ WROWS()-2, 14 GET d2 PICTURE "@D"
  @ WROWS()-2, 28 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "
  READ CYCLE
  IF ex = 2
    EXIT
  ENDIF
  SEEK STOCK.CODE
  SCAN REST WHILE CODE = STOCK.CODE FOR BETWEEN(DOC_DATE,d1, d2)
    IF SALE_TIT.CUS_CODE = 11
      LOOP
    ENDIF
    IF LEFT(DOC_NUM,1) = "A"
      q1 = q1+QNT
      s1 = s1+PRICE_R*QNT
    ELSE
      q2 = q2+QNT
      s2 = s2+PRICE_R*QNT
    ENDIF
  ENDSCAN
  @ WROWS()-2, 3 FILL TO WROWS()-2, WCOLS()-3 COLOR SCHEME 13
  @ WROWS()-2, 25 SAY STR(q1,7)+STR(s1,13,2)+STR(q2,7)+STR(s2,13,2)
  SCROLL 2, 2, WROWS()-2, WCOLS()-3, 1
  d1 = d2+1
  d2 = DATE()
ENDDO
USE
SELECT SALE_TIT
USE
SELECT STOCK
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                       Проверка нормы упаковки.                         ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 07.05.97 10:48:15 ═╝
PROCEDURE Tst_Pack
PARAMETER sw

IF ex = 2 .OR. READKEY() % 256 = 12
  RETURN .T.
ENDIF
IF sw = 1
  IF pk <= 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Таких упаковок не бывает!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
ELSE
  IF pk_m <= 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Таких фасовок не бывает!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
ENDIF

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура New_GTD      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                             Установка ГТД.                             │
*│                                                                        │
*└────────────────────────────────────────────────────────── 26.02.2000 ──┘
PROCEDURE New_GTD
PARAMETERS pPref, pName, pProd

PRIVATE tmpGTD, point1, point2, point3, swQnt, swEmp, swCur
swQnt = .F.
swEmp = .F.
swCur = 1

tmpGtd = Get_GTD(pPref, pName, pProd)
point1 = AT(CHR(13), tmpGtd)
point2 = AT(CHR(13), tmpGtd, 2)
point3 = AT(CHR(13), tmpGtd, 3)
IF EMPTY(point1)
  RETURN
ENDIF
IF Set_Mode()
  PRIVATE r_sav, val1, val2, val3, val4
  val1  = LEFT(tmpGTD, point1-1)
  val2  = SUBSTR(tmpGTD, point1+1, point2-point1-1)
  val3  = SUBSTR(tmpGTD, point2+1, point3-point2-1)
  val4  = SUBSTR(tmpGTD, point3+1)
  IF swCur = 1
    REPLACE GTD_NUM WITH val1, COUNTRY WITH val2, COUNTRYC WITH val4, GTD_LINE WITH val3
  ELSE
    r_sav = RECNO()
    SEEK STOCK.CODE
    SCAN REST WHILE CODE = STOCK.CODE
      IF EMPTY(QNT_REST) .AND. .NOT. swQnt
        LOOP
      ENDIF
      IF .NOT. EMPTY(GTD_NUM) .AND. .NOT. swEmp
        LOOP
      ENDIF
      REPLACE GTD_NUM WITH val1, COUNTRY WITH val2, COUNTRYC WITH val4, GTD_LINE WITH val3
    ENDSCAN
    IF BETWEEN(r_sav, 1, RECCOUNT())
      GO r_sav
    ENDIF
  ENDIF
ENDIF

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Set_Mode     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                  Для каких партий устанавливаем ГТД.                   │
*│                                                                        │
*└────────────────────────────────────────────────────────── 26.02.2000 ──┘
PROCEDURE Set_Mode
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

*┌─────────────────────────────────────────┐
*│ ( ) Текущая партия ( ) Несколько партий │
*│ ┌─ Фильтр ────────────────────────────┐ │
*│ │ [ ] Пустое наличие [ ] Непустая ГТД │ │
*│ └─────────────────────────────────────┘ │
*│    < OK Ctrl-W > < Отказаться Esc >     │
*└─────────────────────────────────────────┘

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 8, 45, "Для каких партий устанавливаем ГТД"
DO Sun_Bord WITH  3,  3,  5, 41, " Фильтр "

*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ 2, 3 GET swCur PICTURE "@*RH Текущая партия;Несколько партий" VALID Clr_F()
@ 4, 5 GET swQnt PICTURE "@*C Пустое наличие" WHEN swCur = 2
@ 4,24 GET swEmp PICTURE "@*C Непустая ГТД" WHEN swCur = 2
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Clr_F        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                        Очистка флажков фильтра.                        │
*│                                                                        │
*└────────────────────────────────────────────────────────── 26.02.2000 ──┘
PROCEDURE Clr_F

IF swCur = 1
  swQnt = .F.
  swEmp = .F.
  SHOW GET swCur
  SHOW GET swEmp
ENDIF

RETURN .T.
