*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Order        Разработчик Андрей Васин           24.04.98 17:20:08 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                   Ручная коррекция цен для филиала.                    ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Mnl_Prc
PARAMETER f_code

PRIVATE s_sav, mss, pic

s_sav = SELECT()
SELECT 0
USE (base_path+"RYN_L_T") ORDER TAG CUS_CODE AGAIN ALIAS RT
IF .NOT. SEEK(f_code)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"За филиалом ничего не числится."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  RETURN
ENDIF
pic = IIF(VALUTA = "$", "99999.9999", "9999999.99")
SELECT 0
USE (base_path+"RYN_L") ORDER TAG CUS_CODE AGAIN ALIAS RL
IF .NOT. SEEK(f_code)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"За филиалом ничего не числится."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT RT
  USE
  SELECT (s_sav)
  RETURN
ENDIF

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE AGAIN ALIAS ST

SELECT 0
DELETE FILE (tmpo_path+"MNL_PRC.DBF")
DELETE FILE (tmpo_path+"MNL_PRC.CDX")
CREATE DBF (tmpo_path+"MNL_PRC") ;
  ( CODE     N( 7), ;
    PREFIX   C( 4), ;
    NAME     C(35), ;
    PRODUCER C( 8), ;
    QNT      N( 6), ;
    PRICE    N(9,4) )
SELECT RL
SET RELATION TO CODE INTO ST
DO Wt_Mess WITH "Подборка номенклатуры филиала..."
SCAN REST WHILE CUS_CODE = f_code FOR QNT # 0
  SELECT MNL_PRC
  APPEND BLANK
  REPLACE CODE     WITH RL.CODE, ;
          PREFIX   WITH ST.PREFIX, ;
          NAME     WITH ST.NAME, ;
          PRODUCER WITH ST.PRODUCER, ;
          QNT      WITH RL.QNT, ;
          PRICE    WITH IIF(RT.VALUTA = "$", RL.PRICE_D, RL.PRICE_R)
  SELECT RL
ENDSCAN
DO Wt_Mess
SET RELATION TO
SELECT MNL_PRC
INDEX ON NAME+PREFIX+PRODUCER TAG NAME
SET RELATION TO CODE INTO ST

DO Modi_Pr    && Все готово, приступаем к коррекции
DIMENSION mss(3)
mss(1) = ""
mss(2) = CHR(0)+"  Сохраним изменения? [ Да ] [ Нет ]"
mss(3) = ""
IF Out_Mess(5, "mss") = 1
  SELECT RL
  SET ORDER TO TAG C_CODE
  pic = STR(f_code,5)
  SELECT MNL_PRC
  SCAN FOR SEEK(pic+STR(CODE, 7), "RL")
    SELECT RL
    IF RT.VALUTA = "$"
      REPLACE PRICE_D WITH MNL_PRC.PRICE
    ELSE
      REPLACE PRICE_R WITH MNL_PRC.PRICE
    ENDIF
    SELECT MNL_PRC
  ENDSCAN
ENDIF

SELECT MNL_PRC
USE
DELETE FILE (tmpo_path+"MNL_PRC.DBF")
DELETE FILE (tmpo_path+"MNL_PRC.CDX")

SELECT RL
USE

SELECT RT
USE

SELECT ST
USE

SELECT (s_sav)

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                         Коррекция списка цен.                          ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 20.06.98 14:14:24 ═╝
PROCEDURE Modi_Pr
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

DEFINE PAD C_Seek OF (menu_name) PROMPT "Контекстн. поиск" KEY Ctrl-A, "F7"
ON SELECTION PAD C_Seek OF (menu_name) DO Sw_Mode WITH "C_Seek"

DEFINE PAD Discount OF (menu_name) PROMPT "Скидка"
ON PAD Discount OF (menu_name) ACTIVATE POPUP Discount

  DEFINE POPUP Discount MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR 1 OF Discount PROMPT "По всему наличию" KEY Ctrl-A, "F2"
  ON SELECTION BAR 1 OF Discount DO Sw_Mode WITH "Dis_H"

  DEFINE BAR 2 OF Discount PROMPT "По списку" KEY Ctrl-A, "Ctrl-F2"
  ON SELECTION BAR 2 OF Discount DO Sw_Mode WITH "Dis_L"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = 12   && Количество видимых строк BROWSE
wd = 75

DO D_Wins WITH ln, wd, ALLTRIM(RT.CUS_NAME), 1, 1
@ 2, 3 SAY ;
"─────────Позиция────────────────────────Склад────Ск.цена─Филиал───Цена фил"
@ 1, 2 SAY "Поиск Tab"

what_do = "List"
SELECT MNL_PRC
GO TOP

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL F7  DO Sw_Mode WITH "C_Seek"
    ON KEY LABEL F2  DO Sw_Mode WITH "Dis_H"
    ON KEY LABEL Ctrl+F2  DO Sw_Mode WITH "Dis_L"
    ON KEY LABEL Tab DO Seek_1

*
*    BROWSE - меню
*
    BROWSE FIELDS PREFIX:H="", ;
           Brw_Name=Nm_Part(NAME):25:H="", ;
           PRODUCER:H="",  ;
           ST.QNT:H="":P="@Z 999999", ;
           F000 = IIF(RT.VALUTA = "$", ST.PRICE, ST.PRICE_R):H="":P=pic, ;
           QNT:H="":P="@Z 999999", ;
           PRICE:H="":P=pic ;
           FREEZE PRICE ;
           WHEN Draw_Foot()    ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = "Exit"
    ENDIF

  CASE what_do = "C_Seek"    && Контекстный поиск

    DO C_Seek
    what_do = "List"

  CASE what_do = "Dis_H"     && Скидка по всему наличию

    DO Dis_H
    what_do = "List"

  CASE what_do = "Dis_L"     && Скидка по списку

    DO Dis_L
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                      Поиск по начальным символам!                      ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 13.04.98 11:51:11 ═╝
PROCEDURE Seek_1
PRIVATE flt_pos, max_len, str_flt, s_n, s_c, ins_flt
PRIVATE chr_shab, i, pos_new, str_new, to_seek, rc_sav

DEFINE WINDOW Seek_Hide FROM 0, 0 TO 0, WCOLS("")-1 NONE COLOR SCHEME 13
SHOW WINDOW Seek_Hide
flt_pos = 0
max_len = FSIZE("NAME")-1
str_flt = ""
s_n = 0
s_c = ""
ins_flt = INSMODE()
chr_shab = ""
FOR i = 32 TO 126
  chr_shab = chr_shab+CHR(i)
ENDFOR
FOR i = 128 TO 175
  chr_shab = chr_shab+CHR(i)
ENDFOR
FOR i = 224 TO 241
  chr_shab = chr_shab+CHR(i)
ENDFOR

ACTIVATE WINDOW (win_name+"_M") SAME

DO WHILE .T.
  to_seek = .F.
  @ 1, 8 SAY PADR(str_flt, max_len+1, "▒") COLOR SCHEME 1
  @ 1, 8+flt_pos SAY ""
  s_n = INKEY(0)
  IF s_n < 0       && Функциональная клавиша - мимо тазика!
    LOOP
  ENDIF
  s_c = CHR(s_n)

  IF s_c $ chr_shab    && Печатный символ!
    str_new = LEFT(str_flt,flt_pos)+s_c+SUBSTR(str_flt,flt_pos+IIF(ins_flt,1,2))
    str_new = LEFT(str_new, max_len+1)
    pos_new = MIN(flt_pos+1, max_len)
    to_seek = .T.
  ENDIF

  IF s_n = 19          && Стрелка влево
    flt_pos = MAX(flt_pos-1,0)
  ENDIF

  IF s_n = 4           && Стрелка вправо
    flt_pos = MIN(flt_pos+1, LEN(str_flt), max_len)
  ENDIF

  IF s_n = 7           && Del
    str_new = LEFT(str_flt,flt_pos)+SUBSTR(str_flt,flt_pos+2)
    pos_new = flt_pos
    to_seek = .T.
  ENDIF

  IF s_n = 127         && BackSpace
    str_new = LEFT(str_flt,flt_pos-1)+SUBSTR(str_flt,flt_pos+1)
    pos_new = MAX(flt_pos-1,0)
    to_seek = .T.
  ENDIF

  IF s_n = 22          && Ins
    ins_flt = .NOT. ins_flt
    = INSMODE(ins_flt)
  ENDIF

  IF s_n = 1           && Home
    flt_pos = 0
  ENDIF

  IF s_n = 6           && End
    flt_pos = MIN(LEN(str_flt), max_len)
  ENDIF

  IF s_n = 13          && Enter
    EXIT
  ENDIF

  IF INLIST(s_n, 24, 5, 3, 18)
    DO CASE
    CASE s_n = 24  && Стрелка вниз
      i = 1
    CASE s_n = 5   && Стрелка вверх
      i = -1
    CASE s_n = 3   && PgDn
      i = WROWS(win_name+"_E")-1
    OTHERWISE    && PgUp
      i = -WROWS(win_name+"_E")+1
    ENDCASE
    SKIP i
    IF BOF()
      GO TOP
    ENDIF
    IF EOF()
      GO BOTTOM
    ENDIF
    SHOW WINDOW (ALIAS()) REFRESH SAME
  ENDIF

  IF to_seek
    rc_sav = RECNO()
    IF SEEK(str_new)
      str_flt = str_new
      flt_pos = pos_new
      SHOW WINDOW (ALIAS()) REFRESH SAME
    ELSE
      IF BETWEEN(rc_sav, 1, RECCOUNT())
        GO rc_sav
      ENDIF
    ENDIF
  ENDIF

ENDDO

RELEASE WINDOW Seek_Hide

@ 1, 8 SAY PADR("Tab", max_len+1)

ACTIVATE WINDOW (ALIAS()) SAME

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                           Контекстный поиск!                           ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 20.06.98 11:51:11 ═╝
PROCEDURE C_Seek

PRIVATE s_sav, shab, rc_sav

shab = Get_Shab()
IF EMPTY(shab)
  RETURN
ENDIF

s_sav = SELECT()

SELECT 0
DELETE FILE(tmpo_path+"C_TMP.DBF")
CREATE DBF (tmpo_path+"C_TMP.DBF") ;
  ( N_REC    N(10), ;
    PREFIX   C( 4), ;
    NAME     C(35), ;
    PRODUCER C( 8), ;
    C_DUMMY  C( 1)  )
SELECT (s_sav)
rc_sav = RECNO()
DO Wt_Mess WITH "Поиск..."
SCAN FOR shab $ NAME
  SELECT C_TMP
  APPEND BLANK
  REPLACE N_REC    WITH RECNO("MNL_PRC"), ;
          PREFIX   WITH MNL_PRC.PREFIX, ;
          NAME     WITH MNL_PRC.NAME, ;
          PRODUCER WITH MNL_PRC.PRODUCER
  SELECT MNL_PRC
ENDSCAN
DO Wt_Mess
SELECT C_TMP
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Ничего не найдено..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  DELETE FILE(tmpo_path+"C_TMP.DBF")
  SELECT (s_sav)
  IF BETWEEN(rc_sav, 1, RECCOUNT())
    GO rc_sav
  ENDIF
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

DEFINE PAD Choose OF (menu_name) PROMPT "Выбор" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = MIN(100, RECCOUNT())   && Количество видимых строк BROWSE
wd = 39

DO D_Wins WITH ln, wd, "Отобрано по контексту", 0, 1
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
    BROWSE FIELDS PREFIX:H="", ;
           Brw_Name=Nm_Part(NAME):25:H="", ;
           PRODUCER:H="",   ;
           C_DUMMY:H="" FREEZE C_DUMMY ;
           WHEN Draw_Foot(.T.)    ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Выбор

    SELECT (s_sav)
    GO C_TMP.N_REC
    EXIT

  OTHERWISE

    SELECT (s_sav)
    IF BETWEEN(rc_sav, 1, RECCOUNT())
      GO rc_sav
    ENDIF
    EXIT

  ENDCASE

ENDDO

SELECT C_TMP
USE
DELETE FILE(tmpo_path+"C_TMP.DBF")
SELECT (s_sav)

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                       Запрос шаблона для поиска!                       ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 20.06.98 11:51:11 ═╝
PROCEDURE Get_Shab

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

*     Задайте шаблон для поиска
*┌──────────────────────────────────┐
*│                                  │
*│    ....:....!....:....!....:     │
*│                                  │
*│ < OK Ctrl-W > < Отказаться Esc > │
*└──────────────────────────────────┘

PRIVATE ex, sb       &&
                     &&   Объявляем и заполняем поля бланка
ex = 1
sb = ""
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 7, 38, "Задайте шаблон для поиска"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3, 6 EDIT sb SIZE 1, 25 COLOR (SCHEME(1,1))
@ 5, 3 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN IIF(ex = 1, sb, "")

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                   Установка скидки по всему наличию                    ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 20.06.98 11:51:11 ═╝
PROCEDURE Dis_H
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

* Скидка по всему наличию в филиале
*┌──────────────────────────────────┐
*│                                  │
*│ Процент скидки (наценки) 999.99  │
*│                                  │
*│ < OK Ctrl-W > < Отказаться Esc > │
*└──────────────────────────────────┘

PRIVATE ex, prc, rc_sav
ex  = 1
prc = 0
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 7, 38, "Скидка по всему наличию в филиале"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3, 3 SAY "Процент скидки (наценки)" GET prc PICTURE "@Z 999.99"
@ 5, 3 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex = 1 .AND. prc # 0
  rc_sav = RECNO()
  REPLACE ALL PRICE WITH PRICE*(100-prc)/100
  IF BETWEEN(rc_sav, 1, RECCOUNT())
    GO rc_sav
  ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                        Установка скидки списку                         ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 20.06.98 11:51:11 ═╝
PROCEDURE Dis_L
*
*  Переменные состояния для навигации
*

PRIVATE s_sav, list_n, list_c, n, mss
s_sav = SELECT()
SELECT 0
n = 0
IF File_O(base_path+"WK_TIT.DBF")
  USE (base_path+"WK_TIT") ALIAS WK AGAIN
  SCAN
    n = n+1
    DIMENSION list_n(n), list_c(n)
    list_n(n) = L_NAME
    list_c(n) = RECNO()
  ENDSCAN
  USE
ENDIF

IF n = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет ни одного списка-заготовки."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SELECT (s_sav)
  RETURN
ENDIF

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

*             Скидка по списку
*┌────────────────────────────────────────────┐
*│ ┌────────────────────────────────────────┐ │
*│ │....:....!....:....!....:....!....:....!│ │
*│ └────────────────────────────────────────┘ │
*│      Процент скидки (наценки) 999.99       │
*│      < OK Ctrl-W > < Отказаться Esc >      │
*└────────────────────────────────────────────┘

PRIVATE ex, point, prc, rc_sav, www
point = 1
ex  = 1
prc = 0
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 8, 48, "Скидка по списку"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 2, 3 GET point PICTURE "@^" FROM list_n SIZE 1, 42
@ 5, 8 SAY "Процент скидки (наценки)" GET prc PICTURE "@Z 999.99"
@ 6, 8 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

SELECT (s_sav)
IF ex = 1 .AND. prc # 0
  rc_sav = RECNO()
  SELECT 0
  USE (base_path+"WK_LIST") ALIAS WL AGAIN ORDER TAG L_CODE
  SELECT (s_sav)
  www = STR(list_c(point),5)
  SCAN FOR SEEK(www+STR(CODE,7),"WL")
    REPLACE PRICE WITH PRICE*(100-prc)/100
  ENDSCAN
  SELECT WL
  USE
  SELECT (s_sav)
  IF BETWEEN(rc_sav, 1, RECCOUNT())
    GO rc_sav
  ENDIF
ENDIF

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
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
PARAMETER brif

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   Вот здесь, рисуем...
*

ACTIVATE WINDOW (ALIAS()) SAME
IF brif
  @ WROWS()-2, 3 SAY NAME COLOR SCHEME 1
ELSE
  @ WROWS()-2, 3 SAY PREFIX COLOR SCHEME 1
  @ WROWS()-2, COL()+1 SAY NAME COLOR SCHEME 1
  @ WROWS()-2, COL()+1 SAY PRODUCER COLOR SCHEME 1
ENDIF

RETURN .T.
