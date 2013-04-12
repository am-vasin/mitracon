*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Inc          Разработчик Андрей Васин           06.05.97 14:58:44 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                     Программа приходования товара.                     ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Inc
PARAMETER nds_main

PRIVATE supl_c, supl_n, mss, d_names, d_codes, n, w, i000
PRIVATE nds_flt
nds_flt = nds_main

*
*   Список разделов номенклатуры
*
USE (base_path+"DIVISION")
n = 0
SCAN
  n = n+1
  DIMENSION w(n,2)
  w(n,1) = B_NAME
  w(n,2) = CODE
ENDSCAN
= ASORT(w,1)

DIMENSION d_names(n), d_codes(n)
FOR i000 = 1 TO n
  d_codes(i000) = w(i000,2)
  d_names(i000) = w(i000,1)
ENDFOR
RELEASE w, i000

*
*   Список поставщиков
*
USE (base_path+"CUSTOMER") ORDER TAG NAME
n = 0
SCAN FOR SUP # " "
  n = n+1
  DIMENSION supl_c(n), supl_n(n)
  supl_c(n) = CODE
  supl_n(n) = B_NAME
ENDSCAN

IF .NOT. File_O(log_path+"DUE_LOG.DBF")
  DELETE FILE (log_path+"DUE_LOG.CDX")
  CREATE DBF (log_path+"DUE_LOG") ;
   ( DATE_DUE D,    ;
     TIME_DUE C(8), ;
     STO_CODE N(7), ;
     GOT      L,    ;
     WHO      N(3), ;
     WHERE    C(3)  )
  FOR n = 1 TO 1000
    APPEND BLANK
  ENDFOR
  INDEX ON DTOS(DATE_DUE)+TIME_DUE TAG HRON
ENDIF

USE
DO Use_Dummy

IF File_O(base_path+"INC_LOG.DBF")
  SELECT 0
  USE (base_path+"INC_LOG") ALIAS LOG
ENDIF

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE

SELECT 0
USE (base_path+"ACT_S_T") ORDER TAG DOC_NUM

SELECT 0
USE (base_path+"ACT_S") ORDER TAG CODE

SELECT 0
USE (base_path+"INC_TIT")

SELECT 0
USE (base_path+"LIST_DET") ORDER TAG DUE_CODE

SELECT 0
USE (base_path+"LIST_DET") ORDER TAG DUE_NAM ALIAS DUE_NAM AGAIN

SELECT 0
USE (base_path+"INC_LIST") ORDER TAG L_CODE
DELETE FILE (tmpo_path+"TMP_INC.DBF")
COPY STRUCTURE TO (tmpo_path+"TMP_INC")
SELECT 0
USE (tmpo_path+"TMP_INC") EXCLUSIVE

SELECT 0
USE (base_path+"INC_DUE") ORDER TAG L_CODE

SELECT 0
USE (base_path+"INC_NEW") ORDER TAG L_NAME
DELETE FILE (tmpo_path+"TMP_NEW.DBF")
COPY STRUCTURE TO (tmpo_path+"TMP_NEW")
SELECT 0
USE (tmpo_path+"TMP_NEW") EXCLUSIVE
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
what_do    = "list"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Corr OF (menu_name) PROMPT "Корр." KEY Ctrl-A, "Enter"
ON SELECTION PAD Corr OF (menu_name) Do Sw_Mode WITH "Corr"

DEFINE PAD Add OF (menu_name) PROMPT "Доб." KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) Do Sw_Mode WITH "Add"

DEFINE PAD Del OF (menu_name) PROMPT "Удал." KEY Ctrl-A, "Del"
ON SELECTION PAD Del OF (menu_name) Do Sw_Mode WITH "Del"

DEFINE PAD Price OF (menu_name) PROMPT "Цены" KEY Ctrl-A, "F2"
ON SELECTION PAD Price OF (menu_name) Do Sw_Mode WITH "Price"

DEFINE PAD File OF (menu_name) PROMPT "Файл" KEY Ctrl-A, "F3"
ON SELECTION PAD File OF (menu_name) Do Sw_Mode WITH "File"

DEFINE PAD Act OF (menu_name) PROMPT "Прих." KEY Ctrl-A, "Tab"
ON SELECTION PAD Act OF (menu_name) Do Sw_Mode WITH "Act"

DEFINE PAD Exit OF (menu_name) PROMPT "Вых." KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-10   && Количество видимых строк BROWSE
wd = 55

DO D_Wins WITH ln, wd, "Заготовки приемных актов", 0, 0
what_do = "List"
@ 1,  2 SAY "Фрм"
@ 1,  7 SAY " Поставщик "
@ 1, 24 SAY " Накл. N "
@ 1, 35 SAY " Дата "
@ 1, 45 SAY "Цены"
@ 1, 51 SAY "%НДС"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    SELECT INC_TIT
    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins   Do Sw_Mode WITH "Add"
    ON KEY LABEL Del   Do Sw_Mode WITH "Del"
    ON KEY LABEL F2    Do Sw_Mode WITH "Price"
    ON KEY LABEL F3    Do Sw_Mode WITH "File"
    ON KEY LABEL Tab   Do Sw_Mode WITH "Act"

*
*    BROWSE - меню
*
    BROWSE FOR SUP_CODE # 0   ;
           FIELDS INC_TIT.FIRM:H="":3,        ;
                  INC_TIT.SUP_NAME:H="":16,   ;
                  INC_TIT.INC_DOC:H="":10,    ;
                  INC_TIT.INC_DATE:H="",      ;
                  F000 = PADC(IS_PRICE,4),    ;
                  NDS_:H="",  ;
                  F001 = IIF(BUY=1, "П","Р"), ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND REST NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
*           WHEN Draw_Foot() ;
*
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Corr")
    ENDIF

  CASE what_do = "Corr"    && Коррекция приемного акта

    IF BETWEEN(RECNO(), 1, RECCOUNT())
      nds_flt = NDS_
      DO Inc_List
    ENDIF
    what_do = "List"

  CASE what_do = "Add"     && Добавление заготовки

    DO Inc_Tit
    what_do = "List"

  CASE what_do = "Del"     && Удаление заготовки

    IF BETWEEN(RECNO(), 1, RECCOUNT())
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"  Действительно удалить? [ Да ] [ Нет ]"
      mss(3) = ""
      IF Out_Mess(7, "mss") = 1
        DO Del_Tit
      ENDIF
    ENDIF

    what_do = "List"

  CASE what_do = "Price"   && Установка режима цен

    IF BETWEEN(RECNO(), 1, RECCOUNT())
      DO Prices
    ENDIF
    what_do = "List"

  CASE what_do = "File"   && Ввод данных из файла

    IF BETWEEN(RECNO(), 1, RECCOUNT())
      DO Ext_Inc WITH RECNO()
    ENDIF
    what_do = "List"

  CASE what_do = "Act"     && Приход

    IF BETWEEN(RECNO(), 1, RECCOUNT())
      nds_flt = NDS_
      DO Prep_Act
    ENDIF
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

CLEAR WINDOWS
CLOSE DATABASES
DELETE FILE (tmpo_path+"TMP_INC.DBF")
DELETE FILE (tmpo_path+"TMP_NEW.DBF")

RELEASE MENU (menu_name) EXTENDED

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Draw_Foot    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                   Рисуем дополнительную информацию.                    │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.10.1998 ──┘
PROCEDURE Draw_Foot
ACTIVATE WINDOW (win_name+"_M") SAME

*
*   Вот здесь, рисуем...
*
@ WROWS()-2, 2 SAY "Номер ГТД "+GTD_NUM
@ WROWS()-2, COL()+1 SAY "Страна "+LEFT(COUNTRY,13)

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                           Установка режима.                            ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 13.05.97 21:43:41 ═╝
PROCEDURE Prices
*
*  Переменные состояния для навигации
*

IF .NOT. Get_L()
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
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    Содержательная часть программы:
*

*┌───────────────────────────────────────┐
*│ ┌─ Цена ────────────────────────────┐ │
*│ │ ( ) Нет   ( ) Рубли   ( ) Доллары │ │
*│ └───────────────────────────────────┘ │
*│           Процент НДС 999.9           │
*│   < OK Ctrl-W > < Отказаться Esc >    │
*└───────────────────────────────────────┘

PRIVATE ex, v_tmp, nds_w
ex    = 1
nds_w = NDS_
IF IS_PRICE = " "
  v_tmp = 1
ELSE
  v_tmp = IIF(IS_PRICE = "$", 3, 2)
ENDIF
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 8, 43, "Режим"
DO Sun_Bord WITH 2, 3, 4, 39, " Цена "
*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3, 5 GET v_tmp PICTURE "@*RH Нет  ;Рубли  ;Доллары" VALID Set_NDS()
@ 5,13 SAY "Процент" GET nds_w PICTURE "@Z 999.9" WHEN v_tmp = 2
@ 6, 5 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex = 1
  SELECT INC_LIST
  nds_flt = nds_w
  DO CASE
  CASE v_tmp = 1
    SEEK STR(RECNO("INC_TIT"),5)
    REPLACE REST WHILE LIST_NO = STR(RECNO("INC_TIT"),5) PRICE WITH 0
    SELECT INC_TIT
    REPLACE IS_PRICE WITH " ", NDS_ WITH nds_w
  CASE v_tmp = 2
    IF INC_TIT.IS_PRICE = "$"
      SEEK STR(RECNO("INC_TIT"),5)
      REPLACE REST WHILE LIST_NO = STR(RECNO("INC_TIT"),5) PRICE WITH Clc_Rub(PRICE, kurs_b)
    ENDIF
    IF INC_TIT.IS_PRICE = "Р"
      SEEK STR(RECNO("INC_TIT"),5)
      REPLACE REST WHILE LIST_NO = STR(RECNO("INC_TIT"),5) PRICE WITH ROUND_6(PRICE)
    ENDIF
    SELECT INC_TIT
    REPLACE IS_PRICE WITH "Р", NDS_ WITH nds_w
  CASE v_tmp = 3
    IF INC_TIT.IS_PRICE = "Р"
      SEEK STR(RECNO("INC_TIT"),5)
      REPLACE REST WHILE LIST_NO = STR(RECNO("INC_TIT"),5) PRICE WITH ROUND(PRICE/kurs_b,2)
    ENDIF
    SELECT INC_TIT
    REPLACE IS_PRICE WITH "$", NDS_ WITH nds_w
  ENDCASE
ENDIF
SELECT INC_TIT
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)

REPLACE INC_TIT.LOCKED WITH "***"

RETURN
*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║            Установка НДС в соответствии с типом цен.                   ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 07.05.97 11:22:51 ═╝
PROCEDURE Set_NDS

DO CASE
CASE v_tmp = 1
  nds_w = 0
CASE v_tmp = 2
  nds_w = nds_flt
CASE v_tmp = 3
  nds_w = 0
ENDCASE
SHOW GET nds_w
RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                           Захватить список!                            ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 07.05.97 11:22:51 ═╝
PROCEDURE Get_L

PRIVATE mss, sel_sav, arm_name

IF INC_TIT.LOCKED # "***"
  sel_sav = SELECT()
  SELECT 0
  USE (base_path+"ARM_LST") ORDER TAG ARM_C ALIAS ARM000 AGAIN
  IF SEEK(INC_TIT.LOCKED)
    arm_name = ALLTRIM(ARM_N)
    DIMENSION mss(4)
    mss(1) = ""
    mss(2) = CHR(0)+"ВНИМАНИЕ! Этот список в работе, АРМ "+'"'+arm_name+'"!'
    mss(3) = CHR(0)+"  Как поступим? [ Пусть работают ] [ Наплевать! ]"
    mss(4) = ""
    IF Out_Mess (7, "mss") = 1
      USE
      SELECT (sel_sav)
      RETURN .F.
    ELSE
      IF INC_TIT.LOCKED # arm
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"Работа с чужим списком недоступна !"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        USE
        SELECT (sel_sav)
        RETURN .F.
      ENDIF
    ENDIF
  ENDIF
  USE
  SELECT (sel_sav)
ENDIF

REPLACE INC_TIT.LOCKED WITH arm

RETURN .T.
