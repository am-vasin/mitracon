*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура V_Due        Разработчик Галина Дмитриенко                 │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Долг по позициям.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 15.12.2000 ──┘
PROCEDURE V_Due

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

DO Wt_Mess WITH "Подбор данных..."

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

DEFINE PAD Pos OF (menu_name) PROMPT "Порядок по хронологии" KEY Ctrl-A, "F1"
ON SELECTION PAD Pos OF (menu_name) Do Sw_Mode WITH "pos"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

DELETE FILE(tmpo_path+"TMP_DUE.DBF")
DELETE FILE(tmpo_path+"TMP_DUE.CDX")

CREATE DBF(tmpo_path+"TMP_DUE") ;
       ( P_CODE   N(  7), ;
         P_PREF   C(  4), ;
         P_NAME   C( 35), ;
         P_PROD   C(  8), ;
         NUM_LST  N(  6), ;
         NAME_LST C( 32), ;
         DATE_LST D,      ;
         ACC_CODE N(  8), ;
         ACC_NAME C( 25), ;
         QNT_DUE  N(  7), ;
         VALUTA   C(  1), ;
         PRICE    N(10,4), ;
         WHO      N(  3), ;
         FAMILY   C( 25)  )
INDEX ON P_NAME TAG P_NAME         
INDEX ON DTOS(DATE_LST) TAG HRON         

DO Use_Dummy

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS P_15

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS A_15

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS S_15

SELECT 0
USE (base_path+"LIST_TIT") ORDER TAG LST_NUM ALIAS L_T_15
SET RELATION TO CLI_CODE INTO A_15, WHO_CR INTO P_15

SELECT 0
USE (base_path+"LIST_DET") ALIAS L_D_15
SET FILTER TO .NOT. EMPTY(QNT_DUE)
SET RELATION TO VAL(ALLTRIM(LIST_NUM)) INTO L_T_15, CODE INTO S_15
SCAN
  SELECT TMP_DUE
  APPEND BLANK
  REPLACE P_CODE   WITH L_D_15.CODE, ;
          P_PREF   WITH IIF(L_D_15.CODE # 0,S_15.PREFIX,L_D_15.PREFIX),       ;
          P_NAME   WITH IIF(L_D_15.CODE # 0,S_15.NAME,L_D_15.NAME),         ;
          P_PROD   WITH IIF(L_D_15.CODE # 0,S_15.PRODUCER,L_D_15.PRODUCER), ;
          NUM_LST  WITH L_T_15.LST_NUM, ;
          NAME_LST WITH L_T_15.LST_NAME, ;
          DATE_LST WITH L_T_15.CR_DAT,   ;
          ACC_CODE WITH L_T_15.CLI_CODE, ;
          ACC_NAME WITH A_15.CUS_NAME,   ;
          QNT_DUE  WITH L_D_15.QNT_DUE,  ;
          VALUTA   WITH IIF(L_T_15.IS_USD,"$","р"), ;
          PRICE    WITH IIF(L_T_15.IS_USD,L_D_15.USD_PRICE,L_D_15.SAL_PRICE), ;
          WHO      WITH L_T_15.WHO_CR,   ;
          FAMILY   WITH ALLTRIM(P_15.FAMILY)+" "+LEFT(P_15.NAME,1)+". "+ ;
                                                 LEFT(P_15.S_NAME,1)+"."
ENDSCAN
SELECT TMP_DUE
SET ORDER TO TAG P_NAME
*
*   Расчет размеров окна
*
PRIVATE ln, wd, flt
flt = .F.
ln = MIN(100,RECCOUNT())        && Количество видимых строк BROWSE
wd = 70

DO Wt_Mess

IF ln = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Позиций с долгом нет..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RELEASE MENU (menu_name) EXTENDED

  CLOSE DATABASES
  DELETE FILE(tmpo_path+"TMP_DUE.DBF")
  DELETE FILE(tmpo_path+"TMP_DUE.CDX")
  RETURN
ENDIF

DO D_Wins WITH ln, wd, "Позиции с долгом", 1, 4
@ 1, 23 SAY "Порядок по наименованию"  
@ 2, 2 SAY "Преф.──────Наименование──────────────────Произв.──Колич.────────Цена─"
@ WROWS()-5,  2 SAY "Список № "
@ WROWS()-5, 20 SAY "от"
@ WROWS()-4,  2 SAY "Имя списка"
@ WROWS()-3,  2 SAY "Клиент"
@ WROWS()-2,  2 SAY "Менеджер"
what_do = "list"

DO WHILE .T.

  DO CASE

  CASE what_do = "list"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F1 DO Sw_Mode WITH "pos"

*
*    BROWSE - меню
*
    BROWSE FIELDS P_PREF:H="",  ;
                  P_NAME:H="",  ;
                  P_PROD:H="",  ;
                  QNT_DUE:H="", ;
                  PRICE:H="",   ;
                  VALUTA:H="",  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_F()   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")

    ON KEY
    IF what_do = "list"
      what_do = IIF(READKEY() % 256 = 12, "exit", "pos")
    ENDIF

  CASE what_do = "pos"       && Порядок по наименованию/Хронологии

    IF flt 
      SET ORDER TO TAG P_NAME
      DEFINE PAD Pos OF (menu_name) PROMPT "Порядок по хронологии" KEY Ctrl-A, "F1"
      @ 1, 23 SAY "Порядок по наименованию"  
      flt = .F.
    ELSE
      SET ORDER TO TAG HRON
      DEFINE PAD Pos OF (menu_name) PROMPT "Порядок по наименованию" KEY Ctrl-A, "F1"
      @ 1, 23 SAY " Порядок по хронологии "  
      flt = .T.
    ENDIF

    what_do = "list"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

CLEAR WINDOWS
RELEASE MENU (menu_name) EXTENDED

CLOSE DATABASES
DELETE FILE(tmpo_path+"TMP_DUE.DBF")
DELETE FILE(tmpo_path+"TMP_DUE.CDX")
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Draw_Foot    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                   Рисуем дополнительную информацию.                    │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.10.1998 ──┘
PROCEDURE Draw_F

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   Вот здесь, рисуем...
*
@ WROWS()-5, 13 SAY TMP_DUE.NUM_LST COLOR (SCHEME(14,2))
@ WROWS()-5, 23 SAY TMP_DUE.DATE_LST COLOR (SCHEME(14,2))
@ WROWS()-4, 13 SAY TMP_DUE.NAME_LST COLOR (SCHEME(14,2))
@ WROWS()-3, 12 SAY TMP_DUE.ACC_NAME COLOR (SCHEME(14,2))
@ WROWS()-2, 12 SAY TMP_DUE.FAMILY COLOR (SCHEME(14,2))

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.
