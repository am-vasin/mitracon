*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла ViewList     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                      Просмотр содержимого списка                       ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 07.09.2006 ══╝
PROCEDURE ViewList
PARAMETERS l_num

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
USE (m.base_path+"ARM_LST") SHARED AGAIN ALIAS ARM_6907 ORDER TAG ARM_C
SELECT 0
USE (m.base_path+"ACCOUNT") SHARED AGAIN ALIAS ACC_6907 ORDER TAG CUS_CODE
SELECT 0
USE (m.base_path+"LIST_TIT") SHARED AGAIN ALIAS TIT_6907 ORDER TAG LST_NUM
SELECT 0
USE (m.base_path+"STOCK") SHARED AGAIN ALIAS STO_6907 ORDER TAG CODE
SELECT 0
USE (m.base_path+"LIST_DET") SHARED AGAIN ALIAS DET_6907 ORDER TAG L_NAME
SET RELATION TO CODE INTO STO_6907
IF .NOT. SEEK(m.l_num, "TIT_6907")
  PRIVATE mss
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"Списка с номером "+ALLTRIM(STR(m.l_num))+"не существует..."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  USE IN DET_6907
  USE IN ARM_6907
  USE IN ACC_6907
  USE IN TIT_6907
  USE IN STO_6907
  SELECT (m.s_sav)
ENDIF
= SEEK(TIT_6907.CLI_CODE, "ACC_6907")
= SEEK(TIT_6907.LST_ARM, "ARM_6907")
*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   Расчет размеров окна
*
PRIVATE ln, wd, tmpPic
m.ln = WROWS("")-5   && Количество видимых строк BROWSE
m.wd = 69

DO D_Wins WITH m.ln, m.wd, "Список № "+ALLTRIM(STR(m.l_num))+ ;
               " АРМ "+ALLTRIM(ARM_6907.ARM_N), 1, 0
@ 1, 1 SAY "Клиент "+ACC_6907.CUS_NAME

IF TIT_6907.IS_USD
  @ 2, 2 SAY ;
  "───────Наименование──────────────────────Фирма────Кол-во──Цена (У Е)"
  *PPP nnnnnnnnnNnnnnnnnnNnnnnnnnnnNnnnnn ffffffff 999999 999999999999
  m.tmpPic = "9999999.9999"
ELSE
  @ 2, 2 SAY ;
  "───────Наименование──────────────────────Фирма────Кол-во──Цена (Руб)"
  *PPP nnnnnnnnnNnnnnnnnnNnnnnnnnnnNnnnnn ffffffff 999999 999999999999
  m.tmpPic = "999999.99999"
ENDIF
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    m.statys_type = 3
    DO Prp_Nav_1
*    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE KEY STR(l_num, 6) ;
           FIELDS STO_6907.PREFIX:H="",		;
                  STO_6907.NAME:H="",		;
                  STO_6907.PRODUCER:H="",	;
                  DET_6907.QNT:H="",		;
                  tmpPr = IIF(TIT_6907.IS_USD, DET_6907.USD_PRICE, DET_6907.SAL_PRICE):P=m.tmpPic, ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

*  CASE m.what_do = "Choose"    && Просмотр списка

*    DO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*    m.what_do = "List"

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
USE IN DET_6907
USE IN ARM_6907
USE IN ACC_6907
USE IN TIT_6907
USE IN STO_6907
SELECT (m.s_sav)

RETURN
