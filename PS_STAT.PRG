*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║          Список персонала с карточками состояния.                     ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Ps_Stat

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

DEFINE PAD Tb_USE OF (menu_name) PROMPT "Карточка" KEY Ctrl-A, "Enter"
ON SELECTION PAD Tb_USE OF (menu_name) Do Sw_Mode WITH "Card"

DEFINE PAD Filter OF (menu_name) PROMPT "Фильтр" KEY Ctrl-A, "F6"
ON SELECTION PAD Filter OF (menu_name) Do Sw_Mode WITH "Filter"

DEFINE PAD Tb_Move OF (menu_name) PROMPT "Движение" KEY Ctrl-A, "F1"
ON SELECTION PAD Tb_Move OF (menu_name) Do Sw_Mode WITH "Move"

DEFINE PAD Karjera OF (menu_name) PROMPT "Карьера" KEY Ctrl-A, "F2"
ON SELECTION PAD Tb_Move OF (menu_name) Do Sw_Mode WITH "Karjera"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

***Определяем есть ли уже такой файл  
 
SELECT 0
USE (base_path+"DEPS") ORDER TAG B_NAME
PRIVATE d_names, d_codes, d_status, d_key
DIMENSION d_names(1), d_codes(1)
d_status = 1
d_names(1) = PADR("Все отделы", FSIZE("B_NAME"))
d_codes(1) = 0
SCAN
  d_status = d_status+1
  DIMENSION d_names(d_status), d_codes(d_status)
  d_names(d_status) = B_NAME
  d_codes(d_status) = CODE
ENDSCAN
d_status = 1

USE (base_path+"SLR_TYPE") ORDER TAG CODE

SELECT 0
USE (base_path+"PS_STAT") ORDER TAG CODE

SELECT 0
USE (base_path+"SLR_MOVE") ORDER TAG PS_CODE

SELECT 0
USE (base_path+"ARM_LST") ORDER TAG ARM_C ALIAS WHERE

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS WHO

SELECT 0
USE (base_path+"PERSONS") ORDER TAG NAME AGAIN

***Определяем есть ли уже такой файл  

DO Use_Dummy

SELECT PERSONS

*
*   Расчет размеров окна
*
PRIVATE ln, wd, f_name, cl, mon_list, mn0_sav, ye0_sav
ln = 18   && Количество видимых строк BROWSE
wd = 30
mn0_sav = MONTH(DATE())
ye0_sav = YEAR(DATE())

DIMENSION mon_list(12)
FOR cl = 1 TO 12
  mon_list(cl) = PADR(Month_R(cl), 10)
ENDFOR

DO D_Wins WITH ln, wd, "Сотрудники", 0, 1
@ WROWS()-3, 3 SAY " Отдел "

what_do = "list"
@ 1, 2 SAY "──Фамилия─────────────────────"
cl = SCHEME(13,1)+","+SCHEME(13,2)+",,,,,"+SCHEME(14,2)

f_name = ""
DO WHILE .T.

  DO CASE

  CASE what_do = "list"    && Просмотр списка

    statys_type = 3
    ACTIVATE WINDOW (win_name+"_M") SAME
    @ WROWS()-2, 3 SAY d_names(d_status)
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F1    Do Sw_Mode WITH "Move"
    ON KEY LABEL F2    Do Sw_Mode WITH "Karjera"
    ON KEY LABEL F6    Do Sw_Mode WITH "Filter"
    SELECT PERSONS
    IF d_status = 1
      SET ORDER TO TAG NAME
      d_key = ""
      DO Key_Seek
    ELSE
      SET ORDER TO TAG DEP_NAME
      d_key = STR(d_codes(d_status),3)
    ENDIF

*
*    BROWSE - меню
*
    BROWSE KEY d_key   ;
           FIELDS F000 = FAMILY-(" "+LEFT(NAME,1)+". "+LEFT(S_NAME,1)+"."):30:H="", ;
           DUMMY.F FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NORGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E") &&COLOR (cl)
    ON KEY
    IF what_do = "list"
      what_do = IIF(READKEY() % 256 = 12, "exit", "Card")
    ENDIF
    SET RELATION TO

  CASE what_do = "Card"      && Карточка сотрудника

    DO Modi_C WITH PERSONS.CODE

    what_do = "list"

  CASE what_do = "Move"      && Движение денег

    DO Move_S

    what_do = "list"

  CASE what_do = "Filter"    && Фильтр

    DO Filter

    what_do = "list"

  CASE what_do = "Karjera"   && Карьера

    DO View_K

    what_do = "list"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO


RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
CLOSE DATABASES

DELETE FILE (tmpo_path+"TMP_MOVS.DBF")
DELETE FILE (tmpo_path+"TMP_MOVS.CDX")

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║              Движение, подготовка временного файла.                    ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 30.10.97 18:11:51 ═╝
PROCEDURE Prep_Tmp
PRIVATE nm_u, nm_p, rst

IF .NOT. USED("TMP_MOVS")
  
  SELECT 0
  DELETE FILE (tmpo_path+"TMP_MOVS.DBF")
  DELETE FILE (tmpo_path+"TMP_MOVS.CDX")
  CREATE DBF (tmpo_path+"TMP_MOVS") ;
  ( MOV_DATE   D,      ;
    MOV_TIME   C( 8),  ;
    MOV_NUMB   C( 7),  ;
    MOV_NAME   C(24),  ;
    MOV_SUM    N(10,2),;
    MOV_PRC    N(10,2),;
    MOV_QNT    N( 8,2),;
    MOV_UNIT   C(12),  ;
    MOV_REST   N(10,2),;
    MOV_YE     N( 4),  ;
    MOV_MN     C( 8),  ;
    MOV_K_N    C( 7),  ;
    MOV_K_D    D,      ;
    MOV_NT     C(24),  ;
    MOV_WHO    C(30),  ;
    MOV_WHR    C(16) )
  INDEX ON DTOS(MOV_DATE)+MOV_TIME TAG HRON
  USE (tmpo_path+"TMP_MOVS") ORDER TAG HRON EXCLU

ELSE

  SELECT TMP_MOVS
  ZAP

ENDIF

SELECT SLR_MOVE
SEEK PERSONS.CODE

SCAN REST WHILE PERSONS.CODE = PS_CODE
  = SEEK(CODE, "SLR_TYPE")
  IF WHO = 0
    nm_u = "Сам!"
  ELSE
    IF SEEK(WHO, "WHO")
      nm_u = ALLTRIM(WHO.FAMILY)+" "+LEFT(WHO.NAME,1)+". "+  ;
             LEFT(WHO.S_NAME,1)+"."
    ELSE
      nm_u = "?"
    ENDIF
  ENDIF
  IF SEEK(WHERE, "WHERE")
    nm_p = WHERE.ARM_N
  ELSE
    nm_p = "?"
  ENDIF
  SELECT TMP_MOVS
  APPEND BLANK
  REPLACE MOV_DATE WITH SLR_MOVE.DOC_DATE, ;
          MOV_TIME WITH SLR_MOVE.DOC_TIME, ;
          MOV_NUMB WITH SLR_MOVE.DOC_NUM,  ;
          MOV_NAME WITH SLR_TYPE.NAME,     ;
          MOV_SUM  WITH Crpt_2(SLR_MOVE.TOTAL), ;
          MOV_PRC  WITH Crpt_2(SLR_MOVE.PRICE), ;
          MOV_QNT  WITH SLR_MOVE.QNT,      ;
          MOV_UNIT WITH SLR_MOVE.UNIT,     ;
          MOV_YE   WITH SLR_MOVE.YEAR,     ;
          MOV_MN   WITH Month_R(SLR_MOVE.MONTH),;
          MOV_K_N  WITH SLR_MOVE.K_DOC,    ;
          MOV_K_D  WITH SLR_MOVE.K_DATE,   ;
          MOV_NT   WITH SLR_MOVE.NOTE,     ;
          MOV_WHO  WITH nm_u,              ;
          MOV_WHR  WITH nm_p
  SELECT SLR_MOVE
ENDSCAN

IF SEEK(PERSONS.CODE,"PS_STAT")
  rst = Crpt_2(PS_STAT.START)
ELSE
  rst = 0
ENDIF

SELECT TMP_MOVS
SCAN
  rst = rst+MOV_SUM
  REPLACE MOV_REST WITH rst
ENDSCAN

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                               Движение.                                ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 30.10.97 19:11:14 ═╝
PROCEDURE Move_S
PRIVATE s_sav

s_sav = SELECT()

DO Wt_Mess WITH "Подготовка данных."
DO Prep_Tmp
DO Wt_Mess
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Движения по клиенту не было..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SELECT (s_sav)
  RETURN
ENDIF

DO View_M

SELECT (s_sav)

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                           Просмотр движения.                           ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 30.10.97 19:32:50 ═╝
PROCEDURE View_M

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

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd, ps_nam
ln = MIN(100, RECCOUNT())   && Количество видимых строк BROWSE
wd = IIF(SET("CENTURY") = "ON", 69, 67)
ps_nam = ALLTRIM(PERSONS.FAMILY)+" "+  ;
         ALLTRIM(PERSONS.NAME)+" "+ALLTRIM(PERSONS.S_NAME)

DO D_Wins WITH ln, wd, ps_nam, 0, 5
@ 1, 2 SAY PADC("Дата", IIF(SET("CENTURY") = "ON", 10, 8), "─")
@ 1, COL()+1 SAY PADC("N опер.", 7, "─")
@ 1, COL()+1 SAY PADC("Тип операции.", 24, "─")
@ 1, COL()+1 SAY PADC("─────Сумма", 12, "─")
@ 1, COL()+1 SAY PADC("───Остаток", 12, "─")
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
    BROWSE FIELDS MOV_DATE:H="",     ;
                  MOV_NUMB:H="":7,   ;
                  MOV_NAME:H="":24,  ;
                  MOV_SUM:H="":P="9 999 999.99", ;
                  MOV_REST:H="":P="9 999 999.99", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Sh_Info()   ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    what_do = "Exit"

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
*║                  Вывод информации в нижнюю часть окна.                 ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 30.10.97 19:56:37 ═╝
PROCEDURE Sh_Info
PRIVATE s

ACTIVATE WINDOW (win_name+"_M") SAME

s = "Количество "+TRANSFORM(MOV_QNT,"@Z 99999.99")+" "+ALLTRIM(MOV_UNIT)+ ;
    " расценка "+TRANSFORM(MOV_PRC,"@Z 9 999 999.99")+" Сумма "+ ;
    TRANSFORM(MOV_SUM,"@Z 9 999 999.99")
@ WROWS()-6, 2 SAY PADC(s, WCOLS()-4)

s = MOV_MN+TRANSFORM(MOV_YE,"@Z 99999")
IF .NOT. EMPTY(s)
  @ WROWS()-5, 2 SAY PADC("За "+s, WCOLS()-4)
ELSE
  @ WROWS()-5, 2 SAY PADC(s, WCOLS()-4)
ENDIF

s = "Номер кассовой операции "+MOV_K_N+" от "+TRANSFORM(MOV_K_D, "@D")
@ WROWS()-4, 2 SAY PADC(s, WCOLS()-4)

@ WROWS()-3, 2 SAY PADC("Примечание: "+MOV_NT, WCOLS()-4)

s = "Кто "+MOV_WHO+"    Раб. место "+MOV_WHR
@ WROWS()-2, 2 SAY PADC(s, WCOLS()-4)

SELECT (ALIAS())

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                                Карьера!                                ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 21.01.98 17:19:38 ═╝
PROCEDURE View_K

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

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
SELECT 0
USE (base_path+"KARJERA") ORDER TAG CODE ALIAS T_000

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = 8   && Количество видимых строк BROWSE
wd = 21

DO D_Wins WITH ln, wd, ALLTRIM(PERSONS.FAMILY), 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1

*
*    BROWSE - меню
*
    BROWSE KEY STR(PERSONS.CODE,5) ;
           FIELDS F_001 = Crpt_2(SALARY):P="99999", ;
                  F_002 = Month_R(MN):10,           ;
                  YE:P="9999",  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    what_do = "Exit"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO
SELECT T_000
USE
SELECT PERSONS

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                                Фильтр!                                 ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 21.01.98 17:19:38 ═╝
PROCEDURE Filter

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

PRIVATE ex
ex = d_status
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH MIN(ALEN(d_names)+3, 18), LEN(d_names(1))+4, "Выберите отдел"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
ex = d_status
DO WHILE .T.
  ex = MIN(ex, ALEN(d_names))
  ex = MAX(ex, 1)
  @ 1, 1 MENU d_names, ALEN(d_names), MIN(ALEN(d_names), WROWS()-3)
  READ MENU TO ex
  IF READKEY() % 256 = 15
    d_status = ex
    EXIT
  ENDIF
  IF READKEY() % 256 = 12
    EXIT
  ENDIF
ENDDO

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
