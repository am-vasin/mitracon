*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла For_Mst      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                        Таблицы для Я Мостового                         ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 03.07.2004 ══╝
PROCEDURE For_Mst

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
PRIVATE tbl_type, rez_file, rez_sw
m.tbl_type = 1
m.rez_file = m.tmpo_path+"result.dbf"
m.rez_sw   = .F.
m.rez_file = PADR(m.rez_file, 58)

*------------------------------------------------------------------------
*    Содержательная часть программы:
*

*┌──────────────────────────────────────────────────────────────┐
*│ ┌─ Тип таблицы ────────────────────────────────────────────┐ │
*│ │ ( ) Einfo ( ) Производители ( ) Классы ( ) Цены ( ) Comm │ │
*│ └──────────────────────────────────────────────────────────┘ │
*│ ┌─ [ ] Куда поместить результат ───────────────────────────┐ │
*│ │                                                          │ │
*│ └──────────────────────────────────────────────────────────┘ │
*│          < OK Ctrl-W > < Отказаться Esc >                    │
*└──────────────────────────────────────────────────────────────┘

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 10, 66, "Отчеты"
DO Sun_Bord WITH  2,  3,  4, 62, " Тип таблицы "
DO Sun_Bord WITH  5,  3,  7, 62, " [ ] Куда поместить результат "

*------------------------------------------------------------------------
*      Ввод полей бланка
*

@  3,  5 GET m.tbl_type PICTURE "@*RH Einfo;Производители;Классы;Цены;Comm"
@  5,  6 GET m.rez_sw PICTURE "@*C Куда поместить результат" VALID Get_File()
@  6,  4 GET m.rez_file WHEN .F.
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_Blank()

IF ex = 1
  *
  * Отрабатываем бланк
  DO CASE
  CASE m.tbl_type = 1
    DO Make_F1
  CASE m.tbl_type = 2
    DO Make_F2
  CASE m.tbl_type = 3
    DO Make_F3
  CASE m.tbl_type = 4
    DO Make_F4
  CASE m.tbl_type = 5
    DO Make_F5
  ENDCASE
  
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Get_File     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                        Выбор файла результата.                         │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03.07.2004 ──┘
PROCEDURE Get_File

PRIVATE tmpPth, tmpFile, i
m.rez_sw = .F.

m.i = RAT("\", ALLTRIM(m.rez_file))
IF m.i = 0
  m.tmpFile = ""
  m.tmpPth = ALLTRIM(m.rez_file)
ELSE
  m.tmpFile = SUBSTR(ALLTRIM(m.rez_file), m.i+1)
  m.tmpPth = LEFT(m.rez_file, m.i-1)
ENDIF
m.rez_file = New_File(tmpPth, "dbf", "Задайте файл", 0, m.tmpFile)
m.rez_file = PADR(m.rez_file, 58)

SHOW GET m.rez_sw
SHOW GET m.rez_file

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Tst_Blank    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Проверка бланка.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03.07.2004 ──┘
PROCEDURE Tst_Blank

PRIVATE tmpPth, mss

IF m.ex # 1
  RETURN .T.
ENDIF
m.i = RAT("\", ALLTRIM(m.rez_file))
IF m.i = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы не указали имя файла"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ELSE
  m.tmpPth = LEFT(m.rez_file, m.i-1)
  IF .NOT. Is_Dir(m.tmpPth)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Некорректная директория"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
ENDIF

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Make_F1      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                    Создание файла в формате EInfo.                     │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03.07.2004 ──┘
PROCEDURE Make_F1

PRIVATE tmpCount, tmpStr
CREATE DBF (rez_file) ;
   (  PREFIX    C ( 4),;
      NAME      C (35),;
      PRODUCER  C ( 8),;
      CLASS     C (64),;
      FUNCTION  C (254),;
      CORPUS    C (16),;
      QNT       N (10),;
      QNT_C     N (10),;
      QNT_H     N (10),;
      PRICE_R_D N(9,2),;
      PRICE_R_R N(9,2)    )
USE (rez_file) ALIAS REZ_TABLE
SELECT 0
USE (base_path+"CLASSES") SHARED AGAIN ORDER TAG CODE
SELECT 0
USE (path_comm+"CORPUS") SHARED AGAIN ORDER TAG CODE
SELECT 0
USE (path_comm+"SUBJECT") SHARED AGAIN ORDER TAG STO_CODE
SET RELATION TO COR_CODE INTO CORPUS
SELECT 0
USE (base_path+"STOCK") SHARED AGAIN ORDER TAG NAME
SET RELATION TO CODE INTO SUBJECT
SET RELATION TO CLASS INTO CLASSES ADDITIVE

DO Wt_Mess WITH "Подготовка данных..."

m.tmpCount = 0
SCAN
  m.tmpCount = m.tmpCount+1
  IF m.tmpCount % 100 = 0
    m.tmpStr = PADC(ALLTRIM(STR(m.tmpCount)), WCOLS(WOUTPUT()))
    @ wROWS()-1, 0 SAY m.tmpStr
  ENDIF
  SELECT REZ_TABLE
  APPEND BLANK
  REPLACE PREFIX    WITH STOCK.PREFIX,     ;
          NAME      WITH STOCK.NAME,       ;
          PRODUCER  WITH STOCK.PRODUCER,   ;
          CLASS     WITH CLASSES.NAME,     ;
          FUNCTION  WITH SUBJECT.FUNCTION, ;
          CORPUS    WITH CORPUS.NAME,      ;
          QNT       WITH STOCK.QNT,        ;
          QNT_C     WITH STOCK.QNT_STORE,  ;
          QNT_H     WITH STOCK.QNT+STOCK.QNT_STORE,;
          PRICE_R_D WITH STOCK.PRICE,      ;
          PRICE_R_R WITH STOCK.PRICE_R
  SELECT STOCK
ENDSCAN

DO Wt_Mess

CLOSE DATABASES

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Make_F2      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                  Создание файла фирм - производителей.                 │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03.07.2004 ──┘
PROCEDURE Make_F2

CREATE DBF (rez_file) ;
   (  PRODUCER C(16), ;
      L_NAME   C(60), ;
      FOLDER   C( 8)  )
USE (rez_file) ALIAS REZ_TABLE
SELECT 0
USE (base_path+"DEVELOP") SHARED AGAIN ORDER TAG B_NAME
SCAN
  SELECT REZ_TABLE
  APPEND BLANK
  REPLACE PRODUCER WITH DEVELOP.B_NAME, ;
          L_NAME   WITH DEVELOP.L_NAME, ;
          FOLDER   WITH DEVELOP.FOLDER
  SELECT DEVELOP
ENDSCAN

DO Wt_Mess

CLOSE DATABASES

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Make_F3      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                  Создание файла классов номенклатуры.                  │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03.07.2004 ──┘
PROCEDURE Make_F3

CREATE DBF (rez_file) ;
   (  CLASS        C(12), ;
      CLASS_NAME   C(64)  )
USE (rez_file) ALIAS REZ_TABLE
SELECT 0
USE (base_path+"CLASSES") SHARED AGAIN ORDER TAG CODE
SCAN
  SELECT REZ_TABLE
  APPEND BLANK
  REPLACE CLASS      WITH CLASSES.CODE, ;
          CLASS_NAME WITH CLASSES.NAME
  SELECT CLASSES
ENDSCAN

DO Wt_Mess

CLOSE DATABASES

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Make_F4      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                    Создание файла для цеников.                         │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03.07.2004 ──┘
PROCEDURE Make_F4

PRIVATE tmpCount, tmpStr
CREATE DBF (rez_file) ;
   (  STO_CODE    N ( 7),;
      PREFIX      C ( 4),;
      NAME        C (35),;
      PRODUCER    C ( 8),;
      CLASS_NAME  C (64),;
      FUNCTION    C (254),;
      PRICE_R_R   N(9,2),;
      PRICE_R_D   N(9,2) )
      
USE (rez_file) ALIAS REZ_TABLE
SELECT 0
USE (base_path+"CLASSES") SHARED AGAIN ORDER TAG CODE
SELECT 0
USE (path_comm+"SUBJECT") SHARED AGAIN ORDER TAG STO_CODE
SELECT 0
USE (base_path+"STOCK") SHARED AGAIN ORDER TAG NAME
SET RELATION TO CODE  INTO SUBJECT
SET RELATION TO CLASS INTO CLASSES ADDITIVE

DO Wt_Mess WITH "Подготовка данных..."

m.tmpCount = 0
SCAN
  m.tmpCount = m.tmpCount+1
  IF m.tmpCount % 100 = 0
    m.tmpStr = PADC(ALLTRIM(STR(m.tmpCount)), WCOLS(WOUTPUT()))
    @ wROWS()-1, 0 SAY m.tmpStr
  ENDIF
  SELECT REZ_TABLE
  APPEND BLANK
  REPLACE STO_CODE    WITH STOCK.CODE,      ;
          PREFIX      WITH STOCK.PREFIX,    ;
          NAME        WITH STOCK.NAME,      ;
          PRODUCER    WITH STOCK.PRODUCER,  ;
          CLASS_NAME  WITH CLASSES.NAME,    ;
          FUNCTION    WITH SUBJECT.FUNCTION,;
          PRICE_R_R   WITH STOCK.PRICE_R,   ;
          PRICE_R_D   WITH STOCK.PRICE
  SELECT STOCK
ENDSCAN

DO Wt_Mess

CLOSE DATABASES

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Make_F5      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                        Создание файла COMM.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03.07.2004 ──┘
PROCEDURE Make_F5

PRIVATE tmpCount, tmpStr
CREATE DBF (rez_file) ;
   (  STO_CODE  N( 7), ;
      QNT_COMM  C(254)  )
USE (rez_file) ALIAS REZ_TABLE
SELECT 0
USE (path_comm+"SUBJECT") SHARED AGAIN ORDER TAG STO_CODE
SELECT 0
USE (base_path+"STOCK") SHARED AGAIN ORDER TAG NAME
SET RELA TO CODE INTO SUBJECT

DO Wt_Mess WITH "Подготовка данных..."

m.tmpCount = 0
SCAN
  m.tmpCount = m.tmpCount+1
  IF m.tmpCount % 100 = 0
    m.tmpStr = PADC(ALLTRIM(STR(m.tmpCount)), WCOLS(WOUTPUT()))
    @ wROWS()-1, 0 SAY m.tmpStr
  ENDIF
  SELECT REZ_TABLE
  APPEND BLANK
  REPLACE STO_CODE WITH STOCK.CODE, ;
          QNT_COMM WITH SUBJECT.FUNCTION
  SELECT STOCK
ENDSCAN

DO Wt_Mess

CLOSE DATABASES

RETURN
