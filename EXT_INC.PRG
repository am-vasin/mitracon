*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Ext_Inc      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                   Обработка файла прихода поставщика                   ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 11.12.2005 ══╝
PROCEDURE Ext_Inc
PARAMETERS inc_ID	&& номер заготовки

PRIVATE mss, s_sav, fld_list, mode_tch, tmpSCode, tmpNDS, st_rec, st_tag
m.mode_tch = .T.
PRIVATE FName
m.FName = ""
m.st_rec = 0
m.st_tag = ""
m.s_sav = SELECT()
SELECT 0
USE (m.base_path+"INC_LIST") SHARED AGAIN ORDER TAG L_CODE ALIAS IL_5C11
SEEK STR(m.inc_ID, 5)
LOCATE REST FOR CODE # 0 .AND. LIST_NO = STR(m.inc_ID, 5)
IF FOUND()
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"Заготовка не пуста, ввод данных запрещен."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (m.s_sav)
  RETURN
ENDIF

DIMENSION fld_list[6]
m.fld_list = ""
SELECT 0
IF .NOT. FilStru()
  USE IN IL_5C11
  SELECT (m.s_sav)
  RETURN
ENDIF

IF .NOT. Fil_Tmp()
  USE IN IL_5C11
  IF USED("LD")
    m.FName = DBF("LD")
    USE IN LD
    DELETE FILE (m.FName)
    DELETE FILE (LEFT(m.FName, LEN(m.FName)-3)+".cdx")
  ENDIF
  SELECT (m.s_sav)
  RETURN
ENDIF

USE (m.base_path+"INC_TIT") SHARED AGAIN ALIAS INC_T_5C12 IN 0
GO m.inc_ID
m.tmpSCode = INC_T_5C12.SUP_CODE
m.tmpNDS   = INC_T_5C12.NDS_
USE IN INC_T_5C12
SELECT STOCK
IF USED("STOCK")
  m.st_rec = RECNO("STOCK")
  m.st_tag = TAG()
ELSE
  USE (m.base_path+"STOCK") SHARED AGAIN ORDER TAG CODE IN 0
ENDIF
DO WHILE .T.
  SELECT LD
  DO Strng WITH m.tmpSCode
  SELECT LD
  LOCATE FOR EMPTY(CODE)
  IF FOUND()
    DIMENSION mss[4]
    mss[1] = ""
    mss[2] = CHR(0)+"ВНИМАНИЕ! Не все позиции распознаны."
    mss[3] = CHR(0)+"  [ Продолжить распознавание ] [ Игнорировать нераспознанные позиции ]"
    mss[4] = ""
    IF Out_Mess(7, "mss") = 1
      LOOP
    ENDIF
    DELETE ALL FOR EMPTY(CODE)
  ENDIF
  EXIT
ENDDO

SELECT STOCK
SET ORDER TO TAG CODE
SELECT LD
SET RELATION TO CODE INTO STOCK
SCAN
  SELECT IL_5C11
  APPEND BLANK
  REPLACE LIST_NO	WITH STR(m.inc_ID, 5), ;
          CODE		WITH LD.CODE,          ;
          PREFIX	WITH LD.PREFIX,        ;
          NAME		WITH LD.NAME,          ;
          PRODUCER	WITH LD.PRODUCER,      ;
          QNT_INV	WITH LD.QNT,           ;
          QNT		WITH LD.QNT,           ;
          PRICE_0	WITH LD.PRICE,         ;
          PRICE		WITH LD.PRICE*(1+m.tmpNDS/100)
  SELECT LD
ENDSCAN

IF m.st_rec = 0
  USE IN STOCK
ELSE
  SELECT STOCK
  IF BETWEEN(m.st_rec, 1, RECCOUNT("STOCK"))
    GO m.st_rec
  ENDIF
  SET ORDER TO TAG (m.st_tag)
ENDIF
USE IN IL_5C11

IF USED("LD")
  m.FName = DBF("LD")
  USE IN LD
  DELETE FILE (m.FName)
  DELETE FILE (LEFT(m.FName, LEN(m.FName)-3)+".cdx")
ENDIF

SELECT (m.s_sav)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура FilStru      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                    Выбор файла и описание структуры                    │
*│                                                                        │
*└────────────────────────────────────────────────────────── 11.12.2005 ──┘
PROCEDURE FilStru

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
m.stat_type  = 1
m.what_do    = ""
m.menu_name  = ""
m.last_mouse = 0
m.win_name   = PROGRAM()

*------------------------------------------------------------------------
*    Содержательная часть программы:
*

*┌─────────────────────────────────────────────────────────────────┐
*│┌─ [ ] Имя файла ───────────────────────────────────────────────┐│
*││....:....!....:....!....:....!....:....!....:....!....:....!...││
*│└───────────────────────────────────────────────────────────────┘│
*│┌─ Описание полей ──────────────────────────────────────────────┐│
*││[ ] Код позиции  ....:....!.... [ ] Фирма        ....:....!....││
*││[ ] Префикс      ....:....!.... [ ] Количество   ....:....!....││
*││[ ] Наименование ....:....!.... [ ] Цена         ....:....!....││
*│└───────────────────────────────────────────────────────────────┘│
*│                 < OK Ctrl-W > < Отказаться Esc >                │
*└─────────────────────────────────────────────────────────────────┘

PRIVATE ex, sw_fil, sw_code, sw_prf, sw_name, sw_prd, sw_qnt, sw_prc, let_fl
PRIVATE tmpFld
DIMENSION tmpFld[1]
tmpFld[1] = ""
m.let_fl = .F.
m.FName = ""
m.ex = 1
STORE .F. TO sw_fil, m.sw_code, m.sw_prf, m.sw_name, m.sw_prd, m.sw_qnt, m.sw_prc
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 12, 69, "Опишите входной файл"
DO Sun_Bord WITH  2,  2,  4, 66, " [ ] Имя файла "
DO Sun_Bord WITH  5,  2,  9, 66, " Описание полей "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@  2,  5 GET m.sw_fil PICTURE "@*C Имя файла " VALID G_File()
@  3,  3 EDIT m.FName SIZE 1, 63 WHEN .F.
@  6,  3 GET sw_code PICTURE "@*C Код позиции" VALID G_Field(1, 6, 20, "N") WHEN m.let_fl
@  7,  3 GET sw_prf  PICTURE "@*C Префикс" VALID G_Field(2, 7, 20, "C") WHEN m.let_fl
@  8,  3 GET sw_name PICTURE "@*C Наименование" VALID G_Field(3, 8, 20, "C") WHEN m.let_fl
@  6, 35 GET sw_prd  PICTURE "@*C Фирма" VALID G_Field(4, 6, 52, "C") WHEN m.let_fl
@  7, 35 GET sw_qnt  PICTURE "@*C Количество" VALID G_Field(5, 7, 52, "N") WHEN m.let_fl
@  8, 35 GET sw_prc  PICTURE "@*C Цена" VALID G_Field(6, 8, 52, "N") WHEN m.let_fl
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_Fld()

IF m.ex = 1
  *
  * Отрабатываем бланк
  POP KEY
  RELEASE WINDOW (win_name)
  RETURN .T.
  
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN .F.


*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура G_File       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Выбор файла                                 │
*│                                                                        │
*└────────────────────────────────────────────────────────── 09.12.2005 ──┘
PROCEDURE G_File
PRIVATE w_sav, tmp_v, tmp_p, new, i, err_sav, err_no, s_sav, mss, ret_v

ret_v = .T.

m.w_sav = WOUTPUT()

m.tmp_p = RAT("\", m.FName)
IF m.tmp_p = 0
  m.tmp_p = ""
ELSE
  m.tmp_p = LEFT(m.FName, m.tmp_p-1)
ENDIF

m.tmp_v = Int_File(m.tmp_P, "dbf", "Укажите файл поставщика")

m.new = m.FName # m.tmp_v
IF .NOT. EMPTY(m.tmp_v)
  IF m.FName # m.tmp_v
    m.new = .T.
    m.FName = m.tmp_v
  ENDIF
ENDIF

ACTIVATE SCREEN
@ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 2
ACTIVATE WINDOW (m.w_sav)

IF .NOT. EMPTY(m.FName) .OR. m.new
  DIMENSION tmpFld[1, 2]
  tmpFld[1, 1] = ""
  tmpFld[1, 2] = "CN"
  m.s_sav = SELECT()
  SELECT 0
  m.err_sav = ON("ERROR")
  m.err_no = 0
  ON ERROR m.err_no = ERROR()
  USE (m.FName) ALIAS TMP_00000
  ON ERROR &err_sav
  IF m.err_no # 0
    DIMENSION mss[3]
    mss[1] = ""
    mss[2] = CHR(0)+"Не удается открыть таблицу. Ошибка "+ALLTRIM(STR(m.err_no))+"."
    mss[3] = ""
    DO Out_mess WITH 7, "mss"
    m.FName = ""
    m.ret_v = .F.
  ELSE
    m.i = 2
    DO WHILE .T.
      IF .NOT. EMPTY(FIELD(m.i-1))
        DIMENSION tmpFld[m.i, 2]
        tmpFld[m.i, 1] = FIELD(m.i-1)
        tmpFld[m.i, 2] = TYPE(FIELD(m.i-1))
      ELSE
        EXIT
      ENDIF
      m.i = m.i+1
    ENDDO
  ENDIF
  USE
  SELECT (m.s_sav)
ENDIF

IF EMPTY(m.FName) .OR. m.new
  m.fld_list = ""
  m.let_fl = .F.
  @ 6, 20 SAY SPACE(14)
  @ 7, 20 SAY SPACE(14)
  @ 8, 20 SAY SPACE(14)
  @ 6, 52 SAY SPACE(14)
  @ 7, 52 SAY SPACE(14)
  @ 8, 52 SAY SPACE(14)
ENDIF

SHOW GET m.FName
m.sw_fil = .F.
SHOW GET m.sw_fil
m.let_fl = .NOT. EMPTY(m.FName)

RETURN m.ret_v

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура G_Field      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                               Выбор поля                               │
*│                                                                        │
*└────────────────────────────────────────────────────────── 12.12.2005 ──┘
PROCEDURE G_Field
PARAMETERS tmpIndex, tmpLine, tmpCol, tmpType

PRIVATE arr_c, arr_n, i, n_c, n_n
STORE 0 TO m.n_c, m.n_n
FOR m.i = 1 TO ALEN(tmpFld, 1)
  IF m.i = 1 .OR. tmpFld[m.i, 2] $ "CN"
    m.n_n = m.n_n+1
    DIMENSION arr_n[m.n_n]
    arr_n[m.n_n] = tmpFld[m.i, 1]
  ENDIF
  IF m.i = 1 .OR. tmpFld[m.i, 2] $ "C"
    m.n_c = m.n_c+1
    DIMENSION arr_c[m.n_c]
    arr_c[m.n_c] = tmpFld[m.i, 1]
  ENDIF
ENDFOR

fld_list[m.tmpIndex] = Ch_Fld(IIF(m.tmpType = "N", "arr_n", "arr_c"), fld_list[m.tmpIndex])
@ m.tmpLine, m.tmpCol SAY PADR(fld_list[m.tmpIndex], 14)
STORE .F. TO sw_fil, m.sw_code, m.sw_prf, m.sw_name, m.sw_prd, m.sw_qnt, m.sw_prc
SHOW GETS
*SHOW GET m.sw_code

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Ch_Fld       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                               Выбор поля                               │
*│                                                                        │
*└────────────────────────────────────────────────────────── 12.12.2005 ──┘
PROCEDURE Ch_Fld
PARAMETERS arr_f, s_val

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
m.stat_type  = 1
m.what_do    = ""
m.menu_name  = ""
m.last_mouse = 0
m.win_name   = PROGRAM()

*------------------------------------------------------------------------
*    Содержательная часть программы:
*

*┌────────────────────────────────┐
*│                                │
*│< OK Ctrl-W > < Отказаться Esc >│
*└────────────────────────────────┘

PRIVATE ex, tmpVal
m.ex = 1
m.tmpVal = m.s_Val
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH MIN(20, ALEN(&arr_f)+6), 36, "Укажите поле"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 2, 2 GET m.tmpVal FROM &arr_f SIZE WROWS()-4, WCOLS()-4

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF m.ex # 1
  m.tmpVal = m.s_Val
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN m.tmpVal

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Tst_Fld      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                Проверка корректности заполнения бланка                 │
*│                                                                        │
*└────────────────────────────────────────────────────────── 12.12.2005 ──┘
PROCEDURE Tst_Fld

PRIVATE mss

IF m.ex # 1
  RETURN .T.
ENDIF

IF EMPTY(fld_list[3])
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"Поле наименования должно быть указано..."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF EMPTY(fld_list[5])
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"Поле количества должно быть указано..."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Fil_Tmp      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                 Создание и заполнение временного файла                 │
*│                                                                        │
*└────────────────────────────────────────────────────────── 12.12.2005 ──┘
PROCEDURE Fil_Tmp

PRIVATE tmpName, tmpFld, mss

m.tmpName = SYS(3)
SELECT 0
CREATE DBF (m.tmpo_path+m.tmpName) ;
	(	CODE		N( 7), ;
	    PREFIX		C( 4), ;
	    NAME		C(35), ;
	    PRODUCER	C( 8), ;
	    QNT			N(10), ;
	    PRICE		n(10,2) )
USE (m.tmpo_path+m.tmpName) EXCLUSIVE ALIAS LD

SELECT 0
USE (m.FName) SHARED AGAIN ALIAS TMP_SOURCE
SCAN
  SELECT LD
  APPEND BLANK

  IF .NOT. EMPTY(fld_list[1])
    m.tmpFld = "TMP_SOURCE."+ALLTRIM(fld_list[1])
    m.tmpFld = &tmpFld
    IF TYPE("m.tmpFld") = "C"
      m.tmpFld = VAL(m.tmpFld)
    ENDIF
    REPLACE CODE WITH m.tmpFld
  ENDIF

  IF .NOT. EMPTY(fld_list[2])
    m.tmpFld = "TMP_SOURCE."+ALLTRIM(fld_list[2])
    m.tmpFld = &tmpFld
    m.tmpFld = PADL(ALLTRIM(m.tmpFld), 4)
    REPLACE PREFIX WITH m.tmpFld
  ENDIF

  IF .NOT. EMPTY(fld_list[3])
    m.tmpFld = "TMP_SOURCE."+ALLTRIM(fld_list[3])
    m.tmpFld = &tmpFld
    REPLACE NAME WITH m.tmpFld
  ENDIF

  IF .NOT. EMPTY(fld_list[4])
    m.tmpFld = "TMP_SOURCE."+ALLTRIM(fld_list[4])
    m.tmpFld = &tmpFld
    REPLACE PRODUCER WITH m.tmpFld
  ENDIF

  IF .NOT. EMPTY(fld_list[5])
    m.tmpFld = "TMP_SOURCE."+ALLTRIM(fld_list[5])
    m.tmpFld = &tmpFld
    IF TYPE("m.tmpFld") = "C"
      m.tmpFld = VAL(m.tmpFld)
    ENDIF
    REPLACE QNT WITH m.tmpFld
  ENDIF

  IF .NOT. EMPTY(fld_list[6])
    m.tmpFld = "TMP_SOURCE."+ALLTRIM(fld_list[6])
    m.tmpFld = &tmpFld
    IF TYPE("m.tmpFld") = "C"
      m.tmpFld = VAL(m.tmpFld)
    ENDIF
    REPLACE PRICE WITH m.tmpFld
  ENDIF

  SELECT TMP_SOURCE

ENDSCAN

USE IN TMP_SOURCE

USE (m.base_path+"STOCK") SHARED AGAIN ORDER TAG CODE ALIAS ST_5C12 IN 0
SELECT LD
SCAN FOR .NOT. EMPTY(CODE)
  IF QNT =< 0
    DELETE
  ELSE
    IF .NOT. SEEK(CODE, "ST_5C12")
      DIMENSION mss[3]
      mss[1] = ""
      mss[2] = CHR(0)+"Список содержит позиции с некорректными кодами."
      mss[3] = ""
      DO Out_Mess WITH 7, "mss"
      USE IN ST_5C12
      RETURN .F.
    ENDIF
  ENDIF
ENDSCAN

SELECT ST_5C12
SET ORDER TO TAG NAME_P
SELECT LD
SCAN FOR EMPTY(CODE)
  IF EMPTY(NAME) .OR. QNT <= 0
    DELETE
  ELSE
    IF SEEK(NAME+PREFIX+PRODUCER, "ST_5C12")
      REPLACE CODE WITH ST_5C12.CODE
    ENDIF
  ENDIF
ENDSCAN

USE IN ST_5C12

RETURN .T.
