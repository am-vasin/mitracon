*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Exp_Sait     Разработчик Галина Дмитриенко                 │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│          Экспорт списка, сформированного по интернет-заказу.           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 30.08.2002 ──┘
PROCEDURE Exp_Sait

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
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

PRIVATE exp_ord, nn

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
s_sav = SELECT()

SELECT 0
DELETE FILE(tmpo_path+"EXP_SAIT.DBF")
CREATE DBF(tmpo_path+"EXP_SAIT");
       (CUST_CODE N( 8),  ;
        ORD_ID    N( 9),  ;
        CODE      N( 7),  ;  
        PREFIX    C( 4),  ;
        NAME      C(35),  ;
        PRODUCER  C( 8),  ;
        QTY       N(10),  ;
        BILL_NUM  C(10),  ;
        BILL_DATE D,      ;
        QNT_SAVE  N(10),  ;
        MONEY_R   N(12,2),;
        STATUS    C( 1) ) 

SELECT 0
USE(base_path+"STOCK") ORDER TAG CODE SHARED ALIAS ST_2830

SELECT 0
USE(base_path+"INCMONEY") ORDER TAG BILL_NUM SHARED ALIAS MON_2830

SELECT 0
USE(base_path+"BILLS") ORDER TAG DOC_NUM SHARED ALIAS BIL_2830

SELECT 0
USE(base_path+"LIST_DET") ORDER TAG LIST SHARED ALIAS LD_2830
SET RELATION TO CODE INTO ST_2830

SELECT 0
USE(base_path+"LIST_TIT") ORDER TAG LST_NUM SHARED ALIAS LT_2830
COUNT TO exp_ord FOR REQ_TYPE = "E"
IF exp_ord = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет ни одного списка по интернет-заказам!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"

  SELECT ST_2830
  USE
  SELECT BIL_2830
  USE
  SELECT LD_2830
  USE
  SELECT LT_2830
  USE
  SELECT EXP_SAIT
  USE
  DELETE FILE(tmpo_path+"EXP_SAIT.DBF")
  SELECT(s_sav)
  RETURN
ENDIF

SET RELATION TO BILL_FIRM+BILL_YEAR+BILL_NUM INTO BIL_2830
SET FILTER TO REQ_TYPE ="E"
nn = 0
SCAN
  m.nn = m.nn+1
  IF m.nn%10 = 0
    WAIT WINDOW ALLTRIM(STR(m.nn))+"/"+ALLTRIM(STR(m.exp_ord)) NOWAIT
  ENDIF
  SELECT LD_2830
  SCAN FOR LIST_NUM = STR(LT_2830.LST_NUM,6)
    SELECT EXP_SAIT
    APPEND BLANK
    REPLACE CUST_CODE WITH LT_2830.CLI_CODE, ;
            ORD_ID    WITH VAL(LT_2830.REQ_DOC),;
            CODE      WITH LD_2830.CODE,     ;  
            PREFIX    WITH ST_2830.PREFIX,   ;
            NAME      WITH ST_2830.NAME,     ;
            PRODUCER  WITH ST_2830.PRODUCER, ;
            QTY       WITH LD_2830.QNT,      ;
            BILL_NUM  WITH LT_2830.BILL_NUM, ;
            BILL_DATE WITH BIL_2830.DOC_DATE,;
            QNT_SAVE  WITH LD_2830.QNT_REQ,  ;
            STATUS    WITH IIF(EMPTY(LT_2830.SAL_NUM),"","*")
    SELECT BIL_2830
    IF SEEK(LT_2830.BILL_FIRM+LT_2830.BILL_YEAR+LT_2830.BILL_NUM, "BIL_2830")
      IF STATUS = "Оплачено"
        SELECT MON_2830
        IF SEEK(BIL_2830.FIRM+LEFT(DTOS(BIL_2830.DOC_DATE),4)+BIL_2830.DOC_NUM, "MON_2830")
          SELECT EXP_SAIT
          REPLACE MONEY_R WITH MON_2830.MONEY_R
        ENDIF
      ENDIF
    ENDIF
    SELECT LD_2830
  ENDSCAN
  SELECT LT_2830
ENDSCAN

*
*   Аккуратненько (аккуратненько!) закрываем DBF-файлы.
*
SELECT LT_2830
USE
SELECT LD_2830
USE
SELECT ST_2830
USE
SELECT BIL_2830
USE
SELECT EXP_SAIT
DO Save_DBF
USE
DELETE FILE(tmpo_path+"EXP_SAIT.DBF")

SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Save_DBF     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Сохранение файла.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 13.11.2001 ──┘
PROCEDURE Save_DBF
PRIVATE tmpDir, tmpFile, mss, tmpName, r_sav
tmpDir = ""
tmpFile = ""

DO WHILE .T.
  tmpName = New_File(tmpDir, "dbf", "Укажите файл",tmpFile)
  IF EMPTY(tmpName)
    RETURN
  ENDIF
  IF File_O(tmpName)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"  Такой файл уже существует. Удалим? [ Да ] [ Нет ]"
    mss(3) = ""
    IF Out_Mess(7, "mss") = 1
      DELETE FILE (tmpName)
    ELSE
      LOOP
    ENDIF
  ENDIF
  EXIT
ENDDO

r_sav = RECNO()
COPY TO (tmpName)
IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF

RETURN