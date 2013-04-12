*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║         Подготовка данных для экспорта в бухгалтерию.                 ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Export_B

PRIVATE dat_0, dat_1, dat_v0, dat_v1, ex, c_name, p_numb, p_line
PRIVATE mss

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
PRIVATE ln, ln1, l_ex, wk_list
USE PLACES
p_numb = RECCOUNT()
p_line = ""
FOR dat_1 = 1 TO p_numb
  p_line = p_line+CHR(ASC("A")+dat_1-1)
ENDFOR

USE PARMS
dat_0  = DAT_START
dat_v0 = DAT_START
USE
dat_1  = DATE()
dat_v1 = DATE()

DIMENSION wk_list(p_numb)
FOR ex = 1 TO p_numb
  wk_list(ex) = CHR(ASC("A")+ex-1) $ p_line
ENDFOR

ex   = 1

*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
*
*┌────────────────────────────────────────────────────────────────┐
*│ ┌─ Интервал ────────────────┐ ┌─ Площадки ───────────────────┐ │
*│ │                           │ │                              │ │
*│ │ Начальная дата ДД.ММ.ГГГГ │ │ ( ) ....:....!....:....!.... │ │
*│ │                           │ │                              │ │
*│ │ Конечная дата  ДД.ММ.ГГГГ │ │                              │ │
*│ │                           │ │                              │ │
*│ └───────────────────────────┘ └──────────────────────────────┘ │
*│               < OK Ctrl-W > < Отказаться Esc >                 │
*└────────────────────────────────────────────────────────────────┘

ln1  = p_numb+4
ln   = ln1+4
l_ex = 6+p_numb

DO D_Win_N WITH ln, 68, "Фильтр"

DO Sun_Bord WITH 2, 3, 8, 31
@  2, 5 SAY " Интервал "

DO Sun_Bord WITH 2, 32, 2+ln1-1, 64
@  2, 34 SAY " Площадки "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 4,  5 SAY "Начальная дата" GET dat_0 PICTURE "@D"
@ 6,  5 SAY "Конечная дата " GET dat_1 PICTURE "@D"
DO G_Place WITH 4, 35, wk_list

@ l_ex, 17 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID V_Dat()

POP KEY
RELEASE WINDOW (win_name)
IF ex = 2
  RETURN
ENDIF
PRIVATE mss

DO Wt_Mess WITH "Подборка данных..."

IF File_O(tmpo_path+"EXPORT_B.DBF")
  DELETE FILE (tmpo_path+"EXPORT_B.DBF")
ENDIF
SET EXCLUSIVE ON
IF .F.    &&&&&&&&&&&&&&&&&&& До правильной версии ИНФОБУХГАЛТРа
  CREATE DBF (tmpo_path+"EXPORT_B.DBF") ;
    ( OPER_NUMB  N( 6), ;
      WORK_PLACE N( 2), ;
      DATE       D,     ;
      DOCUMENT   C(15), ;
      COMMENT   C(250), ;
      DEBIT      C(20), ;
      CREDIT     C(20), ;
      SUM        N(15,2), ;
      VALUE      N(15,3)  )
ELSE
  CREATE DBF (tmpo_path+"EXPORT_B.DBF") ;
    ( OPER_NUMB  N( 6), ;
      WORK_PLACE N( 2), ;
      DATE       D,     ;
      DOCUMENT   C( 8), ;
      COMMENT_1  C(48), ;
      COMMENT_2  C(48), ;
      DEBIT      C(20), ;
      CREDIT     C(20), ;
      SUM        N(15,2), ;
      VALUE      N(15,3)  )
ENDIF

USE (tmpo_path+"EXPORT_B.DBF")
SET EXCLUSIVE OFF

SELECT 0
USE ACCOUNT ORDER TAG CUS_CODE

SELECT 0
USE INCMONEY ORDER TAG DOC_NUM

SELECT 0
USE SALE_TIT ORDER TAG HRON

SEEK DTOS(dat_0)
IF .NOT. FOUND()
  ex = RECNO(0)
  IF ex > 0 .AND. ex <= RECCOUNT()
    GO ex
  ELSE
    GO BOTTOM
    ex = 0
  ENDIF
ELSE
  ex = RECNO()
ENDIF
IF ex # 0
  SCAN REST WHILE DOC_DATE <= dat_1 FOR LEFT(DOC_NUM,1) $ p_line
    IF NEW_TYPE # " " .OR. TOTAL_R = 0
      LOOP
    ENDIF
    c_name = IIF(SEEK(CUS_CODE,"ACCOUNT"),  ;
             ACCOUNT.CUS_NAME, CUS_NAME)
    SELECT EXPORT_B
    APPEND BLANK
    IF .F.    &&&&&&&&&&&&&&&&&&& До правильной версии ИНФОБУХГАЛТРа
      REPLACE OPER_NUMB  WITH RECNO(),               ;
              WORK_PLACE WITH 1,                     ;
              DOCUMENT   WITH IIF(SALE_TIT.NEW_TYPE="0", "*"," ")+  ;
          LEFT(SALE_TIT.NEW_DOC,1)+ALLTRIM(SUBSTR(SALE_TIT.NEW_DOC,2)), ;
              DATE       WITH SALE_TIT.DOC_DATE,     ;
              COMMENT    WITH "Отпущено:"+CHR(13)+"   "+c_name,  ;
              DEBIT      WITH "62", ;
              CREDIT     WITH "46/РОС", ;
              SUM        WITH SALE_TIT.TOTAL_R
    ELSE
      REPLACE OPER_NUMB  WITH RECNO(),               ;
              WORK_PLACE WITH 1,                     ;
              DOCUMENT   WITH IIF(SALE_TIT.NEW_TYPE="0", "*"," ")+  ;
          LEFT(SALE_TIT.NEW_DOC,1)+ALLTRIM(SUBSTR(SALE_TIT.NEW_DOC,2)), ;
              DATE       WITH SALE_TIT.DOC_DATE,     ;
              COMMENT_1  WITH "Отпущены комплектующие",  ;
              COMMENT_2  WITH "   "+c_name,  ;
              DEBIT      WITH "62", ;
              CREDIT     WITH "46/РОС", ;
              SUM        WITH SALE_TIT.TOTAL_R
    ENDIF
    SELECT SALE_TIT
  ENDSCAN
ENDIF

SELECT INCMONEY
SCAN FOR BETWEEN(ENTER_DAT, dat_0, dat_1) .AND. DOC_TYPE = "1" .AND. LEFT(DOC_NUM,1) $ p_line
  c_name = IIF(SEEK(CUS_CODE,"ACCOUNT"),  ;
           ACCOUNT.CUS_NAME, CUS_NAME)
  SELECT EXPORT_B
  APPEND BLANK
  IF .F.    &&&&&&&&&&&&&&&&&&& До правильной версии ИНФОБУХГАЛТРа
    REPLACE OPER_NUMB  WITH RECNO(),             ;
            DATE       WITH INCMONEY.ENTER_DAT,  ;
            WORK_PLACE WITH 1,                     ;
            DOCUMENT   WITH ALLTRIM(INCMONEY.ENTER_DOC), ;
            COMMENT    WITH "Пост. на р/с:"+CHR(13)+"   "+c_name,  ;
            DEBIT      WITH "51", ;
            CREDIT     WITH "62", ;
            SUM        WITH INCMONEY.MONEY_R
  ELSE
    REPLACE OPER_NUMB  WITH RECNO(),             ;
            DATE       WITH INCMONEY.ENTER_DAT,  ;
            WORK_PLACE WITH 1,                     ;
            DOCUMENT   WITH ALLTRIM(INCMONEY.ENTER_DOC), ;
            COMMENT_1  WITH "Пост. на р/с за комплектующие",  ;
            COMMENT_2  WITH "   "+c_name,  ;
            DEBIT      WITH "51", ;
            CREDIT     WITH "62", ;
            SUM        WITH INCMONEY.MONEY_R
  ENDIF
  SELECT INCMONEY
ENDSCAN

SET ORDER TO TAG HRON
SEEK DTOS(dat_0)
IF .NOT. FOUND()
  ex = RECNO(0)
  IF ex > 0 .AND. ex <= RECCOUNT()
    GO ex
  ELSE
    GO BOTTOM
    ex = 0
  ENDIF
ELSE
  ex = RECNO()
ENDIF
IF ex # 0
  SCAN REST WHILE DOC_DATE <= dat_1 FOR LEFT(DOC_NUM,1) $ p_line .AND. DOC_TYPE = " "
    c_name = IIF(SEEK(CUS_CODE,"ACCOUNT"),  ;
             ACCOUNT.CUS_NAME, CUS_NAME)
    SELECT EXPORT_B
    APPEND BLANK
    IF .F.    &&&&&&&&&&&&&&&&&&& До правильной версии ИНФОБУХГАЛТРа
      REPLACE OPER_NUMB  WITH RECNO(),               ;
              DATE       WITH INCMONEY.DOC_DATE,     ;
              WORK_PLACE WITH 1,                     ;
              DOCUMENT   WITH LEFT(INCMONEY.DOC_NUM,1)+ALLTRIM(SUBSTR(INCMONEY.DOC_NUM,2)), ;
              COMMENT    WITH "Поступило в кассу: "+CHR(13)+"   "+c_name, ;
              DEBIT      WITH "50", ;
              CREDIT     WITH "62", ;
              SUM        WITH INCMONEY.MONEY_R
    ELSE
      REPLACE OPER_NUMB  WITH RECNO(),               ;
              DATE       WITH INCMONEY.ENTER_DAT,    ;
              WORK_PLACE WITH 1,                     ;
              DOCUMENT   WITH LEFT(INCMONEY.DOC_NUM,1)+ALLTRIM(SUBSTR(INCMONEY.DOC_NUM,2)), ;
              COMMENT_1  WITH "Приход в кассу: доплата к безналу", ;
              COMMENT_2  WITH "   "+c_name, ;
              DEBIT      WITH "50", ;
              CREDIT     WITH "62", ;
              SUM        WITH INCMONEY.MONEY_R
    ENDIF
    SELECT INCMONEY
  ENDSCAN
ENDIF

DO Wt_Mess

IF RECCOUNT("EXPORT_B") = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"За указанный Вами период не найдено ни одной операции..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
ELSE
  CLOSE DATABASES
  DO To_Disk WITH tmpo_path+"EXPORT_B.DBF"
ENDIF

CLOSE DATABASES
DELETE FILE (tmpo_path+"EXPORT_B.DBF")

RETURN

*********************************************************************
PROCEDURE V_Dat

IF ex = 2
  RETURN .T.
ENDIF

IF dat_0 < dat_v0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Имеем данные только с "+TRANSFORM(dat_v0,"@D")+"."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  dat_0 = dat_v0
  SHOW GETS
  RETURN .F.
ENDIF

IF dat_1 > dat_v1
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Сегодня только "+TRANSFORM(dat_v1,"@D")+"."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  dat_1 = dat_v1
  SHOW GETS
  RETURN .F.
ENDIF
PRIVATE l_tmp, i

l_tmp = ""
FOR i = 1 TO p_numb
  IF wk_list(i)
    l_tmp = l_tmp+CHR(ASC("A")+i-1)
  ENDIF
ENDFOR

IF EMPTY(l_tmp)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Если Вас не интересует ни одна площадка?!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

p_line = l_tmp

RETURN .T.
