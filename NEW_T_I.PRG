*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                     Новая проверка накладных.                         ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE New_T_I
PARAMETER vis

PRIVATE cnt, ab, sch, err1, err2, err3, is_data, log_rec, err_cnt

log_rec = Use_Log("Проверка накладных")

sch = SCHEME(13,1)+","+ ;
      SCHEME(13,2)+","+ ;
      SCHEME(13,2)+","+ ;
      SCHEME(13,4)+","+ ;
      SCHEME(13,5)+","+ ;
      SCHEME(13,6)+","+ ;
      SCHEME(13,7)+","+ ;
      SCHEME(13,8)+","+ ;
      SCHEME(13,9)

IF FILE_O(log_path+"N_T_I0.DBF")   && Проверка была прервана, продолжаем...
  USE (log_path+"N_T_I0") EXCLUSIVE
  GO BOTTOM
  cnt = NUMB
ELSE                               && Начинаем с начала...
  CREATE DBF (log_path+"N_T_I0")  ;
     (  NUMB     N( 7),  ;
        FIRM     C( 3),  ;
        DOC_NUM  C(11),  ;
        DOC_DATE D,      ;
        DOC_NAME C(16),  ;
        SUM_TOT  N(13,2),  ;
        SUM_CNT  N(13,2),  ;
        SUM_TOTD N(13,4),  ;
        SUM_CNTD N(13,4),  ;
        SUM_TOT0 N(13,2),  ;
        SUM_CNT0  N(13,2),  ;
        IS_DEF   C( 3)   )
  USE (log_path+"N_T_I0") EXCLUSIVE
  cnt = 0
  APPEND BLANK
ENDIF

*
*   Открываем файлы, которые нужны при проверке наличия
*

SELECT 0
USE (base_path+"SALE") ORDER TAG DOC_NUM

SELECT 0
USE (base_path+"INCMONEY") ORDER TAG DOC_NUM

SELECT 0
USE (base_path+"SALE_TIT")

IF cnt # 0
  GO cnt
ENDIF

SELECT N_T_I0

PRIVATE ln, wd, tt, ft, w1, w2, w3, res
IF vis
  DIMENSION tt(3), ft(1)
  IF SET("CENTURY") = "ON"
    tt(1) = ;
    "───────┬───┬───────────┬──────────┬────────────────┬───"
    tt(2) = ;
    " N/пор │Фрм│ N Док-та  │   Дата   │   Клиент       │Ош."
    tt(3) = ;
    "───────┼───┼───────────┼──────────┼────────────────┼───"
    ft(1) = ;
    "───────┴───┴───────────┴──────────┴────────────────┴───"
  ELSE
    tt(1) = ;
    "───────┬───┬───────────┬────────┬────────────────┬───"
    tt(2) = ;
    " N/пор │Фрм│ N Док-та  │  Дата  │   Клиент       │Ош."
    tt(3) = ;
    "───────┼───┼───────────┼────────┼────────────────┼───"
    ft(1) = ;
    "───────┴───┴───────────┴────────┴────────────────┴───"
  ENDIF

  ln = WROWS("")-10
  wd = LEN(ft(1))
  DO Def_Win WITH "Tst_I_M", "Tst_I_E", "Tst_I_I", ln, wd, ;
                  "Проверка накладных", tt, ft

  @ 1, 1 SAY "┌"
  @ 2, 1 SAY "│"
  @ 3, 1 SAY "├"
  @ WROWS()-1, 1 SAY "└"

  @ 1, WCOLS()-2 SAY "┐"
  @ 2, WCOLS()-2 SAY "│"
  @ 3, WCOLS()-2 SAY "┤"
  @ WROWS()-1, WCOLS()-2 SAY "┘"

  ACTIVATE WINDOW Tst_I_E
  GO TOP
  BROWSE FIELDS NUMB:H="",     ;
                FIRM:H="",     ;
                DOC_NUM:H="",  ;
                DOC_DATE:H="", ;
                DOC_NAME:H="", ;
                IS_DEF:H=""    ;
                NOWAIT WINDOW Tst_I_I IN WINDOW Tst_I_E COLOR (sch)
  ON KEY ab = .T.
ENDIF
PRIVATE cd00, pr00, q00
ab = .F.
GO BOTTOM
DO WHILE .NOT. EOF("SALE_TIT")
  err1 = .F.
  err2 = .F.
  err3 = .F.
  w1 = 0
  w2 = 0
  w3 = 0
  REPLACE NUMB     WITH RECNO("SALE_TIT"),  ;
          FIRM     WITH SALE_TIT.FIRM,      ;
          DOC_NUM  WITH LEFT(SALE_TIT.DOC_NUM,4)+"-"+  ;
                        ALLTRIM(SUBSTR(SALE_TIT.DOC_NUM,5)),  ;
          DOC_DATE WITH SALE_TIT.DOC_DATE,  ;
          DOC_NAME WITH SALE_TIT.CUS_NAME,  ;
          SUM_TOT  WITH SALE_TIT.TOTAL_R,   ;
          SUM_TOTD WITH SALE_TIT.TOTAL_D,   ;
          SUM_TOT0 WITH SALE_TIT.TOTAL_0,   ;
          SUM_CNT  WITH 0,                  ;
          IS_DEF   WITH " "
  IF ab
    EXIT
  ENDIF
  IF vis
    SHOW WINDOW N_T_I0 IN WINDOW Tst_I_E SAME REFRESH
  ENDIF
  SELECT SALE
  SEEK SALE_TIT.FIRM+LEFT(DTOS(SALE_TIT.DOC_DATE),4)+SALE_TIT.DOC_NUM
  cd00 = -1
  pr00 = -1
  q00  =  0
  SCAN REST WHILE SALE_TIT.DOC_DATE = DOC_DATE .AND. ;
                  SALE_TIT.FIRM = FIRM .AND. ;
                  SALE_TIT.DOC_NUM = DOC_NUM
    IF CODE = cd00 .AND. pr00 = PRICE_R
      q00 = q00+QNT
    ELSE
      w1 = w1+ROUND(pr00*q00,2)
      q00 = QNT
      cd00 = CODE
      pr00 = PRICE_R
    ENDIF
    w2 = w2+QNT*PRICE_D
    w3 = w3+QNT*PRICE_0
  ENDSCAN
  w1 = w1+ROUND(pr00*q00,2)
  res = ""
*  res = "√"
*  DO CASE
*  CASE SALE_TIT.KASSA = 0 .AND. EMPTY(SALE_TIT.PAY_DOC)
*    res = " "
*  CASE SEEK(SALE_TIT.PAY_TYPE+LEFT(DTOS(SALE_TIT.PAY_DATE),4)+SALE_TIT.PAY_DOC,"INCMONEY")
*    IF INCMONEY.MONEY_R = SALE_TIT.KASSA
*      res = " "
*    ENDIF
*  ENDCASE
*
*     С тех пор, как стали разбивать приходники - это не криминал!
*
*
  res = IIF(w1 = SALE_TIT.TOTAL_R, " ", "√") + ;
        IIF(w2 = SALE_TIT.TOTAL_D, " ", "√") + ;
        IIF(w3 = SALE_TIT.TOTAL_0, " ", "√")
  SELECT N_T_I0
  REPLACE SUM_CNT  WITH w1,  ;
          SUM_CNTD WITH w2,  ;
          SUM_CNT0 WITH w3,  ;
          IS_DEF   WITH IIF(EMPTY(res), "OK", res)
  IF vis
    SHOW WINDOW N_T_I0 IN WINDOW Tst_I_E SAME REFRESH
  ENDIF

  IF .NOT. EMPTY(res)
    APPEND BLANK
  ENDIF
  SKIP IN SALE_TIT

ENDDO
ON KEY
IF vis
  = INKEY("M")
ENDIF

is_data = .F.
IF EOF("SALE_TIT")
  is_data = .T.
  DELETE
  DELETE FILE (log_path+"N_T_I.DBF")
  COPY TO (log_path+"N_T_I.DBF") FOR .NOT. DELETED()
  USE (log_path+"N_T_I.DBF")
  err_cnt = RECCOUNT()
  USE
  DO Term_Log WITH log_rec, err_cnt
  DELETE FILE (log_path+"N_T_I0.DBF")
ENDIF

CLOSE DATABASES
IF vis
  RELEASE WINDOW Tst_I_M
  RELEASE WINDOW Tst_I_E
  RELEASE WINDOW Tst_I_I
ENDIF

RETURN is_data
