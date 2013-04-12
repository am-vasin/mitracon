*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                ฎข ๏ ฏเฎขฅเช  ก ซ ญแ  ฏฎ ชซจฅญโ ฌ.                    บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE New_T_C
PARAMETER vis

PRIVATE cnt, d_s, err, ab, sch, is_data, log_rec, err_cnt

log_rec = Use_Log("เฎขฅเช  คฅฏฎงจโฎข")

USE (base_path+"PARMS")
sch = SCHEME(13,1)+","+ ;
      SCHEME(13,2)+","+ ;
      SCHEME(13,2)+","+ ;
      SCHEME(13,4)+","+ ;
      SCHEME(13,5)+","+ ;
      SCHEME(13,6)+","+ ;
      SCHEME(13,7)+","+ ;
      SCHEME(13,8)+","+ ;
      SCHEME(13,9)
d_s = DAT_START

IF FILE_O(log_path+"N_T_C0.DBF") .AND. ;
   FILE_O(log_path+"N_T_CF.CDX") .AND. ;
   FILE_O(log_path+"N_T_CF.DBF")    && เฎขฅเช  ก๋ซ  ฏเฅเข ญ , ฏเฎคฎซฆ ฅฌ...
  
  USE (log_path+"N_T_CF") ORDER TAG FIRM EXCLUSIVE
  SELECT 0
  USE (log_path+"N_T_C0") EXCLUSIVE
  GO BOTTOM
  cnt = NUMB
ELSE                               &&  ็จญ ฅฌ แ ญ ็ ซ ...
  DELETE FILE (log_path+"N_T_CF.DBF")
  DELETE FILE (log_path+"N_T_CF.CDX")
  CREATE DBF (log_path+"N_T_CF")  ;
     (  FIRM      C( 3),    ;
        COUNT_R   N(15,2),  ;
        COUNT_D   N(15,4),  ;
        REST_R    N(15,2),  ;
        REST_D    N(15,4)  )
  INDEX ON FIRM TAG FIRM
  SELECT 0
  USE (base_path+"FIRMS")
  SCAN
    SELECT N_T_CF
    APPEND BLANK
    REPLACE FIRM WITH FIRMS.FIRM_CODE
    SELECT FIRMS
  ENDSCAN
  DELETE FILE (log_path+"N_T_C0.DBF")
  CREATE DBF (log_path+"N_T_C0")  ;
     (  CUS_CODE  N( 8),  ;
        NUMB      N( 7),  ;
        CUS_NAME  C(50),  ;
        DELTA_R   N(15,2), ;
        DELTA_D   N(15,4), ;
        IS_DEF    C( 4)    )
  USE (log_path+"N_T_C0") EXCLUSIVE
  cnt = 0
  APPEND BLANK
ENDIF

*
*   โชเ๋ข ฅฌ ไ ฉซ๋, ชฎโฎเ๋ฅ ญใฆญ๋ ฏเจ ฏเฎขฅเชฅ
*

SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG CUS_CODE
SET FILTER TO DOC_DATE >= d_s

SELECT 0
USE (base_path+"ACT_S_T") ORDER TAG SUP_CODE
SET FILTER TO DOC_DATE >= d_s

SELECT 0
USE (base_path+"INCMONEY") ORDER TAG CUS_CODE
SET FILTER TO DOC_DATE >= d_s

SELECT 0
USE (base_path+"CUS_REST") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"ACCOUNT")
IF cnt # 0
  GO cnt
ENDIF

SELECT N_T_C0
GO TOP

PRIVATE ln, wd, tt, ft, w1, w2, w3, res
IF vis
  DIMENSION tt(3), ft(1)
  tt(1) = ;
  "ฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤ"
  tt(2) = ;
  " N/ฏฎเ ณ         ฎชใฏ โฅซ์                               ณฅไ."
  tt(3) = ;
  "ฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤ"
  ft(1) = ;
  "ฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤ"

  ln = WROWS("")-10
  wd = LEN(ft(1))
  DO Def_Win WITH "Tst_C_M", "Tst_C_E", "Tst_C_I", ln, wd, ;
                  "เฎขฅเช  คฅฏฎงจโฎข", tt, ft

  @ 1, 1 SAY "ฺ"
  @ 2, 1 SAY "ณ"
  @ 3, 1 SAY "ร"
  @ WROWS()-1, 1 SAY "ภ"

  @ 1, WCOLS()-2 SAY "ฟ"
  @ 2, WCOLS()-2 SAY "ณ"
  @ 3, WCOLS()-2 SAY "ด"
  @ WROWS()-1, WCOLS()-2 SAY "ู"

  ACTIVATE WINDOW Tst_C_E


  BROWSE FIELDS NUMB:H="",     ;
                CUS_NAME:H="", ;
                IS_DEF:H=""    ;
                NOWAIT WINDOW Tst_C_I IN WINDOW Tst_C_E COLOR (sch)

  ON KEY ab = .T.
ENDIF
ab = .F.

GO BOTTOM
DO WHILE .NOT. EOF("ACCOUNT")
  err = .F.
  REPLACE CUS_CODE WITH ACCOUNT.CUS_CODE, ;
          NUMB     WITH RECNO("ACCOUNT"), ;
          CUS_NAME WITH ACCOUNT.CUS_NAME, ;
          DELTA_R  WITH 0, ;
          DELTA_D  WITH 0, ;
          IS_DEF   WITH " "
  SELECT N_T_CF
  SCAN
    IF SEEK(STR(ACCOUNT.CUS_CODE)+N_T_CF.FIRM, "CUS_REST")
      REPLACE COUNT_R WITH CUS_REST.ACC_S_S,  ;
              COUNT_D WITH CUS_REST.ACC_SD_S, ;
              REST_R  WITH CUS_REST.ACC_SUM+CUS_REST.ACC_S_H+CUS_REST.REST_SUM,  ;
              REST_D  WITH CUS_REST.ACC_SUMD+CUS_REST.ACC_SD_H
    ELSE
      REPLACE COUNT_R WITH 0, ;
              COUNT_D WITH 0, ;
              REST_R  WITH 0, ;
              REST_D  WITH 0
    ENDIF
  ENDSCAN
  IF ab
    EXIT
  ENDIF
  IF vis
    SHOW WINDOW N_T_C0 IN WINDOW Tst_C_E SAME REFRESH
  ENDIF
  DO Cnt_Rest     &&  แ็ฅโ ฎแโ โ ช 
  SELECT N_T_CF
*  LOCATE FOR COUNT_D # REST_D .OR. COUNT_R # REST_R
  SUM ABS(COUNT_D - REST_D), ABS(COUNT_R - REST_R) TO s00_d, s00_r
*  IF FOUND()
  IF s00_r+s00_d # 0
    err = .T.
    res = " ๛ "
  ELSE
    res = " "
  ENDIF
  SELECT N_T_C0
  REPLACE IS_DEF WITH IIF(EMPTY(res), "OK", res), ;
          DELTA_R WITH s00_r, ;
          DELTA_D WITH s00_d
  IF vis
    SHOW WINDOW N_T_C0 IN WINDOW Tst_C_E SAME REFRESH
  ENDIF
  IF err
    APPEND BLANK
  ENDIF
  SKIP IN ACCOUNT
ENDDO
ON KEY
IF vis
  = INKEY("M")
ENDIF

is_data = .F.
IF EOF("ACCOUNT")
  is_data = .T.
  DELETE FILE (log_path+"N_T_C.DBF")
  COPY TO (log_path+"N_T_C.DBF") FOR "๛" $ IS_DEF
  USE (log_path+"N_T_C.DBF")
  err_cnt = RECCOUNT()
  USE
  DO Term_Log WITH log_rec, err_cnt
  CLOSE DATABASES
  DELETE FILE (log_path+"N_T_C0.DBF")
  DELETE FILE (log_path+"N_T_CF.DBF")
  DELETE FILE (log_path+"N_T_CF.CDX")
ELSE
  CLOSE DATABASES
ENDIF
IF vis
  RELEASE WINDOW Tst_C_M
  RELEASE WINDOW Tst_C_E
  RELEASE WINDOW Tst_C_I
ENDIF

RETURN is_data

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                    ฎคแ็ฅโ ฎแโ โช  ญ  คฅฏฎงจโฅ.                       บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Cnt_Rest

SELECT INCMONEY
SEEK N_T_C0.CUS_CODE
SCAN REST WHILE CUS_CODE = N_T_C0.CUS_CODE
  IF .NOT. SEEK(FIRM, "N_T_CF")
    IF vis
*      WAIT WINDOW "—โฎ-โฎ ญฅ โ ช แ ไจเฌฎฉ !"+FIRM +"!"
    ENDIF
    LOOP
  ENDIF
  DO CASE
  CASE DOC_TYPE $ " 1—"    && / จซจ ฏเจๅ. ฎเคฅเ จซจ ็ฅช
    REPLACE N_T_CF.COUNT_R WITH N_T_CF.COUNT_R+MONEY_R
  CASE DOC_TYPE $ "34"      && USD
    REPLACE N_T_CF.COUNT_D WITH N_T_CF.COUNT_D+MONEY_D
  ENDCASE
ENDSCAN

SELECT SALE_TIT
SEEK N_T_C0.CUS_CODE
SCAN REST WHILE CUS_CODE = N_T_C0.CUS_CODE
  IF .NOT. SEEK(FIRM, "N_T_CF")
    IF vis
*      WAIT WINDOW "—โฎ-โฎ ญฅ โ ช แ ไจเฌฎฉ "+FIRM
    ENDIF
    LOOP
  ENDIF
  REPLACE N_T_CF.COUNT_R WITH N_T_CF.COUNT_R-TOTAL_R
ENDSCAN

SELECT ACT_S_T
SEEK N_T_C0.CUS_CODE
SCAN REST WHILE SUP_CODE = N_T_C0.CUS_CODE
  IF .NOT. SEEK(FIRM, "N_T_CF")
    IF vis
*      WAIT WINDOW "—โฎ-โฎ ญฅ โ ช แ ไจเฌฎฉ "+FIRM
    ENDIF
    LOOP
  ENDIF
  IF VALUTA = "$"                && ใฏจซจ ง  คฎซซ เ๋
    REPLACE N_T_CF.COUNT_D WITH N_T_CF.COUNT_D+TOTAL_H
  ELSE
    REPLACE N_T_CF.COUNT_R WITH N_T_CF.COUNT_R+TOTAL_H
  ENDIF
ENDSCAN

RETURN
