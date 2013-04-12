*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º ” ©« P_Book_S      §à ¡®âç¨ª €­¤à¥© ‚ á¨­           24.03.97 22:06:32 º
*ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
*º                                                                        º
*º                          ¥ç âì ª­¨£¨ ¯à®¤ ¦.                          º
*º                                                                        º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
PROCEDURE P_Book_S

PRIVATE ex, set_mrk, n_types, rc_sv, mss, k, tmpKpp

rc_sv = RECNO()

ex = 1
n_types = 2
set_mrk = .F.

DO Book_Lst.spr

IF ex = 1

  DO Wt_Mess WITH "®¤¡®àª  ¤ ­­ëå..."

  DO CASE
  CASE n_types = 1
    n_types = " *"
  CASE n_types = 2
    n_types = " "
  CASE n_types = 3
    n_types = "*"
  ENDCASE
  
  DELETE FILE (tmpo_path+"SAL_BOOK.DBF")
  DELETE FILE (tmpo_path+"SAL_BOOK.CDX")
  SELECT 0
  SET EXCLUSIVE ON
  CREATE DBF (tmpo_path+"SAL_BOOK.DBF")  ;
     ( DOC_NUM  C(10), ;
       DOC_DATE D,     ;
       CUS_NAME C(50) ,;
       INN      C(12), ;
       KPP      C(16), ;
       PAY_DATE D,     ;
       PRC      N(5,2),;
       SUM_TOV  N(12,2), ;
       SUM_NDS  N(12,2), ;
       SUM_H    N(12,2)   )   

  INDEX ON DTOS(DOC_DATE)+DOC_NUM TAG HRON
  SET EXCLUSIVE OFF
  USE (base_path+"CUS_BIL") SHARED AGAIN ALIAS CB_4407 ORDER TAG CUS_CODE IN 0

  SELECT TMP_LINK
  SCAN
    SELECT BILL_F
    k = IIF(EMPTY(HOLD_DATE), " ", "*")
    IF .NOT. k $ n_types
      SELECT TMP_LINK
      LOOP
    ENDIF
    tmpKpp = ""
    IF SEEK(CUS_CODE, "CB_4407")
      tmpKpp = CB_4407.KPP
    ENDIF
    SELECT SAL_BOOK
    APPEND BLANK
    REPLACE DOC_NUM  WITH BILL_F.DOC_NUM,  ;
            DOC_DATE WITH BILL_F.DOC_DATE, ;
            CUS_NAME WITH BILL_F.CUS_NAME, ;
            INN      WITH BILL_F.INN,      ;
            KPP      WITH tmpKpp,          ;
            PAY_DATE WITH BILL_F.ENTER_DATE,;
            PRC      WITH BILL_F.NDS_,     ;
            SUM_TOV  WITH BILL_F.TOTAL_0,  ;
            SUM_NDS  WITH BILL_F.TOTAL_NDS,;
            SUM_H    WITH BILL_F.TOTAL_R   
    SELECT TMP_LINK
    IF EMPTY(BILL_F.HOLD_DATE) .AND. set_mrk
      REPLACE BILL_F.HOLD_DATE WITH DATE()
    ENDIF
  ENDSCAN
  DO Wt_Mess
  SELECT SAL_BOOK
  IF RECCOUNT() = 0
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"‡  ãª § ­­ë© ¯¥à¨®¤ ­¥ ­ ©¤¥­® ­¨ ®¤­®£® áç¥â -ä ªâãàë!"
    mss(3) = ""
    DO Wt_Mess 
    DO Out_Mess WITH 7, "mss"
    USE
    DELETE FILE (tmpo_path+"SAL_BOOK_BN.DBF")
    DELETE FILE (tmpo_path+"SAL_BOOK_BN.CDX")
    SELECT TMP_LINK
    IF rc_sv > 0 .AND. rc_sv < RECCOUNT()
      GO rc_sv
    ENDIF
    RETURN
  ENDIF

  PRIVATE sb, p_ln, fnt, n_cp, lft, t_p, f_p, f_d, n, i, ffeed, p_drctry
  DIMENSION sb(14,2)

  *{Pgs}      - ‘âà ­¨æ /¢á¥£® áâà ­¨æ
  *{Dat1  }   -  ç «ì­ ï ¤ â 
  *{Dat2  }   - Š®­¥ç­ ï ¤ â 
  *{T001    } - ‘ã¬¬  ¯à®¤ ¦ ¡¥§ „‘ ­  áâà ­¨æ¥
  *{T002    } - ‘ã¬¬  „‘ ­  áâà ­¨æ¥
  *{T003    } - ‘ã¬¬  ¯à®¤ ¦ á „‘ ­  áâà ­¨æ¥
  *{T011    } - ‘ã¬¬  ¯à®¤ ¦ ¡¥§ „‘ ¯® ¤®ªã¬¥­âã
  *{T012    } - ‘ã¬¬  „‘ ¯® ¤®ªã¬¥­âã
  *{T013    } - ‘ã¬¬  ¯à®¤ ¦ á „‘ ¯® ¤®ªã¬¥­âã

  sb(1,1) = "{Pgs}"
  sb(1,2) = ""

  sb(2,1) = "{Dat1  }"
  sb(2,2) = TRANSFORM(dt1,"@D ")

  sb(3,1) = "{Dat2  }"
  sb(3,2) = TRANSFORM(dt2,"@D ")

  sb(4,1) = "{T001      }"
  sb(4,2) = "          "

  sb(5,1) = "{T002      }"
  sb(5,2) = "          "

  sb(6,1) = "{T003      }"
  sb(6,2) = "          "

  sb(7,1) = "{T011      }"
  sb(7,2) = "          "

  sb(8,1) = "{T012      }"
  sb(8,2) = "          "

  sb(9,1) = "{T013      }"
  sb(9,2) = "          "

  sb(10,1) = "{Firm_Name}"
  
  sb(11,1) = "{P_INN     }"
  
  SELECT 0
  USE (base_path+"FIRMS") AGAIN ALIAS FRM_1105 ORDER TAG FIRM_CODE
  SEEK f_c000
  sb(12, 1) = "{ãª®¢®¤¨â¥«ì          }"
  sb(12, 2) = BOSS
  sb(13, 1) = "{ƒ«. ¡ãå£ «â¥à         }"
  sb(13, 2) = ABAK
  sb(14, 1) = "{Š áá¨à                }"
  sb(14, 2) = KASSIR
  sb(10,2) = L_NAME
  sb(11,2) = INN
  USE (base_path+"DOC_FORM")
  LOCATE FOR "SAL_BOOK" == ALLTRIM(UPPER(DOC_NAME))

  p_ln = DOC_FORM.PAGE_LEN
  fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
  n_cp = DOC_FORM.N_COPIES
  lft  = DOC_FORM.LEFT_FIELD
  ffeed = DOC_FORM.F_FEED
  p_drctry = DOC_FORM.P_DIR

  n = MEMLINES(PAGE_H)
  DIMENSION t_p(n)
  FOR i = 1 TO n
    t_p(i) = MLINE(PAGE_H,i)
  ENDFOR

  n = MEMLINES(PAGE_F)
  DIMENSION f_p(n)
  FOR i = 1 TO n
    f_p(i) = MLINE(PAGE_F,i)
  ENDFOR

  n = MEMLINES(DOC_F)
  DIMENSION f_d(n)
  FOR i = 1 TO n
    f_d(i) = MLINE(DOC_F,i)
  ENDFOR

  USE
  SELECT SAL_BOOK
  GO TOP
  DO Wt_Mess

  PRIVATE s001, s002, s003, s101, s102, s103
  PRIVATE n_pg, f_pg, n_det, j, stw

  n_det = p_ln-ALEN(t_p)-MAX(ALEN(f_p),ALEN(f_d))-2
  n_pg  = CEILING(RECCOUNT()/n_det)

  STORE 0 TO s101, s102, s103
  DO Ini_Prn WITH "Š­¨£  ¯à®¤ ¦", p_ln, lft, n_cp, fnt, ffeed, p_drctry
  FOR f_pg = 1 TO n_pg
    IF f_pg # 1
      EJECT
    ENDIF
    STORE 0 TO s001, s002, s003
    sb(1,2) = ALLTRIM(STR(f_pg))+"/"+ALLTRIM(STR(n_pg))
    FOR i = 1 TO ALEN(t_p)
      stw = t_p(i)
      FOR j = 1 TO ALEN(sb,1)
        stw = STRTRAN(stw, sb(j,1), sb(j,2))
      ENDFOR
      @ PROW()+1, 0 SAY stw
    ENDFOR
    FOR i = 1 TO n_det
      stw = PADR(DTOC(DOC_DATE),11)+ ;
            PADR(LEFT(DOC_NUM,1)+"-"+ALLTRIM(SUBSTR(DOC_NUM,2)),11)+ ;
            PADL(CUS_NAME,38)+" "+INN+" "+KPP+" "+DTOC(PAY_DATE)+STR(SUM_H,13,2)+STR(SUM_TOV,13,2)+STR(PRC,6,2)+STR(SUM_NDS,13,2)
      s001 = s001+SUM_TOV
      s002 = s002+SUM_NDS
      s003 = s003+SUM_H
      @ PROW()+1, 0 SAY stw
      SKIP
      IF EOF()
        EXIT
      ENDIF
    ENDFOR

    sb(4,2) = STR(s001, 12, 2)
    sb(5,2) = STR(s002, 12, 2)
    sb(6,2) = STR(s003, 12, 2)

    s101 = s101+s001
    s102 = s102+s002
    s103 = s103+s003

    IF f_pg # n_pg
      FOR i = 1 TO ALEN(f_p)
        stw = f_p(i)
        FOR j = 1 TO ALEN(sb,1)
          stw = STRTRAN(stw, sb(j,1), sb(j,2))
        ENDFOR
        @ PROW()+1, 0 SAY stw
      ENDFOR
    ENDIF
    IF EOF()
      EXIT
    ENDIF
  ENDFOR

  sb(7,2) = STR(s101, 12, 2)
  sb(8,2) = STR(s102, 12, 2)
  sb(9,2) = STR(s103, 12, 2)

  FOR i = 1 TO ALEN(f_d)
    stw = f_d(i)
    FOR j = 1 TO ALEN(sb,1)
      stw = STRTRAN(stw, sb(j,1), sb(j,2))
    ENDFOR
    @ PROW()+1, 0 SAY stw
  ENDFOR

  SELECT SAL_BOOK
  USE
  DO Term_Prn WITH "", tmpo_path+"SAL_BOOK.DBF"

  DELETE FILE (tmpo_path+"SAL_BOOK_BN.DBF")
  DELETE FILE (tmpo_path+"SAL_BOOK_BN.CDX")

ENDIF
SELECT TMP_LINK

IF rc_sv > 0 .AND. rc_sv < RECCOUNT()
  GO rc_sv
ENDIF
RETURN
