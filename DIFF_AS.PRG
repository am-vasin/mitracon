*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ              เฎฃเ ฌฌ  ฏฅ็ โจ เ แๅฎฆคฅญจฉ ฏเจฅฌชจ.                    บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Diff_AS

*
*  ฅเฅฌฅญญ๋ฅ แฎแโฎ๏ญจ๏ คซ๏ ญ ขจฃ ๆจจ
*

PRIVATE stat_type     && ’จฏ ญ ขจฃ ๆจจ: 0 - ญฅแโ ญค เโญ๋ฉ;
                                        1 - กซ ญช;
                                        2 - BROWSE - โ กซจๆ ;
                                        3 - BROWSE - แฏจแฎช.
PRIVATE what_do       && ฌ๏ เฅฆจฌ .
PRIVATE menu_name     && ฌ๏  แจญๅเฎญญฎฃฎ ฌฅญ๎.
PRIVATE last_mouse    && เฅฌ๏ ฏฎแซฅคญฅฃฎ ญ ฆ โจ๏ งขฅเจญฎฉ ชญฎฏชจ.
PRIVATE win_name      && ฌ๏ ฎชญ  ( ฎชฎญ คซ๏ BROWSE ).
PRIVATE s_sav         && ฎฌฅเ เ กฎ็ฅฉ ฎกซ แโจ คซ๏ แฎๅเ ญฅญจ๏ จ ขฎแแโ ญฎขซฅญจ๏!

*
*    ฏฎซญ๏ฅฌ งญ ็ฅญจ๏ฌจ ฏฅเฅฌฅญญ๋ฅ แฎแโฎ๏ญจ๏...
*
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   โชเ๋ข ฅฌ ไ ฉซ๋ .., ใแโ ญ ขซจข ฅฌ แข๏งจ จ โ. ค.
*
s_sav = SELECT()

IF .NOT. File_O(path_comm+"INVOICET.DBF") .OR. .NOT. File_O(path_comm+"INVOICED.DBF") .OR. ;
   .NOT. File_O(path_comm+"INVOICET.CDX") .OR. .NOT. File_O(path_comm+"INVOICED.CDX")
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"ฅ ใค ฅโแ๏ ญ ฉโจ ไ ฉซ๋ แ ญ ชซ คญ๋ฌจ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
SELECT 0
USE (path_comm+"INVOICET")
SELECT 0
DO Use_Dummy
SELECT INVOICET
GO BOTTOM

*
*   ฏเฅคฅซ๏ฅฌ  แจญๅเฎญญฎฅ ฌฅญ๎
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Mark OF (menu_name) PROMPT "โฌฅโจโ์" KEY Ctrl-A, "F3"
ON SELECTION PAD Mark OF (menu_name) DO Sw_Mode WITH "Mark"

DEFINE PAD Work OF (menu_name) PROMPT " กฎโ " KEY Ctrl-A, "Enter"
ON SELECTION PAD Work OF (menu_name) DO Sw_Mode WITH "Work"

DEFINE PAD Exit OF (menu_name) PROMPT "โช ง โ์แ๏" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*    แ็ฅโ เ งฌฅเฎข ฎชญ 
*
PRIVATE ln, wd, m_list, prnt_sw, dt0
prnt_sw = .F.
m_list = ""
dt0 = DATE()
ln = WROWS("")-11   && ฎซจ็ฅแโขฎ ขจคจฌ๋ๅ แโเฎช BROWSE
wd = 22

DO D_Wins WITH ln, wd, "ฅเฅ็ฅญ์ ญ ชซ คญ๋ๅ", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && เฎแฌฎโเ แฏจแช 

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F3    DO Mark_

*
*    BROWSE - ฌฅญ๎
*
    BROWSE FIELDS M000 = IIF(STR(RECNO())+"!" $ m_list, "๛", " "):1:H="", ;
                  EXP_DATE:H="", ;
                  EXP_NUM:H="",  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Work")
    ENDIF

  CASE what_do = "Work"    && เฎแฌฎโเ แฏจแช 

    IF EMPTY(m_list)
      m_list = STR(RECNO())+"!"
    ENDIF
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"  [ ’ฎซ์ชฎ เ แๅฎฆคฅญจ๏ ] [ แฅ ฏฎงจๆจจ ]"
    mss(3) = ""
    prnt_sw = Out_Mess(5,"mss") = 1
    DO Fill_Dbf
    DO P_Diff
    EXIT

  CASE what_do = "Mark"    && โฌฅโช : ใแโ ญฎขช /แญ๏โจฅ

    DO Mark_
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   €ชชใเ โญฅญ์ชฎ ( ชชใเ โญฅญ์ชฎ!) ง ชเ๋ข ฅฌ DBF-ไ ฉซ๋.
*
CLOSE DATABASES
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                      “แโ ญฎขช /แญ๏โจฅ ฎโฌฅโชจ.                        บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Mark_
PRIVATE p

p = AT(STR(RECNO())+"!",m_list)
IF p = 0
  m_list = m_list+STR(RECNO())+"!"
ELSE
  m_list = STUFF(m_list, p, 11, "")
ENDIF

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ             เฎฃเ ฌฌ  ฏเฎขฅเชจ ชฎเเฅชโญฎแโจ ฏฎซฅฉ กซ ญช .             บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Tst_Blank
PRIVATE s, mss, f_w

IF READKEY() % 256 = 12 .OR. ex = 2
  RETURN .T.
ENDIF

f_w = ALLTRIM(f_name)
IF .NOT. "." $ f_w
  f_w = f_w+".DBF"
ENDIF

IF .NOT. File_O(ALLTRIM(f_w))
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"ฅโ ไ ฉซ  แ ญ ชซ คญ๋ฌจ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

f_name = f_w

RETURN .T.

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ           เฎฃเ ฌฌ  ง ฏฎซญฅญจ๏ ขเฅฌฅญญฎฉ ก ง๋ เ แๅฎฆคฅญจฉ.            บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Fill_Dbf
PRIVATE pr_nams, pr_cods, i, n, n_1, n_w

DELETE FILE (tmpo_path+"Diff_Inc.dbf")
DELETE FILE (tmpo_path+"Diff_Inc.cdx")

SELECT 0
USE (base_path+"DEVELOP")
i = 0
SCAN
  i = i+1
  DIMENSION pr_nams(i), pr_cods(i)
  pr_nams(i) = B_NAME
  pr_cods(i) = CODE
ENDSCAN

SET EXCLUSIVE ON
CREATE DBF (tmpo_path+"Diff_Inc")  ;
  ( BOX_NO   C(10), ;
    ACT_NO   C(10), ;
    ACT_DT   D,     ;
    CODE     N( 7), ;
    PREFIX   C( 4), ;
    NAME     C(35), ;
    PRODUCER C( 8), ;
    PRO_CODE N( 5), ;
    MRK      C( 1), ;
    QNT_INV  N( 6), ;
    QNT_ACT  N( 6)  )

USE (tmpo_path+"Diff_Inc")
INDEX ON BOX_NO+STR(CODE,7) TAG CODE
INDEX ON BOX_NO+NAME+PREFIX+STR(PRO_CODE,5) TAG NAME ADDITIVE

SET EXCLUSIVE OFF

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE

SELECT 0
USE (path_comm+"INVOICED") ORDER TAG EXP_NUM ALIAS SRS
SET RELATION TO STO_CODE INTO STOCK

SELECT INVOICET

DO WHILE .NOT. EMPTY(m_list)
  n_w = VAL(ALLTRIM(LEFT(m_list,10)))
  GO n_w
  m_list = SUBSTR(m_list,12)
  IF dt0 > EXP_DATE
    dt0 = EXP_DATE
  ENDIF
  SELECT SRS
  SEEK LEFT(DTOS(INVOICET.EXP_DATE),4)+INVOICET.EXP_NUM
  SCAN REST WHILE INVOICET.EXP_DATE = EXP_DATE .AND. INVOICET.EXP_NUM = EXP_NUM
    n = 1
    FOR i = 1 TO ALEN(pr_nams)
      IF ALLTRIM(PRODUCER) == ALLTRIM(pr_nams(i))
        n = pr_cods(i)
        EXIT
      ENDIF
    ENDFOR
    SELECT DIFF_INC
    APPEND BLANK
    REPLACE BOX_NO   WITH SRS.EXP_NUM,  ;
            CODE     WITH SRS.STO_CODE, ;
            PREFIX   WITH IIF(FOUND("STOCK"), STOCK.PREFIX, SRS.PREFIX), ;
            NAME     WITH IIF(FOUND("STOCK"), STOCK.NAME, SRS.NAME), ;
            PRODUCER WITH IIF(FOUND("STOCK"), STOCK.PRODUCER, SRS.PRODUCER), ;
            PRO_CODE WITH IIF(FOUND("STOCK"), STOCK.PRO_CODE, n), ;
            QNT_INV  WITH SRS.QTY
    SELECT SRS
  ENDSCAN
  SELECT INVOICET

ENDDO
USE
SELECT SRS
USE (base_path+"ACT_S") ORDER TAG DOC_NUM
SET RELATION TO CODE INTO STOCK

SELECT 0
USE (base_path+"ACT_S_T")

n = "**********"
SELECT DIFF_INC
SET ORDER TO TAG CODE
PRIVATE a_r_sav, key_w
SCAN
  IF .NOT. ALLTRIM(n) == ALLTRIM(BOX_NO)   &&  ฅๅ ซจ ญ  ญฎขใ๎ ชฎเฎกชใ!
    n_1 = ALLTRIM(BOX_NO)
    SELECT ACT_S_T
    LOCATE FOR ALLTRIM(EXP_NUM) == n_1 .AND.  DOC_DATE >= dt0
    n = DIFF_INC.BOX_NO
    DO WHILE FOUND()
      DO Add_Act
      SELECT ACT_S_T
      a_r_sav = RECNO()
      key_w = FIRM+DTOS(DOC_DATE)+DOC_NUM
      SCAN REST FOR "!" $ DOC_NUM .AND. key_w = FIRM+DTOS(EXP_DATE)+EXP_NUM  && ฎเเฅชโจเใ๎้จฅ  ชโ๋
        DO Add_Act
        SELECT ACT_S_T
      ENDSCAN
      SELECT ACT_S_T
      GO a_r_sav
      SKIP
      LOCATE REST FOR ALLTRIM(EXP_NUM) == n_1 .AND.   ;
               DOC_DATE >= dt0
    ENDDO
    SELECT DIFF_INC
  ENDIF
ENDSCAN

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ               เฎฃเ ฌฌ  คฎก ขซฅญจ๏ ฏเจฅฌญฎฃฎ  ชโ .                    บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Add_Act
PRIVATE rc, i

rc = RECNO("DIFF_INC")
SELECT ACT_S
SEEK ACT_S_T.FIRM+LEFT(DTOS(ACT_S_T.DOC_DATE),4)+ACT_S_T.DOC_NUM
SCAN REST WHILE FIRM = ACT_S_T.FIRM .AND. DOC_NUM = ACT_S_T.DOC_NUM .AND. DOC_DATE = ACT_S_T.DOC_DATE
  *
  *   ้ฅฌ ฏฎ ชฎคใ...
  *
  SELECT DIFF_INC
  SET ORDER TO TAG CODE
  i = " "
  IF .NOT. SEEK(n+STR(ACT_S.CODE,7))
    SET ORDER TO TAG NAME
    IF SEEK(n+STOCK.NAME+STOCK.PREFIX+STR(STOCK.PRO_CODE,5))
      i = "!"
    ENDIF
  ENDIF
  IF .NOT. FOUND()
    APPEND BLANK
    REPLACE BOX_NO WITH n
  ENDIF
  REPLACE ACT_NO   WITH ACT_S_T.DOC_NUM,  ;
          ACT_DT   WITH ACT_S_T.DOC_DATE, ;
          CODE     WITH ACT_S.CODE,       ;
          PREFIX   WITH STOCK.PREFIX,     ;
          NAME     WITH STOCK.NAME,       ;
          PRODUCER WITH STOCK.PRODUCER,   ;
          PRO_CODE WITH STOCK.PRO_CODE,   ;
          MRK      WITH IIF(ACT_S.DOC_DATE = STOCK.DATE_ON, "", i), ;
          QNT_ACT  WITH QNT_ACT+ACT_S.QNT_INC
  SELECT ACT_S
ENDSCAN

SELECT DIFF_INC
GO rc

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                    เฎฃเ ฌฌ  ฏฅ็ โจ คฎชใฌฅญโ .                        บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE P_Diff

PRIVATE p_ln, fnt, n_cp, lft, sb, n, t_p, f_p, f_d, i, pic, v_sw, p_drctry
PRIVATE n_p_det, n_pg, f_pg, sp1, sp2, sp3, sp4, sd1, sd2, sd3, sd4
PRIVATE mss, ffeed

DIMENSION sb(1,2)
sb( 1,1) = "{Page}"
sb( 1,2) = ""

n = ALEN(sb,1)
FOR i = 1 TO n
  sb(i,2) =PADR(sb(i,2), LEN(sb(i,1)))
ENDFOR

SELECT 0
USE (base_path+"DOC_FORM")
LOCATE FOR "DIFF_INC" = ALLTRIM(UPPER(DOC_NAME))
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

USE

SELECT DIFF_INC
SET ORDER TO TAG NAME

DO Ini_Prn WITH "ฅคฎฌฎแโ์ เ แๅฎฆคฅญจฉ", p_ln, lft, n_cp, fnt, ffeed, p_drctry

GO TOP
PRIVATE pg_wk, doc_wk, f_lin
f_pg  = 1
doc_wk = "**********"
pg_wk  = p_ln-ALEN(t_p)-4
f_lin  = pg_wk+10
DO WHILE .NOT. EOF()
  IF f_lin >= pg_wk
    IF f_pg # 1
      EJECT
    ENDIF
    sb( 1,2) = ALLTRIM(STR(f_pg))
    sb( 1,2) = PADL(sb(1,2), LEN(sb(1,1)))
    FOR i = 1 TO ALEN(t_p)
      str_w = t_p(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      IF i = 1
        @ PROW(), PCOL() SAY str_w
      ELSE
        @ PROW()+1, 0 SAY str_w
      ENDIF
    ENDFOR
    f_lin = 0
    f_pg = f_pg+1
  ENDIF
  IF doc_wk # BOX_NO
    doc_wk = BOX_NO
    str_w = "********** ฎเฎกช  N "+ALLTRIM(BOX_NO)+" *****************************"
    @ PROW()+1, 0 SAY str_w
    f_lin = f_lin+1
    LOOP
  ENDIF
  IF prnt_sw .AND. QNT_INV = QNT_ACT
    SKIP
    LOOP
  ENDIF
  str_w = PREFIX+" "+NAME+" "+PRODUCER+" "+MRK+" "+ ;
          TRANSFORM(QNT_INV, "@Z 999999")+" "+TRANSFORM(QNT_ACT, "@Z 999999")+  ;
          IIF(QNT_INV = QNT_ACT, "  ", " ๛")+IIF(EMPTY(ACT_NO), "    ฅโ",ACT_NO)
  @ PROW()+1, 0 SAY str_w
  f_lin = f_lin+1
  SKIP

ENDDO
USE

DO Term_Prn WITH "", tmpo_path+"Diff_Inc.dbf"

RETURN
