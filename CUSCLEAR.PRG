*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                        ็จแโช  แฏจแช  ชซจฅญโฎข.                       บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE CusClear
PRIVATE c_type, mss, pl, sw

pl = Choose_P()

DO Use_Dummy

SELECT 0
USE (base_path+"CUSTOMER") ORDER TAG CODE

SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"INCMONEY") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"CUS_REST") ORDER TAG CUS_CODE

SELECT 0
IF EMPTY(pl)
  USE (base_path+"ACCOUNT") ORDER TAG CUS_NAME
ELSE
  USE (base_path+"ACCOUNT") ORDER TAG PL_NAME
ENDIF
SEEK pl

SELECT 0
DELETE FILE (tmpo_path+"CUS_TMP.DBF")
DELETE FILE (tmpo_path+"CUS_TMP.CDX")
CREATE DBF (tmpo_path+"CUS_TMP")  ;
    (   CUS_CODE  N( 8),  ;
        CUS_NAME  C(50) )

INDEX ON CUS_NAME TAG NAME
SET ORDER TO TAG NAME

SELECT 0
DELETE FILE (tmpo_path+"DOCS_TMP.DBF")
DELETE FILE (tmpo_path+"DOCS_TMP.CDX")
CREATE DBF (tmpo_path+"DOCS_TMP")  ;
    (   CUS_CODE  N( 8),  ;
        FIRM      C( 3),  ;
        DOC_NUM   C(10),  ;
        DOC_DATE  D    ,  ;
        DOC_TIME  N( 5),  ;
        DOC_TYPE  C( 1),  ;
        DOC_SUM   N(15,2), ;
        DOC_SUMD  N(15,4) )

INDEX ON STR(CUS_CODE,8)+DTOS(DOC_DATE)+STR(DOC_TIME,5) TAG CUS_CODE
SET ORDER TO TAG CUS_CODE

SELECT ACCOUNT

DO Wt_Mess WITH "ฎคฃฎโฎขช  แฏจแช  ชซจฅญโฎข..."

*
*   ฏฎซญ๏ฅฌ ขเฅฌฅญญ๋ฅ ไ ฉซ๋
*
SCAN REST WHILE SUBSYS = pl
  IF SEEK(STR(CUS_CODE), "CUS_REST")
    SELECT CUS_REST
    sw = .F.
    SCAN REST WHILE CUS_CODE = ACCOUNT.CUS_CODE
      IF ACC_SUM # 0 .OR. ACC_S_H # 0 .OR. ACC_SUMD # 0 .OR. ACC_SD_H # 0
        sw = .T.
        EXIT
      ENDIF
    ENDSCAN
    IF sw
      SELECT ACCOUNT
      LOOP
    ENDIF
  ENDIF
  SELECT CUS_TMP
  APPEND BLANK
  REPLACE CUS_CODE WITH ACCOUNT.CUS_CODE,  ;
          CUS_NAME WITH ACCOUNT.CUS_NAME

  IF SEEK(CUS_CODE,"INCMONEY")
    SELECT INCMONEY
    SCAN REST WHILE CUS_CODE = CUS_TMP.CUS_CODE
      SELECT DOCS_TMP
      APPEND BLANK
      REPLACE CUS_CODE WITH INCMONEY.CUS_CODE,  ;
              FIRM     WITH INCMONEY.FIRM,      ;
              DOC_NUM  WITH INCMONEY.DOC_NUM,   ;
              DOC_DATE WITH INCMONEY.DOC_DATE,  ;
              DOC_TIME WITH INCMONEY.DOC_TIME,  ;
              DOC_TYPE WITH INCMONEY.DOC_TYPE,  ;
              DOC_SUM  WITH INCMONEY.MONEY_R,   ;
              DOC_SUMD WITH INCMONEY.MONEY_D
      SELECT INCMONEY
    ENDSCAN
  ENDIF
  IF SEEK(CUS_TMP.CUS_CODE,"SALE_TIT")
    SELECT SALE_TIT
    SCAN REST WHILE CUS_CODE = CUS_TMP.CUS_CODE
      SELECT DOCS_TMP
      APPEND BLANK
      REPLACE CUS_CODE WITH SALE_TIT.CUS_CODE,  ;
              FIRM     WITH SALE_TIT.FIRM,      ;
              DOC_NUM  WITH SALE_TIT.DOC_NUM,   ;
              DOC_DATE WITH SALE_TIT.DOC_DATE,  ;
              DOC_TIME WITH SALE_TIT.DOC_TIME,  ;
              DOC_TYPE WITH "*",                ;
              DOC_SUM  WITH -SALE_TIT.TOTAL_R,  ;
              DOC_SUMD WITH -SALE_TIT.TOTAL_D
      SELECT SALE_TIT
    ENDSCAN
  ENDIF
  SELECT ACCOUNT

ENDSCAN

DO Wt_Mess

IF RECCOUNT("CUS_TMP") = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"ฅโ ญจ ฎคญฎฃฎ ชซจฅญโ  แ ฏใแโ๋ฌ คฅฏฎงจโฎฌ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  DELETE FILE (tmpo_path+"CUS_TMP.DBF")
  DELETE FILE (tmpo_path+"CUS_TMP.CDX")
  DELETE FILE (tmpo_path+"DOCS_TMP.DBF")
  DELETE FILE (tmpo_path+"DOCS_TMP.CDX")
  RETURN
ENDIF
  
*
*  ฅเฅฌฅญญ๋ฅ แฎแโฎ๏ญจ๏ คซ๏ ญ ขจฃขๆจจ
*

PRIVATE stat_type   && ’จฏ ญ ขจฃ ๆจจ: 0 - ญฅแโ ญค เโญ๋ฉ;
                                        1 - กซ ญช;
                                        2 - BROWSE - โ กซจๆ ;
                                        3 - BROWSE - แฏจแฎช.
PRIVATE what_do       && ฌ๏ เฅฆจฌ .
PRIVATE menu_name     && ฌ๏  แจญๅเฎญญฎฃฎ ฌฅญ๎.
PRIVATE last_mouse    && เฅฌ๏ ฏฎแซฅคญฅฃฎ ญ ฆ โจ๏ งขฅเจญฎฉ ชญฎฏชจ.
PRIVATE win_name      && ฌ๏ ฎชญ  ( ฎชฎญ คซ๏ BROWSE ).

*
*    ฏฎซญ๏ฅฌ งญ ็ฅญจ๏ฌจ ฏฅเฅฌฅญญ๋ฅ แฎแโฎ๏ญจ๏...
*
stat_type  = 3
what_do    = "list"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ฏเฅคฅซ๏ฅฌ  แจญๅเฎญญฎฅ ฌฅญ๎
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Docs OF (menu_name) PROMPT "ฎชใฌฅญโ๋" KEY Ctrl-A, "Enter"
ON SELECTION PAD Docs OF (menu_name) Do Sw_Mode WITH "docs"

DEFINE PAD Del OF (menu_name) PROMPT "“ค ซจโ์" KEY Ctrl-A, "Del"
ON SELECTION PAD Del OF (menu_name) Do Sw_Mode WITH "del"

DEFINE PAD Exit OF (menu_name) PROMPT "๋ๅฎค" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*    แ็ฅโ เ งฌฅเฎข ฎชญ 
*
SELECT ACCOUNT
SET ORDER TO TAG CUS_CODE
SELECT CUS_TMP
GO TOP
PRIVATE ln, wd
ln = MIN(100, RECCOUNT())   && ฎซจ็ฅแโขฎ ขจคจฌ๋ๅ แโเฎช BROWSE
wd = FSIZE("CUS_NAME")

DO D_Wins WITH ln, wd, "ซจฅญโ๋ แ ฏใแโ๋ฌ คฅฏฎงจโฎฌ", 0, 0
what_do = "list"

DO WHILE .T.

  DO CASE

  CASE what_do = "list"    && เฎแฌฎโเ แฏจแช 
    
    SELECT CUS_TMP
    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Del   DO Sw_Mode WITH "del"

*
*    BROWSE - ฌฅญ๎
*
    BROWSE FIELDS CUS_NAME,  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "list"
      what_do = IIF(READKEY() % 256 = 12, "exit", "docs")
    ENDIF

  CASE what_do = "docs"    && เฎแฌฎโเ คฎชใฌฅญโฎข

    DO View_Doc
    what_do = "list"

  CASE what_do = "del"     && “ค ซฅญจฅ ชซจฅญโ 

    SELECT ACCOUNT
    SEEK CUS_TMP.CUS_CODE
    REPLACE DATE_OFF WITH DATE()
    SELECT CUSTOMER
    IF SEEK(ACCOUNT.CUS_CODE)
      REPLACE DATE_OFF WITH DATE(), WHO_DEL WITH USER
    ENDIF
    SELECT CUS_TMP
    DELETE
    SKIP
    what_do = "list"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

CLEAR WINDOWS
CLOSE DATABASES
RELEASE MENU (menu_name) EXTENDED
DELETE FILE (tmpo_path+"CUS_TMP.DBF")
DELETE FILE (tmpo_path+"CUS_TMP.CDX")
DELETE FILE (tmpo_path+"DOCS_TMP.DBF")
DELETE FILE (tmpo_path+"DOCS_TMP.CDX")

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ              เฎแฌฎโเ คฎชใฌฅญโฎข คซ๏ ชซจฅญโ .                         บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE View_Doc
PRIVATE tit

SELECT DOCS_TMP
IF .NOT. SEEK(STR(CUS_TMP.CUS_CODE,8))
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"ซ๏ ชซจฅญโ  ญฅโ ญจ ฎคญฎฃฎ คฎชใฌฅญโ !"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SELECT CUS_TMP
  RETURN
ENDIF
tit = ALLTRIM(CUS_TMP.CUS_NAME)

*
*  ฅเฅฌฅญญ๋ฅ แฎแโฎ๏ญจ๏ คซ๏ ญ ขจฃขๆจจ
*

PRIVATE stat_type   && ’จฏ ญ ขจฃ ๆจจ: 0 - ญฅแโ ญค เโญ๋ฉ;
                                        1 - กซ ญช;
                                        2 - BROWSE - โ กซจๆ ;
                                        3 - BROWSE - แฏจแฎช.
PRIVATE what_do       && ฌ๏ เฅฆจฌ .
PRIVATE menu_name     && ฌ๏  แจญๅเฎญญฎฃฎ ฌฅญ๎.
PRIVATE last_mouse    && เฅฌ๏ ฏฎแซฅคญฅฃฎ ญ ฆ โจ๏ งขฅเจญฎฉ ชญฎฏชจ.
PRIVATE win_name      && ฌ๏ ฎชญ  ( ฎชฎญ คซ๏ BROWSE ).

*
*    ฏฎซญ๏ฅฌ งญ ็ฅญจ๏ฌจ ฏฅเฅฌฅญญ๋ฅ แฎแโฎ๏ญจ๏...
*
stat_type  = 3
what_do    = "list"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ฏเฅคฅซ๏ฅฌ  แจญๅเฎญญฎฅ ฌฅญ๎
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Prnt OF (menu_name) PROMPT "ฅ็ โ์ คฎช-โ " KEY Ctrl-A, "Enter"
ON SELECTION PAD Prnt OF (menu_name) Do Sw_Mode WITH "Prnt"

DEFINE PAD Exit OF (menu_name) PROMPT "ฎงขเ โ" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*    แ็ฅโ เ งฌฅเฎข ฎชญ 
*
PRIVATE ln, wd
*ln = MIN(100, RECCOUNT())   && ฎซจ็ฅแโขฎ ขจคจฌ๋ๅ แโเฎช BROWSE
ln = WROWS("")-7
wd = 60

DO D_Wins WITH ln, wd, tit, 0, 0

*ฺ”เฌฤฤฤ โ ฤฤฤฤฤN คฎช-โ ฤฤฤฤฤฤ’จฏ คฎช-โ ฤฤฤฤฤฤฤฤฤ‘ใฌฌ ฤฤฤฤฤฤฤฟ
*ณ””” .. NNNNNNNNNNN AAAAAAAAAAAAAAA 9 999 999 999.99$ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

@ 1, 2 SAY "”เฌฤฤฤ โ ฤฤฤฤฤN คฎช-โ ฤฤฤฤฤฤ’จฏ คฎช-โ ฤฤฤฤฤฤฤฤฤ‘ใฌฌ ฤฤฤฤฤฤฤ"
what_do = "list"

DO WHILE .T.

  DO CASE

  CASE what_do = "list"    && เฎแฌฎโเ แฏจแช 

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - ฌฅญ๎
*
    BROWSE FIELDS FIRM:H="",         ;
                  DOC_DATE:H="",     ;
                  F000 = IIF(DOC_TYPE = "*",  ;
                     LEFT(DOC_NUM,4)+"-"+ALLTRIM(RIGHT(DOC_NUM,5)),  ;
                     LEFT(DOC_NUM,1)+"-"+ALLTRIM(RIGHT(DOC_NUM,2))):H="":11, ;
                  F001 = D_Type():H="":15,   ;
                  F002 = D_Sum():H="":17,    ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           KEY STR(CUS_TMP.CUS_CODE,8) ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "list"
      what_do = IIF(READKEY() % 256 = 12, "exit", "prnt")
    ENDIF

  CASE what_do = "prnt"    && เฎแฌฎโเ แฏจแช 

    DO P_D
    what_do = "list"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_M")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_I")
RELEASE MENU (menu_name) EXTENDED

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                    แ่จไเฎขช  โจฏ  คฎชใฌฅญโ .                         บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE D_Type

DO CASE
CASE DOC_TYPE = " "
  RETURN "เจๅ/เ แๅ ฎเคฅเ"
CASE DOC_TYPE = "1"
  RETURN "ซ โฅฆช      "
CASE DOC_TYPE = "3"
  RETURN "ญคฅชแ.      "
CASE DOC_TYPE = "*"
  RETURN " ชซ คญ ๏    "
ENDCASE
RETURN "             "

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                       ‘ใฌฌ  ฏฎ คฎชใฌฅญโใ.                             บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE D_Sum

DO CASE
CASE DOC_TYPE = "3"
  RETURN TRANSFORM(DOC_SUMD, "@Z 999 999 999.9999")+"$"
OTHERWISE
  RETURN TRANSFORM(DOC_SUM,  "@Z 9 999 999 999.99")+"เ"
ENDCASE

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                     เฎฃเ ฌฌ  ฏฅ็ โจ คฎชใฌฅญโฎข.                      บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE P_D

DEFINE WINDOW H_W_876 FROM 0,0 TO 0, WCOLS("")-1 NONE COLOR SCHEME 13
SHOW WINDOW H_W_876

DO CASE
CASE INLIST(DOC_TYPE, " ", "1", "2", "3")  && ซ โฅฆ
  DO P_Money WITH FIRM, DOC_TYPE, DOC_NUM, DOC_DATE
CASE DOC_TYPE = "*"                  &&  ชซ คญ ๏
  DO Prn_Inv WITH FIRM, DOC_NUM, DOC_DATE
ENDCASE
RELEASE WINDOW H_W_876

RETURN
