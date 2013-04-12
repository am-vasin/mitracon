*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ M_K_Rep       งเ กฎโ็จช €ญคเฅฉ  แจญ           17.09.97 16:59:12 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                        โ็ฅโ ฏฎ ฎแญฎขญฎฉ ช แแฅ.                        บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE M_K_Rep
PRIVATE d0, d1, d0B, d1B, is_USD, nav

USE (base_path+"PARMS")
d0B = DAT_START
d0  = d0B
d1B = DATE()
d1  = d1B
is_USD = 1
USE

IF .NOT. Get_Int()
  RETURN
ENDIF

DO Prp_Tmp

DO Use_Dummy

nav = "Dates"
DO WHILE .T.
  DO CASE
  CASE nav = "Dates"
    nav = View_D()
  CASE nav = "Hole"
    nav = View_H()
  OTHERWISE
    EXIT
  ENDCASE
ENDDO

CLOSE DATABASES
DELETE FILE (tmpo_path+"M_K_REP.DBF")
DELETE FILE (tmpo_path+"M_K_REPD.DBF")

RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ             เฎฃเ ฌฌ  จญโฅเ ชโจขญฎฃฎ ขขฎค  จญโฅเข ซ  ค โ.              บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 17.09.97 17:02:54 อผ
PROCEDURE Get_Int
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

*
*    ฏฎซญ๏ฅฌ งญ ็ฅญจ๏ฌจ ฏฅเฅฌฅญญ๋ฅ แฎแโฎ๏ญจ๏...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ‘ฎคฅเฆ โฅซ์ญ ๏ ็ แโ์ ฏเฎฃเ ฌฌ๋:
*

*       ‘ฏเ ขช  ฏฎ ฎแญฎขญฎฉ ช แแฅ
*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ               ( ) ใกซจ   ( ) USD               ณ
*ณ ฺฤ ญโฅเข ซ ค โ ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ ณ
*ณ ณ  ็. ค โ  ..   ฎญ. ค โ  .. ณ ณ
*ณ ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู ณ
*ณ        < OK Ctrl-W > < โช ง โ์แ๏ Esc >         ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       &&   ขแ๏ชจฉ ฏฎฆ เญ๋ฉ แซใ็ ฉ!
DO Prp_Nav_2
DO D_Win_N WITH 8, 53, "‘ฏเ ขช  ฏฎ ฎแญฎขญฎฉ ช แแฅ"
DO Sun_Bord WITH 3, 3, 5, 49, " ญโฅเข ซ ค โ "

*------------------------------------------------------------------------
*      ขฎค ฏฎซฅฉ กซ ญช 
*
@ 2, 17 GET is_USD PICTURE "@*RH ใกซจ  ;USD"
@ 4,  5 SAY " ็. ค โ " GET d0 PICTURE "@D"
@ 4, 28 SAY "ฎญ. ค โ " GET d1 PICTURE "@D"
@ 6, 10 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? โช ง โ์แ๏ Esc "

READ CYCLE VALID T_Date()

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                    เฎขฅเช  ฏเ ขจซ์ญฎแโจ จญโฅเข ซ .                    บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 17.09.97 17:11:51 อผ
PROCEDURE T_Date
PRIVATE mss

IF ex = 2 .OR. READKEY() % 256 = 12
  RETURN .T.
ENDIF

IF d0 < d0b
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+" ง  ค ญญ๋ๅ แฎคฅเฆจโ จญไฎเฌ ๆจ๎ โฎซ์ชฎ แ "+ ;
                  TRANSFORM(d0b, "@D ")+"."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF d1 > DATE()
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"ฅ แฏฅ่จโฅ, แฅฃฎคญ๏ โฎซ์ชฎ "+ ;
                  TRANSFORM(DATE(), "@D ")+"."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF d1 < d0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+" ็ ซ์ญ ๏ ค โ  กฎซ์่ฅ ชฎญฅ็ญฎฉ, โ ช ญฅ ก๋ข ฅโ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                      ฎคฃฎโฎขช  ขเฅฌฅญญ๋ๅ ไ ฉซฎข.                      บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 17.09.97 17:21:34 อผ
PROCEDURE Prp_Tmp
PRIVATE s_val, i_sum, o_sum, w, dc

DO Wt_Mess WITH "ฎคฃฎโฎขช  ค ญญ๋ๅ..."
DELETE FILE (tmpo_path+"M_K_REP.DBF")
DELETE FILE (tmpo_path+"M_K_REPD.DBF")

dc = IIF(is_USD = 2, 4, 2)
CREATE DBF (tmpo_path+"M_K_REP.DBF") ;
      ( DATE   D,   ;
        INC_M  N(12, dc), ;
        EXP_M  N(12, dc), ;
        REST   N(12, dc)  )
APPEND BLANK

SELECT 0
CREATE DBF (tmpo_path+"M_K_REPD.DBF") ;
      ( DATE    D,   ;
        DOC_NUM C( 7), ;
        O_NAME  C(16), ;
        NOTE    C(32), ;
        INC_M   N(12, dc), ;
        EXP_M   N(12, dc), ;
        REST    N(12, dc)  )
APPEND BLANK

SELECT 0
USE (base_path+"KASSA_S")
s_val = IIF(is_USD = 1, SUM_R_S, SUM_D_S)

USE (base_path+"MM_TYPES") ORDER TAG CODE
SELECT 0

USE (base_path+"KASSA_M") ORDER TAG HRON

SCAN WHILE DOC_DATE < d0
  s_val = s_val + IIF(is_USD = 1, SUM_R, SUM_D)
ENDSCAN

SET RELATION TO CODE INTO MM_TYPES
GO RECNO()
SELECT M_K_REP
REPLACE REST WITH s_val
SELECT M_K_REPD
REPLACE REST WITH s_val

SELECT KASSA_M

i_sum = 0
o_sum = 0
SCAN REST WHILE DOC_DATE <= d1
  w = IIF(is_USD = 1, SUM_R, SUM_D)
  IF w = 0
    LOOP
  ENDIF
  IF KASSA_M.DOC_DATE # M_K_REPD.DATE
    SELECT M_K_REP
    IF .NOT. EMPTY(DATE)
      REPLACE INC_M WITH i_sum, ;
              EXP_M WITH o_sum, ;
              REST  WITH s_val
    ENDIF
    APPEND BLANK
    REPLACE DATE WITH KASSA_M.DOC_DATE
    i_sum = 0
    o_sum = 0
  ENDIF

  s_val = s_val + w
  i_sum = i_sum + MAX(w, 0)
  o_sum = o_sum - MIN(w, 0)
  SELECT M_K_REPD
  APPEND BLANK
  REPLACE DATE    WITH KASSA_M.DOC_DATE,  ;
          DOC_NUM WITH LEFT(KASSA_M.DOC_NUM,1)+  ;
                       ALLTRIM(SUBSTR(KASSA_M.DOC_NUM,2)),  ;
          O_NAME  WITH MM_TYPES.NAME,     ;
          NOTE    WITH KASSA_M.NOTE,      ;
          INC_M   WITH IIF(w > 0, w, 0),  ;
          EXP_M   WITH IIF(w < 0, -w, 0), ;
          REST    WITH s_val
  SELECT KASSA_M
ENDSCAN

USE

SELECT MM_TYPES
USE

SELECT M_K_REP
REPLACE INC_M WITH i_sum, ;
        EXP_M WITH o_sum, ;
        REST  WITH s_val

DO Wt_Mess

RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                              แแ  ฏฎ คญ๏ฌ.                             บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 17.09.97 18:31:59 อผ
PROCEDURE View_D
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

*
*    ฏฎซญ๏ฅฌ งญ ็ฅญจ๏ฌจ ฏฅเฅฌฅญญ๋ฅ แฎแโฎ๏ญจ๏...
*
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ฏเฅคฅซ๏ฅฌ  แจญๅเฎญญฎฅ ฌฅญ๎
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Switch OF (menu_name) PROMPT "ฎ คฎช-โ ฌ" KEY Ctrl-A, "F2"
ON SELECTION PAD Switch OF (menu_name) DO Sw_Mode WITH "Switch"

DEFINE PAD Print OF (menu_name) PROMPT "ฅ็ โ์" KEY Ctrl-A, "Enter"
ON SELECTION PAD Print OF (menu_name) DO Sw_Mode WITH "Print"

DEFINE PAD Exit OF (menu_name) PROMPT "๋ๅฎค" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

SELECT M_K_REP
*
*    แ็ฅโ เ งฌฅเฎข ฎชญ 
*
PRIVATE ln, wd, pic, ex
IF is_USD = 1
  pic = "@Z 99 999 999.9999"
ELSE
  pic = "@Z  999 999 999.99"
ENDIF
ln = RECCOUNT()   && ฎซจ็ฅแโขฎ ขจคจฌ๋ๅ แโเฎช BROWSE
wd = IIF(SET("CENTURY") = "ON", 58, 56)

DO D_Wins WITH ln, wd, ;
              "แญฎขญ ๏ ช แแ  "+IIF(is_USD = 1, "(ใกซจ)", "(USD)"), 0, 0

@ 1, 2 SAY IIF(SET("CENTURY") = "ON", "ฤฤฤ โ ฤฤฤฤ","ฤฤ โ ฤฤฤ")+ ;
"ฤฤฤฤฤ เจๅฎค ฤฤฤฤฤฤฤฤ  แๅฎค ฤฤฤฤฤฤฤฤ แโ โฎช ฤ"
what_do = "List"

GO TOP
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && เฎแฌฎโเ แฏจแช 

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Home  GO TOP
    ON KEY LABEL End   GO BOTTOM
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F2    DO Sw_Mode WITH "Switch"
*
*    BROWSE - ฌฅญ๎
*
    BROWSE FIELDS DATE:H="", ;
                  INC_M:H="":P=pic, ;
                  EXP_M:H="":P=pic, ;
                  REST:H="":P=pic,  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Print")
    ENDIF

  CASE what_do = "Print"    && ฅ็ โ์ คฎช-โ 

    DO Print WITH 1
    what_do = "List"

  CASE what_do = "Switch"   && ฅเฅชซ๎็ ฅฌแ๏

    ex = "Hole"
    EXIT

  OTHERWISE

    ex = "Exit"
    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

RETURN ex

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                           แแ  ฏฎ ฎฏฅเ ๆจ๏ฌ.                           บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 17.09.97 18:31:59 อผ
PROCEDURE View_H
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

*
*    ฏฎซญ๏ฅฌ งญ ็ฅญจ๏ฌจ ฏฅเฅฌฅญญ๋ฅ แฎแโฎ๏ญจ๏...
*
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ฏเฅคฅซ๏ฅฌ  แจญๅเฎญญฎฅ ฌฅญ๎
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Switch OF (menu_name) PROMPT "ฎ คญ๏ฌ" KEY Ctrl-A, "F2"
ON SELECTION PAD Switch OF (menu_name) DO Sw_Mode WITH "Switch"

DEFINE PAD Print OF (menu_name) PROMPT "ฅ็ โ์" KEY Ctrl-A, "Enter"
ON SELECTION PAD Print OF (menu_name) DO Sw_Mode WITH "Print"

DEFINE PAD Exit OF (menu_name) PROMPT "๋ๅฎค" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

SELECT M_K_REPD
*
*    แ็ฅโ เ งฌฅเฎข ฎชญ 
*
PRIVATE ln, wd, pic, ex
IF is_USD = 1
  pic = "@Z 99 999 999.9999"
ELSE
  pic = "@Z  999 999 999.99"
ENDIF

ln = RECCOUNT()   && ฎซจ็ฅแโขฎ ขจคจฌ๋ๅ แโเฎช BROWSE
wd = IIF(SET("CENTURY") = "ON", 75, 73)

DO D_Wins WITH ln, wd, ;
              "แญฎขญ ๏ ช แแ  "+IIF(is_USD = 1, "(ใกซจ)", "(USD)"), 0, 1

@ 1, 2 SAY IIF(SET("CENTURY") = "ON", "ฤฤฤ โ ฤฤฤฤ","ฤฤ โ ฤฤฤ")+ ;
"ฤ ’จฏ ฎฏฅเ ๆจจ ฤฤฤฤฤฤฤ เจๅฎค ฤฤฤฤฤฤฤฤ  แๅฎค ฤฤฤฤฤฤฤฤ แโ โฎช ฤ"

@ WROWS()-3, 3 SAY "N ฎฏฅเ."
@ WROWS()-3, COL()+3 SAY "เจฌฅ็ ญจฅ"

what_do = "List"

GO TOP
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && เฎแฌฎโเ แฏจแช 

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Home  GO TOP
    ON KEY LABEL End   GO BOTTOM
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F2    DO Sw_Mode WITH "Switch"
*
*    BROWSE - ฌฅญ๎
*
    BROWSE FIELDS DATE:H="", ;
                  O_NAME:H="", ;
                  INC_M:H="":P=pic, ;
                  EXP_M:H="":P=pic, ;
                  REST:H="":P=pic,  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Show_F() ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Print")
    ENDIF

  CASE what_do = "Print"    && ฅ็ โ์ คฎช-โ 

    DO Print WITH 2
    what_do = "List"

  CASE what_do = "Switch"   && ฅเฅชซ๎็ ฅฌแ๏

    ex = "Dates"
    EXIT

  OTHERWISE

    ex = "Exit"
    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

RETURN ex

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                         ๋ขฎค คฎฏ. จญไฎเฌ ๆจจ.                         บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 17.09.97 19:33:53 อผ
PROCEDURE Show_F

ACTIVATE WINDOW (win_name+"_M") SAME

@ WROWS()-2, 3 SAY DOC_NUM COLOR SCHEME 1
@ WROWS()-2, COL()+3 SAY NOTE COLOR SCHEME 1

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                       ฅ็ โ์ คฎชใฌฅญโ  ฏฎ คญ๏ฌ.                        บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 17.09.97 19:55:44 อผ
PROCEDURE Print

PARAMETER d_type

PRIVATE p_ln, fnt, n_cp, lft, n, i, j, t_p, f_p, t_d, ffeed, pic, p_drctry
PRIVATE sel_rem, f_name, sb, n_p_det, n_pg, f_pg, s_pg, str_w, nds, doc_tit
PRIVATE rec_rem, mss

IF RECCOUNT() = 0
  RETURN
ENDIF
sel_rem = SELECT()
rec_rem = RECNO()

f_name = SYS(3)
COPY TO (tmpo_path+f_name)

IF is_USD = 1
  pic = "@Z 99 999 999.9999"
ELSE
  pic = "@Z  999 999 999.99"
ENDIF

DIMENSION sb( 4,2)
sb( 1,1) = "{Pg/Pgs}"
sb( 1,2) = ""

sb( 2,1) = "{Date_S}"
sb( 2,2) = TRANSFORM(d0, "@D")

sb( 3,1) = "{Date_T}"
sb( 3,2) = TRANSFORM(d1, "@D")

sb( 4,1) = "{Valuta}"
sb( 4,2) = IIF(is_USD = 1, "ใกซจ", "USD")

n = ALEN(sb,1)

SELECT 0
USE DOC_FORM

LOCATE FOR IIF(d_type = 1, "KASSA_1", "KASSA_2") = ALLTRIM(UPPER(DOC_NAME))
p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed =DOC_FORM.F_FEED
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

n = MEMLINES(DOC_H)
DIMENSION t_d(n)
FOR i = 1 TO n
  t_d(i) = MLINE(DOC_H,i)
ENDFOR

USE

SELECT (sel_rem)

DO Ini_Prn WITH " แแ ", p_ln, lft, n_cp, fnt, ffeed, p_drctry

GO TOP

n_p_det = p_ln-MAX(ALEN(t_p),ALEN(t_d))-ALEN(f_p)
IF n_p_det <= 0
  n_p_det = RECCOUNT()
  n_pg = 1
ELSE
  n_pg = CEILING(RECCOUNT()/n_p_det)
ENDIF

FOR f_pg = 1 TO n_pg
  sb( 1,2) = ALLTRIM(STR(f_pg))+"/"+ALLTRIM(STR(n_pg))
  sb( 1,2) = PADR(sb(1,2), LEN(sb(1,1)))
  IF f_pg # 1
    EJECT
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
  ELSE
    FOR i = 1 TO ALEN(t_d)
      str_w = t_d(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      IF i = 1
        @ PROW(), PCOL() SAY str_w
      ELSE
        @ PROW()+1, 0 SAY str_w
      ENDIF
    ENDFOR
  ENDIF

  FOR i = 1 TO n_p_det
    IF d_type = 1
      str_w = TRANSFORM(DATE, "@D")+" "+  ;
              TRANSFORM(INC_M, pic)+" "+  ;
              TRANSFORM(EXP_M, pic)+" "+  ;
              TRANSFORM(REST, pic)
    ELSE
      str_w = TRANSFORM(DATE, "@D")+" "+  ;
              DOC_NUM              +" "+  ;
              O_NAME               +" "+  ;
              TRANSFORM(INC_M, pic)+" "+  ;
              TRANSFORM(EXP_M, pic)+" "+  ;
              TRANSFORM(REST, pic) +" "+  ;
              NOTE
    ENDIF
    @ PROW()+1,0 SAY str_w
    SKIP
    IF EOF()
      EXIT
    ENDIF
  ENDFOR

  FOR i = 1 TO ALEN(f_p)
    str_w = f_p(i)
    FOR j = 1 TO ALEN(sb,1)
      str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
    ENDFOR
    @ PROW()+1, 0 SAY str_w
  ENDFOR
ENDFOR

DO Term_Prn WITH "", tmpo_path+f_name

DELETE FILE (tmpo_path+f_name+".DBF")
GO rec_rem

RETURN
