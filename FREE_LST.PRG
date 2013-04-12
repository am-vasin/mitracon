*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ Free_Lst      งเ กฎโ็จช €ญคเฅฉ  แจญ           21.06.97 11:58:44 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ          เฎฃเ ฌฌ  แฎงค ญจ๏ จ ชฎเเฅชๆจจ ใญจขฅเแ ซ์ญ๋ๅ แฏจแชฎข.         บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Free_Lst

PRIVATE mss

USE
DO Use_Dummy

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE

SELECT 0
USE (base_path+"ARM_LST") ORDER TAG ARM_C

SELECT 0
USE (base_path+"WK_LIST") ORDER TAG LIST_NO
DELETE FILE (tmpo_path+"TMP_WK.DBF")
COPY STRUCTURE TO (tmpo_path+"TMP_WK")
SELECT 0
USE (tmpo_path+"TMP_WK") EXCLUSIVE
INDEX ON NAME+PREFIX+PRODUCER TAG NAME
INDEX ON CODE TAG CODE ADDITIVE

SELECT 0
USE (base_path+"WK_TIT")

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
what_do    = "list"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ฏเฅคฅซ๏ฅฌ  แจญๅเฎญญฎฅ ฌฅญ๎
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Corr OF (menu_name) PROMPT "ฎเเฅชๆจ๏" KEY Ctrl-A, "Enter"
ON SELECTION PAD Corr OF (menu_name) Do Sw_Mode WITH "Corr"

DEFINE PAD Title OF (menu_name) PROMPT " ฃฎซฎขชจ"
ON PAD Title OF (menu_name) ACTIVATE POPUP Title

  DEFINE POPUP Title MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Title PROMPT "ฎเเฅชโจเฎข โ์" KEY Ctrl-A, "Tab"
  ON SELECTION BAR 1 OF Title DO Sw_Mode WITH "Title"

  DEFINE BAR  2 OF Title PROMPT "ฎก ขจโ์" KEY Ctrl-A, "Ins"
  ON SELECTION BAR 2 OF Title DO Sw_Mode WITH "Add"

  DEFINE BAR  3 OF Title PROMPT "“ค ซจโ์" KEY Ctrl-A, "Del"
  ON SELECTION BAR 3 OF Title DO Sw_Mode WITH "Del"

DEFINE PAD Print OF (menu_name) PROMPT "ฅ็ โ์" KEY Ctrl-A, "Ctrl-P"
ON SELECTION PAD Print OF (menu_name) Do Sw_Mode WITH "Print"

DEFINE PAD Flags OF (menu_name) PROMPT "”ซ ฆชจ"
ON PAD Flags OF (menu_name) ACTIVATE POPUP Flags

  DEFINE POPUP Flags MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Flags PROMPT "ฎซจ็ฅแโขฎ" KEY Ctrl-A, "F1"
  ON SELECTION BAR 1 OF Flags DO Sw_Mode WITH "Qnt"

  DEFINE BAR  2 OF Flags PROMPT "–ฅญ " KEY Ctrl-A, "F2"
  ON SELECTION BAR 2 OF Flags DO Sw_Mode WITH "Price"

DEFINE PAD Exit OF (menu_name) PROMPT "๋ๅ." KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "Exit"

*
*    แ็ฅโ เ งฌฅเฎข ฎชญ 
*
PRIVATE ln, wd
ln = WROWS("")-10   && ฎซจ็ฅแโขฎ ขจคจฌ๋ๅ แโเฎช BROWSE
wd = 54

DO D_Wins WITH ln, wd, " ฃฎโฎขชจ แฏจแชฎข", 0, 2
@ WROWS()-4, 10 SAY " ฅฌ "
@ WROWS()-4, 41 SAY " คฅ "
@ WROWS()-3,  3 SAY "‘ฎงค."
@ WROWS()-2,  3 SAY "ฎเเ."

what_do = "List"
@ 1,  3 SAY " ‘ฏจแฎช "
@ 1, 44 SAY " -ขฎ "
@ 1, 51 SAY " –ฅญ  "

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && เฎแฌฎโเ แฏจแช 

    SELECT WK_TIT
    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins   Do Sw_Mode WITH "Add"
    ON KEY LABEL Tab   Do Sw_Mode WITH "Title"
    ON KEY LABEL Del   Do Sw_Mode WITH "Del"
    ON KEY LABEL F1    Do Sw_Mode WITH "Qnt"
    ON KEY LABEL F2    Do Sw_Mode WITH "Price"
    ON KEY LABEL Ctrl+P Do Sw_Mode WITH "Print"

*
*    BROWSE - ฌฅญ๎
*
    BROWSE FIELDS L_NAME:40:H="",     ;
                  V_QNT=IIF(IS_QNT, "  ๛   ","      "):H="", ;
                  V_PRC=IIF(IS_PRICE, "  ๛   ","      "):H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Show_Who()  ;
           NOAPPEND REST NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Corr")
    ENDIF

  CASE what_do = "Corr"    && ฎเเฅชๆจ๏ แฏจแช 
    DO Fill_Tmp
    IF Corr_Fre()
      DO Save_Tmp
    ENDIF
    SELECT WK_TIT
    REPLACE WK_TIT.LOCKED WITH "***"
    what_do = "List"

  CASE what_do = "Add"     && ฎก ขซฅญจฅ แฏจแช 

    DO Inc_Wk WITH .T.
    what_do = "List"

  CASE what_do = "Print"   && ฅ็ โ์

    DO P_List
    what_do = "List"

  CASE what_do = "Title"   && ฎเเฅชๆจ๏ ง ฃฎซฎขช 

    DO Inc_Wk WITH .F.
    what_do = "List"

  CASE what_do = "Del"     && “ค ซฅญจฅ แฏจแช 

    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"  ฅฉแโขจโฅซ์ญฎ ใค ซจโ์? [   ] [ ฅโ ]"
    mss(3) = ""
    IF Out_Mess(7, "mss") = 1
      SELECT WK_LIST
      DO WHILE SEEK(RECNO("WK_TIT"))
        REPLACE LIST_NO WITH 0
      ENDDO
      SELECT WK_TIT
      DELETE
    ENDIF

    what_do = "List"

  CASE what_do = "Price"   && “แโ ญฎขช  ไซ ฆช  "ๆฅญ๋"

    REPLACE IS_PRICE WITH .NOT. IS_PRICE
    what_do = "List"

  CASE what_do = "Qnt"     && “แโ ญฎขช  ไซ ฆช  "ชฎซจ็ฅแโขฎ"

    REPLACE IS_QNT WITH .NOT. IS_QNT
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

CLEAR WINDOWS
CLOSE DATABASES
DELETE FILE (tmpo_path+"TMP_WK.DBF")

RELEASE MENU (menu_name) EXTENDED

RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                        ญไฎเฌ ๆจฎญญ๋ฉ กซ ญช                            บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 21.06.97 13:12:51 อผ
PROCEDURE Show_Who
PRIVATE w1, w2, p1, p2

ACTIVATE WINDOW (win_name+"_M") SAME

w1 = SPACE(30)
w2 = SPACE(30)
p1 = SPACE(16)
p2 = SPACE(16)

IF SEEK(WK_TIT.WHO_CR,"PERSONS")
  w1 = PADR(ALLTRIM(PERSONS.FAMILY)+" "+  ;
       LEFT(PERSONS.NAME,1)+". "+         ;
       LEFT(PERSONS.S_NAME,1)+".", 30)
ENDIF
IF SEEK(WK_TIT.WHO,"PERSONS")
  w2 = PADR(ALLTRIM(PERSONS.FAMILY)+" "+  ;
       LEFT(PERSONS.NAME,1)+". "+         ;
       LEFT(PERSONS.S_NAME,1)+".", 30)
ENDIF
IF SEEK(WK_TIT.WHERE_CR,"ARM_LST")
  p1 = ARM_LST.ARM_N
ENDIF
IF SEEK(WK_TIT.WHERE,"ARM_LST")
  p2 = ARM_LST.ARM_N
ENDIF
@ WROWS()-3, 9 SAY w1+" "+p1
@ WROWS()-2, 9 SAY w2+" "+p2

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                            ๅข โจโ์ แฏจแฎช!                            บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 07.05.97 11:22:51 อผ
PROCEDURE Get_L

PRIVATE mss, sel_sav, arm_name

IF WK_TIT.LOCKED # "***"
  sel_sav = SELECT()
  SELECT 0
  USE (base_path+"ARM_LST") ORDER TAG ARM_C ALIAS ARM000 AGAIN
  IF SEEK(WK_TIT.LOCKED)
    arm_name = ALLTRIM(ARM_N)
    DIMENSION mss(4)
    mss(1) = ""
    mss(2) = CHR(0)+"€…! โฎโ แฏจแฎช ข เ กฎโฅ, € "+'"'+arm_name+'"!'
    mss(3) = CHR(0)+"   ช ฏฎแโใฏจฌ? [ ใแโ์ เ กฎโ ๎โ ] [  ฏซฅข โ์! ]"
    mss(4) = ""
    IF Out_Mess(7, "mss") = 1
      USE
      SELECT (sel_sav)
      RETURN .F.
    ENDIF
  ENDIF
  USE
  SELECT (sel_sav)
ENDIF

REPLACE WK_TIT.LOCKED WITH arm

RETURN .T.

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                           ฎก ขซฅญจฅ แฏจแช .                           บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 21.06.97 12:20:04 อผ
PROCEDURE Inc_Wk
PARAMETER add

*
*  ฅเฅฌฅญญ๋ฅ แฎแโฎ๏ญจ๏ คซ๏ ญ ขจฃ ๆจจ
*

IF .NOT. add
  IF .NOT. Get_L()
    RETURN
  ENDIF
ENDIF

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

*
*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ ฺฤ ฌ๏ แฏจแช  ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ ณ
*ณ ณ ฐฐฐฐฒฐฐฐฐÛฐฐฐฐฒฐฐฐฐÛฐฐฐฐฒฐฐฐฐÛฐฐฐฐฒฐฐฐฐÛ ณ ณ
*ณ ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู ณ
*ณ ฺฤ ”ซ ฆชจ ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ ณ
*ณ ณ        [ ] ฎซจ็ฅแโขฎ    [ ] –ฅญ         ณ ณ
*ณ ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู ณ
*ณ       < OK Ctrl-W > < โช ง โ์แ๏ Esc >       ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

*------------------------------------------------------------------------
*    ‘ฎคฅเฆ โฅซ์ญ ๏ ็ แโ์ ฏเฎฃเ ฌฌ๋:
*

PRIVATE ex, nm_w, f_p, f_q
IF add
  nm_w = SPACE(FSIZE("L_NAME"))
  f_p  = .F.
  f_q  = .T.
ELSE
  nm_w = L_NAME
  f_q  = IS_QNT
  f_p  = IS_PRICE
ENDIF
                     &&   ก๊๏ขซ๏ฅฌ จ ง ฏฎซญ๏ฅฌ ฏฎซ๏ กซ ญช 
ex   = 1             &&

PUSH KEY CLEAR       &&   ขแ๏ชจฉ ฏฎฆ เญ๋ฉ แซใ็ ฉ!
DO Prp_Nav_2
DO D_Win_N WITH 10, 50, " ฃฎซฎขฎช แฏจแช "
DO Sun_Bord WITH 2, 3, 4, 46, " ฌ๏ แฏจแช  "
DO Sun_Bord WITH 5, 3, 7, 46, " ”ซ ฆชจ "
*------------------------------------------------------------------------
*      ขฎค ฏฎซฅฉ กซ ญช 
*
@ 3,  5 GET nm_w
@ 6, 12 GET f_q PICTURE "@*C ฎซจ็ฅแโขฎ"
@ 6, 30 GET f_p PICTURE "@*C –ฅญ "
@ 8,  9 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? โช ง โ์แ๏ Esc "

READ CYCLE VALID Tst_Blank()

REPLACE LOCKED WITH "***"

*--------------------------------------------------------------------------
POP KEY
RELEASE WINDOW (win_name)
RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                            ๋ๅฎค จง กซ ญช .                            บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 21.06.97 12:20:04 อผ
PROCEDURE Tst_Blank
PRIVATE mss, dlt

IF ex = 2 .OR. READKEY() % 256 = 12
  RETURN .T.
ENDIF

IF EMPTY(nm_w)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"ฌ๏ แฏจแช  ญฅ ฌฎฆฅโ ก๋โ์ ฏใแโ๋ฌ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF add
  dlt = SET("DELETED") = "ON"
  SET DELETED OFF
  LOCATE FOR DELETED()
  IF FOUND()
    RECALL
  ELSE
    APPEND BLANK
  ENDIF
  IF dlt
    SET DELETED ON
  ENDIF
  REPLACE L_NAME   WITH nm_w, ;
          IS_QNT   WITH f_q,  ;
          IS_PRICE WITH f_p,  ;
          WHO_CR   WITH user, ;
          WHERE_CR WITH arm,  ;
          WHO      WITH 0,    ;
          WHERE    WITH "***"
ELSE
  REPLACE L_NAME   WITH nm_w, ;
          IS_QNT   WITH f_q,  ;
          IS_PRICE WITH f_p,  ;
          WHO      WITH user, ;
          WHERE    WITH arm
ENDIF

RETURN .T.

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                       ฏฎซญฅญจฅ ขเฅฌฅญญฎฃฎ ไ ฉซ .                      บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 21.06.97 13:55:51 อผ
PROCEDURE Fill_Tmp
PRIVATE a_trans

IF .NOT. Get_L()  &&  ๅข โจโ์ แฏจแฎช!
  RETURN
ENDIF

SELECT STOCK
SET ORDER TO TAG CODE
SELECT TMP_WK
ZAP
IF SEEK(RECNO("WK_TIT"), "WK_LIST")
  SELECT WK_LIST
  SCAN REST WHILE LIST_NO = RECNO("WK_TIT")
    &&
    && เฎขฅเจฌ STOCK จ ฏฎคญฎขจฌ ญ จฌฅญฎข ญจฅ
    &&
    IF SEEK(CODE, "STOCK") .AND. EMPTY(STOCK.DATE_OFF)
      REPLACE PREFIX WITH STOCK.PREFIX, ;
              NAME   WITH STOCK.NAME,   ;
              PRODUCER WITH STOCK.PRODUCER
      SCATTER TO a_trans
      SELECT TMP_WK
      APPEND FROM ARRAY a_trans
      SELECT WK_LIST
    ELSE
      REPLACE LIST_NO WITH 0
    ENDIF
  ENDSCAN
ENDIF
SELECT TMP_WK
APPEND BLANK

RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                           ‘ฎๅเ ญฅญจฅ แฏจแช .                           บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 23.06.97 09:31:07 อผ
PROCEDURE Save_Tmp
PRIVATE a_trans

SELECT WK_LIST
DO WHILE SEEK(RECNO("WK_TIT"))
  REPLACE LIST_NO WITH 0
ENDDO
SELECT TMP_WK
SCAN FOR CODE # 0 .AND. (QNT # 0 .OR. PRICE # 0)
  SCATTER TO a_trans
  SELECT WK_LIST
  IF SEEK(0)
    GATHER FROM a_trans
  ELSE
    APPEND FROM ARRAY a_trans
  ENDIF
ENDSCAN

RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                             ฅ็ โ์ แฏจแช .                             บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 03.07.98 10:46:13 อผ
PROCEDURE P_List

PRIVATE p_ln, fnt, n_cp, lft, n, i, j, t_p, f_p, t_d, f_d, d_year, ffeed, sw
PRIVATE sel_rem, sb, n_p_det, n_pg, f_pg
PRIVATE s_pg, str_w, nds, doc_tit, p_drctry
PRIVATE k00

sel_rem = SELECT()

DO Wt_Mess WITH "ฎคฃฎโฎขช  แฏจแช "
DO Prep_Tmp
DO Wt_Mess

DIMENSION sb(5,2)
sb( 1,1) = "{Pg/Pgs}"
sb( 1,2) = ""

sb( 2,1) = "{Name}"
sb( 2,2) = ALLTRIM(WK_TIT.L_NAME)

sb( 3,1) = "{Date}"
sb( 3,2) = DTOC(DATE())

sb( 4,1) = "{Who}"
IF WK_TIT.WHO_CR # 0 .AND. SEEK(WK_TIT.WHO_CR, "PERSONS")
  sb( 4,2) = ALLTRIM(PERSONS.FAMILY)+" "+LEFT(PERSONS.NAME,1)+". "+LEFT(PERSONS.S_NAME,1)+"."
ELSE
  sb( 4,2) = IIF(WK_TIT.WHO_CR = 0, "‘ ฌ!", "?")
ENDIF

sb( 5,1) = "{Where}"
IF SEEK(WK_TIT.WHERE_CR, "ARM_LST")
  sb( 5,2) = ALLTRIM(ARM_LST.ARM_N)
ELSE
  sb( 5,2) = "?"
ENDIF

n = ALEN(sb,1)
*FOR i = 1 TO n
*  sb(i,2) =PADR(sb(i,2), LEN(sb(i,1)))
*ENDFOR

SELECT 0
USE DOC_FORM

LOCATE FOR "FREE_LST" == ALLTRIM(UPPER(DOC_NAME))
p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed =DOC_FORM.F_FEED
p_drctry = DOC_FORM.P_DIR

n = MEMLINES(PAGE_H)
IF n = 0
  DIMENSION t_p(1)
  t_p(1) = ""
ELSE
  DIMENSION t_p(n)
  FOR i = 1 TO n
    t_p(i) = MLINE(PAGE_H,i)
  ENDFOR
ENDIF

n = MEMLINES(PAGE_F)
IF n = 0
  DIMENSION f_p(1)
  f_p(1) = ""
ELSE
  DIMENSION f_p(n)
  FOR i = 1 TO n
    f_p(i) = MLINE(PAGE_F,i)
  ENDFOR
ENDIF

n = MEMLINES(DOC_H)
IF n = 0
  DIMENSION t_d(1)
  t_d(1) = ""
ELSE
  DIMENSION t_d(n)
  FOR i = 1 TO n
    t_d(i) = MLINE(DOC_H,i)
  ENDFOR
ENDIF

n = MEMLINES(DOC_F)
IF n = 0
  DIMENSION f_d(1)
  f_d(1) = ""
ELSE
  DIMENSION f_d(n)
  FOR i = 1 TO n
    f_d(i) = MLINE(DOC_F,i)
  ENDFOR
ENDIF
USE

SELECT TMP_LIST

DO Ini_Prn WITH "‘ฏจแฎช", p_ln, lft, n_cp, fnt, ffeed, p_drctry

GO TOP

n_p_det = p_ln-MAX(ALEN(t_p),ALEN(t_d))-MAX(ALEN(f_p),ALEN(f_d))
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
    str_w = PREFIX+" "+NAME+" "+PRODUCER+TRANSFORM(PRICE,"@Z 999999.99")+TRANSFORM(QNT,"@Z 9999999")
    @ PROW()+1,0 SAY str_w
    SKIP
    IF EOF()
      EXIT
    ENDIF
  ENDFOR

  IF f_pg = n_pg
    FOR i = 1 TO ALEN(f_d)
      str_w = f_d(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      @ PROW()+1, 0 SAY str_w
    ENDFOR
  ELSE
    FOR i = 1 TO ALEN(f_p)
      str_w = f_p(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      @ PROW()+1, 0 SAY str_w
    ENDFOR
  ENDIF
ENDFOR
USE

DO Term_Prn WITH "", tmpo_path+"TMP_LIST.DBF"

DELETE FILE (tmpo_path+"TMP_LIST.DBF")
DELETE FILE (tmpo_path+"TMP_LIST.CDX")
SELECT (sel_rem)
RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                     ”ฎเฌจเฎข ญจฅ ขเฅฌฅญญฎฃฎ ไ ฉซ .                     บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 03.07.98 11:13:12 อผ
PROCEDURE Prep_Tmp

DELETE FILE (tmpo_path+"TMP_LIST.DBF")
DELETE FILE (tmpo_path+"TMP_LIST.CDX")
SELECT 0
CREATE DBF (tmpo_path+"TMP_LIST")  ;
   ( CODE     N( 7), ;
     PREFIX   C( 4), ;
     NAME     C(35), ;
     PRODUCER C( 8), ;
     PRICE    N(8,2), ;
     QNT      N( 6) )
USE (tmpo_path+"TMP_LIST") EXCLUSIVE ALIAS TMP_LIST
SELECT WK_LIST
SEEK RECNO("WK_TIT")
SCAN REST WHILE RECNO("WK_TIT") = LIST_NO
  IF .NOT. SEEK(CODE,"STOCK")
    LOOP
  ENDIF
  SELECT TMP_LIST
  APPEND BLANK
  REPLACE CODE     WITH STOCK.CODE, ;
          PREFIX   WITH STOCK.PREFIX, ;
          NAME     WITH STOCK.NAME, ;
          PRODUCER WITH STOCK.PRODUCER, ;
          PRICE    WITH IIF(WK_TIT.IS_PRICE, WK_LIST.PRICE, 0), ;
          QNT      WITH IIF(WK_TIT.IS_QNT, WK_LIST.QNT, 0)
  SELECT WK_LIST
ENDSCAN

SELECT TMP_LIST
INDEX ON NAME+PREFIX+PRODUCER TAG NAME
RETURN
