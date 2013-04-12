*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ Let_Spec      งเ กฎโ็จช €ญคเฅฉ  แจญ           10.04.98 13:14:29 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                  เฎฃเ ฌฌ  ใโขฅเฆคฅญจ๏ แฏฅๆคขจฆฅญจ๏.                   บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Let_Spec

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
*   โชเ๋ข ฅฌ ไ ฉซ๋ .., ใแโ ญ ขซจข ฅฌ แข๏งจ จ โ. ค.
*
DO Use_Dummy
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE
SET FILTER TO EMPTY(DATE_OFF)
PRIVATE cnt
COUNT REST WHILE CUS_CODE < 0 TO cnt

IF cnt = 0
  CLOSE DATABASES
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"ฅโ ญจ ฎคญฎฃฎ แฏฅๆชซจฅญโ !"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF
GO TOP

*
*   ฏเฅคฅซ๏ฅฌ  แจญๅเฎญญฎฅ ฌฅญ๎
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "๋กฎเ" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "๋ๅฎค" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*    แ็ฅโ เ งฌฅเฎข ฎชญ 
*
PRIVATE ln, wd
ln = MIN(100, cnt)   && ฎซจ็ฅแโขฎ ขจคจฌ๋ๅ แโเฎช BROWSE
wd = 24

DO D_Wins WITH ln, wd, "‘ฏฅๆ.ชซจฅญโ๋", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && เฎแฌฎโเ แฏจแช 

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter      KEYBOARD CHR(23)

*
*    BROWSE - ฌฅญ๎
*
    BROWSE KEY -9999999, -1 ;
           FIELDS F000 = LEFT(CUS_NAME,24):24:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && ”ฎเฌจเฎข ญจฅ แฏจแช  จ ใโขฅเฆคฅญจฅ

    DO Execute WITH ACCOUNT.CUS_CODE, ACCOUNT.CUS_NAME
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
CLOSE DATABASES
RELEASE MENU (menu_name) EXTENDED

RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ Execute       งเ กฎโ็จช €ญคเฅฉ  แจญ           10.04.98 13:37:23 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                    ฎคฃฎโฎขช  แฏจแช  จ ใโขฅเฆคฅญจฅ.                    บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Execute
PARAMETERS c_code, c_name
PRIVATE s_sav
s_sav = SELECT()

DO Prep_Tmp WITH c_code, c_name

SELECT TMP
SET ORDER TO TAG NAME

DO Prp_List

SELECT (s_sav)

RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                      ฎคฃฎโฎขช  ขเฅฌฅญญฎฃฎ ไ ฉซ .                      บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 10.04.98 13:39:27 อผ
PROCEDURE Prep_Tmp
PARAMETERS c_code, c_name

PRIVATE mss, f_name, exist

f_name = "L_T_"+ALLTRIM(STR(ABS(c_code)))
exist = .F.
IF File_O(tmpo_path+f_name+".dbf") .AND. File_O(tmpo_path+f_name+".cdx")
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+' ฃฎโฎขช  คซ๏ แฏจแช  โจฏ  "'+ALLTRIM(c_name)+'" ใฆฅ แใ้ฅแโขใฅโ.'
  mss(2) = CHR(0)+"  —โฎ กใคฅฌ คฅซ โ์? [ เฎคฎซฆจฌ ] [ ็จแโจฌ ]"
  mss(3) = ""
  exist = Out_Mess (5, "mss") = 1
ENDIF

IF exist
  DO Refr_Tmp WITH c_code, f_name
ELSE
  DO Crea_Tmp WITH c_code, f_name
ENDIF

RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                      ‘ฎงค ญจฅ  ก ง๋ ค ญญ๋ๅ.                       บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 10.04.98 13:56:00 อผ
PROCEDURE Crea_Tmp
PARAMETERS c_code, f_name

PRIVATE s_sav, c_str

c_str = STR(c_code,8)
s_sav = SELECT()

SELECT 0

DELETE FILE (tmpo_path+f_name+".dbf")
DELETE FILE (tmpo_path+f_name+".cdx")
CREATE DBF (tmpo_path+f_name) ;
  ( CODE     N( 7), ;
    PREFIX   C( 4), ;
    NAME     C(35), ;
    PRODUCER C( 8), ;
    QNT_CUS  N( 6), ;
    QNT      N( 6) )

USE (tmpo_path+f_name) EXCLUSIVE ALIAS TMP

SELECT 0
USE (base_path+"SPEC_SUM") ORDER TAG POS_CUS

SELECT 0
USE (base_path+"STOCK")
SET RELATION TO STR(CODE,7)+c_str INTO SPEC_SUM

DO Wt_Mess WITH "ฎคฃฎโฎขช  ค ญญ๋ๅ..."
SCAN
  SELECT TMP
  APPEND BLANK
  REPLACE CODE     WITH STOCK.CODE,     ;
          PREFIX   WITH STOCK.PREFIX,   ;
          NAME     WITH STOCK.NAME,     ;
          PRODUCER WITH STOCK.PRODUCER, ;
          QNT_CUS  WITH SPEC_SUM.QNT
  SELECT STOCK
ENDSCAN
USE
SELECT SPEC_SUM
USE

SELECT TMP
INDEX ON CODE TAG CODE
INDEX ON NAME+PREFIX+PRODUCER TAG NAME ADDITIVE
INDEX ON NAME+PREFIX+PRODUCER TAG NAME1 FOR QNT_CUS # 0 ADDITIVE
INDEX ON NAME+PREFIX+PRODUCER TAG NAME2 FOR QNT # 0 ADDITIVE

DO Wt_Mess

SELECT (s_sav)

RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                    กญฎขซฅญจฅ ‘’€ ก ง๋ ค ญญ๋ๅ.                      บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 13.04.98 10:20:00 อผ
PROCEDURE Refr_Tmp
PARAMETERS c_code, f_name

PRIVATE s_sav, c_str

c_str = STR(c_code,8)
s_sav = SELECT()
SELECT 0

USE (tmpo_path+f_name) EXCLUSIVE ALIAS TMP ORDER TAG CODE

SELECT 0
USE (base_path+"SPEC_SUM") ORDER TAG POS_CUS

SELECT 0
USE (base_path+"STOCK")
SET RELATION TO STR(CODE,7)+c_str INTO SPEC_SUM
SET RELATION TO CODE INTO TMP ADDITIVE

DO Wt_Mess WITH "กญฎขซฅญจฅ ค ญญ๋ๅ..."
SCAN
  SELECT TMP
  IF .NOT. FOUND()
    APPEND BLANK
  ENDIF
  REPLACE CODE     WITH STOCK.CODE,     ;
          PREFIX   WITH STOCK.PREFIX,   ;
          NAME     WITH STOCK.NAME,     ;
          PRODUCER WITH STOCK.PRODUCER, ;
          QNT_CUS  WITH SPEC_SUM.QNT
  SELECT STOCK
ENDSCAN
USE
SELECT SPEC_SUM
USE

SELECT TMP
INDEX ON CODE TAG CODE
INDEX ON NAME+PREFIX+PRODUCER TAG NAME ADDITIVE
INDEX ON NAME+PREFIX+PRODUCER TAG NAME1 FOR QNT_CUS # 0 ADDITIVE
INDEX ON NAME+PREFIX+PRODUCER TAG NAME2 FOR QNT # 0 ADDITIVE

DO Wt_Mess

SELECT (s_sav)

RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                          ”ฎเฌเฎข ญจฅ แฏจแช .                           บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 13.04.98 10:49:12 อผ
PROCEDURE Prp_List
PRIVATE list_t, rc_sav

list_t = "แ๏ ญฎฌฅญชซ โใเ "
*list_t = "”จซจ ซ          "
*list_t = "“โขฅเฆค ฅฌ๋ฅ    "

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

DEFINE PAD List_T OF (menu_name) PROMPT "”จซ์โเ" KEY Ctrl-A, "F2"
ON SELECTION PAD List_T OF (menu_name) DO Sw_Mode WITH "Type_L"

DEFINE PAD Move OF (menu_name) PROMPT "ขจฆฅญจฅ" KEY Ctrl-A, "F1"
ON SELECTION PAD Move OF (menu_name) DO Sw_Mode WITH "Move"

DEFINE PAD Let_All OF (menu_name) PROMPT "“โขฅเคจโ์ ขแฅ" KEY Ctrl-A, "F6"
ON SELECTION PAD Let_All OF (menu_name) DO Sw_Mode WITH "Let_All"

DEFINE PAD Exit OF (menu_name) PROMPT "๋ๅฎค" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*    แ็ฅโ เ งฌฅเฎข ฎชญ 
*
PRIVATE ln, wd
ln = MIN(100, RECCOUNT())   && ฎซจ็ฅแโขฎ ขจคจฌ๋ๅ แโเฎช BROWSE
wd = 63

DO D_Wins WITH ln, wd, ALLTRIM(ACCOUNT.CUS_NAME), 1, 0
@ 2, 10 SAY "ฎงจๆจ๏"
@ 2, 42 SAY "ฎซ-ขฎ"
@ 2, 50 SAY "“โข."
what_do = "List"
@ 1, 2 SAY "ฎจแช Tab"
GO TOP

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && เฎแฌฎโเ แฏจแช 

    DO CASE
    CASE list_t = "แ๏ ญฎฌฅญชซ โใเ "
      SET ORDER TO TAG NAME
    CASE list_t = "”จซจ ซ          "
      SET ORDER TO TAG NAME1
    OTHERWISE
      SET ORDER TO TAG NAME2
    ENDCASE
    ACTIVATE WINDOW (win_name+"_M") SAME
    @ 1, WCOLS()-LEN(list_t)-2 SAY list_t COLOR SCHEME 1
    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Tab DO Seek_1
    ON KEY LABEL F2  DO Sw_Mode WITH "Type_L"
    ON KEY LABEL F6  DO Sw_Mode WITH "Let_All"
    ON KEY LABEL F1  DO Sw_Mode WITH "Move"

*
*    BROWSE - ฌฅญ๎
*
    BROWSE FIELDS PREFIX:H="",   ;
                  NAME:H="",     ;
                  PRODUCER:H="", ;
                  QNT_CUS:H="":P="@Z 999999", ;
                  QNT:H="":P="@Z 999999" ;
                  FREEZE QNT ;
           NOAPPEND NODELETE NOLGRID NORGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = "Exit"
    ENDIF

  CASE what_do = "Type_L"     && ’จฏ แฏจแช 

    DO CASE
    CASE list_t = "แ๏ ญฎฌฅญชซ โใเ "
      list_t = "”จซจ ซ          "
    CASE list_t = "”จซจ ซ          "
      list_t = "“โขฅเฆค ฅฌ๋ฅ    "
    OTHERWISE
      list_t = "แ๏ ญฎฌฅญชซ โใเ "
    ENDCASE
    what_do = "List"

  CASE what_do = "Let_All"    && “โขฅเคจโ์ ขแฅ!

    rc_sav = RECNO()
    DO Wt_Mess WITH " แแโ ขซ๏ฅฌ ชฎซจ็ฅแโข ..."
    SET ORDER TO
    REPLACE ALL QNT WITH QNT_CUS
    IF BETWEEN(rc_sav, 1, RECCOUNT())
      GO rc_sav
    ENDIF
    DO Wt_Mess
    what_do = "List"

  CASE what_do = "Move"       && ญไฎเฌ ๆจ๏ ฎ คขจฆฅญจจ

    DO Sp_Move WITH CODE, ACCOUNT.CUS_CODE
    what_do = "List"

  OTHERWISE

    IF .NOT. Exp_Spec()
      what_do = "List"
    ELSE
      EXIT
    ENDIF
    
  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                      ฎจแช ฏฎ ญ ็ ซ์ญ๋ฌ แจฌขฎซ ฌ!                      บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 13.04.98 11:51:11 อผ
PROCEDURE Seek_1
PRIVATE flt_pos, max_len, str_flt, s_n, s_c, ins_flt
PRIVATE chr_shab, i, pos_new, str_new, to_seek, rc_sav

DEFINE WINDOW Hide_W FROM 0, 0 TO 0, WCOLS("")-1 NONE COLOR SCHEME 13
SHOW WINDOW Hide_W
flt_pos = 0
max_len = FSIZE("NAME")-1
str_flt = ""
s_n = 0
s_c = ""
ins_flt = INSMODE()
chr_shab = ""
FOR i = 32 TO 126
  chr_shab = chr_shab+CHR(i)
ENDFOR
FOR i = 128 TO 175
  chr_shab = chr_shab+CHR(i)
ENDFOR
FOR i = 224 TO 241
  chr_shab = chr_shab+CHR(i)
ENDFOR

ACTIVATE WINDOW (win_name+"_M") SAME

DO WHILE .T.
  to_seek = .F.
  @ 1, 8 SAY PADR(str_flt, max_len+1, "ฑ") COLOR SCHEME 1
  @ 1, 8+flt_pos SAY ""
  s_n = INKEY(0)
  IF s_n < 0       && ”ใญชๆจฎญ ซ์ญ ๏ ชซ ขจ่  - ฌจฌฎ โ งจช !
    LOOP
  ENDIF
  s_c = CHR(s_n)

  IF s_c $ chr_shab    && ฅ็ โญ๋ฉ แจฌขฎซ!
    str_new = LEFT(str_flt,flt_pos)+s_c+SUBSTR(str_flt,flt_pos+IIF(ins_flt,1,2))
    str_new = LEFT(str_new, max_len+1)
    pos_new = MIN(flt_pos+1, max_len)
    to_seek = .T.
  ENDIF

  IF s_n = 19          && ‘โเฅซช  ขซฅขฎ
    flt_pos = MAX(flt_pos-1,0)
  ENDIF

  IF s_n = 4           && ‘โเฅซช  ขฏเ ขฎ
    flt_pos = MIN(flt_pos+1, LEN(str_flt), max_len)
  ENDIF

  IF s_n = 7           && Del
    str_new = LEFT(str_flt,flt_pos)+SUBSTR(str_flt,flt_pos+2)
    pos_new = flt_pos
    to_seek = .T.
  ENDIF

  IF s_n = 127         && BackSpace
    str_new = LEFT(str_flt,flt_pos-1)+SUBSTR(str_flt,flt_pos+1)
    pos_new = MAX(flt_pos-1,0)
    to_seek = .T.
  ENDIF

  IF s_n = 22          && Ins
    ins_flt = .NOT. ins_flt
    = INSMODE(ins_flt)
  ENDIF

  IF s_n = 1           && Home
    flt_pos = 0
  ENDIF

  IF s_n = 6           && End
    flt_pos = MIN(LEN(str_flt), max_len)
  ENDIF

  IF s_n = 13          && Enter
    EXIT
  ENDIF

  IF INLIST(s_n, 24, 5, 3, 18)
    DO CASE
    CASE s_n = 24  && ‘โเฅซช  ขญจง
      i = 1
    CASE s_n = 5   && ‘โเฅซช  ขขฅเๅ
      i = -1
    CASE s_n = 3   && PgDn
      i = WROWS(win_name+"_E")-1
    OTHERWISE    && PgUp
      i = -WROWS(win_name+"_E")+1
    ENDCASE
    SKIP i
    IF BOF()
      GO TOP
    ENDIF
    IF EOF()
      GO BOTTOM
    ENDIF
    SHOW WINDOW (ALIAS()) REFRESH SAME
  ENDIF

  IF to_seek
    rc_sav = RECNO()
    IF SEEK(str_new)
      str_flt = str_new
      flt_pos = pos_new
      SHOW WINDOW (ALIAS()) REFRESH SAME
    ELSE
      IF BETWEEN(rc_sav, 1, RECCOUNT())
        GO rc_sav
      ENDIF
    ENDIF
  ENDIF

ENDDO

RELEASE WINDOW Hide_W

@ 1, 8 SAY PADR("Tab", max_len+1)

ACTIVATE WINDOW (ALIAS()) SAME

RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                       ขฅเ่ ฅฌ เ กฎโใ แฎ แฏจแชฎฌ.                      บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 13.04.98 15:17:03 อผ
PROCEDURE Exp_Spec

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

*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ                                                                 ณ
*ณ               เจฌฅ็ ญจฅ ....:....!....:....!....               ณ
*ณ                                                                 ณ
*ณ                      [ ] ฅ็ โ์ คฎชใฌฅญโ                        ณ
*ณ                                                                 ณ
*ณ < “โขฅเคจโ์ Ctrl-W > < ‘ฎๅเ ญจโ์ > <  ก๋โ์ > < ฅเญใโ์แ๏ Esc > ณ
*ณ                                                                 ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

PRIVATE ex, nt, pr, f_name
f_name = "L_T_"+ALLTRIM(STR(ABS(ACCOUNT.CUS_CODE)))
ex = 1
nt = SPACE(24)
pr = .F.
*------------------------------------------------------------------------

PUSH KEY CLEAR       &&   ขแ๏ชจฉ ฏฎฆ เญ๋ฉ แซใ็ ฉ!
DO Prp_Nav_2
DO D_Win_N WITH 10, 69, " ช ง ขฅเ่จฌ เ กฎโใ แฎ แฏจแชฎฌ?"

*------------------------------------------------------------------------
*      ขฎค ฏฎซฅฉ กซ ญช 
*
@ 3, 17 SAY "เจฌฅ็ ญจฅ" GET nt
@ 5, 24 GET pr PICTURE "@*C ฅ็ โ์ คฎชใฌฅญโ "
@ 7,  3 GET ex PICTURE "@*HT \! “โขฅเคจโ์ Ctrl-W ;\ ‘ฎๅเ ญจโ์ ;\  ก๋โ์ ;\? ฅเญใโ์แ๏ Esc "

READ CYCLE

DO CASE
CASE ex = 4
  POP KEY
  RELEASE WINDOW (win_name)
  RETURN .F.
CASE ex = 1

  PRIVATE d_num, d_dt, cc_w
  SELECT TMP
  SET ORDER TO TAG NAME2
  GO TOP
  IF BOF() .OR. EOF()
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"‘ฏจแฎช ฏใแโ!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
  ELSE
    d_num = Doc_Num("LET_SPEC", def_firm, arm, DATE())
    d_num = STR(d_num,10)
    d_dt = DATE()
    SELECT 0
    USE (base_path+"STOCK") ORDER TAG CODE ALIAS STO_LET AGAIN
    SELECT 0
    USE (base_path+"SPEC_SUM") ORDER TAG POS_CUS ALIAS SPEC_LST AGAIN
    SELECT 0
    USE (base_path+"LET_SP_T") ALIAS DOC_TIT AGAIN
    APPEND BLANK
    REPLACE DOC_NUM  WITH d_num,    ;
            DOC_DATE WITH d_dt,     ;
            DOC_TIME WITH TIME(),   ;
            CUS_CODE WITH ACCOUNT.CUS_CODE, ;
            WHO_LET  WITH user,     ;
            NOTE     WITH nt,       ;
            WHO      WITH user,     ;
            WHERE    WITH arm

    cc_w = STR(ACCOUNT.CUS_CODE,8)
    USE (base_path+"LET_SPEC") ALIAS DOC_DET AGAIN
    SELECT TMP
    SCAN
      = SEEK(CODE, "STO_LET")
      REPLACE STO_LET.QNT_SPEC WITH STO_LET.QNT_SPEC-TMP.QNT
      SELECT SPEC_LST
      IF .NOT. SEEK(STR(TMP.CODE,7)+cc_w)
        DO New_Rec WITH TMP.CODE, ACCOUNT.CUS_CODE
      ENDIF
      REPLACE QNT WITH QNT-TMP.QNT
      IF QNT = 0
        REPLACE CODE WITH 0, CUS_CODE WITH 0
      ENDIF
      SELECT DOC_DET
      APPEND BLANK
      REPLACE DOC_NUM  WITH d_num,    ;
              DOC_DATE WITH d_dt,     ;
              CODE     WITH TMP.CODE, ;
              QNT      WITH TMP.QNT
      SELECT TMP
    ENDSCAN
    SELECT STO_LET
    USE
    SELECT SPEC_LST
    USE
    SELECT DOC_DET
    USE
    SELECT TMP
    DO SpecToVK WITH d_num, d_dt
    IF pr
      DO P_L_Spec WITH d_num, d_dt
    ENDIF
  ENDIF
  USE
  DELETE FILE (tmpo_path+f_name+".dbf")
  DELETE FILE (tmpo_path+f_name+".cdx")
  POP KEY
  RELEASE WINDOW (win_name)
  RETURN .T.
  
CASE ex = 2

  SELECT TMP
  USE
  POP KEY
  RELEASE WINDOW (win_name)
  RETURN .T.

CASE ex = 3

  SELECT TMP
  USE
  DELETE FILE (tmpo_path+f_name+".dbf")
  DELETE FILE (tmpo_path+f_name+".cdx")
  POP KEY
  RELEASE WINDOW (win_name)
  RETURN .T.

ENDCASE
*--------------------------------------------------------------------------

RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ              ฎก ขซฅญจฅ ง ฏจแจ ข ฌฅคซฅญญฎ เ แโใ้จฉ ไ ฉซ.               บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 06.04.98 13:59:08 อผ
PROCEDURE New_Rec
PARAMETERS s_c, c_c

PRIVATE arr_w, to_add

SCATTER TO arr_w BLANK
arr_w(1) = s_c
arr_w(2) = c_c

to_add = .T.

DO WHILE SEEK("      0       0")
  IF LOCK()
    to_add = .F.
    EXIT
  ENDIF
ENDDO

IF to_add
  APPEND FROM ARRAY arr_w
ELSE
  GATHER FROM arr_w
ENDIF

UNLOCK

RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                          ‘ฏเ ขช  ฎ คขจฆฅญจจ.                           บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 14.04.98 09:22:37 อผ
PROCEDURE Sp_Move
PARAMETERS s_code, c_code

PRIVATE s_sav, tim_w, tim_w1, tim_w2, tim_w3, dt_s

s_sav = SELECT()

SELECT 0

USE (base_path+"PARMS") ALIAS PPP AGAIN
dt_s = DAT_START
USE
DELETE FILE (tmpo_path+"SP_MOVE.DBF")
DELETE FILE (tmpo_path+"SP_MOVE.CDX")

CREATE DBF (tmpo_path+"SP_MOVE") ;
   (  DOC_NUM   C(10),  ;
      FIRM      C( 3),  ;
      DOC_NUMW  C(11),  ;
      DOC_TYPE  N( 1),  ;
      DOC_T_W   C( 6),  ;
      DOC_DATE  D,      ;
      DOC_TIME  C( 8),  ;
      WHO       N( 3),  ;
      QNT       N( 6),  ;
      PRICE     N(8,2), ;
      REST      N( 6)  )
USE (tmpo_path+"SP_MOVE") EXCLUSIVE

*
*     แๅฎค
*
SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG DOC_NUM ALIAS MOVE_TIT AGAIN
SELECT 0
USE (base_path+"SALE") ORDER TAG CODE ALIAS MOVE_DET AGAIN
SET RELATION TO FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM INTO MOVE_TIT

SEEK s_code
SCAN REST WHILE CODE = s_code
  IF MOVE_TIT.CUS_CODE # c_code .OR. DOC_DATE < dt_s
    LOOP
  ENDIF
  tim_w  = MOVE_TIT.DOC_TIME
  tim_w1 = FLOOR(tim_w/3600)
  tim_w2 = FLOOR((tim_w-tim_w1*3600)/60)
  tim_w3 = ROUND(tim_w-tim_w1*3600-tim_w2*60, 0)
  tim_w  = RIGHT(STR(100+tim_w1,3),2)+":"+ ;
           RIGHT(STR(100+tim_w2,3),2)+":"+ ;
           RIGHT(STR(100+tim_w3,3),2)
  SELECT SP_MOVE
  APPEND BLANK
  REPLACE DOC_NUM   WITH MOVE_TIT.DOC_NUM,  ;
          FIRM      WITH MOVE_TIT.FIRM,     ;
          DOC_NUMW  WITH LEFT(MOVE_TIT.DOC_NUM,4)+"-"+ALLTRIM(RIGHT(MOVE_TIT.DOC_NUM,6)),  ;
          DOC_TYPE  WITH 0,  ;
          DOC_T_W   WITH " แๅฎค",  ;
          DOC_DATE  WITH MOVE_TIT.DOC_DATE,  ;
          DOC_TIME  WITH tim_w,  ;
          WHO       WITH MOVE_TIT.WHO,     ;
          PRICE     WITH MOVE_DET.PRICE_R, ;
          QNT       WITH MOVE_DET.QNT
  SELECT MOVE_DET
ENDSCAN

*
*    เจๅฎค
*
SELECT MOVE_TIT
USE (base_path+"ACT_S_T") ORDER TAG DOC_NUM ALIAS MOVE_TIT AGAIN
SELECT MOVE_DET
USE (base_path+"ACT_S") ORDER TAG CODE ALIAS MOVE_DET AGAIN
SET RELATION TO FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM INTO MOVE_TIT

SEEK s_code
SCAN REST WHILE CODE = s_code
  IF MOVE_TIT.SUP_CODE # c_code .OR. DOC_DATE < dt_s
    LOOP
  ENDIF
  SELECT SP_MOVE
  APPEND BLANK
  REPLACE DOC_NUM   WITH MOVE_TIT.DOC_NUM,  ;
          FIRM      WITH MOVE_TIT.FIRM,     ;
          DOC_NUMW  WITH ALLTRIM(LEFT(MOVE_TIT.DOC_NUM,8))+"/"+ALLTRIM(RIGHT(MOVE_TIT.DOC_NUM,2)), ;
          DOC_TYPE  WITH 1,         ;
          DOC_T_W   WITH "เจๅฎค",  ;
          DOC_DATE  WITH MOVE_TIT.DOC_DATE,  ;
          WHO       WITH MOVE_TIT.WHO_ACT,   ;
          PRICE     WITH MOVE_DET.PRICE_R,   ;
          QNT       WITH -MOVE_DET.QNT_INC
  SELECT MOVE_DET
ENDSCAN

*
*    “โขฅเฆคฅญจฅ
*
SELECT MOVE_TIT
USE (base_path+"LET_SP_T") ORDER TAG DOC_NUM ALIAS MOVE_TIT AGAIN
SELECT MOVE_DET
USE (base_path+"LET_SPEC") ORDER TAG CODE ALIAS MOVE_DET AGAIN
SET RELATION TO LEFT(DTOS(DOC_DATE),4)+DOC_NUM INTO MOVE_TIT

SEEK s_code
SCAN REST WHILE CODE = s_code
  IF MOVE_TIT.CUS_CODE # c_code .OR. DOC_DATE < dt_s
    LOOP
  ENDIF
  SELECT SP_MOVE
  APPEND BLANK
  REPLACE DOC_NUM   WITH MOVE_TIT.DOC_NUM,  ;
          DOC_NUMW  WITH MOVE_TIT.DOC_NUM,  ;
          DOC_TYPE  WITH 2,         ;
          DOC_T_W   WITH "“โขฅเ.",  ;
          DOC_DATE  WITH MOVE_TIT.DOC_DATE,  ;
          DOC_TIME  WITH MOVE_TIT.DOC_TIME,  ;
          WHO       WITH MOVE_TIT.WHO,       ;
          QNT       WITH -MOVE_DET.QNT
  SELECT MOVE_DET
ENDSCAN

SELECT MOVE_TIT
USE
SELECT MOVE_DET
USE
SELECT SP_MOVE
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"ฅโ คขจฆฅญจ๏ ฏฎ ค ญญฎฉ ฏฎงจๆจจ."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  DELETE FILE (tmpo_path+"SP_MOVE.DBF")
  SELECT (s_sav)
  RETURN
ENDIF

INDEX ON DTOS(DOC_DATE)+DOC_TIME TAG HRON

SELECT 0
USE (base_path+"SPEC_SUM") ORDER TAG CUS_POS ALIAS M_SP000 AGAIN
PRIVATE q_flt
q_flt = 0

IF SEEK(STR(c_code,8)+STR(s_code,7))
  q_flt = QNT
ENDIF
USE
SELECT SP_MOVE
GO BOTTOM
DO WHILE .NOT. BOF()
  REPLACE REST WITH q_flt
  q_flt = q_flt-QNT
  SKIP -1
ENDDO

DO View_Mov WITH ALLTRIM(TMP.PREFIX)+" "+ALLTRIM(TMP.NAME)+"  /"+ALLTRIM(TMP.PRODUCER)+"/"

SELECT SP_MOVE
USE
DELETE FILE (tmpo_path+"SP_MOVE.DBF")
DELETE FILE (tmpo_path+"SP_MOVE.CDX")

SELECT (s_sav)
RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                           เฎแฌฎโเ คขจฆฅญจ๏.                           บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 14.04.98 10:41:35 อผ
PROCEDURE View_Mov
PARAMETER pos_name
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

DEFINE PAD Print OF (menu_name) PROMPT "ฅ็ โ์ คฎชใฌฅญโ " KEY Ctrl-A, "Enter"
ON SELECTION PAD Print OF (menu_name) DO Sw_Mode WITH "Print"

DEFINE PAD Exit OF (menu_name) PROMPT "๋ๅฎค" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   โชเ๋ข ฅฌ ไ ฉซ๋ .., ใแโ ญ ขซจข ฅฌ แข๏งจ จ โ. ค.
*
PRIVATE who_len
DO Use_Dummy
SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS WHO_CR AGAIN
who_len = FSIZE("FAMILY")+6
SELECT SP_MOVE
SET RELATION TO WHO INTO WHO_CR
GO BOTTOM
*
*    แ็ฅโ เ งฌฅเฎข ฎชญ 
*
PRIVATE ln, wd
ln = MIN(100, RECCOUNT())   && ฎซจ็ฅแโขฎ ขจคจฌ๋ๅ แโเฎช BROWSE
wd = 63

DO D_Wins WITH ln, wd, pos_name, 0, 1
@ 1, 2 SAY ;
"”เฌฤ โ  คฎช.ฤฤฎฌฅเ คฎช.ฤฤฤ’จฏฤฤฤฤฤฤ–ฅญ ฤฤเจๅฎคฤ แๅฎคฤแโ โ."
@ WROWS()-2, 3 SAY "ฎชใฌฅญโ แฎงค ซ"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && เฎแฌฎโเ แฏจแช 

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - ฌฅญ๎
*
    BROWSE FIELDS FIRM:H="":3,      ;
                  DOC_DATE:H="":10, ;
                  DOC_NUMW:H="":11, ;
                  DOC_T_W:6:H="",   ;
                  PRICE:H="":P="@Z 99999.99", ;
                  Q_INC = IIF(QNT > 0, QNT, 0):H="":P="@Z 999999", ;
                  Q_EXP = IIF(QNT < 0, -QNT, 0):H="":P="@Z 999999", ;
                  REST:H="":P="@Z 999999", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Show_Who() ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Print")
    ENDIF

  CASE what_do = "Print"    && ฅ็ โ์ คฎชใฌฅญโ 

    DO P_Doc
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

SET RELA TO
SELECT WHO_CR
USE
SELECT SP_MOVE
RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                                Footer!                                 บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 14.04.98 11:29:09 อผ
PROCEDURE Show_Who
PRIVATE nm

DO CASE
CASE SP_MOVE.WHO = 0
  nm = "‘€!"
CASE .NOT. FOUND("WHO_CR")
  nm = "ญชฎฃญจโฎ!"
OTHERWISE
  nm = ALLTRIM(WHO_CR.FAMILY)+" "+LEFT(WHO_CR.NAME,1)+". "+LEFT(WHO_CR.S_NAME,1)+"."
ENDCASE
nm = PADR(nm, who_len)
ACTIVATE WINDOW (win_name+"_M") SAME
@ WROWS()-2, 19 SAY nm COLOR SCHEME 1
ACTIVATE WINDOW (ALIAS()) SAME
RETURN .T.

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                           ฅ็ โ์ คฎชใฌฅญโ .                            บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 14.04.98 11:38:13 อผ
PROCEDURE P_Doc

DO CASE
CASE DOC_TYPE = 0
  DO Prn_Inv WITH FIRM, DOC_NUM, DOC_DATE
CASE DOC_TYPE = 1
  DO P_Act WITH FIRM, DOC_NUM, DOC_DATE
CASE DOC_TYPE = 2
  DO P_L_Spec WITH DOC_NUM, DOC_DATE
ENDCASE

RETURN
