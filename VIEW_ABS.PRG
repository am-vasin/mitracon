*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º ” ©« View_Abs      §à ¡®âç¨ª €­¤à¥© ‚ á¨­           02.02.98 09:49:03 º
*ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
*º                                                                        º
*º                à®á¬®âà ¯®§¨æ¨© ­¥ ­ ©¤¥­ëå ­  áª« ¤¥.                 º
*º                                                                        º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
PROCEDURE View_Abs
*
*  ¥à¥¬¥­­ë¥ á®áâ®ï­¨ï ¤«ï ­ ¢¨£ æ¨¨
*

PRIVATE stat_type     && ’¨¯ ­ ¢¨£ æ¨¨: 0 - ­¥áâ ­¤ àâ­ë©;
                                        1 - ¡« ­ª;
                                        2 - BROWSE - â ¡«¨æ ;
                                        3 - BROWSE - á¯¨á®ª.
PRIVATE what_do       && ˆ¬ï à¥¦¨¬ .
PRIVATE menu_name     && ˆ¬ï  á¨­åà®­­®£® ¬¥­î.
PRIVATE last_mouse    && ‚à¥¬ï ¯®á«¥¤­¥£® ­ ¦ â¨ï §¢¥à¨­®© ª­®¯ª¨.
PRIVATE win_name      && ˆ¬ï ®ª­  ( ®ª®­ ¤«ï BROWSE ).

*
*   ‡ ¯®«­ï¥¬ §­ ç¥­¨ï¬¨ ¯¥à¥¬¥­­ë¥ á®áâ®ï­¨ï...
*
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   Žâªàë¢ ¥¬ ä ©«ë .„., ãáâ ­ ¢«¨¢ ¥¬ á¢ï§¨ ¨ â. ¤.
*

IF .NOT. File_O(base_path+"ABSENT.DBF")
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"¥¦¨¬ á¡®à  ®âª«îç¥­ - ­¥â ¤ ­­ëå!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

USE (base_path+"ABSENT.DBF") ORDER TAG CODE DESCENDING
IF EOF() .OR. BOF() .OR. CODE = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"” ©« ¤¥ä¥ªâ­ëå ¯®§¨æ¨© ¯ãáâ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  RETURN
ENDIF

DO Prep_Tmp

*
*   Ž¯à¥¤¥«ï¥¬  á¨­åà®­­®¥ ¬¥­î
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Delete OF (menu_name) PROMPT "“¤ «¨âì" KEY Ctrl-A, "Del"
ON SELECTION PAD Delete OF (menu_name) DO Sw_Mode WITH "Delete"

DEFINE PAD Del_All OF (menu_name) PROMPT "“¤ «¨âì ¢á¥" KEY Ctrl-A, "F7"
ON SELECTION PAD Del_All OF (menu_name) DO Sw_Mode WITH "Del_All"

DEFINE PAD Print OF (menu_name) PROMPT "¥ç âì" KEY Ctrl-A, "F1"
ON SELECTION PAD Print OF (menu_name) DO Sw_Mode WITH "Print"

DEFINE PAD Exit OF (menu_name) PROMPT "‚ëå®¤" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*    áç¥â à §¬¥à®¢ ®ª­ 
*
PRIVATE ln, wd, r_sav, pic_w
pic_w = Addr_Pic()
ln = MIN(100, RECCOUNT())   && Š®«¨ç¥áâ¢® ¢¨¤¨¬ëå áâà®ª BROWSE
wd = FSIZE("NAME", "ABS_TMP")+FSIZE("PRODUCER", "ABS_TMP")+ ;
     FSIZE("PREFIX", "ABS_TMP")+LEN(Addr_Pic())+ ;
     IIF(SET("CENTURY") = "ON", 14, 12)

DO D_Wins WITH ln, wd, "‘¯¨á®ª ¤¥ä¥ªâ­ëå ¯®§¨æ¨©", 0, 1
@ 1, 2 SAY PADC(" €¤à¥á ",LEN(Addr_Pic()),"Ä")+"Ä"
@ 1, COL() SAY PADR("ÄÄÄÄÄÄÄÄÄ  ¨¬¥­®¢ ­¨¥ ",  ;
     FSIZE("NAME", "ABS_TMP")+FSIZE("PRODUCER", "ABS_TMP")+FSIZE("PREFIX", "ABS_TMP")+3,"Ä")
@ 1, COL() SAY PADR(" „ â  ", IIF(SET("CENTURY") = "ON", 10, 8),"Ä")

@ WROWS()-3, 2 SAY "Ä‘ª« ¤ÄÄà®­ìÄÄÄ”¨«.ÄÄ„®¯.  ¤à¥áÄÄÄÄÄ"

what_do = "List"

GO TOP

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && à®á¬®âà á¯¨áª 

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Del DO Sw_Mode WITH "Delete"
    ON KEY LABEL F7  DO Sw_Mode WITH "Del_All"
    ON KEY LABEL F1  DO Sw_Mode WITH "Print"

*
*    BROWSE - ¬¥­î
*
    BROWSE FIELDS STOCK.ADDRESS:H="":P=pic_w, PREFIX:H="", NAME:H="", PRODUCER:H="",   ;
           ABSENT.D_DATE:H= "",  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Show_F()  ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = "Exit"
    ENDIF

  CASE what_do = "Delete"    && “¤ «¥­¨¥ ¯®§¨æ¨¨

    REPLACE ABSENT.CODE WITH 0
    SELECT ABS_TMP
    DELETE
    SKIP
    IF EOF() .OR. BOF()
      GO BOTTOM
    ENDIF
    what_do = "List"

  CASE what_do = "Del_All"    && “¤ «¥­¨¥ ¢á¥

    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+" “¤ «¨âì? ’®ç­®? [ ¥â ] [ „  ]"
    mss(3) = ""
    IF Out_Mess(7, "mss") = 1
      what_do = "List"
      LOOP
    ENDIF
    REPLACE ALL ABSENT.CODE WITH 0
    what_do = "Exit"

  CASE what_do = "Print"      && ¥ç âì

    r_sav = RECNO("ABS_TMP")
    DO P_Doc
    IF BETWEEN(r_sav, 1, RECCOUNT("ABS_TMP"))
      GO r_sav IN ABS_TMP
    ENDIF
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
CLOSE DATABASES
DELETE FILE(tmpo_path+"ABS_TMP.DBF")
DELETE FILE(tmpo_path+"ABS_TMP.CDX")

RETURN

*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º                                                                        º
*º                      ®¤£®â®¢ª  ¢à¥¬¥­­®£® ä ©« .                      º
*º                                                                        º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ 02.02.98 09:54:01 Í¼
PROCEDURE Prep_Tmp

DO Use_Dummy

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE

DELETE FILE(tmpo_path+"ABS_TMP.DBF")
DELETE FILE(tmpo_path+"ABS_TMP.CDX")
SELECT 0
CREATE DBF (tmpo_path+"ABS_TMP") ;
  (  CODE     N( 7), ;
     PREFIX   C( 4), ;
     NAME     C(35), ;
     PRODUCER C( 8) )
USE (tmpo_path+"ABS_TMP") EXCLUSIVE
INDEX ON NAME+PREFIX+PRODUCER TAG NAME

SELECT ABSENT
SCAN WHILE CODE > 0
  IF .NOT. SEEK(CODE,"STOCK")
    LOOP
  ENDIF
  SELECT ABS_TMP
  APPEND BLANK
  REPLACE CODE     WITH STOCK.CODE, ;
          PREFIX   WITH STOCK.PREFIX, ;
          NAME     WITH STOCK.NAME, ;
          PRODUCER WITH STOCK.PRODUCER
  SELECT ABSENT
ENDSCAN

SELECT ABS_TMP
SET RELATION TO CODE INTO ABSENT
SET RELATION TO CODE INTO STOCK ADDITIVE

RETURN

*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º                                                                        º
*º                            ¨¦­ïï áâà®ª ...                            º
*º                                                                        º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ 02.02.98 10:31:39 Í¼
PROCEDURE Show_F

ACTIVATE WINDOW (win_name+"_M") SAME

@ WROWS()-2, 2 SAY STR(STOCK.QNT,6) COLOR SCHEME 1
@ WROWS()-2, COL()+1 SAY STR(STOCK.QNT_SAVE+STOCK.QNT_REZ,6) COLOR SCHEME 1
@ WROWS()-2, COL()+1 SAY STR(STOCK.QNT_REAL,6) COLOR SCHEME 1
@ WROWS()-2, COL()+1 SAY STOCK.MEMO_ST COLOR SCHEME 1

ACTIVATE WINDOW (ALIAS())

RETURN .T.

*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º                                                                        º
*º                           ¥ç âì ¤®ªã¬¥­â .                            º
*º                                                                        º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ 02.02.98 15:37:49 Í¼
PROCEDURE P_Doc

PRIVATE p_ln, fnt, n_cp, lft, n, i, t_p, f_p, nn, ffeed, p_drctry

SELECT 0
USE DOC_FORM
LOCATE FOR "ABSENT" = ALLTRIM(UPPER(DOC_NAME))
p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed = DOC_FORM.F_FEED
p_drctry = DOC_FORM.P_DIR

n = MEMLINES(PAGE_H)
IF n > 0
  DIMENSION t_p(n)
  FOR i = 1 TO n
    t_p(i) = MLINE(PAGE_H,i)
  ENDFOR
ELSE
  DIMENSION t_p(1)
  t_p(1) = ""
ENDIF


n = MEMLINES(PAGE_F)
IF n > 0
  DIMENSION f_p(n)
  FOR i = 1 TO n
    f_p(i) = MLINE(PAGE_F,i)
  ENDFOR
ELSE
  DIMENSION f_p(1)
  f_p(1) = ""
ENDIF

USE

SELECT ABS_TMP
COUNT TO lines

PRIVATE n_p_det, sum_d, sum_p, cur_p, hol_p, j, lines, sub

n_p_det = p_ln-ALEN(t_p)-ALEN(f_p)-1
hol_p = CEILING(lines/n_p_det)

DO Ini_Prn WITH "„¥ä¥ªâ­ë¥ ¯®§¨æ¨¨", p_ln, lft, n_cp, fnt, ffeed, p_drctry

GO TOP
nn = 0

FOR cur_p = 1 TO hol_p
  sub = ALLTRIM(STR(cur_p))+"/"+ALLTRIM(STR(hol_p))
  FOR i = 1 TO ALEN(t_p)
    @ PROW()+1, 0 SAY STRTRAN(t_p(i), "{Pages}", sub)
  ENDFOR
  FOR i = 1 TO n_p_det
    nn = nn+1
    @ PROW()+1,0 SAY STR(nn,4)+" "+V_Addr(STOCK.ADDRESS)+IIF(EMPTY(STOCK.MEMO_ST),"  ","û ")+ ;
    PREFIX+" "+NAME+" "+PRODUCER+" "+STR(STOCK.QNT,6)+" "+DTOC(ABSENT.D_DATE)
    SKIP
    IF EOF()
      EXIT
    ENDIF
  ENDFOR
  FOR i = 1 TO ALEN(f_p)
    @ PROW()+1, 0 SAY f_p(i)
  ENDFOR
ENDFOR

DO Term_Prn WITH ""

RETURN
