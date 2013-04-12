*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º ” ©« Move_S        §à ¡®âç¨ª €­¤à¥© ‚ á¨­           16.06.97 14:58:12 º
*ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
*º                                                                        º
*º                          „¢¨¦¥­¨¥ ¯® áª« ¤ã.                           º
*º                                                                        º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
PROCEDURE Move_S
PARAMETERS cd_pos, rep_sale
PRIVATE sel_sav, is_dummy, doc_wd, t_col, c_w, tmpTime

t_col = LEFT(SCHEME(1,8), AT("/",SCHEME(1,8))-1)
IF RIGHT(t_col,1) = "+"
  t_col = LEFT(t_col,LEN(t_col)-1)+"*"
ENDIF
t_col = LEFT(SCHEME(1,1), AT("/",SCHEME(1,1))-1)+"/"+t_col
PUSH KEY CLEAR
sel_sav = SELECT()
IF USED("DUMMY")
  is_dummy = .T.
ELSE
  is_dummy = .F.
  DO Use_Dummy
ENDIF
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE AGAIN ALIAS STO_0
SEEK cd_pos
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
*   Ž¯à¥¤¥«ï¥¬  á¨­åà®­­®¥ ¬¥­î
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

IF rep_sale
  DEFINE PAD Prn_Docs OF (menu_name) PROMPT "¥ç âì"
  ON PAD Prn_Docs OF (menu_name) ACTIVATE POPUP Prn_Docs
 
  DEFINE POPUP Prn_Docs MARGIN RELATIVE SHADOW COLOR SCHEME 4

    DEFINE BAR 1 OF Prn_Docs PROMPT "¥ç âì ¤®ªã¬¥­â " KEY Ctrl-A, "Enter"
    ON SELECTION BAR 1 OF Prn_Docs DO Sw_Mode WITH "P_Docs"

    DEFINE BAR 2 OF Prn_Docs PROMPT "¥ç âì ¨áâ®à¨¨ ¯à®¤ ¦" KEY Ctrl-A, "Shift+F2"
    ON SELECTION BAR 2 OF Prn_Docs DO Sw_Mode WITH "V_Pos"
ELSE
  DEFINE PAD P_Doc OF (menu_name) PROMPT "¥ç âì ¤®ªã¬¥­â " KEY Ctrl-A, "Enter"
  ON SELECTION PAD P_Doc OF (menu_name) DO Sw_Mode WITH "P_Doc"
ENDIF

DEFINE PAD Seek OF (menu_name) PROMPT "®¨áª ª«¨¥­â " KEY Ctrl-A, "F7"
ON SELECTION PAD Seek OF (menu_name) DO Sw_Mode WITH "Seek"

DEFINE PAD Cource OF (menu_name) PROMPT "Šãàá" KEY Ctrl-A, "F4"
ON SELECTION PAD Cource OF (menu_name) DO Sw_Mode WITH "Cource"

DEFINE PAD Exit OF (menu_name) PROMPT "‚ëå®¤" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   Žâªàë¢ ¥¬ ä ©«ë .„., ãáâ ­ ¢«¨¢ ¥¬ á¢ï§¨ ¨ â. ¤.
*
IF .NOT. Prep_M()
  SELECT STO_0
  USE
  SELECT (sel_sav)
  POP KEY
  RETURN
ENDIF
*
*    áç¥â à §¬¥à®¢ ®ª­ 
*
PRIVATE ln, wd
ln = MIN(WROWS("")-14, RECCOUNT())   && Š®«¨ç¥áâ¢® ¢¨¤¨¬ëå áâà®ª BROWSE
wd = IIF(SET("CENTURY") = "ON", 77, 75)

DO D_Wins WITH ln, wd, ;
               ALLTRIM(STO_0.PREFIX)+ALLTRIM(STO_0.NAME)+" "+ALLTRIM(STO_0.PRODUCER), 1, 3

@ 2, 2 SAY PADC("„ â ",IIF(SET("CENTURY") = "ON", 10, 8),"Ä")+"Ä"+ ;
     "’¨¯ ¤®ª.ÄÄÄÄÄ–¥­ ÄŠ®«-¢®ÄÄ¥ «.ÄÄÄ”¨«.ÄŠ®«-¢®ÄÄ¥ «.ÄÄÄ”¨«.ÄÄ‘¯¥æ."
doc_wd = IIF(SET("CENTURY") = "ON", 19, 17)

@ 1, doc_wd+3 SAY PADC("„¢¨¦¥­¨¥",29) COLOR (t_col)
c_w = COL()+1
@ 1, c_w      SAY PADC("Žáâ âª¨",27) COLOR (t_col)
@ WROWS()-5,  3 SAY "  ¡. ¬¥áâ® "
@ WROWS()-5, 20 SAY " à®¤ ¢¥æ "
@ WROWS()-5, c_w SAY PADC("  §  ¤ ­­ëå ", 20, "Ä")
@ WROWS()-4, c_w SAY STR(STO_0.QNT+STO_0.QNT_SAVE+STO_0.QNT_REZ,6) COLOR SCHEME 1
@ WROWS()-4, COL()+1 SAY STR(STO_0.QNT_REP,6) COLOR SCHEME 1
@ WROWS()-4, COL()+1 SAY STR(STO_0.QNT_REAL,6) COLOR SCHEME 1
@ WROWS()-4, COL()+1 SAY STR(STO_0.QNT_SPEC,6) COLOR SCHEME 1

what_do = "List"
GO BOTTOM


DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && à®á¬®âà á¯¨áª 

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F4    DO Cource
    ON KEY LABEL F7    DO Sw_Mode WITH "Seek"
    ON KEY LABEL Shift+F2 Do Sw_Mode WITH "V_Pos"

*
*    BROWSE - ¬¥­î
*
    BROWSE FIELDS DOC_DATE:H="",  ;
                  TYPE_VIS:H="",  ;
                  PRICE:H="":P="@Z 99999.99", ;
                  QNT:H="":P="@Z 999999", ; 
                  QNT_REP:H="":P="@Z 999999", ; 
                  QNT_FIL:H="":P="@Z 999999", ; 
                  QNT_REST:H="":P="@Z 999999", ; 
                  REST_REP:H="":P="@Z 999999", ; 
                  REST_FIL:H="":P="@Z 999999", ; 
                  REST_SPEC:H="":P="@Z 999999", ; 
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN ShowWho()              ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "P_Docs")
    ENDIF

  CASE what_do = "Seek"      && ®¨áª ª«¨¥­â 

    DO Seek_Cus
    what_do = "List"

  CASE what_do = "P_Docs"    && ¥ç âì ¤®ªã¬¥­â 

    DO P_Docs
    what_do = "List"

  CASE what_do = "V_Pos"    && ¥ç âì ¨áâ®à¨¨ ¯à®¤ ¦

    DO V_Pos WITH cd_pos
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
USE
DELETE FILE (tmpo_path+"MOVE.DBF")
DELETE FILE (tmpo_path+"MOVE.CDX")
RELEASE MENU (menu_name) EXTENDED
SELECT STO_0
USE
IF .NOT. is_dummy
  SELECT DUMMY
  USE
ENDIF
SELECT (sel_sav)
POP KEY

RETURN

*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º                                                                        º
*º                      ®¤£®â®¢ª  ¢à¥¬¥­­®£® ä ©« .                      º
*º                                                                        º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ 16.06.97 15:01:08 Í¼
PROCEDURE Prep_M

PRIVATE qnt_s, dat_s, n_vis, w_who, w_where, w_cus
PRIVATE w_fil, c_sav, f_sav, p_sav, d_sav, dt_sav
PRIVATE q_s, q_s_r, q_s_f, q_s_s, inc_price, tmp_price

DELETE FILE (tmpo_path+"MOVE.DBF")
DELETE FILE (tmpo_path+"MOVE.CDX")
m.inc_price = .F.
IF TYPE("m.modi") = "L"
  m.inc_price = m.modi
ENDIF

SELECT 0
USE (base_path+"PARMS") ALIAS PARMS_0
dat_s = DAT_START
USE

CREATE DBF (tmpo_path+"MOVE.DBF")  ;
   ( FIRM      C( 3), ;
     DOC_DATE  D,     ;
     DOC_TIME  C( 5), ;
     DOC_NUM   C(10), ;
     DOC_VIS   C(15), ;
     DOC_TYPE  C( 1), ;
     TYPE_VIS  C( 8), ;
     PRICE     N( 8,2), ;
     QNT       N( 8), ;
     QNT_REP   N( 8), ;
     QNT_FIL   N( 8), ;
     QNT_SPEC  N( 8), ;
     IS_FIL    L,     ;
     QNT_REST  N( 8), ;
     L_NO      C( 6), ;
     REST_FIL  N( 8), ;
     REST_REP  N( 8), ;
     REST_SPEC N( 8), ;
     CUSTOMER  C(50), ;
     NOTE      C(72), ;
     WHO       C(29), ;
     WHERE     C(16)  )
INDEX ON DTOS(DOC_DATE)+DOC_TIME TAG HRON

SELECT 0
USE (base_path+"ARM_LST") ORDER TAG ARM_C ALIAS WHERE_0 AGAIN

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS WHO_0 AGAIN

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS CUST_0 AGAIN

*
*    à¨å®¤
*
SELECT 0
USE (base_path+"ACT_S_T") ORDER TAG DOC_NUM ALIAS ACT_T_0 AGAIN
SET RELATION TO SUP_CODE  INTO CUST_0, ;
                WHO_ACT   INTO WHO_0, ;
                WHERE_ACT INTO WHERE_0

SELECT 0
USE (base_path+"ACT_S") ORDER CODE ALIAS ACT_0 AGAIN
SET RELATION TO FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM INTO ACT_T_0

SEEK STO_0.CODE
SCAN REST WHILE CODE = STO_0.CODE FOR DOC_DATE >= dat_s
  n_vis = ALLTRIM(RIGHT(DOC_NUM,2))
  n_vis = ALLTRIM(LEFT(DOC_NUM,8))+IIF(EMPTY(n_vis), "", "/"+n_vis)+"/"+FIRM
  IF FOUND("WHO_0")
    w_who = ALLTRIM(WHO_0.FAMILY)+" "+  ;
            LEFT(WHO_0.NAME,1)+". "+    ;
            LEFT(WHO_0.S_NAME,1)+"."
  ELSE
    w_who = ""
  ENDIF
  IF FOUND("WHERE_0")
    w_where = WHERE_0.ARM_N
  ELSE
    w_where = ""
  ENDIF
  w_cus = CUST_0.CUS_NAME
  SELECT MOVE
  APPEND BLANK
  m.tmp_price = IIF(m.inc_price, ACT_0.PRICE_R, 0)
  REPLACE FIRM     WITH ACT_T_0.FIRM,    ;
          DOC_DATE WITH ACT_T_0.DOC_DATE,;
          DOC_TIME WITH str(ACT_T_0.DOC_TIME,5),;
          DOC_NUM  WITH ACT_T_0.DOC_NUM, ;
          DOC_VIS  WITH n_vis,           ;
          DOC_TYPE WITH "I",             ;
          TYPE_VIS WITH IIF(ACT_T_0.SUP_CODE < 0, "Š®àà.", "à.  ªâ"), ;
          PRICE    WITH m.tmp_price,     ;
          QNT      WITH ACT_0.QNT_INC,   ;
          QNT_REP  WITH ACT_0.QNT_REP,   ;
          QNT_SPEC WITH IIF(ACT_T_0.SUP_CODE < 0, -ACT_0.QNT_INC, 0), ;
          CUSTOMER WITH CUST_0.CUS_NAME, ;
          WHO      WITH w_who,           ;
          WHERE    WITH w_where
  IF FSIZE("NOTE", "ACT_T_0") # 0
    REPLACE NOTE WITH ACT_T_0.NOTE
  ENDIF
  SELECT ACT_0
ENDSCAN

*
*      ª« ¤­ë¥ ¯¥à¥¤ ç¨ ­  à¥ «¨§ æ¨î
*
USE (base_path+"RYN_M_T") ORDER TAG DOC_NUM ALIAS RYN_T_0 AGAIN
SET RELATION TO CUS_CODE INTO CUST_0, ;
                WHO      INTO WHO_0, ;
                WHERE    INTO WHERE_0

SELECT ACT_T_0
USE (base_path+"RYN_M") ORDER TAG CODE ALIAS RYN_0 AGAIN
SET RELATION TO LEFT(DTOS(DOC_DATE),4)+STR(DOC_NUM,5) INTO RYN_T_0

SEEK STO_0.CODE
SCAN REST WHILE CODE = STO_0.CODE FOR DOC_DATE >= dat_s
  n_vis = ALLTRIM(STR(DOC_NUM))
  IF FOUND("WHO_0")
    w_who = ALLTRIM(WHO_0.FAMILY)+" "+  ;
            LEFT(WHO_0.NAME,1)+". "+    ;
            LEFT(WHO_0.S_NAME,1)+"."
  ELSE
    w_who = ""
  ENDIF
  IF FOUND("WHERE_0")
    w_where = WHERE_0.ARM_N
  ELSE
    w_where = ""
  ENDIF
  w_cus = CUST_0.CUS_NAME
  SELECT MOVE
  APPEND BLANK
  REPLACE DOC_DATE WITH RYN_T_0.DOC_DATE,;
          DOC_TIME WITH STR(RYN_T_0.DOC_TIME,5),;
          DOC_NUM  WITH n_vis,           ;
          L_NO     WITH STR(RYN_T_0.LST_NUM,6),;
          DOC_VIS  WITH n_vis,           ;
          DOC_TYPE WITH "F",             ;
          TYPE_VIS WITH "  à¥ «.",      ;
          QNT      WITH -RYN_0.QNT,      ;
          QNT_FIL  WITH RYN_0.QNT,       ;
          IS_FIL   WITH .T.,             ;
          CUSTOMER WITH CUST_0.CUS_NAME, ;
          WHO      WITH w_who,           ;
          WHERE    WITH w_where
  IF FSIZE("NOTE", "RYN_T_0") # 0
    REPLACE NOTE WITH RYN_T_0.NOTE
  ENDIF
  SELECT RYN_0
ENDSCAN
*
*     €ªâë ãâ¢¥à¦¤¥­¨ï á¯¥æ¤¢¨¦¥­¨ï
*
SELECT RYN_T_0
USE (base_path+"LET_SP_T") ORDER TAG DOC_NUM ALIAS LET_T_0 AGAIN
SET RELATION TO CUS_CODE INTO CUST_0, ;
                WHO      INTO WHO_0, ;
                WHERE    INTO WHERE_0
SELECT RYN_0
USE (base_path+"LET_SPEC") ORDER TAG CODE ALIAS LET_0 AGAIN
SET RELATION TO LEFT(DTOS(DOC_DATE),4)+DOC_NUM INTO LET_T_0

SEEK STO_0.CODE
SCAN REST WHILE CODE = STO_0.CODE FOR DOC_DATE >= dat_s
  n_vis = ALLTRIM(DOC_NUM)
  IF FOUND("WHO_0")
    w_who = ALLTRIM(WHO_0.FAMILY)+" "+  ;
            LEFT(WHO_0.NAME,1)+". "+    ;
            LEFT(WHO_0.S_NAME,1)+"."
  ELSE
    w_who = ""
  ENDIF
  IF FOUND("WHERE_0")
    w_where = WHERE_0.ARM_N
  ELSE
    w_where = ""
  ENDIF
  w_cus = CUST_0.CUS_NAME
  tmpTime = VAL(ALLTRIM(LEFT(LET_T_0.DOC_TIME,2)))*3600
  tmpTime = tmpTime+VAL(ALLTRIM(SUBSTR(LET_T_0.DOC_TIME,4,2)))*60
  tmpTime = tmpTime+VAL(ALLTRIM(RIGHT(LET_T_0.DOC_TIME,2)))
  SELECT MOVE
  APPEND BLANK
  REPLACE DOC_DATE WITH LET_T_0.DOC_DATE,;
          DOC_TIME WITH STR(tmpTime,5),  ;
          DOC_NUM  WITH LET_T_0.DOC_NUM, ;
          DOC_VIS  WITH n_vis,           ;
          DOC_TYPE WITH "L",             ;
          TYPE_VIS WITH "“â¢.",          ;
          QNT_SPEC WITH -LET_0.QNT,      ;
          IS_FIL   WITH .F.,             ;
          CUSTOMER WITH CUST_0.CUS_NAME, ;
          WHO      WITH w_who,           ;
          WHERE    WITH w_where
  IF FSIZE("NOTE", "LET_T_0") # 0
    REPLACE NOTE WITH LET_T_0.NOTE
  ENDIF
  SELECT LET_0
ENDSCAN

*
*      ª« ¤­ë¥ ¯à®¤ ¦¨
*
USE

SELECT LET_T_0
USE (base_path+"SALE_TIT") ORDER TAG DOC_NUM ALIAS SALE_T_0 AGAIN
SET RELATION TO CUS_CODE INTO CUST_0, ;
                WHO      INTO WHO_0, ;
                WHERE    INTO WHERE_0

SELECT 0
USE (base_path+"SALE") ORDER TAG CODE ALIAS SALE_0 AGAIN
SET RELATION TO FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM INTO SALE_T_0

SEEK STO_0.CODE
c_sav = 0
f_sav = "   "
p_sav = 0
d_sav = "          "
dt_sav = {}
SCAN REST WHILE CODE = STO_0.CODE FOR DOC_DATE >= dat_s .AND. LEFT(DOC_NUM,1) = sys_char
  w_fil = .F.
  IF EMPTY(SALE_T_0.LIST_NUM)
    w_fil = .T.
  ENDIF
      
  n_vis = LEFT(DOC_NUM,4)+"-"+ALLTRIM(SUBSTR(DOC_NUM,5))+"/"+FIRM
  IF FOUND("WHO_0")
    w_who = ALLTRIM(WHO_0.FAMILY)+" "+  ;
            LEFT(WHO_0.NAME,1)+". "+    ;
            LEFT(WHO_0.S_NAME,1)+"."
  ELSE
    w_who = ""
  ENDIF
  IF FOUND("WHERE_0")
    w_where = WHERE_0.ARM_N
  ELSE
    w_where = ""
  ENDIF
  IF FOUND("CUST_0")
    w_cus = CUST_0.CUS_NAME
  ELSE
    w_cus = SALE_T_0.CUS_NAME
  ENDIF
  SELECT MOVE
  IF c_sav = SALE_0.CODE .AND. p_sav = SALE_0.PRICE_R .AND. ;
     d_sav = SALE_0.DOC_NUM .AND. dt_sav = SALE_0.DOC_DATE .AND. f_sav = SALE_0.FIRM
    REPLACE QNT      WITH QNT-IIF(w_fil, 0, SALE_0.QNT), ;
            QNT_FIL  WITH QNT_FIL-IIF(w_fil, SALE_0.QNT, 0), ;
            QNT_SPEC WITH QNT_SPEC+IIF(SALE_T_0.CUS_CODE < 0, SALE_0.QNT, 0), ;
            QNT_REP  WITH QNT_REP-SALE_0.QNT_REP
  ELSE
    APPEND BLANK
    REPLACE FIRM     WITH SALE_T_0.FIRM,   ;
            DOC_DATE WITH SALE_T_0.DOC_DATE,;
            DOC_TIME WITH STR(SALE_T_0.DOC_TIME,5),;
            L_NO     WITH SALE_T_0.LIST_NUM, ;
            DOC_NUM  WITH SALE_0.DOC_NUM,  ;
            DOC_VIS  WITH n_vis,           ;
            DOC_TYPE WITH "S",             ;
            TYPE_VIS WITH IIF(SALE_T_0.CUS_CODE < 0, "Š®àà.", "à®¤ ¦ "), ;
            IS_FIL   WITH w_fil,           ;
            QNT      WITH IIF(w_fil, 0, -SALE_0.QNT), ;
            QNT_FIL  WITH IIF(w_fil, -SALE_0.QNT, 0), ;
            QNT_REP  WITH -SALE_0.QNT_REP, ;
            QNT_SPEC WITH IIF(SALE_T_0.CUS_CODE < 0, SALE_0.QNT, 0), ;
            PRICE    WITH SALE_0.PRICE_R,  ;
            CUSTOMER WITH w_cus,           ;
            WHO      WITH w_who,           ;
            WHERE    WITH w_where
  ENDIF
  c_sav = SALE_0.CODE
  p_sav = SALE_0.PRICE_R
  d_sav = SALE_0.DOC_NUM
  dt_sav = SALE_0.DOC_DATE
  IF FSIZE("NOTE", "SALE_T_0") # 0
    REPLACE NOTE WITH SALE_T_0.NOTE
  ENDIF
  SELECT SALE_0
ENDSCAN

USE
SELECT SALE_T_0
USE
SELECT CUST_0
USE
SELECT WHO_0
USE
SELECT WHERE_0
USE

SELECT MOVE
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"® ¯®§¨æ¨¨ ¤¢¨¦¥­¨ï ­¥ ¡ë«®!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  DELETE FILE (tmpo_path+"MOVE.DBF")
  DELETE FILE (tmpo_path+"MOVE.CDX")
  RETURN .F.
ENDIF
q_s   = STO_0.QNT_START
q_s_r = STO_0.QNT_S_REP
q_s_f = STO_0.QNT_S_RYN
q_s_s = STO_0.QNT_S_SPEC

SCAN
  q_s   = q_s+QNT
  q_s_r = q_s_r+QNT_REP
  q_s_f = q_s_f+QNT_FIL
  q_s_s = q_s_s+QNT_SPEC
  
  REPLACE QNT_REST WITH q_s,   ;
          REST_FIL WITH q_s_f, ;
          REST_REP WITH q_s_r, ;
          REST_SPEC WITH q_s_s
ENDSCAN
GO TOP

RETURN .T.

*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º                                                                        º
*º                        ˆ­ä®à¬ æ¨®­­ ï áâà®ª .                          º
*º                                                                        º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ 16.06.97 12:35:10 Í¼
PROCEDURE ShowWho

ACTIVATE WINDOW (win_name+"_M") SAME

@ 1,  2 SAY PADC("„®ª-â "+ALLTRIM(DOC_VIS), doc_wd) COLOR (t_col)

@ WROWS()-4,  3 SAY WHERE  COLOR SCHEME 1
@ WROWS()-4, COL()+1 SAY WHO COLOR SCHEME 1
@ WROWS()-3,  2 SAY "Š«¨¥­â"
@ WROWS()-3, COL()+1 SAY CUSTOMER && COLOR SCHEME 1
@ WROWS()-3, COL()+1 SAY "‘¯."
@ WROWS()-3, COL()+1 SAY L_NO && COLOR SCHEME 1
@ WROWS()-2, 3 SAY NOTE

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º                                                                        º
*º                      à®£à ¬¬  ¯¥ç â¨ ¤®ªã¬¥­â®¢.                      º
*º                                                                        º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ 17.06.97 14:43:58 Í¼
PROCEDURE P_Docs

DO CASE
CASE DOC_TYPE = "S"      &&  ª« ¤­ ï ¯à®¤ ¦¨
  DO Prn_Inv WITH FIRM, DOC_NUM, DOC_DATE
CASE DOC_TYPE = "I"      && à¨¥¬­ë©  ªâ
  DO P_Act WITH FIRM, DOC_NUM, DOC_DATE
CASE DOC_TYPE = "F"      &&  ª« ¤­ ï ¯¥à¥¤ ç¨ ¢ ä¨«¨ «
  DO P_Inv_F WITH VAL(ALLTRIM(DOC_NUM)), DOC_DATE
CASE DOC_TYPE = "L"      && €ªâ ãâ¢¥à¦¤¥­¨ï á¯¥æ¤¢¨¦¥­¨ï
  DO P_L_Spec WITH DOC_NUM, DOC_DATE
ENDCASE

RETURN

*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º                                                                       º
*º                à®£à ¬¬  ¢ë¢®¤  ªãàá  ¤«ï ­ ª« ¤­®©.                  º
*º                                                                       º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
PROCEDURE Cource
PRIVATE mss

IF DOC_TYPE # "S"
  ACTIVATE SCREEN
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"â® ­¥ ­ ª« ¤­ ï!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  ACTIVATE WINDOW (ALIAS()) SAME
  RETURN
ENDIF

DO Crs_Inv WITH FIRM, DOC_NUM, DOC_DATE
SHOW MENU (menu_name)

ACTIVATE WINDOW (ALIAS()) SAME

RETURN

*ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
*³   à®æ¥¤ãà  Seek_Cus      §à ¡®âç¨ª €­¤à¥© ‚ á¨­                      ³
*ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
*³                                                                        ³
*³                             ®¨áª ª«¨¥­â                               ³
*³                                                                        ³
*ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ 07/17/2000 ÄÄÙ
PROCEDURE Seek_Cus

PRIVATE shab, r_sav

shab = Get_Shab()
IF EMPTY(shab)
  RETURN
ENDIF

shab = SYS(15, lwr, shab)
r_sav = RECNO()
SELECT 0
DELETE FILE (tmpo_path+"SEEK_CUS.DBF")
CREATE DBF (tmpo_path+"SEEK_CUS")  ;
  (  LINK     N(10), ;
     DOC_DATE D,     ;
     CUSTOMER C(50), ;
     QNT      N( 6), ;
     QNT_FIL  N( 6)  )
SELECT MOVE
DO Wt_Mess WITH "ˆé¥¬!"
SCAN
  IF shab $ SYS(15, lwr, CUSTOMER)
    SELECT SEEK_CUS
    APPEND BLANK
    REPLACE LINK     WITH RECNO("MOVE"), ;
            DOC_DATE WITH MOVE.DOC_DATE, ;
            CUSTOMER WITH MOVE.CUSTOMER, ;
            QNT      WITH MOVE.QNT,      ;
            QNT_FIL  WITH MOVE.QNT_FIL
    SELECT MOVE
  ENDIF
ENDSCAN
DO Wt_Mess
IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF
SELECT SEEK_CUS
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"¨ç¥£® ­¥ ­ ©¤¥­®!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT MOVE
  DELETE FILE (tmpo_path+"SEEK_CUS.DBF")
  RETURN
ENDIF

DO View_L
SELECT SEEK_CUS
USE
DELETE FILE (tmpo_path+"SEEK_CUS.DBF")
SELECT MOVE

RETURN

*ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
*³   à®æ¥¤ãà  View_L        §à ¡®âç¨ª €­¤à¥© ‚ á¨­                      ³
*ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
*³                                                                        ³
*³                       à®á¬®âà ­ ©¤¥­ëå § ¯¨á¥©.                       ³
*³                                                                        ³
*ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ 17.07.2000 ÄÄÙ
PROCEDURE View_L


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
PRIVATE s_sav         && ®¬¥à à ¡®ç¥© ®¡« áâ¨ ¤«ï á®åà ­¥­¨ï ¨ ¢®ááâ ­®¢«¥­¨ï!

*
*   ‡ ¯®«­ï¥¬ §­ ç¥­¨ï¬¨ ¯¥à¥¬¥­­ë¥ á®áâ®ï­¨ï...
*
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   Žâªàë¢ ¥¬ ä ©«ë .„., ãáâ ­ ¢«¨¢ ¥¬ á¢ï§¨ ¨ â. ¤.
*

s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT SEEK_CUS

*
*   Ž¯à¥¤¥«ï¥¬  á¨­åà®­­®¥ ¬¥­î
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "‚ë¡à âì" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "Žâª § âìáï" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*    áç¥â à §¬¥à®¢ ®ª­ 
*
*ÚÄÄÄ„ â ÄÄÄÄÄŠ«¨¥­âÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄà®¤.ÄÄ¥ «.¿
*³„„.ŒŒ.ƒƒƒƒ ....:....!....:....!....:....!....:....!....:....! 999999 999999³
*ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PRIVATE ln, wd
ln = WROWS("")-11   && Š®«¨ç¥áâ¢® ¢¨¤¨¬ëå áâà®ª BROWSE
wd = 75

DO D_Wins WITH ln, wd, "¥§ã«ìâ â ¯®¨áª ", 0, 0
@ 1, 2 SAY "ÄÄÄ„ â ÄÄÄÄÄŠ«¨¥­âÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄà®¤.ÄÄ¥ «."
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && à®á¬®âà á¯¨áª 

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - ¬¥­î
*
    BROWSE FIELDS DOC_DATE:H="", CUSTOMER:H="", QNT:H="", QNT_FIL:H="",  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && ‚ë¡®à

    GO SEEK_CUS.LINK IN MOVE
    EXIT

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   €ªªãà â­¥­ìª® ( ªªãà â­¥­ìª®!) § ªàë¢ ¥¬ DBF-ä ©«ë.
*
SELECT (s_sav)

RETURN
