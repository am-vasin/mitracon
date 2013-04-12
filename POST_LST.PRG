*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º   ˆ¬ï ä ©«  Post_Lst      §à ¡®âç¨ª €­¤à¥© ‚ á¨­                      º
*ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
*º                                                                        º
*º     ‘¯¨á®ª ¯®çâ®¢ëå ãá«ã£ ¤«ï ¨§êïâ¨ï ¨§ áâ®¨¬®áâ¨ ¢ ¯®çâ. ª àâ®çª¥    º
*º                                                                        º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ 22.02.2007 ÍÍ¼
PROCEDURE Post_Lst

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
m.stat_type  = 3
m.menu_name  = PROGRAM()
m.last_mouse = 0
m.win_name   = PROGRAM()

*
*   Žâªàë¢ ¥¬ ä ©«ë .„., ãáâ ­ ¢«¨¢ ¥¬ á¢ï§¨ ¨ â. ¤.
*
m.s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT 0
USE (m.base_path+"STOCK") SHARED AGAIN ALIAS STO_70222 ORDER TAG CODE
SELECT 0
USE (m.base_path+"POS_LIST") SHARED AGAIN ALIAS PL_70222 ORDER TAG CODE
SET RELATION TO CODE INTO STO_70222
REPLACE ALL FOR CODE # 0 ;
	PREFIX		WITH STO_70222.PREFIX, ;
	NAME		WITH STO_70222.NAME,   ;
	PRODUCER	WITH STO_70222.PRODUCER
SET RELATION TO
SELECT STO_70222
SET RELATION TO CODE INTO PL_70222
SET ORDER TO TAG NAME_P
GO TOP

*
*   Ž¯à¥¤¥«ï¥¬  á¨­åà®­­®¥ ¬¥­î
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Modify OF (menu_name) PROMPT "Žâ¬¥â¨âì" KEY Ctrl-A, "Enter"
ON SELECTION PAD Modify OF (menu_name) DO Sw_Mode WITH "Modify"

DEFINE PAD Switch OF (menu_name) PROMPT "®¬./‘¯¨á®ª" KEY Ctrl-A, "Ctrl-Enter"
ON SELECTION PAD Switch OF (menu_name) DO Sw_Mode WITH "Switch"

DEFINE PAD Exit OF (menu_name) PROMPT "‚ëå®¤" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*    áç¥â à §¬¥à®¢ ®ª­ 
*
PRIVATE ln, wd, rcSav, rcNew
m.ln = WROWS("")-11   && Š®«¨ç¥áâ¢® ¢¨¤¨¬ëå áâà®ª BROWSE
m.wd = FSIZE("NAME")+FSIZE("PREFIX")+FSIZE("PRODUCER")+4

DO D_Wins WITH m.ln, m.wd, "®çâ®¢ë¥ ãá«ã£¨", 1, 0
@ 1, 2 SAY "®¨áª Tab"
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && à®á¬®âà á¯¨áª 

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ctrl-Enter DO Sw_Mode WITH "Switch"
    ON KEY LABEL Tab   DO Locator WITH 1, 2, 30

*
*    BROWSE - ¬¥­î
*
    IF ALIAS() = "STO_70222"
      BROWSE FIELDS PREFIX:H="",   ;
                    NAME:H="",     ;
                    PRODUCER:H="", ;
                    TMPMRK = IIF(FOUND("PL_70222"), "û", " "):1:H="", ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             NOMODIFY   ;
             NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ELSE
      BROWSE FIELDS STO_70222.PREFIX:H="",   ;
                    STO_70222.NAME:H="",     ;
                    STO_70222.PRODUCER:H="", ;
                    TMPMRK = "û":1:H="", ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             NOMODIFY   ;
             NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ENDIF
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Modify")
    ENDIF

  CASE m.what_do = "Modify"    && ‚áâ ¢¨âì/¨áª«îç¨âì

    IF .NOT. BOF() .AND. .NOT. EOF()
      IF ALIAS() = "STO_70222"
        IF FOUND("PL_70222")
          REPLACE PL_70222.CODE		WITH 0,		;
                  PL_70222.PREFIX	WITH "",	;
                  PL_70222.NAME		WITH "",	;
                  PL_70222.PRODUCER	WITH ""
        ELSE
          SELECT PL_70222
          GO TOP
          IF CODE # 0 .OR. BOF()
            APPEND BLANK
          ENDIF
          REPLACE	CODE		WITH STO_70222.CODE,	;
     		   		PREFIX		WITH STO_70222.PREFIX,	;
        			NAME		WITH STO_70222.NAME,	;
        			PRODUCER	WITH STO_70222.PRODUCER
          SELECT STO_70222
        ENDIF
      ELSE
        m.rcSav = RECNO()
        SKIP
        IF EOF()
          m.rcNew = 0
        ELSE
          m.rcNew = RECNO()
        ENDIF
        GO m.rcSav
        REPLACE PL_70222.CODE		WITH 0,		;
                PL_70222.PREFIX		WITH "",	;
                PL_70222.NAME		WITH "",	;
                PL_70222.PRODUCER	WITH ""
        IF m.rcNew = 0
          GO BOTTOM
        ELSE
          GO m.rcNew
        ENDIF
      ENDIF
    ENDIF
    m.what_do = "List"

  CASE m.what_do = "Switch"    && ‘¯./­®¬.

    IF ALIAS() = "STO_70222"
      SET RELATION TO
      SET ORDER TO TAG CODE
      SELECT PL_70222
      SET ORDER TO TAG NAME
      SET RELATION TO CODE INTO STO_70222
      IF .NOT. FOUND("PL_70222")
        GO TOP
      ENDIF
    ELSE
      SET RELATION TO
      SET ORDER TO TAG CODE
      SELECT STO_70222
      SET ORDER TO TAG NAME_P
      SET RELATION TO CODE INTO PL_70222
    ENDIF
    m.what_do = "List"

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
USE IN STO_70222
USE IN PL_70222
SELECT (m.s_sav)

RETURN


*ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
*³   à®æ¥¤ãà  Locator       §à ¡®âç¨ª €­¤à¥© ‚ á¨­                      ³
*ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
*³                                                                        ³
*³                           ®á¨¬¢®«ì­ë© ¯®¨áª                           ³
*³                                                                        ³
*ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ 17.12.2005 ÄÄÙ
PROCEDURE Locator

PROCEDURE Locator
PARAMETERS l001, c001, ln

PRIVATE prf_flt, is_ins, c_flt, chr_type, c, c_c, c_t
PRIVATE prf_sav, c_sav, r_sav

is_ins = INSMODE()   && ¥¦¨¬ ¢áâ ¢ª /§ ¬¥­ 

*
*   "*" - ª®¤ë, á®®â¢¥âáâ¢ãîé¨¥ ¯¥ç â­ë¬ á¨¬¢®« ¬
*
chr_type = SPACE(31)+     REPLICATE("*",126-31 )+ ;
           SPACE(127-126)+REPLICATE("*",175-127)+ ;
           SPACE(223-175)+REPLICATE("*",241-223)+ ;
           SPACE(255-223)
*
*   "I" - ª®¤ë, á®®â¢¥âáâ¢ãîé¨¥ á¨¬¢®« ¬ ¢­ãâà¥­­¥© ­ ¢¨£ æ¨¨
*
chr_type = STUFF(chr_type,   1, 1, "I")  &&  Home
chr_type = STUFF(chr_type,   4, 1, "I")  &&  ‘âà¥«ª  ¢¯à ¢®
chr_type = STUFF(chr_type,   6, 1, "I")  &&  End
chr_type = STUFF(chr_type,   7, 1, "I")  &&  Del
chr_type = STUFF(chr_type,  13, 1, "I")  &&  Enter
chr_type = STUFF(chr_type,  19, 1, "I")  &&  ‘âà¥«ª  ¢«¥¢®
chr_type = STUFF(chr_type,  22, 1, "I")  &&  Ins
chr_type = STUFF(chr_type, 127, 1, "I")  &&  BackSpace

*
*   "E" - ª®¤ë, á®®â¢¥âáâ¢ãîé¨¥ á¨¬¢®« ¬ ¢­¥è­¥© ­ ¢¨£ æ¨¨ ( ¯® ¡ §¥ )
*

chr_type = STUFF(chr_type,  3, 1, "E")  &&  PgDn
chr_type = STUFF(chr_type,  5, 1, "E")  &&  ‘âà¥«ª  ¢¢¥àå
chr_type = STUFF(chr_type, 18, 1, "E")  &&  PgUp
chr_type = STUFF(chr_type, 24, 1, "E")  &&  ‘âà¥«ª  ¢­¨§

prf_flt = ""    && à¥ä¨ªá
c_flt   = 0     && ’¥ªãé ï ¯®§¨æ¨ï ªãàá®à 
prf_sav = prf_flt
c_sav   = c_flt

ACTIVATE WINDOW (m.win_name+"_M") SAME
DO WHILE .T.
  IF .NOT. prf_sav == prf_flt
    r_sav = RECNO()
    IF .NOT. SEEK(prf_flt)
      prf_flt = prf_sav
      c_flt   = c_sav
      IF BETWEEN(r_sav, 1, RECCOUNT())
        GO r_sav
      ENDIF
    ELSE
      prf_sav = prf_flt
      c_sav   = c_flt
      SHOW WINDOW (ALIAS()) REFRESH
    ENDIF
  ENDIF
  c_sav   = c_flt
  @ l001, c001 SAY PADR(prf_flt, ln, "±")
  @ l001, c001+c_flt SAY ""

  c_c = INKEY(0)                && Š®¤ á¨¬¢®« 
  IF c_c >= 0
    c = CHR(c_c)                && ‘¨¬¢®«
    c_t = SYS(15, chr_type, c)  && ’¨¯ á¨¬¢®« 
  ENDIF
  DO CASE
  CASE c_c < 0                  && ”ã­ªæ¨®­ «ì­ ï ª« ¢¨è 
  CASE c_t = "*"                && ¥ç â­ë© á¨¬¢®«
    prf_flt = LEFT(prf_flt, c_flt)+c+  ;
              SUBSTR(prf_flt, c_flt+IIF(is_ins,1,2))
    prf_flt = LEFT(prf_flt, ln)
    c_flt = c_flt+1
    c_flt = MIN(c_flt, ln)
    LOOP
  CASE c_t = "E"                &&  ¢¨£ æ¨ï ¯® ¡ §¥
  CASE c_t = "I"                && ‚­ãâà¥­­ïï ­ ¢¨£ æ¨ï
    DO CASE
    CASE c_c = 7                && Del
      prf_flt = LEFT(prf_flt, c_flt)+SUBSTR(prf_flt, c_flt+2)
    CASE c_c = 127              && BackSpace
      prf_flt = LEFT(prf_flt, c_flt-1)+SUBSTR(prf_flt, c_flt+1)
      c_flt = MAX(c_flt-1,0)
    CASE c_c = 19               && ‘âà¥«ª  ¢«¥¢®
      c_flt = MAX(c_flt-1,0)
    CASE c_c = 4                && ‘âà¥«ª  ¢¯à ¢®
      c_flt = MIN(c_flt+1,LEN(prf_flt))
    CASE c_c = 22               && Ins
      is_ins = .NOT. is_ins
      = INSMODE(is_ins)
    CASE c_c = 13
      EXIT
    ENDCASE
  OTHERWISE
    EXIT
  ENDCASE
ENDDO

@ l001, c001 SAY PADR("®¨áª Tab", ln)

ACTIVATE WINDOW (ALIAS())

RETURN

