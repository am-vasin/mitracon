*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º ” ©« Get_Corp      §à ¡®âç¨ª €­¤à¥© ‚ á¨­           04.04.2004        º
*ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
*º                                                                        º
*º                             ‚ë¡®à ª®à¯ãá .                             º
*º                                                                        º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
PROCEDURE Get_Corp
PARAMETERS prmCode, prmName

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

SELECT 0
USE (path_comm+"CORPUS") ORDER TAG CODE ALIAS TMP_4404 AGAIN
IF prmCode > 0 .AND. SEEK(prmCode)
  SET ORDER TO TAG NAME
ELSE
  SET ORDER TO TAG NAME
  GO TOP
ENDIF

*
*   Ž¯à¥¤¥«ï¥¬  á¨­åà®­­®¥ ¬¥­î
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "‚ë¡à âì" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Seek OF (menu_name) PROMPT "®¨áª"
ON PAD Seek OF (menu_name) ACTIVATE POPUP Seek

  DEFINE POPUP Seek MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Seek PROMPT "® ª®­â¥ªáâã" KEY Ctrl-A, "F7"
  ON SELECTION BAR 1 OF Seek DO Sw_Mode WITH "Seek_1"
  
  DEFINE BAR  2 OF Seek PROMPT "®¢â®à" KEY Ctrl-A, "Ctrl-F7"
  ON SELECTION BAR 2 OF Seek DO Sw_Mode WITH "Again"
  
DEFINE PAD Exit OF (menu_name) PROMPT "Žâª § âìáï" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*    áç¥â à §¬¥à®¢ ®ª­ 
*
PRIVATE ln, wd, tmpShab, r_sav, mss
ln = WROWS("")-11   && Š®«¨ç¥áâ¢® ¢¨¤¨¬ëå áâà®ª BROWSE
wd = FSIZE("NAME")

DO D_Wins WITH ln, wd, "Š®à¯ãá ", 1, 0
@ 1, 2 SAY "®¨áª "
@ 1, 8 SAY "Tab" COLOR (SCHEME(14,2))
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && à®á¬®âà á¯¨áª 

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter   KEYBOARD CHR(23)
    ON KEY LABEL Tab     DO Locator WITH 1, 8, 10
    ON KEY LABEL F7      DO Sw_Mode WITH "Seek_1"
    ON KEY LABEL Ctrl+F7 DO Sw_Mode WITH "Again"
*
*    BROWSE - ¬¥­î
*
    BROWSE FIELDS NAME:H="",       ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      IF READKEY() % 256 = 12
        what_do = "Exit"
      ENDIF
      IF READKEY() % 256 = 14
        what_do = "Choose"
      ENDIF
    ENDIF

  CASE what_do = "Seek_1"    && Š®­â¥ªáâ­ë© ¯®¨áª

    m.tmpShab = Get_Shab()
    IF .NOT. EMPTY(m.tmpShab)
      m.r_sav = RECNO()
      LOCATE FOR m.tmpShab $ NAME
      IF .NOT. FOUND()
        DIMENSION mss[3]
        mss[1] = ""
        mss[2] = CHR(0)+"¨ç¥£® ­¥ ­ ©¤¥­®..."
        mss[3] = ""
        DO Out_Mess WITH 7, "mss"
        IF BETWEEN(m.r_sav, 1, RECCOUNT())
          GO m.r_sav
        ENDIF
      ENDIF
    ENDIF
    what_do = "List"

  CASE what_do = "Again"    && ®¢â®à ¯®¨áª 

    
    IF .NOT. EMPTY(m.tmpShab)
      m.r_sav = RECNO()
      IF .NOT. EOF()
        SKIP
      ENDIF
      LOCATE REST FOR m.tmpShab $ NAME
      IF .NOT. FOUND()
        DIMENSION mss[3]
        mss[1] = ""
        mss[2] = CHR(0)+"¨ç¥£® ­¥ ­ ©¤¥­®..."
        mss[3] = ""
        DO Out_Mess WITH 7, "mss"
        IF BETWEEN(m.r_sav, 1, RECCOUNT())
          GO m.r_sav
        ENDIF
      ENDIF
    ENDIF
    what_do = "List"

  CASE what_do = "Choose"    && ‚ë¡®à

    prmCode = CODE
    IF TYPE("prmName") = "C"
      prmName = NAME
    ENDIF
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
SELECT TMP_4404
USE

SELECT (s_sav)

RETURN

*ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
*³   à®æ¥¤ãà  Locator       §à ¡®âç¨ª €­¤à¥© ‚ á¨­                      ³
*ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
*³                                                                        ³
*³                       ®¨áª ¯® ­ ç «ì­ë¬ ¡ãª¢ ¬                        ³
*³                                                                        ³
*ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ 04.05.2001 ÄÄÙ
PROCEDURE Locator
PARAMETERS l001, c001, ln

PRIVATE prf_flt, is_ins, c_flt, chr_type, c, c_c, c_t, v_sav, r_sav, c_sav

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

DO WHILE .T.
  ACTIVATE WINDOW (win_name+"_M") SAME
  @ l001, c001 SAY PADR(prf_flt, ln, "±")
  @ l001, c001+c_flt SAY ""
  v_sav   = prf_flt
  c_sav   = c_flt

  c_c = INKEY(0)                && Š®¤ á¨¬¢®« 
  IF c_c >= 0
    c = CHR(c_c)                && ‘¨¬¢®«
    c_t = SYS(15, chr_type, c)  && ’¨¯ á¨¬¢®« 
  ENDIF
  IF INLIST(c_c, 27, 9)
    EXIT
  ENDIF
  DO CASE
  CASE c_c < 0                  && ”ã­ªæ¨®­ «ì­ ï ª« ¢¨è 
  CASE c_t = "*"                && ¥ç â­ë© á¨¬¢®«
    prf_flt = LEFT(prf_flt, c_flt)+c+  ;
              SUBSTR(prf_flt, c_flt+IIF(is_ins,1,2))
    prf_flt = LEFT(prf_flt, ln)
    c_flt = c_flt+1
    c_flt = MIN(c_flt, ln)
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
    LOOP
  ENDCASE
  IF .NOT. v_sav == prf_flt
    r_sav   = RECNO()
    IF .NOT. SEEK(prf_flt)
      prf_flt = v_sav
       c_flt   = c_sav
       IF BETWEEN(r_sav, 1, RECCOUNT())
          GO r_sav
       ENDIF
    ENDIF
    SHOW WINDOW (ALIAS()) SAME REFRESH
  ENDIF

ENDDO

@ 1, 8 SAY PADR("Tab", ln) COLOR (SCHEME(14,2))

RETURN
