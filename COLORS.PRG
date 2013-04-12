*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º                                                                       º
*º               à®£à ¬¬  ä®à¬¨à®¢ ­¨ï æ¢¥â®¢®© áå¥¬ë.                  º
*º                                                                       º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
PROCEDUR Colors

*
*   …á«¨ ­¥â ä ©«  - á®§¤ ¥¬!
*
IF .NOT. FILE (prmo_path+"COLORS_N.DBF")
  CREATE DBF (prmo_path+"COLORS_N")  ;
         ( C_NAME C(20),   ;
           CL     C( 2),   ;
           BRGHT  L     )
  PRIVATE a_col
  DIMENSION a_col(3)

  a_col(1) = "”®­ "
  a_col(2) = "W"
  a_col(3) = .F.
  APPEND FROM ARRAY a_col

  a_col(1) = "’¥ªáâ"
  a_col(2) = "N"
  a_col(3) = .F.
  APPEND FROM ARRAY a_col

  a_col(1) = "‚ë¤¥«¥­­ë© â¥ªáâ"
  a_col(2) = "GR"
  a_col(3) = .T.
  APPEND FROM ARRAY a_col

  a_col(1) = "”®­ ¯®¤áª §ª¨"
  a_col(2) = "B"
  a_col(3) = .F.
  APPEND FROM ARRAY a_col

  a_col(1) = "”®­ ®è¨¡ª¨"
  a_col(2) = "R"
  a_col(3) = .F.
  APPEND FROM ARRAY a_col
  
  a_col(1) = "’¥­ì-á¨¬¢®«"
  a_col(2) = "N"
  a_col(3) = .T.
  APPEND FROM ARRAY a_col

  a_col(1) = "’¥­ì-ä®­"
  a_col(2) = "N"
  a_col(3) = .F.
  APPEND FROM ARRAY a_col

  RELEASE a_col

ELSE
  USE (prmo_path+"COLORS_N")
ENDIF
GO TOP

PRIVATE point1, point2, p1, p2, c_shab, sh_ln, msg, i

sh_ln = RECCOUNT()

DIMENSION point1(sh_ln), point2(sh_ln), c_shab(8), msg(4)

c_shab(1) = "N"
c_shab(2) = "B"
c_shab(3) = "GR"
c_shab(4) = "BG"
c_shab(5) = "G"
c_shab(6) = "RB"
c_shab(7) = "R"
c_shab(8) = "W"

msg(1) = '‚ë¡®à í«¥¬¥­â  æ¢¥â®¢®© áå¥¬ë - , , PgUp, PgDn'
msg(2) = '¨«¨ ¬ëèª .                                      '
msg(3) = '‚ë¡®à æ¢¥â  - '+CHR(27)+', '+CHR(26)+' ¨«¨, ®¯ïâì ¦¥, ¬ëèª .        '
msg(4) = '‚ëå®¤ - Esc.                                    '

SCAN
  
  point1(RECNO("COLORS_N")) = 1
  point2(RECNO("COLORS_N")) = IIF(BRGHT, 2, 1)
  FOR i = 1 TO 8
    IF UPPER(ALLTRIM(CL)) = c_shab(i)
      point1(RECNO("COLORS_N")) = i
      EXIT
    ENDIF
  ENDFOR

ENDSCAN

PRIVATE ln, wd, l, c

ln = 10+RECCOUNT()+ALEN(msg)
wd = 54
l  = CEILING((WROWS("")-ln)/2)
c  = FLOOR((WCOLS("")-wd)/2)

DEFINE WINDOW W_Hide FROM 0, 0 TO 0, WCOLS("")-1 NONE COLOR SCHEME 13
SHOW WINDOW W_Hide

DEFINE WINDOW W_Main FROM l, c TO l+ln-1, c+wd-1 NONE SHADOW COLOR SCHEME 13
ACTIVATE WINDOW W_Main

@ 1, 1 TO WROWS()-1, WCOLS()-2
@ 0, 0 SAY PADC("–¢¥â®¢ ï áå¥¬ ",WCOLS()) COLOR SCHEME 5

ln = RECCOUNT()+1
wd = FSIZE("C_NAME")+4
l  = l+3
c  = FLOOR((WCOLS("")-wd)/2)
DEFINE WINDOW W_Ext FROM l, c TO l+ln-1, c+wd-5 NONE COLOR SCHEME 13
DEFINE WINDOW W_Int FROM l-3, c-2 TO l+ln, c+wd+20 NONE COLOR SCHEME 13

DO Use_Dummy
SELECT COLORS_N
PRIVATE c_r, l1, cw, j

c_r = WLCOL("W_Ext")-WLCOL("W_Main")+FSIZE("C_NAME")+1
l1  = 2+RECCOUNT()

FOR i = 1 TO RECCOUNT()
  j = point1(i)
  cw = "N/"+c_shab(j)+IIF(point2(i)=2, "*", "")
  @ 2+i, c_r SAY "   " COLOR (cw)
ENDFOR

FOR i = 1 TO 8
  cw = "N/"+c_shab(i)
  @ l1+2, i*6-2 SAY "    " COLOR (cw)
ENDFOR

FOR i = 1 TO 8
  cw = "N/"+c_shab(i)+"*"
  @ l1+4, i*6-2 SAY "    " COLOR (cw)
ENDFOR

FOR i = 1 TO ALEN(msg)
  @ l1+5+i, 2 SAY PADC(msg(i), WCOLS()-4)
ENDFOR

GO TOP

@ l1+point2(1)*2, point1(1)*6-3 SAY ""
@ l1+point2(1)*2, point1(1)*6+2 SAY ""
ACTIVATE WINDOW W_Ext

ON KEY LABEL LeftMouse DO M_Proc
BROWSE FIELDS C_NAME, DUMMY.F:V=Mov_P():F   ;
       WHEN Set_P(1) VALID :F Set_P(0)  ;
       FREEZE DUMMY.F NOMENU NOAPPEND NODELETE NOMODIFY   ;
       WINDOW W_Int IN WINDOW W_Ext
ON KEY LABEL LeftMouse

DIMENSION msg(3)
msg(1) = ""
msg(2) = CHR(0)+"  ‘®åà ­¨¬ ¨§¬¥­¥­¨ï? [ „  ] [ ¥â ]"
msg(3) = ""
IF Out_Mess(7, "msg") = 1
  GO TOP
  SCAN
    i = RECNO("COLORS_N")
    cw = point1(i)
    cw = c_shab(cw)
    wd = point2(i) = 2
    REPLACE CL   WITH cw, BRGHT WITH wd
  ENDSCAN
ENDIF

CLOSE DATABASES
RELEASE WINDOWS

RETURN

**************************************************************************
PROCEDURE Set_P
PARAMETERS typ
PRIVATE p1, p2, s, mv

ACTIVATE WINDOW W_Main

p1 = point1(RECNO("COLORS_N"))
p2 = point2(RECNO("COLORS_N"))

@ l1+p2*2, p1*6-3 SAY IIF(typ=1, "", " ")
@ l1+p2*2, p1*6+2 SAY IIF(typ=1, "", " ")

ACTIVATE WINDOW Colors

RETURN .T.

**************************************************************************
PROCEDURE Mov_P
PRIVATE p1, p2, s, mv

ACTIVATE WINDOW W_Main

s = MOD(READKEY(),256)

p1 = point1(RECNO("COLORS_N"))
p2 = point2(RECNO("COLORS_N"))
IF s = 0 .OR. s = 1
  @ l1+p2*2, p1*6-3 SAY " "
  @ l1+p2*2, p1*6+2 SAY " "
ENDIF

mv = .F.
IF s = 0
  p1 = p1-1
  IF p1 = 0
    p1 = 8
    p2 = IIF(p2=1, 2, 1)
  ENDIF
  mv = .T.
ENDIF
IF s = 1
  p1 = p1+1
  IF p1 = 9
    p1 = 1
    p2 = IIF(p2=1, 2, 1)
  ENDIF
  mv = .T.
ENDIF
point1(RECNO("COLORS_N")) = p1
point2(RECNO("COLORS_N")) = p2
IF mv
  cw = "N/"+c_shab(p1)+IIF(p2=2, "*", "")
  @ 2+RECNO("COLORS_N"), c_r SAY "   " COLOR (cw)
  @ l1+p2*2, p1*6-3 SAY ""
  @ l1+p2*2, p1*6+2 SAY ""
ENDIF

ACTIVATE WINDOW Colors

RETURN .T.

**************************************************************************
PROCEDURE M_Proc
PRIVATE mc, mr, s, p1, p2, cw, jmp

IF MROW("W_Ext") >= 0 .AND. MCOL("W_Ext") >= 0
  RETURN
ENDIF
= INKEY("M")
jmp = .T.
mr = MROW("W_Main")
mc = MCOL("W_Main")

DO CASE
CASE mr = l1+2
  mr = 1
CASE mr = l1+4
  mr = 2
OTHERWISE
  jmp = .F.
ENDCASE

mc = mc-4
IF MOD(mc,6) > 3
  jmp = .F.
ENDIF

mc = FLOOR(mc/6)+1

IF mc > 8
  jmp = .F.
ENDIF

ACTIVATE WINDOW W_Main

IF jmp
  p1 = point1(RECNO("COLORS_N"))
  p2 = point2(RECNO("COLORS_N"))
  @ l1+p2*2, p1*6-3 SAY " "
  @ l1+p2*2, p1*6+2 SAY " "
  point1(RECNO("COLORS_N")) = mc
  point2(RECNO("COLORS_N")) = mr
  cw = "N/"+c_shab(p1)+IIF(p2=2, "*", "")
  @ 2+RECNO("COLORS_N"), c_r SAY "   " COLOR (cw)
ELSE
  mc = point1(RECNO("COLORS_N"))
  mr = point2(RECNO("COLORS_N"))
ENDIF
@ l1+mr*2, mc*6-3 SAY ""
@ l1+mr*2, mc*6+2 SAY ""
cw = "N/"+c_shab(mc)+IIF(mr=2, "*", "")
@ 2+RECNO("COLORS_N"), c_r SAY "   " COLOR (cw)

ACTIVATE WINDOW Colors
RETURN
