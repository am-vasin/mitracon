*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ      เฎฃเ ฌฌ  ฏเฎแฌฎโเ , ฏฅ็ โจ จ ใโขฅเฆคฅญจ๏ ง ๏ขฎช ญ  แชซ ค.       บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE View_Ord
PARAMETERS let    && .T. - แ ไใญชๆจฅฉ ใโขฅเฆคฅญจ๏
PRIVATE arm_nams, arm_chrs, i, a_c, utv
*
*    PAD - ฌฅญ๎ ฏฅเขฎฃฎ ใเฎขญ๏
*
a_c = "   "
utv = .F.
IF let
  USE ARM_LST
  i = 1
  DIMENSION arm_nams(i), arm_chrs(i)
  arm_nams(i) = PADR("แฅ",FSIZE("ARM_N"))
  arm_chrs(i) = "   "
  SCAN
    i = i+1
    DIMENSION arm_nams(i), arm_chrs(i)
    arm_nams(i) = ARM_N
    arm_chrs(i) = ARM_C
  ENDSCAN
  USE
ENDIF
    
DEFINE MENU View_Ord IN SCREEN COLOR SCHEME 4
DEFINE PAD Print OF View_Ord PROMPT "ฅ็ โ์" KEY Ctrl-A, "Enter"
IF let
  DEFINE PAD Let OF View_Ord PROMPT "“โขฅเคจโ์" KEY Ctrl-A, "Ins"
  DEFINE PAD Filter OF View_Ord PROMPT "”จซ์โเ" KEY Ctrl-A, "F6"
ENDIF
DEFINE PAD Exit   OF View_Ord PROMPT "๋ๅฎค" KEY Ctrl-A, "Esc"

ON SELECTION PAD Print OF View_Ord DO Ch_Mode WITH "View_Ord","print"
IF let
  ON SELECTION PAD Let OF View_Ord DO Set_Let
  ON SELECTION PAD Filter OF View_Ord DO Ch_Mode WITH "View_Ord","filter"
ENDIF
ON SELECTION PAD Exit  OF View_Ord DO Ex_Proc WITH "View_Ord"

HIDE MENU Main_M
DO Entry_P WITH "", "View_Ord"

DO Use_Dummy

SELECT 0
USE ARM_LST ORDER TAG ARM_C

SELECT 0
USE ORD_T ORDER TAG DOC_NUM
SET RELATION TO WHERE INTO ARM_LST
IF .NOT. let
  SET FILTER TO WHERE = arm
ENDIF
GO BOTTOM

DO Prep_W

PRIVATE what_do, clr, i1, i2, s1

clr = SCHEME(13)
i1  = AT("/",clr)
s1  = LEFT(clr,i1-1)
IF RIGHT(s1,1) = "+"
  s1 = LEFT(s1,LEN(s1)-1)+"*"
ENDIF
i1 = AT("/",clr,3)+1
i2 = AT(",",clr,3)-i1
clr = STUFF(clr,i1,i2,s1)

what_do = "list"

DO WHILE .T.

  DO CASE
  CASE what_do = "list"     && ‘ฏจแฎช ง ๏ขฎช
    SHOW MENU View_Ord
    ON KEY LABEL Enter DO Ch_Mode WITH "View_Ord","print"
    IF let
      ON KEY LABEL F6  DO Ch_Mode WITH "View_Ord","filter"
      ON KEY LABEL Ins DO  Set_Let
    ENDIF
    ON KEY LABEL Home GO TOP
    ON KEY LABEL End  GO BOTTOM
    ON KEY LABEL LeftMouse DO M_Proc_1 WITH "View_Ord", "Ord_"
    ACTIVATE WINDOW Ord_E
    BROWSE FIELDS f001 = LIST_NUM+"/"+ALLTRIM(STR(ORD_NUM)):10:H="" , ;
                  ORD_DATE:H="" , ;
                  LIST_NAME:H="" , ;
                  ARM_LST.ARM_N:H="" , ;
                  f002 = IIF(EMPTY(STO_MARK), "   ", " ๛ "):H="", ;
                  DUMMY.F:H=""   ;
           FREEZE DUMMY.F  ;
           NOMENU NOAPPEND NODELETE NOMODIFY REST NOCLEAR ;
           WINDOW Ord_I IN WINDOW Ord_E COLOR (clr)

*           NOMENU NORGRID NOAPPEND NODELETE NOMODIFY REST NOCLEAR ;
*
*
    ON KEY LABEL F6
    ON KEY LABEL Enter
    ON KEY LABEL Ins
    ON KEY LABEL Home
    ON KEY LABEL End
    ON KEY LABEL LeftMouse
    IF what_do = "list"
      what_do = "exit"
    ENDIF
  CASE what_do = "print"     && ฅ็ โ์ ง ๏ขชจ
    IF new_ord
      DO Prn_OrdN WITH VAL(ALLTRIM(LIST_NUM)), ORD_NUM
    ELSE
      DO Prn_Ord WITH VAL(ALLTRIM(LIST_NUM)), ORD_NUM
    ENDIF
    what_do = "list"
  CASE what_do = "filter"    && “แโ ญฎขช  ไจซ์โเ 
    DO Set_F
    what_do = "list"
  OTHERWISE
    EXIT
  ENDCASE
ENDDO

CLOSE DATABASES
CLEAR WINDOWS
DO Exit_P WITH "", "View_Ord"
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                เฎฃเ ฌฌ  ไฎเฌจเฎข ญจ๏ ฎชฎญ คซ๏ BROWSE.                บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Prep_W

PRIVATE p1, p2, p3, p4, p5, ln, wd, tit, foot

p1 = FSIZE("LIST_NUM")+FSIZE("ORD_NUM")+1
p2 = IIF(SET("CENTURY") = "ON", 10, 8)
p3 = FSIZE("LIST_NAME")
p4 = FSIZE("ARM_N","ARM_LST")
p5 = 3
wd = p1+p2+p3+p4+p5+4
ln = WROWS("")-6

DIMENSION tit(1), foot(1)
tit(1) = PADC("ฎฌฅเ",      p1, "ฤ")+"ย"+   ;
         PADC(" โ ",       p2, "ฤ")+"ย"+   ;
         PADC("ฌ๏ แฏจแช ", p3, "ฤ")+"ย"+   ;
         PADC(" ก. ฌฅแโฎ", p4, "ฤ")+"ย"+   ;
         PADC("“โข",        p5, "ฤ")

foot(1) = REPLICATE("ฤ",p1)+"ม"+   ;
          REPLICATE("ฤ",p2)+"ม"+   ;
          REPLICATE("ฤ",p3)+"ม"+   ;
          REPLICATE("ฤ",p4)+"ม"+   ;
          REPLICATE("ฤ",p5)

DO Def_Win WITH "Ord_M", "Ord_E", "Ord_I", ln, wd, " ๏ขชจ ญ  แชซ ค", tit, foot

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                        “โขฅเฆคฅญจฅ ง ๏ขชจ.                            บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Set_Let

IF EMPTY(STO_MARK)
  REPLACE STO_MARK WITH user
ENDIF

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                    เฎฃเ ฌฌ  ใแโ ญฎขชจ ไจซ์โเ .                       บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Set_F
PRIVATE i, wd, ln, c1, c2, c3, ex, utv_w

IF LEN(a_c) = 0
  i = 1
ELSE
  FOR i = 2 TO ALEN(arm_nams)
    IF a_c = arm_chrs(i)
      EXIT
    ENDIF
  ENDFOR
ENDIF
ex = 1

*                                ”จซ์โเ
*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ                 ษออออออออออออออออป                               ณ
*ณ   กฎ็ฅฅ ฌฅแโฎ  บ  FS+4          บ    [ ] ’ฎซ์ชฎ ญฅใโขฅเฆคฅญญ๋ฅ  ณ
*ณ    (13)         ศออออออออออออออออผ         (25)                  ณ
*ณ                 < OK Ctrl-W > < โช ง โ์แ๏ Esc >  (32)           ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

utv_w = utv
ln = 7
wd = 4+13+2+LEN(arm_nams(1))+4+4+25+4
c1 = 19
c2 = c1+LEN(arm_nams(1))+4+4
c3 = FLOOR((wd-32)/2)

DO D_W WITH "Filter", ln, wd, "”จซ์โเ", 1

@ 3, 4 SAY " กฎ็ฅฅ ฌฅแโฎ"

@ 2, c1 GET i PICTURE "@^" FROM arm_nams SIZE 1, LEN(arm_nams(1))+2
@ 3, c2 GET utv_w PICTURE "@*C ’ฎซ์ชฎ ญฅใโขฅเฆคฅญญ๋ฅ"
@ 5, c3 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? โช ง โ์แ๏ Esc "
READ CYCLE VALID Inst_F()

RELEASE WINDOW Filter
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ        เฎฃเ ฌฌ  - ฎกเ กฎโ็จช ฌ๋่ชจ คซ๏ แฏจแชฎข.                      บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE M_Proc_1
PARAMETER menu_nm, wind_nm
PRIVATE mc, mr, cr

*
*  €แจญๅเฎญญฎฅ ฌฅญ๎
*
IF MENU() == UPPER(menu_nm)
  RETURN
ENDIF

IF MROW("") = 0
  IF .NOT. MENU() == UPPER(menu_nm)
    ACTIVATE MENU (menu_nm)
  ENDIF
  RETURN
ENDIF

*
*  ชญฎ BROWSE
*
mc = MCOL(wind_nm+"E")
mr = MROW(wind_nm+"E")
IF mc >= 0 .AND. mr >= 0
  KEYBOARD CHR(13)
  RETURN
ENDIF

= INKEY("M")

*
*  แญฎขญฎฅ ฎชญฎ
*
mc = MCOL(wind_nm+"M")
mr = MROW("")
IF mc >= 0
  IF mr = WLROW(wind_nm+"E")-1
    KEYBOARD "{PgUP}"
  ENDIF
  IF mr = WLROW(wind_nm+"E")+WROWS(wind_nm+"E")
    KEYBOARD "{PgDn}"
  ENDIF
ENDIF
RETURN


*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                  เฎฃเ ฌฌ  ใแโ ญฎขชจ ไจซ์โเ .                         บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Inst_F

IF ex = 2
  RETURN .T.
ENDIF

utv = utv_w
a_c = arm_chrs(i)

IF utv
  SET FILTER TO EMPTY(STO_MARK) .AND. WHERE = a_c
ELSE
  SET FILTER TO WHERE = a_c
ENDIF

RETURN .T.
