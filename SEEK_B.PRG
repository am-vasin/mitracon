*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                เฎฃเ ฌฌ  ฏฎจแช  แฏจแช  ฏฎ แ็ฅโใ.                      บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Seek_B

PRIVATE ln, wd, ex, t_seek, sum_b, bill_y, bill_n, get_y
PRIVATE f_c000, f_n000, b_c000, b_n000, b_sw000, f_c_old
PRIVATE shab1, shab2
shab1 = "abcdefghijklmnopqrstuvwxyz กขฃคฅ๑ฆงจฉชซฌญฎฏเแโใไๅๆ็่้๊๋์ํ๎๏"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZ€…๐‘’“”•–—"

STORE "" TO f_n000, b_c000, b_n000, b_sw000, f_c_old
bill_n = SPACE(11)
bill_y = YEAR(DATE())
f_c000 = LIST_TIT.FIRM
IF EMPTY(f_c000)
  f_c000 = "   "
ENDIF
ln     = 11
wd     = 41
t_seek = 1
sum_b  = 0
ex     = 1
get_y = .F.

DO D_W WITH "Seek_B", ln, wd, "ฎจแช แฏจแช  ฏฎ แ็ฅโใ", 0

@ 0, 0 SAY ""

? " ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ"
? " ณ ฺฤ ’จฏ ฏฎจแช  ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ ณ"
? " ณ ณ ( ) ฎ ญฎฌฅเใ     ( ) ฎ แใฌฌฅ  ณ ณ"
? " ณ ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู ณ"
? " ณ                                     ณ"
? " ณ ฺฤ ฎค ฤ ฎฌฅเ ฤฤฤฤฟ ฺฤ ‘ใฌฌ  ฤฤฤฤฟ ณ"
? " ณ ณ 9999 99999999999 ณ ณ 9999999.99 ณ ณ"
? " ณ ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู ภฤฤฤฤฤฤฤฤฤฤฤฤู ณ"
? " ณ  < OK Ctrl-W > < โช ง โ์แ๏ Esc >   ณ"
? " ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู"

@ 3,  5 GET t_seek PICTURE "@*RH ฎ ญฎฌฅเใ    ;ฎ แใฌฌฅ" VALID V_Type()
DO Get_Firm WITH 5, 2, .T.
@ 7,  5 GET bill_y PICTURE "@Z 9999" WHEN W_Year()   && VALID V_Year()
@ 7, 10 GET bill_n WHEN t_seek = 1     && VALID V_Num()
@ 7, 26 GET sum_b PICTURE "@Z 9999999.99" WHEN t_seek = 2    && VALID V_Sum()
@ 9,  4 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? โช ง โ์แ๏ Esc "

READ CYCLE VALID All_V()
RELEASE WINDOW Seek_B
IF ex = 2
  RETURN
ENDIF

IF t_seek = 1
  DO Seek_B_N
ELSE
  DO Seek_B_S
ENDIF

RETURN

**************************************************************************
PROCEDURE V_Type

IF READKEY() % 256 = 12
  RETURN .T.
ENDIF

IF t_seek = 1
  sum_b = 0
  IF bill_y = 0
    bill_y = YEAR(DATE())
    get_y = .F.
  ENDIF
ELSE
  bill_y = 0
  bill_n = SPACE(11)
ENDIF

SHOW GETS

RETURN

**************************************************************************
PROCEDURE W_Year

IF t_seek = 2
  RETURN .F.
ENDIF

IF get_y
  RETURN .T.
ENDIF

get_y = .T.
RETURN .F.

**************************************************************************
PROCEDURE V_Year

IF READKEY() % 256 = 12
  RETURN .T.
ENDIF

IF .NOT. BETWEEN(bill_y, YEAR(DATE())-5, YEAR(DATE()) )
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+" ชฎฉ, ฏเฎแโจโฅ, ฃฎค?..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

**************************************************************************
PROCEDURE V_Num

IF READKEY() % 256 = 12
  RETURN .T.
ENDIF

IF .NOT. BETWEEN(AT("-", bill_n), 2, 10)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"ฅ งญ ๎ ็โฎ ํโฎ โ ชฎฅ, ญฎ โฎ็ญฎ ญฅ ญฎฌฅเ แ็ฅโ !"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

**************************************************************************
PROCEDURE V_Sum

IF READKEY() % 256 = 12
  RETURN .T.
ENDIF

IF sum_b <= 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+" ํโฎ - แใฌฌ  ฏฎ แ็ฅโใ?!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

**************************************************************************
PROCEDURE All_V

IF READKEY() % 256 = 12 .OR. ex = 2
  RETURN .T.
ENDIF

IF t_seek = 2
  RETURN V_Sum()
ENDIF
RETURN V_Num() .AND. V_Year()

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                       ฎจแช แ็ฅโ  ฏฎ ญฎฌฅเใ.                          บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Seek_B_N
PRIVATE sel_sav, ln, wd, mss, num_w
PRIVATE arm_w0, nam_w

sel_sav = SELECT()

SELECT BILLS
ln = AT("-", bill_n)
num_w = LEFT(bill_n, ln-1)+PADL(ALLTRIM(SUBSTR(bill_n,ln+1)), 6)
num_w = f_c000+STR(bill_y,4)+num_w

IF .NOT. SEEK(num_w)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"‘็ฅโ "+ALLTRIM(bill_n)+" ญฅ ญ ฉคฅญ."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SELECT (sel_sav)
  RETURN
ENDIF

IF NUM_LIST = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"‘็ฅโ ก๋ซ ข๋ฏจแ ญ กฅง แฏจแช ."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SELECT (sel_sav)
  RETURN
ENDIF
num_w = NUM_LIST
SELECT 0
USE LIST_TIT ORDER TAG LST_NUM ALIAS L_TMP AGAIN
IF .NOT. SEEK(num_w)
  USE
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"‘ฏจแฎช ใฆฅ ใค ซฅญ..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SELECT (sel_sav)
  RETURN
ENDIF
IF LST_ARM = arm
  ln = RECNO()
  USE
  SELECT (sel_sav)
  GO ln IN LIST_TIT
  RETURN
ENDIF

nam_w = LST_NAME
arm_w0 = LST_ARM

SELECT 0
USE ARM_LST ORDER TAG ARM_C ALIAS A_TMP AGAIN

IF arm_w0 = "***"
  arm_w0 = "ก้ ๏ ฎกซ แโ์"
ELSE
  SELECT A_TMP
  SEEK arm_w0
  arm_w0 = ARM_N
ENDIF

ln = 8
wd = 51
DO D_W WITH "Seek_B", ln, wd, "‘็ฅโ ข๋ฏจแ ญ ฏฎ แฏจแชใ:", 0

PRIVATE ex, r_sav
SELECT LIST_TIT
r_sav = RECNO()
GO RECNO("L_TMP")
ex = 1
@ 0, 0 SAY ""

? " ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ"
? " ณ ฺฤฤฤฎฌฅเฤ งข ญจฅ แฏจแช ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ ณ"
? " ณ ณ  999999 ....:....!....:....!....:....!..  ณ ณ"
? " ณ ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู ณ"
? " ณ         กฎ็ฅฅ ฌฅแโฎ ....:....!....:.         ณ"
? " ณ         < ญไฎเฌ ๆจ๏ > < ๋ๅฎค Esc >          ณ"
? " ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู"

@ 3,  6 SAY STR(num_w,6) COLOR SCHEME 1
@ 3, 13 SAY nam_w COLOR SCHEME 1
@ 5, 24 SAY arm_w0 COLOR SCHEME 1

@ 6, 11 GET ex PICTURE "@*HT \ ญไฎเฌ ๆจ๏ ;\? ๋ๅฎค Esc "

READ CYCLE VALID W_Info()

CLEAR WINDOW Seek_B

IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF

SELECT L_TMP
USE
SELECT A_TMP
USE
SELECT (sel_sav)
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                        ฎจแช แ็ฅโ  ฏฎ แใฌฌฅ.                          บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Seek_B_S
PRIVATE sel_sav, mss, w

sel_sav = SELECT()

SELECT 0
DELETE FILE (tmpo_path+"B_L_TMP.DBF") 
CREATE DBF (tmpo_path+"B_L_TMP.DBF")  ;
   ( BILL_NUM  C(15), ;
     LST_NUM   N( 6), ;
     LST_NAME  C(32), ;
     REC_NO    N(10), ;
     STRANGE   L,     ;
     ARM_C     C( 3), ;
     ARM_N     C(16) )

USE (tmpo_path+"B_L_TMP.DBF") EXCLUSIVE

SELECT BILLS
SCAN FOR NUM_LIST # 0 .AND. BILL_SUM = sum_b
  SELECT B_L_TMP
  APPEND BLANK
  REPLACE BILL_NUM WITH LEFT(BILLS.DOC_NUM, 4)+"-"+  ;
                        ALLTRIM(SUBSTR(BILLS.DOC_NUM,5))+ ;
                        IIF(BILLS.N_VER=0, "", "/"+ALLTRIM(STR(BILLS.N_VER,5))), ;
          LST_NUM  WITH BILLS.NUM_LIST
  SELECT BILLS
ENDSCAN

SELECT B_L_TMP
IF RECCOUNT() = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"ฅ ญ ฉคฅญฎ ญจ ฎคญฎฃฎ แ็ฅโ  ญ  แใฌฌใ "+ALLTRIM(STR(sum_b,13,2))+"."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  DELETE FILE (tmpo_path+"B_L_TMP.DBF")
  SELECT (sel_sav)
  RETURN
ENDIF

SELECT 0
USE LIST_TIT ORDER TAG LST_NUM ALIAS L_TMP AGAIN
SELECT 0
USE ARM_LST ORDER TAG ARM_C ALIAS A_TMP AGAIN
SELECT B_L_TMP

SCAN

 IF SEEK(STR(LST_NUM,6), "L_TMP")
   w = ""
   IF L_TMP.LST_ARM = "***"
     w = "ก้ ๏ ฎกซ แโ์"
   ELSE
     IF SEEK(L_TMP.LST_ARM, "A_TMP")
       w = A_TMP.ARM_N
     ENDIF
   ENDIF     
   REPLACE  LST_NAME WITH L_TMP.LST_NAME, ;
            REC_NO   WITH RECNO("L_TMP"), ;
            STRANGE  WITH L_TMP.LST_ARM # arm, ;
            ARM_C    WITH L_TMP.LST_ARM, ;
            ARM_N    WITH w
 ELSE
   REPLACE  LST_NAME WITH "  << ‘ฏจแฎช ใค ซฅญ! >>"
 ENDIF

ENDSCAN

SELECT L_TMP
USE

SELECT A_TMP
USE

*
*   เฃ ญจงใฅฌ ฏเฎแฌฎโเ ฏฎซใ็ฅญญฎฃฎ แฏจแช !
*
PRIVATE ln, wd, tt, ft

DIMENSION tt(1), ft(2)

tt(1) = ;
"ฤ N แ็ฅโ  ฤฤฤฤ N แฏจแช  ฤฤ ฌ๏ แฏจแช  ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ  กฎ็ฅฅ ฌฅแโฎ ฤ"
wd = LEN(tt(1))
ft(1) = ""
ft(2) = "F9 - จญไฎเฌ ๆจ๏, ๋ๅฎค: Enter - ข๋กฎเ แฏจแช , Esc - ฎโช ง โ์แ๏."

SELECT B_L_TMP
GO TOP
ln = MIN(WROWS("")-9, RECCOUNT())

DO Def_Win WITH "B_L_M", "B_L_E", "B_L_I", ln, wd, ;
                 "‘ฏจแชจ แฎ แ็ฅโ ฌจ ญ  แใฌฌใ "+ALLTRIM(STR(sum_b,13,2))+".", ;
                 tt, ft

DO WHILE .T.
  ACTIVATE WINDOW B_L_E
  ON KEY LABEL Enter KEYBOARD CHR(23)
  ON KEY LABEL F9    DO W0_Info
  
  BROW FIELDS BILL_NUM:H="",  ;
              LST_NUM:H="",   ;
              LST_NAME:H="",  ;
              ARM_N:H="",     ;
              DUMMY.F:H=""    ;
              FREEZE DUMMY.F  ;
              NOEDIT NOCLEAR NOMENU WINDOW B_L_I IN WINDOW B_L_E
  ON KEY
  IF READKEY() % 256 = 12
    EXIT
  ELSE
    IF REC_NO = 0 .OR. STRANGE
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"ฎแฌฎโเจโฅ ขญจฌ โฅซ์ญฎ: แฏจแช  ญฅโ จซจ ฎญ ็ใฆฎฉ!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
    ELSE
      w = REC_NO
      IF BETWEEN(w, 1, RECCOUNT("LIST_TIT"))
        GO w IN LIST_TIT
      ENDIF
      EXIT
    ENDIF
  ENDIF
ENDDO

RELEASE WINDOW B_L_I
RELEASE WINDOW B_L_E
RELEASE WINDOW B_L_M

USE
DELETE FILE (tmpo_path+"B_L_TMP.DBF")
SELECT (sel_sav)

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                        ญไฎเฌ ๆจ๏ ฎ ชซจฅญโฅ.                          บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE W_Info

IF ex = 2
  RETURN .T.
ENDIF

DO Lst_Info WITH ""

RETURN .F.

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                        ญไฎเฌ ๆจ๏ ฎ ชซจฅญโฅ.                          บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE W0_Info

IF EOF() .OR. BOF()
  RETURN
ENDIF

IF .NOT. BETWEEN(B_L_TMP.REC_NO, 1, RECCOUNT("LIST_TIT"))
  RETURN
ENDIF

PRIVATE r_sav
SELECT LIST_TIT
r_sav = RECNO("LIST_TIT")
GO B_L_TMP.REC_NO
DO Lst_Info WITH ""
IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF

RETURN .F.
