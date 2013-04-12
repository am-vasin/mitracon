*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                เฎฃเ ฌฌ  - คฎก ขซฅญจ๏ แโเฎชจ ข แฏเฎแ.                 บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Dem_Line
PARAMETER item

PRIVATE ex, tit, tmpCode, tmpProd, s_sav

s_sav = SELECT()
SELECT 0
USE base_path+"STOCK" SHARED AGAIN ORDER TAG CODE ALIAS ST_4619
SELECT 0
USE base_path+"DEMAND" SHARED AGAIN ALIAS DM_4619
SCATTER MEMVAR BLANK
m.qty   = 1
m.who   = user
m.date  = DATE()
m.where = arm
m.producer = "        "
m.pro_code = 1

IF TYPE("item") = "N"
  = SEEK(m.item, "ST_4619")
  m.sto_code = ST_4619.CODE
  m.producer = ST_4619.PRODUCER
  m.pro_code = ST_4619.PRO_CODE
  m.prefix   = ST_4619.PREFIX
  m.name     = ST_4619.NAME
ELSE
  m.name = PADR(m.item, LEN(m.name))
ENDIF
tmpCode = m.pro_code
tmpProd = m.producer
  
ex = 1

*
*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณฺฤ ฎงจๆจ๏ ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟฺฤ ”จเฌ  ฤฤฤฤฟฺฎซ-ขฎฟณ
*ณณ ฐฐฐฐ ฐฐฐฐฑฐฐฐฐฑฐฐฐฐฑฐฐฐฐฑฐฐฐฐฑ ณณ  ฐฐฐฐฑฐฐฐ  ณณฐฐฐฐฑฐณณ
*ณภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤูภฤฤฤฤฤฤฤฤฤฤฤฤูภฤฤฤฤฤฤูณ
*ณ  ”ใญชๆ. ญ งญ ็ฅญจฅ  ฐฐฐฐฑฐฐฐฐฑฐฐฐฐฑฐฐฐฐฑฐฐฐฐฑฐฐฐฐฑฐฐ   ณ
*ณ                                                        ณ
*ณ       ฎคฅซ์  ฏฏ เ โ   ฐฐฐฐฑฐฐฐฐฑฐฐฐฐฑฐฐฐฐฑฐฐฐ         ณ
*ณ                                                        ณ
*ณ           < OK Ctrl-W > < โช ง โ์แ๏ Esc >             ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
tit = "ฎก ขซฅญจฅ แโเฎชจ ข แฏเฎแ"
ACTIVATE SCREEN
@ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
DO D_W WITH "Demand", 11, 60, tit, 1
@ 2,  2 TO 4, 35
@ 2,  4 SAY " ฎงจๆจ๏ "
@ 2, 36 TO 4, 49
@ 2, 38 SAY " ”จเฌ  "
@ 2, 50 TO 4, 57
@ 2, 51 SAY "ฎซ-ขฎ"
@ 5,  4 SAY "”ใญชๆ. ญ งญ ็ฅญจฅ"
@ 7,  9 SAY "ฎคฅซ์  ฏฏ เ โ "
@ 3,  4 GET m.prefix WHEN m.sto_code = 0
@ 3,  9 GET m.name   WHEN m.sto_code = 0
@ 3, 39 GET tmpProd WHEN m.sto_code = 0 VALID Develop(tmpCode)
@ 3, 51 GET m.qty PICTURE "@Z 999999"
@ 5, 23 GET m.function
@ 7, 26 GET m.amodel
@ 9, 13 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? โช ง โ์แ๏ Esc "
READ CYCLE VALID Sav_Bas()

RELEASE WINDOW Demand
USE IN DM_4619
USE IN ST_4619
SELECT (s_sav)

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                    เฎฃเ ฌฌ  ข๋ๅฎค  จง กซ ญช .                        บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Sav_Bas
PRIVATE mss
IF ex = 2
  RETURN .T.
ENDIF
IF m.sto_code = 0 .AND. EMPTY(m.name)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"ฎงจๆจ๏ แ ฏใแโ๋ฌ ญ งข ญจฅฌ? ’ ช ญฅ ก๋ข ฅโ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
APPEND BLANK
m.pro_code = tmpCode
m.producer = tmpProd
GATHER MEMVAR
RETURN .T.
