*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ Mould      งเ กฎโ็จช €ญคเฅฉ + €ซซ              16.04.97 20:20:43 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                     เฎฃเ ฌฌ  ไฎเฌจเฎข ญจ๏ ่ กซฎญ .                    บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Mould
PARAMETERS tmpMONTH, tmpYEAR

**tmpMONTH - N(2), ญฎฌฅเ ฌฅแ๏ๆ 
**tmpYEAR  - N(4), ฃฎค
	
PRIVATE s_date, st

tmpDate = {}

s_date  = "01."+STR(tmpMONTH,2)+"." +STR(tmpYEAR,4)
s_date	= CTOD(s_date)

*** ญฎแจฌ ข  
	
st = ""
DO WHILE tmpMONTH = MONTH(s_date)
  st = st+IIF(INLIST(DOW(s_date), 1, 7), "", "")
  s_date = s_date + 1
ENDDO
APPEND BLANK
REPLACE MONTH WITH tmpMONTH, ;
        YEAR  WITH tmpYEAR,  ;
        DAYS  WITH st
RETURN
