*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Sto_Send      งเ กฎโ็จช  ซจญ  ฌจโเจฅญชฎ                 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                      ฅเฅค ็  ไ ฉซ  ข DBF-ไฎเฌ โฅ.                     บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 20.11.2001 ออผ
PROCEDURE Sto_Send
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
PRIVATE s_sav         && ฎฌฅเ เ กฎ็ฅฉ ฎกซ แโจ คซ๏ แฎๅเ ญฅญจ๏ จ ขฎแแโ ญฎขซฅญจ๏!

*
*    ฏฎซญ๏ฅฌ งญ ็ฅญจ๏ฌจ ฏฅเฅฌฅญญ๋ฅ แฎแโฎ๏ญจ๏...
*
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()


s_sav = SELECT()

PRIVATE s_n, s_name, s_list, s_fl

SELECT 0
USE (base_path+"TYP_SEND")
n_s = 0
SCAN
  n_s = n_s + 1
  DIMENSION s_name(n_s), s_fl(n_s)
  s_name(n_s) = NOTE
  s_fl(n_s)   = ALLTRIM(FILE_STO)
ENDSCAN

IF n_s = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"ฅโ ญจ ฎคญฎฃฎ โจฏ  ฏฎแ๋ซชจ..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SELECT (s_sav)
  RETURN
ENDIF

s_list = 1
*------------------------------------------------------------------------
*    ‘ฎคฅเฆ โฅซ์ญ ๏ ็ แโ์ ฏเฎฃเ ฌฌ๋:
*
*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ               ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ    ณ
*ณ  ’จฏ๋ ฏฎแ๋ซชจ ณ ....:....:....:....:....: ณ    ณ
*ณ               ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู    ณ
*ณ           ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ ณ
*ณ [ ] ” ฉซ  ณ ....:....:....:....:....:....:.. ณ ณ
*ณ           ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู ณ
*ณ          < OK Ctrl-W >< โช ง โ์แ๏ Esc >       ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
PRIVATE is_file, ex, sav_file
is_file = .F.
ex = 1
sav_file = ALLTRIM(s_fl(s_list))
PUSH KEY CLEAR       &&   ขแ๏ชจฉ ฏฎฆ เญ๋ฉ แซใ็ ฉ!
DO Prp_Nav_2
DO D_Win_N WITH 10, 52, "ฎฌฅญชซ โใเ  ข DBF-ไฎเฌ โฅ"
@ 3, 4 SAY "’จฏ ฏฎแ๋ซชจ"

*      ขฎค ฏฎซฅฉ กซ ญช 

@ 2,17 GET s_list   PICTURE "@^" FROM s_name SIZE 1, 25 VALID Set_File()
@ 6, 3 GET is_file  PICTURE "@*C ” ฉซ" VALID Ch_File()
@ 6,12 EDIT sav_file SIZE 1, 25 WHEN .F.
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? โช ง โ์แ๏ Esc "

READ CYCLE VALID Tst_Send()

IF ex = 1
  DO Send_Ch WITH s_list
ENDIF

USE
SELECT (s_sav)
POP KEY
RELEASE WINDOW(win_name)
RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                      ใโ์ คซ๏ ข๋กเ ญญฎฃฎ โจฏ  ฏฎแ๋ซชจ                  บ
*บ                                                                        บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 21.11.2001 อผ
PROCEDURE Set_File
sav_file = ALLTRIM(s_fl(s_list))
SHOW GET sav_file
RETURN .T.

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                      ใโ์ คซ๏ ข๋กเ ญญฎฃฎ โจฏ  ฏฎแ๋ซชจ                  บ
*บ                                                                        บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 21.11.2001 อผ
PROCEDURE Ch_File
PRIVATE tmpPth, tmpName, i, tmp
IF .NOT. is_file
  RETURN .T.
ENDIF  
i = RAT("\",sav_file)
tmpPth = "C:\"
tmpName = ""
IF i > 0 
  tmpPth = ALLTRIM(LEFT(sav_file,i-1))
  tmpName = ALLTRIM(SUBSTR(sav_file,i+1))
ENDIF
tmp = New_File(tmpPth, "DBF", "คฅ แฎๅเ ญจโ์ เฅงใซ์โ โ?", 0, tmpName)   
IF .NOT. EMPTY(tmp)
  sav_file = ALLTRIM(tmp)
  SHOW GET sav_file
ENDIF
is_file = .F.  
SHOW GET is_file
RETURN .T.
  
*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                             ฎแ๋ซช  !!!                                บ
*บ                                                                        บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 20.11.2001 อผ
PROCEDURE Send_Ch
PARAMETERS s_fld

PRIVATE n_fld, sav_pth
n_fld = ""
SELECT TYP_SEND
GO s_fld
n_fld = ALLTRIM(FIELD_SEND)

SELECT 0
USE (base_path+"STOCK") ORDER TAG NAME
COPY FIELDS &n_fld TO (sav_file)
USE
SELECT TYP_SEND
REPLACE FILE_STO WITH sav_file
USE
RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                             เฎขฅเช .                                  บ
*บ                                                                        บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 20.11.2001 อผ
PROCEDURE Tst_Send
IF ex = 1
  IF File_O(sav_file)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"  ’ ชฎฉ ไ ฉซ ใฆฅ ฅแโ์, ใค ซจฌ? [   ] [ ฅโ ]"
  mss(3) = ""
  IF Out_Mess(7, "mss") = 2
    RETURN .F.
  ENDIF
  DELETE FILE (sav_file)
  ENDIF
ENDIF  

SELECT TYP_SEND
IF RECCOUNT() = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"ฅโ ญจ ฎคญฎฃฎ โจฏ  ฏฎแ๋ซชจ..."
  mss(3) = ""
  IF .NOT. mute
    DO Out_Mess WITH 7, "mss"
  ENDIF
  SELECT (s_sav)
  RETURN
ENDIF

RETURN