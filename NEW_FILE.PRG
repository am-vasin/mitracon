*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ New_File      งเ กฎโ็จช €ญคเฅฉ  แจญ           16.02.97 15:16:27 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ               ๋กฎเ ฏใโจ จ จฌฅญจ คซ๏ แฎงค ข ฅฌฎฃฎ ไ ฉซ .               บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE New_File

PARAMETER st_path, ext, titl, sz, fileName
&&  ็ ซ์ญฎฅ งญ ็ฅญจฅ คซ๏ ฏใโจ, ง ฃฎซฎขฎช ฎชญ  จ โเฅกใฅฌฎฅ ฏเฎแโเ ญแโขฎ.

PRIVATE drv, wk_path, mss, drv_list, i, titl_w, sz_w, ext_w
PRIVATE sp_f, sp_h, f_name

sp_f = 0
sp_h = 0
f_name = SPACE(14)
IF TYPE("fileName") = "C"
  f_name = PADR(fileName,14)
ENDIF

*
*    ‘ฏจแฎช ใแโเฎฉแโข
*
drv = Drv_Lst()
i = 0
DO WHILE LEN(drv) > 0
  i = i+1
  DIMENSION drv_list(i)
  drv_list(i) = LEFT(drv,2)
  drv = SUBSTR(drv,3)
ENDDO

*
*    ”ฎเฌจเใฅฌ จฌ๏ คจแช  จ ฏใโ์
*
IF TYPE("titl") = "C"
  titl_w = ALLTRIM(titl)
ELSE
  titl_w = "๋กจเ ฅฌ คจเฅชโฎเจ๎..."
ENDIF

IF TYPE("ext") = "C"
  ext_w = "."+ALLTRIM(ext)
ELSE
  ext_w = "."
ENDIF

IF TYPE("sz") = "N"
  sz_w = sz
ELSE
  sz_w = 0
ENDIF

IF TYPE("st_path") = "C"
  wk_path = ALLTRIM(st_path)
ELSE
  wk_path = "\"
ENDIF

IF EMPTY(wk_path)
  wk_path = "\"
ELSE
  wk_path = IIF(RIGHT(wk_path,1) = "\", wk_path, wk_path+"\")
ENDIF

IF .NOT. Disk_OK(wk_path)     && ฅ ข ฏฎเ๏คชฅ คจแชฅโ ...
  wk_path = "\"
ENDIF

DO WHILE .T.
  wk_path = FULLPATH(wk_path)   && ฎซใ็ ฅฌ ฏฎซญ๋ฉ ฏใโ์ จ...
  drv = LEFT(wk_path, 2)        &&    ข๋คฅซ๏ฅฌ คเ ฉข 
  wk_path = SUBSTR(wk_path,3)   &&      จ แฎกแโขฅญญฎ ฏใโ์

  IF LEN(wk_path) > 1            &&  ชฎฉ โฎ ฏใโ์ ใช ง ญ...
    IF .NOT. Is_Dir(drv+wk_path) &&       ฏเฎขฅเ๏ฅฌ ฅฃฎ ญ ซจ็จฅ
      DIMENSION mss(4)
      mss(1) = ""
      mss(2) = CHR(0)+"“ช ง ญญฎฉ  ฌจ คจเฅชโฎเจจ ญฅ แใ้ฅแโขใฅโ!"
      mss(3) = CHR(0)+"ฅเฅฌ ชฎเญฅขใ๎."
      mss(4) = ""
      DO Out_Mess WITH 7, "mss"
      wk_path = "\"
      LOOP
    ENDIF
  ENDIF
  IF ASCAN(drv_list, drv) = 0
    DIMENSION mss(4)
    mss(1) = ""
    mss(2) = CHR(0)+"ฅ แใ้ฅแโขใฅโ ใแโเฎฉแโข  "+drv+"!"
    mss(3) = CHR(0)+"ฅเฅฌ ชฎเญฅขใ๎ คจเฅชโฎเจ๎ โฅชใ้ฅฃฎ ใแโเฎฉแโข ."
    mss(4) = ""
    DO Out_Mess WITH 7, "mss"
    wk_path = "\"
    LOOP
  ENDIF
  EXIT
ENDDO

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

*
*    ฏฎซญ๏ฅฌ งญ ็ฅญจ๏ฌจ ฏฅเฅฌฅญญ๋ฅ แฎแโฎ๏ญจ๏...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ‘ฎคฅเฆ โฅซ์ญ ๏ ็ แโ์ ฏเฎฃเ ฌฌ๋:
*
DEFINE WINDOW No_Space FROM 0, WCOLS("")-50 TO 1, WCOLS("")-5 NONE SHADOW COLOR SCHEME 7
ACTIVATE WINDOW No_Space NOSHOW

@ 0, 0 SAY PADC("€…! ’เฅกใฅโแ๏ "+  ;
             ALLTRIM(TRANSFORM(sz_w, "99 999 999 999"))+" ก ฉโ.", WCOLS())
@ 1, 0 SAY PADC("  ํโฎฌ ใแโเฎฉแโขฅ แโฎซ์ชฎ ญฅโ!", WCOLS())

ACTIVATE SCREEN

*         ๋กจเ ฅฌ คจเฅชโฎเจ๎...
*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ ฺฤ ’ฅชใ้ ๏ คจเฅชโฎเจ๏ ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ ณ
*ณ ณ ....:....1....:....2....:....3....:....4 ณ ณ
*ณ ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู ณ
*ณ     จเฅชโฎเจจ      ฺฤ ’ฅชใ้จฉ คจแช ฤฤฤฤฤฤฤฟ ณ
*ณ ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ  ณ     ฺฤฤฤฤฤฤฤฤฤฤฟ     ณ ณ
*ณ ณ 12345678 12345 ณ  ณ     ณ          ณ     ณ ณ
*ณ ณ 12345678 12345 ณ  ณ     ภฤฤฤฤฤฤฤฤฤฤู     ณ ณ
*ณ ณ 12345678 12345 ณ  ณ แฅฃฎ 99 999 999 999 ณ ณ
*ณ ณ 12345678 12345 ณ  ณ ‘ขฎก. 99 999 999 999 ณ ณ
*ณ ณ 12345678 12345 ณ  ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู ณ
*ณ ณ 12345678 12345 ณ   ฌ๏ ไ ฉซ  12345678.123  ณ
*ณ ณ 12345678 12345 ณ                           ณ
*ณ ณ 12345678 12345 ณ     <   OK Ctrl-W    >    ณ
*ณ ณ 12345678 12345 ณ                           ณ
*ณ ณ 12345678 12345 ณ     < โช ง โ์แ๏ Esc >    ณ
*ณ ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู                           ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

&&   ก๊๏ขซ๏ฅฌ จ ง ฏฎซญ๏ฅฌ ฏฎซ๏ กซ ญช 
PRIVATE wk_list, nm_w, list_p, drv_p, ex

DIMENSION wk_list(1)

drv_p = ASCAN(drv_list, drv)
list_p = 1
nm_w = drv_list(drv_p)+wk_path
DO Dir_List WITH nm_w
ex   = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       &&   ขแ๏ชจฉ ฏฎฆ เญ๋ฉ แซใ็ ฉ!
DO Prp_Nav_2
DO D_Win_N WITH 19, 50, titl_w
DO Sun_Bord WITH 2, 3, 4, 46
@ 2, 5 SAY " ’ฅชใ้ ๏ คจเฅชโฎเจ๏ "
DO Sun_Bord WITH 5, 23, 11, 46
@ 5, 25 SAY " ’ฅชใ้จฉ คจแช "
@  9,25 SAY "แฅฃฎ"
@ 10,25 SAY "‘ขฎก."
@ 5,  7 SAY "จเฅชโฎเจจ"
DO Show_Space

*------------------------------------------------------------------------
*      ขฎค ฏฎซฅฉ กซ ญช 
*
@  3,  5 EDIT nm_w SIZE 1, 40 WHEN .F. COLOR (SCHEME(13,2))
@  6, 30 GET  drv_p  FROM drv_list PICTURE "@^" SIZE 1,  10 VALID Exit_D()
@  6,  4 GET  list_p FROM wk_list  PICTURE "@&" SIZE 12, 18 VALID Exit_P()  ;
              WHEN ALEN(wk_list) > 1 .OR. .NOT. EMPTY(wk_list(1))
@ 12, 23 SAY "ฌ๏ ไ ฉซ " GET f_name VALID Tst_F_Name()
@ 14, 25 GET  ex PICTURE "@*VT \!   OK Ctrl-W    ;\? โช ง โ์แ๏ Esc " SIZE 1, 16, 1

READ CYCLE VALID Enough_Sp()

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RELEASE WINDOW No_Space

RETURN IIF(ex = 1, drv+wk_path+UPPER(ALLTRIM(f_name)), "")

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                         เฎขฅเช  จฌฅญจ ไ ฉซ .                          บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 16.02.97 15:33:08 อผ
PROCEDURE Tst_F_Name

PRIVATE mss

IF ex # 1 .OR. READKEY() % 256 = 12 .OR. EMPTY(f_name)
  RETURN .T.
  nm_w = (drv_list(drv_p)+wk_path+UPPER(ALLTRIM(f_name)))
  SHOW GET nm_w
ENDIF

IF .NOT. "." $ f_name
  f_name = PADR(ALLTRIM(f_name)+ext_w, LEN(f_name))
ENDIF
nm_w = (drv_list(drv_p)+wk_path+UPPER(ALLTRIM(f_name)))
SHOW GET nm_w

RETURN .T.

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                 เฎขฅเช  ญ ซจ็จ๏ ฏเฎแโเ ญแโข  ญ  คจแชฅ.                บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 13.02.97 16:38:28 อผ
PROCEDURE Enough_Sp

PRIVATE mss
IF ex # 1 .OR. READKEY() % 256 = 12
  RETURN .T.
ENDIF
IF sp_f < sz_w
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"  คจแชฅ ญฅ คฎแโ โฎ็ญฎ ฌฅแโ !"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF EMPTY(f_name)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"๋ โ ช จ ญฅ ใช ง ซจ จฌ๏ ไ ฉซ !"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF File_O(drv_list(drv_p)+wk_path+UPPER(ALLTRIM(f_name)))
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"  ’ ชฎฉ ไ ฉซ ใฆฅ ฅแโ์, ใค ซจฌ? [   ] [ ฅโ ]"
  mss(3) = ""
  IF Out_Mess(7, "mss") = 2
    RETURN .F.
  ENDIF
  DELETE FILE (drv_list(drv_p)+wk_path+UPPER(ALLTRIM(f_name)))
ENDIF

RETURN Tst_F_Name()

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                      ๋ขฎค ฏเฎแโเ ญแโข  ญ  คจแชฅ.                      บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 13.02.97 16:38:28 อผ
PROCEDURE Show_Space

DO Disk_Space WITH drv_list(drv_p), sp_f, sp_h
@  9, 31 SAY sp_h PICTURE "99 999 999 999"
@ 10, 31 SAY sp_f PICTURE "99 999 999 999"
IF sp_f < sz_w
  SHOW WINDOW No_Space TOP
ELSE
  HIDE WINDOW No_Space
ENDIF

RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                 เฎฃเ ฌฌ  ข๋ๅฎค  จง แฏจแช  คจเฅชโฎเจฉ.                 บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 13.02.97 13:43:53 อผ
PROCEDURE Exit_P

PRIVATE prmpt, s

s = READKEY() % 256

IF s # 15
  RETURN .T.
ENDIF

prmpt = ALLTRIM(LEFT(wk_list(list_p),8))

DO CASE
CASE EMPTY(prmpt)      && เ ฉข ญฅ แฎคฅเฆจโ คจเฅชโฎเจฉ
  RETURN .T.
CASE prmpt == "."      && ฎฌฎฉ!
  wk_path = "\"
CASE prmpt == ".."     && ขฅเๅ!
  wk_path = LEFT(wk_path, RAT("\", wk_path, 2))
OTHERWISE
  wk_path = wk_path+prmpt+"\"
ENDCASE

nm_w = drv_list(drv_p)+wk_path
DO Dir_List WITH nm_w
nm_w = nm_w+UPPER(ALLTRIM(f_name))
SHOW GET nm_w
list_p = 1
SHOW GET list_p
DO Show_Space

RETURN .F.

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                      เฎฃเ ฌฌ  ข๋ๅฎค  จง คเ ฉข .                       บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 13.02.97 13:59:53 อผ
PROCEDURE Exit_D

IF drv_list(drv_p) $ "A:B:"
  IF .NOT. Disk_OK(drv_list(drv_p))
    drv_p = ASCAN(drv_list, drv)
    SHOW GET drv_p
    RETURN .F.
  ENDIF
ENDIF

IF drv # drv_list(drv_p)
  drv = drv_list(drv_p)
  list_p = 1
  wk_path = "\"
  nm_w = drv_list(drv_p)+wk_path
  SHOW GET nm_w
  DO Dir_List WITH nm_w
  list_p = 1
  SHOW GET list_p
ENDIF

DO Show_Space

RETURN .T.
