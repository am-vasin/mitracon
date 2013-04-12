*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ Int_File      งเ กฎโ็จช €ญคเฅฉ  แจญ           12.02.97 16:31:04 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                      ญโฅเ ชโจขญ๋ฉ ข๋กฎเ ไ ฉซ .                        บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Int_File

PARAMETER st_path, ext, titl     &&  ็ ซ์ญฎฅ งญ ็ฅญจฅ คซ๏ ฏใโจ

PRIVATE drv, wk_path, mss, drv_list, i, ext_w, titl_w

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
  titl_w = "๋กจเ ฅฌ ไ ฉซ..."
ENDIF

IF TYPE("st_path") = "C"
  wk_path = ALLTRIM(st_path)
ELSE
  wk_path = "\"
ENDIF
IF TYPE("ext") = "C"
  ext_w = ALLTRIM(ext)
ELSE
  ext_w = "*"
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

*         ๋กจเ ฅฌ ไ ฉซ...
*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ ฺฤ ’ฅชใ้ ๏ คจเฅชโฎเจ๏ ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ ณ
*ณ ณ ....:....1....:....2....:....3....:....4 ณ ณ
*ณ ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู ณ
*ณ ” ฉซ๋ จ คจเฅชโฎเจจ                           ณ
*ณ ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ  ฺฤ ’ฅชใ้จฉ คจแช ฤฤฤฤฤฤฤฟ ณ
*ณ ณ 12345678 12345 ณ  ณ     ฺฤฤฤฤฤฤฤฤฤฤฟ     ณ ณ
*ณ ณ 12345678 12345 ณ  ณ     ณ          ณ     ณ ณ
*ณ ณ 12345678 12345 ณ  ณ     ภฤฤฤฤฤฤฤฤฤฤู     ณ ณ
*ณ ณ 12345678 12345 ณ  ณ แฅฃฎ 99 999 999 999 ณ ณ
*ณ ณ 12345678 12345 ณ  ณ ‘ขฎก. 99 999 999 999 ณ ณ
*ณ ณ 12345678 12345 ณ  ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู ณ
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
DO Fil_List WITH nm_w, ext_w
ex   = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       &&   ขแ๏ชจฉ ฏฎฆ เญ๋ฉ แซใ็ ฉ!
DO Prp_Nav_2
DO D_Win_N WITH 19, 50, titl_w
DO Sun_Bord WITH 2, 3, 4, 46
@ 2, 5 SAY " ’ฅชใ้ ๏ คจเฅชโฎเจ๏ "
DO Sun_Bord WITH 6, 23, 12, 46
@ 6, 25 SAY " ’ฅชใ้จฉ คจแช "
@ 10,25 SAY "แฅฃฎ"
@ 11,25 SAY "‘ขฎก."
@ 5,  3 SAY "” ฉซ๋ จ คจเฅชโฎเจจ"
DO Show_Space

*------------------------------------------------------------------------
*      ขฎค ฏฎซฅฉ กซ ญช 
*
@  3,  5 EDIT nm_w SIZE 1, 40 WHEN .F. COLOR (SCHEME(13,2))
@  7, 30 GET  drv_p  FROM drv_list PICTURE "@^" SIZE 1,  10 VALID Exit_D()
@  6,  3 GET  list_p FROM wk_list  PICTURE "@&" SIZE 12, 18 VALID Exit_P() ;
                          WHEN Refr_P()
@ 14, 25 GET  ex PICTURE "@*VT \!   OK Ctrl-W    ;\? โช ง โ์แ๏ Esc " SIZE 1, 16, 1

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)

RETURN IIF(ex = 1, drv+wk_path+ALLTRIM(wk_list(list_p)), "")

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                      ๋ขฎค ฏเฎแโเ ญแโข  ญ  คจแชฅ.                      บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 13.02.97 16:38:28 อผ
PROCEDURE Show_Space
PRIVATE sp_f, sp_h

sp_f = 0
sp_h = 0
DO Disk_Space WITH drv_list(drv_p), sp_f, sp_h
@ 10, 31 SAY sp_h PICTURE "99 999 999 999"
@ 11, 31 SAY sp_f PICTURE "99 999 999 999"

RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                          ...ฎกญฎขซ๏๏ ฏใโ์...                           บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 13.02.97 16:09:10 อผ
PROCEDURE Refr_P

nm_w = drv_list(drv_p)+wk_path
SHOW GET nm_w
RETURN .T.

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                ”ฎเฌจเฎข ญจฅ แฏจแช  ไ ฉซฎข จ คจเฅชโฎเจฉ.                บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 13.02.97 15:36:22 อผ
PROCEDURE Fil_List
PARAMETER pth, ext

PRIVATE n1, n2, a_w, i, n_sv

DO Dir_List WITH pth
n1 = ALEN(wk_list)
IF n1 = 1 .AND. EMPTY(wk_list(1))
  n1 = 0
ENDIF
n_sv = n1+1

n2 = ADIR(a_w,pth+"*."+ext)
IF n2 # 0
  FOR i = 1 TO n2
    n1 = n1+1
    DIMENSION wk_list(n1)
    wk_list(n1) = LOWER(a_w(i,1))
  ENDFOR
  = ASORT(wk_list, n_sv)
ENDIF
RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                 เฎฃเ ฌฌ  ข๋ๅฎค  จง แฏจแช  คจเฅชโฎเจฉ.                 บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 13.02.97 13:43:53 อผ
PROCEDURE Exit_P

PRIVATE prmpt, s, ex

s = READKEY() % 256

IF s # 15
  RETURN .T.
ENDIF

prmpt = wk_list(list_p)

IF "<DIR>" $ prmpt .OR. INLIST(ALLTRIM(prmpt), ".", "..")
  prmpt = ALLTRIM(LEFT(prmpt,8))
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
  list_p = 1
  DO Fil_List WITH nm_w, ext_w
  SHOW GET list_p
  ex = .F.
ELSE
  prmpt = ALLTRIM(prmpt)
  nm_w = nm_w+prmpt
  ex = .T.
ENDIF

SHOW GET nm_w
DO Show_Space

RETURN ex

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                      เฎฃเ ฌฌ  ข๋ๅฎค  จง คเ ฉข .                       บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 13.02.97 13:59:53 อผ
PROCEDURE Exit_D
PRIVATE mss

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
  DO Fil_List WITH nm_w, ext_w
  list_p = 1
  SHOW GET list_p
ENDIF
DO Show_Space
IF ALEN(wk_list) = 1 .AND. EMPTY(wk_list(1))
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"€ ญ  ใช ง ญญฎฌ  ฌจ ใแโเฎฉแโขฅ ขฎฎก้ฅ ญฅโ ไ ฉซฎข!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN 0
ENDIF

RETURN .T.
