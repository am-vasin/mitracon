*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ Tst_Main      งเ กฎโ็จช €ญคเฅฉ  แจญ           09.07.97 09:31:53 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                 เฎฃเ ฌฌ   ขโฎญฎฌญฎฉ ฏเฎขฅเชจ ก ซ ญแ .                 บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Tst_Main
PARAMETER fff
PRIVATE base_path, log_path, tmpo_path, p, sys_char
SET DELETED ON
SET EXCLUSIVE OFF
SET TALK OFF

************************ ใโจ ช ก งฅ ค ญญ๋ๅ ฆใเญ ซใ จ ขเฅฌฅญญ๋ฌ ไ ฉซ ฌ

IF fff      && .T. - ฎโซ คช , .F. - เ กฎ็ ๏ . .
  base_path = "D:\VASIN\SALE\NEW_SALE\BASE\"
  log_path  = "D:\VASIN\SALE\NEW_SALE\LOG\"
  tmpo_path = ""
ELSE
  base_path = "Z:\STORE\BASE\"
  log_path  = "Z:\STORE\LOG\"
  tmpo_path = ""
ENDIF

SET PATH TO (base_path)
USE PARMS
sys_char = SYS_ID
USE
DO Set_Lwr
IF MOD(DAY(DATE()),2) = 0
  DO Tst_Bal WITH "N"
ELSE  
  DO Tst_Bal WITH "SCIAK"
ENDIF  
*DO Tst_Bal WITH "N"


*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                      เฎฃเ ฌฌ  ข๋ค ็จ แฎฎก้ฅญจ๏.                      บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Out_Mess
PARAMETERS c_sch, ind
*
* c_sch - ญฎฌฅเ ๆขฅโฎขฎฉ แๅฅฌ๋
* ind   - จญคฅชแ แฎฎก้ฅญจ๏
*
PRIVATE ln, l, w_sav, c_sav, alt, st_w, i1, i2, ind_w, j1, j2, i, s_d
IF TYPE(ind+"(1)") # "C"   && ญฅ ฌ แแจข
  RETURN
ENDIF
*IF c_sch = 7
*  ! spp €…
*ENDIF
s_d = SET("DEVICE") = "PRINTER"
SET DEVICE TO SCREEN
c_sav = SET("CURSOR") = "ON"
SET CURSOR OFF
ln = ALEN(&ind)            && ็จแซฎ แโเฎช ข แฎฎก้ฅญจจ
DIMENSION ind_w(ln)

*
*                ฎจแช  ซ์โฅเญ โจข
*
PRIVATE alt_n, alt_lst
alt_n = 0
FOR l = 1 TO ln   && ฏเฎแฌ โเจข ฅฌ ฏฎแซฅคฎข โฅซ์ญฎ แโฎชจ แฎฎก้ฅญจ๏ จ...
  st_w = &ind(l)
  IF LEFT(st_w,1) = CHR(0)
    st_w = PADC(SUBSTR(st_w,2),WCOLS("")-2)
  ENDIF
  ind_w(l) = ""   && ฏฅเฅฏจแ๋ข ฅฌ จๅ ข เ กฎ็จฉ ฌ แแจข จ ฏเจ ํโฎฌ:
  DO WHILE .NOT. EMPTY(st_w)
    i1 = AT("[",st_w)   && จ้ฅฌ ญ ็ ซฎ ฎ็ฅเฅคญฎฉ  ซ์โฅเญ โจข๋
    IF i1 # 0           && ฅแซจ ญ ่ซจ โฎ...
      ind_w(l) = ind_w(l)+LEFT(st_w,i1-1)
      st_w = SUBSTR(st_w,i1+1)
      i2 = AT("]",st_w)   && จ้ฅฌ ชฎญฅๆ ํโฎฉ  ซ์โฅเญ โจข๋
      IF i2 # 0
        alt_n = alt_n+1
        DIMENSION alt_lst(alt_n,3)
        alt_lst(alt_n,1) = l
        alt_lst(alt_n,2) = LEN(ind_w(l))+1
        ind_w(l) = ind_w(l)+LEFT(st_w,i2-1)
        alt_lst(alt_n,3) = LEN(ind_w(l))
        st_w = SUBSTR(st_w,i2+1)
      ELSE
        ind_w(l) = ind_w(l)+st_w
        st_w = ""
      ENDIF
    ELSE
      ind_w(l) = ind_w(l)+st_w
      st_w = ""
    ENDIF
  ENDDO
ENDFOR
alt = alt_n > 1

*
*   ฅซ ฅฌ ฎชญฎ
*
w_sav = WOUTPUT()
ACTIVATE SCREEN
l = WROWS()-ln-2
DEFINE WINDOW w_mess FROM l, 0 TO WROWS()-1, WCOLS()-1 COLOR SCHEME c_sch ;
                                SHADOW NONE
ACTIVATE WINDOW w_mess
@ 0,0 TO WROWS()-1, WCOLS()-1 DOUBLE
l = FLOOR((WCOLS()-IIF(alt,32,23))/2)
@ WROWS()-1, l SAY IIF(alt, " ๋กฅเจโฅ ฎโขฅโ จ ญ ฆฌจโฅ Enter", ;
                            "  ฆฌจโฅ ซ๎กใ๎ ชซ ขจ่ใ ")
FOR l = 1 TO ln
  @ l, 1 SAY ind_w(l)
ENDFOR
i1 = 0
IF alt              && ฅแซจ แฎฎก้ฅญจฅ แ  ซ์โฅเญ โจข ฌจ...
  i1 = 1
  st_w = SCHEME(c_sch,2)
  DO WHILE .T.
    i1 = IIF(i1<1, alt_n, i1)
    i1 = IIF(i1>alt_n, 1, i1)
    @ alt_lst(i1,1),alt_lst(i1,2) FILL TO alt_lst(i1,1),alt_lst(i1,3) ;
          COLOR (st_w)
    i2 = INKEY(0,"M")
    IF i2 = 19 .OR. i2 = 4 .OR. i2 = 151
       @ alt_lst(i1,1),alt_lst(i1,2) FILL TO alt_lst(i1,1),alt_lst(i1,3) ;
            COLOR SCHEME c_sch
    ENDIF
    IF i2 = 28
      IF TYPE("hlp_idx") # "C"
        LOOP
      ENDIF
*      DO Hlp_Mess WITH hlp_idx
    ENDIF
    IF i2 = 19     && แโเฅซช  ขซฅขฎ
      i1 = i1-1
    ENDIF
    IF i2 = 4      && แโเฅซช  ขฏเ ขฎ
      i1 = i1+1
    ENDIF
    IF i2 = 13     && Enter
      EXIT
    ENDIF
    IF i2 = 151    && ฌ๋่ช 
      j1 = MROW()
      j2 = MCOL()
      FOR i = 1 TO alt_n
        IF j1 = alt_lst(i,1) .AND. ;
           BETWEEN(j2,alt_lst(i,2),alt_lst(i,3))
          i1 = i
          KEYBOARD CHR(13)
          EXIT
        ENDIF
      ENDFOR
    ENDIF
  ENDDO
ELSE
  i1 = 0
  = INKEY(0,"M")
ENDIF
RELEASE WINDOW w_mess
IF .NOT. EMPTY(w_sav)
  ACTIVATE WINDOW (w_sav)
ENDIF
IF c_sav
  SET CURSOR ON
ENDIF
IF s_d
  SET DEVICE TO PRINTER
ENDIF
RETURN i1

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ             เฎฃเ ฌฌ  ฏเฎขฅเชจ ญ ซจ็จ๏ ไ ฉซ .                         บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE File_O
PARAMETERS f_nam

PRIVATE arr_w
RETURN adir(arr_w, f_nam) > 0

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ          เฎๆฅคใเ  ฎโชเ๋โจ๏ ( แฎงค ญจ๏ ) ไ ฉซ  - ฏใแโ๋่ชจ.            บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Use_Dummy

SELECT 0
IF .NOT. FILE_O(tmpo_path+"DUMMY.DBF")
  CREATE DBF (tmpo_path+"DUMMY") ( F C(1))
  APPEND BLANK
ENDIF
USE (tmpo_path+"DUMMY") EXCLUSIVE
RETURN
