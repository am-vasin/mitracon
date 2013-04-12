*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ St_Send2      งเ กฎโ็จช €ญคเฅฉ  แจญ           27.06.97 10:02:27 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ         ฅเฅแ๋ซช  แโ ญค เโญฎฃฎ แฏจแช  ไ ฉซฎข ญ  ฎแญ. ฏซฎ้ คชใ.         บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE St_Send2
PRIVATE mss, pthw

IF sys_char = "A"
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"โฎ ฎแญฎขญ ๏ ฏซฎ้ คช !"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

DO Prep_TD    && ‘ฎงค ฅฌ จซจ ฎ็จ้ ฅฌ โเ ญงจโญใ๎ คจเฅชโฎเจ๎
pthw = tmpo_path+"transmit\"

DO Prep_St2 WITH wk_char, pthw

DO Arch WITH pthw, "ST_SEND2"

PRIVATE out_path
n = ADIR(arr_w,pthw+"*.*")
FOR i = 1 TO n
  IF LEFT(arr_w(i,1),10) # "ST_SEND2.A")
    DELETE FILE (pthw+arr_w(i,1))
  ENDIF
ENDFOR

out_path = "\"
out_path = Int_Dir(out_path, "ใค  ฏจ่ฅฌ ค ญญ๋ฅ?", 0)

IF .NOT. EMPTY(out_path)
  DO Copy_A WITH pthw+"ST_SEND2", out_path+"ST_SEND2"  && ‘ฎๅเ ญฅญจฅ  เๅจข 
ENDIF

RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ  ฎคฃฎโฎขช  แโ ญค เโญฎฃฎ แฏจแช  ไ ฉซฎข คซ๏ ฏฅเฅค ็จ ญ  ฎแญ. ฏซฎ้ คชใ.  บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 27.06.97 10:52:22 อผ
PROCEDURE Prep_St2
PARAMETERS w_ch, pth
PRIVATE arr_w, p, i

*
*     ซจฅญโ๋
*
DO Wt_Mess WITH "ซจฅญโ๋"
USE (base_path+"ACCOUNT")
COPY TO (pth+"ACCOUNT")
DO Wt_Mess

CLOSE DATABASES

RETURN
