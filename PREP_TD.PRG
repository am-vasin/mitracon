*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ          ฎคฃฎโฎขช  คจเฅชโฎเจจ คซ๏ ฏฅเฅค ็จ/ฏเจฅฌ .                   บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Prep_TD
PRIVATE pth                    && ’เ ญงจโญ ๏ คจเฅชโฎเจ๏
PRIVATE w, i                   && •ซ ฌ

*
*   เฎขฅเ๏ฅฌ ญ ซจ็จฅ โเ ญงจโญฎฉ คจเฅชโฎเจจ จ, ฅแซจ ญ คฎ, แฎงค ฅฌ...
*
pth = tmpo_path+"transmit"

IF .NOT. Is_Dir(pth)
  w = "! MD "+pth
  &w
ENDIF
*
*    ็จ้ ฅฌ...
*
pth = pth+"\"
RELEASE w
IF ADIR(w,pth+"*.*") # 0
  FOR i = 1 TO ALEN(w,1)
    DELETE FILE (pth+w(i,1))
  ENDFOR
ENDIF

RETURN
