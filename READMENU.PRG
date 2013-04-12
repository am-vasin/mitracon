*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ ReadMenu      งเ กฎโ็จช €ญคเฅฉ  แจญ           20.03.97 21:26:00 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                   งขฅเโ๋ข ฅฌ ฌฅญ๎ จ ข๋กจเ ฅฌ ฏใญชโ.                   บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE ReadMenu
PARAMETERS cd, arr_cd, arr_nm, tit
*
*  cd     - ญ ็ ซ์ญฎฅ งญ ็ฅญจฅ ( ชฎค! );
*  arr_cd - ฌ แแจข ชฎคฎข;
*  arr_nm - ฌ แแจข จฌฅญ;
*  tit    - ง ฃฎซฎขฎช ฌฅญ๎.

PRIVATE win_name, ln, wd, n, i
n = ALEN(arr_cd)
DIMENSION arr_cd(n), arr_nm(n)

i = ASCAN(arr_cd, cd)    && ญ ็ ซ์ญฎฅ งญ ็ฅญจฅ ใช ง โฅซ๏
win_name = "RM0000"      && จฌ๏ ฎชญ 
PUSH KEY CLEAR
ln = MIN(n, WROWS("")-8)
wd = LEN(arr_nm(1))+4

DO D_Win_N WITH ln+3, wd, tit

ln = ln
DO WHILE .T.
  i = MIN(i, n)
  i = MAX(i, 1)
  @ 1, 1 MENU arr_nm, n, ln
  READ MENU TO i
  wd = READKEY() % 256
  IF wd = 15    && Cr
    i = arr_cd(i)
    EXIT
  ENDIF
  IF wd = 12    && Esc
    i = cd
    EXIT
  ENDIF
ENDDO

RELEASE WINDOW (win_name)
POP KEY

RETURN i
