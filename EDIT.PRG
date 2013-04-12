*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ Edit          งเ กฎโ็จช €ญคเฅฉ  แจญ           11.02.97 14:50:58 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                           ฅค ชโฎเ โฅชแโฎข.                            บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Edit

PRIVATE f_name

f_name = Int_File("", "*", "ฅค ชโจเใฅฌ๋ฉ ไ ฉซ")
IF EMPTY(f_name)
  RETURN
ENDIF

DELETE FILE (tmpo_path+"EDI_TMP.TXT")

COPY FILE (f_name) TO (tmpo_path+"EDI_TMP.TXT")


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


*PUSH KEY CLEAR       &&   ขแ๏ชจฉ ฏฎฆ เญ๋ฉ แซใ็ ฉ!
*DO Prp_Nav_2
DO D_Win_N WITH WROWS("")-3, WCOLS("")-4, f_name
DEFINE WINDOW W_Ed FROM WLROW()+2, WLCOL()+2 TO WLROW()+WROWS()-2, WLCOL()+WCOLS()-3 ;
              NONE COLOR SCHEME 13

PRIVATE what_do
what_do = "Edit"
DO WHILE .T.
  DO CASE
  CASE what_do = "Edit"
    SET SYSMENU AUTOMATIC
    HIDE MENU _msysmenu
    ON KEY LABEL Escape DO Ab_Proc
    MODIFY COMMAND (tmpo_path+"EDI_TMP.TXT") WINDOW W_Ed
    ON KEY LABEL Escape
    what_do = IIF(what_do = "Edit", "Exit", what_do)
  CASE what_do = "Repeat"
     what_do = "Edit"
  CASE what_do = "Exit"
    DELETE FILE (f_name)
    COPY FILE (tmpo_path+"EDI_TMP.TXT") TO (f_name)
    EXIT
  OTHERWISE
    EXIT
  ENDCASE
ENDDO  
DELETE FILE (tmpo_path+"EDI_TMP.TXT")

*POP KEY
RELEASE WINDOW W_Ed
RELEASE WINDOW (win_name)
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                  เฎฃเ ฌฌ  ฎกเ กฎโ็จช ชซ ขจ่จ Esc.                    บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Ab_Proc
PRIVATE mss
DIMENSION mss(3)
mss(1) = ""
mss(2) = CHR(0)+"   ็โฎ, คฅฉแโขจโฅซ์ญฎ ขแฅ ง กใคฅฌ? [  , ง กใคฅฌ! ] [ ฅโ, ขฅเญฅฌแ๏! ]"
mss(3) = ""
DO WHILE INKEY() # 0   &&&&&&&&&&&&
ENDDO

IF Out_Mess(7, "mss") = 1
*  ACTIVATE WINDOW W_Ed
  KEYBOARD "{Ctrl+W}"
  what_do = "Abort"
ELSE
*  ACTIVATE WINDOW W_Ed
  KEYBOARD "{Ctrl+W}"
  what_do = "Repeat"
ENDIF
RETURN
