*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�          蹍▲爲� � ��玻オ罔� 瘍モ腮�� ��皋↓諷 ��｀腮� �メ�.         �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Arms_Cnt

PRIVATE pic, mss, mode_ex, k, ok

USE PARMS
SELECT 0

USE ARM_LST
COUNT FOR .NOT. EMPTY(SYS_NAM) TO k

mode_ex = 1
ok = k = PARMS.EXCLU

IF ok
  mss = "�腑砒┴ � ��瑙お�!"
  pic = "@*HT \? �ム�竄赱� Esc ;\\ ��瘁����※碎 Ctrl-W ; �腮痰�碎 ≡�! "
ELSE
  mss = " �腑砒┴ ��珮茱�! "
  pic = "@*HT \? �ム�竄赱� Esc ;\! ��瘁����※碎 Ctrl-W ; �腮痰�碎 ≡�! "
ENDIF

DO D_W WITH "Arms_Cnt", 8, 65, "蹍▲爲� 瘍モ腮�� ��皋↓諷 ��｀腮� �メ�", 1

@ 3, 24 SAY mss
@ 5,  3 GET mode_ex PICTURE pic
READ CYCLE

IF mode_ex = 2
  SELECT PARMS
  REPLACE EXCLU WITH k
ENDIF

IF mode_ex = 3
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"�� �����痰跪 �發�モ� 瓮． �砒モ � 皰�, 艪� 甌；��モメ� 瓩カ�碎?"
  mss(3) = CHR(0)+"  [ �� ]         [ �モ ]"
  mss(4) = ""
  IF Out_Mess(7, "mss") = 1
    REPLACE ALL SYS_NAM WITH " ", ;
                SYS_DAT WITH {},  ;
                SYS_TIM WITH " "
    SELECT PARMS
    REPLACE EXCLU WITH 0
    RELEASE WINDOW Arms_Cnt
    CLOSE DATABASES
    CLEAR
    SET DEFAULT TO (sav_dir)
    s = "! "+LEFT(sav_dir,2)
    &s
    s = "! CD "+SUBSTR(sav_dir,3)
    s = LEFT(s,LEN(s)-1)
    &s
    QUIT
  ENDIF

ENDIF

RELEASE WINDOW Arms_Cnt
CLOSE DATABASES

RETURN
