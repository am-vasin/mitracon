*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                      ��оヨキ┘ �牀��Θ.                              �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Let_Sale

PRIVATE mss, us_let

IF Refr_Lst(LIST_TIT.LST_NUM)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"踳Δ� 腑� 竄▲爨��碎, 甄イ礇� ��М÷�碎瘴 � �メ���め�━� ��Ж罔鍖�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

IF .NOT. EMPTY(LIST_TIT.IS_DUE)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"踳Δ� 腑� 竄▲爨��碎, ��Аム�皀瘡 � ぎ����!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF
  
IF .NOT. EMPTY(LIST_TIT.IS_DEF)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"踳Δ� 腑� 竄▲爨��碎, ��Аム�皀瘡 � ぅ筥罔皰�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

IF .NOT. EMPTY(LIST_TIT.SAL_NUM)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�皰 竄▲爨��碎? �Ε �牀����!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

IF EMPTY(LIST_TIT.IS_SPEC) .AND. EMPTY(LIST_TIT.IS_DIS) .AND. EMPTY(LIST_TIT.PRESENT)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�皰 竄▲爨��碎? �モ �� 瓷┐��, �� 甎ユ罐�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

IF .NOT. EMPTY(LIST_TIT.WHO_LET)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"  ���甌� 礒� 竄▲爨ぅ�! ��━ガ 竄▲爨ぅ�┘ [ �� ] [ �モ ]"
  mss(3) = ""
  IF Out_Mess(7, "mss") = 1
    REPLACE LIST_TIT.WHO_LET WITH 0
  ENDIF
  RETURN
ENDIF
PUSH KEY CLEAR
us_let = -1
IF acc_level < 2
  us_let = user
ELSE
  us_let = Get_Acc()
ENDIF
IF us_let > 0
  REPLACE LIST_TIT.WHO_LET WITH us_let
  PRIVATE l, c, ln, wd, w_sav, crsr
  ln = 7
  wd = 36
  l = CEILING((WROWS("")-ln)/2)
  c = FLOOR((WCOLS("")-wd)/2)
  w_sav = WOUTPUT()
  crsr = SET("CURSOR") = "ON"
  SET CURSOR OFF
  DEFINE WINDOW w001 FROM l, c TO l+ln-1, c+wd-1 SHADOW NONE COLOR SCHEME 5
  ACTIVATE WINDOW w001
  @ 2, 0 SAY PADC("���甌� 竄▲爨ぅ�.", WCOLS())
  @ 4, 0 SAY PADC("���!", WCOLS())
  = INKEY(3,"M")
  RELEASE WINDOW w001
  IF crsr
    SET CURSOR ON
  ENDIF
  IF EMPTY(w_sav)
    ACTIVATE SCREEN
  ELSE
    ACTIVATE WINDOW (w_sav)
  ENDIF
ENDIF

POP KEY

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                   蹍▲爲� ����罔�.                                   �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Get_Acc

PRIVATE sel, w_sav, n_list, c_list, i, mss, s, v

sel = SELECT()
w_sav = WOUTPUT()

SELECT 0
USE S_RIGHTS ORDER TAG RIGHT
SELECT 0
USE USERS ORDER TAG USER_NAME
SET RELATION TO S_RIGHTS INTO S_RIGHTS
i = 0
SCAN FOR FOUND("S_RIGHTS")
  IF VAL(LEFT(S_RIGHTS.R_FLAGS,1)) > 1
    LOOP
  ENDIF
  i = i+1
  DIMENSION n_list(i), c_list(i)
  n_list(i) = USER_NAME
  c_list(i) = USER_CODE
ENDSCAN
USE
SELECT S_RIGHTS
USE
SELECT (sel)

IF i = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���-皰 ��� ���竍┼�瘡, 艪� �┴皰 �� ━ゥ� �����..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN -1
ENDIF
  
PRIVATE ln, ln_h, wd, l0, c0
ln_h = i
ln   = MIN(ln_h, WROWS("")-10)
wd   = LEN(n_list(1))+4
l0   = CEILING((WROWS("")-ln-3)/2)
c0   = FLOOR((WCOLS("")-wd)/2)

DEFINE WINDOW G_Pass FROM l0, c0 TO l0+ln+2, c0+wd-1 NONE SHADOW COLOR SCHEME 13
ACTIVATE WINDOW G_Pass
@ 0, 0 SAY PADC("�皰 竅����※� 罐��?", WCOLS()) COLOR SCHEME 5
i = 1
DO WHILE .T.
  i = MAX(i, 1)
  i = MIN(i, ln_h)
  @ 1,1 MENU n_list, ln_h, ln
  READ MENU TO i
  s = READKEY() % 256
  IF s = 15
    v = c_list(i)
    EXIT
  ENDIF
  IF s = 12
    v = -1
    EXIT
  ENDIF
ENDDO

RELEASE WINDOW G_Pass

IF EMPTY(w_sav)
  ACTIVATE SCREEN
ELSE
  ACTIVATE WINDOW (w_sav)
ENDIF

RETURN v
