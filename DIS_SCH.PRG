*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                蹍������ �������� 痳ガ ����硅.                        �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Dis_Sch

PRIVATE ln_b, wd_b, dm, ex_f

DEFINE MENU Dis_Sch IN SCREEN COLOR SCHEME 4
DEFINE PAD Modi OF Dis_Sch PROMPT "��玻オ罔�" KEY Ctrl-A, "F2"
DEFINE PAD Exit OF Dis_Sch PROMPT "�諷��" KEY Ctrl-A, "Esc"

ON SELECTION PAD Modi OF Dis_Sch Do Menu_Proc WITH .F.
ON SELECTION PAD Exit OF Dis_Sch Do Menu_Proc WITH .T.
HIDE MENU Main_M
DO Entry_P WITH "", "Dis_Sch"

USE DIS_SCH ORDER TAG COLLECT
DIMENSION dm(1)
dm(1) = ""
ln_b = RECCOUNT()
wd_b = FSIZE("SCH_NAM")

PUSH KEY CLEAR
ON KEY LABEL F10 ACTIVATE MENU Dis_Sch
ON KEY LABEL F2 Do Menu_Proc WITH .F.
ON KEY LABEL LeftMouse DO M_Prc

DO Def_Win WITH "Sch_M", "Sch_E", "Sch_I", ln_b, wd_b, "�絅�� ����硅", dm
ZOOM WINDOW Sch_I NORM SIZE WROWS("Sch_I")+1, WCOLS("Sch_I")

DO WHILE .T.
  ex_f = .T.
  ACTIVATE WINDOW Sch_E
  BROWSE FIELDS SCH_NAM:V=Exit_N():F  ;
         NOAPPEND NODELETE NOMENU NORGRID WINDOW Sch_I IN Sch_E
  IF ex_f
    EXIT
  ENDIF
  DO Sch_Bound
ENDDO
POP KEY
RELEASE WINDOW Sch_M
RELEASE WINDOW Sch_E
RELEASE WINDOW Sch_I

DO Exit_P WITH "", "Dis_Sch"
CLOSE DATABASES
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�             蹍������ ��玻オ罔� ������ 痳ガ� ����硅.                  �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Sch_Bound

PRIVATE bound, perc, numb, pic_m, pic_p, ln, wd, i, ex

numb  = 7
pic_m = "@Z 999999.99"
pic_p = "@Z 9999.99"
DIMENSION perc(numb), bound(numb)

perc  = 0
bound = 0

DO Get_B

ln = 5+numb
wd = 41

ACTIVATE SCREEN
HIDE MENU Dis_Sch
@ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
PUSH KEY CLEAR

DO D_W WITH "Sch_Bound", ln, wd, "�絅��: "+ALLTRIM(SCH_NAM), 0

@ 2, 1 TO WROWS()-2, WCOLS()-2
@ 1, 0 SAY "       �祠�� (USD)       ��┐�� (%)      "
ex = 1
FOR i = 1 TO numb-1
  @ i+2, 8 GET bound(i) PICTURE pic_m WHEN W_F() VALID V_F()
  @ i+2,24 GET perc(i) PICTURE pic_p WHEN W_F()
ENDFOR
@ WROWS()-1, 1 GET ex PICTURE "@*H \ ��縲���碎 Ctrl-W ; �皖���碎瘴 Esc "
READ CYCLE

IF READKEY() % 256 # 12 .AND. ex = 1
  DO Put_B
ENDIF
RELEASE WINDOW Sch_Bound
ACTIVATE SCREEN
@ 0, 0 SAY PADL("�キ� F10 ",WCOLS()) COLOR SCHEME 13
SHOW MENU Dis_Sch

POP KEY
  
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                    蹍������ ��甎���→� ������.                       �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Get_B

PRIVATE str_w, v, ind, n
str_w = ALLTRIM(SUBSTR(SCH_VAL,5))
n = 2*numb

FOR i = 1 TO n
  IF EMPTY(str_w)
    EXIT
  ENDIF
  str_w = SUBSTR(str_w,2)
  ind = AT("!",str_w)
  IF ind = 0
    v = ALLTRIM(str_w)
    str_w = ""
  ELSE
    v = ALLTRIM(LEFT(str_w,ind-1))
    str_w = SUBSTR(str_w,ind)
  ENDIF
  v = VAL(v)
  ind = CEILING(i/2)
  IF i % 2 # 0
    bound(ind) = v
  ELSE
    perc(ind) = v
  ENDIF
ENDFOR

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                      蹍������ 祚���→� ������.                       �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Put_B

PRIVATE str_w, i, v

str_w = "!0!0"    &&+ALLTRIM(STR(perc(1),7,2))

FOR i = 1 TO numb
  IF bound(i) = 0 .AND. i # 1
    EXIT
  ENDIF
  str_w = str_w+"!"+ALLTRIM(STR(bound(i),10,2))
  str_w = str_w+"!"+ALLTRIM(STR(perc(i),7,2))
ENDFOR
REPLACE SCH_VAL WITH str_w

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                      蹍▲爲� √��� � ����.                           �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE W_F

PRIVATE ind, i1, i2, nm

ind = VARREAD()
nm  = LEFT(ind,1)
i1  = AT("(",ind)+1
i2  = AT(")",ind)-i1
ind = SUBSTR(ind,i1,i2)
ind = VAL(ALLTRIM(ind))
@ 1,0 FILL TO WROWS()-1,WCOLS()-1 COLOR SCHEME 13
IF nm = "B"
  IF ind < 3
    RETURN .T.
  ENDIF
  RETURN bound(ind-1) # 0
ELSE
  IF ind = 1
    RETURN .T.
  ENDIF
  RETURN bound(ind) # 0
ENDIF
  

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                 蹍▲爲� �諷��� ├ ���� �����肓.                      �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE V_F

IF READKEY() % 256 = 12
  RETURN .T.
ENDIF

PRIVATE ind, i1, i2, mss

ind = VARREAD()
IF LEFT(ind,1) = "P"
  @ 1,0 FILL TO WROWS()-1,WCOLS()-1 COLOR SCHEME 13
  RETURN .T.
ENDIF  
i1  = AT("(",ind)+1
i2  = AT(")",ind)-i1
ind = SUBSTR(ind,i1,i2)
ind = VAL(ALLTRIM(ind))
IF bound(ind) < 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�矗���皀�讚�� �������? �� ��� �� °��モ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
IF bound(ind) = 0 .AND. ind # 1
  FOR i1 = ind TO numb
    perc(i1) = 0
    bound(i1) = 0
  ENDFOR
  SHOW GETS
  @ 1,0 FILL TO WROWS()-1,WCOLS()-1 COLOR SCHEME 13
  RETURN .T.
ENDIF
IF ind > 1
  IF bound(ind) < bound(ind-1)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"��イ竡��� ������� �キ跏� �爛るゃ薀�? �皰-皰 Гメ� �� ���!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
ENDIF
RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                蹍▲爲� �諷��� ├ ���� ��Б����.                      �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Exit_N

IF EMPTY(SCH_NAM)
  REPLACE SCH_VAL WITH " "
ENDIF
RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                      ｡��｀砒┴ �祗��� �キ�.                          �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Menu_Proc
PARAMETER s

ex_f = s
KEYBOARD CHR(23)
IF MENU() = "DIS_SCH"
  DEACTIVATE MENU
ENDIF

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                               �謌��.                                  �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE M_Prc

IF MROW("") = 0
  ACTIVATE MENU Dis_Sch
  RETURN
ENDIF
IF MROW("Sch_E") >= 0 .AND. MCOL("Sch_E") >= 0
  RETURN
ENDIF
= INKEY("M")
RETURN
