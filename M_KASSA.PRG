*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                        ���� �।�����.                             �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE M_Kassa
PARAMETERS l_corr   && �᫨ ���� - ���४�� ࠧ�襭�

PRIVATE let_w, oper_nam, oper_lst, oper_flt, oper_num, i, j, w, tmpRec
PRIVATE i_cod, i_nam, i_dir, i_usd, i_rub, i_func, i_bill, f_num, pic
PRIVATE q_usd, q_rub, seek_shab, seek_rest, seek_back, case_ign
DIMENSION q_usd(6), q_rub(9)

seek_shab = ""
seek_rest = .F.
seek_back = .T.
case_ign  = .T.
q_rub = 0
q_usd = 0

let_w = IIF(TYPE("l_corr") = "L", l_corr, .T.)

STORE 0 TO i_dir, i_cod, i_nam, i_usd, i_rub, i_func, i_bill, i_hot

USE (base_path+"MM_Types") ORDER TAG NAME
f_num = FCOUNT()
FOR i = 1 TO f_num
  DO CASE
  CASE FIELD(i) == "CODE"
    i_cod = i
  CASE FIELD(i) == "NAME"
    i_nam = i
  CASE FIELD(i) == "M_DIR"
    i_dir = i
  CASE FIELD(i) == "M_USD"
    i_usd = i
  CASE FIELD(i) == "M_RUB"
    i_rub = i
  CASE FIELD(i) == "C_BILL"
    i_bill = i
  CASE FIELD(i) == "M_FUNC"
    i_func = i
  CASE FIELD(i) == "HOT_KEY"
    i_hot = i
  ENDCASE
ENDFOR

oper_num = RECCOUNT()
DIMENSION oper_nam(oper_num), oper_lst(oper_num, f_num)
FOR i = 1 TO oper_num
  SCATTER TO w
  FOR j = 1 TO f_num
    oper_lst(i, j) = w(j)
  ENDFOR
  oper_nam(i) = w(i_nam)
  IF w(i_hot) > 0 .AND. w(i_hot) <= LEN(w(i_nam))
    oper_nam(i) = LEFT(oper_nam(i), w(i_hot)-1)+"\<"+SUBSTR(oper_nam(i), w(i_hot))
  ENDIF
  SKIP
ENDFOR
SET ORDER TO TAG CODE

SELECT 0
USE (base_path+"KASSA_S")

SELECT 0
USE (base_path+"ARM_LST") ORDER TAG ARM_C

SELECT 0
USE (base_path+"USERS") ORDER TAG USER_CODE

SELECT 0
USE (base_path+"KASSA_M") ORDER TAG HRON ALIAS VIEW AGAIN
SET RELATION TO CODE  INTO MM_TYPES,   ;
                WHO   INTO USERS,      ;
                WHERE INTO ARM_LST
PRIVATE ln, wd, tt, ft, wd_n, p1, p2, p3
DIMENSION tt(1), ft(3)
ln = WROWS("")-9
*p1 = "@Z "+REPLICATE("9",FSIZE("SUM_R","VIEW"))
*p2 = "@Z "+REPLICATE("9",FSIZE("SUM_D","VIEW"))
*p2 = STUFF(p2, LEN(p2)-3, 1, ".")
p1 = "@Z 9 999 999.99"
p2 = "@Z 99 999 999.9999"
tt(1) = PADC("��� ����樨",FSIZE("NAME","MM_TYPES"), "�")+"�"+  ;
        PADC("�㡫�",12, "�")+"�"+  ;
        PADC("USD",15, "�")+"�"
wd_n = MIN(WCOLS("")-LEN(tt(1)), FSIZE("NOTE","VIEW") )
p3 = "@S"+ALLTRIM(STR(wd_n))
pic = "@S"+ALLTRIM(STR(FSIZE("DOC_NUM","VIEW")+1))
tt(1) = tt(1)+PADC("�ਬ�砭��",wd_n, "�")
wd = LEN(tt(1))
ft(1) = ""
ft(2) = ""
ft(3) = ""
DO Def_Win WITH "Kassa_M", "Kassa_E", "Kassa_I", ln, wd,   ;
                "���� �।�����", tt, ft

ACTIVATE WINDOW Kassa_E
GO BOTTOM
PRIVATE what_do
what_do = "List"

DEFINE MENU M_Kassa IN SCREEN COLOR SCHEME 4
DEFINE PAD Cup OF M_Kassa PROMPT "�����"
ON PAD Cup OF M_Kassa ACTIVATE POPUP Cup
*
*    POPUP - ���� ��ண� �஢��
*
DEFINE POPUP Cup MARGIN RELATIVE SHADOW COLOR SCHEME 4

DEFINE BAR  1 OF Cup PROMPT "�㡫� " KEY Ctrl-A, "F1"
DEFINE BAR  2 OF Cup PROMPT "������� " KEY Ctrl-A, "Ctrl-F1"
ON SELECTION BAR 1 OF Cup DO Ch_Mode WITH "M_Kassa", "Clc_R"
ON SELECTION BAR 2 OF Cup DO Ch_Mode WITH "M_Kassa", "Clc_Usd"

DEFINE PAD Seek OF M_Kassa PROMPT "����"
ON PAD Seek OF M_Kassa ACTIVATE POPUP Seek
DEFINE POPUP Seek MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR  1 OF Seek PROMPT "���⥪��� ���� � �ਬ�砭�� " KEY Ctrl-A, "F7"
DEFINE BAR  2 OF Seek PROMPT "�த������� ���᪠ " KEY Ctrl-A, "Ctrl-F7"
DEFINE BAR  3 OF Seek PROMPT "���� �� �㬬�" KEY Ctrl-A, "Shift-F7"
ON SELECTION BAR 1 OF Seek DO Ch_Mode WITH "M_Kassa", "Seek_F"
ON SELECTION BAR 2 OF Seek DO Ch_Mode WITH "M_Kassa", "Seek_N"
ON SELECTION BAR 3 OF Seek DO Ch_Mode WITH "M_Kassa", "Seek_S"

DEFINE PAD Rest OF M_Kassa PROMPT "���⮪" KEY Ctrl-A, "F2"
ON SELECTION PAD Rest OF M_Kassa DO Rest

IF let_w
  DEFINE PAD Inc  OF M_Kassa PROMPT "��室/���室" KEY Ctrl-A, "F3"
  ON SELECTION PAD Inc OF M_Kassa DO Ch_Mode WITH "M_Kassa", "Inc"
ENDIF

DEFINE PAD Arms OF M_Kassa PROMPT "���.�." KEY Ctrl-A, "F6"
ON SELECTION PAD Arms OF M_Kassa DO Ch_Mode WITH "M_Kassa", "Arms"

DEFINE PAD Exit OF M_Kassa PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF M_Kassa DO Ch_Mode WITH "M_Kassa", "Exit"

DO WHILE .T.
  DO CASE
  CASE what_do = "List"
    DO Prp_Menu WITH "M_Kassa"
    ON KEY LABEL F10 ACTIVATE MENU M_Kassa
    ON KEY LABEL F2 DO Rest
    ON KEY LABEL F1 DO Ch_Mode WITH "M_Kassa", "Clc_R"
    ON KEY LABEL Ctrl+F1 DO Ch_Mode WITH "M_Kassa", "Clc_USD"
    ON KEY LABEL F7 DO Ch_Mode WITH "M_Kassa", "Seek_F"
    ON KEY LABEL F6 DO Ch_Mode WITH "M_Kassa", "Arms"
    ON KEY LABEL Ctrl+F7 DO Ch_Mode WITH "M_Kassa", "Seek_N"
    ON KEY LABEL Shift+F7 DO Ch_Mode WITH "M_Kassa", "Seek_S"
    IF let_w
      ON KEY LABEL F3 DO Ch_Mode WITH "M_Kassa", "Inc"
*      ON KEY LABEL F4 DO Ch_Mode WITH "M_Kassa", "Exp"
      BROWSE FIELDS MM_TYPES.NAME:H="", ;
                    SUM_R:H="":P=p1,    ;
                    SUM_D:H="":P=p2,    ;
                    NOTE:H="":P=p3      ;
                    FREEZE NOTE NOLGRID NOMENU NOAPPEND NODELETE NOCLEAR ;
                    WHEN Add_Inf()      ;
                    WINDOW Kassa_I IN WINDOW Kassa_E
    ELSE
      BROWSE FIELDS MM_TYPES.NAME:H="", ;
                    SUM_R:H="":P=p1,    ;
                    SUM_D:H="":P=p2,    ;
                    NOTE:R:H="":P=p3    ;
                    FREEZE NOTE NOLGRID NOMENU NOAPPEND NODELETE NOCLEAR ;
                    WHEN Add_Inf()      ;
                    WINDOW Kassa_I IN WINDOW Kassa_E
    ENDIF
    IF what_do = "List"
      IF READKEY() % 256 = 12
        what_do = "Exit"
      ENDIF
    ENDIF
    ON KEY
    ACTIVATE SCREEN
    @ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
  CASE what_do = "Clc_R"
    DO Clc_R
    what_do = "List"
  CASE what_do = "Clc_USD"
    DO Clc_USD
    what_do = "List"
  CASE what_do = "Arms"
    DO Arms_K
    what_do = "List"
  CASE what_do = "Seek_F"
    DO Seek_F
    what_do = "List"
  CASE what_do = "Seek_N"
    DO Seek_N
    what_do = "List"
  CASE what_do = "Seek_S"
    tmpRec = Seek_S()
    IF tmpRec # 0
      GO tmpRec
    ENDIF
    what_do = "List"
  CASE what_do = "Rest"
    DO Rest
    what_do = "List"
  CASE what_do = "Inc"
    DO Add_Rec && WITH "Inc"
    what_do = "List"
  CASE what_do = "Exp"
    DO Add_Rec && WITH "Exp"
    what_do = "List"
  OTHERWISE
    EXIT
  ENDCASE
ENDDO   

DEACTIVATE MENU M_Kassa
RELEASE MENU M_Kassa EXTENDED

RELEASE WINDOW Kassa_M
RELEASE WINDOW Kassa_E
RELEASE WINDOW Kassa_I
CLOSE DATA
RETURN

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                 �뤠� ���ଠ樨 � �����.                          �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Add_Inf

PRIVATE wd1, wd2, wd3, wd4
wd1= FSIZE("DOC_NUM", "VIEW")
wd2= FSIZE("DOC_DATE", "VIEW")
wd3= FSIZE("ARM_N", "ARM_LST")
wd4= FSIZE("USER_NAME", "USERS")

wd2 = LEFT(VIEW.DOC_NUM,1)+"-"+ALLTRIM(SUBSTR(VIEW.DOC_NUM,2))
wd1 = VIEW.DOC_DATE
IF .NOT. FOUND("ARM_LST")
  w3 = SPACE(wd3)
ELSE
  wd3 = ARM_LST.ARM_N
ENDIF

IF .NOT. FOUND("USERS")
  wd4 = IIF(WHO = 0, PADR("���!", wd4), SPACE(wd4))
ELSE
  wd4 = USERS.USER_NAME
ENDIF

ACTIVATE WINDOW Kassa_M BOTTOM
@ WROWS()-2,  2 SAY "��� "
@ WROWS()-2, 19 SAY wd1 COLOR (SCHEME(14,2))
@ WROWS()-1,  2 SAY "����� ���㬥��"
@ WROWS()-1, 19 SAY wd2 COLOR (SCHEME(14,2))
@ WROWS()-2, 40 SAY "����祥 ����"
@ WROWS()-2, 55 SAY wd3 COLOR (SCHEME(14,2))
@ WROWS()-1, 40 SAY "����� "
@ WROWS()-1, 55 SAY wd4 COLOR (SCHEME(14,2))

IF .NOT. EMPTY(KURS_FL)
  WAIT "���� "+ALLTRIM(STR(KURS_FL,15,3)) WINDOW NOWAIT
ENDIF
ACTIVATE WINDOW (ALIAS())

RETURN .T.

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                        ������ ���⪠.                               �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Rest
PRIVATE rc, sel_sav, s_r, s_d

sel_sav = SELECT()
SELECT VIEW
rc = RECNO()

GO TOP
IF BOF() .OR. EOF()
  SELECT (sel_sav)
  RETURN
ENDIF

DEFINE WINDOW W_Hide FROM 0, 0 TO 0, WCOLS("") NONE COLOR SCHEME 13
SHOW WINDOW W_Hide

s_r = KASSA_S.SUM_R_S
s_d = KASSA_S.SUM_D_S

SCAN WHILE RECNO() # rc
  s_r = s_r+SUM_R
  s_d = s_d+SUM_D
ENDSCAN

PRIVATE ln, wd, t, t1

*            ���⮪ � ���� �� ��.��.��, ��:��
*�������������������������������������������������������������Ŀ
*�                      �       �㡫�        �      USD        �
*�������������������������������������������������������������Ĵ
*� �� ��.��.����, ��:�� � 999 999 999 999.99 � 99 999 999.9999 �
*�������������������������������������������������������������Ĵ
*� ����騩              � 999 999 999 999.99 � 99 999 999.9999 �
*���������������������������������������������������������������
*                  ������ ���� �������

wd = FLOOR(DOC_TIME/60)
ln = RIGHT(STR(100+wd%60,3),2)
wd = FLOOR(wd/60)
ln = ", "+RIGHT(STR(100+wd%60,3),2)+":"+ln

t  = "���⮪ � ����"
t1 = TRANSFORM(DOC_DATE,"@D ")+ln

ln = 9
wd = 65
DO D_W WITH "Rest", ln, wd, t, 0
@ 1, 0 SAY ""
TEXT
 �������������������������������������������������������������Ŀ
 �                      �       �㡫�        �      USD        �
 �������������������������������������������������������������Ĵ
 � �� ��.��.����, ��:�� � 999 999 999 999.99 � 99 999 999.9999 �
 �������������������������������������������������������������Ĵ
 � ����騩              � 999 999 999 999.99 � 99 999 999.9999 �
 ���������������������������������������������������������������
    ������ ��-����� ( ��������: F1-��., ^F1-������� )
ENDTEXT
* ��������������������������������������������������������Ŀ
* �                    �      �㡫�      �      USD        �
* ��������������������������������������������������������Ĵ
* � �� ��.��.��, ��:�� �  999 999 999.99 � 99 999 999.9999 �
* ��������������������������������������������������������Ĵ
* � ����騩            �  999 999 999.99 � 99 999 999.9999 �
* ����������������������������������������������������������

@ 4, 6 SAY t1
@ 4,25 SAY s_r PICTURE "@Z 999 999 999 999.99"
@ 4,46 SAY s_d PICTURE "@Z 99 999 999.9999"
@ 6,25 SAY KASSA_S.SUM_R PICTURE "@Z 999 999 999 999.99"
@ 6,46 SAY KASSA_S.SUM_D PICTURE "@Z 99 999 999.9999"
@ 0, 0 SAY PADC(t,WCOLS()) COLOR SCHEME 5

DO WHILE .T.
  t = INKEY(0,"HM")
  IF t = 28
    = Cupury("�", q_rub, 0, 23)
    LOOP
  ENDIF
  IF t = 94
    = Cupury("$", q_usd, 0, -23)
    LOOP
  ENDIF
  EXIT
ENDDO

RELEASE WINDOW Rest
RELEASE WINDOW W_Hide

SELECT (sel_sav)
DEACTIVATE MENU M_Kassa

RETURN

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                      ���� ����� �����.                               �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Add_Rec
&& PARAMETER dr    && Inc - ��室, Exp - ��室

PRIVATE ln, wd, oper_n, oper_c, wk_name, wk_ind, wk_len, wk

*ln = IIF(dr = "Inc", 0, 1)
*wk_len = 0
*FOR wd = 1 TO oper_num
*  IF ln # oper_lst(wd, i_dir)
*    LOOP
*  ENDIF
*  wk_len = wk_len+1
*  DIMENSION wk_name(wk_len), wk_ind(wk_len)
*  wk_name(wk_len) = oper_nam(wd)
*  wk_ind(wk_len)  = wd
*ENDFOR

*ln = MIN(WROWS("")-5, wk_len+3)
*wd = LEN(oper_nam(1))+4

oper_n = ""
oper_c = 0
*DO WHILE .T.
*  oper_n = MAX(oper_n, 1)
*  oper_n = MIN(oper_n, wk_len)
*  @ 1, 1 MENU wk_name, wk_len, WROWS()-3
*  READ MENU TO oper_n
*  IF READKEY() % 256 = 12
*    RELEASE WINDOW O_List
*    RETURN
*  ENDIF
*  IF READKEY() % 256 = 15
*    RELEASE WINDOW O_List
*    EXIT
*  ENDIF
*ENDDO

DO Ch_Oper WITH oper_c, oper_n

IF EMPTY(oper_c)
  RETURN
ENDIF

*DO D_W WITH "O_List", ln, wd, ALLTRIM(oper_n), 0

PRIVATE s_sav, wk_dir, wk_rub, wk_bill
s_sav = SELECT()
SELECT 0
USE (base_path+"MM_TYPES") ORDER TAG CODE ALIAS W001 AGAIN
SEEK oper_c
ln = M_FUNC
wk_dir = M_DIR
wk_rub = M_RUB
wk_bill = C_BILL
USE
SELECT (s_sav)

IF .NOT. EMPTY(ln)
  &ln
ELSE
  DO Stnd_Add
ENDIF

RETURN

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                   �������� ����� �� ���ᠭ��.                         �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Stnd_Add
PRIVATE ln, wd, s, nt, ex, p1, p2, pic, dn, tit

*                ���������
*��������������������������������������Ŀ
*�    ��室   9 999 999.99 �� (F1)    �
*�    ���室 9 999 999.9999 USD (F1)    �
*� �� �ਬ�砭�� ��������������������Ŀ �
*� � ....:....1....:....2....:....3.. � �
*� ������������������������������������ �
*�   < OK Ctrl-W > < �⪠������ Esc >   �
*����������������������������������������

ln =  8
wd = 42
*tit = ALLTRIM(oper_nam(oper_n))
*tit = STRTRAN(tit, "\<")
DO D_W WITH "Stnd_Add", ln, wd, ALLTRIM(oper_n), 1

p1 = IIF(wk_dir = 0, "��室", "���室")

IF wk_rub
  p2 = "�� (F1)"
  pic = "@Z 9 999 999.99"
  p1 = p1+"  "
ELSE
  p2 = "USD (F1)"
  pic = "@Z 9 999 999.9999"
ENDIF

nt = SPACE(32)
ex = 1
s  = 0

@ 2, 28 SAY p2
@ 3, 3 TO 5, 38
@ 3, 5 SAY " �ਬ�砭�� "

@ 2, 6 SAY p1 GET s PICTURE pic WHEN Set_F1() VALID Clr_F1()
@ 4, 5 GET nt
@ 6, 5 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "
READ CYCLE
DO Clr_F1

RELEASE WINDOW Stnd_Add
IF ex # 2 .AND. s # 0
  s = IIF(wk_dir=0, s, -s)
  dn = Doc_Num("KASSA_M", "", arm, DATE())
  SELECT KASSA_S
  = LOCK()
  REPLACE SUM_R    WITH SUM_R+IIF(wk_rub, s, 0),  ;
          SUM_D    WITH SUM_D+IIF(wk_rub, 0, s)
  UNLOCK
  SELECT VIEW
  APPEND BLANK
  REPLACE DOC_NUM  WITH sys_char+STR(dn,6), ;
          DOC_DATE WITH DATE(),             ;
          DOC_TIME WITH SECONDS(),          ;
          CODE     WITH oper_c,             ;
          C_BILL   WITH wk_bill,            ;
          SUM_R    WITH IIF(wk_rub, s, 0),  ;
          SUM_D    WITH IIF(wk_rub, 0, s),  ;
          NOTE     WITH nt,                 ;
          WHO      WITH user,               ;
          WHERE    WITH arm
ENDIF

RETURN

**********************************************************
PROCEDURE Set_F1

IF LEFT(p2,3) = "USD"
  ON KEY LABEL F1 s = Cupury("$", q_usd, 0, 23)
ELSE
  ON KEY LABEL F1 s = Cupury(" ", q_rub, 0, 23)
ENDIF

RETURN .T.

**********************************************************
PROCEDURE Clr_F1

ON KEY LABEL F1

RETURN .T.

**********************************************************
PROCEDURE Clc_R
= Cupury(" ", q_rub, 0, 23)
DEACTIVATE MENU M_Kassa
RETURN

**********************************************************
PROCEDURE Clc_Usd
= Cupury("$", q_usd, 0, -23)
DEACTIVATE MENU M_Kassa
RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Seek_F       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                         ���� 蠡���� � ����.                          �
*�                                                                        �
*����������������������������������������������������������� 21.10.1998 ���
PROCEDURE Seek_F

PRIVATE rc_sav
*
*  ��६���� ���ﭨ� ��� ������樨
*

PRIVATE stat_type     && ��� ������樨: 0 - ���⠭�����;
                                        1 - �����;
                                        2 - BROWSE - ⠡���;
                                        3 - BROWSE - ᯨ᮪.
PRIVATE what_do       && ��� ०���.
PRIVATE menu_name     && ��� �ᨭ�஭���� ����.
PRIVATE last_mouse    && �६� ��᫥����� ������ ���ਭ�� ������.
PRIVATE win_name      && ��� ���� ( ���� ��� BROWSE ).

*
*   ������塞 ���祭�ﬨ ��६���� ���ﭨ�...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ����ঠ⥫쭠� ���� �ணࠬ��:
*

*     ���⥪��� ���� � ���� �ਬ�砭��
*����������������������������������������������Ŀ
*� �� ������ ��� ���᪠ ���������������������Ŀ �
*� � ....:....!....:....!....:....!....:....! � �
*� �������������������������������������������� �
*� �� ��ࠬ���� ���᪠ ����������������������Ŀ �
*� � [ ] ���� �� ���� � ��砫�              � �
*� � [ ] ���� �� ⥪�饩 ��ப�              � �
*� � [ ] �����஢��� ॣ����                 � �
*� �������������������������������������������� �
*�        < OK Ctrl-W > < �⪠������ Esc >      �
*������������������������������������������������

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 12, 50, "���⥪��� ���� � ���� �ਬ�砭��"
DO Sun_Bord WITH  2,  3,  4, 46, " ������ ��� ���᪠ "
DO Sun_Bord WITH  5,  3,  9, 46, " ��ࠬ���� ���᪠ "
*------------------------------------------------------------------------
*      ���� ����� ������
*
@ 3, 5 EDIT seek_shab SIZE 1, 40 PICTURE "@K"
@ 6, 5 GET seek_back PICTURE "@*C ���� �� ���� � ��砫�"
@ 7, 5 GET seek_rest PICTURE "@*C ���� �� ⥪�饩 ��ப�"
@ 8, 5 GET case_ign  PICTURE "@*C �����஢��� ॣ����"

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE

IF ex = 1 .AND. .NOT. EMPTY(seek_shab)
  *
  * ��ࠡ��뢠�� ����
  PRIVATE v_shab
  rc_sav = RECNO()
  v_shab = IIF(case_ign, SYS(15, lwr, seek_shab), seek_shab)
  IF .NOT. seek_rest
    IF seek_back
      GO BOTTOM
    ELSE
      GO TOP
    ENDIF
  ENDIF
  DO Wt_Mess WITH "�饬..."
  DO WHILE .NOT. BOF() .AND. .NOT. EOF()
    IF v_shab $ IIF(case_ign, SYS(15, lwr, NOTE), NOTE)
      EXIT
    ENDIF
    SKIP IIF(seek_back, -1, 1)
  ENDDO
  DO Wt_Mess
  IF EOF() .OR. BOF()
    IF BETWEEN(rc_sav, 1, RECCOUNT())
      GO rc_sav
    ENDIF
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"������ �� ������!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
  ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)PRETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Seek_N       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                          �த������� ���᪠.                           �
*�                                                                        �
*����������������������������������������������������������� 21.10.1998 ���
PROCEDURE Seek_N

PRIVATE rc_sav
*
* ��ࠡ��뢠�� ����
IF EMPTY(seek_shab)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� �� ������ 蠡��� ��� ���᪠!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF
PRIVATE v_shab
rc_sav = RECNO()
v_shab = IIF(case_ign, SYS(15, lwr, seek_shab), seek_shab)
DO Wt_Mess WITH "�饬..."
DO WHILE .NOT. BOF() .AND. .NOT. EOF()
  SKIP IIF(seek_back, -1, 1)
  IF v_shab $ IIF(case_ign, SYS(15, lwr, NOTE), NOTE)
    EXIT
  ENDIF
ENDDO
DO Wt_Mess
IF EOF() .OR. BOF()
  IF BETWEEN(rc_sav, 1, RECCOUNT())
     GO rc_sav
  ENDIF
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"������ �� ������!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
ENDIF

RETURN

*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Seek_S       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                      �ணࠬ�� ���᪠ �� �㬬�.                        �
*�                                                                        �
*����������������������������������������������������������� 04/30/1999 �ͼ
PROCEDURE Seek_S
PRIVATE d0, d1, tmpSum, TmpMoney, retValue
d0 = DATE()
d1 = DATE()
tmpSum   = 0
TmpMoney = 1
retValue = 0
*
*  ��६���� ���ﭨ� ��� ������樨
*

PRIVATE stat_type     && ��� ������樨: 0 - ���⠭�����;
                                        1 - �����;
                                        2 - BROWSE - ⠡���;
                                        3 - BROWSE - ᯨ᮪.
PRIVATE what_do       && ��� ०���.
PRIVATE menu_name     && ��� �ᨭ�஭���� ����.
PRIVATE last_mouse    && �६� ��᫥����� ������ ���ਭ�� ������.
PRIVATE win_name      && ��� ���� ( ���� ��� BROWSE ).

*
*   ������塞 ���祭�ﬨ ��६���� ���ﭨ�...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ����ঠ⥫쭠� ���� �ணࠬ��:
*

*���������������������������������������������������������������Ŀ
*� ��砫쭠� ��� ��.��.���� (F1)  ����筠� ��� ��.��.���� (F1) �
*�                                                               �
*�        �㬬� 99 999 999.9999    ����� ( ) �� ( ) USD        �
*�                                                               �
*�               < OK Ctrl-W > < �⪠������ Esc >                �
*�����������������������������������������������������������������

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 8, 67, "���� �� �㬬�"

*------------------------------------------------------------------------
*      ���� ����� ������
*
@ 2, 3 SAY "��砫쭠� ���            (F1)  ����筠� ���            (F1)"
@ 4, 10 SAY "�㬬�" GET tmpSum PICTURE "@Z 99 999 999.9999"
@ 4, 35 SAY "�����"
@ 4, 42 GET tmpMoney PICTURE "@*RH ��; USD"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "
@ 2, 18 GET d0 PICTURE "@D" WHEN Set_F_1(1) VALID Clr_F1()
@ 2, 49 GET d1 PICTURE "@D" WHEN Set_F_1(2) VALID Clr_F1()

READ CYCLE VALID Tst_Blank()

IF ex = 1 .AND. tmpSum # 0
  DO TmpSeek WITH d0, d1, tmpSum, tmpMoney
  retValue = ViewList()
ENDIF

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN retValue

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Set_F_1      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                       ��⠭���� ��ࠡ��稪� F1.                        �
*�                                                                        �
*����������������������������������������������������������� 04/30/1999 ���
PROCEDURE Set_F_1
PARAMETERS v

IF v = 1
  ON KEY LABEL F1 DO Int_D WITH 1
ELSE
  ON KEY LABEL F1 DO Int_D WITH 2
ENDIF
RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Clr_F1       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                        ���⪠ ��ࠡ��稪� F1.                         �
*�                                                                        �
*����������������������������������������������������������� 04/30/1999 ���
PROCEDURE Clr_F1

ON KEY LABEL F1

RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Int_D        ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                            �맮� ���������.                            �
*�                                                                        �
*����������������������������������������������������������� 30.04.1999 ���
PROCEDURE Int_D
PARAMETERS v

IF v = 1
  d0 = Get_Date(d0)
  SHOW GETS && d0
ELSE
  d1 = Get_Date(d1)
  SHOW GETS && d1
ENDIF

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Tst_Blank    ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                    �஢�ઠ ���४⭮�� ���ࢠ��.                    �
*�                                                                        �
*����������������������������������������������������������� 30.04.1999 ���
PROCEDURE Tst_Blank

PRIVATE mss

IF ex # 1
  RETURN .T.
ENDIF

IF d0 > d1
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��砫쭠� ��� ����� ����筮�? ��� �� �뢠��!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� TmpSeek      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                            ������ �� �㬬�.                            �
*�                                                                        �
*����������������������������������������������������������� 14.02.2002 ���
PROCEDURE TmpSeek
PARAMETERS d0, d1, tmpSum, tmpMoney

PRIVATE tmpName, s_sav, r_sav, tmpRec

s_sav = SELECT()
SELECT 0
tmpName = SYS(3)
CREATE DBF (tmpo_path+tmpName) ( LINK N(10) )
USE (tmpo_path+tmpName) ALIAS TMP_LIST
SELECT VIEW
r_sav = RECNO()
IF .NOT. SEEK(DTOS(d0))
  tmpRec = RECNO(0)
  IF BETWEEN(tmpRec, 1, RECCOUNT())
    GO tmpRec
  ENDIF
ENDIF
SCAN REST WHILE DOC_DATE <= d1
  IF tmpMoney = 1
    IF SUM_R # tmpSum
      LOOP
    ENDIF
  ELSE
    IF SUM_D # tmpSum
      LOOP
    ENDIF
  ENDIF
  SELECT TMP_LIST
  APPEND BLANK
  REPLACE LINK WITH RECNO("VIEW")
  SELECT VIEW
ENDSCAN

IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF

SELECT (s_sav)

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� ViewList     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                          ��ᬮ�� ����������.                          �
*�                                                                        �
*����������������������������������������������������������� 14.02.2002 ���
PROCEDURE ViewList
PARAMETERS tot_r, tot_d

PRIVATE tmpMode, tmpRet

tmpRet = 0

tmpMode = TYPE("tot_r") = "N"

*
*  ��६���� ���ﭨ� ��� ������樨
*

PRIVATE stat_type     && ��� ������樨: 0 - ���⠭�����;
                                        1 - �����;
                                        2 - BROWSE - ⠡���;
                                        3 - BROWSE - ᯨ᮪.
PRIVATE what_do       && ��� ०���.
PRIVATE menu_name     && ��� �ᨭ�஭���� ����.
PRIVATE last_mouse    && �६� ��᫥����� ������ ���ਭ�� ������.
PRIVATE win_name      && ��� ���� ( ���� ��� BROWSE ).
PRIVATE s_sav         && ����� ࠡ�祩 ������ ��� ��࠭���� � ����⠭�������!

*
*   ������塞 ���祭�ﬨ ��६���� ���ﭨ�...
*
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ���뢠�� 䠩�� �.�., ��⠭�������� �裡 � �. �.
*
s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT 0
USE (base_path+"ARM_LST") SHARED AGAIN ALIAS A_20214 ORDER TAG ARM_C
SELECT 0
USE (base_path+"PERSONS") SHARED AGAIN ALIAS P_20214 ORDER TAG CODE
SELECT 0
USE (base_path+"MM_TYPES") SHARED AGAIN ALIAS O_20214 ORDER TAG CODE
SELECT 0
USE (base_path+"KASSA_M") SHARED AGAIN ALIAS K_20214
SET RELATION TO WHO INTO P_20214
SET RELATION TO WHERE INTO A_20214 ADDITIVE
SET RELATION TO CODE INTO O_20214 ADDITIVE
SELECT TMP_LIST
SET RELATION TO LINK INTO K_20214
GO TOP

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

IF .NOT. tmpMode
  DEFINE PAD Choose OF (menu_name) PROMPT "�����" KEY Ctrl-A, "Enter"
  ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"
ENDIF

DEFINE PAD Exit OF (menu_name) PROMPT IIF(tmpMode, "��室", "�⪠������") KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
wd = 62
*��Ą�������č�������Ē�� ����樨���đ㬬� (��)��đ㬬� (USD)�
*���.��.���� NNNNNNN ....:....!....:. 99_999_999.99 999_999.9999�
*����������������������������������������������������������������
*��������������������������������������������������������������Ŀ
*�  ��� ....:....!....:.   ����� ....:....!....:....!....:...  �
*�         �ਬ�砭�� ....:....!....:....!....:....!..          �
*����������������������������������������������������������������

DO D_Wins WITH ln, wd, "���ᮢ� ����樨", 0, 2
@ 1, 2 SAY "�Ą�������č�������Ē�� ����樨���đ㬬� (��)��đ㬬� (USD)"
@ WROWS()-3,  4 SAY "���"
@ WROWS()-3, 27 SAY "�����"
@ WROWS()-2, 11 SAY "�ਬ�砭��"
IF tmpMode
  @ WROWS()-4, 38 SAY tot_r PICTURE "99 999 999.99"
  @ WROWS()-4, 52 SAY tot_d PICTURE "999 999.9999"
ENDIF
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - ����
*
    BROWSE FIELDS K_20214.DOC_DATE:H="",  ;
                  K_20214.DOC_NUM:H="",   ;
                  O_20214.NAME:H="",      ;
                  K_20214.SUM_R:H="":P="@Z 99 999 999.99", ;
                  K_20214.SUM_D:H="":P="@Z 999 999.9999",  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      IF .NOT. tmpMode
        what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
      ELSE
        what_do = "Exit"
      ENDIF
    ENDIF

  CASE what_do = "Choose"    && �롮�

    tmpRet = LINK
    EXIT

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   �����⭥�쪮 (�����⭥�쪮!) ����뢠�� DBF-䠩��.
*
SELECT A_20214
USE
SELECT P_20214
USE
SELECT O_20214
USE
SELECT K_20214
USE
SELECT TMP_LIST
tmpMode = DBF()
USE
DELETE FILE (tmpMode)
SELECT (s_sav)

RETURN tmpRet

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Draw_Foot    ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                   ���㥬 �������⥫��� ���ଠ��.                    �
*�                                                                        �
*����������������������������������������������������������� 20.10.1998 ���
PROCEDURE Draw_Foot

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� �����, ��㥬...
*
@ WROWS()-3,  8 SAY A_20214.ARM_N
@ WROWS()-3, 34 SAY SPACE(28)
@ WROWS()-3, 34 SAY ALLTRIM(P_20214.FAMILY)+" "+LEFT(P_20214.NAME,1)+" "+LEFT(P_20214.S_NAME,1)
@ WROWS()-2, 22 SAY K_20214.NOTE
ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.