*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                  ��｀�� � ��ぎ砒モ�覓� ぅ�譯���.                      �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Rp_Money

PRIVATE c_qnt
DIMENSION c_qnt(1)
c_qnt = 0

*
*  �ムガキ�襯 甌痰�鎰�� か� ��※�∽┬
*

PRIVATE stat_type   && �┓ ��※��罔�: 0 - �メ�����珥�覃;
                                        1 - ゛���;
                                        2 - BROWSE - ��゛���;
                                        3 - BROWSE - 甎�甌�.
PRIVATE what_do       && ��� 爛Θ��.
PRIVATE menu_name     && ��� �瓱�縲������ �キ�.
PRIVATE last_mouse    && �爛�� ��甄イ�ィ� ����皋� Бム┃�� ������.
PRIVATE win_name      && ��� ���� ( ���� か� BROWSE ).

*
*   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
*
stat_type  = 3
what_do    = "list"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

PRIVATE bill_n, bill_c, cnt

cnt = 0
USE CORRBILL ORDER TAG ORD
SCAN FOR CODE # 2
  cnt = cnt+1
  DIMENSION bill_n(cnt), bill_c(cnt)
  bill_n(cnt) = NAME
  bill_c(cnt) = CODE
ENDSCAN
IF cnt = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��� �� �牀痰� �ギ≦�え�� �爛Δ� ��▲痰� �メ���讓� ��玻. 瘍モ��!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  RETURN
ENDIF
SET ORDER TO TAG CODE

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Corr OF (menu_name) PROMPT "��玻オ罔�" KEY Ctrl-A, "Enter"
ON SELECTION PAD Corr OF (menu_name) Do Sw_Mode WITH "corr"

DEFINE PAD Add  OF (menu_name) PROMPT "����※碎" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) Do Sw_Mode WITH "add"

DEFINE PAD Del OF (menu_name) PROMPT "�����碎" KEY Ctrl-A, "Del"
ON SELECTION PAD Del OF (menu_name) Do Sw_Mode WITH "del"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*
PRIVATE start, current, wd_rest

SELECT 0
USE USERS ORDER TAG USER_CODE
SEEK user
STORE USER_MONEY TO start, current

USE PARMS

SELECT 0
USE RP_MONEY ORDER TAG PREV
SET RELATION TO CORR_BILL INTO CORRBILL

SEEK user
SCAN REST WHILE WHO_REP = user
  current = current+TOTAL_R
ENDSCAN
SEEK user

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd, tit, pic1, pic2, pic3
ln = 100          && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = IIF(SET("DATE") = "ON", 11, 9)+ ;
     FSIZE("PREV_NUM")+1+FSIZE("NAME","CORRBILL")+1+ ;
     2*FSIZE("TOTAL_R")+2

wd_rest = MIN(WCOLS("")-6-wd, FSIZE("NOTE"))
wd = wd+wd_rest
pic1 = "@Z "+REPLICATE("9", FSIZE("TOTAL_R")-3)+".99"
pic2 = "@S"+ALLTRIM(STR(wd_rest))+" "
pic3 = "@S"+ALLTRIM(STR(FSIZE("PREV_NUM")))+" "
DO D_Wins WITH ln, wd, "�砒モ �ムイ ＜紕��皀爬ォ", 1, 1
tit = PADC("����", IIF(SET("DATE") = "ON", 10, 8))+" "+ ;
      PADC("N ��.",FSIZE("PREV_NUM"))+" "+ ;
      PADC("��玻. 瘍モ",FSIZE("NAME","CORRBILL"))+" "+ ;
      PADC("踸絎�",FSIZE("TOTAL_R"))+" "+  ;
      PADC("��痳��",FSIZE("TOTAL_R"))+" "+  ;
      PADC("踸�ョ��┘", wd_rest)

@ 1, 2 SAY tit
DO Show_Foot
what_do = "list"

DO WHILE .T.

  DO CASE

  CASE what_do = "list"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins   Do Sw_Mode WITH "add"
    ON KEY LABEL Del   Do Sw_Mode WITH "del"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS PREV_DAT:H="",  ;
                  f001 = LEFT(PREV_NUM,1)+ALLTRIM(SUBSTR(PREV_NUM,2)):H="":P=pic3, ;
                  CORRBILL.NAME:H="", ;
                  f002 = IIF(TOTAL_R>0, TOTAL_R, 0):H="":P=pic1,  ;
                  f003 = IIF(TOTAL_R<0,-TOTAL_R, 0):H="":P=pic1,  ;
                  NOTE:H="":R:P=pic2 FREEZE NOTE ;
           KEY user     ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "list"
      what_do = IIF(READKEY() % 256 = 12, "exit", "corr")
    ENDIF

  CASE what_do = "corr"    && ��玻オ罔�
  
    DO C_C_Line WITH .F.
    DO Show_Foot
    what_do = "list"
  
  CASE what_do = "add"    && ����←キ┘
  
    DO C_C_Line WITH .T.
    DO Show_Foot
    what_do = "list"

  CASE what_do = "del"    && ����キ┘
    
    current = current-TOTAL_R
    DELETE
    SKIP
    DO Show_Foot
    what_do = "list"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

CLEAR WINDOWS
CLOSE DATABASES
RELEASE MENU (menu_name) EXTENDED

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                       蹍罐ゃ�� �襤��� 痺��.                          �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Show_Foot

PRIVATE t1, wd, t2, w_sav

w_sav = WOUTPUT()

ACTIVATE WINDOW (win_name+"_M") BOTTOM
t1 = "�祠�� �� ＜紕. "+ALLTRIM(STR(start,12,2))
t2 = "�痰�皰� "+ALLTRIM(STR(current,12,2))
wd = FLOOR(WCOLS()/2)-3
t1 = PADR(t1, wd)
t2 = PADR(t2, wd)
@ WROWS()-2, 3 SAY t1+t2

IF EMPTY(w_sav)
  ACTIVATE SCREEN
ELSE
  ACTIVATE WINDOW (w_sav)
ENDIF

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�          蹍罐ゃ�� 甌Г���� ��※��罔����� 痼イ� か� ゛����.           �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Prp_Nav_2

ACTIVATE SCREEN
@ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
ON KEY LABEL LeftMouse DO Mouse

RETURN


*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                 蹍罐ゃ�� 甌Г����/��玻オ罔� 痰牀��.                  �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE C_C_Line
PARAMETERS is_new

PRIVATE tmp_point, tmp_sum, ex, tmp_note, tmp_dir, i, n_d

IF is_new 
  tmp_point = 1
  tmp_sum   = 0
  ex        = 1
  tmp_note  = SPACE(FSIZE("NOTE", "RP_MONEY"))
  tmp_dir   = 1
ELSE
  tmp_point = 1
  FOR i = 1 TO ALEN(bill_c)
    IF RP_MONEY.CORR_BILL = bill_c(i)
      tmp_point = i
      EXIT
    ENDIF
  ENDFOR
  tmp_sum   = ABS(RP_MONEY.TOTAL_R)
  ex        = 1
  tmp_note  = RP_MONEY.NOTE
  tmp_dir   = IIF(RP_MONEY.TOTAL_R >= 0, 1, 2)
ENDIF

*
*  �ムガキ�襯 甌痰�鎰�� か� ��※�∽┬
*

PRIVATE stat_type   && �┓ ��※��罔�: 0 - �メ�����珥�覃;
                                        1 - ゛���;
                                        2 - BROWSE - ��゛���;
                                        3 - BROWSE - 甎�甌�.
PRIVATE what_do       && ��� 爛Θ��.
PRIVATE menu_name     && ��� �瓱�縲������ �キ�.
PRIVATE last_mouse    && �爛�� ��甄イ�ィ� ����皋� Бム┃�� ������.
PRIVATE win_name      && ��� ���� ( ���� か� BROWSE ).

*
*   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃...
DO Prp_Nav_2

DO D_Win_N WITH 15, 41, "�������皀 ゛���..."

@  2, 2 SAY "               敖陳陳陳陳陳陳陳跳    "
@  3, 2 SAY "    ��玻. 瘍モ �                �    "
@  4, 2 SAY "               塒様様様様様様様夕    "
@  5, 2 SAY "    敖 �����←キ┘ 陳陳陳陳陳陳朕    "
@  6, 2 SAY "    �  ( ) 踸絎�   ( ) ��痳��  �    "
@  7, 2 SAY "    青陳陳陳陳陳陳陳陳陳陳陳陳陳�    "
@  8, 2 SAY "        �祠�� 9999999.99 (F1)        "
@  9, 2 SAY "                                     "
@ 10, 2 SAY " 踸�ョ��┘ ....:....!....:....!.... "
@ 11, 2 SAY "                                     "
@ 12, 2 SAY "  < OK Ctrl-W > < �皖���碎瘴 Esc >   "
@ 13, 2 SAY "                                     "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
IF is_new
  DO Part_1
  DO Part_2
ELSE
  DO Part_2
  DO Part_1
ENDIF

READ CYCLE VALID Tst_Sum()

IF ex = 1
  tmp_point = bill_c(tmp_point)
  tmp_sum   = IIF(tmp_dir = 1, tmp_sum, -tmp_sum)
  IF is_new
    SELECT PARMS
    n_d = LAST_REP0+1
    REPLACE LAST_REP0 WITH n_d
    SELECT RP_MONEY
    n_d = sys_char+STR(n_d,5)
    current = current+tmp_sum
    APPEND BLANK
    REPLACE PREV_NUM  WITH n_d,      ;
            PREV_DAT  WITH DATE(),   ;
            PREV_TIM  WITH SECONDS(),;
            WHO_REP   WITH user,     ;
            WHERE_REP WITH arm,      ;
            CORR_BILL WITH tmp_point,;
            TOTAL_R   WITH tmp_sum,  ;
            NOTE      WITH tmp_note
  ELSE
    current = current+tmp_sum-TOTAL_R
    REPLACE CORR_BILL WITH tmp_point,;
            TOTAL_R   WITH tmp_sum,  ;
            NOTE      WITH tmp_note
  ENDIF
ENDIF
POP KEY
RELEASE WINDOW (win_name)
RETURN

***************************************************************************
PROCEDURE Part_1   && �ム��� ����※�� ゛����

@  2, 17 GET tmp_point FROM bill_n PICTURE "@^ " SIZE 1, 18
@  6,  9 GET tmp_dir PICTURE "@*RH 踸絎�  ;��痳��"

RETURN

***************************************************************************
PROCEDURE Part_2   && �皰��� ����※�� ゛����

@  8, 16 GET tmp_sum  PICTURE "@Z " SIZE 1, 10 WHEN Set_F1() VALID Tst_Sum()
@ 10, 14 GET tmp_note SIZE 1, 24
@ 12,  4 GET ex       PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

RETURN

***************************************************************************
PROCEDURE Tst_Sum

PRIVATE mss

IF ex = 2 .OR. READKEY() % 256 = 12
  ON KEY LABEL F1
  RETURN .T.
ENDIF

IF tmp_sum = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�祠�� ��↓� �祀�? ��� �牀痰� 磴���皀 轤� 痰牀��!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF tmp_sum < 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�����←キ┘ あ└キ�� ぅ�ィ ��爛ぅ�錺矚� 錢�� � �爛るゃ薀� ���� ゛����!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

ON KEY LABEL F1

RETURN .T.

***************************************************************************
PROCEDURE Set_F1

ON KEY LABEL F1 tmp_sum = Cupury(" ", c_qnt, 0, 23)

RETURN .T.
