*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� New_Sup      ��о�｀砒┴ ��むォ ��瓱�           06.07.97 11:32:05 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                  ����� �牀������ 甌Г���� ��痰�∫┴�.                  �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE New_Sup

*
*  �ムガキ�襯 甌痰�鎰�� か� ��※��罔�
*
PRIVATE tmpType

PRIVATE stat_type     && �┓ ��※��罔�: 0 - �メ�����珥�覃;
                                        1 - ゛���;
                                        2 - BROWSE - ��゛���;
                                        3 - BROWSE - 甎�甌�.
PRIVATE what_do       && ��� 爛Θ��.
PRIVATE menu_name     && ��� �瓱�縲������ �キ�.
PRIVATE last_mouse    && �爛�� ��甄イ�ィ� ����皋� Бム┃�� ������.
PRIVATE win_name      && ��� ���� ( ���� か� BROWSE ).
PRIVATE tmpINN, tmpKPP
m.tmpINN = ""
m.tmpKPP = ""
DO Get_INN WITH 0, m.tmpINN, m.tmpKPP

*
*   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ��ぅ爨�皀�讚�� ��痰� �牀������:
*

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�    ( ) ��≡ガ ���覃   ( ) �Бメ皀� ��� ���祚�皀��    �
*楽� ��� ��痰�∫┴� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕�
*崖 ....:....!....:....!....:....!....:....!....:....! 崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳抒
*�       ��� ....:....!..    ��� ....:....!....:.       �
*�           < OK Ctrl-W > < �皖���碎瘴 Esc >           �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

USE (base_path+"CLI_INFO") ORDER TAG CUS_CODE ALIAS C_3326 SHARED AGAIN
SELECT 0
USE (base_path+"CUSTOMER") ORDER TAG CODE
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_NAME
DO Use_Dummy
DO Use_Link
PRIVATE ex, nm_w, is_new, cntr
nm_w   = SPACE(FSIZE("CUS_NAME","ACCOUNT"))
is_new = 1
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 9, 58, "��痰�∫┴"
DO Sun_Bord WITH 3, 2, 5, 55, " ��� ��痰�∫┴� "
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 2,  7 GET is_new PICTURE "@*RH ��≡ガ ���覃  ;�Бメ皀� ��� ���祚�皀��"  ;
        VALID Get_Cus()
@ 4,  4 GET nm_w WHEN is_new = 1
@ 6,  9 SAY "���" GET m.tmpINN
@ 6, 29 SAY "���" GET m.tmpKPP
@ 7, 13 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID ex # 1 .OR. EMPTY(nm_w) .OR. ;
                 Tst_INN(IIF(is_new = 1, 0, ACCOUNT.CUS_CODE), m.tmpINN)

IF ex = 1 .AND. .NOT. EMPTY(nm_w)
  IF is_new = 1
    cntr = SPACE(16)
    SELECT ACCOUNT
    IF (sys_char = "A" .OR. sale_mode = "Mitin")
      tmpType = WhatFace()
      ex = Doc_Num("CUS_CODE", "", "", DATE())
      APPEND BLANK
      REPLACE SUBSYS   WITH sys_char, ;
              CUS_CODE WITH ex,   ;
              CUS_NAME WITH nm_w, ;
              DATE_ON  WITH DATE(),;
              MARK1    WITH tmpType,;
              WHO      WITH user, ;
              WHERE    WITH arm
    ELSE
      SET ORDER TO TAG EMP_NAME
      GO TOP
      IF EOF() .OR. BOF()
        PRIVATE mss
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"�� �瘍ム���� ≡� ����皰→�!"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        CLOSE DATABASES
        POP KEY
        RELEASE WINDOW (win_name)
        RETURN
      ENDIF
      tmpType = WhatFace()
      REPLACE CUS_NAME WITH nm_w, MARK1 WITH tmpType
    ENDIF
    SELECT C_3326
    APPEND BLANK
    REPLACE CUS_CODE WITH ex,  ;
            NAME     WITH nm_w,;
            COUNTRY  WITH cntr,;
            WHO      WITH user,;
            WHERE    WITH arm
  ENDIF
  DO Put_Inn WITH CUS_CODE, m.tmpINN, m.tmpKPP
  SELECT CUSTOMER
  IF .NOT. SEEK(ACCOUNT.CUS_CODE, "CUSTOMER")
    APPEND BLANK
    REPLACE CODE    WITH ACCOUNT.CUS_CODE,  ;
            B_NAME  WITH ACCOUNT.CUS_NAME,  ;
            L_NAME  WITH ACCOUNT.CUS_NAME,  ;
            DATE_ON WITH ACCOUNT.DATE_ON,   ;
            WHO_ADD WITH ACCOUNT.WHO,       ;
            WHERE_ADD WITH ACCOUNT.WHERE
  ENDIF
  REPLACE SUP WITH "*"
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
CLOSE DATABASES
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                         �襦�� 痼イ� ��┘�皰�.                          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 06.07.97 11:50:35 夕
PROCEDURE Get_Cus
PRIVATE mss, pl

IF is_new = 1
  RETURN .T.
ENDIF
SELECT ACCOUNT
pl = sys_char
IF .NOT. Custs("", .F., .F., .F., pl)
  is_new = 1
  SHOW GET is_new
  RETURN .T.
ENDIF

IF SEEK(ACCOUNT.CUS_CODE, "CUSTOMER") .AND. CUSTOMER.SUP # " "
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�皰� ��┘�� 礒� ���ョキ ��� ��痰�∫┴!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  is_new = 1
  SHOW GET is_new
  RETURN .T.
ENDIF

DO Get_INN WITH ACCOUNT.CUS_CODE, m.tmpINN, m.tmpKPP
nm_w = ACCOUNT.CUS_NAME
SHOW GET nm_w
SHOW GET tmpINN
SHOW GET tmpKPP


RETURN .T.
