*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�         蹍������ ��玻オ罔�/甌Г���� �矗─竄�� ��┘���.               �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Cus_Bil
PARAMETERS cd, nm, stop_corr

*
*  �ムガキ�襯 甌痰�鎰�� か� ��※��罔�
*

PRIVATE stat_type     && �┓ ��※��罔�: 0 - �メ�����珥�覃;
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

*------------------------------------------------------------------------
*    ��ぅ爨�皀�讚�� ��痰� �牀������:
*

*
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�      ���祚�皀�� ....:....!....:....!....:....!....:....!....:....!       �
*�                                                                          �
*�    �むメ ....:....!....:....!....:....!....:....!....:....!....:....!    �
*�                                                                          �
*��カ. ....:....!....:. �/� ....:....!....:....! �/� N ....:....!....:....! �
*�                                                                          �
*�             � ����� ....:....!....:....!....:....!....:....!             �
*�                                                                          �
*�          ��� ....:....!....:....!        ��牀� ....:....!....:.          �
*�                                                                          �
*�  ��� ....:....!..   ��� ....:....!....:.   ����� ....:   ���� ....:...   �
*楽� 踸�ョ��┘ 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕�
*崖....:....!....:....!....:....!....:....!....:....!....:....!....:....!..崖
*崖                                                                        崖
*崖                   < OK Ctrl-W > < �皖���碎瘴 Esc >                     崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳抒
*� �皰 甌Г��                                 ���� 甌Г����                 �
*� �皰 ��玻オ皋牀���                          ���� ��玻オ罔�                �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
PRIVATE f01, f02, f03, f04, f05, f06, f07, f08, f09, f10, f11, f12, tmpKPP, ex
PRIVATE sel, us, new
PRIVATE c01, c02, c03, c04, c11, c12

sel = SELECT()
**************************
SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS P_2510 AGAIN

c01 = ""
c02 = ""
c03 = {}
c04 = {}
c11 = " "
c12 = " "

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS ACC_2510 AGAIN
SET RELATION TO WHO_CORR INTO P_2510
SET RELATION TO WHO INTO P_2510 

IF SEEK (cd)
  c01 = ACC_2510.WHO
  c02 = ACC_2510.WHO_CORR
  c03 = DATE_ON
  c04 = DATE_CORR
ENDIF
USE

SELECT P_2510
IF SEEK (c01)
  c11 = ALLTRIM(P_2510.FAMILY)+" "+LEFT(P_2510.NAME,1)+". "+ ;
              LEFT(P_2510.S_NAME,1)+"."
ENDIF
IF SEEK (c02)              
  c12 = ALLTRIM(P_2510.FAMILY)+" "+LEFT(P_2510.NAME,1)+". "+ ;
              LEFT(P_2510.S_NAME,1)+"."
ENDIF              
USE

********************************
IF USED("CUS_BIL")
  us = .T.
  SELECT CUS_BIL
ELSE
  us = .F.
  SELECT 0
  USE CUS_BIL ORDER TAG CUS_CODE SHARED AGAIN
ENDIF

ex   = 1
IF SEEK(cd)
  new = .F.
  f01 = CUS_NAME
  f02 = ADDRESS
  f03 = PHONE
  f04 = ACC_BILL
  f05 = BANK
  f06 = CORR_BILL
  f07 = BIK
  f08 = CITY
  f09 = INN
  tmpKPP = KPP
  f10 = OKOHX
  f11 = OKPO
  f12 = NOTE
ELSE
  new = .T.
  f01 = SPACE(FSIZE("CUS_NAME"))
  IF TYPE("nm") = "C"
    f01 = PADR(nm, FSIZE("CUS_NAME"))
  ENDIF
  f02 = SPACE(FSIZE("ADDRESS"))
  f03 = SPACE(FSIZE("PHONE"))
  f04 = SPACE(FSIZE("ACC_BILL"))
  f05 = SPACE(FSIZE("BANK"))
  f06 = SPACE(FSIZE("CORR_BILL"))
  f07 = SPACE(FSIZE("BIK"))
  f08 = SPACE(FSIZE("CITY"))
  f09 = SPACE(FSIZE("INN"))
  f10 = SPACE(FSIZE("OKOHX"))
  f11 = SPACE(FSIZE("OKPO"))
  f12 = SPACE(FSIZE("NOTE"))
  tmpKPP = SPACE(FSIZE("KPP"))
ENDIF
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 22, 78, "�矗─竄� ��┘���"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@  2,  8 SAY "���祚�皀��" GET f01
@  4,  6 SAY "�むメ" GET f02
@  6,  2 SAY "�カ." GET f03
@  6, 24 SAY "�/�" GET f06
@  6, 49 SAY "�/�" GET f04
@  8, 15 SAY "� �����" GET f05
@ 10, 12 SAY "���" GET f07
@ 10, 44 SAY "��牀�" GET f08
@ 12,  4 SAY "���" GET f09
@ 12, 23 SAY "���" GET tmpKPP
@ 12, 46 SAY "�����" GET f10
@ 12, 60 SAY "����" GET f11
DO Sun_Bord WITH 13, 2, 15, 75
@ 13, 4 SAY " 踸�ョ��┘ "
@ 14,  3 GET f12
DO Sun_Bord WITH 16, 2, 19, 75
@ WROWS()-5,  3 SAY "�皰 甌Г��         "+c11
@ WROWS()-4,  3 SAY "�皰 ��玻オ皋牀���  "+c12
@ WROWS()-5, 47 SAY "���� 甌Г����   "+DTOC(c03)
@ WROWS()-4, 47 SAY "���� ��玻オ罔�  "+DTOC(c04)
IF stop_corr
  CLEAR GETS
  @ WROWS()-2, FLOOR(WCOLS()/2-3) GET ex PICTURE "@*HT \ OK "
  READ CYCLE
ELSE
  @ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "
  READ CYCLE

  IF ex = 1
    IF new
      APPEND BLANK
    ENDIF
    REPLACE CUS_CODE  WITH cd,  ;
            CUS_NAME  WITH f01, ;
            ADDRESS   WITH f02, ;
            PHONE     WITH f03, ;
            ACC_BILL  WITH f04, ;
            BANK      WITH f05, ;
            CORR_BILL WITH f06, ;
            BIK       WITH f07, ;
            CITY      WITH f08, ;
            INN       WITH f09, ;
            KPP       WITH tmpKPP, ;
            OKOHX     WITH f10, ;
            OKPO      WITH f11, ;
            NOTE      WITH f12, ;
            DATE_CORR  WITH DATE(),  ;
            WHO_CORR   WITH user,    ;
            WHERE_CORR WITH arm
  ENDIF
ENDIF
*------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
IF .NOT. us
  USE
ENDIF
SELECT (sel)

RETURN ex = 1
