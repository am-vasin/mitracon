*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Set_Ord      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�               ����←キ┘ ���ム� �爬絎き��� �爐ム� � 腑�.               �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 16.12.2004 様�
PROCEDURE Set_Ord
PARAMETERS pFirm, pType, pDoc, pDate

*
* pFirm - 筥爼�;
* pType - 皋� ぎ�-��;
* pDoc  - ���ム ぎ�祠キ��;
* pDate - ���� ぎ�祠キ��.
*
PRIVATE tmpYear, s_sav, tmpDoc, tmpCus

IF m.pType # "�"
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���祠キ� �� 錢�錺矚� 腑���!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

m.tmpYear = Doc_Year(m.pDate)
USE (m.base_path+"INCMONEY") SHARED AGAIN ORDER TAG DOC_NUM ALIAS MON_04C16 IN 0
IF .NOT. SEEK(m.pFirm+"�"+m.tmpYear+m.pDoc, "MON_04C16")
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���祠キ� �� ���ぅ�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE IN MON_04C16
  RETURN
ENDIF

USE (m.base_path+"ACCOUNT") SHARED AGAIN ORDER TAG CUS_CODE ALIAS CUS_04C16 IN 0
= SEEK(MON_04C16.CUS_CODE, "CUS_04C16")
m.tmpCus = CUS_04C16.CUS_NAME
USE IN CUS_04C16
IF .NOT. Is_Ur(MON_04C16.CUS_CODE)
  PRIVATE mss
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"��┘�� "+ALLTRIM(m.tmpCus)
  mss(3) = CHR(0)+"  �� 釵┐�腑瓷�� ��罧. ��爼�珮ガ �爬絎き覃 �爐ム? [ �� ] [ �モ ]"
  mss(4) = ""
  IF Out_Mess(7, "mss") = 2
    USE IN MON_04C16
    RETURN
  ENDIF
ENDIF
IF LEFT(MON_04C16.ENTER_DOC, 1) = CHR(0)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��� 轤��� 腑�� �爬絎き覃 �爐ム 礒� 痲�爼�牀���!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE IN MON_04C16
  RETURN
ENDIF

IF Conf()
  m.s_sav = SELECT()
  SELECT MON_04C16
  IF MON_04C16.MONEY_R > 0
    m.tmpDoc = Doc_Num("INC_ORDER ", pFirm, m.arm, pDate)
  ELSE
    m.tmpDoc = Doc_Num("EXP_ORDER ", pFirm, m.arm, pDate)
  ENDIF
  REPLACE ENTER_DOC WITH CHR(0)+sys_char+STR(m.tmpDoc, 9)
  SELECT (m.s_sav)
ENDIF

USE IN MON_04C16

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Conf         ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                   ��も▲爨ぅ�┘ か� 甌Г���� ぎ�-��.                   �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 16.12.2004 陳�
PROCEDURE Conf
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�      �オ �� 痺��� 999 999 999.99 �� ��.��.����      �
*楽� ��┘�� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳審
*崖 ....:....!....:....!....:....!....:....!....:....!崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰�
*�          < OK Ctrl-W > < �皖���碎瘴 Esc >           �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 8, 57, "��も▲爐�皀 甌Г��┘ ぎ�祠キ��"
DO Sun_Bord WITH  3,  2,  5, 54, " ��┘�� "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 2, 8 SAY "�オ �� 痺��� "+TRANSFORM(MON_04C16.MONEY_R, "999 999 999.99")+  ;
           " �� "+DTOC(MON_04C16.DOC_DATE)
@ 4, 4 SAY m.tmpCus
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)

RETURN ex = 1
