*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� ViewList     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                      蹍甃�矗 甌ぅ爨━��� 甎�瓷�                       �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 07.09.2006 様�
PROCEDURE ViewList
PARAMETERS l_num

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
PRIVATE s_sav         && ���ム ��｀腑� �゛�痰� か� 甌縲��キ�� � 〓瘁����←キ��!

*
*   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
*
m.stat_type  = 3
m.menu_name  = PROGRAM()
m.last_mouse = 0
m.win_name   = PROGRAM()

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*

m.s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT 0
USE (m.base_path+"ARM_LST") SHARED AGAIN ALIAS ARM_6907 ORDER TAG ARM_C
SELECT 0
USE (m.base_path+"ACCOUNT") SHARED AGAIN ALIAS ACC_6907 ORDER TAG CUS_CODE
SELECT 0
USE (m.base_path+"LIST_TIT") SHARED AGAIN ALIAS TIT_6907 ORDER TAG LST_NUM
SELECT 0
USE (m.base_path+"STOCK") SHARED AGAIN ALIAS STO_6907 ORDER TAG CODE
SELECT 0
USE (m.base_path+"LIST_DET") SHARED AGAIN ALIAS DET_6907 ORDER TAG L_NAME
SET RELATION TO CODE INTO STO_6907
IF .NOT. SEEK(m.l_num, "TIT_6907")
  PRIVATE mss
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"���瓷� � ���ム�� "+ALLTRIM(STR(m.l_num))+"�� 痺薀痰≪モ..."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  USE IN DET_6907
  USE IN ARM_6907
  USE IN ACC_6907
  USE IN TIT_6907
  USE IN STO_6907
  SELECT (m.s_sav)
ENDIF
= SEEK(TIT_6907.CLI_CODE, "ACC_6907")
= SEEK(TIT_6907.LST_ARM, "ARM_6907")
*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd, tmpPic
m.ln = WROWS("")-5   && ����腑痰〓 ※え�諷 痰牀� BROWSE
m.wd = 69

DO D_Wins WITH m.ln, m.wd, "���甌� � "+ALLTRIM(STR(m.l_num))+ ;
               " ��� "+ALLTRIM(ARM_6907.ARM_N), 1, 0
@ 1, 1 SAY "��┘�� "+ACC_6907.CUS_NAME

IF TIT_6907.IS_USD
  @ 2, 2 SAY ;
  "陳陳陳���━キ����┘陳陳陳陳陳陳陳陳陳陳陳��爼�陳陳���-〓陳�キ� (� �)"
  *PPP nnnnnnnnnNnnnnnnnnNnnnnnnnnnNnnnnn ffffffff 999999 999999999999
  m.tmpPic = "9999999.9999"
ELSE
  @ 2, 2 SAY ;
  "陳陳陳���━キ����┘陳陳陳陳陳陳陳陳陳陳陳��爼�陳陳���-〓陳�キ� (�磧)"
  *PPP nnnnnnnnnNnnnnnnnnNnnnnnnnnnNnnnnn ffffffff 999999 999999999999
  m.tmpPic = "999999.99999"
ENDIF
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    m.statys_type = 3
    DO Prp_Nav_1
*    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE KEY STR(l_num, 6) ;
           FIELDS STO_6907.PREFIX:H="",		;
                  STO_6907.NAME:H="",		;
                  STO_6907.PRODUCER:H="",	;
                  DET_6907.QNT:H="",		;
                  tmpPr = IIF(TIT_6907.IS_USD, DET_6907.USD_PRICE, DET_6907.SAL_PRICE):P=m.tmpPic, ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

*  CASE m.what_do = "Choose"    && 蹍甃�矗 甎�瓷�

*    DO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*    m.what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   ���窶�皚キ讓� (���窶�皚キ讓�!) ���琺��ガ DBF-�����.
*
USE IN DET_6907
USE IN ARM_6907
USE IN ACC_6907
USE IN TIT_6907
USE IN STO_6907
SELECT (m.s_sav)

RETURN
