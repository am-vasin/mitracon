*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� V_B_Req      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                   蹍甃�矗 矗ァ����┤ � ＜紕��皀爬�.                   �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 24.12.1998 様�
PROCEDURE V_B_Req
PARAMETERS wh

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
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*
* BUHG_REQ (BR_8C24)
*  団 LIST_TIT (LT_8C24)
*  団 ACCOUNT  (CUS_8C24)
*  団 PERSONS  (S_8C24)
*  青 PERSONS  (B_8C24)
*
s_sav = SELECT()
SELECT 0
DO Use_Dummy
SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS B_8C24 AGAIN
SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS S_8C24 AGAIN
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS CUS_8C24 AGAIN
SELECT 0
USE (base_path+"LIST_TIT") ORDER TAG LST_NUM ALIAS LT_8C24 AGAIN
SELECT 0
USE (base_path+"BUHG_REQ") ORDER TAG DOC_NUM ALIAS BR_8C24 AGAIN
IF TYPE("wh") = "C"
  SET ORDER TO TAG WHERE
ENDIF
GO BOTTOM
SET RELATION TO WHO_REQ INTO S_8C24 ADDITIVE
SET RELATION TO WHO_INP INTO B_8C24 ADDITIVE
SET RELATION TO CUS_CODE INTO CUS_8C24 ADDITIVE
SET RELATION TO LST_NUM INTO LT_8C24 ADDITIVE

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Print OF (menu_name) PROMPT "�ョ�碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Print OF (menu_name) DO Sw_Mode WITH "Print"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ��瘍モ ��Кム�� ����
*
*敖 ���ム 陳陳� ���� 陳陳 ��� 甎�瓷� 陳陳陳陳陳陳陳陳陳陳 �覩���. 朕
*�1234567890 ��.��.���� 12345678901234567890123456789012 ��.��.�����
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*敖 ��┘�� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*� 12345678901234567890123456789012345678901234567890              �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*敖 蹍��▲� 陳陳陳陳陳陳陳陳陳陳陳� ��瘁�� 陳陳陳陳陳陳陳陳陳陳陳朕
*� 123456789012345678901234 �. �.   123456789012345678901234 �. �. �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
PRIVATE ln, wd
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 65

DO D_Wins WITH ln, wd, "�爛｀����� � ＜紕��皀爬�", 0, 4
@ 1, 2 SAY "� ���ム 陳陳� ���� 陳陳 ��� 甎�瓷� 陳陳陳陳陳陳陳陳陳陳 �覩���. "
DO Sun_Bord WITH WROWS()-6, 1, WROWS()-4, WCOLS()-2, " ��┘�� "
DO Sun_Bord WITH WROWS()-3, 1, WROWS()-1, WCOLS()-2, " 蹍��▲� "
@ WROWS()-3, 36 SAY " ��瘁�� "
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE FIELDS BR_8C24.DOC_NUM:H="",  ;
                  BR_8C24.DOC_DATE:H="":P="@D", ;
                  LT_8C24.LST_NAME:H="",  ;
                  TERM00 = IIF(EMPTY(BR_8C24.DATE_INP), SPACE(10), DTOC(BR_8C24.DATE_INP)):H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Print")
    ENDIF

  CASE what_do = "Print"    && 蹍甃�矗 甎�瓷�

    DO P_B_Req WITH BR_8C24.DOC_NUM, BR_8C24.DOC_DATE
    what_do = "List"

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
SELECT B_8C24
USE
SELECT S_8C24
USE
SELECT CUS_8C24
USE
SELECT LT_8C24
USE
SELECT BR_8C24
USE

SELECT (s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Draw_Foot    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                   ��痺ガ ぎ�����皀�讚竡 ┃筮爼�罔�.                    �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 20.10.1998 陳�
PROCEDURE Draw_Foot
PRIVATE nm_s, nm_k

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� Гメ�, 爬痺ガ...
*
IF BR_8C24.CUS_CODE = 0
  @ WROWS()-5, 3 SAY SPACE(50)
ELSE
  @ WROWS()-5, 3 SAY CUS_8C24.CUS_NAME
ENDIF

IF BR_8C24.WHO_REQ = 0
  nm_s = "���!"
ELSE
  nm_s = ALLTRIM(S_8C24.FAMILY)+" "+ ;
         LEFT(S_8C24.NAME,1)+". "+LEFT(S_8C24.NAME,1)+"."
ENDIF
nm_s = PADR(nm_s,28)


IF EMPTY(BR_8C24.DATE_INP)
  nm_b = ""
ELSE
  IF BR_8C24.WHO_INP = 0
    nm_b = ""
  ELSE
    nm_b = ALLTRIM(B_8C24.FAMILY)+" "+ ;
           LEFT(B_8C24.NAME,1)+". "+LEFT(B_8C24.NAME,1)+"."
  ENDIF
ENDIF
nm_b = PADR(nm_s,28)

@ WROWS()-2, 3 SAY nm_s

@ WROWS()-2, 36 SAY nm_b

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.
