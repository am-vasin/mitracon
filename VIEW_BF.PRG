*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� View_BF      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                       �腑��-���矣琺 �� ���皀Ε�                        �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 10.01.2009 様�
PROCEDURE View_BF
PARAMETERS prmCus 

PRIVATE tmpKey
m.tmpKey = IIF(EMPTY(m.prmCus), "", STR(m.prmCus))
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
USE (m.base_path+"ACCOUNT") SHARED AGAIN ORDER TAG CUS_CODE ALIAS PB_ACC
SELECT 0
USE (m.base_path+"INCMONEY") SHARED AGAIN ORDER TAG DOC_NUM ALIAS PB_MON
SELECT 0
USE (m.base_path+"SALE_TIT") SHARED AGAIN ORDER TAG DOC_NUM ALIAS PB_SAL
SELECT 0
USE (m.base_path+"INCMONEY") SHARED AGAIN ORDER TAG DOC_NUM ALIAS PB_SRC

SELECT 0
IF EMPTY(m.tmpKey)
  USE (m.base_path+"PAY_BILL") SHARED AGAIN ORDER TAG HRON DESCENDING
ELSE
  USE (m.base_path+"PAY_BILL") SHARED AGAIN ORDER TAG CUS_CODE DESCENDING
ENDIF
SET RELATION TO	CUS_CODE INTO PB_ACC, ;
				FIRM+"1"+PAY_YEAR+PAY_DOC INTO PB_SRC, ;
				FIRM+"1"+REV_YEAR+REV_DOC INTO PB_MON, ;
				FIRM+REV_YEAR+REV_DOC     INTO PB_SAL

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
PRIVATE ln, wd
m.ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
m.wd = 69
DO D_Wins WITH m.ln, m.wd, "�腑�� ���矣琺 �� ���皀���", 0, 2
@ 1, 2 SAY "�爼陳���-� �陳陳陳陳陳�����陳陳陳陳陳�祠��陳陳��痳. ���皀δ陳������"
* ��� 12345678901/999 ��.��.���� 999999999999.99 12345678901 ��.��.����
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE KEY m.tmpKey REST ;
           FIELDS FIRM:H="",  ;
                  D1=Show_Doc(DOC_NUM, SUB_DOC):15:H="", ;
                  DOC_DATE:H="", ;
                  BILL_SUM:H="":P="999999999999.99", ;
                  D2=LEFT(PB_SRC.DOC_NUM,1)+"-"+ALLTRIM(SUBSTR(PB_SRC.DOC_NUM,2)):11:H="", ;
                  PB_SRC.DOC_DATE:H="",	;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Print")
    ENDIF

  CASE m.what_do = "Print"    && �ョ�碎

    WAIT WINDOW "�ョ�碎"
    m.what_do = "List"

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
SELECT (m.s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Draw_Foot    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                   ��痺ガ ぎ�����皀�讚竡 ┃筮爼�罔�.                    �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 20.10.1998 陳�
PROCEDURE Draw_Foot

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� Гメ�, 爬痺ガ...
*
@ WROWS()-3, 3 SAY "��┘��"
@ WROWS()-3, COL()+1 SAY PB_ACC.CUS_NAME COLOR SCHEME 1

DO CASE
CASE REV_TYPE = " "
  @ WROWS()-2, 2 SAY SPACE(WCOLS()-4)
CASE REV_TYPE = "S"
  @ WROWS()-2, 2 SAY " 蹍���� � "+LEFT(PB_SAL.NEW_DOC, 1)+"-"+ALLTRIM(SUBSTR(PB_SAL.NEW_DOC, 2))+ ;
                     " �� "+DTOC(PB_SAL.DOC_DATE)+" 痺��� "+ALLTRIM(TRANSFORM(PB_SAL.TOTAL_R, "999 999 999.99"))
CASE REV_TYPE = "M"
  @ WROWS()-2, 2 SAY " ���皀� � "+LEFT(PB_MON.DOC_NUM, 1)+"-"+ALLTRIM(SUBSTR(PB_MON.DOC_NUM, 2))+ ;
                     " �� "+DTOC(PB_MON.DOC_DATE)+" 痺��� "+ALLTRIM(TRANSFORM(PB_MON.MONEY_R, "999 999 999.99"))
ENDCASE

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Show_Doc     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                       ���ム ぎ�祠キ�� か� �ョ�皋                       �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 11.01.2009 陳�
PROCEDURE Show_Doc
PARAMETERS tmpDoc, tmpSub

PRIVATE res
m.res = LEFT(m.tmpDoc,2)+"-"+ALLTRIM(SUBSTR(m.tmpDoc,3))
IF .NOT. EMPTY(m.tmpSub)
  m.res = m.res+"/"+ALLTRIM(m.tmpSub)
ENDIF

RETURN m.res
