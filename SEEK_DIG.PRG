*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Seek_Dig     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                  ���瓷 �� 罔籥�〓� ��痰� ��━キ������                  �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 14.04.2008 様�
PROCEDURE Seek_Dig
PARAMETERS digKey

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
PRIVATE tmpKey, i, c, retValue

m.retValue = 0
m.tmpKey = ""
FOR m.i = 1 TO LEN(m.digKey)
  c = SUBSTR(m.digKey, m.i, 1)
  IF m.c $ "0123456789"
    m.tmpKey = m.tmpKey+m.c
  ENDIF
ENDFOR

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
USE (path_comm+"SUBJECT") SHARED AGAIN ORDER TAG STO_CODE ALIAS DG_SUB
SELECT 0
USE (base_path+"CLASSES") SHARED AGAIN ORDER TAG CODE ALIAS DG_CLS
SELECT 0
USE (base_path+"STOCK") SHARED AGAIN ORDER TAG NUM_PART ALIAS DG_STO
SET RELATION TO CLASS INTO DG_CLS, CODE INTO DG_SUB
*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�襦��碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Filter OF (menu_name) PROMPT "�┼赳�" KEY Ctrl-A, "Tab"
ON SELECTION PAD Filter OF (menu_name) DO Sw_Mode WITH "Filter"

DEFINE PAD Exit OF (menu_name) PROMPT "�皖���碎瘴" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
m.ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
m.wd = 72	&&IIF(SET("CENTURY") = "ON", !!!!!!!!!,  !!!!!!!!!!)

DO D_Wins WITH m.ln, m.wd, "���瓷 �� 罔籥�〓� ��痰�", 0, 1

@ 1, 2 SAY ;
"陳��━キ����┘陳陳陳陳陳陳陳陳陳陳陳陳��爼�陳���-〓陳陳�キ�陳陳陳���痰."
*....:....!....:....!....:....!....: AAAAAAAA 999999 999999.99999 999999

@ WROWS()-3, 3 SAY "��牆竅"

m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Tab   DO Sw_Mode WITH "Filter"

*
*    BROWSE - �キ�
*
    BROWSE KEY m.tmpKey			;
    FIELDS NAME:35:H="",		;
           PRODUCER:8:H="",		;
           QNT:H="":P="999999",	;
           PRICE_R:H="":P="9999999.99999",	;
           QNT_STORE:H="":P="999999",	;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
*

    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Choose"    && 蹍甃�矗 甎�瓷�

    m.retValue = RECNO()
    EXIT
*    m.what_do = "List"

  CASE m.what_do = "Filter"    && �┼赳�

    IF EMPTY(SET("FILTER"))
      SET FILTER TO QNT # 0 .OR. QNT_STORE # 0
    ELSE
      SET FILTER TO
    ENDIF
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
USE IN DG_CLS
USE IN DG_STO
USE IN DG_SUB
SELECT (m.s_sav)

RETURN m.retValue

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
@ WROWS()-2, 3 SAY DG_SUB.CORPUS

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.
