*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� V_Ord1       ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                        蹍甃�矗 ��錢�� �� 瓷���                        �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 11.11.2005 様�
PROCEDURE V_Ord1
PARAMETERS prmList, prmNum

*
* prmList - ���ム 甎�瓷�;
* prmNum  - ���ム ��錢��.
*


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
USE (m.base_path+"STOCK") SHARED AGAIN ORDER TAG CODE ALIAS ST_5B11
SELECT 0
USE (m.base_path+"ORD_D") SHARED AGAIN ORDER TAG DOC_NUM ALIAS OD_5B11
SET RELATION TO CODE INTO ST_5B11
SELECT 0
DO Use_Dummy
SELECT OD_5B11

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
m.ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
m.wd = 65

DO D_Wins WITH m.ln, m.wd, ;
"��錢�� � "+ALLTRIM(m.prmList)+"/"+ALLTRIM(STR(m.prmNum)), 0, 0

@ 1, 2 SAY ;
"�むメ陳陳陳陳���━キ����┘陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳����-〓"
*AAAAAAAA PPPP nnnnnnnnnNnnnnnnnnnNnnnnnnnnnNnnnnn FFFFFFFF 999999
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
    BROWSE KEY m.prmList+STR(m.prmNum, 3) ;
               FIELDS ST_5B11.ADDRESS:H="",   ;
                      ST_5B11.PREFIX:H="",    ;
                      ST_5B11.NAME:35:H="",   ;
                      ST_5B11.PRODUCER:H="",  ;
                      QNT:H="":P="999999",    ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
*      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
      m.what_do = "Exit"
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
USE IN ST_5B11
USE IN OD_5B11

SELECT (m.s_sav)

RETURN
