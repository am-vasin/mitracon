*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� V_Order      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                            蹍甃�矗 甎�瓷�                             �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 10.10.2005 様�
PROCEDURE V_Order
PARAMETERS prmFirm, prmDate, prmNum

PRIVATE tmpName, tmpSup

m.tmpName = SYS(3)
m.tmpSup = ""
DO Prp_Data


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
SELECT TMP_ORD

*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Manager OF (menu_name) PROMPT "�皰 �������" KEY Ctrl-A, "Enter"
ON SELECTION PAD Manager OF (menu_name) DO Sw_Mode WITH "Manager"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
m.ln = MIN(WROWS("")-11, RECCOUNT())   && ����腑痰〓 ※え�諷 痰牀� BROWSE
m.wd = 58

DO D_Wins WITH m.ln, m.wd, ;
               "��錢�� � "+ALLTRIM(m.prmNum)+"/"+ALLTRIM(m.prmFirm)+" �� "+DTOC(m.prmDate),  ;
               1, 0
m.what_do = "List"
@ 1, 2 SAY "��痰. "
@ 1, 8 SAY LEFT(m.tmpSup, 48) COLOR SCHEME 1
@ 2, 2 SAY ;
"陳陳���━キ����┘陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳����-〓��"
*pppp nnnnnnnnnNnnnnnnnnnNnnnnnnnnnNnnnnn PPPPPPPP 999999 M

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE FIELDS PREFIX:H="",     ;
                  NAME:H="",       ;
                  PRODUCER:H="",   ;
                  QNT:H="",        ;
                  IS_MAN:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Manager")
    ENDIF

  CASE m.what_do = "Manager"    && 蹍甃�矗 甎�瓷�

    DO Pos_Ord1 WITH m.prmFirm, m.prmDate, m.prmNum, CODE
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
USE IN TMP_ORD
DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
DELETE FILE (m.tmpo_path+m.tmpName+".cdx")
SELECT (m.s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Prp_Data     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           ��ぃ�皰→� ����諷                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 10.10.2005 陳�
PROCEDURE Prp_Data

PRIVATE s_sav

m.s_sav = SELECT()
SELECT 0
CREATE DBF (m.tmpo_path+m.tmpName) ;
	(	CODE		N( 7), ;
		PREFIX		C( 4), ;
		NAME		C(35), ;
		PRODUCER	C( 8), ;
		QNT			N( 6), ;
		IS_MAN		C( 1)  )
USE (m.tmpo_path+m.tmpName) EXCLUSIVE ALIAS TMP_ORD

SELECT 0
USE (m.base_path+"LINK_ORD") SHARED AGAIN ALIAS LO_5A10 ORDER TAG ORD_CODE

SELECT 0
USE (m.base_path+"STOCK") SHARED AGAIN ALIAS ST_5A10 ORDER TAG CODE

SELECT 0
USE (m.base_path+"ORDER") SHARED AGAIN ALIAS O_DET_5A10 ORDER TAG ORD_NUM
SET RELATION TO CODE INTO ST_5A10, ;
                FIRM+LEFT(DTOS(ORD_DATE), 4)+ORD_NUM+STR(CODE) INTO LO_5A10

SELECT 0
USE (m.base_path+"ACCOUNT") SHARED AGAIN ALIAS SUP_5A10 ORDER TAG CUS_CODE

SELECT 0
USE (m.base_path+"ORD_TIT") SHARED AGAIN ALIAS O_TIT_5A10 ORDER TAG ORD_NUM
SET RELATION TO SUP_CODE INTO SUP_5A10

SEEK m.prmFirm+LEFT(DTOS(m.prmDate), 4)+m.prmNum
m.tmpSup = ALLTRIM(SUP_5A10.CUS_NAME)

SELECT O_DET_5A10
SEEK m.prmFirm+LEFT(DTOS(m.prmDate), 4)+m.prmNum
SCAN REST WHILE m.prmFirm = FIRM .AND. m.prmDate = ORD_DATE .AND. m.prmNum = ORD_NUM
  SELECT TMP_ORD
  APPEND BLANK
  REPLACE	CODE		WITH O_DET_5A10.CODE,  ;
			PREFIX		WITH ST_5A10.PREFIX,   ;
			NAME		WITH ST_5A10.NAME,     ;
			PRODUCER	WITH ST_5A10.PRODUCER, ;
			QNT			WITH O_DET_5A10.QNT,  ;
			IS_MAN		WITH IIF(FOUND("LO_5A10"), "�", " ")
  SELECT O_DET_5A10
ENDSCAN

USE IN O_DET_5A10
USE IN O_TIT_5A10
USE IN LO_5A10
USE IN SUP_5A10
USE IN ST_5A10
SELECT TMP_ORD
INDEX ON NAME+PREFIX+PRODUCER TAG NAME
GO TOP

SELECT (m.s_sav)

RETURN
