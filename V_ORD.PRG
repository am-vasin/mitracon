*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� V_Ord        ��о�｀砒┴ ��むォ ��瓱�           02.02.98 09:34:56 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                     ��錢�� �� ��Ж罔� か� 甎�瓷�.                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE V_Ord
PARAMETERS prmList, prmCode

*
*  prmList - ���ム 甎�瓷� (C);
*  prmCode - ��� ��Ж罔�.
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
PRIVATE tmpName, s_sav, tmpDoc
m.tmpName = SYS(3)

*
*   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
*
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*
m.s_sav = SELECT()
SELECT 0
USE (m.base_path+"LINK_ORD") SHARED AGAIN ORDER TAG LIST_CODE ALIAS LO_6112
IF .NOT. SEEK(m.prmList+STR(m.prmCode))
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��� ��Ж罔� �� ����щ����瘡..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (m.s_sav)
  RETURN
ENDIF
SELECT 0
DO Use_Dummy
SELECT 0
USE (m.base_path+"STOCK") SHARED AGAIN ORDER TAG CODE ALIAS ST_6112
SEEK m.prmCode
SELECT 0
USE (m.base_path+"ACCOUNT") SHARED AGAIN ORDER TAG CUS_CODE ALIAS CUS_6112
SELECT 0
USE (m.base_path+"PERSONS") SHARED AGAIN ORDER TAG CODE ALIAS PRS_6112
SELECT 0
USE (m.base_path+"ORD_TIT") SHARED AGAIN ORDER TAG ORD_NUM ALIAS ORD_6112
SET RELATION TO SUP_CODE INTO CUS_6112, ;
                WHO_ORD  INTO PRS_6112
SELECT 0
CREATE DBF (m.tmpo_path+m.tmpName) ;
	(	DOC_NUM		C(14), ;
		DOC_DATE	D,     ;
		WHO_ORD		C(37), ;
		QNT			N(10), ;
		SUPPLIER	C(50)  )
USE (m.tmpo_path+m.tmpName) ALIAS TMP_6112
SELECT LO_6112
SCAN REST WHILE CODE = m.prmCode .AND. LIST_NUM = m.prmList
  IF ORD_NUM # "**********"
    = SEEK(ORD_FIRM+ORD_YEAR+ORD_NUM, "ORD_6112")
    m.tmpDoc = PADL(ALLTRIM(ORD_6112.ORD_NUM)+"/"+ALLTRIM(ORD_6112.FIRM), 14)
    SELECT TMP_6112
    APPEND BLANK
    REPLACE	DOC_NUM		WITH m.tmpDoc,          ;
			DOC_DATE	WITH ORD_6112.ORD_DATE, ;
			SUPPLIER	WITH CUS_6112.CUS_NAME, ;
			QNT			WITH LO_6112.QNT,       ;
			WHO_ORD		WITH ALLTRIM(PRS_6112.FAMILY)+" "+ ;
			                 LEFT(PRS_6112.NAME, 1)+" "+ ;
			                 LEFT(PRS_6112.S_NAME, 1)
  ELSE
    SELECT TMP_6112
    APPEND BLANK
    REPLACE	DOC_NUM		WITH "****************", ;
			DOC_DATE	WITH CTOD(""),           ;
			QNT			WITH LO_6112.QNT
  ENDIF
  SELECT LO_6112
ENDSCAN
USE IN LO_6112
USE IN PRS_6112
USE IN CUS_6112
USE IN ORD_6112

SELECT TMP_6112
GO TOP
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
ln = MIN(10, RECCOUNT())   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 74

DO D_Wins WITH ln, wd, "��錢�� �� ��Ж罔� か� 甎�瓷�", 1, 1
@ 1, 2 SAY "��Ж罔� "+IIF(EMPTY(ST_6112.PREFIX), "", ALLTRIM(ST_6112.PREFIX)+" ")+ ;
 ALLTRIM(ST_6112.NAME)+IIF(EMPTY(ST_6112.PRODUCER), "", " /"+ALLTRIM(ST_6112.PRODUCER)+"/")
USE IN ST_6112

@ 2, 2 SAY "���ム ぎ�.陳陳陳陳����陳陳��皰 �������陳陳陳陳陳陳陳陳陳陳陳陳陳陳���-〓"

@ WROWS()-3, 3 SAY "��痰�∫┴"

what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
*    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE FIELDS DOC_NUM:H="", DOC_DATE:H="", WHO_ORD:H="", QNT:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Send")
    ENDIF

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
USE IN TMP_6112
DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
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
@ WROWS()-2, 3 SAY TMP_6112.SUPPLIER COLOR SCHEME 1

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.
