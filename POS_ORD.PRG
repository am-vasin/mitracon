*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Pos_Ord      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                     ��錢�� �� ��痰�→� �� ��Ж罔�.                     �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 06.10.2005 様�
PROCEDURE Pos_Ord
PARAMETERS prmCode


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
PRIVATE tmpName
m.tmpName = SYS(3)
DO P_Lst_Ord
IF RECCOUNT("TMP_LIST") = 0
  PRIVATE mss
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"��� ��Ж罔� �┴���� �� ����щ����瘡..."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  USE IN TMP_LIST
  DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
  DELETE FILE (m.tmpo_path+m.tmpName+".cdx")
  RETURN
ENDIF
m.s_sav = SELECT()
PRIVATE tmpPosName
SELECT 0
USE (m.base_path+"STOCK") SHARED AGAIN ALIAS STO_5A06 ORDER TAG CODE
SEEK m.prmCode
m.tmpPosName = ALLTRIM(STO_5A06.PREFIX)
m.tmpPosName = m.tmpPosName+IIF(.NOT. EMPTY(m.tmpPosName), " ", "")+ ;
               ALLTRIM(STO_5A06.NAME)+ ;
               IIF(.NOT. EMPTY(STO_5A06.PRODUCER), " /"+ALLTRIM(STO_5A06.PRODUCER)+"/", "")
USE
DO Use_Dummy
SELECT TMP_LIST

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Orders OF (menu_name) PROMPT "�キイΕ琺" KEY Ctrl-A, "Enter"
ON SELECTION PAD Orders OF (menu_name) DO Sw_Mode WITH "Orders"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
m.ln = MIN(WROWS("")-11, RECCOUNT())   && ����腑痰〓 ※え�諷 痰牀� BROWSE
m.wd = 76

DO D_Wins WITH m.ln, m.wd, "����щ �� ��痰�→� �� ��Ж罔�", 1, 1
@ 1, 2 SAY "��Ж罔� "+m.tmpPosName
@ 2, 2 SAY ;
 "陳������ �陳����陳陳陳��痰�∫┴陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳����-〓��"
* 1234567890 dd.mm.yyyy ....:....!....:....!....:....!....:....!....: 999999 M
* ....:....!....:....!....:....!..  ....:....!....:....!....:....!..
@ WROWS()-3,  3 SAY "�皰 痲�爼�牀��� ��錢��"
@ WROWS()-3, 43 SAY "踸�ョ��┘"

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
    BROWSE FIELDS ORD_NUM:H="",   ;
                  ORD_DATE:H="",  ;
                  F000 = LEFT(SUPPL, 45):H="", ;
                  QNT:H="":P="999999", ;
                  IS_CONT:H="",        ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Orders")
    ENDIF

  CASE m.what_do = "Orders"    && 蹍甃�矗 甎�瓷�

    DO Pos_Ord1 WITH ORD_FIRM, ORD_DATE, ORD_NUM, m.prmCode
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
USE IN TMP_LIST
DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
DELETE FILE (m.tmpo_path+m.tmpName+".cdx")

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
@ WROWS()-2,  3 SAY MAN COLOR SCHEME 1
@ WROWS()-2, 43 SAY NOTE COLOR SCHEME 1
ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� P_Lst_Ord    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�           ��ぃ�皰→� ∇ガキ���� ����� � ��錢���� �� ��Ж罔�            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 06.10.2005 陳�
PROCEDURE P_Lst_Ord

PRIVATE s_sav

m.s_sav = SELECT()
SELECT 0
CREATE DBF (m.tmpo_path+m.tmpName) ;
	(	ORD_NUM		C(10), ;
		ORD_DATE	D,     ;
		ORD_FIRM	C( 3), ;
		SUPPL		C(50), ;
		MAN			C(32), ;
		NOTE		C(32), ;
		QNT			N( 6), ;
		IS_CONT		C( 1)  )
USE (m.tmpo_path+m.tmpName) EXCLUSIVE ALIAS TMP_LIST

SELECT 0
USE (m.base_path+"LINK_ORD") SHARED AGAIN ALIAS LINK_5A06 ORDER TAG ORD_CODE

SELECT 0
USE (m.base_path+"ACCOUNT") SHARED AGAIN ALIAS SUP_5A06 ORDER TAG CUS_CODE

SELECT 0
USE (m.base_path+"PERSONS") SHARED AGAIN ALIAS MAN_5A06 ORDER TAG CODE

SELECT 0
USE (m.base_path+"ORD_TIT") SHARED AGAIN ALIAS TIT_5A06 ORDER TAG ORD_NUM
SET RELATION TO SUP_CODE INTO SUP_5A06, WHO_ORD INTO MAN_5A06

SELECT 0
USE (m.base_path+"ORDER") SHARED AGAIN ALIAS ORD_5A06 ORDER TAG CODE
SET RELATION TO FIRM+LEFT(DTOS(ORD_DATE), 4)+ORD_NUM INTO TIT_5A06,   ;
                FIRM+LEFT(DTOS(ORD_DATE), 4)+ORD_NUM+STR(CODE) INTO LINK_5A06

SEEK m.prmCode
SCAN REST WHILE CODE = m.prmCode
  SELECT TMP_LIST
  APPEND BLANK
  REPLACE	ORD_NUM		WITH ORD_5A06.ORD_NUM, ;
			ORD_DATE	WITH ORD_5A06.ORD_DATE,;
			ORD_FIRM	WITH ORD_5A06.FIRM,    ;
			SUPPL		WITH SUP_5A06.CUS_NAME,;
			MAN			WITH ALLTRIM(MAN_5A06.FAMILY)+" "+ ;
							 LEFT(MAN_5A06.NAME, 1)  +" "+ ;
							 LEFT(MAN_5A06.S_NAME, 1),     ;
			QNT			WITH ORD_5A06.QNT,     ;
			NOTE		WITH TIT_5A06.NOTE,    ;
			IS_CONT		WITH IIF(FOUND("LINK_5A06"), "�", " ")
  SELECT ORD_5A06
ENDSCAN

USE IN ORD_5A06
USE IN MAN_5A06
USE IN SUP_5A06
USE IN TIT_5A06
USE IN LINK_5A06

SELECT TMP_LIST
INDEX ON DTOS(ORD_DATE) TAG HRON
SET ORDER TO TAG HRON DESCENDING
GO TOP

SELECT (m.s_sav)

RETURN
