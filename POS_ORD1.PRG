*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Pos_Ord1     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                    ��錢�� �� ��痰�→� �� �キイΕ牀�                    �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 06.10.2005 様�
PROCEDURE Pos_Ord1
PARAMETERS prmFirm, prmDate, prmNum, prmCode

PRIVATE tmpName, posName
m.tmpName = SYS(3)
DO Prp_Data
IF RECCOUNT("T_L_5A06") = 0
  PRIVATE mss
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"��� ��Ж罔� ����щ����瘡 �� 瓷���..."
  mss[3] = ""
  DO Out_Mess with 7, "mss"
  USE IN T_L_5A06
  DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
  DELETE FILE (m.tmpo_path+m.tmpName+".cdx")
  RETURN
ENDIF

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
USE (m.base_path+"STOCK") SHARED AGAIN ORDER TAG CODE ALIAS S_5A06
SEEK m.prmCode
m.posName = ALLTRIM(S_5A06.PREFIX)
IF EMPTY(m.posName)
  m.posName = ALLTRIM(S_5A06.NAME)
ELSE
  m.posName = m.posName+" "+ALLTRIM(S_5A06.NAME)
ENDIF
IF .NOT. EMPTY(S_5A06.PRODUCER)
  m.posName = m.posName+" /"+ALLTRIM(S_5A06.PRODUCER)+"/"
ENDIF
USE

DO Use_Dummy
SELECT T_L_5A06

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
m.ln = MIN(RECCOUNT(), WROWS("")-11)   && ����腑痰〓 ※え�諷 痰牀� BROWSE
m.wd = 54

DO D_Wins WITH m.ln, m.wd, "����щ �キイΕ牀� �� ��痰�→� ��Ж罔�", 1, 1

@  1,  2 SAY "��Ж罔� "
@  1,  COL() SAY m.posName COLOR SCHEME 1
@  1,  COL() SAY " ����� �� "
@  1,  COL() SAY DTOC(m.prmDate) COLOR SCHEME 1
@  2,  2 SAY ;
"���甌�����-〓��キイΕ狡�"

@ WROWS()-3, 2 SAY "��┘��"

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
    BROWSE FIELDS LIST_NUM:H="",       ;
                  QNT:H="":P="999999", ;
                  MANAGER:H="",        ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

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
USE IN T_L_5A06
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
@ WROWS()-2, 3 SAY T_L_5A06.CUSTOMER COLOR SCHEME 1
ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Prp_Date     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      ��ぃ�皰→� ∇ガキ���� �����                       �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 06.10.2005 陳�
PROCEDURE Prp_Data

PRIVATE s_sav, tmpMan, tmpCus

m.s_sav = SELECT()
SELECT 0
CREATE DBF (m.tmpo_path+m.tmpName) ;
	(	LIST_NUM	C( 6), ;
		QNT			N( 6), ;
		MANAGER		C(40), ;
		CUSTOMER	C(50)  )
USE (m.tmpo_path+m.tmpName) ALIAS T_L_5A06
SELECT 0
USE (m.base_path+"PERSONS") SHARED AGAIN ORDER TAG CODE ALIAS PRS_5A06
SELECT 0
USE (m.base_path+"ACCOUNT") SHARED AGAIN ORDER TAG CUS_CODE ALIAS CUS_5A06
SELECT 0
USE (m.base_path+"SALE_TIT") SHARED AGAIN ORDER TAG LIST_NUM ALIAS SAL_5A06
SELECT 0
USE (m.base_path+"LIST_TIT") SHARED AGAIN ORDER TAG LST_NUM ALIAS LST_5A06
SELECT 0
USE (m.base_path+"LINK_ORD") SHARED AGAIN ORDER TAG ORD_CODE ALIAS LORD_5A06
SEEK m.prmFirm+LEFT(DTOS(m.prmDate),4)+m.prmNum+STR(m.prmCode)
SCAN REST WHILE m.prmFirm = ORD_FIRM .AND. ;
                LEFT(DTOS(m.prmDate),4) = ORD_YEAR .AND. ;
                m.prmNum = ORD_NUM .AND. ;
                m.prmCode = CODE
  m.tmpMan = ""
  m.tmpCus = ""
  = SEEK(MANAGER, "PRS_5A06")
  m.tmpMan = ALLTRIM(PRS_5A06.FAMILY)+" "+LEFT(PRS_5A06.NAME, 1)+" "+LEFT(PRS_5A06.S_NAME, 1)
  IF SEEK(VAL(LIST_NUM), "LST_5A06")
    = SEEK(LST_5A06.CLI_CODE, "CUS_5A06")
    m.tmpCus = CUS_5A06.CUS_NAME
  ELSE
    IF SEEK(LIST_NUM, "SAL_5A06")
      = SEEK(SAL_5A06.CUS_CODE, "CUS_5A06")
      m.tmpCus = CUS_5A06.CUS_NAME
    ENDIF
  ENDIF
  SELECT T_L_5A06
  APPEND BLANK
  REPLACE	LIST_NUM	WITH LORD_5A06.LIST_NUM, ;
			QNT			WITH LORD_5A06.QNT,      ;
			MANAGER		WITH m.tmpMan,           ;
			CUSTOMER	WITH m.tmpCus
  SELECT LORD_5A06
ENDSCAN

USE IN PRS_5A06
USE IN CUS_5A06
USE IN LST_5A06
USE IN SAL_5A06
USE IN LORD_5A06
SELECT (m.s_sav)

RETURN
