*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Seek_Eml     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                            ���瓷 �� E-mail                             �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 11.01.2011 様�
PROCEDURE Seek_Eml

PRIVATE tmpSh, s_sav, tmpName, tmpRec

m.tmpSh = Get_ShE()
m.tmpSh = LOWER(ALLTRIM(m.tmpSh))
IF EMPTY(m.tmpSh)
  RETURN 0
ENDIF
m.s_sav = SELECT()
SELECT 0
m.tmpName = SYS(3)
CREATE DBF (m.tmpo_path+m.tmpName) ;
	(	LINK	N(10),	;
		C_NAME	C(50),	;
		C_EML	C(32)	)
USE (m.tmpo_path+m.tmpName) ALIAS TMP_0110 EXCLUSIVE
SELECT 0
USE (base_path+"ACCOUNT") ALIAS CL_0110 AGAIN ORDER TAG CUS_CODE
SELECT 0
USE (base_path+"CLI_INFO") ALIAS CI_0110 AGAIN
SET RELATION TO CUS_CODE INTO CL_0110
DO Wt_Mess WITH "���瓷"
SCAN FOR tmpSh $ LOWER(EML)
  SELECT TMP_0110
  APPEND BLANK
  REPLACE LINK		WITH RECNO("CL_0110"),	;
          C_NAME	WITH CL_0110.CUS_NAME,	;
          C_EML		WITH CI_0110.EML
  SELECT CI_0110
ENDSCAN
DO Wt_Mess

USE IN CL_0110
USE IN CI_0110
IF RECCOUNT("TMP_0110") = 0
  PRIVATE mss
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"��腑�� �� ���ぅ�..."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  USE IN TMP_0110
  DELETE FILE (m.tmpo_path+m.tmpName+".DBF")
  SELECT (m.s_sav)
  RETURN 0
ENDIF

SELECT TMP_0110
INDEX ON LOWER(C_NAME) TAG C_NAME
GO TOP

m.tmpRec = Ch_EML()
USE IN TMP_0110
DELETE FILE (m.tmpo_path+m.tmpName+".DBF")
DELETE FILE (m.tmpo_path+m.tmpName+".CDX")
SELECT (m.s_sav)

RETURN m.tmpRec

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Ch_Eml       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                        �襦�� ��┘��� ├ 甎�瓷�                         �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 11.01.2011 陳�
PROCEDURE Ch_Eml

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
PRIVATE r_val
*
*   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
*
m.stat_type  = 3
m.menu_name  = PROGRAM()
m.last_mouse = 0
m.win_name   = PROGRAM()

m.s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT TMP_0110

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�襦��碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "�皖���碎瘴" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
m.ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
m.wd = 50

DO D_Wins WITH m.ln, m.wd, "��┘�硅", 0, 1
m.what_do = "List"
@ WROWS()-2, 2 SAY "E-mail"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE FIELDS C_NAME,     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Eml()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Choose"    && 蹍甃�矗 甎�瓷�

    m.r_val = TMP_0110.LINK
    EXIT

  OTHERWISE

    m.r_val = 0
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

RETURN m.r_val

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Draw_Eml    ��о�｀砒┴ ��むォ ��瓱�                       �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                   ��痺ガ ぎ�����皀�讚竡 ┃筮爼�罔�.                    �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 20.10.1998 陳�
PROCEDURE Draw_Eml

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� Гメ�, 爬痺ガ...
*
@ WROWS()-2, 9 SAY C_EML
ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.
