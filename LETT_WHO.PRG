*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Lett_Who     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                     ���甌� ���竍�皀�ォ 甌�♂キ��.                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 22.10.2005 様�
PROCEDURE Lett_Who
PARAMETERS tmpID		&& ��� 甌�♂キ��


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
PRIVATE tmpName, tmpTit
m.tmpName = SYS(3)

DO TmpList

m.s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT 0
USE base_path+"LETT_OUT" SHARED AGAIN ALIAS LT_5A22 ORDER TAG MESS_ID
SEEK m.tmpID
m.tmpTit = TITLE
USE
SELECT TMP_LIST

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
m.ln = MIN(WROWS("")-11, RECCOUNT())   && ����腑痰〓 ※え�諷 痰牀� BROWSE
m.wd = FSIZE("NAME")+FSIZE("R_TIME")+FSIZE("R_DATE")+2

DO D_Wins WITH m.ln, m.wd, "���竍�皀�� 甌�♂キ��", 1, 1

@ 1, 2 SAY "�ガ� "
@ 1, COL() SAY m.tmpTit COLOR SCHEME 1
@ 2, 2 SAY PADR("���竍�皀��", FSIZE("NAME")+1, "�")+"���♂. �牀腮����"
@ WROWS()-3, 2 SAY "�ガ �牀腮����"

m.what_do = "List"

GO TOP

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    m.statys_type = 3
    DO Prp_Nav_1

*
*    BROWSE - �キ�
*
    BROWSE FIELDS NAME:H="",   ;
                  R_DATE:H="", ;
                  R_TIME:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

*  CASE m.what_do = "Choose"    && 蹍甃�矗 甎�瓷�
*
*    DO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*    m.what_do = "List"
*
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

@ WROWS()-2, 2 SAY WHO_RD COLOR SCHEME 1

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.


*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� TmpList      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      ��ぃ�皰→� ∇ガキ���� �����                       �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 22.10.2005 陳�
PROCEDURE TmpList

PRIVATE s_sav, tmpN

m.s_sav = SELECT()
SELECT 0
CREATE DBF (m.tmpo_path+m.tmpName) ;
	(	NAME	C(40), ;
		R_DATE	C(10), ;
		R_TIME	C( 5), ;
		WHO_RD	C(40)  )
USE (m.tmpo_path+m.tmpName) ALIAS TMP_LIST
SELECT 0
USE base_path+"PERSONS" SHARED AGAIN ALIAS WHO_5A22 ORDER TAG CODE
SELECT 0
USE base_path+"ARM_LST" SHARED AGAIN ALIAS WHERE_5A22 ORDER TAG ARM_C
SELECT 0
USE base_path+"LETT_IN" SHARED AGAIN ALIAS LT_5A22 ORDER TAG MESS_ID
SEEK m.tmpID

SCAN REST WHILE m.tmpID = MESS_ID
  SELECT TMP_LIST
  APPEND BLANK
  IF EMPTY(LT_5A22.ADDRESS_T)
    = SEEK(VAL(LT_5A22.ADDRESS), "WHO_5A22")
    m.tmpN = ALLTRIM(WHO_5A22.FAMILY)+" "+LEFT(WHO_5A22.NAME, 1)+" "+LEFT(WHO_5A22.S_NAME, 1)
  ELSE
    = SEEK(LEFT(LT_5A22.ADDRESS, FSIZE("ARM_C", "WHERE_5A22")), "WHERE_5A22")
    m.tmpN = WHERE_5A22.ARM_N
  ENDIF
  REPLACE NAME WITH m.tmpN
  IF .NOT. EMPTY(LT_5A22.READ_DATE)
    = SEEK(LT_5A22.WHO_READ, "WHO_5A22")
    m.tmpN = ALLTRIM(WHO_5A22.FAMILY)+" "+LEFT(WHO_5A22.NAME, 1)+" "+LEFT(WHO_5A22.S_NAME, 1)
    REPLACE	R_DATE	WITH DTOC(LT_5A22.READ_DATE),    ;
			R_TIME	WITH LEFT(LT_5A22.READ_TIME, 5), ;
			WHO_RD	WITH m.tmpN
  ENDIF
  SELECT LT_5A22
ENDSCAN

USE IN LT_5A22
USE IN WHERE_5A22
USE IN WHO_5A22

SELECT (m.s_sav)

RETURN
