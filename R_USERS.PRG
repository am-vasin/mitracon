*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� R_Users      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                ���甌� ���讌���皀�ォ � ������覓 ぎ痰祚��                �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 20.02.2002 様�
PROCEDURE R_Users
PARAMETERS r_code

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
PRIVATE tmpName, tmpRight

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*

s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT 0
USE (base_path+"S_RIGHTS") SHARED AGAIN ALIAS TMP_20220 ORDER TAG RIGHT
SEEK r_code
tmpRight = ALLTRIM(NAME)
USE (base_path+"PERSONS") SHARED AGAIN ALIAS PS_20220 ORDER TAG CODE
SELECT 0
USE (base_path+"USERS") SHARED AGAIN ALIAS USER_20220
SET RELATION TO USER_CODE INTO PS_20220

SELECT 0
tmpName = SYS(3)
CREATE DBF (tmpo_path+tmpName) ;
  ( CODE   N( 5), ;
    FAMILY C(FSIZE("FAMILY", "PS_20220")), ;
    NAME   C(FSIZE("NAME", "PS_20220")),   ;
    S_NAME C(FSIZE("S_NAME", "PS_20220"))  )
USE (tmpo_path+tmpName) EXCLUSIVE ALIAS TMP_20220

SELECT USER_20220
SCAN FOR S_RIGHTS = r_code
  SELECT TMP_20220
  APPEND BLANK
  REPLACE CODE   WITH PS_20220.CODE,  ;
          FAMILY WITH PS_20220.FAMILY,;
          NAME   WITH PS_20220.NAME,  ;
          S_NAME WITH PS_20220.S_NAME
  SELECT USER_20220
ENDSCAN
USE
SELECT PS_20220
USE
SELECT TMP_20220
INDEX ON SYS(15, lwr, FAMILY+NAME+S_NAME) TAG NAME
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
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = FSIZE("FAMILY")+FSIZE("NAME")+FSIZE("S_NAME")+2

DO D_Wins WITH ln, wd, tmpRight, 0, 0
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
    BROWSE FIELDS FAMILY:H="",  ;
                  NAME:H="",    ;
                  S_NAME:H="",  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && 蹍甃�矗 甎�瓷�

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
tmpRight = DBF()
USE
DELETE FILE (tmpRight)
DELETE FILE (LEFT(tmpRight, LEN(tmpRight)-3)+"cdx")
SELECT (s_sav)

RETURN
