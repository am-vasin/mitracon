*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Own_Cus      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                   �襦�� ├ 甎�瓷� 甌＝癶. ��┘�皰�.                    �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 29.12.2000 様�
PROCEDURE Own_Cus
PARAMETERS tmpCode, tmpNm

PRIVATE s_sav, r_no, tmpName, mss

r_no = 0
s_sav = SELECT()
SELECT 0
tmpName = SYS(3)
CREATE DBF (tmpo_path+tmpName) (CUS_CODE N( 8), CUS_NAME C(50))
USE (tmpo_path+tmpName) ALIAS TLST_0C29 EXCLUSIVE
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG WHO_NEW AGAIN ALIAS ACCNT_0C29
SEEK user
SCAN REST WHILE WHO_NEW = user
  SELECT TLST_0C29
  APPEND BLANK
  REPLACE CUS_CODE WITH ACCNT_0C29.CUS_CODE, ;
          CUS_NAME WITH ACCNT_0C29.CUS_NAME
  IF tmpCode = CUS_CODE
    r_no = RECNO()
  ENDIF
  SELECT ACCNT_0C29
ENDSCAN

USE
IF RECCOUNT("TLST_0C29") = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� ���� �� ���爛��キ� �� �き��� ��┘���!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SELECT TLST_0C29
  tmpName = DBF()
  USE
  DELETE FILE (tmpName)
  DELETE FILE (LEFT(tmpName, LEN(tmpName)-3)+"cdx")
  SELECT (s_sav)
  RETURN
ENDIF
SELECT TLST_0C29
INDEX ON SYS(15, lwr, CUS_NAME) TAG NAME
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

SELECT 0
DO Use_Dummy
SELECT TLST_0C29
IF r_no # 0
  GO r_no
ELSE
  GO TOP
ENDIF

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�襦��碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Seek OF (menu_name) PROMPT "���皀�痰�覃 ���瓷" KEY Ctrl-A, "F7"
ON SELECTION PAD Seek OF (menu_name) DO Sw_Mode WITH "Seek"

DEFINE PAD Exit OF (menu_name) PROMPT "�皖���碎瘴" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = MIN(WROWS("")-11, RECCOUNT())   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 50

DO D_Wins WITH ln, wd, "���甌� ��┘�皰�", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F7    DO Sw_Mode WITH "Seek"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS CUS_NAME:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && �襦��

    tmpCode = CUS_CODE
    IF TYPE("tmpNm") = "C"
      tmpNm = CUS_NAME
    ENDIF
    EXIT

  CASE what_do = "Seek"      && ���皀�痰�覃 ���瓷

    DO C_Seek
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

SELECT TLST_0C29
tmpName = DBF()
USE
DELETE FILE (tmpName)
DELETE FILE (LEFT(tmpName, LEN(tmpName)-3)+"cdx")
SELECT (s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� C_Seek       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           ���皀�痰�覃 ���瓷.                           �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 20.04.2001 陳�
PROCEDURE C_Seek

PRIVATE tmpPatt, s_sav, fileName, r_sav, mss

tmpPatt = Get_Shab()
IF EMPTY(tmpPatt)
  RETURN
ENDIF

DO Wt_Mess WITH "���瓷..."
s_sav = SELECT()
fileName = SYS(3)
SELECT 0
CREATE DBF (tmpo_path+fileName) ;
  ( LINK     N(10),  ;
    CUS_NAME C(FSIZE("CUS_NAME", "TLST_0C29")) )
USE (tmpo_path+fileName) ALIAS TMPSEEK
SELECT TLST_0C29
r_sav = RECNO()
tmpPatt = SYS(15, lwr, tmpPatt)
SCAN
  IF tmpPatt $ SYS(15, lwr, CUS_NAME)
    SELECT TMPSEEK
    APPEND BLANK
    REPLACE LINK     WITH RECNO("TLST_0C29"), ;
            CUS_NAME WITH TLST_0C29.CUS_NAME
    SELECT TLST_0C29
  ENDIF
ENDSCAN

DO Wt_Mess

IF RECCOUNT("TMPSEEK") = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��腑�� �� ���ぅ��"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SELECT TMPSEEK
  USE
  DELETE FILE (tmpo_path+fileName+".DBF")
  IF BETWEEN(r_sav, 1, RECCOUNT("TLST_0C29"))
    GO r_sav IN TLST_0C29
  ENDIF
  SELECT (s_sav)
  RETURN
ENDIF

DO Ch_Found

SELECT TMPSEEK
USE
DELETE FILE (tmpo_path+fileName+".DBF")
SELECT (s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Ch_Found     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                         �襦�� ├ ���ぅ�����...                         �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 20.04.2001 陳�
PROCEDURE Ch_Found


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

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*

s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT TMPSEEK
GO TOP

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
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 50

DO D_Wins WITH ln, wd, "��, 艪� 磴���瘡 ���皋...", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE FIELDS CUS_NAME:H="":50,    ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && �襦��

    GO TMPSEEK.LINK IN TLST_0C29
    EXIT

  OTHERWISE
    IF BETWEEN(r_sav, 1, RECCOUNT("TLST_0C29"))
      GO r_sav IN TLST_0C29
    ENDIF
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
SELECT (s_sav)

RETURN
