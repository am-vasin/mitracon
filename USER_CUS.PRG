*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� User_Cus     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                       ���甌� ��┘�皰� �キイΕ��                        �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 28.12.2000 様�
PROCEDURE User_Cus
PARAMETERS super

PRIVATE curUserC, tmpName, curUserN, s_sav, mss, patt
patt = ""

tmpName = SYS(3)
curUserC = user
s_sav = SELECT()
SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE AGAIN ALIAS P_0C28
SEEK user
curUserN = ALLTRIM(FAMILY)+" "+LEFT(NAME,1)+" "+LEFT(S_NAME,1)
USE
CREATE DBF (tmpo_path+tmpName) ;
   ( CUS_CODE  N( 8), CUS_NAME C(50) )
USE (tmpo_path+tmpName) EXCLUSIVE ALIAS TMP_LIST
INDEX ON CUS_CODE TAG CUS_CODE
INDEX ON SYS(15, lwr, CUS_NAME) TAG CUS_NAME ADDITIVE

DO Prp_List

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
SELECT 0
DO Use_Dummy
SELECT TMP_LIST

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

IF super
  DEFINE PAD Manager OF (menu_name) PROMPT "�キイΕ�" KEY Ctrl-A, "Tab"
  ON SELECTION PAD Manager OF (menu_name) DO Sw_Mode WITH "Manager"
ENDIF

DEFINE PAD Seek OF (menu_name) PROMPT "���瓷" KEY Ctrl-A, "F7"
ON SELECTION PAD Seek OF (menu_name) DO Sw_Mode WITH "Seek"

DEFINE PAD Add OF (menu_name) PROMPT "����※碎" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"

DEFINE PAD Del OF (menu_name) PROMPT "�����碎" KEY Ctrl-A, "Del"
ON SELECTION PAD Del OF (menu_name) DO Sw_Mode WITH "Del"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 50

DO D_Wins WITH ln, wd, "���甌� ��┘�皰� �キイΕ��", 1, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    ACTIVATE WINDOW (win_name+"_M")
    @ 1, 2 SAY "�キイΕ� "+PADR(curUserN, 30)
    DO Prp_Nav_1
    ON KEY LABEL Del DO Sw_Mode WITH "Del"
    ON KEY LABEL Ins DO Sw_Mode WITH "Add"
    ON KEY LABEL F7  DO Sw_Mode WITH "Seek"
    IF super
      ON KEY LABEL Tab DO Sw_Mode WITH "Manager"
    ENDIF

*
*    BROWSE - �キ�
*
    BROWSE FIELDS CUS_NAME,     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Exit")
    ENDIF

  CASE what_do = "Del"    && ����錺�

    IF .NOT. BOF() .AND. .NOT. EOF()
      DELETE
    ENDIF
    what_do = "List"

  CASE what_do = "Add"    && ����←錺�

    DO Add_Cus
    what_do = "List"

  CASE what_do = "Manager"    && �襦�� �キイΕ��

    DO C_Manager
    what_do = "List"

  CASE what_do = "Seek"      && ���瓷

    patt = Get_Shab(patt, "���Θ皀 ��゛�� か� ���瓷�")
    DO Seek_Cus WITH patt
    what_do = "List"

  OTHERWISE

    DO Save_List
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
SELECT TMP_LIST
tmpName = DBF()
USE
DELETE FILE (tmpName)
DELETE FILE (LEFT(tmpName, LEN(tmpName)-3)+"cdx")
RETURN
SELECT (s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Prp_List     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                ��ぃ�皰→� 甎�瓷� ��┘�皰� か� �キイΕ��                �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 28.12.2000 陳�
PROCEDURE Prp_List

SELECT TMP_LIST
ZAP
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG WHO_NEW AGAIN ALIAS ACC_0C28
SEEK curUserC
SCAN REST WHILE WHO_NEW = curUserC
  SELECT TMP_LIST
  APPEND BLANK
  REPLACE CUS_CODE WITH ACC_0C28.CUS_CODE, ;
          CUS_NAME WITH ACC_0C28.CUS_NAME
  SELECT ACC_0C28
ENDSCAN
USE

SELECT TMP_LIST
GO TOP
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Add_Cus      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                       ����※碎 ��┘��� � 甎�甌�                        �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 28.12.2000 陳�
PROCEDURE Add_Cus

PRIVATE s_sav, skp

s_sav = SELECT()
skp = .F.
SELECT 0
DO Use_Dummy
SELECT 0
DO Use_Link
SELECT 0
USE (base_path+"ACCOUNT") AGAIN ORDER TAG CUS_NAME
IF Custs("", .F., .F., .F., sys_char)
  SELECT TMP_LIST
  SET ORDER TO TAG CUS_CODE
  IF .NOT. SEEK(ACCOUNT.CUS_CODE)
    IF .NOT. EMPTY(ACCOUNT.WHO_NEW) .AND. ACCOUNT.WHO_NEW # curUserC
      SELECT 0
      USE (base_path+"PERSONS") ORDER TAG CODE AGAIN ALIAS PER_0C29
      SEEK ACCOUNT.WHO_NEW
      PRIVATE mss
      DIMENSION mss(4)
      mss(1) = ""
      mss(2) = CHR(0)+"�皋� ��┘�皰� 礒� ���━�モ瘴 "+ALLTRIM(FAMILY)+" "+ ;
               LEFT(NAME,1)+" "+LEFT(S_NAME,1)
      mss(3) = CHR(0)+"  �キ錺� �キイΕ��? [ �� ] [ �モ ]"
      mss(4) = ""
      skp = Out_Mess(7, "mss") = 2
      USE
      SELECT TMP_LIST
    ENDIF
    IF .NOT. skp
      APPEND BLANK
      REPLACE CUS_CODE WITH ACCOUNT.CUS_CODE, ;
              CUS_NAME WITH ACCOUNT.CUS_NAME
    ENDIF
  ENDIF
  SET ORDER TO TAG CUS_NAME
ENDIF
SELECT ACCOUNT
USE
SELECT (s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Seek_Cus     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                       ���皀�痰�覃 ���瓷 ��┘���.                       �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 26.12.2000 陳�
PROCEDURE Seek_Cus
PARAMETERS patt

IF EMPTY(patt)
  RETURN
ENDIF

DO Prp_Seek WITH patt
DO Choose_C

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Prp_Seek     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      �癲�� ��Ж罔� �� ���皀�痰�.                       �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 26.12.2000 陳�
PROCEDURE Prp_Seek
PARAMETERS patt

PRIVATE s_sav, tmpName, r_sav, patt_w
patt_w = SYS(15, lwr, patt)
s_sav = SELECT()
SELECT 0
tmpName = SYS(3)
CREATE DBF (tmpo_path+tmpName) ;
  ( LINK     N(10), ;
    CUS_NAME C(50)  )
USE (tmpo_path+tmpName) ALIAS FOUND

SELECT TMP_LIST
r_sav = RECNO()
SCAN
  IF patt $ SYS(15, lwr, CUS_NAME)
    SELECT FOUND
    APPEND BLANK
    REPLACE LINK     WITH RECNO("TMP_LIST"), ;
            CUS_NAME WITH TMP_LIST.CUS_NAME
    SELECT TMP_LIST
  ENDIF
ENDSCAN

IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF

SELECT (s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Choose_C     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�               �襦�� ��┘��� ├ �癲������� �� ���皀�痰�.                �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 26.12.2000 陳�
PROCEDURE Choose_C

PRIVATE tmpName, mss

SELECT FOUND
tmpName = DBF()
*
*    � °� �� ���趺┴?
IF RECCOUNT("FOUND") = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��腑�� �� ���ぅ��..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  DELETE FILE(tmpName)        && 踸｀爲�...
  SELECT TMP_DEM
  RETURN
ENDIF
GO TOP
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
ln = MIN(WROWS("")-11, RECCOUNT())   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 50
DO D_Wins WITH ln, wd, "�ェ祀赳�� ���瓷�", 0, 0
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
    BROWSE FIELDS CUS_NAME:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"	&& �Д� �襦���...

    GO FOUND.LINK IN TMP_LIST
    EXIT

  OTHERWISE					&& �Д� �皖����瘴

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
USE
DELETE FILE(tmpName)        && 踸｀爲�...
SELECT TMP_LIST

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Save_List    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           ��縲��キ┘ 甎�瓷�.                           �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 29.12.2000 陳�
PROCEDURE Save_List

PRIVATE mss, s_sav

DIMENSION mss(3)
mss(1) = ""
mss(2) = CHR(0)+"��縲��━ ├�キキ��? [ �� ] [ �モ ]"
mss(3) = ""
IF Out_Mess(5, "mss") = 2
  RETURN
ENDIF

s_sav = SELECT()
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG WHO_NEW AGAIN ALIAS CUS_0C29
DO WHILE SEEK(CurUserC)
  REPLACE WHO_NEW WITH 0
ENDDO
SET ORDER TO TAG CUS_CODE
SELECT TMP_LIST
SCAN
  SELECT CUS_0C29
  SEEK TMP_LIST.CUS_CODE
  REPLACE WHO_NEW WITH CurUserC
  SELECT TMP_LIST
ENDSCAN

ZAP
SELECT CUS_0C29
USE

SELECT (s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� C_Manager    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                            ��キ� �キイΕ��                             �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 29.12.2000 陳�
PROCEDURE C_Manager

PRIVATE tmpC, tmpN, mss

tmpC = curUserC
tmpN = curUserN

DO Ch_Pers WITH tmpC, tmpN

IF tmpC # curUserC
  DO Save_List
  curUserC = tmpC
  curUserN = tmpN
  DO Prp_List
ENDIF
