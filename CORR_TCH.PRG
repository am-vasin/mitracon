*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Corr_Tch     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                    ��玻オ罔� ���キ���矣琺 ��┘���.                     �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 21.11.2000 様�
PROCEDURE Corr_Tch

PRIVATE tmpCusCode, tmpCusName

tmpCusCode = 0
tmpCusName = ""
DO Ch_Cus WITH tmpCusCode, tmpCusName
IF EMPTY(tmpCusCode)
  RETURN
ENDIF

DO Modi_Sto







*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Ch_Cus       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                             �襦�� ��┘���.                             �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 21.11.2000 陳�
PROCEDURE Ch_Cus
PARAMETERS c_code, c_name

DO Prp_File

DO Choose

SELECT CUS_LIST
USE
DELETE FILE (tmpo_path+"CUS_LIST.DBF")
DELETE FILE (tmpo_path+"CUS_LIST.CDX")

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Prp_File     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           ��ぃ�皰→� �����.                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 21.11.2000 陳�
PROCEDURE Prp_File

DELETE FILE (tmpo_path+"CUS_LIST.DBF")
DELETE FILE (tmpo_path+"CUS_LIST.CDX")
CREATE DBF (tmpo_path+"CUS_LIST.DBF") ;
  ( CUS_CODE N( 8),  ;
    CUS_NAME C(50)   )
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE AGAIN
SELECT 0
USE (base_path+"TEACH") AGAIN
INDEX ON CUS_CODE TO (tmpo_path+"TEACH") UNIQUE FOR CUS_CODE # 0
SET RELATION TO CUS_CODE INTO ACCOUNT
SCAN
  SELECT CUS_LIST
  APPEND BLANK
  REPLACE CUS_CODE WITH ACCOUNT.CUS_CODE, ;
          CUS_NAME WITH ACCOUNT.CUS_NAME
  SELECT TEACH
ENDSCAN
USE
DELETE FILE (tmpo_path+"TEACH.IDX")
SELECT ACCOUNT
USE
SELECT CUS_LIST
INDEX ON SYS(15, lwr, CUS_NAME) TAG CUS_NAME
GO TOP
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Choose       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                             �襦�� ��┘���.                             �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 21.11.2000 陳�
PROCEDURE Choose

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
SELECT CUS_LIST

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
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 50

DO D_Wins WITH ln, wd, "��┘�硅", 0, 0
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
    BROWSE FIELDS CUS_NAME:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && �襦��

    c_code = CUS_LIST.CUS_CODE
    c_name = CUS_LIST.CUS_NAME
    EXIT

  CASE what_do = "Seek"      && ���皀�痰�覃 ���瓷

    DO Seek_Sh
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
SELECT (s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Seek_Sh      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           ���皀�痰�覃 ���瓷.                           �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 21.11.2000 陳�
PROCEDURE Seek_Sh

PRIVATE shab, mss

shab = Get_Shab()
IF EMPTY(shab)
  RETURN
ENDIF

shab = SYS(15, lwr, ALLTRIM(shab))
DO Prep_Tmp

IF RECCOUNT("LINK_TMP") = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��腑�� �� ���ぅ��!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SELECT LINK_TMP
  USE
  DELETE FILE (tmpo_path+"LINK_TMP.DBF")
  SELECT CUS_LIST
ELSE
  DO Ch_Sh
ENDIF

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Get_Shab     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                             �〓� ��゛���.                              �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 21.11.2000 陳�
PROCEDURE Get_Shab

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
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ��ぅ爨�皀�讚�� ��痰� �牀������:
*

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� 敖 ��゛�� か� ���瓷� 陳陳陳陳陳陳陳陳陳陳陳� �
*� � ....:....!....:....!....:....!....:....! � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�       < OK Ctrl-W > < �皖���碎瘴 Esc >       �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, tmpShab
tmpShab = SPACE(40)
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 7, 50, "���皀�痰�覃 ���瓷"
DO Sun_Bord WITH  2,  3,  4, 46, " ��゛�� か� ���瓷� "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@ 3, 5 GET tmpShab
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN IIF(ex = 1, ALLTRIM(tmpShab), "")

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Prep_Tmp     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      ��ぃ�皰→� ∇ガキ���� �����.                      �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 21.11.2000 陳�
PROCEDURE Prep_Tmp

PRIVATE r_sav

SELECT 0
DELETE FILE (tmpo_path+"LINK_TMP.DBF")
CREATE DBF (tmpo_path+"LINK_TMP.DBF") ( LINK N(10) )
SELECT CUS_LIST
r_sav = RECNO()
SCAN
  IF shab $ SYS(15, lwr, CUS_NAME)
    SELECT LINK_TMP
    APPEND BLANK
    REPLACE LINK WITH RECNO("CUS_LIST")
  ENDIF
  SELECT CUS_LIST
ENDSCAN

IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Ch_Sh        ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                   �襦�� ├ �皰÷������ �� ���皀�痰�.                   �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 21.11.2000 陳�
PROCEDURE Ch_Sh

PRIVATE r_sav
SELECT CUS_LIST
r_sav = RECNO()
SET ORDER TO
SELECT LINK_TMP
SET RELATION TO LINK INTO CUS_LIST
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
ln = MIN(WROWS("")-11, RECCOUNT("LINK_TMP"))   && ����腑痰〓 ※え�諷 痰牀� BROWSE
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
    BROWSE FIELDS CUS_LIST.CUS_NAME:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && �襦��

    USE
    SELECT CUS_LIST
    SET ORDER TO CUS_NAME
    EXIT

  OTHERWISE

    USE
    SELECT CUS_LIST
    SET ORDER TO CUS_NAME
    IF BETWEEN(r_sav, 1, RECCOUNT())
      GO r_sav
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
DELETE FILE (tmpo_path+"LINK_TMP.DBF")

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Modi_Sto     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                    ��玻オ罔� ���キ���矣琺 ��┘���.                     �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 21.11.2000 陳�
PROCEDURE Modi_Sto

DELETE FILE (tmpo_path+"FOR_DEL.DBF")
DELETE FILE (tmpo_path+"FOR_DEL.CDX")

CREATE DBF (tmpo_path+"FOR_DEL.DBF") ;
  ( LINK N(10) )
INDEX ON LINK TAG LINK

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE

SELECT 0
USE (base_path+"TEACH") ORDER TAG NAME
SET RELATION TO CODE INTO STOCK
SET RELATION TO WHO INTO PERSONS ADDITIVE
SET RELATION TO RECNO() INTO FOR_DEL ADDITIVE

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

DEFINE PAD Del OF (menu_name) PROMPT "�����碎/��瘁�." KEY Ctrl-A, "Tab"
ON SELECTION PAD Del OF (menu_name) DO Sw_Mode WITH "Del"

DEFINE PAD Seek OF (menu_name) PROMPT "����. ���瓷" KEY Ctrl-A, "F7"
ON SELECTION PAD Seek OF (menu_name) DO Sw_Mode WITH "Seek"

DEFINE PAD Save OF (menu_name) PROMPT "��縲���碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Save OF (menu_name) DO Sw_Mode WITH "Save"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
PRIVATE mss
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 63

DO D_Wins WITH ln, wd, ALLTRIM(tmpCusName), 0, 2
*敖陳陳���━キ����┘ ��┘���陳陳陳陳陳���━キ����┘陳陳陳陳陳陳陳�
*�! XXXX xxxxXxxxxXxxxxXxxxxXxxxxX XXXX xxxxXxxxxXxxxxXxxxxXxxxxX�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*敖陳陳陳陳陳陳���爼�陳陳陳陳陳陳陳陳陳陳陳陳陳��爼�陳陳陳陳陳陳朕
*�             xxxxXxxx                       xxxxXxxx           �
*� ��Ж罔� ���メキ�                                              �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
@ 1, 2 SAY "陳陳陳��━キ����┘ ��┘���陳陳陳陳陳���━キ����┘"
@ WROWS()-4, 2 SAY "陳陳陳陳陳陳陳��爼�陳陳陳陳陳陳陳陳陳陳陳陳陳��爼�"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F7    DO Sw_Mode WITH "Seek"
    ON KEY LABEL Tab   DO Sw_Mode WITH "Del"

*
*    BROWSE - �キ�
*
    BROWSE KEY STR(tmpCusCode)         ;
           FIELDS ToDel = IIF(FOUND("FOR_DEL"), "�", " "):1:H="",  ;
                  PREFIX:H="",         ;
                  Brw_Name=Nm_Part(NAME):25:H="",           ;
                  STOCK.PREFIX:H="",   ;
                  Brw_Name1=Nm_Part(STOCK.NAME):25:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Save")
    ENDIF

  CASE what_do = "Save"    && 蹍甃�矗 甎�瓷�

    EXIT

  CASE what_do = "Seek"    && ���瓷

    DO S_Sh
    what_do = "List"

  CASE what_do = "Del"     && �����碎/〓瘁����※碎

    IF FOUND("FOR_DEL")
      SELECT FOR_DEL
      REPLACE LINK WITH 0
      SELECT TEACH
    ELSE
      SELECT FOR_DEL
      GO TOP
      IF LINK # 0 .OR. RECCOUNT() = 0
        APPEND BLANK
      ENDIF
      REPLACE LINK WITH RECNO("TEACH")
      SELECT TEACH
    ENDIF

    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO
SET RELATION TO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   ���窶�皚キ讓� (���窶�皚キ讓�!) ���琺��ガ DBF-�����.
*
SELECT PERSONS
USE
SELECT STOCK
USE
SELECT FOR_DEL
IF what_do = "Save"
  SCAN FOR LINK # 0
    SELECT TEACH
    GO FOR_DEL.LINK
    REPLACE CUS_CODE WITH 0
    SELECT FOR_DEL
  ENDSCAN
ENDIF
USE
DELETE FILE (tmpo_path+"FOR_DEL.DBF")
DELETE FILE (tmpo_path+"FOR_DEL.CDX")
SELECT TEACH
USE

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
@ WROWS()-3, 15 SAY PRODUCER
@ WROWS()-3, 46 SAY STOCK.PRODUCER
@ WROWS()-2,  3 SAY "��Ж罔� ���メキ� "+DTOC(WHEN)+" "+ ;
   PADR(ALLTRIM(PERSONS.FAMILY)+" "+LEFT(PERSONS.NAME,1)+" "+LEFT(PERSONS.S_NAME,1), 30)
ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� S_Sh         ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           ���皀�痰�覃 ���瓷.                           �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 21.11.2000 陳�
PROCEDURE S_Sh

PRIVATE shab, mss

shab = Get_Shab()
IF EMPTY(shab)
  RETURN
ENDIF

DO Pr_Tmp

IF RECCOUNT("LINK_TMP") = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��腑�� �� ���ぅ��!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SELECT LINK_TMP
  USE
  DELETE FILE (tmpo_path+"LINK_TMP.DBF")
  SELECT TEACH
ELSE
  DO C_Sh
ENDIF

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Pr_Tmp       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      ��ぃ�皰→� ∇ガキ���� �����.                      �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 21.11.2000 陳�
PROCEDURE Pr_Tmp

PRIVATE r_sav

SELECT 0
DELETE FILE (tmpo_path+"LINK_TMP.DBF")
CREATE DBF (tmpo_path+"LINK_TMP.DBF") ( LINK N(10) )
SELECT TEACH
r_sav = RECNO()
SEEK STR(tmpCusCode)
SCAN REST WHILE CUS_CODE = tmpCusCode
  IF shab $ PREFIX+NAME+PRODUCER
    SELECT LINK_TMP
    APPEND BLANK
    REPLACE LINK WITH RECNO("TEACH")
  ENDIF
  SELECT TEACH
ENDSCAN

IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� C_Sh         ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                   �襦�� ├ �皰÷������ �� ���皀�痰�.                   �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 21.11.2000 陳�
PROCEDURE C_Sh

PRIVATE r_sav
SELECT TEACH
r_sav = RECNO()
SET ORDER TO
SELECT LINK_TMP
SET RELATION TO LINK INTO TEACH
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
ln = MIN(WROWS("")-11, RECCOUNT("LINK_TMP"))   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 39

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
    BROWSE FIELDS TEACH.PREFIX:H="",   ;
                  Brw_Name=Nm_Part(TEACH.NAME):25:H="",     ;
                  TEACH.PRODUCER:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && �襦��

    USE
    SELECT TEACH
    SET ORDER TO TAG NAME
    EXIT

  OTHERWISE

    USE
    SELECT TEACH
    SET ORDER TO NAME
    IF BETWEEN(r_sav, 1, RECCOUNT())
      GO r_sav
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
DELETE FILE (tmpo_path+"LINK_TMP.DBF")

RETURN
