*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� GetArm       ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                         �襦�� ��｀腑�� �メ��.                          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 18.04.2000 様�
PROCEDURE GetArm
PARAMETERS startV

PRIVATE tmpPoint, armPoint, s_sav, retVal

s_sav = SELECT()
SELECT 0

tmpPoint = 1
DELETE FILE (tmpo_path+"SQUARES.DBF")
DELETE FILE (tmpo_path+"SQUARES.CDX")

CREATE DBF (tmpo_path+"SQUARES") ;
  ( SQ_CHAR C( 1), ;
    SQ_NAME C(12)  )
INDEX ON SQ_CHAR TAG SQ_CHAR
APPEND BLANK
REPLACE SQ_NAME WITH "�瓮 �����お�"

SELECT 0
DELETE FILE (tmpo_path+"GETARM.DBF")
DELETE FILE (tmpo_path+"GETARM.CDX")

CREATE DBF (tmpo_path+"GETARM") ;
  ( SQ_CHAR  C( 1), ;
    ARM_CODE N( 4), ;
    ARM_NAME C(16)  )
INDEX ON SQ_CHAR+SYS(15, lwr, ARM_NAME)  TAG SQ_CHAR
INDEX ON SYS(15, lwr, ARM_NAME) TAG ARM_NAME ADDITIVE

SELECT 0
USE (base_path+"ARM_LST") ALIAS ARMS_0418 AGAIN
SCAN
  IF .NOT. EMPTY(ARM_S)
    IF .NOT. SEEK(ARM_S, "SQUARES")
      SELECT SQUARES
      APPEND BLANK
      REPLACE SQ_CHAR WITH ARMS_0418.ARM_S, ;
              SQ_NAME WITH "ç��お� "+ARMS_0418.ARM_S
      IF sys_char = SQ_CHAR
        tmpPoint = RECNO()
      ENDIF
    ENDIF
  ENDIF
  SELECT GETARM
  APPEND BLANK
  REPLACE SQ_CHAR  WITH ARMS_0418.ARM_S,    ;
          ARM_CODE WITH ARMS_0418.ARM_CODE, ;
          ARM_NAME WITH ARMS_0418.ARM_N
  IF TYPE("startV") = "N"
    IF startV = ARM_CODE
      armPoint = RECNO()
    ENDIF
  ENDIF
  SELECT ARMS_0418
ENDSCAN

USE
SELECT SQUARES
GO tmpPoint
SELECT GETARM
IF EMPTY(SQUARES.SQ_CHAR)
  SET ORDER TO TAG ARM_NAME
ELSE
  SET ORDER TO TAG SQ_CHAR
ENDIF

IF TYPE("armPoint") = "N"
  GO armPoint
  IF SQ_CHAR # SQUARES.SQ_CHAR
    GO 1 IN SQUARES
  ENDIF
ELSE
  GO TOP
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
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*

SELECT 0
DO Use_Dummy
SELECT GETARM

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�襦��碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Square OF (menu_name) PROMPT "ç��お�" KEY Ctrl-A, "F1"
ON SELECTION PAD Square OF (menu_name) DO Sw_Mode WITH "Square"

DEFINE PAD Exit OF (menu_name) PROMPT "�皖���碎瘴" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = FSIZE("ARM_NAME")

DO D_Wins WITH ln, wd, "��｀腮� �メ��", 1, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    ACTIVATE WINDOW (win_name+"_M") SAME
    @ 1, 2 SAY SQUARES.SQ_NAME
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F1    DO Sw_Mode WITH "Square"

*
*    BROWSE - �キ�
*
    BROWSE KEY ALLTRIM(SQUARES.SQ_CHAR);
           FIELDS ARM_NAME:H="",       ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && �襦��碎

    retVal = ARM_CODE
    EXIT

  CASE what_do = "Square"    && ç��お�

    DO Get_Sq
    what_do = "List"

  OTHERWISE

    retVal = IIF(TYPE("startV") = "N", startV, -1)
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
SELECT SQUARES
USE

DELETE FILE (tmpo_path+"SQUARES.DBF")
DELETE FILE (tmpo_path+"SQUARES.CDX")
DELETE FILE (tmpo_path+"GETARM.DBF")
DELETE FILE (tmpo_path+"GETARM.CDX")

SELECT (s_sav)

RETURN retVal

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Get_Sq       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                            �襦�� �����お�.                             �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 18.04.2000 陳�
PROCEDURE Get_Sq

PRIVATE r_sav
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

s_sav = SELECT()

SELECT SQUARES

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�謖��碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "�皖���碎瘴" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = WROWS("")-18   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = FSIZE("SQ_NAME")

DO D_Wins WITH ln, wd, "ç��お�", 0, 0
what_do = "List"

r_sav = RECNO()
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE FIELDS SQ_NAME:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && 蹍甃�矗 甎�瓷�

    IF r_sav # RECNO()
      SELECT GETARM
      IF EMPTY(SQUARES.SQ_CHAR)
        SET ORDER TO TAG ARM_NAME
      ELSE
        SET ORDER TO TAG SQ_CHAR
      ENDIF
      GO TOP IN GETARM
    ENDIF
    EXIT

  OTHERWISE

    GO r_sav
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
