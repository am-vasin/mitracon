*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Lett_Get      ��о�｀砒┴ ��むォ ��瓱�                     �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                      ���竍キ┘ √�わ薑� 甌�♂キ┤                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 28.10.2005 様�
PROCEDURE Lett_Get
PARAMETERS prmUser, prmArm

PRIVATE tmpUser, tmpArm, tmpBlank, tmpName
m.tmpUser = IIF(.NOT. EMPTY(m.prmUser), m.prmUser, m.user)
m.tmpArm  = IIF(.NOT. EMPTY(m.prmArm ), m.prmArm , m.arm )
m.tmpName = SYS(3)

m.s_sav = SELECT()

IF .NOT. Is_Lett()
  RETURN
ENDIF
GO TOP IN TMP_LIST

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

SELECT 0
DO Use_Dummy

SELECT 0
USE (base_path+"PERSONS") SHARED AGAIN ORDER TAG CODE ALIAS TMP_PRS

SELECT 0
USE (base_path+"LETT_OUT") SHARED AGAIN ORDER TAG MESS_ID ALIAS TMP_OUT
SET RELATION TO WHO INTO TMP_PRS

SELECT 0
USE (base_path+"LETT_IN") SHARED AGAIN ALIAS TMP_IN
SET RELATION TO MESS_ID INTO TMP_OUT

SELECT TMP_LIST
SET RELATION TO LINK INTO TMP_IN

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Read OF (menu_name) PROMPT "蹍腮��碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Read OF (menu_name) DO Sw_Mode WITH "Read"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
m.ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
m.wd = 52

DO D_Wins WITH m.ln, m.wd, "��� ��� ��痰祚┼� 甄イ竡薑� 甌�♂キ��", 0, 1
m.what_do = "List"

@ 1, 2 SAY ;
 "�ガ�陳陳陳陳陳陳陳陳陳陳陳陳陳陳��皴��←キ�"
*X....:....!....:....!....:....!..X��.��.����X��:��:��X
@ WROWS()-3, 2 SAY "�皴��※皀��"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    ACTIVATE SCREEN
    @ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 2
    SHOW MENU (m.menu_name)
    ACTIVATE WINDOW (win_name+"_M") SAME
    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE FIELDS TMP_OUT.TITLE:H="",    ;
                  TMP_OUT.CR_DATE:H="",  ;
                  TMP_OUT.CR_TIME:H="",  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Read")
    ENDIF

  CASE m.what_do = "Read"    && 蹍甃�矗 甌�♂キ��

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO LettView WITH TMP_IN.MESS_ID
      REPLACE TMP_IN.READ_DATE WITH DATE(), ;
              TMP_IN.READ_TIME WITH TIME(), ;
              TMP_IN.WHO_READ  WITH m.user
      DELETE
      SKIP
      IF EOF()
        GO BOTT
      ENDIF
    ENDIF
    IF EOF() .OR. BOF()
      EXIT
    ENDIF
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
USE IN TMP_IN
USE IN TMP_OUT
USE IN TMP_PRS
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
PRIVATE tmpV

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� Гメ�, 爬痺ガ...
*
m.tmpV = ALLTRIM(TMP_PRS.FAMILY)+" "+LEFT(TMP_PRS.NAME, 1)+" "+LEFT(TMP_PRS.S_NAME, 1)
m.tmpV = PADR(m.tmpV, WCOLS()-6)
@ 	WROWS()-2, 3 SAY m.tmpV

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Is_Lett      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�        蹍▲爲� ����腮� �ク牀腮����諷 ��瓮� � ��ぃ�皰→� 甎�瓷�        �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 28.10.2005 陳�
PROCEDURE Is_Lett

PRIVATE s_sav, now

m.s_sav = SELECT()
m.now   = DTOS(DATE())+TIME()
SELECT 0
USE (m.base_path+"LETT_IN") SHARED AGAIN ALIAS TMP_L_5A28 ORDER TAG ADDRESS
IF SEEK("* "+STR(m.tmpUser, 5))
  IF DTOS(SHOW_DATE)+SHOW_TIME <= m.now
    DO Cr_Tmp
    SCAN REST WHILE EMPTY(READ_DATE) .AND. ADDRESS_T = " " .AND. ADDRESS = STR(m.tmpUser, 5) ;
                    .AND. DTOS(SHOW_DATE)+SHOW_TIME <= m.now
      SELECT TMP_LIST
      APPEND BLANK
      REPLACE LINK      WITH RECNO("TMP_L_5A28"), ;
              WHEN_SHOW WITH DTOS(TMP_L_5A28.SHOW_DATE)+TMP_L_5A28.SHOW_TIME
      SELECT TMP_L_5A28
    ENDSCAN
  ENDIF
ENDIF

m.tmpArm = PADR(m.tmpArm, 5)
IF SEEK("**"+m.tmpArm)
  IF DTOS(SHOW_DATE)+SHOW_TIME <= m.now
    DO Cr_Tmp
    SCAN REST WHILE EMPTY(READ_DATE) .AND. ADDRESS_T = "*" .AND. ADDRESS = m.tmpArm ;
                    .AND. DTOS(SHOW_DATE)+SHOW_TIME <= m.now
      SELECT TMP_LIST
      APPEND BLANK
      REPLACE LINK      WITH RECNO("TMP_L_5A28"), ;
              WHEN_SHOW WITH DTOS(TMP_L_5A28.SHOW_DATE)+TMP_L_5A28.SHOW_TIME
      SELECT TMP_L_5A28
    ENDSCAN
  ENDIF
ENDIF

USE
SELECT (m.s_sav)

RETURN USED("TMP_LIST")

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Cr_Tmp       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                       ��Г��┘ ∇ガキ���� �����                        �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 28.10.2005 陳�
PROCEDURE Cr_Tmp

PRIVATE s_sav

IF USED("TMP_LIST")
  RETURN
ENDIF

m.s_sav = SELECT()

SELECT 0

CREATE DBF(m.tmpo_path+m.tmpName) ;
	(	LINK		N(10), ;
		WHEN_sHOW	C(16)  )
USE (m.tmpo_path+m.tmpName) EXCLUSIVE ALIAS TMP_LIST
INDEX ON WHEN_SHOW TAG HRON

SELECT (m.s_sav)

RETURN
