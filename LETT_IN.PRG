*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Lett_In      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                      蹍甃�矗 √�わ薑� 甌�♂キ┤                       �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 28.10.2005 様�
PROCEDURE Lett_In
PARAMETERS prmUser, prmArm

PRIVATE tmpUser, tmpArm, tmpBlank
m.tmpUser = IIF(.NOT. EMPTY(m.prmUser), m.prmUser, m.user)
m.tmpArm  = IIF(.NOT. EMPTY(m.prmArm ), m.prmArm , m.arm )

PRIVATE tmpKey, tmpSw, whoName, whereName
m.tmpSw = .T.


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
DO Use_Dummy

SELECT 0
USE (base_path+"PERSONS") SHARED AGAIN ORDER TAG CODE ALIAS TMP_PRS
SEEK m.tmpUser
m.whoName = ALLTRIM(FAMILY)+" "+LEFT(NAME, 1)+" "+LEFT(S_NAME, 1)
m.whoName = PADR(m.whoName, 40)

SELECT 0
USE (base_path+"ARM_LST") SHARED AGAIN ORDER TAG ARM_C ALIAS TMP_ARM
SEEK m.tmpArm
m.whereName = "��� "+ALLTRIM(ARM_N)
m.whereName = PADR(m.whereName, 40)

USE (base_path+"LETT_OUT") SHARED AGAIN ORDER TAG MESS_ID ALIAS TMP_OUT
SET RELATION TO WHO INTO TMP_PRS

SELECT 0
USE (base_path+"LETT_IN") SHARED AGAIN ORDER TAG ADDRESS DESCENDING ALIAS TMP_IN
SET RELATION TO MESS_ID INTO TMP_OUT

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Read OF (menu_name) PROMPT "蹍腮��碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Read OF (menu_name) DO Sw_Mode WITH "Read"

DEFINE PAD Address OF (menu_name) PROMPT "���竍�皀��" KEY Ctrl-A, "Tab"
ON SELECTION PAD Address OF (menu_name) DO Sw_Mode WITH "Address"

DEFINE PAD Answer OF (menu_name) PROMPT "�癶モ�碎" KEY Ctrl-A, "Ins"
ON SELECTION PAD Answer OF (menu_name) DO Sw_Mode WITH "Answer"

DEFINE PAD Delete OF (menu_name) PROMPT "�����碎" KEY Ctrl-A, "Del"
ON SELECTION PAD Delete OF (menu_name) DO Sw_Mode WITH "Delete"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
m.ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
m.wd = 52

DO D_Wins WITH m.ln, m.wd, "�絎わ薑� 甌�♂キ��", 1, 1
m.what_do = "List"

@ 1, 2 SAY "���竍�皀��"
@ 2, 2 SAY ;
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
    IF m.tmpSw
      @ 1, 13 SAY m.whoName COLOR SCHEME 1
      m.tmpKey = "  "+STR(m.tmpUser, 5)
    ELSE
      @ 1, 13 SAY m.whereName COLOR SCHEME 1
      m.tmpKey = " *"+PADR(m.tmpArm, 5)
    ENDIF
    
    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Tab   DO Sw_Mode WITH "Address"
    ON KEY LABEL Ins   DO Sw_Mode WITH "Answer"
    ON KEY LABEL Del   DO Sw_Mode WITH "Delete"

*
*    BROWSE - �キ�
*
    BROWSE KEY m.tmpKey REST    ;
           FIELDS TMP_OUT.TITLE:H="",    ;
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
      DO LettView WITH MESS_ID
    ENDIF
    m.what_do = "List"

  CASE m.what_do = "Answer"  && �癶モ�碎

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO Lett_Ans WITH TMP_OUT.WHO
    ENDIF
    m.what_do = "List"

  CASE m.what_do = "Delete"  && �����碎

    IF .NOT. BOF() .AND. .NOT. EOF()
      SCATTER TO tmpBlank BLANK
      GATHER FROM tmpBlank
    ENDIF
    m.what_do = "List"

  CASE m.what_do = "Address" && ���竍�皀��

    m.tmpSw = .NOT. m.tmpSw
    GO TOP
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


