*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Lett_Out     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                          �痳�わ薑� 甌�♂キ��.                          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 19.10.2005 様�
PROCEDURE Lett_Out
PARAMETERS prmWho

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
PRIVATE tmpWho, tmpRc

IF TYPE("m.prmWho") = "N"
  m.tmpWho = m.prmWho
ELSE
  m.tmpWho = m.user
ENDIF

m.s_sav = SELECT()

SELECT 0
DO Use_Dummy

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE AGAIN ALIAS PR_5A19

SELECT 0
USE (base_path+"ARM_LST") ORDER TAG ARM_C AGAIN ALIAS ARM_5A19

SELECT 0
USE (base_path+"LETT_IN") ORDER TAG MESS_ID AGAIN ALIAS IN_5A19

SELECT 0
USE (base_path+"LETT_OUT") ORDER TAG WHO DESCENDING AGAIN ALIAS OUT_5A19
SET RELATION TO MESS_ID INTO IN_5A19

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Text OF (menu_name) PROMPT "�オ痰 甌�♂キ��" KEY Ctrl-A, "Enter"
ON SELECTION PAD Text OF (menu_name) DO Sw_Mode WITH "Text"

DEFINE PAD For_Who OF (menu_name) PROMPT "�むメ�硅" KEY Ctrl-A, "Tab"
ON SELECTION PAD For_Who OF (menu_name) DO Sw_Mode WITH "For_Who"

DEFINE PAD Create OF (menu_name) PROMPT "��Г�碎" KEY Ctrl-A, "Ins"
ON SELECTION PAD Create OF (menu_name) DO Sw_Mode WITH "Create"

*DEFINE PAD Delete OF (menu_name) PROMPT "�����碎" KEY Ctrl-A, "Del"
*ON SELECTION PAD Delete OF (menu_name) DO Sw_Mode WITH "Delete"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd, d_name, ln_1
m.ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
m.ln_1 = FSIZE("TITLE")
m.wd = m.ln_1+20

DO D_Wins WITH m.ln, m.wd, "�痳�わ薑� 甌�♂キ��", 0, 1
@ 1, 2 SAY "�ガ�"
@ 1, WCOLS()-21 SAY "����"
@ WROWS()-3, WCOLS()-21 SAY "����"
@ 1, WCOLS()-10 SAY "�爛��"
@ WROWS()-3, WCOLS()-10 SAY "�爛��"

what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    ACTIVATE SCREEN
    @ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 2
    SHOW MENU (m.menu_name)
    ACTIVATE WINDOW (win_name+"_M") SAME
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Tab DO Sw_Mode WITH "For_Who"
    ON KEY LABEL Ins DO Sw_Mode WITH "Create"
*    ON KEY LABEL Del DO Sw_Mode WITH "Delete"

*
*    BROWSE - �キ�
*
    BROWSE KEY STR(m.tmpWho) REST ;
           FIELDS TITLE:H="",   ;
                  CR_DATE:H="", ;
                  CR_TIME:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Text")
    ENDIF

  CASE what_do = "Text"			&& 蹍甃�矗 甌�♂キ��

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO LettView WITH MESS_ID
    ENDIF
    what_do = "List"

  CASE what_do = "For_Who"		&& �むメ�硅

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO Lett_Who WITH MESS_ID
    ENDIF
    what_do = "List"

  CASE what_do = "Delete"		&& �����碎

    IF .NOT. BOF() .AND. .NOT. EOF()
      wait wind what_do
    ENDIF
    what_do = "List"

  CASE what_do = "Create"		&& ��Г�碎

    m.tmpRc = Lett_Cr()
    IF BETWEEN(m.tmpRc, 1, RECCOUNT())
      GO m.tmpRc
    ENDIF
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
USE IN PR_5A19
USE IN ARM_5A19
USE IN IN_5A19
USE IN OUT_5A19

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
PRIVATE tmpAddr

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� Гメ�, 爬痺ガ...
*

IF EMPTY(IN_5A19.ADDRESS_T)
  @ WROWS()-3, 3 SAY "����"
  = SEEK(VAL(IN_5A19.ADDRESS), "PR_5A19")
  m.tmpAddr = ALLTRIM(PR_5A19.FAMILY)+" "+LEFT(PR_5A19.NAME, 1)+" "+LEFT(PR_5A19.S_NAME, 1)
ELSE
  @ WROWS()-3, 3 SAY "�磴�"
  = SEEK(LEFT(IN_5A19.ADDRESS, FSIZE("ARM_C", "ARM_5A19")), "ARM_5A19")
  m.tmpAddr = ARM_5A19.ARM_N
ENDIF
m.tmpAddr = PADR(m.tmpAddr, ln_1)
@ WROWS()-2, 2 SAY m.tmpAddr
@ WROWS()-2, COL()+1 SAY IN_5A19.SHOW_DATE
@ WROWS()-2, COL()+1 SAY IN_5A19.SHOW_TIME

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.
