*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Brend_Sp     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                  �爛�る 甌 甎ユ���讚覓� ����腑痰����                   �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 10/24/2008 様�
PROCEDURE Brend_Sp

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
PRIVATE tmpShab, tmpRec, mss

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
USE (m.base_path+"BREND_SP") SHARED AGAIN ORDER TAG CODE
SET FILTER TO CODE # 0
USE (m.base_path+"BREND_SP") SHARED AGAIN ORDER TAG CODE ALIAS MODI IN 0
SELECT 0
USE (m.base_path+"DEVELOP") SHARED AGAIN ORDER TAG B_NAME
SET RELATION TO CODE INTO BREND_SP

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Modi OF (menu_name) PROMPT "��玻オ罔�" KEY Ctrl-A, "Enter"
ON SELECTION PAD Modi OF (menu_name) DO Sw_Mode WITH "Modi"

DEFINE PAD Seek OF (menu_name) PROMPT "���瓷" KEY Ctrl-A, "F7"
ON SELECTION PAD Seek OF (menu_name) DO Sw_Mode WITH "Seek"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
m.ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
m.wd = 15

DO D_Wins WITH m.ln, m.wd, "�爛�る", 0, 0
@ 1, 2 SAY "���鐵陳陳���-〓"
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F7 DO Sw_Mode WITH "Seek"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS tmpName = LEFT(B_NAME, 8):H="",	;
                  BREND_SP.QNT:H="":P="@Z",			;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")

*           WHEN Draw_Foot()    ;
*

    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Modi")
    ENDIF

  CASE m.what_do = "Modi"    && ��玻オ罔�

    DO Sp_Modi
    m.what_do = "List"

  CASE m.what_do = "Seek"    && ���瓷

    m.tmpShab = Get_S(SPACE(8))
    IF .NOT. EMPTY(m.tmpShab)
      m.tmpRec = RECNO()
      IF .NOT. SEEK(m.tmpShab)
        DIMENSION mss[3]
        mss[1] = ""
        mss[2] = CHR(0)+"��腑�� �� ���ぅ��..."
        mss[3] = ""
        DO Out_Mess WITH 7, "mss"
        IF BETWEEN(m.tmpRec, 1, RECCOUNT())
          GO m.tmpRec
        ENDIF
      ENDIF
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
USE IN DEVELOP
USE IN BREND_SP
USE IN MODI

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

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Sp_Modi      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                   ��玻オ罔�                                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 23.10.2008 陳�
PROCEDURE Sp_Modi

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
m.stat_type  = 1
m.what_do    = ""
m.menu_name  = ""
m.last_mouse = 0
m.win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ��ぅ爨�皀�讚�� ��痰� �牀������:
*

*   ��玻オ罔�
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�                                  �
*�   ����〓膈�� ����腑痰〓 99999    �
*�                                  �
*� < OK Ctrl-W > < �皖���碎瘴 Esc > �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex
m.ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 7, 38, "��玻オ罔�"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

m.tmpQnt = 0
IF SEEK(DEVELOP.CODE, "BREND_SP")
  m.tmpQnt = BREND_SP.QNT
ENDIF

@ 3, 5 SAY "����〓膈�� ����腑痰〓" GET m.tmpQnt PICTURE "@Z 99999"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

IF m.ex = 1
  *
  * �矗���硅��ガ ゛���
  DO CASE
  CASE m.tmpQnt = 0 .AND. FOUND("BREND_SP")
    SELECT MODI
    GO RECNO("BREND_SP")
    REPLACE CODE WITH 0, QNT WITH 0
  CASE m.tmpQnt # 0 .AND. FOUND("BREND_SP")
    SELECT MODI
    GO RECNO("BREND_SP")
    REPLACE QNT WITH m.tmpQnt
  CASE m.tmpQnt # 0 .AND. .NOT. FOUND("BREND_SP")
    SELECT MODI
    GO TOP
    IF CODE # 0 .OR. BOF() .OR. EOF()
      APPEND BLANK
    ENDIF
    REPLACE CODE WITH DEVELOP.CODE, QNT WITH m.tmpQnt
  ENDCASE
  SELECT DEVELOP
  IF .NOT. BOF() .AND. .NOT. EOF()
    GO RECNO()
  ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
