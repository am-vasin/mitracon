*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� EnabList     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�   ���甌� ��｀腮� �メ�, ぎ痰祚�諷 ���讌���皀�� か� ━��珥�/蹣甎�珥�.    �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 15.09.2006 様�
PROCEDURE EnabList
PARAMETERS prmUser, prmArmN

PRIVATE tmpUser, mss

m.tmpUser = IIF(EMPTY(m.prmUser), m.user, m.prmUser)
USE (m.base_path+"LET_LIST") SHARED AGAIN ALIAS LL_6915 ORDER TAG USER_CODE IN 0
IF .NOT. SEEK(STR(m.tmpUser), "LL_6915")
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"��� �� ぎ痰祚�� 甎�瓷� �� �き��� ��｀腑�� �メ��"
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  USE IN LL_6915
  RETURN "   "
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
USE (m.base_path+"ARM_LST") SHARED AGAIN ALIAS ARM_6915 ORDER TAG NAME
SET RELATION TO STR(m.tmpUser)+ARM_C INTO LL_6915
SET FILTER TO FOUND("LL_6915")
GO TOP
IF BOF() .OR. EOF()
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"��� �� ぎ痰祚�� 甎�瓷� �� �き��� ��｀腑�� �メ��"
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  USE IN LL_6915
  USE IN ARM_6915
  SELECT (m.s_sav)
  RETURN "   "
ENDIF

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�襦��碎" KEY Ctrl-A, "Ctrl-Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "�皖���碎瘴" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd, retV
m.retV = "   "
m.ln = WROWS("")-7   && ����腑痰〓 ※え�諷 痰牀� BROWSE
m.wd = 16

DO D_Wins WITH m.ln, m.wd, "��痰. ��｀腮� �メ��", 0, 0
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE FIELDS ARM_N:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Choose"    && 蹍甃�矗 甎�瓷�

    m.retV = ARM_C
    IF TYPE("m.prmArmN") = "C"
      m.prmArmN = ARM_N
    ENDIF
    EXIT
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
USE IN LL_6915
USE IN ARM_6915
SELECT (m.s_sav)

RETURN m.retV
