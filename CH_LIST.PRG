*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Ch_List      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                     �襦�� 甎�瓷� �� ��｀腑� �メ皀                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 07.09.2006 様�
PROCEDURE Ch_List
PARAMETERS prmArmC


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

PRIVATE tmpArmN, retValue, seekShab, seekMode, tmpShab, tmpRec
m.seekShab = SPACE(40)
m.seekMode = 1
m.s_sav = SELECT()
m.retValue = "      "

SELECT 0
DO Use_Dummy
SELECT 0
USE (m.base_path+"ARM_LST") SHARED AGAIN ALIAS AL_6907 ORDER TAG ARM_C
SEEK m.prmArmC
m.tmpArmN = ALLTRIM(ARM_N)
USE (m.base_path+"LIST_TIT") SHARED AGAIN ALIAS LT_6907 ORDER TAG LST_ARM
USE (m.base_path+"ACCOUNT") SHARED AGAIN ALIAS AC_6907 ORDER TAG CUS_CODE IN 0
SET RELATION TO CLI_CODE INTO AC_6907
PRIVATE mss
IF .NOT. SEEK(m.prmArmC)
  DIMENSION mss[4]
  mss[1] = ""
  mss[2] = CHR(0)+'�� ��｀腮� �メ皰� "'+m.tmpArmN+'"'
  mss[3] = CHR(0)+"�� 腮甄�矚� �� �き��� 甎�瓷�!"
  mss[4] = ""
  DO Out_Mess WITH 7, "mss"
  USE
  USE IN AC_6907
  SELECT (m.s_sav)
  RETURN m.retValue
ENDIF

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�襦��碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Seek OF (menu_name) PROMPT "���瓷" KEY Ctrl-A, "F7"
ON SELECTION PAD Seek OF (menu_name) DO Sw_Mode WITH "Seek"

DEFINE PAD View OF (menu_name) PROMPT "��ぅ爨━��" KEY Ctrl-A, "Tab"
ON SELECTION PAD View OF (menu_name) DO Sw_Mode WITH "View"

DEFINE PAD Exit OF (menu_name) PROMPT "�皖���碎瘴" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
m.ln = WROWS("")-5   && ����腑痰〓 ※え�諷 痰牀� BROWSE
m.wd = 76

DO D_Wins WITH m.ln, m.wd, '���瓷� ��｀腑�� �メ�� "'+m.tmpArmN+'"', 0, 0
m.what_do = "List"
@ 1, 2 SAY ;
"�爼陳���ム陳陳����陳陳���┘�眥陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�祠��陳陳陳�"
*FFF 999999 99.99.9999 ....:....!....:....!....:....!....:. 9999999999.9999 �

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL F7    DO Sw_Mode WITH "Seek"
    ON KEY LABEL Tab   DO Sw_Mode WITH "View"
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE KEY m.prmArmC ;
           FIELDS FIRM:H="",		;
                  LST_NUM:H="",		;
                  CR_DAT:H="",		;
                  tmpCust = LEFT(AC_6907.CUS_NAME, 36):H="",;
                  R_TOTAL:H="",		;
                  tmpSaled = IIF(EMPTY(SAL_NUM), " ", "�"):1:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Choose"    && 蹍甃�矗 甎�瓷�

    m.retValue = STR(LST_NUM, 6)
    EXIT

  CASE m.what_do = "View"      && ��ぅ爨━�� 甎�瓷�

    DO ViewList WITH LST_NUM
    m.what_do = "List"

  CASE m.what_do = "Seek"      && ���瓷

    DO Seek_Lst
    IF .NOT. EMPTY(m.seekShab) .AND. m.seekMode # 0
      m.tmpShab = ALLTRIM(m.seekShab)
      m.tmpShab = SYS(15, m.lwr, m.tmpShab)
      m.tmpRec = RECNO()
      IF m.seekMode = 1
        SEEK m.prmArmC
      ELSE
        SKIP
        IF EOF()
          GO BOTTOM
        ENDIF
      ENDIF
      LOCATE REST FOR m.tmpShab $ SYS(15, m.lwr, AC_6907.CUS_NAME)
      IF .NOT. FOUND()
        DIMENSION mss[3]
        mss[1] = ""
        mss[2] = CHR(0)+"��腑�� �� ���ぅ��..."
        mss[3] = ""
        DO Out_Mess WITH 7, "mss"
        GO m.tmpRec
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
USE IN AC_6907
USE IN LT_6907
SELECT (m.s_sav)

RETURN m.retValue

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Seek_Lst     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                        ���瓷 甎�瓷� �� ��┘�矣                         �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 07.09.2006 陳�
PROCEDURE Seek_Lst
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� 敖 �皰 �薀� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕 �
*� �....:....!....:....!....:....!....:....!� �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�   ( ) �瓷�碎 � ������  ( ) �瓷�碎 ���ゥ    �
*�     < OK Ctrl-W > < �皖���碎瘴 Esc >       �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex
m.ex = 1
m.seekMode = MAX(m.seekMode, 1)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 8, 48, "���瓷 甎�瓷� �� ��┘�矣"
DO Sun_Bord WITH  2, 3,  4, 44, " �皰 �薀� "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 3, 4 GET m.seekShab PICTURE "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
@ 5, 5 GET m.seekMode PICTURE "@*RH �瓷�碎 � ������;�瓷�碎 ���ゥ"

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

IF m.ex # 1
  m.seekMode = 0
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
