*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� V_Note       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                         ���ଠ�� � ������.                          �
*�                                                                        �
*����������������������������������������������������������� 24.02.1999 �ͼ
PROCEDURE V_Note
PARAMETER let_m, parmCode
PRIVATE s_sav, tmpCode

s_sav = SELECT()

IF TYPE("parmCode") = "N"
  tmpCode = parmCode
ELSE
  tmpCode = ACCOUNT.CUS_CODE
ENDIF

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS ACC_TMP AGAIN
SEEK tmpCode

DO Prep_F
DO Prep_V
DO Get_Parms
SELECT F_LIST
USE
SELECT ACC_TMP
USE
DELETE FILE (tmpo_path+"F_LIST.DBF")
SELECT (s_sav)

RETURN

*������������������������������������������������������������������������ͻ
*�                                                                        �
*�                  ��ᬮ�� � ���४�� ���� ������.                   �
*�                                                                        �
*����������������������������������������������������� 25.05.97 20:09:11 ͼ
PROCEDURE Exi_Doss

IF doss_sw
  DO Dossier WITH ACC_TMP.CUS_CODE, IIF(let_m, "C", "")
  doss_sw = .F.
  SHOW GET doss_sw
ENDIF

RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Prep_F       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                     �����⮢�� ᯨ᪠ �।���⨩.                     �
*�                                                                        �
*����������������������������������������������������������� 16.02.1999 ���
PROCEDURE Prep_F
PRIVATE s_sav

s_sav = SELECT()
SELECT 0
DELETE FILE (tmpo_path+"F_LIST.DBF")
CREATE DBF (tmpo_path+"F_LIST")  ;
  ( F_CODE   C( 3),  ;
    F_NAME   C(16),  ;
    ACC_SUM  N(12,2),;
    ACC_SUMD N(12,4),;
    ACC_S_H  N(12,2),;
    ACC_SD_H N(12,4),;
    CR_SUM   N(12,2),;
    CRD_SUM  N(12,4),;
    ACC_S0   N(12,2),;
    ACC_SD0  N(12,4),;
    ACC_SH0  N(12,2),;
    ACC_SDH0 N(12,4),;
    CR_SUM0  N(12,2),;
    CRD_SUM0 N(12,4) )

SELECT 0
USE (base_path+"FIRMS") ORDER TAG NAME ALIAS F_9306 AGAIN
SCAN
  SELECT F_LIST
  APPEND BLANK
  REPLACE F_CODE WITH F_9306.FIRM_CODE,  ;
          F_NAME WITH F_9306.B_NAME
  SELECT F_9306
ENDSCAN
USE
SELECT (s_sav)

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Prep_V       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                    �����⮢�� ���祭�� ��� ������.                    �
*�                                                                        �
*����������������������������������������������������������� 16.02.1999 ���
PROCEDURE Prep_V

SELECT 0
USE (base_path+"CUS_REST") ORDER TAG CUS_CODE ALIAS CR_9306 AGAIN

SELECT F_LIST
SCAN
  IF .NOT. SEEK(STR(ACC_TMP.CUS_CODE)+F_LIST.F_CODE, "CR_9306")
    REPLACE ACC_SUM  WITH 0,;
            ACC_SUMD WITH 0,;
            ACC_S_H  WITH 0,;
            ACC_SD_H WITH 0,;
            CR_SUM   WITH 0,;
            CRD_SUM  WITH 0,;
            ACC_S0   WITH 0,;
            ACC_SD0  WITH 0,;
            ACC_SH0  WITH 0,;
            ACC_SDH0 WITH 0,;
            CR_SUM0  WITH 0,;
            CRD_SUM0 WITH 0
  ELSE
    REPLACE ACC_SUM  WITH CR_9306.ACC_SUM,  ;
            ACC_SUMD WITH CR_9306.ACC_SUMD, ;
            ACC_S_H  WITH CR_9306.ACC_S_H,  ;
            ACC_SD_H WITH CR_9306.ACC_SD_H, ;
            CR_SUM   WITH CR_9306.CR_SUM,   ;
            CRD_SUM  WITH CR_9306.CRD_SUM,  ;
            ACC_S0   WITH CR_9306.ACC_SUM,  ;
            ACC_SD0  WITH CR_9306.ACC_SUMD, ;
            ACC_SH0  WITH CR_9306.ACC_S_H,  ;
            ACC_SDH0 WITH CR_9306.ACC_SD_H, ;
            CR_SUM0  WITH CR_9306.CR_SUM,   ;
            CRD_SUM0 WITH CR_9306.CRD_SUM
  ENDIF
ENDSCAN

SELECT CR_9306
USE

SELECT F_LIST
RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Get_Parms    ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�             ����⢥��� ����� ���४樨 ��ࠬ��஢ ������.             �
*�                                                                        �
*����������������������������������������������������������� 16.02.1999 ���
PROCEDURE Get_Parms
*
*  ��६���� ���ﭨ� ��� ������樨
*
PRIVATE tmpMark1
PRIVATE stat_type     && ��� ������樨: 0 - ���⠭�����;
                                        1 - �����;
                                        2 - BROWSE - ⠡���;
                                        3 - BROWSE - ᯨ᮪.
PRIVATE what_do       && ��� ०���.
PRIVATE menu_name     && ��� �ᨭ�஭���� ����.
PRIVATE last_mouse    && �६� ��᫥����� ������ ���ਭ�� ������.
PRIVATE win_name      && ��� ���� ( ���� ��� BROWSE ).

*
*   ������塞 ���祭�ﬨ ��६���� ���ﭨ�...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ����ঠ⥫쭠� ���� �ணࠬ��:
*

*�������������������������������������������������������������Ŀ
*� �� [ ] <��� �������> ������������Ŀ  [ ] ����             �
*� ��� �ਬ�砭�� ������������������Ŀ�  [ ] �������� �⬥⮪  �
*� ��....:....!....:....!....:....!..��                        �
*� ����������������������������������ٳ  [ ] �⬥⪠ 1         �
*� �     �������   999 999 999.99     �  [ ] �⬥⪠ 2         �
*� �     ����ঠ�� 999 999 999.99     �  [ ] �⬥⪠ 3         �
*� �     �।��    999 999 999.99     �  [ ] �⬥⪠ 4         �
*� �       [ ] �� �।�����        �  [ ] �⬥⪠ 5         �
*� ������������������������������������                        �
*�        < OK Ctrl-W > < �⪠������ Esc >                     �
*���������������������������������������������������������������

*
*  ��騥 ��ࠬ����
*
PRIVATE p_nav      && ������樮���� ��६�����

PRIVATE d_type_sw  && ��४���⥫� ⨯� �������
PRIVATE d_type_nm  && ��� �������
PRIVATE doss_sw    && ������ ����
PRIVATE firm_sw    && ��४���⥫� �᪫���� �� �।�����
PRIVATE m_names    && ����� 䫠����
PRIVATE m_names_sw && ������ ���४樨 ���� 䫠����
PRIVATE sw1, sw2, sw3, sw4, sw5  && ���祭�� ��४���⥫��
m_names_sw = .F.
d_type_sw = .F.
d_type_nm = "�㡫��� "
&&          "������.  "
doss_sw   = .F.
firm_sw   = .F.
tmpMark1 = ACC_TMP.MARK1
IF File_O(base_path+"M_NAMES.MEM")
  RESTORE FROM (base_path+"M_NAMES.MEM") ADDITIVE
ELSE
  DIMENSION m_names(5)
  m_names(2) = "�⬥⪠ 2   "
  m_names(3) = "�⬥⪠ 3   "
  m_names(4) = "�⬥⪠ 4   "
  m_names(5) = "�⬥⪠ 5   "
ENDIF
m_names(1) = NameM1(tmpMark1)

sw1 = .F.
sw2 = .NOT. EMPTY(ACC_TMP.MARK2)
sw3 = .NOT. EMPTY(ACC_TMP.MARK3)
sw4 = .NOT. EMPTY(ACC_TMP.MARK4)
sw5 = .NOT. EMPTY(ACC_TMP.MARK5)

*
*  ��ࠬ���� �㡫����� �������
*
PRIVATE note_w_r   && �ਬ�砭��
PRIVATE s_r        && �㬬� �� �㡫���� �������
PRIVATE h_r        && ����ঠ�� �� �㡫���� �������
PRIVATE c_r        && �।�� �� �㡫���� �������
note_w_r = ACC_TMP.ACC_NOTE

*
*  ��ࠬ���� �������㥬��� �������
*
PRIVATE note_w_d   && �ਬ�砭��
PRIVATE s_d        && �㬬� �� �������㥬�� �������
PRIVATE h_d        && ����ঠ�� �� �������㥬�� �������
PRIVATE c_d        && �।�� �� �������㥬�� �������
note_w_d = ACC_TMP.ACC_NOTED

STORE 0 TO s_r, h_r, c_r, s_d, h_d, c_d
DO Sum_Dep

PRIVATE ex
ex = 1

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 13, 65, ALLTRIM(ACC_TMP.CUS_NAME)
DO Sun_Bord WITH  2,  3, 10, 38, "  "
DO Sun_Bord WITH  3,  4,  5, 37, " �ਬ�砭�� "
@  6,  9 SAY "�������"
@  7,  9 SAY "����ঠ��"
@  8,  9 SAY "�।��"
@  9, 15 SAY "�� �।�����"
@  2, 45 SAY "����"
@  3, 45 SAY "�������� �⬥⮪"
*------------------------------------------------------------------------
*      ���� ����� ������
*
p_nav = "RUB"
DO WHILE .T.
  DO CASE
  CASE p_nav = "RUB"
    @  2, 10 SAY d_type_nm
    @  2,  6 GET d_type_sw PICTURE "@*CT "
    @  4,  5 GET note_w_r WHEN let_m
    @  6, 19 GET s_r PICTURE "999 999 999.99" WHEN .F. COLOR (","+SCHEME(1,2))
    @  7, 19 GET h_r PICTURE "999 999 999.99" WHEN .F. COLOR (","+SCHEME(1,2))
    @  8, 19 GET c_r PICTURE "999 999 999.99" WHEN .F. COLOR (","+SCHEME(1,2))
    @  9, 11 GET firm_sw PICTURE "@*C " VALID Firms_D()
    @  2, 41 GET doss_sw PICTURE "@*C " VALID Exi_Doss()
    @  3, 41 GET m_names_sw PICTURE "@*C " VALID F_Names() WHEN let_m
    @  5, 45 GET m_names(1) WHEN .F. COLOR (","+SCHEME(1,2))
    @  6, 45 GET m_names(2) WHEN .F. COLOR (","+SCHEME(1,2))
    @  7, 45 GET m_names(3) WHEN .F. COLOR (","+SCHEME(1,2))
    @  8, 45 GET m_names(4) WHEN .F. COLOR (","+SCHEME(1,2))
    @  9, 45 GET m_names(5) WHEN .F. COLOR (","+SCHEME(1,2))
    @  5, 41 GET sw1 PICTURE "@*C " WHEN let_m VALID G_Face()
    @  6, 41 GET sw2 PICTURE "@*C " WHEN let_m
    @  7, 41 GET sw3 PICTURE "@*C " WHEN let_m
    @  8, 41 GET sw4 PICTURE "@*C " WHEN let_m
    @  9, 41 GET sw5 PICTURE "@*C " WHEN let_m
    IF let_m
      @ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "
    ELSE
      @ WROWS()-2, FLOOR(WCOLS()/2-6) GET ex PICTURE "@*HT \ OK "
    ENDIF
    READ CYCLE
    IF d_type_sw
      d_type_sw = .F.
      p_nav = "USD"
      d_type_nm = "������.  "
    ELSE
      IF ex = 1
        p_nav = "save"
      ELSE
        p_nav = "exit"
      ENDIF
    ENDIF
  CASE p_nav = "USD"
    @  2, 10 SAY d_type_nm
    @  2,  6 GET d_type_sw PICTURE "@*CT "
    @  4,  5 GET note_w_d
    @  6, 19 GET s_d PICTURE "9 999 999.9999" WHEN .F. COLOR (","+SCHEME(1,2))
    @  7, 19 GET h_d PICTURE "9 999 999.9999" WHEN .F. COLOR (","+SCHEME(1,2))
    @  8, 19 GET c_d PICTURE "9 999 999.9999" WHEN .F. COLOR (","+SCHEME(1,2))
    @  9, 11 GET firm_sw PICTURE "@*C " VALID Firms_D()
    @  2, 41 GET doss_sw PICTURE "@*C " VALID Exi_Doss()
    @  3, 41 GET m_names_sw PICTURE "@*C " VALID F_Names()
    @  5, 45 GET m_names(1) WHEN .F. COLOR (","+SCHEME(1,2))
    @  6, 45 GET m_names(2) WHEN .F. COLOR (","+SCHEME(1,2))
    @  7, 45 GET m_names(3) WHEN .F. COLOR (","+SCHEME(1,2))
    @  8, 45 GET m_names(4) WHEN .F. COLOR (","+SCHEME(1,2))
    @  9, 45 GET m_names(5) WHEN .F. COLOR (","+SCHEME(1,2))
    @  5, 41 GET sw1 PICTURE "@*C " VALID G_Face()
    @  6, 41 GET sw2 PICTURE "@*C "
    @  7, 41 GET sw3 PICTURE "@*C "
    @  8, 41 GET sw4 PICTURE "@*C "
    @  9, 41 GET sw5 PICTURE "@*C "
    IF let_m
      @ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "
    ELSE
      @ WROWS()-2, FLOOR(WCOLS()/2-6) GET ex PICTURE "@*HT \ OK "
    ENDIF
    READ CYCLE
    IF d_type_sw
      d_type_sw = .F.
      p_nav = "RUB"
      d_type_nm = "�㡫��� "
    ELSE
      IF ex = 1
        p_nav = "save"
      ELSE
        p_nav = "exit"
      ENDIF
    ENDIF
  CASE p_nav = "save"
    SELECT ACC_TMP
    REPLACE ACC_NOTE   WITH note_w_r,  ;
            ACC_NOTED  WITH note_w_d,  ;
            MARK1      WITH tmpMark1,  ;
            MARK2      WITH IIF(sw2, "X", " "),  ;
            MARK3      WITH IIF(sw3, "X", " "),  ;
            MARK4      WITH IIF(sw4, "X", " "),  ;
            MARK5      WITH IIF(sw5, "X", " ")
    SAVE ALL LIKE m_names TO (base_path+"M_NAMES")
    SELECT 0
    USE (base_path+"CUS_REST") ORDER TAG CUS_CODE ALIAS C_R_9306 AGAIN
    SELECT F_LIST
    SCAN
      IF .NOT. SEEK(STR(ACC_TMP.CUS_CODE)+F_CODE, "C_R_9306")
        IF ACC_SUMD = 0 .AND. ACC_S_H = 0 .AND.   ;
           ACC_SD_H = 0 .AND. CR_SUM = 0 .AND. CRD_SUM = 0
          LOOP
        ELSE
          SELECT C_R_9306
          APPEND BLANK
          REPLACE CUS_CODE WITH ACC_TMP.CUS_CODE, ;
                  FIRM     WITH F_LIST.F_CODE
        ENDIF
      ENDIF
      SELECT C_R_9306
      REPLACE ACC_SUM    WITH ACC_SUM+F_LIST.ACC_SUM-F_LIST.ACC_S0,     ;
              ACC_SUMD   WITH ACC_SUMD+F_LIST.ACC_SUMD-F_LIST.ACC_SD0,  ;
              ACC_S_H    WITH ACC_S_H+F_LIST.ACC_S_H-F_LIST.ACC_SH0,    ;
              ACC_SD_H   WITH ACC_SD_H+F_LIST.ACC_SD_H-F_LIST.ACC_SDH0, ;
              CR_SUM     WITH CR_SUM+F_LIST.CR_SUM-F_LIST.CR_SUM0,      ;
              CRD_SUM    WITH CRD_SUM+F_LIST.CRD_SUM-F_LIST.CRD_SUM0
      SELECT F_LIST
    ENDSCAN
    SELECT C_R_9306
    USE
    p_nav = "exit"
  OTHERWISE
    EXIT
  ENDCASE
ENDDO

*--------------------------------------------------------------------------
POP KEY
RELEASE WINDOW (win_name)
RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Sum_Dep      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                      �㬬� ������⮢ ��� ������.                      �
*�                                                                        �
*����������������������������������������������������������� 16.02.1999 ���
PROCEDURE Sum_Dep

SELECT F_LIST
SUM ACC_SUM, ACC_SUMD, ACC_S_H, ACC_SD_H, CR_SUM, CRD_SUM TO   ;
   s_r, s_d, h_r, h_d, c_r, c_d
RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� F_Names      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                        ���४�� ���� 䫠����.                         �
*�                                                                        �
*����������������������������������������������������������� 16.02.1999 ���
PROCEDURE F_Names
IF .NOT. m_names_sw
  RETURN .T.
ENDIF
m_names_sw = .F.
*
*  ��६���� ���ﭨ� ��� ������樨
*

PRIVATE stat_type     && ��� ������樨: 0 - ���⠭�����;
                                        1 - �����;
                                        2 - BROWSE - ⠡���;
                                        3 - BROWSE - ᯨ᮪.
PRIVATE what_do       && ��� ०���.
PRIVATE menu_name     && ��� �ᨭ�஭���� ����.
PRIVATE last_mouse    && �६� ��᫥����� ������ ���ਭ�� ������.
PRIVATE win_name      && ��� ���� ( ���� ��� BROWSE ).

*
*   ������塞 ���祭�ﬨ ��६���� ���ﭨ�...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ����ঠ⥫쭠� ���� �ணࠬ��:
*

*��������������������������������Ŀ
*�     �������������������Ŀ      �
*�     �  1. ....:....!..  �      �
*�     �                   �      �
*�     �  2. ....:....!..  �      �
*�     �                   �      �
*�     �  3. ....:....!..  �      �
*�     �                   �      �
*�     �  4. ....:....!..  �      �
*�     �                   �      �
*�     �  5. ....:....!..  �      �
*�     ���������������������      �
*�< OK Ctrl-W > < �⪠������ Esc >�
*����������������������������������

PRIVATE ex, v1, v2, v3, v4, v5
ex = 1
*------------------------------------------------------------------------
v1 = ""  && m_names(1)
v2 = m_names(2)
v3 = m_names(3)
v4 = m_names(4)
v5 = m_names(5)

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 15, 36, "����� 䫠����"
DO Sun_Bord WITH  2,  7, 12, 27

*------------------------------------------------------------------------
*      ���� ����� ������
*

@  3, 10 SAY "1." GET v1 WHEN .F.
@  5, 10 SAY "2." GET v2
@  7, 10 SAY "3." GET v3
@  9, 10 SAY "4." GET v4
@ 11, 10 SAY "5." GET v5
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE

IF ex = 1
  *
  * ��ࠡ��뢠�� �����
*  m_names(1) = v1
  m_names(2) = v2
  m_names(3) = v3
  m_names(4) = v4
  m_names(5) = v5
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)

SHOW GETS

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Firms_D      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                        ���죨 �� �।�����.                         �
*�                                                                        �
*����������������������������������������������������������� 16.02.1999 ���
PROCEDURE Firms_D

IF .NOT. firm_sw
  RETURN .T.
ENDIF
firm_sw = .F.
*
*  ��६���� ���ﭨ� ��� ������樨
*

PRIVATE stat_type     && ��� ������樨: 0 - ���⠭�����;
                                        1 - �����;
                                        2 - BROWSE - ⠡���;
                                        3 - BROWSE - ᯨ᮪.
PRIVATE what_do       && ��� ०���.
PRIVATE menu_name     && ��� �ᨭ�஭���� ����.
PRIVATE last_mouse    && �६� ��᫥����� ������ ���ਭ�� ������.
PRIVATE win_name      && ��� ���� ( ���� ��� BROWSE ).
PRIVATE s_sav         && ����� ࠡ�祩 ������ ��� ��࠭���� � ����⠭�������!

*
*   ������塞 ���祭�ﬨ ��६���� ���ﭨ�...
*
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ���뢠�� 䠩�� �.�., ��⠭�������� �裡 � �. �.
*

SELECT F_LIST
GO TOP
*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd, v_sav
ln = MIN(RECCOUNT(), WROWS("")-11)   && ������⢮ ������� ��ப BROWSE
wd = 61
*ڏ।���⨥����������Ą�����������ć���ঠ����������Ċ।���Ŀ
*�....:....!....:. 999_999_999.99 999_999_999.99 999_999_999.99�
*���������������������������������������������������������������

DO D_Wins WITH ln, wd, "�।�����", 0, 0
@ 1, 2 SAY "�।���⨥����������Ą�����������ć���ঠ����������Ċ।��"
what_do = "List"
v_sav = 0
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1

*
*    BROWSE - ����
*
    IF p_nav = "RUB"
      BROWSE FIELDS F_NAME:W=.F.:H="",  ;
                    ACC_SUM:W=.F.:H="":P="@Z 999 999 999.99":W=.F.,  ;
                    ACC_S_H:H="":P="@Z 999 999 999.99":W=let_m .AND. I_Sav():V=C_Sum(),  ;
                    CR_SUM:H="":P="@Z 999 999 999.99":W=let_m  ;
             NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ELSE
      BROWSE FIELDS F_NAME:W=.F.:H="",  ;
                    ACC_SUMD:W=.F.:H="":P="@Z 9 999 999.9999":W=.F.,  ;
                    ACC_SD_H:H="":P="@Z 9 999 999.9999":W=let_m .AND. I_Sav():V=C_Sum(),  ;
                    CRD_SUM:H="":P="@Z 9 999 999.9999":W=let_m  ;
             NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ENDIF
    ON KEY
    IF what_do = "List"
      what_do = "Exit"
    ENDIF

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
DO Sum_Dep
IF .f. &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&let_m
  SELECT 0
  USE (base_path+"CUS_REST") ORDER TAG CUS_CODE ALIAS CR_9311
  SELECT F_LIST
  SCAN
    IF .NOT. SEEK(STR(ACC_TMP.CUS_CODE)+F_CODE, "CR_9311")
      IF ACC_SUM = 0 .AND. ACC_SUMD = 0 .AND. ;
         ACC_S_H = 0 .AND. ACC_SD_H = 0 .AND. ;
         CR_SUM = 0 .AND. CRD_SUM = 0
        LOOP
      ENDIF
      SELECT CR_9311
      APPEND BLANK
      REPLACE CUS_CODE WITH ACC_TMP.CUS_CODE, FIRM WITH F_LIST.F_CODE
    ELSE
      SELECT CR_9311
    ENDIF
    REPLACE ACC_SUM    WITH ACC_SUM+F_LIST.ACC_SUM-F_LIST.ACC_S0,     ;
            ACC_SUMD   WITH ACC_SUMD+F_LIST.ACC_SUMD-F_LIST.ACC_SD0,  ;
            ACC_S_H    WITH ACC_S_H+F_LIST.ACC_S_H-F_LIST.ACC_SH0,    ;
            ACC_SD_H   WITH ACC_SD_H+F_LIST.ACC_SD_H-F_LIST.ACC_SDH0, ;
            CR_SUM     WITH CR_SUM+F_LIST.CR_SUM-F_LIST.CR_SUM0,      ;
            CRD_SUM    WITH CRD_SUM+F_LIST.CRD_SUM-F_LIST.CRD_SUM0
  ENDSCAN
  SELECT CR_9311
  USE
ENDIF
SHOW GETS
SELECT F_LIST

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� I_Sav        ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                    ���࠭���� ��砫쭮�� ���祭��.                     �
*�                                                                        �
*����������������������������������������������������������� 16.02.1999 ���
PROCEDURE I_Sav
v_sav = IIF(p_nav = "RUB", ACC_S_H, ACC_SD_H)
RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� C_Sum        ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                        ������ �㬬� �������.                        �
*�                                                                        �
*����������������������������������������������������������� 16.02.1999 ���
PROCEDURE C_Sum
IF p_nav = "RUB"
  REPLACE ACC_SUM WITH ACC_SUM+v_sav-ACC_S_H
ELSE
  REPLACE ACC_SUMD WITH ACC_SUMD+v_sav-ACC_SD_H
ENDIF
RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� NameM1       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                         �������� ��ࢮ� ��⪨                          �
*�                                                                        �
*����������������������������������������������������������� 18.01.2002 ���
PROCEDURE NameM1
PARAMETERS pMark

PRIVATE tmpRet
DO CASE
CASE pMark = "1"
  tmpRet = "�।�ਭ���⥫� "
CASE pMark = "2"
  tmpRet = "���⭮� ���    "
OTHERWISE
  tmpRet = "�ਤ��᪮� ���"
ENDCASE

RETURN tmpRet

*������������������������������������������������������������������������Ŀ
*�   ��楤�� G_Face       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                         �롮� ⨯� ���� ���                          �
*�                                                                        �
*����������������������������������������������������������� 18.01.2002 ���
PROCEDURE G_Face

IF .NOT. sw1
  RETURN .T.
ENDIF

tmpMark1 = WhatFace(tmpMark1)

sw1 = .F.

SHOW GET sw1
m_names(1) = NameM1(tmpMark1)
SHOW GET m_names(1)

RETURN .T.