*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Set_F_B      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                �痰���→� �����お� � ����� �� 祠�������.                �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 15.01.1999 様�
PROCEDURE Set_F_B

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
PRIVATE flt, frm_n, bnk_n, p_list, shab1, shab2
shab1 = "abcdefghijklmnopqrstuvwxyz�、�ぅ颱Ж�������珀矣粤肄蓍裨跋鈿"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZ���������������������������������"
p_list = ""
STORE "" TO flt
STORE SPACE(16) TO frm_n, bnk_n
s_sav = SELECT()
SELECT 0
DO Use_Dummy
SELECT 0
USE (base_path+"PLACES") ALIAS B_9115 AGAIN
SCAN
  p_list = p_list+p_char
ENDSCAN
USE (base_path+"BANKS") ORDER TAG BANK ALIAS B_9115 AGAIN
SELECT 0
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS F_9115 AGAIN
SELECT 0
USE (base_path+"PARMS") ALIAS P_9115 AGAIN

IF .NOT. EMPTY(SYS_FIRM)
  IF SEEK(SYS_FIRM, "F_9115")
    frm_n = F_9115.B_NAME
    IF .NOT. EMPTY(SYS_BANK)
      IF SEEK(SYS_FIRM+SYS_BANK, "B_9115")
        bnk_n = B_9115.B_NAME
      ELSE
        REPLACE SYS_BANK WITH "   "
      ENDIF
    ENDIF
  ELSE
    REPLACE SYS_FIRM WITH "   "
  ENDIF
ENDIF

SELECT 0
USE (base_path+"ARM_LST") ALIAS A_9115 AGAIN
SET RELATION TO ARM_FIRM INTO F_9115
SET RELATION TO ARM_FIRM+ARM_BANK INTO B_9115 ADDITIVE

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Square OF (menu_name) PROMPT "ç��お�" KEY Ctrl-A, "Enter"
ON SELECTION PAD Square OF (menu_name) DO Sw_Mode WITH "Square"

DEFINE PAD Firm OF (menu_name) PROMPT "踳く�." KEY Ctrl-A, "Tab"
ON SELECTION PAD Firm OF (menu_name) DO Sw_Mode WITH "Firm"

DEFINE PAD Filter OF (menu_name) PROMPT "�┼赳�" KEY Ctrl-A, "F6"
ON SELECTION PAD Filter OF (menu_name) DO Sw_Mode WITH "Filter"

DEFINE PAD Main OF (menu_name) PROMPT "�甅. ��瘁�" KEY Ctrl-A, "F1"
ON SELECTION PAD Firm OF (menu_name) DO Sw_Mode WITH "Main"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
*wd = 60
wd = 58

DO D_Wins WITH ln, wd, "���甌� ��｀腮� �メ�", 0, 1
*����. �メ皰陳陳陳���.�踳く爬閧┘陳陳陳�陳����陳陳陳陳陳陳��ｿ
*�9999999999999999 999 9999999999999999 9 9999999999999999 9 9�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
*� ｡薑� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�  踳く爬閧┘ 9999999999999999     ���� 9999999999999999  �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
*@ 1, 2 SAY "���. �メ皰陳陳陳���.�踳く爬閧┘陳陳陳�陳����陳陳陳陳陳陳���"
@ 1, 2 SAY "���. �メ皰陳陳陳���.�踳く爬閧┘陳陳陳�陳����陳陳陳陳陳陳�"
@ WROWS()-3, 2 SAY " ｡薑� "
@ WROWS()-2, 4 SAY "踳く爬閧┘ "+frm_n+"     ���� "+bnk_n

what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F6    DO Sw_Mode WITH "Filter"
    ON KEY LABEL Tab   DO Sw_Mode WITH "Firm"
    ON KEY LABEL F1    DO Sw_Mode WITH "Main"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS ARM_N:16:H="",  ;
                  F000 = PADC(ARM_S,3):3:H="", ;
                  F_9115.B_NAME:16:H="", ;
                  F001 = IIF(LET_FIRM, "�", " "):1:H="", ;
                  B_9115.B_NAME:16:H="", ;
                  F002 = IIF(LET_BANK, "�", " "):1:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")

*                  F003 = IIF(MAIN_KASSA, "�", " "):1:H="", ;
*

    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Square")
    ENDIF

  CASE what_do = "Square"    && ��������� か� ��｀腑�� �メ��

    DO Set_Def WITH .T.
    what_do = "List"

  CASE what_do = "Firm"      && ��������� か� �爛く爬閧��

    DO Set_Def WITH .F.
    ACTIVATE WINDOW (win_name+"_M")
    @ WROWS()-2, 4 SAY "踳く爬閧┘ "+frm_n+"     ���� "+bnk_n
    what_do = "List"

  CASE what_do = "Main"      && �甅. ��瘁�

    IF .NOT. EOF() .AND. .NOT. BOF()
      REPLACE MAIN_KASSA WITH .NOT. MAIN_KASSA
    ENDIF
    what_do = "List"

  CASE what_do = "Filter"    && �痰���→�/�腮痰�� 筥�赳��

    DO S_Filter
    IF EMPTY(flt)
      DEFINE PAD Filter OF (menu_name) PROMPT "�┼赳�" KEY Ctrl-A, "F6"
    ELSE
      DEFINE PAD Filter OF (menu_name) PROMPT "�┼赳� "+flt KEY Ctrl-A, "F6"
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
USE
SELECT B_9115
USE
SELECT F_9115
USE
SELECT P_9115
USE
SELECT (s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� S_Filter     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                       �痰���→�/�腮痰�� 筥�赳��.                       �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 15.01.1999 陳�
PROCEDURE S_Filter
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� 敖 ç��お� 陳陳陳陳陳陳陳陳陳朕 < �痰���※碎 Ctrl-W > �
*� � ( ) ....:....!....:....!.... � < �腮痰�碎          > �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 < �皖���碎瘴 Esc    > �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, point
ex = 1
point = IIF(EMPTY(flt), 0, AT(flt, p_list))
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH LEN(p_list)+5, 60, "�┼赳�"
DO Sun_Bord WITH  2,  3, LEN(p_list)+3, 34, " ç��お� "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
DO G_Place WITH 3, 5, point
@ 2, 36 GET ex PICTURE   ;
"@*T \! �痰���※碎 Ctrl-W ;\ �腮痰�碎          ;\? �皖���碎瘴 Esc    "

READ CYCLE

DO CASE
CASE ex = 2 .OR. point = 0
  flt = ""
  SET FILTER TO
CASE ex = 1
  flt = SUBSTR(p_list,point,1)
  SET FILTER TO ARM_S = flt
ENDCASE
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Set_Def      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                          �痰���→� 祠�����┤.                          �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 15.01.1999 陳�
PROCEDURE Set_Def
PARAMETER is_arm
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� 踳く爬閧┘ 999 9999999999999999 (F1) [ ] �襦�� ��оヨキ �
*�                                                          �
*�      [ ] ���� 9999999999999999  [ ] �襦�� ��оヨキ       �
*�                                                          �
*�             < OK Ctrl-W > < �皖���碎瘴 Esc >             �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, f_c000, f_n000, b_c000, b_n000, b_sw000, sw_f, sw_b, f_c_old
ex = 1
f_c_old = ""
IF is_arm
  f_c000  = A_9115.ARM_FIRM
  f_n000  = PADL(F_9115.B_NAME,16)
  b_c000  = A_9115.ARM_BANK
  b_n000  = PADL(B_9115.B_NAME,16)
  b_sw000 = .F.
  sw_f    = A_9115.LET_FIRM
  sw_b    = A_9115.LET_BANK
ELSE
  f_c000  = P_9115.SYS_FIRM
  f_n000  = PADL(frm_n,16)
  b_c000  = P_9115.SYS_BANK
  b_n000  = PADL(bnk_n,16)
  b_sw000 = .F.
  sw_f    = .F.
  sw_b    = .F.
ENDIF
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 8, 62, "��������� か� "+IIF(is_arm, ;
       '��｀腑�� �メ�� "'+ALLTRIM(ARM_N)+'"',  ;
       "�爛く爬閧��")

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

DO Get_Firm WITH 2, 3, .T.
IF is_arm
  @ 2, 41 GET sw_f PICTURE "@*C �襦�� ��оヨキ"
ELSE
  @ 2, 41 GET sw_f PICTURE "@*C �襦�� ��оヨキ" DISABLE
ENDIF
DO Get_Bank WITH 4, 8, .T.
IF is_arm
  @ 4, 35 GET sw_b PICTURE "@*C �襦�� ��оヨキ"
ELSE
  @ 4, 35 GET sw_b PICTURE "@*C �襦�� ��оヨキ" DISABLE
ENDIF
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

IF ex = 1
  *
  * �矗���硅��ガ ゛���
  IF is_arm
    REPLACE A_9115.ARM_FIRM WITH f_c000, ;
            A_9115.ARM_BANK WITH b_c000, ;
            A_9115.LET_FIRM WITH sw_f,   ;
            A_9115.LET_BANK WITH sw_b
    GO RECNO()
  ELSE
    REPLACE P_9115.SYS_FIRM WITH f_c000, ;
            P_9115.SYS_BANK WITH b_c000
    frm_n = f_n000
    bnk_n = b_n000
  ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
