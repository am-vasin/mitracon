*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Own_Parm     ��о�｀砒┴ ��むォ ��瓱�           07.03.97 13:18:41 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                       ��痰牀��� �����モ牀� ����.                       �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Own_Parm

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

PRIVATE ex, p00, p01, p02, p03, p04, scr
PRIVATE inv_list, nam_list, i, s_sav, point
scr = .F.
ex   = 1
p00  = m.showLett
p01  = prn_mode
p02  = own_mail
s_sav = SELECT()
SELECT 0
USE (base_path+"DOC_FORM") ALIAS D_9502 AGAIN && IN 0
i = 0
SCAN
  IF LEFT(DOC_NAME,7) # "INVOICE"
    LOOP
  ENDIF
  i = i+1
  DIMENSION inv_list(i), nam_list(i)
  inv_list(i) = SUBSTR(DOC_NAME,8,1)
  nam_list(i) = DOC_PROMPT
ENDSCAN
USE

IF choose_inv == "-"
  p03 = .T.
  p04 = SPACE(40)
  point = 1
ELSE
  p03 = .F.
  point = ASCAN(inv_list, choose_inv)
  p04 = nam_list(point)
ENDIF

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 12, 49, "��痰牀��� ����"
DO Sun_Bord WITH  5,  2,  8, 46, " ��爼�� �����き�� "
*
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*� [ ] ����щ��碎 甌�♂キ�� ↓竄爛��ォ ��艪�   �
*� [ ] �襪���碎 ��痰�→� �爬 �ョ�皋 ぎ�祠キ皰� �
*�      [ ] ��＝癶キ��� 蹕オ矗����� ��艪�      �
*楽� ��爼�� �����き�� 陳陳陳陳陳陳陳陳陳陳陳陳審
*崖      [ ] �襦�� 筮爼��� �爬 �ョ�皋         崖
*崖 ....:....!....:....!....:....!....:....!  崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰�
*�      < ��く�瓷� �� ↓竄爛��ォ ��艪� >       �
*�      < OK Ctrl-W > < �皖���碎瘴 Esc >       �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@  2, 3 GET p00 PICTURE "@*C ����щ��碎 甌�♂キ�� ↓竄爛��ォ ��艪�"
@  3, 3 GET p01 PICTURE "@*C �襪���碎 ��痰�→� �爬 �ョ�皋 ぎ�祠キ皰�"
@  3, 3 GET p01 PICTURE "@*C �襪���碎 ��痰�→� �爬 �ョ�皋 ぎ�祠キ皰�"
@  4, 8 GET p02 PICTURE "@*C ��＝癶キ��� 蹕オ矗����� ��艪�"
@  6, 9 GET p03 PICTURE "@*C �襦�� 筮爼��� �爬 �ョ�皋" VALID C_Form()
@  7, 4 GET p04 WHEN .F.
@  9, 8 GET scr PICTURE "@*C ��く�瓷� �� ↓竄爛��ォ ��艪� " VALID Ss()
*@  9, 8 GET scr PICTURE "@*H \ ��く�瓷� �� ↓竄爛��ォ ��艪� " VALID Ss()
@ 10, 8 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

IF ex = 1 .AND. READKEY() % 256 # 12
  m.showLett = p00
  m.prn_mode = p01
  m.own_mail = p02
  IF p03
    m.choose_inv = "-"
  ELSE
    m.choose_inv = inv_list(point)
  ENDIF
  SELECT 0
  DELETE FILE (prmo_path+"OWN_PARM.DBF")
  CREATE DBF (prmo_path+"OWN_PARM.DBF")  ;
      ( PRN_MODE   L,    ;
        OWN_MAIL   L,    ;
        SHOW_LETT  L,    ;
        CHOOSE_INV C(1)  )
  APPEND BLANK
  REPLACE OWN_PARM.PRN_MODE   WITH m.prn_mode,  ;
          OWN_PARM.SHOW_LETT  WITH m.showLett,  ;
          OWN_PARM.OWN_MAIL   WITH m.own_mail,  ;
          OWN_PARM.CHOOSE_INV WITH m.choose_inv
  USE
ENDIF
SELECT (s_sav)
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� C_Form       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                        �襦�� 筮爼��� ├ 甎�瓷�.                        �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 02.06.1999 陳�
PROCEDURE C_Form

IF p03
  p04 = SPACE(40)
  point = 1
  SHOW GET p04
  RETURN .T.
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

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH MIN(ALEN(nam_list)+3, WROWS("")-5), LEN(nam_list(1))+4, "��爼�硅 �����き諷"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
DO WHILE .T.
  point = MIN(point, ALEN(nam_list))
  point = MAX(point, 1)
  @ 1, 1 MENU nam_list, ALEN(nam_list), WROWS()-3
  READ MENU TO point
  IF READKEY() = 12
    point = 1
    p03   = .T.
    EXIT
  ENDIF
  IF READKEY() = 15
    p04 = nam_list(point)
    EXIT
  ENDIF
ENDDO

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
SHOW GET p03
SHOW GET p04

RETURN .T.

*--------------------------------------------------------------------------
PROCEDURE Ss

DO Sscrpt
scr = .F.
SHOW GETS
RETURN .T.
