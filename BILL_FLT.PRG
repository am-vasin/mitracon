*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                 �痰���→� 筥�赳�� か� 瘍モ��-���矣�.                   �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 03.04.97 17:12:25 夕
PROCEDURE Bill_flt

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
PRIVATE ex, p01, p02, p03, p04, p05, p06, l1, c1, ln, wk_arr
PRIVATE p07, p08

DIMENSION wk_arr(p_numb)
FOR ln = 1 TO p_numb
  wk_arr(ln) = CHR(ASC("A")+ln-1) $ p_line
ENDFOR
ln = MAX(15, 11+p_numb)

p01 = is_hr
p02 = dt1
p03 = dt2
p04 = f_c000
p05 = g_shab
p06 = c_shab
p07 = nds_0
p08 = nds_1
ex  = 1

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH ln, 59, "�痰���※皀 筥�赳�"

*
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*� 敖 ��皀燿�� ��� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*� �   [ ] �痰���※碎    � ��.��.��    �� ��.��.��     � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*�         踳く爬閧┘ 999 9999999999999999 (F1)         �
*楽� ����� 陳陳陳陳陳陳陳笥� ç��お� 陳陳陳陳陳陳陳陳陳審
*崖 ....:....!....:....! 崖 [ ] ....:....!....:....!....崖
*垣陳陳陳陳陳陳陳陳陳陳陳抒 [ ]                         崖
*楽� ��┘�� 陳陳陳陳陳陳朕� [ ]                         崖
*崖 ....:....!....:....! 崖                             崖
*垣陳陳陳陳陳陳陳陳陳陳陳拈陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰�
*�                 ��� � 999.9 �� 999.9                  �
*�         < OK Ctrl-W > < �皖���碎瘴 Esc >              �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

DO Sun_Bord WITH  2,  3, 4, 55, " ��皀燿�� ��� "
DO Sun_Bord WITH  6,  2,  8, 25, " ��┘�� "
DO Sun_Bord WITH  9,  2, 11, 25, " ����� "
DO Sun_Bord WITH  6, 26, WROWS()-4, 56, " ç��お� "

@  3,  7 GET p01 PICTURE "@*C �痰���※碎" VALID P01_V()
@  3, 25 SAY "�" GET p02 PICTURE "@D" WHEN p01
@  3, 39 SAY "��" GET p03 PICTURE "@D" WHEN p01
DO Get_Firm WITH 5, 11, enab_firm
@  7,  4 GET p06
@ 10,  4 GET p05
DO G_Place WITH 7, 28, wk_arr
@ WROWS()-3, 19 SAY "��� �" GET p07 PICTURE "999.9"
@ WROWS()-3, 31 SAY "��" GET p08 PICTURE "999.9"
@ WROWS()-2, 11 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

PRIVATE s, s0
POP KEY
RELEASE WINDOW (win_name)

IF READKEY() % 256 = 12 .OR. ex = 2
  f_c000 = p04
  RETURN .F.
ENDIF

is_hr  = p01
dt1    = p02
dt2    = p03
nds_0  = p07
nds_1  = p08
g_shab = SYS(15, lwr, p05)
c_shab = SYS(15, lwr, p06)
p_line = ""
FOR ln = 1 TO p_numb
  IF wk_arr(ln)
    p_line = p_line+CHR(ASC("A")+ln-1)
  ENDIF
ENDFOR
DO Prp_Tmp

RETURN .T.

FUNCTION P01_V     &&  p01 VALID
#REGION 1
IF .NOT. p01
  p02 = {}
  p03 = DATE()
  SHOW GET p01
  SHOW GET p02
ENDIF
RETURN .T.
