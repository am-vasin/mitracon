*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� P_Screen     ��о�｀砒┴ ��むォ ��瓱�           14.09.98 17:49:13 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                     ��痰�→� �爬 �ョ�皋 ぎ�祠キ��.                     �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE P_Screen
PARAMETERS tit, is_dbf
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�        ���� 甄ア� 99      ���-〓 蹣� 99                     �
*� 敖 蹍��� � ���罐 ぎ�祠キ�� 陳陳陳陳陳陳陳朕  < �ョ�碎 ^W > �
*� �  ( ) ��爼���     ( ) �矗��   ���-〓 999  �  < 蹍甃�矗  > �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰  < WordPad   > �
*� 敖� �����キ� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�  < ���� TXT  > �
*� � [ ] �メ� ぎ�祠キ�  �矗���肓 � 999 �� 999 �  < ���� DBF  > �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰  < ����      > �
*� 敖 �爬キ��罔� 陳陳陳陳陳陳陳朕敖 �爬籵 陳陳�  < E-Mail    > �
*� � ( ) ��└���  ( ) ��譟����� 崖 [ ] ���硅� �  < �皖�� Esc > �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳潰青陳陳陳陳陳潰                �
*�         敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕                �
*� 踸�皀� � ....:....!....:....!....:....!.. �                �
*�         青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰                �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ornt, is_con
ornt = IIF(EMPTY(SUBSTR(fnt,2,1)), 1, 2)
is_con = .NOT. EMPTY(LEFT(fnt,1))
PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 16, 65, tit
DO Sun_Bord WITH 3, 3, 5, 46, " 蹍��� � ���罐 ぎ�祠キ�� "
DO Sun_Bord WITH 6, 3, 8, 46, " �����キ� "
DO Sun_Bord WITH 9, 3, 11, 32, " �爬キ��罔� "
DO Sun_Bord WITH 9, 33, 11, 46, " �爬籵 "
@ 13, 3 SAY "踸�皀�"
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@  3, 49 GET trm PICTURE "@*T \! �ョ�碎 ^W ;\ 蹍甃�矗  ;\ ���� TXT  ;\"+  ;
  IIF(is_dbf, "", "\")+" ���� DBF  ;\ WordPad   ;\ ����      ;\ E-Mail    ;"+ ;
  IIF(TYPE("winType") = "C", "\", "\\")+" Win_Sale ;\? �皖�� ESC "
@  2, 10 SAY "���� 甄ア�" GET l_f PICTURE "@Z 99" VALID V_Left()
@  2, 29  SAY "���-〓 蹣�" GET n_c PICTURE "@Z 99" VALID V_Copy()
@  4,  6 GET f_sw PICTURE "@*RH ��爼���    ;�矗��" VALID V_Switch()
@  4, 34 SAY "���-〓" GET f_num PICTURE "@Z 999" WHEN f_sw = 2
@  7,  5 GET frg PICTURE "@*C �メ� ぎ�祠キ�" VALID V_Frag()
@  7, 24 SAY "�矗���肓 �" GET p_s PICTURE "@Z 999" WHEN .NOT. frg VALID V_First()
@  7, 39 SAY "��" GET p_e PICTURE "@Z 999" WHEN .NOT. frg VALID V_Last()
@ 10,  5 GET ornt PICTURE "@*RH ��└��� ;��譟�����"
@ 10, 35 GET is_con PICTURE "@*C ���硅�"
@ 12, 11 GET p_d0 PICTURE "@^" FROM p_list SIZE 1, 36 WHEN p_len > 1
READ CYCLE VALID V_Read()
fnt = STUFF(fnt, 2, 1, IIF(ornt=1, " ", "*"))
fnt = STUFF(fnt, 1, 1, IIF(is_con, "*", " "))
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN trm

*************************************************************************
*
*     蹍▲爲� ���※�讚�痰� ���ォ.
*
PROCEDURE V_Left

PRIVATE s, mss
s = READKEY() % 256
IF s = 12
  RETURN .T.
ENDIF

IF l_f < 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"����� ���ォ � �爬牀ぅ �� °��モ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*************************************************************************
*
*     蹍▲爲� ���※�讚�痰� 腮甄� ���┤.
*
PROCEDURE V_Copy

PRIVATE s, mss
s = READKEY() % 256
IF s = 12
  RETURN .T.
ENDIF

IF n_c < 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�モ �┴���� 〓К�Ν�痰� ���ョ���碎 痰��讓� ���┤!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF n_c = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�甄� �� �� 絎皋皀 �ョ���碎 ぎ�祠キ�, �� �磴爬皀 - ��Μ�皀 Esc!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*************************************************************************
*
*     蹍▲爲� ����腮� 籥���キ��.
*
PROCEDURE V_Frag

PRIVATE s, mss
s = READKEY() % 256
IF s = 12
  RETURN .T.
ENDIF

IF frg
  p_s = 0
  p_e = 0
  SHOW GET p_s
  SHOW GET p_e
ENDIF

RETURN .T.

*************************************************************************
*
*     蹍▲爲� �ムオ�鈑�皀�� �牀���� 筮爼���.
*
PROCEDURE V_Switch

PRIVATE s, mss
s = READKEY() % 256
IF s = 12
  RETURN .T.
ENDIF

IF f_sw = 1
  f_num = 0
  SHOW GET f_num
ENDIF

RETURN .T.

*************************************************************************
*
*     蹍▲爲� ���ム� �����讚�� 痰����肓.
*
PROCEDURE V_First

PRIVATE s, mss
s = READKEY() % 256
IF s = 12
  RETURN .T.
ENDIF
IF frg
  RETURN .T.
ENDIF

IF p_s <= 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"����� ���ム�� 痰����� � �爬牀ぅ �� °��モ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.


*************************************************************************
*
*     蹍▲爲� ���ム� ���ョ��� 痰����肓.
*
PROCEDURE V_Last

PRIVATE s, mss
s = READKEY() % 256
IF s = 12
  RETURN .T.
ENDIF
IF frg
  RETURN .T.
ENDIF

IF p_e <= 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"����� ���ム�� 痰����� � �爬牀ぅ �� °��モ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF p_e < p_s
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"����� ┃皀燿���� 痰����� � �爬牀ぅ �� °��モ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*************************************************************************
*
*     蹍▲爲� �諷��� ├ ゛����.
*
PROCEDURE V_Read

PRIVATE s, mss
s = READKEY() % 256
IF trm = 7 .OR. READKEY() = 12
  RETURN .T.
ENDIF

IF trm = 1
  IF .NOT. V_Left()
    RETURN .F.
  ENDIF
  IF .NOT. V_Copy()
    RETURN .F.
  ENDIF
  IF .NOT. V_Frag()
    RETURN .F.
  ENDIF
  IF .NOT. V_First()
    RETURN .F.
  ENDIF
  IF .NOT. V_Last()
    RETURN .F.
  ENDIF
ENDIF

RETURN .T.
