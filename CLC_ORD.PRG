*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Clc_Ord      ��о�｀砒┴ ��むォ ��瓱�           06.05.98 16:29:41 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                     ��瘍モ 爛���キゃガ��� ������.                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Clc_Ord

*
*   �����モ琺 ������
*
PRIVATE is1, is2, is3, is4, is5, is6, is7
PRIVATE set_p1, set_p2, set_p3, set_p4, set_p5

STORE .F. TO is1, is2, is3, is4, is5, is6, is7
STORE .F. TO set_p1, set_p2, set_p3, set_p4, set_p5

IF G_Parm()
  DO Clc_Qnt
ENDIF

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                      �痰���→� �����モ牀� ������.                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 06.05.98 18:49:31 夕
PROCEDURE G_Parm

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*� 敖 ��痰�� ��錢�� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕 �
*� � �┓ 甌痰�←闔薀�                  �����モ琺 � �
*� � [ ] 蹍��Θ                          [ ]    � �
*� � [ ] ��牀�                            [ ]    � �
*� � [ ] ��↓� �矚竄痰≪鉗┘ �� 瓷��ぅ    [ ]    � �
*� � [ ] ���襯                            [ ]    � �
*� � [ ] ��爼�-�牀├〓え皀��           ........  � �
*� � [ ] ����                                    � �
*� � [ ] �� ����� ....:....!....:....!....:....! � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*� 敖 ｡薑� �����モ琺 陳陳陳陳陳陳陳陳陳陳陳陳陳朕 �
*� � [ ] ������碎 ��▲璢                         � �
*� � [ ] �腮硅��碎 筰�Ξ� "�� ����щ��碎"        � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*�        < OK Ctrl-W > < �皖���碎瘴 Esc >         �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex
ex   = 1             &&
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 18, 53, "�����モ琺 ��錢��"
DO Sun_Bord WITH  2,  3, 11, 49, " ��痰�� ��錢�� "
DO Sun_Bord WITH 12,  3, 15, 49, " ｡薑� �����モ琺 "
@ 3, 5 SAY "�┓ 甌痰�←闔薀�                  �����モ琺"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@  4,  5 GET is1 PICTURE "@*C 蹍��Θ" WHEN .NOT. EMPTY(d1) .AND. .NOT. EMPTY(d2)
@  4, 42 GET set_p1 PICTURE "@*C " WHEN is1 VALID S_P1()
@  5,  5 GET is2 PICTURE "@*C ��牀�" WHEN .NOT. EMPTY(d1) .AND. .NOT. EMPTY(d2)
@  5, 42 GET set_p2 PICTURE "@*C " WHEN is2 VALID S_P2()
@  6,  5 GET is3 PICTURE "@*C ��↓� �矚竄痰≪鉗┘ �� 瓷��ぅ" WHEN .NOT. EMPTY(d1) .AND. .NOT. EMPTY(d2)
@  6, 42 GET set_p3 PICTURE "@*C " WHEN is3 VALID S_P3()
@  7,  5 GET is4 PICTURE "@*C ���襯"
@  7, 42 GET set_p4 PICTURE "@*C " WHEN is4 VALID S_P4()
@  8,  5 GET is5 PICTURE "@*C ��爼�-�牀├〓え皀��" VALID S_P5()
@  9,  5 GET is6 PICTURE "@*C ����" WHEN .NOT. EMPTY(d1) .AND. .NOT. EMPTY(d2)
@ 10,  5 GET is7 PICTURE "@*C �� �����" VALID S_P7()
@ 13,  5 GET __over PICTURE "@*C ������碎 ��▲璢"
@ 14,  5 GET __stop_ord PICTURE '@*C �腮硅��碎 筰�Ξ� "�� ����щ��碎"'
@ 16, 10 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�           ��玻オ罔� �����モ牀� か� 甌痰�←闔薀� �� �牀�����.           �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 07.05.98 13:31:03 夕
PROCEDURE S_P1
EXTERNAL ARRAY __c_scale

IF .NOT. set_p1
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�    ��゛��� �����罔� MIN � MAX    �
*�          ����甌� � き閻          �
*�    敖 �キ� ��...陳MIN陳MAX陳�    �
*�    �  99999.999   999  999  �    �
*�    �  99999.999   999  999  �    �
*�    �  99999.999   999  999  �    �
*�    �  99999.999   999  999  �    �
*�    �  99999.999   999  999  �    �
*�    �  99999.999   999  999  �    �
*�    �  99999.999   999  999  �    �
*�    �  99999.999   999  999  �    �
*�    �  99999.999   999  999  �    �
*�    �  99999.999   999  999  �    �
*�    青陳陳陳陳陳陳陳陳陳陳陳潰    �
*�  MIN 痺��� �� ��Ж罔� 9999.999   �
*�                                  �
*� < OK Ctrl-W > < �皖���碎瘴 Esc > �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, _0c_scale, _0min_p, i, j
DIMENSION _0c_scale(10,3)
FOR i = 1 TO 10
  FOR j = 1 To 3
   _0c_scale(i,j) = __c_scale(i,j)
  ENDFOR
ENDFOR
_0min_p   = __min_p
ex   = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 20, 38, "��痰�←闔��� �� �牀�����"
@ 2,  6 SAY "��゛��� �����罔� MIN � MAX"
@ 3, 12 SAY "����甌� � き閻"
DO Sun_Bord WITH 4, 6, 15, 31
@ 4, 8 SAY " �キ� ��...陳MIN陳MAX"


*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@  5,  9 GET _0c_scale( 1,1) WHEN .F. PICTURE "@Z 99999.999"
@  5, 21 GET _0c_scale( 1,2) PICTURE "@Z 999" WHEN Enter2(1) VALID Exit2(1)
@  5, 26 GET _0c_scale( 1,3) PICTURE "@Z 999" WHEN Enter3(1) VALID Exit3(1)

@  6,  9 GET _0c_scale( 2,1) PICTURE "@Z 99999.999" WHEN Enter1(2) VALID Exit1(2)
@  6, 21 GET _0c_scale( 2,2) PICTURE "@Z 999" WHEN Enter2(2) VALID Exit2(2)
@  6, 26 GET _0c_scale( 2,3) PICTURE "@Z 999" WHEN Enter3(2) VALID Exit3(2)

@  7,  9 GET _0c_scale( 3,1) PICTURE "@Z 99999.999" WHEN Enter1(3) VALID Exit1(3)
@  7, 21 GET _0c_scale( 3,2) PICTURE "@Z 999" WHEN Enter2(3) VALID Exit2(3)
@  7, 26 GET _0c_scale( 3,3) PICTURE "@Z 999" WHEN Enter3(3) VALID Exit3(3)

@  8,  9 GET _0c_scale( 4,1) PICTURE "@Z 99999.999" WHEN Enter1(4) VALID Exit1(4)
@  8, 21 GET _0c_scale( 4,2) PICTURE "@Z 999" WHEN Enter2(4) VALID Exit2(4)
@  8, 26 GET _0c_scale( 4,3) PICTURE "@Z 999" WHEN Enter3(4) VALID Exit3(4)

@  9,  9 GET _0c_scale( 5,1) PICTURE "@Z 99999.999" WHEN Enter1(5) VALID Exit1(5)
@  9, 21 GET _0c_scale( 5,2) PICTURE "@Z 999" WHEN Enter2(5) VALID Exit2(5)
@  9, 26 GET _0c_scale( 5,3) PICTURE "@Z 999" WHEN Enter3(5) VALID Exit3(5)

@ 10,  9 GET _0c_scale( 6,1) PICTURE "@Z 99999.999" WHEN Enter1(6) VALID Exit1(6)
@ 10, 21 GET _0c_scale( 6,2) PICTURE "@Z 999" WHEN Enter2(6) VALID Exit2(6)
@ 10, 26 GET _0c_scale( 6,3) PICTURE "@Z 999" WHEN Enter3(6) VALID Exit3(6)

@ 11,  9 GET _0c_scale( 7,1) PICTURE "@Z 99999.999" WHEN Enter1(7) VALID Exit1(7)
@ 11, 21 GET _0c_scale( 7,2) PICTURE "@Z 999" WHEN Enter2(7) VALID Exit2(7)
@ 11, 26 GET _0c_scale( 7,3) PICTURE "@Z 999" WHEN Enter3(7) VALID Exit3(7)

@ 12,  9 GET _0c_scale( 8,1) PICTURE "@Z 99999.999" WHEN Enter1(8) VALID Exit1(8)
@ 12, 21 GET _0c_scale( 8,2) PICTURE "@Z 999" WHEN Enter2(8) VALID Exit2(8)
@ 12, 26 GET _0c_scale( 8,3) PICTURE "@Z 999" WHEN Enter3(8) VALID Exit3(8)

@ 13,  9 GET _0c_scale( 9,1) PICTURE "@Z 99999.999" WHEN Enter1(9) VALID Exit1(9)
@ 13, 21 GET _0c_scale( 9,2) PICTURE "@Z 999" WHEN Enter2(9) VALID Exit2(9)
@ 13, 26 GET _0c_scale( 9,3) PICTURE "@Z 999" WHEN Enter3(9) VALID Exit3(9)

@ 14,  9 GET _0c_scale(10,1) PICTURE "@Z 99999.999" WHEN Enter1(10) VALID Exit1(10)
@ 14, 21 GET _0c_scale(10,2) PICTURE "@Z 999" WHEN Enter2(10) VALID Exit2(10)
@ 14, 26 GET _0c_scale(10,3) PICTURE "@Z 999" WHEN Enter3(10) VALID Exit3(10)

@ 16,  4 SAY "MIN 痺��� �� ��Ж罔�" GET _0min_p PICTURE "9999.999" WHEN Clr_Tab()
@ 18,  3 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc " WHEN Clr_Tab()

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
IF ex = 1
  FOR i = 1 TO 10
    FOR j = 1 To 3
      __c_scale(i,j) = _0c_scale(i,j)
    ENDFOR
  ENDFOR
  __min_p   = _0min_p
ENDIF

set_p1 = .F.
SHOW GET set_p1
RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                      �絎�/�諷�� ├ ���ォ ��゛�肓.                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 07.05.98 15:16:09 夕
************************* ������� ****************************************
PROCEDURE Enter1
PARAMETER lin
@ 5, 7 FILL TO 14, 30 COLOR (SCHEME(13,1))
IF lin = 2 .OR. _0c_scale(lin-1,1) # 0
  RETURN .T.
ENDIF
RETURN .F.

PROCEDURE Exit1
PARAMETER lin
PRIVATE i
DO CASE
CASE _0c_scale(lin,1) < 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�矗���皀�讚�� 罐��? �ぅ瘡 艪�-皰 �� ���!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
CASE _0c_scale(lin,1) = 0
  FOR i = lin TO 10
    _0c_scale(i,1) = 0
    SHOW GET _0c_scale(i,1)
    _0c_scale(i,2) = 0
    SHOW GET _0c_scale(i,2)
    _0c_scale(i,3) = 0
    SHOW GET _0c_scale(i,3)
  ENDFOR
OTHERWISE
  IF _0c_scale(lin,1) < _0c_scale(lin-1,1)
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"��甃�矗�皀 �� �爛るゃ蜒� �����聶. �ぅ瘡 艪�-皰 �� ���!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
ENDCASE
@ 5, 7 FILL TO 14, 30 COLOR (SCHEME(13,1))
RETURN .T.

************************* MIN ****************************************
PROCEDURE Enter2
PARAMETER lin
IF _0c_scale(lin,1) = 0 .AND. lin # 1
  RETURN .F.
ENDIF
@ 5, 7 FILL TO 14, 30 COLOR (SCHEME(13,1))
RETURN .T.

PROCEDURE Exit2
PARAMETER lin
IF _0c_scale(lin,2) < 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�ぅ瘡 �矗���皀�讚諷 腮瓮� °碎 �� ��Ε�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
@ 5, 7 FILL TO 14, 30 COLOR (SCHEME(13,1))
RETURN .T.

************************* MAX ****************************************
PROCEDURE Enter3
PARAMETER lin
IF _0c_scale(lin,1) = 0 .AND. lin # 1
  RETURN .F.
ENDIF
@ 5, 7 FILL TO 14, 30 COLOR (SCHEME(13,1))
RETURN .T.

PROCEDURE Exit3
PARAMETER lin
IF _0c_scale(lin,3) < 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�ぅ瘡 �矗���皀�讚諷 腮瓮� °碎 �� ��Ε�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
@ 5, 7 FILL TO 14, 30 COLOR (SCHEME(13,1))
RETURN .T.

PROCEDURE Clr_Tab
@ 5, 7 FILL TO 14, 30 COLOR (SCHEME(13,1))
RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�              ��玻オ罔� �����モ牀� 甌痰�←闔薀� �� 甎牀痺.              �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 08.05.98 11:16:37 夕
PROCEDURE S_P2
*
*  �ムガキ�襯 甌痰�鎰�� か� ��※��罔�
*

IF .NOT. set_p2
  RETURN .T.
ENDIF

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�                                              �
*�  ��辟筥罔キ� か� ����腑痰�� ぅ���ォ  999.99  �
*�                                              �
*�  ��辟筥罔キ� か� ����腑痰�� ���牀甌� 999.99  �
*�                                              �
*�       ��牀� �� ����腑痰≪ ���牀甌� 999       �
*�                                              �
*�   Min 痺��� 999999999  Max 痺��� 999999999   �
*�                                              �
*�       < OK Ctrl-W > < �皖���碎瘴 Esc >       �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, k_1, k_2, p, m1, m2
k_1 = __koeff_q       && ��辟筥罔キ� か� ����腑痰�� ぅ���ォ
k_2 = __koeff_n       && ��辟筥罔キ� か� ����腑痰�� ���牀甌�
p   = __porog         && ��牀� �� ���-≪ ���牀甌�
m1  = __min_s         && �┃━��讚�� 痰�━�痰� ������
m2  = __max_s         && ���瓱���讚�� 痰�━�痰� ������
ex   = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 13, 50, "�����モ琺 甌痰�←闔薀� �� 甎牀痺"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@  3,  4 SAY "��辟筥罔キ� か� ����腑痰�� ぅ���ォ " GET k_1 PICTURE "999.99"
@  5,  4 SAY "��辟筥罔キ� か� ����腑痰�� ���牀甌�" GET k_2 PICTURE "999.99"
@  7,  9 SAY "��牀� �� ����腑痰≪ ���牀甌�" GET p PICTURE "999"
@  9,  5 SAY "Min 痺���" GET m1 PICTURE "999999999"
@  9, 26 SAY "Max 痺���" GET m2 PICTURE "999999999"
@ 11,  9 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

IF ex = 1
  __koeff_q = k_1      && ��辟筥罔キ� か� ����腑痰�� ぅ���ォ
  __koeff_n = k_2      && ��辟筥罔キ� か� ����腑痰�� ���牀甌�
  __porog   = p        && ��牀� �� ���-≪ ���牀甌�
  __min_s   = m1       && �┃━��讚�� 痰��皙�痰� ������
  __max_s   = m2       && ���瓱���讚�� 痰��皙�痰� ������
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
set_p2 = .F.
SHOW GET set_p2
RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�   ��玻オ罔� �����モ牀� 甌痰�←闔薀� �� ��↓� �矚竄痰≪鉗━ ��Ж罔鍖.   �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 08.05.98 11:16:37 夕
PROCEDURE S_P3
*
*  �ムガキ�襯 甌痰�鎰�� か� ��※��罔�
*

IF .NOT. set_p3
  RETURN .T.
ENDIF

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�                                              �
*�   ����讓� きォ ��Ж罔� �� �牀������瘡 9999   �
*�                                              �
*�              ����腑痰〓 999999               �
*�                                              �
*�       < OK Ctrl-W > < �皖���碎瘴 Esc >       �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, k_1, k_2
k_1 = __abs_q   && ����腑痰〓
k_2 = __abs_age && �牀�
ex   = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 9, 50, "�矚竄痰≪鉗┘ ��Ж罔�"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 3, 5 SAY "����讓� きォ ��Ж罔� �� �牀������瘡" GET k_2 PICTURE "9999"
@ 5,16 SAY "����腑痰〓" GET k_1 PICTURE "999999"
@ 7, 9 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

IF ex = 1
  __abs_q   = k_1 && ����腑痰〓
  __abs_age = k_2 && �牀�
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
set_p3 = .F.
SHOW GET set_p3
RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�           ��玻オ罔� �����モ牀� 甌痰�←闔薀� �� ���覓 ��Ж罔鍖.         �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 08.05.98 11:16:37 夕
PROCEDURE S_P4
*
*  �ムガキ�襯 甌痰�鎰�� か� ��※��罔�
*

IF .NOT. set_p4
  RETURN .T.
ENDIF

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�                                   �
*�   "��о�痰" ��Ж罔� � き閻 9999   �
*�                                   �
*�         ����腑痰〓 999999         �
*�                                   �
*� < OK Ctrl-W > < �皖���碎瘴 Esc >  �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex, k_1, k_2
k_1 = __new_q   && ����腑痰〓
k_2 = __new_d   && "��о�痰" � き閻
ex   = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 9, 39, "���襯 ��Ж罔�"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 3, 5 SAY '"��о�痰" ��Ж罔� � き閻' GET k_2 PICTURE "9999"
@ 5,11 SAY "����腑痰〓" GET k_1 PICTURE "999999"
@ 7, 3 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

IF ex = 1
  __new_q = k_1   && ����腑痰〓
  __new_d = k_2   && "��о�痰" � き閻
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
set_p4 = .F.
SHOW GET set_p4
RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                        ��爼� - �牀├〓え皀��.                          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 08.05.98 11:16:37 夕
PROCEDURE S_P5
*
*  �ムガキ�襯 甌痰�鎰�� か� ��※��罔�
*

IF .NOT. is5
  @ 8, 39 SAY "        "
  RETURN .T.
ENDIF

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
*��爼�-�牀├〓え皀��
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�                                �
*�         ��爼� ....!...         �
*�                                �
*�       ����腑痰〓 999999        �
*�                                �
*�< OK Ctrl-W > < �皖���碎瘴 Esc >�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, k_1, point, i
s_sav = SELECT()
SELECT 0
USE (base_path+"DEVELOP") ORDER TAG CODE ALIAS W0003 AGAIN
IF SEEK(fir_code)
  point = LEFT(B_NAME,8)
ELSE
  point = SPACE(8)
ENDIF
USE
SELECT (s_sav)
k_1 = __fir_q   && ����腑痰〓
ex   = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 9, 36, "��爼�-�牀├〓え皀��"
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 3,11 SAY "��爼�" GET point VALID Develop(fir_code)
@ 5,  9 SAY "����腑痰〓" GET k_1 PICTURE "999999"
@ 7,  2 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
IF ex = 1
  __fir_q = k_1
ELSE  
  is5 = .F.
  SHOW GET is5
ENDIF
RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                          ���甌� ├ �����.                              �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 08.05.98 11:16:37 夕
PROCEDURE S_P7

IF .NOT. is7
  @ 10, 18 SAY SPACE(30)
  RETURN .T.
ENDIF

__f_name = Int_File("", "*", "���Θ皀 ���� 甌 甎�瓷��")
IF .NOT. EMPTY(__f_name)
  PRIVATE s_sav, err_cod, err_lst
  DIMENSION err_lst(1)
  err_lst(1) = 15
  err_cod = 0
  s_sav = SELECT()
  SELECT 0
  USE (ALLTRIM(__f_name)) ALIAS w333
  IF err_cod = 0
    IF TYPE("CODE") # "N" .OR. ;
       TYPE("PREFIX") # "C" .OR. ;
       TYPE("NAME") # "C" .OR. ;
       TYPE("PRODUCER") # "C" .OR. ;
       TYPE("QNT") # "N"
      PRIVATE mss
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"�矗礫矣�� ����� �� 甌�癶モ痰≪モ ����с!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      __f_name = ""
    ENDIF
  ELSE
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"�皰 �� DBF-����!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    __f_name = ""
  ENDIF
  USE
  SELECT (s_sav)
ENDIF
IF EMPTY(__f_name)
  is7 = .F.
  SHOW GET is7
  @ 10, 18 SAY SPACE(30)
ELSE
  @ 10, 18 SAY PADR(__f_name,30) COLOR SCHEME 1
ENDIF

RETURN .T.
*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�              ��瘍モ ����腑痰� か� 爛���キゃガ��� ������.               �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 12.05.98 13:52:43 夕
PROCEDURE Clc_Qnt

PRIVATE s_sav, ord_sav, rc_sav, q_w, p_own
PRIVATE p_sup_d, p_sup_r, own_v, i, q_n, q_p, cnt, sw

IF .NOT. is1 .AND. .NOT. is2 .AND. .NOT. is3 .AND. .NOT. is4 .AND.   ;
   .NOT. is5 .AND. .NOT. is6 .AND. .NOT. is7
  RETURN
ENDIF

s_sav = SELECT()

SELECT 0
USE (base_path+"PARMS") ALIAS P13598 AGAIN
own_v = VALUTA
USE
USE (base_path+"STOCK") ORDER TAG CODE ALIAS ST12598 AGAIN

SELECT ORD_TMP
rc_sav = RECNO()

IF is1 .OR. is2 .OR. is3 .OR. is4 .OR. is5 .OR. is6
  DO Wt_Mess WITH "��爼�牀���┘ ������"
  cnt = 0
  SCAN
    cnt = cnt+1
    IF cnt % 100 = 0
      @ WROWS()-1, 0 SAY PADC(ALLTRIM(STR(cnt)),WCOLS())
    ENDIF
    q_w = 0
    p_own = 0
    p_sup_d = 0
    p_sup_r = 0
    IF OWN
      IF SEEK(CODE,"ST12598")
        IF .NOT. EMPTY(ST12598.WK_SW1) .AND. __stop_ord
          LOOP
        ENDIF
        p_own = IIF(own_v = "$", ST12598.PRICE, ST12598.PRICE_R)
      ENDIF
    ENDIF
    IF ORD_TMP.OFFER_REC # 0
      GO ORD_TMP.OFFER_REC IN OFFER
      p_sup_r = OFFER.PRICE_R
      p_sup_d = OFFER.PRICE_D
    ENDIF
    IF is1    && �� �牀�����
      q_n = __c_scale(1,2)
      q_p = __c_scale(1,3)
      FOR i = 2 TO 10
        IF __c_scale(i,1) = 0 .OR. p_own <= __c_scale(i,1)
          EXIT
        ENDIF
        q_n = __c_scale(i,2)
        q_p = __c_scale(i,3)
      ENDFOR
      q_w = QNT_SALE/__srok   && �爛きイ�ア��� �牀����
      IF OWN .AND. FOUND("ST12598")
        q_w = IIF(q_w*q_n > ST12598.QNT, ROUND(q_w*q_p,0)-ST12598.QNT, 0)
      ENDIF
      *
      *   ����←錺� ぎ �礒��� 痺��� �� ��Ж罔�
      IF q_w > 0 .AND. __min_p > 0
        IF own_v = "$"
          IF p_sup_d > 0
            q_w = MAX(q_w, CEILING(__min_p/p_sup_d))
          ENDIF
        ELSE
          IF p_sup_r > 0
            q_w = MAX(q_w, CEILING(__min_p/p_sup_r))
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    IF is2    && ��牀�
      sw = .T.
      IF OWN
        IF QNT_SALE > 0   && �矣 ��Ж罔� ����щ��ガ �� �甅�▲ �牀���
          sw = .F.
        ENDIF
        IF FOUND("ST12598") .AND. ST12598.QNT > 0  && � 轤� � ��� 礒� メ碎
          sw = .F.
        ENDIF
      ENDIF
      IF QNT_DEM_N < __porog     && �イ�痰�皰膈� ����� ��� 甎牀瓱��!
        sw = .F.
      ENDIF
      IF sw
        p_sup_r = 0
        p_sup_d = 0
        IF ORD_TMP.OFFER_REC # 0
          GO ORD_TMP.OFFER_REC IN OFFER
          p_sup_r = OFFER.PRICE_R
          p_sup_d = OFFER.PRICE_D
        ENDIF
        q_w = q_w+ROUND(__koeff_q*QNT_DEM_Q+__koeff_n*QNT_DEM_N, 0)
        IF own_v = "$"
          IF p_sup_d > 0 .AND. __min_s > 0
            q_w = MAX(q_w, CEILING(__min_s/p_sup_d))
          ENDIF
          IF p_sup_d > 0 .AND. __max_s > 0
            q_w = MIN(q_w, CEILING(__max_s/p_sup_d))
          ENDIF
        ELSE
          IF p_sup_r > 0 .AND. __min_s > 0
            q_w = MAX(q_w, CEILING(__min_s/p_sup_r))
          ENDIF
          IF p_sup_r > 0 .AND. __max_s > 0
            q_w = MIN(q_w, CEILING(__max_s/p_sup_r))
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    IF is3    && ��↓� �矚竄痰≪鉗�� �� 瓷��ぅ
      IF OWN .AND. FOUND("ST12598")
        IF ST12598.QNT = 0
          q_w = q_w+IIF(DATE()-LAST_SALE > __abs_age .OR. LAST_SALE = {}, __abs_q, 0)
        ENDIF
      ENDIF
    ENDIF
    IF is4     && ���襯
      IF ORD_TMP.OFFER_REC # 0
        GO ORD_TMP.OFFER_REC IN OFFER
        IF .NOT. EMPTY(OFFER.DATE_ON) .AND. DATE()-OFFER.DATE_ON < __new_d
          q_w = q_w+__new_q
        ENDIF
      ENDIF
    ENDIF
    IF is5     && ��爼�
      IF FOUND("ST12598") .AND. fir_code = ST12598.PRO_CODE
        q_w = q_w+__fir_q
      ENDIF
    ENDIF
    IF is6     && ����
      q_w = q_w+QNT_DUE
    ENDIF
    REPLACE QNT WITH q_w+IIF(__over, 0, QNT)
  ENDSCAN
  DO Wt_Mess
ENDIF

*
*  ��錢�� ├ �����
*
ord_sav = TAG()
SET ORDER TO TAG CODE
IF is7
  DO Wt_Mess WITH "�皀�┘ ������ ├ �����"
  SELECT 0
  USE (__f_name) ALIAS IN
  SCAN FOR CODE # 0
    IF .NOT. SEEK(CODE, "ORD_TMP")
      LOOP
    ENDIF
    IF SEEK(CODE,"ST12598")
      IF .NOT. EMPTY(ST12598.WK_SW1) .AND. __stop_ord
        LOOP
      ENDIF
    ENDIF
    REPLACE ORD_TMP.QNT WITH ORD_TMP.QNT+QNT
  ENDSCAN

  USE
  DO Wt_Mess
ENDIF

SELECT ST12598
USE
SELECT ORD_TMP
SET ORDER TO TAG (ord_sav)
IF BETWEEN(rc_sav, 1, RECCOUNT())
  GO rc_sav
ENDIF

SELECT (s_sav)

RETURN
