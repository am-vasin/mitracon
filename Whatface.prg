*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� WhatFace     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                              �┓ ��┘���                               �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 18.01.2002 様�
PROCEDURE WhatFace
PARAMETER s_value, cli_code
PRIVATE s_sel, is_country, tmpINN, tmpKPP
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
m.tmpINN = ""
m.tmpKPP = ""
IF TYPE("m.cli_code") = "N"
  DO Get_INN WITH m.cli_code, m.tmpINN, m.tmpKPP
ENDIF
*------------------------------------------------------------------------
*    ��ぅ爨�皀�讚�� ��痰� �牀������:
*
*   �┓ ��┘���
*敖陳陳陳陳陳陳陳陳陳陳陳陳�
*� �矗��� ....:....1....:. �
*�  ( ) �爬え腑瓷�� ��罧   �
*�  ( ) 踳く爬�━�皀��    �
*�  ( ) ��痰��� ��罧       �
*�   ��� ....:....1..      �
*�   ��� ....:....1....:.  �
*�         < OK >          �
*青陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex, c_type
IF TYPE("s_value") = "C"
  c_type = VAL(s_value)+1
ELSE
  c_type = 0
ENDIF

ex = 1
*------------------------------------------------------------------------
is_country = TYPE("cntr") = "C"
PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH IIF(is_country, 8, 7)+IIF(TYPE("m.cli_code") = "N", 2, 0), 29, "�┓ ��┘���"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

IF is_country
  @ 2, 3 SAY "�矗���" GET cntr
ENDIF
@ IIF(is_country, 3, 2), 3 GET c_type PICTURE "@*RV �爬え腑瓷�� ��罧;踳く爬�━�皀��;��痰��� ��罧"
IF TYPE("m.cli_code") = "N"
  @ WROWS()-4, 5 SAY "���" GET m.tmpINN
  @ WROWS()-3, 5 SAY "KPP" GET m.tmpKPP
ENDIF
@ WROWS()-2, 10 GET ex PICTURE "@*HT \! OK "

READ CYCLE VALID Tst_Type()

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN TRANSFORM(c_type-1,"@Z 9")+" "+m.tmpINN+CHR(0)+m.tmpKPP

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Tst_Type     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      蹍▲爲� ��玻オ皚�痰� 皋��.                       �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 18.01.2002 陳�
PROCEDURE Tst_Type
PRIVATE mss

IF .NOT. INLIST(c_type, 1, 2, 3)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� ��� � �� 礫����� 皋� ��┘���!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF is_country
  IF EMPTY(cntr)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"�� �� 礫����� 痰����!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
ENDIF

IF TYPE("m.cli_code") = "N"
  RETURN Tst_INN(m.cli_code, m.tmpINN)
ENDIF
RETURN .T.
