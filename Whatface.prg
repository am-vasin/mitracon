*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� WhatFace     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                              ��� ������                               �
*�                                                                        �
*����������������������������������������������������������� 18.01.2002 �ͼ
PROCEDURE WhatFace
PARAMETER s_value, cli_code
PRIVATE s_sel, is_country, tmpINN, tmpKPP
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
m.tmpINN = ""
m.tmpKPP = ""
IF TYPE("m.cli_code") = "N"
  DO Get_INN WITH m.cli_code, m.tmpINN, m.tmpKPP
ENDIF
*------------------------------------------------------------------------
*    ����ঠ⥫쭠� ���� �ணࠬ��:
*
*   ��� ������
*�������������������������Ŀ
*� ��࠭� ....:....1....:. �
*�  ( ) �ਤ��᪮� ���   �
*�  ( ) �।�ਭ���⥫�    �
*�  ( ) ���⭮� ���       �
*�   ��� ....:....1..      �
*�   ��� ....:....1....:.  �
*�         < OK >          �
*���������������������������

PRIVATE ex, c_type
IF TYPE("s_value") = "C"
  c_type = VAL(s_value)+1
ELSE
  c_type = 0
ENDIF

ex = 1
*------------------------------------------------------------------------
is_country = TYPE("cntr") = "C"
PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH IIF(is_country, 8, 7)+IIF(TYPE("m.cli_code") = "N", 2, 0), 29, "��� ������"

*------------------------------------------------------------------------
*      ���� ����� ������
*

IF is_country
  @ 2, 3 SAY "��࠭�" GET cntr
ENDIF
@ IIF(is_country, 3, 2), 3 GET c_type PICTURE "@*RV �ਤ��᪮� ���;�।�ਭ���⥫�;���⭮� ���"
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

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Tst_Type     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                      �஢�ઠ ���४⭮�� ⨯�.                       �
*�                                                                        �
*����������������������������������������������������������� 18.01.2002 ���
PROCEDURE Tst_Type
PRIVATE mss

IF .NOT. INLIST(c_type, 1, 2, 3)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� ⠪ � �� 㪠���� ⨯ ������!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF is_country
  IF EMPTY(cntr)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"�� �� 㪠���� ��࠭�!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
ENDIF

IF TYPE("m.cli_code") = "N"
  RETURN Tst_INN(m.cli_code, m.tmpINN)
ENDIF
RETURN .T.
