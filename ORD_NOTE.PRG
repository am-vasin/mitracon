*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Ord_Note     ��о�｀砒┴ ��むォ ��瓱�           20.03.98 13:50:18 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                     踸�ョ��┘ � ��錢�� �� 瓷���.                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Ord_Note
PARAMETERS tit, dt, tm, nm, to_print, queue_t

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�         ��÷�碎 � ��.��.����(F1) ��� ���         �
*�   敖 踸�ョ��┘ 陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕   �
*�   � ....:....!....:....!....:....!....:....! �   �
*�   青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰   �
*�               [ ] �ョ�碎 ぎ�祠キ��               �
*�        < OK Ctrl-W > < �皖���碎瘴 Esc >          �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, p, d, h, m, f1_butt, clr_butt, nm_w, prnt
                     &&   ｡褊←錺� � ������錺� ���� ゛����
ex   = 1             &&
d = dt
h = VAL(ALLTRIM(SUBSTR(tm,1,2)))
m = VAL(ALLTRIM(SUBSTR(tm,4,2)))
f1_butt = 1
clr_butt = 1
prnt = to_print
nm_w = nm
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
ON KEY LABEL F1 DO Set_Date
DO Prp_Nav_2
DO D_Win_N WITH 9, 54, tit
DO Sun_Bord WITH 3, 5, 5, 48, " 踸�ョ��┘ "
@ 2, 11 SAY "��÷�碎 �           (F1)   �   �"
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
IF .NOT. hard_sale
  @ 7, 10 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "
ENDIF
@ 2, 21 GET d PICTURE "@D"
*@ 2, 24 GET f1_butt PICTURE "@*N F1" VALID Set_Date()
@ 2, 36 GET h PICTURE "@Z 99"
@ 2, 40 GET m PICTURE "@Z 99"
*@ 2, 38 GET clr_butt PICTURE "@*N �� 甌；��碎" VALID Clr_Ready()
@ 4,  7 GET nm_w
@ 6, 17 GET prnt PICTURE "@*C �ョ�碎 ぎ�祠キ��"
IF hard_sale
  @ 7, 10 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "
ENDIF
READ CYCLE VALID Tst_Blank()

IF ex = 1
  to_print = prnt
  nm = nm_w
  IF EMPTY(d)
    dt = {}
    tm = "        "
  ELSE
    dt = d
    tm = RIGHT(STR(100+h),2)+":"+RIGHT(STR(100+m),2)+":00"
  ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Clr_Ready    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                        �腮痰�� ��硅 � ∇ガキ�.                         �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 05.01.2000 陳�
PROCEDURE Clr_Ready

d = {}
h = 0
m = 0
SHOW GET d
SHOW GET m
SHOW GET h

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Set_Date     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      �痰���→� ��硅 �� ���キ��瑟.                      �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 05.01.2000 陳�
PROCEDURE Set_Date

d = Get_Date(d)
SHOW GET d
IF EMPTY(d)
  h = 0
  m = 0
  SHOW GET h
  SHOW GET m
ENDIF

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Tst_Blank    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                     蹍▲爲� ��玻オ皚�痰� ゛����.                      �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 05.01.2000 陳�
PROCEDURE Tst_Blank

PRIVATE tm_w, mss

IF ex = 2
  RETURN .T.
ENDIF

tm_w = RIGHT(STR(100+h),2)+":"+RIGHT(STR(100+m),2)+":00"
IF EMPTY(d)
  RETURN .T.
ENDIF

IF .NOT. BETWEEN(tm_w, b_stock, e_stock) .OR. .NOT. BETWEEN(m, 0, 59)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� �オ�玻オ皚� 礫����� ∇ガ�, ┼� 瓷��� � 轤� ∇ガ� �� ��｀��モ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
IF .NOT. EMPTY(d) .AND. hard_sale
  IF d # dt .OR. tm_w # tm
    DO Get_Sto WITH queue_t
    IF Delta_T(d, tm_w, DATE(), TIME()) < t_stock
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"����� �� � 甌痰�鎰┬ 甌÷�碎 ��錢�� � 轤��� ∇ガキ�!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      RETURN .F.
    ENDIF
  ENDIF
ENDIF

RETURN .T.
