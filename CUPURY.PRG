*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Cupury       ��о�｀砒┴ ��むォ ��瓱�           24.12.97 13:48:45 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                             �腑砒┴ �祚釵.                             �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Cupury
PARAMETERS valuta, val_list, mov_l, mov_c
*
* valuta   - ���鈞� ( "$" - ぎ���琺, � �遏�� む磽�� 甄竍�� - 珮゛� );
* val_list - 甎�甌� ����腑痰�;
* mov_l   - 瓩※� ���� �� ▲珥┴���;
* mov_c   - 瓩※� ���� �� ��爬М�����.
*
PRIVATE IsUSD, DeltaL, DeltaC

IsUSD = "�"
IF TYPE("valuta") = "C"
  IsUSD = IIF(valuta = "$", "$", "�")
ENDIF

IF IsUSD = "$"
  DO Prep_Cup WITH "$"
ELSE
  DO Prep_Cup WITH "�"
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� 敖� �祚釵� 陳陳� ���-〓 朕 �
*� �  000000000000  000000  � �
*� 青陳陳陳陳陳陳陳陳陳陳陳潰 �
*�    �皰��  999 999 999.99   �
*�< OK ^W > < �皖���碎瘴 Esc >�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, res_sum, ln_arr, tit, det_n, i, mov1, mov2
*
*    ��Кム ��あ└�� ����
*
mov1 = 0
mov2 = 0
IF TYPE("mov_l") = "N"
  mov1 = mov_l
ENDIF
IF TYPE("mov_c") = "N"
  mov2 = mov_c
ENDIF

ex      = 1
res_sum = 0
*    ������〓� ����
tit     = IIF(IsUSD = "$", "�����琺", "�磧��")
ln_arr  = IIF(IsUSD = "$", ALEN(CupUsdN), ALEN(CupRubN))
DIMENSION det_n(ln_arr)
det_n = 0
i = "VAL_LIST(" + ALLTRIM(STR(ln_arr)) + ")"
IF TYPE(i) = "N"
  FOR i = 1 TO ln_arr
    ln = val_list(i)
    det_n(i) = IIF(TYPE("ln") = "N", MAX(ln,0), 0)
    res_sum = res_sum + IIF(IsUSD = "$", CupUsdW(i), CupRubW(i))*det_n(i)
  ENDFOR
ENDIF
ln = ln_arr+7

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH ln, 32, tit
MOVE WINDOW (win_name) BY mov1, mov2
DO Sun_Bord WITH 2, 3, 3+ln_arr, 28
@ 2, 6 SAY " �祚釵� "
@ 2,19 SAY " ���-〓 "
@ WROWS()-3, 6 SAY "�皰��"
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
FOR i = 1 TO ln_arr
  @ 2+i, 6 SAY IIF(IsUSD = "$", CupUsdN(i), CupRubN(i))+" "  ;
           GET det_n(i) PICTURE "@Z 999999" VALID Calc_V() WHEN Fill_W()
ENDFOR
@ WROWS()-2, 2 GET ex PICTURE "@*HT \! OK ^W ;\? �皖���碎瘴 Esc " ;
                              WHEN Fill_W()

DO Fill_W
DO Calc_V

READ CYCLE

IF ex = 1
  IF TYPE("val_list") # "U"
    DIMENSION val_list(ln_arr)
    FOR i = 1 TO ln_arr
      val_list(i) = det_n(i)
    ENDFOR
  ENDIF
ELSE
  IF TYPE("val_list") # "U"
    DIMENSION val_list(ln_arr)
    FOR i = 1 TO ln_arr
      val_list(i) = 0
    ENDFOR
  ENDIF
  res_sum = 0
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN res_sum

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                 ��ぃ�皰→� ��瘁│�� � ������┘� �祚釵.                 �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 24.12.97 14:05:14 夕
PROCEDURE Prep_Cup
PARAMETER CType  && �┓ �祚釵

PRIVATE sel_sav, i

IF CType = "$"
  IF TYPE("CupUsdN") = "U"    && �モ �������� �祚釵 - 甌Г�碎!
    PUBLIC CupUsdN, CupUsdW, CupUsdC
    sel_sav = SELECT()
    SELECT 0
    USE (base_path+"CUPURY") ORDER TAG VISUAL
    i = 0
    SEEK CType
    SCAN REST WHILE CUPURY.VALUTA = CType
      i = i+1
      DIMENSION CupUsdN(i), CupUsdW(i), CupUsdC(i)
      CupUsdN(i) = CUPURY.NAME
      CupUsdW(i) = CUPURY.WAIGHT
      CupUsdC(i) = CUPURY.CODE
    ENDSCAN
    USE
    SELECT (sel_sav)
  ENDIF
ELSE
  IF TYPE("CupRubN") = "U"    && �モ �������� �祚釵 - 甌Г�碎!
    PUBLIC CupRubN, CupRubW, CupRubC
    sel_sav = SELECT()
    SELECT 0
    USE (base_path+"CUPURY") ORDER TAG VISUAL
    i = 0
    SEEK CType
    SCAN REST WHILE CUPURY.VALUTA = CType
      i = i+1
      DIMENSION CupRubN(i), CupRubW(i), CupRubC(i)
      CupRubN(i) = CUPURY.NAME
      CupRubW(i) = CUPURY.WAIGHT
      CupRubC(i) = CUPURY.CODE
    ENDSCAN
    USE
    SELECT (sel_sav)
  ENDIF
ENDIF

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                             ��瘍モ 痺���.                              �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 24.12.97 16:29:24 夕
PROCEDURE Calc_V
PRIVATE i
res_sum = 0

IF IsUSD = "$"
  FOR i = 1 TO ln_arr
    res_sum = res_sum+CupUsdW(i)*det_n(i)
  ENDFOR
ELSE
  FOR i = 1 TO ln_arr
    res_sum = res_sum+CupRubW(i)*det_n(i)
  ENDFOR
ENDIF

@ WROWS()-3, 13 SAY res_sum PICTURE "999 999 999.99"

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                             ��瓷��瓷� ����.                            �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 24.12.97 16:29:24 夕
PROCEDURE Fill_W

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� 敖� �祚釵� 陳陳� ���-〓 朕 �
*� �  000000000000  000000  � �
*� 青陳陳陳陳陳陳陳陳陳陳陳潰 �
*�    �皰��  999 999 999.99   �
*�< OK ^W > < �皖���碎瘴 Esc >�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳潰
@ 3, 4 FILL TO 2+ln_arr, 27 COLOR SCHEME 13

RETURN .T.
