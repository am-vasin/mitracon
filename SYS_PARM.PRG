*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Sys_Parm     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                           �����モ琺 瓱痰ガ�.                           �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 02/28/2000 様�
PROCEDURE Sys_Parm

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� [ ] ��縲��閧� �牀��Θ � �♂ガ �����    �
*� [ ] ��縲��閧� ��� �爬 �爬絎ぅ 皰����   �
*� [ ] ���覃 筮爼�� ��錢��                �
*� [ ] ����腮� ��瘁�〓�� ��������         �
*� [ ] �ギ≦�え��痰� 竄▲爨ぅ��� ��錢��   �
*� ���-〓 きォ �� �牀��Θ ぎ 〓Б�. 999   �
*楽�踳ゃ�爛Δキ┘ �� ├�キキ┬ 罐�陳陳陳審
*崖 ��о�痰 甎�瓷� ｀�ゥ 99999 きォ      崖
*崖 ��キ跏キ┘ 痺��� ｀�ゥ 腑� �� 99 %   崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳抒
*�    < OK Ctrl-W > < �皖���碎瘴 Esc >    �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, sw_0, sw_1, sw_2, sw_3, sw_4, sw_5
sw_0 = .NOT. EMPTY(Get_Sys("COM_SALE"))
sw_1 = .NOT. EMPTY(Get_Sys("SAVE_GTD"))
sw_2 = .NOT. EMPTY(Get_Sys("NEW ORD"))
sw_3 = .NOT. EMPTY(Get_Sys("KKM"))
sw_4 = .NOT. EMPTY(Get_Sys("HARD SALE"))
sw_5 = ALLTRIM(Get_Sys("REV DAYS"))
sw_5 = VAL(sw_5)
IF sw_5 = 0
  sw_5 = 15
ENDIF
sw_6 = ALLTRIM(Get_Sys("SALE BOUND"))
IF EMPTY(sw_6)
  sw_6 = 0
  sw_7 = 0
ELSE
  ex = AT(" ", sw_6)
  sw_7 = VAL(SUBSTR(sw_6, ex+1))
  sw_6 = VAL(LEFT(sw_6, ex-1))
ENDIF

ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 15, 44, "�����モ琺 瓱痰ガ�"
@  8, 2 SAY "敖踳ゃ�爛Δキ┘ �� ├�キキ┬ 罐�陳陳陳�"
@  9, 2 SAY "� ��о�痰 甎�瓷� ｀�ゥ 99999 きォ      �"
@ 10, 2 SAY "� ��キ跏キ┘ 痺��� ｀�ゥ 腑� �� 99 %   �"
@ 11, 2 SAY "青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 2, 3 GET sw_0 PICTURE "@*C ��縲��閧� �牀��Θ � �♂ガ �����"
@ 3, 3 GET sw_1 PICTURE "@*C ��縲��閧� ��� �爬 �爬絎ぅ 皰����"
@ 4, 3 GET sw_2 PICTURE "@*C ���覃 筮爼�� ��錢��"
@ 5, 3 GET sw_3 PICTURE "@*C ����腮� ��瘁�〓�� ��������"
@ 6, 3 GET sw_4 PICTURE "@*C �ギ≦�え��痰� 竄▲爨ぅ��� ��錢��"
@ 7, 3 SAY "���-〓 きォ �� �牀��Θ ぎ 〓Б�." GET sw_5 PICTURE "999"
@ 9,25 GET sw_6 PICTURE "@Z 99999"
@ 10,34 GET sw_7 PICTURE "@Z 99"

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

IF ex = 1
  *
  * �矗���硅��ガ ゛���
  DO Put_Sys WITH "COM_SALE", IIF(sw_0, "!", " ")
  DO Put_Sys WITH "SAVE_GTD", IIF(sw_1, "!", " ")
  DO Put_Sys WITH "NEW ORD", IIF(sw_2, "!", " ")
  DO Put_Sys WITH "KKM", IIF(sw_3, "!", " ")
  DO Put_Sys WITH "HARD SALE", IIF(sw_4, "!", " ")
  DO Put_Sys WITH "REV DAYS", STR(sw_5)
  IF sw_6 = 0 .OR. sw_7 = 0
    sw_6 = ""
  ELSE
    sw_6 = ALLTRIM(STR(sw_6))+" "+ALLTRIM(STR(sw_7))
  ENDIF
  DO Put_Sys WITH "SALE BOUND", sw_6
  PRIVATE mss
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"��━��┘! �オ�皰琺� �����モ琺 ≡矣�閧 � ぅ�痰※� 皰�讓�"
  mss(3) = CHR(0)+"��甄� �ムェ��竅�� 瓱痰ガ� �� ��｀腮� �メ���!           "
  mss(4) = ""
  DO Out_Mess WITH 7, "mss"
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
