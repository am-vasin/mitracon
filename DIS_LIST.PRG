*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�          蹍������ ��爛ぅ�キ�� 瓷┐�� か� 甎�瓷�.                     �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Dis_List
PARAMETERS l_num
*
*  l_num - ���ム 甎�瓷�.
*
PRIVATE s_sav, is_d

s_sav = SELECT()
SELECT 0
USE (base_path+"LIST_TIT") ORDER TAG LST_NUM AGAIN ALIAS TMP_9927
IF .NOT. SEEK(l_num)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�モ ������ 甎�瓷�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  RETURN
ENDIF
is_d = IS_USD
USE (base_path+"LIST_DET") ORDER TAG LIST AGAIN ALIAS TMP_9927
PRIVATE s0, s_sp, s_dis
s0    = 0
s_sp  = 0
s_dis = 0
SEEK STR(l_num,6)
SCAN REST WHILE STR(l_num,6) = LIST_NUM
  IF is_d
    IF EMPTY(IS_SPEC)
      s_dis = s_dis + QNT*ROUND(USD_DEF-USD_PRICE,4)
    ELSE
      s_sp = s_sp + QNT*ROUND(USD_DEF-USD_PRICE,4)
    ENDIF
    s0 = s0+QNT*ROUND(USD_DEF,4)
  ELSE
    IF EMPTY(IS_SPEC)
      s_dis = s_dis + QNT*ROUND(DEF_PRICE-SAL_PRICE,2)
    ELSE
      s_sp = s_sp + QNT*ROUND(DEF_PRICE-SAL_PRICE,2)
    ENDIF
    s0 = s0+QNT*ROUND(DEF_PRICE,2)
  ENDIF
ENDSCAN

DO View_Dis

USE
SELECT (s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� View_Dis     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                        ����щ��ガ 爛с�赳��...                         �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 22.09.1999 陳�
PROCEDURE View_Dis
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� �祠�� �� ����甌�覓 罐��� 99 999 999 999.99         �
*� ��┐��                   99 999 999 999.99 999.99% �
*� ��ユ罐��                 99 999 999 999.99 999.99% �
*�                       < OK >                       �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 7, 56, "���甌� N "+ALLTRIM(STR(l_num))+ ;
                   IIF(is_d, " (� �)"," (珮�)")
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@ 2, 3 SAY "�祠�� �� ����甌�覓 罐��� "+TRANSFORM(s0, "99 999 999 999.99")
@ 3, 3 SAY "��┐��                   "+TRANSFORM(s_dis, "99 999 999 999.99")+ ;
               TRANSFORM(ROUND(100*s_dis/s0,2), " 999.99")+"%"
@ 4, 3 SAY "��ユ罐��                 "+TRANSFORM(s_sp, "99 999 999 999.99")+ ;
               TRANSFORM(ROUND(100*s_sp/s0,2), " 999.99")+"%"
@ WROWS()-2, 25 GET ex PICTURE "@*HT \ OK "

READ CYCLE

IF ex = 1
  *
  * �矗���硅��ガ ゛���
  
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
