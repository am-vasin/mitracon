*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�          �ணࠬ�� ��।������ ᪨��� ��� ᯨ᪠.                     �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Dis_List
PARAMETERS l_num
*
*  l_num - ����� ᯨ᪠.
*
PRIVATE s_sav, is_d

s_sav = SELECT()
SELECT 0
USE (base_path+"LIST_TIT") ORDER TAG LST_NUM AGAIN ALIAS TMP_9927
IF .NOT. SEEK(l_num)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��� ⠪��� ᯨ᪠!"
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

*������������������������������������������������������������������������Ŀ
*�   ��楤�� View_Dis     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                        �����뢠�� १����...                         �
*�                                                                        �
*����������������������������������������������������������� 22.09.1999 ���
PROCEDURE View_Dis
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

*------------------------------------------------------------------------
*    ����ঠ⥫쭠� ���� �ணࠬ��:
*

*����������������������������������������������������Ŀ
*� �㬬� �� �ࠩᮢ� 業�� 99 999 999 999.99         �
*� ������                   99 999 999 999.99 999.99% �
*� ����業�                 99 999 999 999.99 999.99% �
*�                       < OK >                       �
*������������������������������������������������������

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 7, 56, "���᮪ N "+ALLTRIM(STR(l_num))+ ;
                   IIF(is_d, " (� �)"," (��)")
*------------------------------------------------------------------------
*      ���� ����� ������
*

@ 2, 3 SAY "�㬬� �� �ࠩᮢ� 業�� "+TRANSFORM(s0, "99 999 999 999.99")
@ 3, 3 SAY "������                   "+TRANSFORM(s_dis, "99 999 999 999.99")+ ;
               TRANSFORM(ROUND(100*s_dis/s0,2), " 999.99")+"%"
@ 4, 3 SAY "����業�                 "+TRANSFORM(s_sp, "99 999 999 999.99")+ ;
               TRANSFORM(ROUND(100*s_sp/s0,2), " 999.99")+"%"
@ WROWS()-2, 25 GET ex PICTURE "@*HT \ OK "

READ CYCLE

IF ex = 1
  *
  * ��ࠡ��뢠�� �����
  
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN