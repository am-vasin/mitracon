*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Lst_Sum      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                            �祠�� �� 甎�瓷�.                            �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 06.03.1999 様�
PROCEDURE Lst_Sum
PARAMETERS l_num

PRIVATE s_sav, s000, is_rub, mss, l_chr

s_sav = SELECT()
SELECT 0
USE (base_path+"LIST_TIT") ORDER TAG LST_NUM ALIAS L_9306 AGAIN
IF .NOT. SEEK(l_num)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�モ 甎�瓷� � ���ム�� "+ALLTRIM(STR(l_num))+"!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  RETURN
ENDIF
is_rub = .NOT. IS_USD

USE (base_path+"LIST_DET") ORDER TAG LIST ALIAS L_9306 AGAIN
s000 = 0
l_chr = STR(l_num,6)
SEEK l_chr
SCAN REST WHILE l_chr = LIST_NUM
  IF is_rub
    s000 = s000+round(SAL_PRICE*QNT,2)
  ELSE
    s000 = s000+USD_PRICE*QNT
  ENDIF
ENDSCAN
USE
SELECT (s_sav)

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
*�祠�� �� 甎�瓷� N 999999
*敖陳陳陳陳陳陳陳陳陳陳陳陳�
*�   99999999999.999 珮�   �
*�                         �
*�  ��Μ�皀 艪�-�─磴�...  �
*青陳陳陳陳陳陳陳陳陳陳陳陳�

*------------------------------------------------------------------------
*    ��ぅ爨�皀�讚�� ��痰� �牀������:
*

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO D_Win_N WITH 6, 29, "�祠�� �� 甎�瓷� N "+ALLTRIM(l_chr)

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
IF is_rub
  @ 2, 2 SAY PADC(ALLTRIM(TRANSFORM(s000, "999999999999.99")+" 珮�"), WCOLS()-4)
ELSE
  @ 2, 2 SAY PADC(ALLTRIM(TRANSFORM(s000, "9999999999.9999")+" USD"), WCOLS()-4)
ENDIF
@ 4, 4 SAY "��Μ�皀 艪�-�─磴�..."
= INKEY(0, "MH")
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
