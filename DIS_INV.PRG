*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                  蹍������ ��爛ぅ�キ�� 瓷┐��.                        �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Dis_Inv
PARAMETERS f_code, inv_n, inv_d
*
*  f_code - ��� �爛く爬閧��;
*  inv_n  - ���ム �����き�� ( ↓竄爛��┤ 筮爼�� );
*  inv_d  - �─� ���� �����き�� �─� ��� � 瓱�〓�讚�� ┼� 腮甄�〓� 筮爼�皀;
*
PRIVATE s_sav, d_year, mss

IF TYPE("inv_d") = "D"
  d_year = LEFT(DTOS(inv_d),4)
ENDIF
IF TYPE("inv_d") = "N"
  d_year = STR(inv_d,4)
ENDIF
IF TYPE("inv_d") = "C"
  d_year = inv_d
ENDIF

s_sav = SELECT()
SELECT 0
USE (base_path+"SALE") ORDER TAG DOC_NUM AGAIN ALIAS SL_9927
IF .NOT. SEEK(f_code+d_year+inv_n)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�����き�� �� ���ぅ�� ┼� �竅��."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  RETURN
ENDIF
PRIVATE s0, s_sp, s_dis
s0    = 0
s_sp  = 0
s_dis = 0
SCAN REST WHILE f_code = FIRM .AND. d_year = LEFT(DTOS(DOC_DATE),4) .AND. inv_n = DOC_NUM
  IF PRICE_TYPE = "S"
    s_sp = s_sp + QNT*ROUND(PRICE_R_L-PRICE_R,2)
  ENDIF
  IF PRICE_TYPE = "D"
    s_dis = s_dis + QNT*ROUND(PRICE_R_L-PRICE_R,2)
  ENDIF
  s0 = s0+QNT*ROUND(PRICE_R_L,2)
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
DO D_Win_N WITH 7, 56, "�����き�� N "+LEFT(inv_n,4)+"-"+ALLTRIM(SUBSTR(inv_n,5))+ ;
                   "/"+f_code
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
