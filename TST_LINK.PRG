*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Tst_Link     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                   蹍▲爲� ����爬皙� ���瓷� ��Ж罔�.                   �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 02/10/2000 様�
PROCEDURE Tst_Link
*
*  �ムガキ�襯 甌痰�鎰�� か� ��※��罔�
*
SET DELE ON
SET EXCLU OFF

USE Z:\STORE\BASE\STOCK
SELECT 0
USE LINK_POS ORDER TAG NAME1

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*� 敖 ��Ж罔� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕 �
*� � XXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX XXXXXXXX � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*�           < OK Ctrl-W > < �皖���碎瘴 Esc >            �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex, p0, n0, f0, np
ex = 1
p0 = SPACE(4)
n0 = SPACE(35)
f0 = SPACE(8)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 7, 59, "蹍▲爲� ����爬皙� ���瓷�"
DO Sun_Bord WITH  2,  3,  4, 55, " ��Ж罔� "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
DO WHILE .T.
  @ 3,  5 GET p0
  @ 3, 10 GET n0
  @ 3, 46 GET f0
  @ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

  READ CYCLE

  IF ex = 1
    np = Link_Pos(p0, n0, f0)
    *
    * �矗���硅��ガ ゛���
    IF BETWEEN(np, 1, RECCOUNT("STOCK"))
      GO np IN STOCK
      WAIT WINDOW STOCK.PREFIX+" "+STOCK.NAME+" "+STOCK.PRODUCER
    ELSE
      WAIT WIND "�� ��腑�� ��ゅ�わ薀��! (� �� 痺え皀 �キ� 痰牀��!)"
    ENDIF
  ELSE
    EXIT
  ENDIF
ENDDO
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
