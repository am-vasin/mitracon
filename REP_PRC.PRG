*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Rep_Prc                                                           �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                   �キ� ��皰�襯, �カ����皰�襯, 牀Л�膈襯 ...            �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様 08.08.2001 夕
PROCEDURE Rep_Prc
PARAMETERS p_code
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
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

PRIVATE sel_sav, w_name
PRIVATE prc_o, prc_o_r, prc_mo, prc_mo_r, prc_r, prc_r_r, qtr

PUSH KEY CLEAR
sel_sav = SELECT()

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS S_1808 AGAIN

SEEK p_code
w_name = ALLTRIM(PREFIX+" "+NAME+" "+PRODUCER)

prc_o    = PRICE_O    && ��皰��� 罐�� � �.�.
prc_o_r  = PRICE_O_R  && ��皰��� 罐�� � 珮�. 
prc_mo   = PRICE_O1   && �カ����皰��� 罐�� � �.�.
prc_mo_r = PRICE_O1_R && �カ����皰��� 罐�� � 珮�. 
prc_r    = PRICE      && 牀Л�膈�� 罐�� � �.�. 
prc_r_r  = PRICE_R    && 牀Л�膈�� 罐�� � 珮�. 
qtr      = QNT_OPT    && �カ����皰〓� ����腑痰〓
*qtr     = QNT_ROUND  && ��爼� 祚���→�

PRIVATE wd, l0, c0
wd = 50
l0 = FLOOR((WROWS("")-8)/2)
c0 = FLOOR((WCOLS("")-wd)/2)
DEFINE WINDOW W_PRC FROM l0, c0 TO IIF(qtr=< 1,l0+9,l0+7), c0+wd-1 NONE SHADOW COLOR SCHEME 13
ACTIVATE WINDOW W_PRC
DO Sun_Bord WITH 0, 1, WROWS()-1, WCOLS()-2
@ 1, 2 SAY "��Ж罔� "+ w_name
@ 3, 8 SAY "�キ�:  ｯ皰���  �カ�����.  ��Л����"
@ 4, 8 SAY "USD "+STR(price_o,10,2)+STR(prc_mo,11,2)+STR(prc_r,10,2)
@ 5, 8 SAY "�磧."+STR(price_o_r,10,2)+STR(prc_mo_r,11,2)+STR(prc_r_r,10,2)
IF qtr =< 1
  @ 7, 10 SAY "��爼� 祚���→� �� 竅����←キ� !"
  @ 9, 15 SAY "��Μ�皀 艪�-�─磴�..."
ELSE
  @ 7, 15 SAY "��Μ�皀 艪�-�─磴�..."
ENDIF

= INKEY(0, "MH")
RELEASE WINDOW W_PRC

SELECT S_1808
USE

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

SELECT(sel_sav)
POP KEY

RETURN