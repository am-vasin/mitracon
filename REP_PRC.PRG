*������������������������������������������������������������������������ͻ
*� ���� Rep_Prc                                                           �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                   ���� ��⮢�, �������⮢�, ஧���� ...            �
*�                                                                        �
*������������������������������������������������������������ 08.08.2001 ͼ
PROCEDURE Rep_Prc
PARAMETERS p_code
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
*   ���뢠�� 䠩�� �.�., ��⠭�������� �裡 � �. �.
*
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS S_1808 AGAIN

SEEK p_code
w_name = ALLTRIM(PREFIX+" "+NAME+" "+PRODUCER)

prc_o    = PRICE_O    && ��⮢�� 業� � �.�.
prc_o_r  = PRICE_O_R  && ��⮢�� 業� � ��. 
prc_mo   = PRICE_O1   && �������⮢�� 業� � �.�.
prc_mo_r = PRICE_O1_R && �������⮢�� 業� � ��. 
prc_r    = PRICE      && ஧��筠� 業� � �.�. 
prc_r_r  = PRICE_R    && ஧��筠� 業� � ��. 
qtr      = QNT_OPT    && �������⮢�� ������⢮
*qtr     = QNT_ROUND  && ��ଠ 㯠�����

PRIVATE wd, l0, c0
wd = 50
l0 = FLOOR((WROWS("")-8)/2)
c0 = FLOOR((WCOLS("")-wd)/2)
DEFINE WINDOW W_PRC FROM l0, c0 TO IIF(qtr=< 1,l0+9,l0+7), c0+wd-1 NONE SHADOW COLOR SCHEME 13
ACTIVATE WINDOW W_PRC
DO Sun_Bord WITH 0, 1, WROWS()-1, WCOLS()-2
@ 1, 2 SAY "������ "+ w_name
@ 3, 8 SAY "����:  ��⮢��  ��������.  �������"
@ 4, 8 SAY "USD "+STR(price_o,10,2)+STR(prc_mo,11,2)+STR(prc_r,10,2)
@ 5, 8 SAY "��."+STR(price_o_r,10,2)+STR(prc_mo_r,11,2)+STR(prc_r_r,10,2)
IF qtr =< 1
  @ 7, 10 SAY "��ଠ 㯠����� �� ��⠭������ !"
  @ 9, 15 SAY "������ ��-�����..."
ELSE
  @ 7, 15 SAY "������ ��-�����..."
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