*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�           蹍������ 甌Г���� ���� か� 甎�瓷� ��Ж罔�.                 �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE D_Win
PARAMETERS tit, tp, is_rub
*
*   tit  - ������〓�;
*   tp   - 爛Θ�: 1 - � ����皰牀�, 2 - ．� ����皰��
*
PRIVATE l, c, wd, ln, c_w
ln = WROWS("")-3
wd = 75 && 69  &&74
IF is_rub 
  wd = 77
ENDIF
l  = CEILING((WROWS("")-ln)/2)
c  = FLOOR((WCOLS("")-wd)/2)
*c_w = IIF(tp=1, 2, 1)
c_w = 1
IF .NOT. WEXIST("L_Det_M")
  DEFINE WINDOW L_Det_M FROM l, c TO l+ln-1, c+wd-1 NONE SHADOW COLOR SCHEME 13
  ACTIVATE WINDOW L_Det_M
  @ c_w, 1 TO WROWS()-IIF(sale_mode = "Mitracon", 5, 4), WCOLS()-2
  IF is_rub
    @ c_w,   0 SAY    " 敖陳���爲�牀→�陳陳陳陳陳陳陳陳��������-〓陳陳陳���.罐��陳陳����Ｄ陳�ﾄ����"
    @ WROWS()-IIF(sale_mode = "Mitracon", 5, 4), 0 SAY ;
    " 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�陳陳陳�"
*    @ WROWS()-3,0 SAY " 敖陳陳陳陳陳陳� ��爼� 陳 ���.陳���祚.�  �牀�� (��痰.�, ����.�)陳潰崖 "
*    @ WROWS()-2,0 SAY " �              ....:... ....:. 999999�  �ヤ�罔�(�), 甅閧(�)陳陳陳潰� "
*    @ WROWS()-1,0 SAY " 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰  ��┐��(�), 甎ユ罐�� (�)陳陳� "

    @ WROWS()-3,0 SAY " 敖陳陳陳陳陳陳� ��爼� 陳 ���.陳���祚.����.�.�   �牀�貭陳陳陳陳陳潰崖 "
    @ WROWS()-2,0 SAY " �              ....:... ....:. 999999 999999�   �ヤ�罔眥陳陳陳陳陳抒 "
    @ WROWS()-1,0 SAY " 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�   ��┐.(�), 甎ユ.(�)潰 "

  ELSE
    @ c_w,   0 SAY    " 敖陳���爲�牀→�陳陳陳陳陳陳陳陳��������-〓陳陳��.罐��陳����Ｄ陳陳陳ﾄ����"
    @ WROWS()-IIF(sale_mode = "Mitracon", 5, 4), 0 SAY ;
    " 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�陳陳陳�"
    @ WROWS()-3,0 SAY " 敖陳陳陳陳陳陳� ��爼� 陳 ���.陳���祚.����.�.� �牀�貭陳陳陳陳陳潰崖 "
    @ WROWS()-2,0 SAY " �              ....:... ....:. 999999 999999� �ヤ�罔眥陳陳陳陳陳抒 "
    @ WROWS()-1,0 SAY " 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� ��┐.(�), 甎ユ.(�)潰 "
*    @ WROWS()-3,0 SAY " 敖陳陳陳陳陳陳� ��爼� 陳 ���.陳���祚.� �牀�� (��痰.�, ����.�)抒� "
*    @ WROWS()-2,0 SAY " �              ....:... ....:. 999999� �ヤ�罔�(�), 甅閧(�)陳陳抒 "
*    @ WROWS()-1,0 SAY " 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 ��┐��(�), 甎ユ罐�� (�)潰 "
  ENDIF
ENDIF
ACTIVATE WINDOW L_Det_M
@ 0, 0 SAY PADC(tit,WCOLS()) COLOR SCHEME 5
*@ WROWS()-2, 3 SAY STR(sum_list,12) COLOR SCHEME 1
ln = ln-IIF(sale_mode = "Mitracon", 6, 5)-c_w
wd = wd-4
l  = l+c_w+1
c  = c+2
IF .NOT. WEXIST("L_Det_E")
  DEFINE WINDOW L_Det_E FROM l, c TO l+ln-1, c+wd-1 NONE COLOR SCHEME 13
ELSE
  ZOOM WINDOW L_Det_E NORM AT l, c SIZE ln, wd
ENDIF

ln = ln+4
wd = wd+20
l  = l-3
c  = c-2
IF .NOT. WEXIST("L_Det_I")
  DEFINE WINDOW L_Det_I FROM l, c TO l+ln-1, c+wd-1 NONE SHADOW COLOR (cl_w)
ELSE
  ZOOM WINDOW L_Det_I NORM AT l, c SIZE ln, wd
ENDIF

RETURN
