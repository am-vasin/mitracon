*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                 蹍������ ��ぃ�皰→� 甎�瓷��.                         �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Prep_Lst
PRIVATE l, c, wd, ln

*
*   ��爼�珮ガ ���� か� �ムョ�� 甎�瓷��
*

ln = WROWS("")-WROWS("FltL")-3
wd = FSIZE("LST_NUM","LIST_TIT")+IIF(SET("CENTURY")="ON",10,8)+   ;
     FSIZE("LST_NAME","LIST_TIT")+7
l  = 2
c  = FLOOR((WCOLS("")-wd)/2)
DEFINE WINDOW Lst_M FROM l, c TO l+ln-1, c+wd-1 SHADOW NONE COLOR SCHEME 13
DEFINE WINDOW Lst_E FROM l+2, c+2 TO l+ln-2, c+wd-3 NONE COLOR SCHEME 13
DEFINE WINDOW Lst_I FROM l-1, c TO l+ln-1, c+wd+10 NONE COLOR SCHEME 13
ACTIVATE WINDOW Lst_M BOTTOM
@ 0, 0 SAY PADC("�ムョキ� 甎�瓷��",WCOLS()) COLOR SCHEME 5
@ 1, 1 TO WROWS()-1, WCOLS()-2

RETURN
