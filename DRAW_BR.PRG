*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                    蹍������ �牀爬甌→� BROWSE                        �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Draw_Br
PRIVATE i

*l_br0  = 3           && �ム��� 痰牀�� BROWSE
*c_br0  = 2           && �ム��� ��Ж罔� BROWSE
*ln_br0 = WROWS()-4   && ����腑痰〓 痰牀� BROWSE

*fl_br0 = 1           && �����皀�� BROWSE

*recs_m = ""          && ���ム� ����瓮� �甅�↓��� �����
*recs_c = ""          && ���ム� ����瓮� 瓣錻������ �����
*imag_br = ""         && 踳め��←キ┘ 痰牀� BROWSE
*vals_br = ""         && ���腑��� �÷���硅��ガ諷 ���ォ 

FOR i = 1 TO ln_br0
  @ l_br0+i-1, c_br0 SAY Get_Part(imag_br,i)
ENDFOR
i = l_br0+fl_br0-1
@ i, c_br0 FILL TO i, WCOLS()-3 COLOR SCHEME 1
RETURN

PROCEDURE Get_Part
PARAMETERS shab, n_part
PRIVATE i0, i1

IF n_part = 1
  i0 = 1
ELSE
  i0 = AT(CHR(13), shab, n_part-1)+1
ENDIF
i1 = AT(CHR(13), shab, n_part)

RETURN SUBSTR(shab, i0, i1-i0)

