*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                蹍������ ��ぃ�皰→� ��瘁│�� BROWSE                   �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Md_Image
PARAMETER stop_
PRIVATE i, dr, s, fnd, s1, s2, s3, s4, rt

*l_br0  = 3           && �ム��� 痰牀�� BROWSE
*c_br0  = 2           && �ム��� ��Ж罔� BROWSE
*ln_br0 = WROWS()-4   && ����腑痰〓 痰牀� BROWSE

fl_br0 = 1           && �����皀�� BROWSE

recs_m = ""          && ���ム� ����瓮� �甅�↓��� �����
recs_c = ""          && ���ム� ����瓮� 瓣錻������ �����
imag_br = ""         && 踳め��←キ┘ 痰牀� BROWSE
vals_br = ""         && ���腑��� �÷���硅��ガ諷 ���ォ 

rt = 0
dr = 1
FOR i = 1 TO ln_br0
  IF stop_ .AND. rt = 0
    rt = INKEY()
    IF rt > 0
      IF c_t = SYS(15, chr_type, CHR(rt)) = "*"
        RETURN rt
      ENDIF
    ENDIF
  ENDIF
  fnd = SEEK(l_c+STR(CODE,7),"LD")
  s1 = STR(RECNO(),9)+CHR(13)
  s2 = IIF(fnd,STR(RECNO("LD"),9), SPACE(9))+CHR(13)
  s4 = IIF(fnd, STR(LD.QNT,6), "      ")+CHR(13)
  s3 = PREFIX+" "+NAME+" "+IIF(QNT > 0, " ", "�")+" "+s4
  IF dr = 1
    recs_m  = recs_m+s1
    recs_c  = recs_c+s2
    imag_br = imag_br+s3
    vals_br = vals_br+s4
  ELSE
    recs_m  = s1+recs_m
    recs_c  = s2+recs_c
    imag_br = s3+imag_br
    vals_br = s4+vals_br
    fl_br0  = fl_br0+1
  ENDIF
  SKIP dr
  IF EOF()
    s1 = VAL(ALLTRIM(LEFT(recs_m,9)))
    GO s1
    dr = -1
    SKIP dr
  ENDIF
  IF BOF()
    EXIT
  ENDIF
ENDFOR

RETURN rt
