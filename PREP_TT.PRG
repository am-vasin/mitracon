*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�             蹍������ ��ぃ�皰→� ��瘁│� ��Б���� 瓱痰ガ�.            �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Prep_Tt

DIMENSION bg_tit(7)

bg_tit(1) = "栩栩栩栩  栩栩栩�     栩栩       栩栩�     栩栩�  �   栩   �  栩    栩"
bg_tit(2) = "栩    栩  栩    栩   �    �     �   栩    �   栩   �  栩  �   栩    栩"
bg_tit(3) = "栩    栩  栩    栩  栩    栩   �    栩   �    栩    � 栩 �    栩   栩�"
bg_tit(4) = "栩    栩  栩    栩  栩    栩  �     栩  �     栩     栩栩     栩  � 栩"
bg_tit(5) = "栩    栩  栩栩栩�   栩    栩  �     栩  �     栩    栩栩栩    栩 �  栩"
bg_tit(6) = "栩    栩  栩         �    �   �     栩  栩栩栩栩   栩 栩 栩   栩�   栩"
bg_tit(7) = "栩    栩  栩          栩栩   栩栩栩栩栩 �     栩  栩  栩  栩  栩    栩"

c1 = SCHEME(5,1)
c2 = SCHEME(5,2)

DO Draw_T
*********************************************************************
PROCEDURE Draw_T

PRIVATE i, j, l0, c0, w

l0 = FLOOR((WROWS()-ALEN(bg_tit)-1)/2)
c0 = FLOOR((WCOLS()-LEN(bg_tit(1))-1)/2)

FOR i = 1 TO ALEN(bg_tit)
  FOR j = 1 TO LEN(bg_tit(i))
    w = SUBSTR(bg_tit(i),j,1)
    IF w = " "
      LOOP
    ENDIF
    @ l0+i-1, c0+j-1 SAY "�" COLOR (SCHEME(13,2))
    @ l0+i, c0+j SAY " " COLOR  (SCHEME(13,2))
  ENDFOR
ENDFOR
