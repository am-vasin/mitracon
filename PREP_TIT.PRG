*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�             蹍������ ��ぃ�皰→� ��瘁│�� 筮��.                       �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Draw_Tit
PRIVATE i, cl, a_tit, st, w, c1, c2

DIMENSION a_tit(7)

a_tit(1) = "栩栩栩栩  栩栩栩�     栩栩       栩栩�     栩栩�  �   栩   �  栩    栩"
a_tit(2) = "栩    栩  栩    栩   �    �     �   栩    �   栩   �  栩  �   栩    栩"
a_tit(3) = "栩    栩  栩    栩  栩    栩   �    栩   �    栩    � 栩 �    栩   栩�"
a_tit(4) = "栩    栩  栩    栩  栩    栩  �     栩  �     栩     栩栩     栩  � 栩"
a_tit(5) = "栩    栩  栩栩栩�   栩    栩  �     栩  �     栩    栩栩栩    栩 �  栩"
a_tit(6) = "栩    栩  栩         �    �   �     栩  栩栩栩栩   栩 栩 栩   栩�   栩"
a_tit(7) = "栩    栩  栩          栩栩   栩栩栩栩栩 �     栩  栩  栩  栩  栩    栩"

st = 15
FOR i = 0 TO WROWS()-1
  w = Rn()
  w = REPLICATE("�",i*1)+w
  w = PADR(w,80)
  @ i, 0 SAY w COLOR SCHEME 13
ENDFOR

c1 = SCHEME(13,1)
c2 = SCHEME(13,2)

DO Draw_T WITH a_tit, c1, c2
DO Draw_Info

RETURN


DIMENSION a_auto(6)

a_auto(1) = "                                          "
a_auto(2) = "   (c) ���� ������, 1994-1995             "
a_auto(3) = "              �カ/���� (095) 911-9558     "
a_auto(4) = "              E-mail: Alex@compel.msk.su  "
a_auto(5) = PADC("�ム瓱� "+vrs+"    �ィ. ���ム "+ALLTRIM(rg_nom),42)
a_auto(6) = "                                          "

ln = ALEN(a_auto)
wd = LEN(a_auto(1))

c = CEILING((WCOLS()-wd)*0.8)
l = WROWS()-ln-3

cl = SCHEME(1,8)
@ l+1, c+2 FILL TO l+ln, c+wd+1 COLOR (cl)
FOR i = 1 TO ln
  @ l+i-1, c SAY a_auto(i) COLOR SCHEME 13
ENDFOR

RETURN
****************************************************************************
PROCEDURE Rn

PRIVATE i, p, m, w0

w0 = ""
FOR i = 1 TO st
  m = i/st
  p = rand()
  w0 = w0+IIF(p>m,"�","�")
ENDFOR

FOR i = 1 TO st
  m = i/st
  p = rand()
  w0 = w0+IIF(p>m,"�","�")
ENDFOR
  
FOR i = 1 TO st
  m = i/st
  p = rand()
  w0 = w0+IIF(p>m,"�","�")
ENDFOR

FOR i = 1 TO st
  m = i/st
  p = rand()
  w0 = w0+IIF(p>m,"�"," ")
ENDFOR
RETURN w0
*********************************************************************
PROCEDURE Draw_T
PARAMETERS arr, c_c, c_s
PRIVATE i, j, l0, c0, w, cl_1, cl_2
cl_1 = "N/"+c_c
cl_2 = "N/"+c_s

l0 = FLOOR((WROWS()-ALEN(arr)-1)/2)
c0 = FLOOR((WCOLS()-LEN(arr(1))-1)/2)

FOR i = 1 TO ALEN(arr)
  FOR j = 1 TO LEN(arr(i))
    w = SUBSTR(arr(i),j,1)
    IF w = " "
      LOOP
    ENDIF
    @ l0+i-1, c0+j-1 SAY " " COLOR (cl_1)
    @ l0+i, c0+j SAY " " COLOR (cl_2)
  ENDFOR
ENDFOR
