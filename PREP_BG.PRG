*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�             蹍������ ��ぃ�皰→� ��瘁│� 筮��.                        �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Prep_Bg
PRIVATE st_1, st_2, n_rows, n_cols, i0

n_rows = WROWS("")
n_cols = WCOLS("")

st_1 = ROUND(n_cols/5,0)
st_2 = st_1/n_rows

DIMENSION bg_arr(n_rows)

FOR i0 = 1 TO n_rows
  bg_arr(i0) = PADR(REPLICATE(" ",(i0-1)*st_2)+Rn(), n_cols, "�")
ENDFOR

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
FOR i = 1 TO st_1
  m = i/st_1
  p = RAND()
  w0 = w0+IIF(p<m,"�"," ")
ENDFOR
  
FOR i = 1 TO st_1
  m = i/st_1
  p = RAND()
  w0 = w0+IIF(p<m,"�","�")
ENDFOR

FOR i = 1 TO st_1
  m = i/st_1
  p = RAND()
  w0 = w0+IIF(p<m,"�","�")
ENDFOR

FOR i = 1 TO st_1
  m = i/st_1
  p = RAND()
  w0 = w0+IIF(p<m,"�","�")
ENDFOR
RETURN w0
