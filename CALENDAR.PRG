*������������������������������������������������������������������������ͻ
*� ���� Calendar     ���ࠡ��稪 ���३ + ����          08.04.97 20:26:38 �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                     �ணࠬ�� �ᮢ���� ���������.                     �
*�                                                                        �
*������������������������������������������������������������������������ͼ
PROCEDURE Calendar
PARAMETERS ye, mn
PRIVATE arr_w, title, i, s_date, j, cl_sav, win_name, ln, wd, n
PRIVATE col0, col1, sel_sav

SET DATE GERMAN
s_date = CTOD("01."+STR(mn,2)+"."+STR(ye,4))

sel_sav = SELECT()
SELECT 0
USE (base_path+"MOULD") ORDER TAG MONTH ALIAS T8765 AGAIN
IF SEEK(STR(ye,4)+STR(mn,2))
  arr_w = ALLTRIM(DAYS)
ELSE
  i = s_date
  arr_w = ""
  DO WHILE MONTH(i) = mn
    arr_w = arr_w+IIF(INLIST(DOW(i), 1, 7), "�", "�")
    i = i+1
  ENDDO
ENDIF
USE
SELECT (sel_sav)

*
*   ��ନ�㥬 ��������� ����!
*
title = ""
DO CASE
CASE mn = 1
  title = "������"
CASE mn = 2
  title = "���ࠫ�"
CASE mn = 3
  title = "����"
CASE mn = 4
  title = "��५�"
CASE mn = 5
  title = "���"
CASE mn = 6
  title = "���"
CASE mn = 7
  title = "���"
CASE mn = 8
  title = "������"
CASE mn = 9
  title = "�������"
CASE mn = 10
  title = "������"
CASE mn = 11
  title = "�����"
CASE mn = 12
  title = "�������"
ENDCASE

title = title + STR(ye,5)

cl_sav = 5
DIMENSION cl(LEN(arr_w)), rw(LEN(arr_w)), h_sw(LEN(arr_w))
FOR i = 1 TO LEN(arr_w)
  j = DOW(s_date)
  rw(i) = 1+IIF(j = 1, 7, j-1)
  cl(i) = cl_sav
  IF j = 1
    cl_sav = cl_sav+3
  ENDIF
  h_sw(i) = SUBSTR(arr_w,i) = "�"
  s_date = s_date+1

ENDFOR
n = ALEN(h_sw)

win_name = "Calendar"
ln = 10
wd = cl(n)+4
DO D_Win_N WITH ln, wd, title

col0   = SCHEME(13,1)
col1   = SCHEME(14,2)

@ 2, 2 SAY "��" COLOR (col0)
@ 3, 2 SAY "��" COLOR (col0)
@ 4, 2 SAY "��" COLOR (col0)
@ 5, 2 SAY "��" COLOR (col0)
@ 6, 2 SAY "��" COLOR (col0)
@ 7, 2 SAY "��" COLOR (col1)
@ 8, 2 SAY "��" COLOR (col1)

FOR i = 1 TO n
  @ rw(i), cl(i) SAY STR(i,2) COLOR (IIF(h_sw(i), col1, col0))
ENDFOR

RETURN