*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Calendar     ��о�｀砒┴ ��むォ + ����          08.04.97 20:26:38 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                     蹍������ 爬甌����� ��．��.                        �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Tabel
PRIVATE arr_w, title, i, s_date, date_sav, j, cl_sav, win_name, ln, wd, n
PRIVATE col0, col1

SCATTER TO arr_w

*
*   ��爼�珮ガ ������〓� ����!
*
DO CASE
CASE MONTH = 1
  title = "����瑕"
CASE MONTH = 2
  title = "�ア����"
CASE MONTH = 3
  title = "��珥"
CASE MONTH = 4
  title = "��爛��"
CASE MONTH = 5
  title = "���"
CASE MONTH = 6
  title = "�遶�"
CASE MONTH = 7
  title = "�遨�"
CASE MONTH = 8
  title = "�■竅�"
CASE MONTH = 9
  title = "�キ硼÷�"
CASE MONTH = 10
  title = "ｪ硼÷�"
CASE MONTH = 11
  title = "��錙瑕"
CASE MONTH = 12
  title = "�オ�÷�"
ENDCASE

title = title + STR(YEAR,5)+"  "+PS.FAMILY

date_sav = SET("DATE")
SET DATE GERMAN

s_date = CTOD("01."+STR(MONTH,2)+"."+STR(YEAR,4))
cl_sav = 5
FOR i = 3 TO 33
  IF arr_w(i) = "!"
    EXIT
  ENDIF
  DIMENSION cl(i-2), rw(i-2), h_sw(i-2)
  j = DOW(s_date)
  rw(i-2) = 1+IIF(j = 1, 7, j-1)
  cl(i-2) = cl_sav
  IF j = 1
    cl_sav = cl_sav+3
  ENDIF
  h_sw(i-2) = arr_w(i) # " "
  s_date = s_date+1

ENDFOR
n = ALEN(h_sw)

win_name = "TABEL"
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

