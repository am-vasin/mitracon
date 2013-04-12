*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                �ணࠬ�� ��ᬮ�� ����� 䨫�����.                    �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE F_View

PRIVATE s, d, sd, sr, pd, pr, nr

*
*   �᫨ ��� 䠩��, � ᮧ����!
*

IF .NOT. File_O(base_path+"F_VIEW.DBF")
  CREATE DBF (base_path+"F_VIEW")  ;
       (  F_CODE  N( 8),   ;
          FIL_NAM C(16),   ;
          P_DATE  D,       ;
          USD_P   N( 6,3), ;
          SUM_D   N(10,4), ;
          SUM_R   N(10,2), ;
          PRC     N(7,3)     )
  USE (base_path+"F_VIEW") EXCLUSIVE
  INDEX ON F_CODE TAG CODE
ENDIF

*
*   ��ନ�㥬/���४��㥬 ᯨ᮪ 䨫�����
*
USE (base_path+"F_VIEW") ORDER TAG CODE

SELECT 0
USE (base_path+"CUSTOMER") ORDER TAG CODE
SET FILTER TO .NOT. EMPTY(RYN)

SELECT F_VIEW
SCAN
  IF .NOT. SEEK(F_CODE, "CUSTOMER")
    DELETE
  ENDIF
  REPLACE P_DATE WITH {},  ;
          USD_P  WITH 0,   ;
          SUM_D  WITH 0,   ;
          SUM_R  WITH 0
ENDSCAN
SET ORDER TO TAG CODE

SELECT CUSTOMER
GO TOP
SCAN
  IF .NOT. SEEK(CODE, "F_VIEW")
    SELECT F_VIEW
    APPEND BLANK
    REPLACE F_CODE  WITH CUSTOMER.CODE, ;
            FIL_NAM WITH CUSTOMER.B_NAME
  ELSE
    SELECT F_VIEW
    REPLACE FIL_NAM WITH CUSTOMER.B_NAME
  ENDIF
  SELECT CUSTOMER
ENDSCAN

*
*  ��⮢�� ����� �� 䨫�����
*
USE (base_path+"CURS_SET") ORDER TAG HRON DESCENDING ALIAS CURS

SELECT 0
USE (base_path+"RYN_L") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"RYN_L_T")
DO Wt_Mess WITH "�����⮢�� ������..."

SCAN
  IF .NOT. SEEK(CUS_CODE, "F_VIEW")
    LOOP
  ENDIF
  IF .NOT. SEEK(CUS_CODE, "RYN_L")
    LOOP
  ENDIF
  *
  *   ��।��塞 ���� ������ � �� ������
  *
  s = Get_Curs("CURS", "CURS", RYN_L_T.DATE_P)
  d = RYN_L_T.DATE_P
  sr = 0
  sd = 0
  SELECT RYN_L
  SCAN REST WHILE CUS_CODE = F_VIEW.F_CODE
    IF RYN_L_T.VALUTA = "$"
      sd = sd+QNT*PRICE_D
    ELSE
      sr = sr+QNT*PRICE_R
    ENDIF
  ENDSCAN
  
  SELECT F_VIEW
  REPLACE P_DATE WITH d,  ;
          USD_P  WITH s,  ;
          SUM_D  WITH sd, ;
          SUM_R  WITH sr

  SELECT RYN_L_T

ENDSCAN

USE

SELECT CURS
USE

SELECT RYN_L
USE

SELECT F_VIEW
SET FILTER TO SUM_R # 0 .OR. SUM_D # 0

SUM SUM_R, SUM_D, SUM_R*PRC/100, SUM_D*PRC/100 TO sr, sd, pr, pd

DO Wt_Mess

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
what_do    = "list"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Print OF (menu_name) PROMPT "����� ���㬥��" KEY Ctrl-A, "F1"
ON SELECTION PAD Print OF (menu_name) Do Sw_Mode WITH "Print"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd, pic_r, pic_d, pic_p

pic_r = "@Z 9999999.99"
pic_d = "@Z 999999.9999"
pic_p = "@Z 999.999"

ln = MIN(100, RECCOUNT())   && ������⢮ ������� ��ப BROWSE

wd = FSIZE("FIL_NAM")+53

DO D_Wins WITH ln, wd, "�������", 1, 2

@ 1, 2 SAY PADR("������",FSIZE("FIL_NAM"))+" "+ ;
           PADL("�㬬� (�)", 10)+" "+  ;
           PADL("�㬬� ($)", 10)+" "+  ;
           PADL("��業�", 7)+" "+  ;
           PADL("% (�)", 10)+" "+  ;
           PADL("% ($)", 10)
@ WROWS()-3, 3  SAY "�⮣�"
@ WROWS()-3, 19 SAY TRANSFORM(sr,pic_r)+" "+TRANSFORM(sd,pic_d)
@ WROWS()-3, 49 SAY TRANSFORM(pr,pic_r)+" "+TRANSFORM(pd,pic_d)
what_do = "list"

SET ORDER TO 0
GO TOP
DO WHILE .T.

  DO CASE

  CASE what_do = "list"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL F1 DO Sw_Mode WITH "Print"

*
*    BROWSE - ����
*
    BROWSE FIELDS FIL_NAM:H="",   ;
                  SUM_R:H="":P=pic_r, ;
                  SUM_D:H="":P=pic_d, ;
                  PRC:H="":P=pic_p:V=Show_Tot(),     ;
                  F00_R= PRC*SUM_R/100:H="":P=pic_r, ;
                  F00_D= PRC*SUM_D/100:H="":P=pic_d  ;
           FREEZE PRC      ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WHEN Show_D()   ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "list"
      what_do = IIF(READKEY() % 256 = 12, "exit", "edit")
    ENDIF

  CASE what_do = "Print"    && ����� ���㬥��

    DO P_View
    what_do = "list"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

CLEAR WINDOWS
CLOSE DATABASES
RELEASE MENU (menu_name) EXTENDED

RETURN

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                �ணࠬ�� �뢮�� ���� � ����.                         �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Show_D

PRIVATE s

s = "���� ��⠭������ "+DTOC(P_DATE)+" �� ����� "+ALLTRIM(STR(USD_P,10,3))+"      "
ACTIVATE WINDOW (win_name+"_M") SAME
@ WROWS()-2, 2 SAY s
ACTIVATE WINDOW (ALIAS())

RETURN .T.

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                �ணࠬ�� �뢮�� �⮣���� �㬬�.                       �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Show_Tot

PRIVATE s

SUM SUM_R*PRC/100, SUM_D*PRC/100 TO pr, pd
ACTIVATE WINDOW (win_name+"_M") SAME
@ WROWS()-3, 49 SAY TRANSFORM(pr,pic_r)+" "+TRANSFORM(pd,pic_d)
ACTIVATE WINDOW (ALIAS())

RETURN .T.

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                   �ணࠬ�� ���� ���㬥��.                         �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE P_View

PRIVATE p_ln, fnt, n_cp, lft, ffeed, p_drctry
PRIVATE sb, n, t_p, f_p, f_d, rc_sav

*{��.��.��}    - ��� ���㬥��;
*{Pg/Pgs}      - ����� ��࠭���/���-�� ��࠭��;
*{Tot_R_P_1  } - �㬬� � �㡫�� �� ��࠭��;
*{Tot_D_P_1  } - �㬬� � USD �� ��࠭��;
*{Tot_R_P_2  } - ��業� � �㡫�� �� ��࠭��;
*{Tot_D_P_2  } - ��業� � USD �� ��࠭��;
*{Tot_R_D_1  } - �㬬� � �㡫�� �� ���㬥���;
*{Tot_D_D_1  } - �㬬� � USD �� ���㬥���;
*{Tot_R_D_2  } - ��業� � �㡫�� �� ���㬥���;
*{Tot_D_D_2  } - ��業� � USD �� ���㬥���.

DIMENSION sb(10,2)
sb( 1,1) = "{��.��.��}"
sb( 1,2) = PADL(DTOC(DATE()), 10)

sb( 2,1) = "{Pg/Pgs}"
sb( 2,2) = ""

sb( 3,1) = "{Tot_R_P_1  }"
sb( 3,2) = ""

sb( 4,1) = "{Tot_D_P_1  }"
sb( 4,2) = ""

sb( 5,1) = "{Tot_R_P_2  }"
sb( 5,2) = ""

sb( 6,1) = "{Tot_D_P_2  }"
sb( 6,2) = ""

sb( 7,1) = "{Tot_R_D_1  }"
sb( 7,2) = ""

sb( 8,1) = "{Tot_D_D_1  }"
sb( 8,2) = ""

sb( 9,1) = "{Tot_R_D_2  }"
sb( 9,2) = ""

sb(10,1) = "{Tot_D_D_2  }"
sb(10,2) = ""

rc_sav = RECNO()

SELECT 0
USE (base_path+"DOC_FORM")
LOCATE FOR "F_VIEW" = ALLTRIM(UPPER(DOC_NAME))
p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed = DOC_FORM.F_FEED
p_drctry = DOC_FORM.P_DIR

n = MEMLINES(PAGE_H)
DIMENSION t_p(n)
FOR i = 1 TO n
  t_p(i) = MLINE(PAGE_H,i)
ENDFOR

n = MEMLINES(PAGE_F)
DIMENSION f_p(n)
FOR i = 1 TO n
  f_p(i) = MLINE(PAGE_F,i)
ENDFOR

n = MEMLINES(DOC_F)
DIMENSION f_d(n)
FOR i = 1 TO n
  f_d(i) = MLINE(DOC_F,i)
ENDFOR
USE

SELECT F_VIEW
GO TOP

DO Ini_Prn WITH "����稥 � 䨫����� (���죨)", p_ln, lft, n_cp, fnt, ffeed, p_drctry

PRIVATE n_p_det, n_pg, f_pg, f_lines
COUNT TO f_lines
GO TOP

n_p_det = p_ln-ALEN(t_p)-MAX(ALEN(f_p),ALEN(f_d))
IF n_p_det <= 0
  n_p_det = f_lines
  n_pg = 1
ELSE
  n_pg = CEILING(f_lines/n_p_det)
ENDIF

PRIVATE sp1, sp2, sp3, sp4, sd1, sd2, sd3, sd4, str_w, i, j

STORE 0 TO sd1, sd2, sd3, sd4
FOR f_pg = 1 TO n_pg
  sb( 2,2) = ALLTRIM(STR(f_pg))+"/"+ALLTRIM(STR(n_pg))
  sb( 2,2) = PADL(sb(2,2), LEN(sb(2,1)))
  IF f_pg # 1
    EJECT
  ENDIF
  FOR i = 1 TO ALEN(t_p)
    str_w = t_p(i)
    FOR j = 1 TO ALEN(sb,1)
      str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
    ENDFOR
    IF i = 1
      @ PROW(), PCOL() SAY str_w
    ELSE
      @ PROW()+1, 0 SAY str_w
    ENDIF
  ENDFOR
  STORE 0 TO sp1, sp2, sp3, sp4
  FOR i = 1 TO n_p_det

    @ PROW()+1,0 SAY   ;
       FIL_NAM+" "+PADR(DTOC(P_DATE),10)+STR(USD_P,6,3)+" "+ ;
       TRANSFORM(SUM_R,"@Z ## ### ###.##")+" "+ ;
       TRANSFORM(SUM_D,"@Z ### ###.####")+" "+ ;
       TRANSFORM(PRC,"@Z 999.999")+" "+ ;
       TRANSFORM(SUM_R*PRC/100,"@Z ## ### ###.##")+" "+ ;
       TRANSFORM(SUM_D*PRC/100,"@Z ### ###.####")
    sp1 = sp1 + SUM_R
    sp2 = sp2 + SUM_D
    sp3 = sp3 + SUM_R*PRC/100
    sp4 = sp4 + SUM_D*PRC/100
    SKIP
    IF EOF()
      EXIT
    ENDIF
  ENDFOR
  IF f_pg = n_pg
    EXIT
  ENDIF
  sb( 3,2) = TRANSFORM(sp1,"@Z ## ### ###.##")
  sb( 4,2) = TRANSFORM(sp2,"@Z ### ###.####")
  sb( 5,2) = TRANSFORM(sp3,"@Z ## ### ###.##")
  sb( 6,2) = TRANSFORM(sp4,"@Z ### ###.####")
  FOR i = 1 TO ALEN(f_p)
    str_w = f_p(i)
    FOR j = 1 TO ALEN(sb,1)
      str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
    ENDFOR
    @ PROW()+1, 0 SAY str_w
  ENDFOR
  sd1 = sd1+sp1
  sd2 = sd2+sp2
  sd3 = sd3+sp3
  sd4 = sd4+sp4
ENDFOR

sd1 = sd1+sp1
sd2 = sd2+sp2
sd3 = sd3+sp3
sd4 = sd4+sp4
sb( 3,2) = TRANSFORM(sp1,"@Z ## ### ###.##")
sb( 4,2) = TRANSFORM(sp2,"@Z ### ###.####")
sb( 5,2) = TRANSFORM(sp3,"@Z ## ### ###.##")
sb( 6,2) = TRANSFORM(sp4,"@Z ### ###.####")
sb( 7,2) = TRANSFORM(sd1,"@Z ## ### ###.##")
sb( 8,2) = TRANSFORM(sd2,"@Z ### ###.####")
sb( 9,2) = TRANSFORM(sd3,"@Z ## ### ###.##")
sb(10,2) = TRANSFORM(sd4,"@Z ### ###.####")

FOR i = 1 TO ALEN(f_d)
  str_w = f_d(i)
  FOR j = 1 TO ALEN(sb,1)
    str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
  ENDFOR
  @ PROW()+1, 0 SAY str_w
ENDFOR

DO Term_Prn WITH ""

GO rc_sav

RETURN