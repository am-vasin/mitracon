*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� P_Act        ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                         �ョ�碎 �爬ガ���� ����.                         �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 27.01.1999 様�
PROCEDURE P_Act
PARAMETERS firm_c, doc_n, doc_d, flg
*
*  firm_c - ��� �爛く爬閧��;
*  doc_n  - ���ム ぎ�祠キ��;
*  doc_d  - �─� ���� ぎ�祠キ��, �─� ��� � 瓱�〓�讚�� ┼� 腮甄�〓� 筮爼�皀;
*  flg    - 筰�Κ�.
*

PRIVATE s_sav   && ���ム ��｀腑� �゛�痰� か� 〓Б����
PRIVATE d_year  && ��� ぎ�祠キ��
PRIVATE d_n     && ���ム ぎ�祠キ�� � 皀�痰�〓� ※ぅ
PRIVATE d_d     && ���� ぎ�祠キ�� � 皀�痰�〓� ※ぅ
PRIVATE c_name  && ��� ��┘��� (メ�� ����〓� ━ゥ� �メ皰)
PRIVATE c_code  && ��� ��┘��� (メ�� ����〓� ━ゥ� �メ皰)
PRIVATE sb      && ��瘁│ �������� �瓮△����ォ
PRIVATE t_d     && ��瘁│ 甌 痰牀���� ��゛��� "������〓� ぎ�祠モ�"
PRIVATE t_p     && ��瘁│ 甌 痰牀���� ��゛��� "������〓� 痰����肓"
PRIVATE f_d     && ��瘁│ 甌 痰牀���� ��゛��� "��あ�� ぎ�祠モ�"
PRIVATE f_p     && ��瘁│ 甌 痰牀���� ��゛��� "��あ�� 痰����肓"
PRIVATE n, j    && �腑砒┴�, ┃ぅ�瘠, �����モ琺 罔���
PRIVATE p_ln    && ��┃� 痰����肓 � 痰牀���
PRIVATE fnt     && �爬籵: " " - �°膈覃, "1" - 瓲�硅� (condenced)
PRIVATE n_cp    && ��甄� ���┤
PRIVATE lft     && ���� 甄ア� � 瓱�〓���
PRIVATE ffeed   && ��▲琥キ┘ ぎ�祠キ��: �� �矗�罐皀�讚�� - �牀��� 痰牀�,
                &&      �矗���皀�讚�� - �牀��� 筮爼���
PRIVATE p_drcty && �����←キ┘ �ョ�皋 (�爬�皀�)
PRIVATE k00     && ��鈑 か� ���瓷� ぎ�祠キ��

s_sav = SELECT()

*
* ��爼�珮ガ ��� ぎ�祠キ��
IF TYPE("doc_d") = "D"
  d_year = LEFT(DTOS(doc_d),4)
ENDIF
IF TYPE("doc_d") = "N"
  d_year = STR(doc_d,4)
ENDIF
IF TYPE("doc_d") = "C"
  d_year = doc_d
ENDIF

*
*  �����モ琺 �������� 爛Θ�� �ョ�皋 ぎ�祠キ�� � ��｀腮� �ムガキ�襯.
*
PRIVATE is_u        && ���祠キ� � USD
PRIVATE is_full     && ����覃 (甌���薀��覃) ぎ�祠キ�
PRIVATE is_diff     && ���讓� ��痳�Δキ��
PRIVATE is_corr     && ������キ┘ � �爬ガ���� ��矣
PRIVATE is_narr     && �И┤ - ．� 痰��≧�� � 痺�����  NEW 30.06.1999
PRIVATE w_arr       && ��瘁│ か� 筮爼�牀����� 痺��� �牀��瘡�

PRIVATE sum_0       && �祠�� ．� ���
PRIVATE sum_1       && �祠�� � ���
PRIVATE sum_11      && �祠�� � ��� �� �����き��
PRIVATE sum_12      && �А謔��/�イ�痰��� 痺��� � ���

PRIVATE sum_0_p     && �祠�� ．� ��� �� 痰����罐
PRIVATE sum_1_p     && �祠�� � ��� �� 痰����罐
PRIVATE sum_11_p    && �祠�� � ��� �� �����き�� �� 痰����罐
PRIVATE sum_12_p    && �А謔��/�イ�痰��� 痺��� � ��� �� 痰����罐

PRIVATE sum_0_d     && �祠�� ．� ��� �� ぎ�祠キ矣
PRIVATE sum_1_d     && �祠�� � ��� �� ぎ�祠キ矣
PRIVATE sum_11_d    && �祠�� � ��� �� �����き�� �� ぎ�祠キ矣
PRIVATE sum_12_d    && �А謔��/�イ�痰��� 痺��� � ��� �� ぎ�祠キ矣

PRIVATE flg_w, shab_name, i, flg_valid, ch, a_code

flg_w = IIF(TYPE("flg") = "C", UPPER(flg), "D")

* NEW 30.06.1999
* 蹍▲瑙ガ ぎ�竅皋��痰� 筰���� � flg_w - ���腑� �.

IF EMPTY(flg_w)
  flg_w = "?"
ENDIF

flg_valid="DFS?#"  && ���甌� ぎ�竅皋�諷 Л�腑�┤ 筰����

l_flg=LEN(flg_w)
FOR i = 1 TO l_flg              && scan all char flags in string flg_w
  ch = SUBSTR(flg_w,i,1)
  IF NOT (ch $ flg_valid)       && replace an invalid flag by "?"
    flg_w = CHRTRAN(flg_w, ch, "?")
  ENDIF
ENDFOR

* ���ユ �牀▲爲� 筰����
* END OF NEW 30.06.1999

shab_name = ""   && ��� ��゛��� ぎ�祠キ�� � DOC_LST

is_u    = .F.    && ���祠キ� � USD
is_full = .T.    && ����覃 (甌���薀��覃) ぎ�祠キ�
is_corr = .F.    && ������キ┘ � �爬ガ���� ��矣
is_diff = .F.
is_narr = .F.    && �И┤ - ．� 痰��≧�� � 痺����� NEW 30.06.1999

sum_0   = 0    && �祠�� ．� ���
sum_1   = 0    && �祠�� � ���
sum_11  = 0    && �祠�� � ��� �� �����き��
sum_12  = 0    && �А謔��/�イ�痰��� 痺��� � ���

sum_0_p  = 0   && �祠�� ．� ��� �� 痰����罐
sum_1_p  = 0   && �祠�� � ��� �� 痰����罐
sum_11_p = 0   && �祠�� � ��� �� �����き�� �� 痰����罐
sum_12_p = 0   && �А謔��/�イ�痰��� 痺��� � ��� �� 痰����罐

sum_0_d  = 0   && �祠�� ．� ��� �� ぎ�祠キ矣
sum_1_d  = 0   && �祠�� � ��� �� ぎ�祠キ矣
sum_11_d = 0   && �祠�� � ��� �� �����き�� �� ぎ�祠キ矣
sum_12_d = 0   && �А謔��/�イ�痰��� 痺��� � ��� �� ぎ�祠キ矣


*
* �━〓�讚覃 ���ム ぎ�祠キ��
d_n = ALLTRIM(doc_n)

*
*  ������→� ぎ�祠キ皰�...
SELECT 0
USE (base_path+"ACT_S_T") ORDER TAG DOC_NUM ALIAS TIT_PRINT AGAIN
k00 = firm_c+d_year+doc_n
*
*  �薀�...
IF .NOT. SEEK(k00)  && �� ��茫�...
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�モ ぎ�祠キ�� � ���ム�� "+d_n
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  RETURN
ENDIF
d_d = DTOC(DOC_DATE)
a_code = WHO_ACT

IF .NOT. Set_Flg()  && �痰���→� 皋�� ぎ�祠キ��
  USE
  SELECT (s_sav)
  RETURN
ENDIF

*
*  �薀� ��┘���...
IF .NOT. EMPTY(SUP_CODE)
  SELECT 0
  USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS ACC_NNNN AGAIN
  SEEK TIT_PRINT.SUP_CODE
  c_name = ALLTRIM(CUS_NAME)
  c_code = CUS_CODE
  USE
ENDIF
SELECT TIT_PRINT

*
*   ��ぃ�皰→� ∇ガキ���� �����.
*
*  ��������! 踸 �諷�ぅ ├ ����� ≡� �皖琺硅� � �牀������ �ョ�皋 �����
*            �牀�� ∇ガキ���� � ������→�� ぎ�祠キ皰� ぎ�Ν� °碎 ���琺硅!
*            �爛�キ�覃 ���� �� ��Ε� ━モ� ┃ぅ�甌�!
*
DO Wt_Mess WITH "��ぃ�皰→� 甎�瓷�"
DO Prep_Tmp
DO Wt_Mess

*
*  ｯ�瘠��ガ �瓮△�����
DIMENSION sb(26,2)
sb(26,1) = "{踸�ョ��┘                                                            }"
sb(26,2) = ALLTRIM(TIT_PRINT.NOTE)

sb( 1,1) = "{�矗/�矗}"
sb( 1,2) = ""

sb( 2,1) = "{N ���_��}"
sb( 2,2) = d_n

sb( 3,1) = "{���� ぎ�}"
sb( 3,2) = d_d

sb( 4,1) = "{�┓ �爬絎��}"
sb( 4,2) = IIF(TIT_PRINT.FLG_BUY = "B", "���祚��", "����├�罔�")

sb( 5,1) = "{���鈞�}"
sb( 5,2) = IIF(TIT_PRINT.VALUTA = "$", "� �", "�磧")

j = SELECT()
SELECT 0
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS PRN_FIRM AGAIN
SEEK firm_c
sb(23, 1) = "{�礫�〓え皀��          }"
sb(23, 2) = BOSS
sb(24, 1) = "{��. ＜紕��皀�         }"
sb(24, 2) = ABAK
sb(25, 1) = "{��瘁��                }"
sb(25, 2) = KASSIR

sb( 6,1) = "{踳く爬閧┘                                     }"
sb( 6,2) = L_NAME
USE
SELECT (j)

sb( 7,1) = "{��痰�∫┴                                       }"
sb( 7,2) = c_name

sb( 8,1) = "{N ����. }"
sb( 8,2) = ALLTRIM(TIT_PRINT.EXP_NUM)

sb( 9,1) = "{���� �. }"
sb( 9,2) = DTOC(TIT_PRINT.EXP_DATE)

sb(10,1) = "{�祠�� �/� 0�}"
sb(10,2) = ""

sb(11,1) = "{�祠�� � � 0�}"
sb(11,2) = ""

sb(12,1) = "{�祠�� � � 1�}"
sb(12,2) = ""

sb(13,1) = "{�祠�� � � 2�}"
sb(13,2) = ""

sb(14,1) = "{�祠�� �/� 0�}"
sb(14,2) = TRANSFORM(TIT_PRINT.TOTAL_0, "999 999 999.99")

sb(15,1) = "{�祠�� � � 0�}"
sb(15,2) = TRANSFORM(TIT_PRINT.TOTAL_H, "999 999 999.99")

sb(16,1) = "{�祠�� � � 1�}"
sb(16,2) = ""

sb(17,1) = "{�祠�� � � 2�}"
sb(17,2) = ""

sb(18,1) = "{�祠�� ���   }"
sb(18,2) = TRANSFORM(TIT_PRINT.TOTAL_H-TIT_PRINT.TOTAL_0, "999 999 999.99")

sb(19,1) = "{%��}"
sb(19,2) = TRANSFORM(TIT_PRINT.NDS_, "999.9")

DIMENSION w_arr(2)
sb(20,1) = "{�祠�� �牀��瘡� 1                                                    }"
sb(21,1) = "{�祠�� �牀��瘡� 2                                                    }"
w_arr(1) = LEN(sb(20,1))
w_arr(2) = LEN(sb(21,1))

DO Nt_Lines WITH ABS(TIT_PRINT.TOTAL_H), w_arr, TIT_PRINT.VALUTA
sb(20,2) = w_arr(1)
sb(21,2) = w_arr(2)

sb(22,1) = "{Autor                       }"
SELECT 0
USE (base_path+"PERSONS.DBF") ORDER TAG CODE ALIAS C9903 AGAIN
IF a_code = 0
  sb(22,2) = "���!"
ELSE
  IF SEEK (a_code)
    sb(22,2) = ALLTRIM(FAMILY)+" "+LEFT(NAME,1)+" "+LEFT(S_NAME,1)    
  ELSE
    sb(22,2) = "?!!"
  ENDIF
ENDIF         
sb(22,2) = PADR(sb(22,2),LEN(sb(22,1)))
USE

* �с��ガ ������┘ ぎ�祠キ��
SELECT TIT_PRINT
USE (base_path+"DOC_FORM")
LOCATE FOR shab_name == ALLTRIM(UPPER(DOC_NAME))

p_ln = DOC_FORM.PAGE_LEN   && ��┃� 痰����肓 � 痰牀���
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT   && �爬籵: " " - �°膈覃, "1" - 瓲�硅� (condenced)
n_cp = DOC_FORM.N_COPIES   && ��甄� ���┤
lft  = DOC_FORM.LEFT_FIELD && ���� 甄ア� � 瓱�〓���
ffeed =DOC_FORM.F_FEED     && ��▲琥キ┘ ぎ�祠キ��:
                           &&      �� �矗�罐皀�讚�� - �牀��� 痰牀�,
                           &&      �矗���皀�讚�� - �牀��� 筮爼���
p_drctry = DOC_FORM.P_DIR  && �����←キ┘ �ョ�皋 (�爬�皀�)

*
*  ��爼�珮ガ ��゛��� か� ������→�� � ��あ����
*
n = MEMLINES(PAGE_H)  &&     ������〓� 痰����肓 (�� �ム〓�)
IF n > 0
  DIMENSION t_p(n)
  FOR i = 1 TO n
    t_p(i) = MLINE(PAGE_H,i)
  ENDFOR
ELSE
  DIMENSION t_p(1)
  t_p(1) = ""
ENDIF

n = MEMLINES(DOC_H)  &&     ������〓� �ム〓� 痰����肓
IF n > 0
  DIMENSION t_d(n)
  FOR i = 1 TO n
    t_d(i) = MLINE(DOC_H,i)
  ENDFOR
ELSE
  DIMENSION t_d(ALEN(t_p))
  FOR j = 1 TO ALEN(t_p)
    t_d(j) = t_p(j)
  ENDFOR
ENDIF

n = MEMLINES(PAGE_F)  &&     ��あ�� 痰����肓 (�� ��甄イ�ォ!)
IF n > 0
  DIMENSION f_p(n)
  FOR i = 1 TO n
    f_p(i) = MLINE(PAGE_F,i)
  ENDFOR
ELSE
  DIMENSION f_p(1)
  f_p(1) = ""
ENDIF

n = MEMLINES(DOC_F)  &&     ��あ�� ��甄イ�ォ 痰����肓
IF n > 0
  DIMENSION f_d(n)
  FOR i = 1 TO n
    f_d(i) = MLINE(DOC_F,i)
  ENDFOR
ELSE
  DIMENSION f_d(ALEN(f_p))
  FOR j = 1 TO ALEN(f_p)
    f_d(j) = f_p(j)
  ENDFOR
ENDIF

USE

SELECT TMP_LST

DO Ini_Prn WITH "", p_ln, lft, n_cp, fnt, ffeed, p_drctry

GO TOP

PRIVATE n_p_det  && ��甄� ぅ���讚諷 痰牀� �� 痰����罐
PRIVATE n_pg     && ��甄� 痰����� � ぎ�祠キ皀
PRIVATE f_pg     && �オ竕�� 痰������ ぎ�祠キ�� �爬 �ョ�皋
PRIVATE str_w    && ｡��� �ョ���ガ�� 痰牀��
PRIVATE nn       && ���ム 痰牀�� �� ��瑙お�...

n_p_det = p_ln-MAX(ALEN(f_p),ALEN(f_d))-MAX(ALEN(t_p),ALEN(t_d))
IF n_p_det <= 0
  n_p_det = RECCOUNT()
  n_pg = 1
ELSE
  n_pg = MAX(CEILING(RECCOUNT()/n_p_det),1)
ENDIF

*
*  ������〓� ぎ�祠キ��
sb( 1,2) = "1/"+ALLTRIM(STR(n_pg))
sb( 1,2) = PADL(sb(1,2), LEN(sb(1,1)))
FOR i = 1 TO ALEN(t_d)
  str_w = t_d(i)
  FOR j = 1 TO ALEN(sb,1)
    str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
  ENDFOR
  IF i = 1
    @ PROW(), PCOL() SAY str_w
  ELSE
    @ PROW()+1, 0 SAY str_w
  ENDIF
ENDFOR
nn = 0
FOR f_pg = 1 TO n_pg    && �� 痰�������...

  IF f_pg # 1  && ������〓� 痰����肓 (�� �ム〓�!)
    *  �甄� ━ゥ� �メ皰 �祠ム�罔� 痰�����...
    sb( 1,2) = ALLTRIM(STR(f_pg))+"/"+ALLTRIM(STR(n_pg))
    sb( 1,2) = PADR(sb(1,2), LEN(sb(1,1)))
    EJECT
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
  ENDIF
  sum_0_p  = 0   && �祠�� ．� ��� �� 痰����罐
  sum_1_p  = 0   && �祠�� � ��� �� 痰����罐
  sum_11_p = 0   && �祠�� � ��� �� �����き�� �� 痰����罐
  sum_12_p = 0   && �А謔��/�イ�痰��� 痺��� � ��� �� 痰����罐
  IF RECCOUNT() # 0
    *
    *  �モ��讚襯 痰牀�� �腑爛き�� 痰����肓
    FOR i = 1 TO n_p_det
      nn = nn+1
      sum_0  = SUM_0
      sum_1  = SUM_P_NDS
      sum_11 = SUM_INV
      sum_12 = SUM_DELTA
      * NEW 30.06.1999
      IF is_narr   && �キ跏� 痰��≧�� � ���-〓�
*        @ PROW()+1,0 SAY PREFIX+" "+NAME+" "+PRODUCER+  ;
*            IIF(is_u,STR(PRICE,10,4),STR(PRICE,10,2))+STR(QNT, 7)+ ;
*              STR(QNT_INV,7)+STR(QNT_DELTA,7)+ ;
*              IIF(is_u, "", TRANSFORM(SUM_P_NDS,"9999999.99"))

        @ PROW()+1,0 SAY PREFIX+" "+NAME+" "+PRODUCER+  ;
            +STR(QNT, 7)+STR(QNT_INV,7)+STR(QNT_DELTA,7)

      ELSE
        @ PROW()+1,0 SAY PREFIX+" "+NAME+" "+PRODUCER+  ;
            STR(QNT, 7)+  ;
            IIF(.NOT. is_full, "",  ;
              STR(QNT_INV,7)+ ;
              STR(QNT_DELTA,7))
      ENDIF  && END OF NEW 30.06.1999
      sum_0_p  = sum_0_p+sum_0     && �祠�� ．� ��� �� 痰����罐
      sum_1_p  = sum_1_p+sum_1     && �祠�� � ��� �� 痰����罐
      sum_11_p = sum_11_p+sum_11   && �祠�� � ��� �� �����き�� �� 痰����罐
      sum_12_p = sum_12_p+sum_12   && �А謔��/�イ�痰��� 痺��� � ��� �� 痰����罐
      SKIP
      IF EOF()
        EXIT
      ENDIF
    ENDFOR
  ENDIF
  sb(10,2) = IIF(is_u,TRANSFORM(sum_0_p, "9 999 999.9999"),TRANSFORM(sum_0_p, "999 999 999.99"))
  sb(10,2) = PADL(sb(10,2), LEN(sb(10,1)))
  sb(11,2) = IIF(is_u,TRANSFORM(sum_1_p, "9 999 999.9999"),TRANSFORM(sum_1_p, "999 999 999.99"))
  sb(11,2) = PADL(sb(11,2), LEN(sb(11,1)))
  sb(12,2) = IIF(is_u,TRANSFORM(sum_11_p,"9 999 999.9999"),TRANSFORM(sum_11_p, "999 999 999.99"))
  sb(12,2) = PADL(sb(12,2), LEN(sb(12,1)))
  sb(13,2) = IIF(is_u,TRANSFORM(sum_12_p,"9 999 999.9999"),TRANSFORM(sum_12_p, "999 999 999.99"))
  sb(13,2) = PADL(sb(13,2), LEN(sb(13,1)))
  sum_0_d  = sum_0_d+sum_0_p
  sum_1_d  = sum_1_d+sum_1_p
  sum_11_d = sum_11_d+sum_11_p
  sum_12_d = sum_12_d+sum_12_p
  *
  *  ｪ�����┘ �腑爛き�� 痰����肓 (�� ��甄イ�ォ!)
  *
  IF f_pg # n_pg
    FOR i = 1 TO ALEN(f_p)
      str_w = f_p(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      @ PROW()+1, 0 SAY str_w
    ENDFOR
  ENDIF
ENDFOR
sb(14,2) = IIF(is_u,TRANSFORM(sum_0_p, "9 999 999.9999"),TRANSFORM(sum_0_p, "999 999 999.99"))
sb(14,2) = PADL(sb(14,2), LEN(sb(14,1)))
sb(15,2) = IIF(is_u,TRANSFORM(sum_1_p, "9 999 999.9999"),TRANSFORM(sum_1_p, "999 999 999.99"))
sb(15,2) = PADL(sb(15,2), LEN(sb(15,1)))
sb(16,2) = IIF(is_u,TRANSFORM(sum_11_p, "9 999 999.9999"),TRANSFORM(sum_11_p, "999 999 999.99"))
sb(16,2) = PADL(sb(16,2), LEN(sb(16,1)))
sb(17,2) = IIF(is_u,TRANSFORM(sum_12_p, "9 999 999.9999"),TRANSFORM(sum_12_p, "999 999 999.99"))
sb(17,2) = PADL(sb(17,2), LEN(sb(17,1)))
*
*   ｪ�����┘ ��甄イ�ォ 痰����肓
*
FOR i = 1 TO ALEN(f_d)
  str_w = f_d(i)
  FOR j = 1 TO ALEN(sb,1)
    str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
  ENDFOR
  @ PROW()+1, 0 SAY str_w
ENDFOR
str_w = DBF("TMP_LST")
USE

DO Term_Prn WITH "", str_w, IIF(TYPE("c_code") = "N", c_code, .F.)

DELETE FILE (str_w)
SELECT (s_sav)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Prep_Tmp     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                    ����皰→� 甌ぅ爨━��� ぎ�祠キ��.                    �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 01/19/1999 陳�
PROCEDURE Prep_Tmp

PRIVATE tag_n, tag_exp, pr_w
SELECT 0
DELETE FILE (tmpo_path+"PRINT.TMP")
CREATE DBF (tmpo_path+"PRINT.TMP") ;
   (CODE      N( 7), ;
    PREFIX    C( 4), ;
    NAME      C(35), ;
    PRODUCER  C( 8), ;
    QNT       N( 6), ;
    PRICE     N( 9,4), ;
    SUM_0     N(12,2), ;
    SUM_P_NDS N(12,2), ;
    QNT_INV   N( 6),   ;
    SUM_INV   N(12,2), ;
    QNT_DELTA N( 6),   ;
    SUM_DELTA N(12,2) )

USE (tmpo_path+"PRINT.TMP") ALIAS TMP_LST EXCLUSIVE
INDEX ON NAME+PREFIX+PRODUCER TAG NAME

*
*   ���キ���矣��
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS ST_PRINT AGAIN
*
*   �モ��讚覃 ����
SELECT 0
USE (base_path+"ACT_S") ORDER TAG DOC_NUM ALIAS DET_PRINT AGAIN
SET RELATION TO CODE INTO ST_PRINT

*
*   ｯ爛ぅ�錺� ┃ぅ�甅�� �諤�Ε�┘
*
tag_n = 1
DO WHILE .NOT. TAG() == TAG(tag_n)
  tag_n = tag_n+1
ENDDO
tag_exp = SYS(14,tag_n)


= SEEK(k00)

SCAN REST WHILE EVALUATE(tag_exp) = k00   && �� 痰牀��� ぎ�祠キ��
  IF is_diff .AND. QNT_INC = QNT_EXP
    LOOP
  ENDIF
  IF .NOT. is_full .AND. QNT_INC = 0
    LOOP
  ENDIF

  pr_w = IIF(is_u, PRICE_D, PRICE_R)
  SELECT TMP_LST
  APPEND BLANK
  REPLACE CODE      WITH DET_PRINT.CODE,    ;
          PREFIX    WITH ST_PRINT.PREFIX,   ;
          NAME      WITH ST_PRINT.NAME,     ;
          PRODUCER  WITH ST_PRINT.PRODUCER, ;
          QNT       WITH DET_PRINT.QNT_INC, ;
          PRICE     WITH IIF(is_u,DET_PRINT.PRICE_D,DET_PRINT.PRICE_0), ;
          SUM_0     WITH IIF(is_u,DET_PRINT.PRICE_D*DET_PRINT.QNT_INC,DET_PRINT.PRICE_0*DET_PRINT.QNT_INC), ;
          SUM_P_NDS WITH ROUND(pr_w*DET_PRINT.QNT_INC,2), ;
          QNT_INV   WITH DET_PRINT.QNT_EXP, ;
          SUM_INV   WITH ROUND(pr_w*DET_PRINT.QNT_EXP,2), ;
          QNT_DELTA WITH DET_PRINT.QNT_INC-DET_PRINT.QNT_EXP
  REPLACE SUM_DELTA WITH SUM_P_NDS-SUM_INV
  SELECT DET_PRINT
ENDSCAN
USE

SELECT ST_PRINT
USE

SELECT TMP_LST
COPY TO (tmpo_path+"PRINT_T")
USE
DELETE FILE (tmpo_path+"PRINT.TMP")
USE (tmpo_path+"PRINT_T") ALIAS TMP_LST EXCLUSIVE

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Set_Flg      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      �皰膈錺� Л�腑��� 筰�Κ��...                      �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 27.01.1999 陳�
PROCEDURE Set_Flg

is_u = TIT_PRINT.VALUTA = "$"
is_corr = "!" $ TIT_PRINT.DOC_NUM
DO CASE
CASE is_corr
  is_full = .F.
  shab_name = IIF(is_u, "ACT_SH!$", "ACT_SH!")
  RETURN .T.
CASE "D" $ flg_w          && CORR 30.06.1999
  is_full = .NOT. EMPTY(TIT_PRINT.EXP_NUM)
  DO Set_Type
  shab_name = "ACT_"+IIF(is_full, "LN", "SH")+IIF(is_u, "$", "")
  RETURN .T.
CASE "F" $ flg_w          && CORR 30.06.1999
  is_full = .T.
  DO Set_Type
  shab_name = "ACT_"+IIF(is_full, "LN", "SH")+IIF(is_u, "$", "")
  RETURN .T.
CASE "S" $ flg_w .OR. "?" $ flg_w       && CORR 30.06.1999
  is_full = .F.
  DO Set_Type
  shab_name = "ACT_"+IIF(is_full, "LN", "SH")+IIF(is_u, "$", "")
  RETURN .T.
CASE "#" $ flg_w           && NEW 30.06.1999
  is_narr = .T.
  is_full = .F.
  DO Set_Type
  shab_name = "ACT_N"
  RETURN .T.               && END OF NEW 30.06.1999
ENDCASE

*is_full = .F.
*shab_name = "ACT_SH"+IIF(is_u, "$", "")

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Set_Type     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                       �痰���→� 皋�� ぎ�祠キ��.                        �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 26/02/1999 陳�
PROCEDURE Set_Type

IF EMPTY(TIT_PRINT.EXP_NUM)
  RETURN
ENDIF

*
*  �ムガキ�襯 甌痰�鎰�� か� ��※��罔�
*

PRIVATE stat_type     && �┓ ��※��罔�: 0 - �メ�����珥�覃;
                                        1 - ゛���;
                                        2 - BROWSE - ��゛���;
                                        3 - BROWSE - 甎�甌�.
PRIVATE what_do       && ��� 爛Θ��.
PRIVATE menu_name     && ��� �瓱�縲������ �キ�.
PRIVATE last_mouse    && �爛�� ��甄イ�ィ� ����皋� Бム┃�� ������.
PRIVATE win_name      && ��� ���� ( ���� か� BROWSE ).

*
*   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ��ぅ爨�皀�讚�� ��痰� �牀������:
*

*敖陳陳陳陳陳陳陳陳陳陳陳陳朕
*�                          �
*�  [ ] ���讓� ��痳�Δキ��  �
*�                          �
*�          < �� >          �
*青陳陳陳陳陳陳陳陳陳陳陳陳潰
PRIVATE ex
ex = 1

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 7, 30, "�┓ ぎ�祠キ��"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 3,  4 GET is_diff PICTURE "@*C ���讓� ��痳�Δキ��"
@ 5, 12 GET ex PICTURE "@*T \ OK "
READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN