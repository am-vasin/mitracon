*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Acc_Rest     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                         �痰�皖� �� ぅ��Ж���.                          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 05/03/2000 様�
PROCEDURE Acc_Rest

PRIVATE f_code, sign_v, f_name, sq, sq_name

f_code = ""
f_name = ""
sign_v = 1
sq = ""
sq_name = ""

IF .NOT. Get_P()
  RETURN
ENDIF

DO Prep_Tmp
DO Prn_Doc

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Get_P        ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                  ��皀���皋↓�� ������キ┘ �����モ牀�.                  �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 05/03/2000 陳�
PROCEDURE Get_P

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� 敖 ���腑��� ぅ��Ж皰� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕 �
*� � ( ) �ク竅硅�  ( ) �矗���皀�讚襯  ( ) ����Θ皀�讚襯 � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�                         敖陳陳陳陳陳陳陳朕             �
*�             踳く爬閧┘ �....:....!....:.�             �
*�                         青陳陳陳陳陳陳陳潰             �
*�                    敖陳陳陳陳陳陳陳陳陳陳陳朕          �
*�           ç��お� �....:....!....:....!....�          �
*�                    青陳陳陳陳陳陳陳陳陳陳陳潰          �
*�            < OK Ctrl-W > < �皖���碎瘴 Esc >            �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, s_sav, n_list, c_list, n, point, point1, sq_c, sq_n
ex = 1
point = 1
point1 = 1
s_sav = SELECT()
SELECT 0
USE (base_path+"FIRMS") ORDER TAG B_NAME ALIAS F0503 AGAIN
DIMENSION n_list(1), c_list(1)
n = 1
n_list(1) = PADR("�瓮 �爛く爬閧��", FSIZE("B_NAME"))
c_list(1) = ""
SCAN
  n = n+1
  DIMENSION n_list(n), c_list(n)
  n_list(n) = B_NAME
  c_list(n) = FIRM_CODE
ENDSCAN
USE (base_path+"PLACES") ALIAS F0503 AGAIN
DIMENSION sq_c(1), sq_n(1)
n = 1
sq_n(1) = PADR("�瓮 �����お�", FSIZE("P_NAME"))
sq_c(1) = ""
SCAN
  n = n+1
  DIMENSION sq_c(n), sq_n(n)
  sq_n(n) = P_NAME
  sq_c(n) = P_CHAR
ENDSCAN
USE
SELECT (s_sav)

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 13, 60, "�������皀 ��゛�聶"
DO Sun_Bord WITH  2,  3,  4, 56, " ���腑��� ぅ��Ж皰� "
@ 6, 15 SAY "踳く爬閧┘"
@ 9, 13 SAY "ç��お�"
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 3,  5 GET sign_v PICTURE "@*RH �ク竅硅� ;�矗���皀�讚襯 ;����Θ皀�讚襯"
@ 5, 27 GET point PICTURE "@^" FROM n_list SIZE 1, 18
@ 8, 22 GET point1 PICTURE "@^" FROM sq_n SIZE 1, 26

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE
f_code = c_list(point)
f_name = n_list(point)
sq     = sq_c(point1)
sq_name= sq_n(point1)

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)

RETURN ex = 1

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Prep_Tmp     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      ��ぃ�皰→� ∇ガキ���� �����.                      �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 03.05.2000 陳�
PROCEDURE Prep_Tmp

PRIVATE s_sav, sum_r, sum_d

s_sav = SELECT()

SELECT 0
DELETE FILE (tmpo_path+"ACC_REST.DBF")
CREATE DBF (tmpo_path+"ACC_REST") ;
  ( CUS_CODE N( 8), ;
    CUS_NAME C(50), ;
    REST_R   N(15,2), ;
    REST_D   N(15,4) )
SELECT 0
USE (base_path+"CUS_REST") ORDER TAG CUS_CODE ALIAS RST_0503 AGAIN
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_NAME ALIAS CUS_0503 AGAIN
SET RELATION TO STR(CUS_CODE)+f_code INTO RST_0503
DO Wt_Mess WITH "��ぃ�皰→� ����諷!"
SCAN FOR CUS_CODE > 0
  IF .NOT. EMPTY(sq)
    IF SUBSYS # sq
      LOOP
    ENDIF
  ENDIF
  IF .NOT. FOUND("RST_0503")
    LOOP
  ENDIF
  IF EMPTY(f_code)
    sum_r = 0
    sum_d = 0
    SELECT RST_0503
    SCAN REST WHILE CUS_CODE = CUS_0503.CUS_CODE
      sum_r = sum_r+RST_0503.ACC_SUM+RST_0503.ACC_S_H
      sum_d = sum_d+RST_0503.ACC_SUMD+RST_0503.ACC_SD_H
    ENDSCAN
    SELECT CUS_0503
  ELSE
    sum_r = RST_0503.ACC_SUM+RST_0503.ACC_S_H
    sum_d = RST_0503.ACC_SUMD+RST_0503.ACC_SD_H
  ENDIF
  IF sum_r = 0 .AND. sum_d = 0
    LOOP
  ENDIF
  IF sign_v = 2 .AND. sum_r >= 0 .AND. sum_d >= 0
    LOOP
  ENDIF
  IF sign_v = 3 .AND. sum_r <= 0 .AND. sum_d <= 0
    LOOP
  ENDIF
  SELECT ACC_REST
  APPEND BLANK
  REPLACE CUS_CODE WITH CUS_0503.CUS_CODE, ;
          CUS_NAME WITH CUS_0503.CUS_NAME, ;
          REST_R   WITH sum_r, ;
          REST_D   WITH sum_d
  SELECT CUS_0503
ENDSCAN
USE
SELECT RST_0503
USE
SELECT (s_sav)
DO Wt_Mess

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Prn_Doc      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      ��＝癶キ�� �ョ�碎 ぎ�祠キ��.                      �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 03.05.2000 陳�
PROCEDURE Prn_Doc
PRIVATE p_ln, fnt, n_cp, lft, n, i, j
PRIVATE t_p, f_p, f_d, f_d, d_year, k00, sw, ffeed, p_drctry, a_code
PRIVATE sb, n_p_det, n_pg, f_pg, s_pg, str_w, nds, nn
PRIVATE s_sum_dp, s_sum_dn, s_sum_rp, s_sum_rn 

sel_rem = SELECT()

s_sum_dp = 0
s_sum_dn = 0
s_sum_rp = 0
s_sum_rn = 0

SELECT ACC_REST
SCAN
  IF REST_R < 0
   s_sum_rn = s_sum_rn + REST_R
 ELSE
   s_sum_rp = s_sum_rp + REST_R
 ENDIF
 IF REST_D < 0
    s_sum_dn = s_sum_dn + REST_D
  ELSE
    s_sum_dp = s_sum_dp + REST_D
  ENDIF
ENDSCAN

DIMENSION sb(10,2)
sb( 1,1) = "{�矗/�矗}"
sb( 1,2) = ""

sb( 2,1) = "{踳く爬閧┘   }"
sb( 2,2) = f_name

sb( 3,1) = "{ç��お�              }"
sb( 3,2) = sq_name

sb( 4,1) = "{�甄�※� }"
sb( 4,2) = "�ク竅硅�     "
IF sign_v = 2
  sb(4,2) = "�矗���皀�讚襯"
ENDIF
IF sign_v = 3
  sb(4,2) = "����Θ皀�讚襯"
ENDIF

sb( 5,1) = "{����    }"
sb( 5,2) = DTOC(DATE())

sb( 6,1) = "{�爛�� }"
sb( 6,2) = TIME()

sb( 7,1) = "{SUM_DP      }"
sb( 7,2) = STR(s_sum_dp,11,4)

sb( 8,1) = "{SUM_DN      }"
sb( 8,2) = STR(s_sum_dn,11,4)

sb( 9,1) = "{SUM_RP      }"
sb( 9,2) = STR(s_sum_rp,11,2)

sb(10,1) = "{SUM_RN      }"
sb(10,2) = STR(s_sum_rn,11,2)

SELECT 0
USE (base_path+"DOC_FORM")
LOCATE FOR "ACC_REST" == ALLTRIM(UPPER(DOC_NAME))
p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed =DOC_FORM.F_FEED
p_drctry = DOC_FORM.P_DIR

n = MEMLINES(PAGE_H)
IF n = 0
  DIMENSION t_p(1)
  t_p(1) = ""
ELSE
  DIMENSION t_p(n)
  FOR i = 1 TO n
    t_p(i) = MLINE(PAGE_H,i)
  ENDFOR
ENDIF

n = MEMLINES(PAGE_F)
IF n = 0
  DIMENSION f_p(1)
  f_p(1) = ""
ELSE
  DIMENSION f_p(n)
  FOR i = 1 TO n
    f_p(i) = MLINE(PAGE_F,i)
  ENDFOR
ENDIF

n = MEMLINES(DOC_H)
IF n = 0
  DIMENSION t_d(1)
  t_d(1) = ""
ELSE
  DIMENSION t_d(n)
  FOR i = 1 TO n
    t_d(i) = MLINE(DOC_H,i)
  ENDFOR
ENDIF

n = MEMLINES(DOC_F)
IF n = 0
  DIMENSION f_d(1)
  f_d(1) = ""
ELSE
  DIMENSION f_d(n)
  FOR i = 1 TO n
    f_d(i) = MLINE(DOC_F,i)
  ENDFOR
ENDIF

USE

SELECT ACC_REST

DO Ini_Prn WITH "�痰. �� ぅ��Ж���", p_ln, lft, n_cp, fnt, ffeed, p_drctry

GO TOP
nn = 0

n_p_det = FLOOR(p_ln-MAX(ALEN(t_p),ALEN(t_d))-MAX(ALEN(f_p),ALEN(f_d)))
IF n_p_det <= 0
  n_p_det = RECCOUNT()
  n_pg = 1
ELSE
  n_pg = CEILING(RECCOUNT()/n_p_det)
ENDIF

FOR f_pg = 1 TO n_pg
  sb( 1,2) = ALLTRIM(STR(f_pg))+"/"+ALLTRIM(STR(n_pg))
  sb( 1,2) =PADR(sb(1,2), LEN(sb(1,1)))
  IF f_pg # 1
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
  ELSE
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
  ENDIF
  FOR i = 1 TO n_p_det
    nn = nn+1
    @ PROW()+1,0 SAY CUS_NAME+STR(REST_R,16,2)+STR(REST_D,16,4)
    SKIP
    IF EOF()
      EXIT
    ENDIF
  ENDFOR
  IF f_pg # n_pg
    FOR i = 1 TO ALEN(f_p)
      str_w = f_p(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      @ PROW()+1, 0 SAY str_w
    ENDFOR
  ELSE
    FOR i = 1 TO ALEN(f_d)
      str_w = f_d(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      @ PROW()+1, 0 SAY str_w
    ENDFOR
  ENDIF
ENDFOR
USE

DO Term_Prn WITH "", tmpo_path+"ACC_REST.DBF"

DELETE FILE (tmpo_path+"ACC_REST.DBF")
SELECT (sel_rem)
RETURN
