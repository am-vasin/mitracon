*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� View_Dem     ��о�｀砒┴ ��むォ ��瓱�           20.07.97 12:13:10 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                     ��゛��� 甎牀�� �� �牀��∽��.                       �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE View_Dem

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

*           ���Θ皀 �ム┏�
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�   ���. ���� ��.��.����    ���. ���� ��.��.����  �
*�                                                 �
*�        < OK Ctrl-W > < �皖���碎瘴 Esc >         �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex, d1, d2   &&
                     &&   ｡褊←錺� � ������錺� ���� ゛����
ex = 1             &&
d1 = DATE()
d2 = DATE()
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 6, 53, "���Θ皀 �ム┏�"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 2,  5 SAY "���. ����" GET d1 PICTURE "@D"
@ 2, 29 SAY "���. ����" GET d2 PICTURE "@D"
@ 4, 10 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Tst_Dat()

IF ex = 1
  DO Prep_Doc
ENDIF

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                  蹍▲爲� ��玻オ皚�痰� ┃皀燿��� ���.                  �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 20.07.97 12:19:37 夕
PROCEDURE Tst_Dat
PRIVATE mss

IF ex = 2 .OR. MOD(READKEY(),256) = 12
  RETURN .T.
ENDIF

IF d1 > DATE() .OR. d2 > DATE()
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�ィ�き� ラ� 皰�讓� "+TRANSFORM(DATE(),"@D")
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF d1 > d2
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�����讚�� ���� ｀�跏� ���ョ���? ��� �� °��モ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                   蹍������ 筮爼�牀����� ぎ�祠キ��.                    �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 20.07.97 12:23:58 夕
PROCEDURE Prep_Doc
PRIVATE n, mss, us_sav, del_sav, q_h, q_a, q_f

DELETE FILE (tmpo_path+"VIEW_DEM.DBF")
DELETE FILE (tmpo_path+"VIEW_DEM.CDX")

*
*  ��Г�ガ � ������錺� ∇ガキ�覃 ���� - ����皰→�.
*
CREATE DBF (tmpo_path+"VIEW_DEM")  ;
   (  USER_C N( 4),  ;
      USER_N C(24),  ;
      Q_HOLE N( 5),  ;
      Q_MOD  N(15),  ;
      Q_FUNC N( 5)  )
USE (tmpo_path+"VIEW_DEM") EXCLUSIVE
del_sav = SET("DELETED") = "OFF"
SET DELETED ON
SELECT 0
USE (base_path+"DEMAND") ORDER TAG WHO
SET FILTER TO BETWEEN(DATE, d1, d2)
STORE 0 TO q_h, q_a, q_f
DO Wt_Mess WITH "蹍甃�矗│�ガ 甎牀�..."
us_sav = 10000000
SCAN
  IF us_sav # WHO
    IF us_sav # 10000000
      SELECT VIEW_DEM
      APPEND BLANK
      REPLACE USER_C WITH us_sav, ;
              Q_HOLE WITH q_h,    ;
              Q_MOD  WITH q_a,    ;
              Q_FUNC WITH q_f
      STORE 0 TO q_h, q_a, q_f
      SELECT DEMAND
    ENDIF
    us_sav = WHO
  ENDIF
  q_h = q_h+1
  IF .NOT. EMPTY(FUNCTION)
    q_f = q_f+1
  ENDIF
  IF .NOT. EMPTY(AMODEL)
    q_a = q_a+1
  ENDIF
ENDSCAN
IF us_sav # 10000000
  SELECT VIEW_DEM
  APPEND BLANK
  REPLACE USER_C WITH us_sav, ;
          Q_HOLE WITH q_h,    ;
          Q_MOD  WITH q_a,    ;
          Q_FUNC WITH q_f
  STORE 0 TO q_h, q_a, q_f
  SELECT DEMAND
ENDIF

IF del_sav
  SET DELETED OFF
ENDIF

SELECT DEMAND
USE (base_path+"PERSONS") ORDER TAG CODE

SELECT VIEW_DEM
SET RELATION TO USER_C INTO PERSONS
REPLACE ALL FOR FOUND("PERSONS") USER_N WITH PERSONS.FAMILY

DO Wt_Mess

IF RECCOUNT() = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� 礫�����覃 �ム┏� 甎牀� �� 甌；���瘴!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  DELETE FILE (tmpo_path+"VIEW_DEM.DBF")
  DELETE FILE (tmpo_path+"VIEW_DEM.CDX")
  RETURN
ENDIF

INDEX ON USER_N TAG USER_N

SELECT 0

PRIVATE sb
DIMENSION sb(5,2)

sb( 1,1) = "{Date_1}"
sb( 1,2) = TRANSFORM(d1, "@D")

sb( 2,1) = "{Date_2}"
sb( 2,2) = TRANSFORM(d2, "@D")

sb( 3,1) = "{Q_H }"
sb( 3,2) = ""

sb( 4,1) = "{Q_A }"
sb( 4,2) = ""

sb( 5,1) = "{Q_F }"
sb( 5,2) = ""

n = ALEN(sb,1)

PRIVATE doc_tit, p_ln, fnt, n_cp, lft, ffeed, t_d, f_d, i, j, str_w, p_drctry
PRIVATE sqh, sqf, sqa

SELECT 0
USE DOC_FORM
doc_tit = "��゛��� 甎牀��"

LOCATE FOR "VIEW_DEM" == ALLTRIM(UPPER(DOC_NAME))
p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed =DOC_FORM.F_FEED
p_drctry = DOC_FORM.P_DIR

n = MEMLINES(DOC_H)
DIMENSION t_d(n)
FOR i = 1 TO n
  t_d(i) = MLINE(DOC_H,i)
ENDFOR

n = MEMLINES(DOC_F)
DIMENSION f_d(n)
FOR i = 1 TO n
  f_d(i) = MLINE(DOC_F,i)
ENDFOR
USE

DO Ini_Prn WITH doc_tit, p_ln, lft, n_cp, fnt, ffeed, p_drctry

STORE 0 TO sqh, sqa, sqf

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

SELECT VIEW_DEM
SCAN
  str_w = USER_N+TRANSFORM(Q_HOLE,"@Z 9999999")+ ;
                 TRANSFORM(Q_MOD, "@Z 9999999")+ ;
                 TRANSFORM(Q_FUNC,"@Z 9999999")
  @ PROW()+1,0 SAY str_w
  sqh = sqh+Q_HOLE
  sqf = sqf+Q_MOD
  sqa = sqa+Q_FUNC
ENDSCAN
sb(3,2) = STR(sqh,6)
sb(4,2) = STR(sqa,6)
sb(5,2) = STR(sqf,6)
FOR i = 1 TO ALEN(f_d)
  str_w = f_d(i)
  FOR j = 1 TO ALEN(sb,1)
    str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
  ENDFOR
  @ PROW()+1, 0 SAY str_w
ENDFOR

SELECT VIEW_DEM
USE

DO Term_Prn WITH "", tmpo_path+"VIEW_DEM.DBF"
CLOSE DATABASES

DELETE FILE (tmpo_path+"VIEW_DEM.DBF")
DELETE FILE (tmpo_path+"VIEW_DEM.CDX")

RETURN
