*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� View_Sal     ��о�｀砒┴ ��むォ ��瓱�           20.07.97 12:13:10 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                   ��゛��� �牀��� �� ��｀腮� �メ���.                    �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE View_Sal

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
PRIVATE n, mss

DELETE FILE (tmpo_path+"VIEW_SAL.DBF")
DELETE FILE (tmpo_path+"VIEW_SAL.CDX")

*
*  ��Г�ガ � ������錺� ∇ガキ�覃 ���� - ����皰→�.
*
CREATE DBF (tmpo_path+"VIEW_SAL")  ;
   (  ARM_C  C( 3),  ;
      ARM_N  C(16),  ;
      Q_HOLE N( 5),  ;
      S_HOLE N(15,2),;
      Q_CH   N( 5),  ;
      S_CH   N(15,2) )
USE (tmpo_path+"VIEW_SAL") EXCLUSIVE
INDEX ON ARM_C TAG ARM_C
SELECT 0
USE (base_path+"ARM_LST")
SCAN
  SELECT VIEW_SAL
  APPEND BLANK
  REPLACE ARM_C WITH ARM_LST.ARM_C, ;
          ARM_N WITH ARM_LST.ARM_N
  SELECT ARM_LST
ENDSCAN

USE (base_path+"SALE_TIT") ORDER TAG HRON
SET RELATION TO WHERE INTO VIEW_SAL

IF .NOT. SEEK(DTOS(d1))
  n = RECNO(0)
  IF n < 1 .OR. n > RECCOUNT()
    GO BOTTOM
  ELSE
    GO n
  ENDIF
ENDIF

SCAN REST WHILE DOC_DATE <= d2
  n = SUBSTR(DOC_NUM,2, 3) = "***"
  SELECT VIEW_SAL
  REPLACE Q_HOLE WITH Q_HOLE+1,  ;
          S_HOLE WITH S_HOLE+SALE_TIT.TOTAL_R,  ;
          Q_CH   WITH Q_CH+IIF(n, 1, 0),  ;
          S_CH   WITH S_CH+IIF(n, SALE_TIT.TOTAL_R, 0)
  SELECT SALE_TIT
ENDSCAN

USE
SELECT VIEW_SAL
DELETE ALL FOR Q_HOLE = 0
PACK
GO TOP
IF BOF() .OR. EOF()
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� 礫�����覃 �ム┏� �モ �� �き�� �牀��Θ."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  DELETE FILE (tmpo_path+"VIEW_SAL.DBF")
  DELETE FILE (tmpo_path+"VIEW_SAL.CDX")
  RETURN
ENDIF

SELECT 0

PRIVATE sb
DIMENSION sb(6,2)

sb( 1,1) = "{Date_1}"
sb( 1,2) = TRANSFORM(d1, "@D")

sb( 2,1) = "{Date_2}"
sb( 2,2) = TRANSFORM(d2, "@D")

sb( 3,1) = "{Q_H}"
sb( 3,2) = ""

sb( 4,1) = "{SUM_H        }"
sb( 4,2) = ""

sb( 5,1) = "{Q_C}"
sb( 5,2) = ""

sb( 6,1) = "{SUM_C        }"
sb( 6,2) = ""

n = ALEN(sb,1)

PRIVATE doc_tit, p_ln, fnt, n_cp, lft, ffeed, t_d, f_d, i, j, str_w, p_drctry
PRIVATE sq0, sm0, sq1, sm1

SELECT 0
USE (base_path+"DOC_FORM")
doc_tit = "��゛��� �牀���"

LOCATE FOR "VIEW_SAL" == ALLTRIM(UPPER(DOC_NAME))
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

STORE 0 TO sq0, sm0, sq1, sm1

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

SELECT VIEW_SAL
SCAN
  str_w = ARM_N+TRANSFORM(Q_HOLE,"@Z 999999")+ ;
                TRANSFORM(S_HOLE,"@Z 9999999999999.99")+ ;
                TRANSFORM(Q_CH,"@Z 999999")+ ;
                TRANSFORM(S_CH,"@Z 9999999999999.99")
  @ PROW()+1,0 SAY str_w
  sq0 = sq0+Q_HOLE
  sm0 = sm0+S_HOLE
  sq1 = sq1+Q_CH
  sm1 = sm1+S_CH
ENDSCAN
sb(3,2) = STR(sq0,5)
sb(4,2) = STR(sm0,15,2)
sb(5,2) = STR(sq1,5)
sb(6,2) = STR(sm1,15,2)
FOR i = 1 TO ALEN(f_d)
  str_w = f_d(i)
  FOR j = 1 TO ALEN(sb,1)
    str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
  ENDFOR
  @ PROW()+1, 0 SAY str_w
ENDFOR

SELECT VIEW_SAL
USE

DO Term_Prn WITH "", tmpo_path+"VIEW_SAL.DBF"
CLOSE DATABASES

DELETE FILE (tmpo_path+"VIEW_SAL.DBF")
DELETE FILE (tmpo_path+"VIEW_SAL.CDX")

RETURN
