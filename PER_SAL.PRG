*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Per_Sal      ��о�｀砒┴ ��むォ ��瓱�           15.01.98 15:53:12 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                   ����→� �� ��牆��皀 (甌矗磴�┴�).                    �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Per_Sal
PRIVATE d0, d1

SET DATE GERMAN
d1 = DATE()
d0 = CTOD("01"+SUBSTR(DTOC(d1),3))

IF .NOT. G_Period()
  RETURN
ENDIF

DO Prep_Tmp

DO Prn_Doc

CLOSE DATABASES

DELETE FILE (tmpo_path+"SLR_TMP.DBF")
DELETE FILE (tmpo_path+"SLR_TMP.DBF")
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�             蹍������ ┃皀���皋↓��� □��� ┃皀燿��� ���.              �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 15.01.98 15:58:48 夕
PROCEDURE G_Period

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
*�                                                        �
*�   �����讚�� ���� ��.��.����  ���ョ��� ���� ��.��.����  �
*�                                                        �
*�                                                        �
*�            < OK Ctrl-W > < �皖���碎瘴 Esc >            �
*�                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex
ex   = 1             &&
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 9, 60, "�����皀 ┃皀燿�� ���"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 3,  5 SAY "�����讚�� ����" GET d0 PICTURE "@D" VALID Tst_1(d0)
@ 3, 32 SAY "���ョ��� ����" GET d1 PICTURE "@D" VALID Tst_1(d1)
@ 5, 14 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Tst_1()

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                    蹍▲爲� ��玻オ皚�痰� ┃皀燿���.                    �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 15.01.98 16:05:37 夕
PROCEDURE Tst_1
PARAMETER dt
PRIVATE mss

IF ex = 2 .OR. READKEY() % 256 = 12
  RETURN .T.
ENDIF
IF TYPE("dt") = "D"
  IF dt > DATE()
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"���┼礬皀, "+DTOC(dt)+" ラ� � �� ��痰祚┼�!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
ELSE
  IF d0 > d1
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"�����讚�� ���� ｀�跏� ���ョ��? �皰-皰 Гメ� �� ���..."
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
ENDIF

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                      ��ぃ�皰→� ∇ガキ���� �����.                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 15.01.98 16:23:20 夕
PROCEDURE Prep_Tmp
PRIVATE rst

USE (base_path+"SLR_TYPE") ORDER TAG CODE
SELECT 0
USE (base_path+"SLR_MOVE")
SET RELATION TO CODE INTO SLR_TYPE
SELECT 0

DELETE FILE (tmpo_path+"SLR_TMP.DBF")
DELETE FILE (tmpo_path+"SLR_TMP.DBF")
CREATE DBF (tmpo_path+"SLR_TMP.DBF") ;
  ( PS_CODE N( 5), ;
    D_DATE  D,     ;
    D_TIME  C( 8), ;
    D_NUM   C( 7), ;
    OPER_N  C(24), ;
    QNT     N(5,2),;
    UNIT    C(12), ;
    PRICE   N(8,2),;
    TOTAL   N(8,2),;
    R_PART  N(8,2),;
    COURCE  N(8,3),;
    YEAR    N( 4), ;
    MONTH   N( 2), ;
    NOTE    C(24), ;
    REST    N(8,2)  )

INDEX ON STR(PS_CODE,5)+DTOS(D_DATE)+D_TIME TAG ORD

SELECT SLR_MOVE
SCAN
  SELECT SLR_TMP
  APPEND BLANK
  REPLACE PS_CODE WITH SLR_MOVE.PS_CODE,  ;
          D_DATE  WITH SLR_MOVE.DOC_DATE, ;
          D_TIME  WITH SLR_MOVE.DOC_TIME, ;
          D_NUM   WITH SLR_MOVE.DOC_NUM,  ;
          OPER_N  WITH SLR_TYPE.NAME,     ;
          QNT     WITH SLR_MOVE.QNT,      ;
          UNIT    WITH SLR_MOVE.UNIT,     ;
          YEAR    WITH SLR_MOVE.YEAR,     ;
          MONTH   WITH SLR_MOVE.MONTH,    ;
          NOTE    WITH SLR_MOVE.NOTE,     ;
          PRICE   WITH Crpt_2(SLR_MOVE.PRICE), ;
          R_PART  WITH Crpt_2(SLR_MOVE.R_PART),;
          COURCE  WITH Crpt_2(SLR_MOVE.COURCE),;
          TOTAL   WITH Crpt_2(SLR_MOVE.TOTAL)
  SELECT SLR_MOVE
ENDSCAN

USE
SELECT SLR_TYPE

USE (base_path+"PS_STAT")

SCAN FOR SEEK(STR(CODE,5), "SLR_TMP")
  rst = Crpt_2(PS_STAT.START)
  SELECT SLR_TMP
  SCAN REST WHILE PS_CODE = PS_STAT.CODE
    rst = rst+TOTAL
    IF BETWEEN(D_DATE, d0, d1)
      REPLACE REST WITH rst
    ELSE
      DELETE
    ENDIF
  ENDSCAN
  SELECT PS_STAT
ENDSCAN

USE

RETURN
*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                           �ョ�碎 ぎ�祠キ��.                            �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 15.01.98 17:39:40 夕
PROCEDURE Prn_Doc
PRIVATE p_ln, sb, fnt, n_cp, lft, ffeed, n, i, l_flt, str_w, t_p, p_drctry
PRIVATE distr

DIMENSION sb(3,2)

sb( 1,1) = "{Client  }"
sb( 1,2) = ""

sb( 2,1) = "{S_Date}"
sb( 2,2) = DTOC(d0)

sb( 3,1) = "{E_Date}"
sb( 3,2) = DTOC(d1)

SELECT 0
USE DOC_FORM
LOCATE FOR "SLR_REP" = ALLTRIM(UPPER(DOC_NAME))
p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed =DOC_FORM.F_FEED
p_drctry = DOC_FORM.P_DIR

n = MEMLINES(PAGE_H)
DIMENSION t_p(n)
FOR i = 1 TO n
  t_p(i) = MLINE(PAGE_H,i)
ENDFOR
USE

USE (base_path+"PERSONS") ORDER TAG DEP_NAME
SET RELATION TO STR(CODE,5) INTO SLR_TMP

DO Ini_Prn WITH "��牆����", p_ln, lft, n_cp, fnt, ffeed, p_drctry
l_flt = 0
SCAN FOR FOUND("SLR_TMP")
  sb( 1,2) = ALLTRIM(FAMILY)+" "+ALLTRIM(NAME)+" "+ALLTRIM(S_NAME)
  FOR i = 1 TO ALEN(t_p)
    str_w = t_p(i)
    FOR j = 1 TO ALEN(sb,1)
      str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
    ENDFOR
    IF l_flt = 0
      @ PROW(), PCOL() SAY str_w
    ELSE
      @ PROW()+1, 0 SAY str_w
    ENDIF
*    IF p_ln # 0
*      IF l_flt % p_ln = 0 .AND. l_flt # 0
*        EJECT
*      ENDIF
*    ENDIF
    l_flt = l_flt+1
  ENDFOR
  SELECT SLR_TMP
  SCAN REST WHILE PS_CODE = PERSONS.CODE
    IF EMPTY(R_PART)
      distr = ""
    ELSE
      distr = " ("+ALLTRIM(STR(ABS(TOTAL)-R_PART,10,2))+","+ ;
                   ALLTRIM(STR(R_PART,10,2))+"*"+ ;
                   ALLTRIM(STR(COURCE,8,3))+"="+  ;
                   ALLTRIM(STR(ROUND(R_PART*COURCE,2),8,3))+")"
    ENDIF
    str_w = DTOC(D_DATE)+" "+D_NUM+" "+OPER_N+" "+  ;
            TRANSFORM(QNT,"@Z 999.99")+" "+UNIT+" "+   ;
            TRANSFORM(PRICE,"@Z 99999.99")+" "+     ;
            TRANSFORM(TOTAL,"@Z 99999.99")+" "+     ;
            TRANSFORM(REST,"99999.99")+distr
*    IF p_ln # 0
*      IF l_flt % p_ln = 0 .AND. l_flt # 0
*        EJECT
*      ENDIF
*    ENDIF
    l_flt = l_flt+1
    @ PROW()+1, 0 SAY str_w
    str_w = IIF(.NOT. EMPTY(YEAR), "         �� "+STR(MONTH,2)+"."+STR(YEAR,4), ;
                                   "                   ")
    str_w = str_w+IIF(.NOT. EMPTY(NOTE), "        踸�ョ��┘ "+NOTE, "")
    IF .NOT. EMPTY(str_w)
      l_flt = l_flt+1
      @ PROW()+1, 0 SAY str_w
    ENDIF
  ENDSCAN
  SELECT PERSONS
ENDSCAN
USE

DO Term_Prn WITH ""

RETURN
