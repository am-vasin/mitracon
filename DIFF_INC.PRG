*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�              蹍������ �ョ�皋 ��痳�Δキ┤ �爬ガ��.                    �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Diff_Inc

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

*                      �������皀 ゛���
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�                                         敖陳陳陳陳陳陳陳跳 �
*� ���. ���� ���瓷� ��.��.��     ��痰�∫┴ �                � �
*�                                         塒様様様様様様様夕 �
*� ���� � �����き覓� 旭旭碓旭異旭旭碓旭異旭旭碓旭異旭旭碓旭異 �
*�                                                            �
*�              [ ] �ョ���碎 皰�讓� ��痳�Δキ��               �
*�                                                            �
*�              < OK Ctrl-W > < �皖���碎瘴 Esc >              �
*�                                                            �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
PRIVATE ex, cus_c, names, codes, dt0, f_name, prnt_sw
USE (base_path+"CUSTOMER")
ex = 0
SCAN FOR .NOT. EMPTY(SUP)
  ex = ex+1
  DIMENSION names(ex), codes(ex)
  names(ex) = B_NAME
  codes(ex) = CODE
ENDSCAN

USE
IF ex = 0
  DIMENSION names(3)
  names(1) = ""
  names(2) = CHR(0)+"�モ �� �き��� ��痰�∫┴�!"
  names(3) = ""
  DO Out_Mess WITH 7, "names"
  RETURN
ENDIF
cus_c   = 1
dt0     = {}
f_name  = ""
DO Get_Path WITH f_name, "DIFF_INC"
f_name  = PADR(f_name, 40)
prnt_sw = .F.
ex      = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 12, 64, "�������皀 ゛���"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 3, 33 SAY "��痰�∫┴"
@ 3,  3 SAY "���. ���� ���瓷�" GET dt0 PICTURE "@D"
@ 2, 43 GET cus_c PICTURE "@^" FROM names SIZE 3, 18
@ 5,  3 SAY "���� � �����き覓�" GET f_name
@ 7, 16 GET prnt_sw PICTURE "@*C �ョ���碎 皰�讓� ��痳�Δキ��"
@ 9, 16 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Tst_Blank()
RELEASE WINDOW (win_name)
*--------------------------------------------------------------------------
IF ex = 1
  cus_c = codes(cus_c)
  DO Fill_Dbf  && ������錺� ∇ガキ�竡 ��с ����諷
  DO P_Diff    && �ョ�碎 ����� ��Й�腮�
ENDIF

CLOSE DATABASES
*DELETE FILE (tmpo_path+"Diff_Inc.dbf")
*DELETE FILE (tmpo_path+"Diff_Inc.cdx")

POP KEY
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�             蹍������ �牀▲爲� ��玻オ皚�痰� ���ォ ゛����.             �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Tst_Blank
PRIVATE s, mss, f_w

IF READKEY() % 256 = 12 .OR. ex = 2
  RETURN .T.
ENDIF

f_w = ALLTRIM(f_name)
IF .NOT. "." $ f_w
  f_w = f_w+".DBF"
ENDIF

IF .NOT. File_O(ALLTRIM(f_w))
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�モ ����� � �����き覓�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

f_name = f_w

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�           蹍������ ������キ�� ∇ガキ��� ��щ ��痳�Δキ┤.            �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Fill_Dbf
PRIVATE pr_nams, pr_cods, i, n, n_1

DELETE FILE (tmpo_path+"Diff_Inc.dbf")
DELETE FILE (tmpo_path+"Diff_Inc.cdx")

SET EXCLUSIVE ON
CREATE DBF (tmpo_path+"Diff_Inc")  ;
  ( BOX_NO   C(10), ;
    ACT_NO   C(10), ;
    ACT_DT   D,     ;
    CODE     N( 7), ;
    PREFIX   C( 4), ;
    NAME     C(35), ;
    PRODUCER C( 8), ;
    PRO_CODE N( 5), ;
    MRK      C( 1), ;
    QNT_INV  N( 6), ;
    QNT_ACT  N( 6)  )

USE (tmpo_path+"Diff_Inc")
INDEX ON BOX_NO+STR(CODE,7) TAG CODE
INDEX ON BOX_NO+NAME+PREFIX+PRODUCER TAG NAME ADDITIVE

SET EXCLUSIVE OFF

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE

SELECT 0
USE (base_path+"DEVELOP") ORDER TAG 

SELECT 0
USE (f_name) ALIAS SRS
SET RELATION TO STO_CODE INTO STOCK, PRODUCER INTO DEVELOP

SCAN
  SELECT DIFF_INC
  APPEND BLANK
  REPLACE BOX_NO   WITH STR(SRS.BOXNO,10),  ;
          CODE     WITH SRS.STO_CODE, ;
          PREFIX   WITH SRS.PREFIX, ;
          NAME     WITH SRS.NAME, ;
          PRODUCER WITH SRS.PRODUCER, ;
          PRO_CODE WITH IIF(FOUND("STOCK"), STOCK.PRO_CODE, DEVELOP.CODE), ;
          QNT_INV  WITH SRS.QTY
  SELECT SRS
ENDSCAN
USE

SELECT DEVELOP

USE (base_path+"ACT_S") ORDER TAG DOC_NUM
SET RELATION TO CODE INTO STOCK

SELECT 0
USE (base_path+"ACT_S_T")

n = "**********"
SELECT DIFF_INC
SET ORDER TO TAG CODE
PRIVATE a_r_sav, key_w
SCAN
  IF .NOT. ALLTRIM(n) == ALLTRIM(BOX_NO)   && ��ュ��� �� ��≪� ��牀！�!
    n_1 = ALLTRIM(BOX_NO)
    SELECT ACT_S_T
    LOCATE FOR ALLTRIM(EXP_NUM) == n_1 .AND.   ;
               DOC_DATE > dt0 .AND. cus_c = SUP_CODE
    n = DIFF_INC.BOX_NO
    DO WHILE FOUND()
      DO Add_Act
      SELECT ACT_S_T
      a_r_sav = RECNO()
      key_w = FIRM+DTOS(DOC_DATE)+DOC_NUM
      SCAN REST FOR "!" $ DOC_NUM .AND. key_w = FIRM+DTOS(EXP_DATE)+EXP_NUM  && ��玻オ皋珮鉗┘ ��硅
        DO Add_Act
        SELECT ACT_S_T
      ENDSCAN
      SELECT ACT_S_T
      GO a_r_sav
      SKIP
      LOCATE REST FOR ALLTRIM(EXP_NUM) == n_1 .AND.   ;
               DOC_DATE > dt0 .AND. cus_c = SUP_CODE
    ENDDO
    SELECT DIFF_INC
  ENDIF
ENDSCAN

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�               蹍������ ぎ��←キ�� �爬ガ���� ����.                    �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Add_Act
PRIVATE rc, i

rc = RECNO("DIFF_INC")
SELECT ACT_S
SEEK ACT_S_T.FIRM+LEFT(DTOS(ACT_S_T.DOC_DATE),4)+ACT_S_T.DOC_NUM
SCAN REST WHILE FIRM = ACT_S_T.FIRM .AND. DOC_NUM = ACT_S_T.DOC_NUM .AND. DOC_DATE = ACT_S_T.DOC_DATE
  *
  *   �薀� �� ��ゃ...
  *
  SELECT DIFF_INC
  SET ORDER TO TAG CODE
  i = " "
  IF .NOT. SEEK(n+STR(ACT_S.CODE,7))
    SET ORDER TO TAG NAME
    IF SEEK(n+STOCK.NAME+STOCK.PREFIX+PRODUCER)
      i = "!"
    ENDIF
  ENDIF
  IF .NOT. FOUND() .OR. ;
           STOCK.NAME # DIFF_INC.NAME .OR. ;
           STOCK.PREFIX # DIFF_INC.PREFIX .OR. ;
           STOCK.PRODUCER # DIFF_INC.PRODUCER
    APPEND BLANK
    REPLACE BOX_NO WITH n
  ENDIF
  REPLACE ACT_NO   WITH ACT_S_T.DOC_NUM,  ;
          ACT_DT   WITH ACT_S_T.DOC_DATE, ;
          CODE     WITH ACT_S.CODE,       ;
          PREFIX   WITH STOCK.PREFIX,     ;
          NAME     WITH STOCK.NAME,       ;
          PRODUCER WITH STOCK.PRODUCER,   ;
          PRO_CODE WITH STOCK.PRO_CODE,   ;
          MRK      WITH IIF(ACT_S.DOC_DATE = STOCK.DATE_ON, "�", i), ;
          QNT_ACT  WITH QNT_ACT+ACT_S.QNT_INC
  SELECT ACT_S
ENDSCAN

SELECT DIFF_INC
GO rc

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                    蹍������ �ョ�皋 ぎ�祠キ��.                        �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE P_Diff

PRIVATE p_ln, fnt, n_cp, lft, sb, n, t_p, f_p, f_d, i, pic, v_sw, p_drctry
PRIVATE n_p_det, n_pg, f_pg, sp1, sp2, sp3, sp4, sd1, sd2, sd3, sd4
PRIVATE mss, ffeed

DIMENSION sb(1,2)
sb( 1,1) = "{Page}"
sb( 1,2) = ""

n = ALEN(sb,1)
FOR i = 1 TO n
  sb(i,2) =PADR(sb(i,2), LEN(sb(i,1)))
ENDFOR

SELECT 0
USE (base_path+"DOC_FORM")
LOCATE FOR "DIFF_INC" = ALLTRIM(UPPER(DOC_NAME))
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

USE

SELECT DIFF_INC
SET ORDER TO TAG NAME

DO Ini_Prn WITH "�イ���痰� ��痳�Δキ┤", p_ln, lft, n_cp, fnt, ffeed, p_drctry

GO TOP
PRIVATE pg_wk, doc_wk, f_lin
f_pg  = 1
doc_wk = "**********"
pg_wk  = p_ln-ALEN(t_p)-4
f_lin  = pg_wk+10
DO WHILE .NOT. EOF()
  IF f_lin >= pg_wk
    IF f_pg # 1
      EJECT
    ENDIF
    sb( 1,2) = ALLTRIM(STR(f_pg))
    sb( 1,2) = PADL(sb(1,2), LEN(sb(1,1)))
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
    f_lin = 0
    f_pg = f_pg+1
  ENDIF
  IF doc_wk # BOX_NO
    doc_wk = BOX_NO
    str_w = "********** ��牀！� N "+ALLTRIM(BOX_NO)+" *****************************"
    @ PROW()+1, 0 SAY str_w
    f_lin = f_lin+1
    LOOP
  ENDIF
  IF prnt_sw .AND. QNT_INV = QNT_ACT
    SKIP
    LOOP
  ENDIF
  str_w = PREFIX+" "+NAME+" "+PRODUCER+" "+MRK+" "+ ;
          TRANSFORM(QNT_INV, "@Z 999999")+" "+TRANSFORM(QNT_ACT, "@Z 999999")+  ;
          IIF(QNT_INV = QNT_ACT, "  ", " �")+IIF(EMPTY(ACT_NO), "    �モ",ACT_NO)
  @ PROW()+1, 0 SAY str_w
  f_lin = f_lin+1
  SKIP

ENDDO
USE

DO Term_Prn WITH "", tmpo_path+"Diff_Inc.dbf"

RETURN
