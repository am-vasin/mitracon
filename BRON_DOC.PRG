*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Bron_Doc     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                        ���甌� ��÷���牀�������.                        �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 07/03/2000 様�
PROCEDURE Bron_Doc

DO Prep_Tmp
DO Prn_Doc
SELECT BR_TMP0
USE
DELETE FILE (tmpo_path+"BR_TMP0.DBF")
DELETE FILE (tmpo_path+"BR_TMP0.CDX")
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Prep_Tmp     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      ��ぃ�皰→� ∇ガキ���� �����                       �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 07/03/2000 陳�
PROCEDURE Prep_Tmp

PRIVATE tmpNum, tmpCust, tmpSal
DELETE FILE (tmpo_path+"BR_TMP0.DBF")
DELETE FILE (tmpo_path+"BR_TMP0.CDX")

CREATE DBF (tmpo_path+"BR_TMP0") ;
  (  CODE     N( 7), ;
     PREFIX   C( 4), ;
     NAME     C(35), ;
     PRODUCER C( 8), ;
     QNT_HOLE N( 6), ;
     REST     N( 6), ;
     SALESMAN C(24), ;
     ORD_DATE D,     ;
     QNT_ORD  N( 6), ;
     LIST_NUM C( 6), ;
     CUSTOMER C(50)   )

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS STO_0703

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS CUS_0703

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS WHO_0703

SELECT 0
USE (base_path+"ORD_T") ORDER TAG DOC_NUM ALIAS ORDT_0703
SET RELATION TO WHO INTO WHO_0703

SELECT 0
USE (base_path+"ORD_D") ORDER TAG DOC_NUM ALIAS ORDD_0703
SET RELATION TO LIST_NUM+STR(ORD_NUM,3) INTO ORDT_0703
SET RELATION TO CODE INTO STO_0703 ADDITIVE

SELECT 0
USE (base_path+"LIST_TIT") ALIAS LST_0703
SET RELATION TO CLI_CODE INTO CUS_0703

SCAN FOR EMPTY(SAL_NUM)
  SELECT ORDD_0703
  tmpNum = STR(LST_0703.LST_NUM,6)
  SEEK tmpNum
  IF FOUND("CUS_0703")
    tmpCust = CUS_0703.CUS_NAME
  ELSE
    tmpCust = LST_0703.LST_NAME
  ENDIF
  SCAN REST WHILE LIST_NUM = tmpNum
    IF FOUND("WHO_0703")
      tmpSal = WHO_0703.FAMILY
    ELSE
      tmpSal = ""
    ENDIF
    SELECT BR_TMP0
    APPEND BLANK
    REPLACE CODE     WITH ORDD_0703.CODE,    ;
            PREFIX   WITH STO_0703.PREFIX,   ;
            NAME     WITH STO_0703.NAME,     ;
            PRODUCER WITH STO_0703.PRODUCER, ;
            REST     WITH STO_0703.QNT,      ;
            SALESMAN WITH tmpSal,            ;
            ORD_DATE WITH ORDT_0703.ORD_DATE,;
            QNT_ORD  WITH ORDD_0703.QNT,     ;
            LIST_NUM WITH tmpNum,            ;
            CUSTOMER WITH tmpCust
    SELECT ORDD_0703
  ENDSCAN
  SELECT LST_0703
ENDSCAN

SELECT STO_0703
USE

SELECT CUS_0703
USE

SELECT WHO_0703
USE

SELECT ORDT_0703
USE

SELECT ORDD_0703
USE

SELECT LST_0703
USE

SELECT BR_TMP0
INDEX ON NAME+PREFIX+PRODUCER+STR(CODE) TAG NAME

tmpSal = -1
tmpCus = 0
SCAN
  IF CODE # tmpSal
    tmpSal = CODE
    tmpCus = 0
  ENDIF
  tmpCus = tmpCus+QNT_ORD
  REPLACE QNT_HOLE WITH tmpCus
ENDSCAN

tmpSal = -1
tmpCus = 0
SET ORDER TO TAG NAME DESCENDING
SCAN
  IF CODE # tmpSal
    tmpSal = CODE
    tmpCus = QNT_HOLE
  ENDIF
  REPLACE QNT_HOLE WITH tmpCus
ENDSCAN
SET ORDER TO TAG NAME ASCENDING

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Prn_Doc      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                            �ョ�碎 ぎ�祠キ��                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 03.07.2000 陳�
PROCEDURE Prn_Doc

PRIVATE sb

DIMENSION sb(3, 2)

sb( 1,1) = "{��/�砌  }"
sb( 1,2) = ""

sb( 2,1) = "{����    }"
sb( 2,2) = DTOC(DATE())

sb( 3,1) = "{�爛�� }"
sb( 3,2) = TIME()

* �с��ガ ������┘ ぎ�祠キ��
SELECT 0
USE (base_path+"DOC_FORM")
LOCATE FOR "BRON_DOC" == ALLTRIM(UPPER(DOC_NAME))

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

SELECT BR_TMP0

DO Ini_Prn WITH "", p_ln, lft, n_cp, fnt, ffeed, p_drctry

GO TOP

PRIVATE n_p_det  && ��甄� ぅ���讚諷 痰牀� �� 痰����罐
PRIVATE n_pg     && ��甄� 痰����� � ぎ�祠キ皀
PRIVATE f_pg     && �オ竕�� 痰������ ぎ�祠キ�� �爬 �ョ�皋
PRIVATE str_w    && ｡��� �ョ���ガ�� 痰牀��
PRIVATE sum_p0, sum_p1 && ...�祠�� �� 痰�������
PRIVATE sum_d0, sum_d1 && ...�祠�� �� ぎ�祠キ矣
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

STORE 0 TO sum_d0, sum_d1, nn
PRIVATE tmpCode
tmpCode = -1

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
  STORE 0 TO sum_p0, sum_p1
  IF RECCOUNT() # 0
    *
    *  �モ��讚襯 痰牀�� �腑爛き�� 痰����肓
    FOR i = 1 TO n_p_det
      IF tmpCode # CODE
        str_w = PREFIX+" "+NAME+" "+PRODUCER+STR(REST,7)+STR(QNT_HOLE,7)+" "
        tmpCode = CODE
      ELSE
        str_w = SPACE(54)
      ENDIF
      @ PROW()+1,0 SAY str_w+STR(QNT_ORD,6)+" "+LEFT(SALESMAN,15)+" "+  ;
                             LIST_NUM+" "+DTOC(ORD_DATE)+" "+LEFT(CUSTOMER,34)
      SKIP
      IF EOF()
        EXIT
      ENDIF
    ENDFOR
  ENDIF
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
COPY TO (tmpo_path+"TMP000.DBF")

DO Term_Prn WITH "", tmpo_path+"TMP000.DBF", .F.

DELETE FILE (tmpo_path+"TMP000.DBF")

RETURN
