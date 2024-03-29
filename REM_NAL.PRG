*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Rem_nal      ��о�｀砒┴ ��むォ ��瓱�           14.01.98 14:55:04 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                         �ムメ����→� ����腮�.                          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Rem_Nal
PARAMETER vis

PRIVATE cnt, d_s, err, ab, sch, q_s_all, q_s_fil, q_s_rep, q_i_all, ;
        q_i_rep, q_e_fil, q_b, q_f, is_data, q0, q0_r, log_rec, err_cnt

USE (base_path+"PARMS")
d_s = DAT_START

DELETE FILE TST_NAL.DBF
DELETE FILE TST_NAL.CDX

CREATE DBF TST_NAL ;
  (  CODE  N( 7),  ;
     NEW_N N( 6),  ;
     NEW_F N( 6),  ;
     NEW_R N( 6)   )
INDEX ON CODE TAG CODE

SELECT 0
USE (base_path+"SALE") ORDER TAG CODE
SET FILTER TO DOC_DATE >= d_s .AND. LEFT(DOC_NUM,1) = sys_char

SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG DOC_NUM
SET FILTER TO LEFT(DOC_NUM,1) = sys_char .AND. DOC_DATE >= d_s

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"ACT_S") ORDER TAG CODE

SELECT 0
USE (base_path+"RYN_M") ORDER TAG CODE
SET FILTER TO DOC_DATE >= d_s

SELECT 0
USE (base_path+"STOCK")
cnt = 0
SCAN
  cnt= cnt+1
  @ 10, 10 SAY STR(cnt)
  SELECT TST_NAL
  APPEND BLANK
  REPLACE CODE  WITH STOCK.CODE,  ;
          NEW_N WITH STOCK.QNT+STOCK.QNT_SAVE,  ;
          NEW_F WITH STOCK.QNT_REAL,  ;
          NEW_R WITH STOCK.QNT_REP
  SELECT STOCK
ENDSCAN

USE
SELECT TST_NAL
PRIVATE w1, w2, w3

cnt = 0
SCAN
  cnt = cnt+1
  @ 10, 10 SAY STR(cnt)
  STORE 0 TO w1, w2, w3

  DO Cnt_Sale     && 蹍��Θ ( �牀痰� � 筥����� )
  DO Cnt_Inc      && 踸絎�
  DO Cnt_Fil      && �※Ε�┘ �� 筥�����

  SELECT TST_NAL
  REPLACE NEW_N WITH NEW_N+w1,  ;
          NEW_F WITH NEW_F+w2,  ;
          NEW_R WITH NEW_R+w3

ENDSCAN

CLOSE DATABASES

USE TST_NAL ORDER TAG CODE
SELECT 0
USE (base_path+"STOCK")
SET RELATION TO CODE INTO TST_NAL
SET FILTER TO FOUND("TST_NAL")

REPLACE ALL QNT_START WITH TST_NAL.NEW_N, ;
            QNT_S_RYN WITH TST_NAL.NEW_F, ;
            QNT_S_REP WITH TST_NAL.NEW_R

CLOSE DATABASES

CLEAR

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                          ��め腑� �牀���.                              �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Cnt_Sale

SELECT SALE
SEEK TST_NAL.CODE
SCAN REST WHILE CODE = TST_NAL.CODE
  w1 = w1+QNT     && ｡薑� �牀��Θ
  w3 = w3+QNT_REP && 蹍��Θ ��Ж罔�, ▼閧諷 �� 爛��├�罔�
  IF SEEK(LEFT(DTOS(DOC_DATE),4)+DOC_NUM,"SALE_TIT")
    IF EMPTY(SALE_TIT.LIST_NUM)
      w2 = w2+QNT    && 蹍��Θ 筥�����
      w1 = w1-QNT    && 蹍痰� �牀��Θ �皖�硅��ガ!
    ENDIF
  ENDIF
ENDSCAN
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                  ��め腑� ��痰祚�キ�� 皰����.                          �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Cnt_Inc

SELECT ACT_S
SEEK TST_NAL.CODE
SCAN REST WHILE CODE = TST_NAL.CODE
  IF DOC_DATE >= d_s
    w1 = w1-QNT_INC    && ｡薑� �爬絎�
    w3 = w3-QNT_REP    && 踸絎� ��Ж罔�, ▼閧諷 �� 爛��├�罔�
  ENDIF
ENDSCAN
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                  ��め腑� �皴竅�� � 筥����.                            �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Cnt_Fil

SELECT Ryn_M
SEEK TST_NAL.CODE
SCAN REST WHILE CODE = TST_NAL.CODE
  w2 = w2-QNT
  w1 = w1+QNT
ENDSCAN
RETURN
