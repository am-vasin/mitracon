*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�           蹍������ ��ぃ�皰→� ����� ├�キキ��� ����腮�.              �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Delta_Q
PARAMETERS date_s, date_e, is_win
*
*  date_s - �����讚�� ����;
*  date_e - ���ョ��� ����;
*  is_win - ����腮� ┃え��罔�.
*

PRIVATE wd, sc0, clc

USE STOCK ALIAS ST_DELTA AGAIN

SELECT 0
DELETE FILE (tmpo_path+"DELTA_Q.DBF")
DELETE FILE (tmpo_path+"DELTA_Q.CDX")
CREATE DBF (tmpo_path+"DELTA_Q.DBF")  ;
     (   CODE   N(FSIZE("CODE", "ST_DELTA")), ;
         INC_Q  N(8),    ;
         INC_QR N(8),    ;
         EXP_Q  N(8),    ;
         EXP_QR N(8),    ;
         EXP_QF N(8),    ;
         MOV_F  N(8)    )
USE (tmpo_path+"DELTA_Q") EXCLUSIVE
INDEX ON CODE TAG CODE

sc0      = 0
clc      = 0
wd = FSIZE("NAME","ST_DELTA")+FSIZE("PREFIX","ST_DELTA")+21
IF is_win
  DO D_W WITH "Delta_Q", 6, wd, "", 0
  wd = wd-20
  @ 1, 1 SAY "敖陳陳陳賃陳陳陳�"+REPLICATE("�",wd)+"�"
  @ 2, 1 SAY "� �爛��  � ���.N�"+PADC("��Ж罔�     ",wd)+"�"
  @ 3, 1 SAY "団陳陳陳津陳陳陳�"+REPLICATE("�",wd)+"�"
  @ 4, 1 SAY "�        �      �"+SPACE(wd)+"�"
  @ 5, 1 SAY "青陳陳陳珍陳陳陳�"+REPLICATE("�",wd)+"�"
ENDIF

DO Clc_Sale

DO Clc_Inc




IF is_win
  RELEASE WINDOW Delta_Q
ENDIF

SELECT ST_DELTA
USE
SELECT DELTA_Q
USE
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                     蹍������ ��瘍モ� ��痳���.                        �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Clc_Sale
PRIVATE w, w1, w2, w3

SELECT 0
USE SALE_TIT ORDER TAG DOC_NUM ALIAS TIT_DELTA AGAIN

SELECT 0
USE SALE     ORDER TAG CODE    ALIAS DET_DELTA AGAIN
SET RELATION TO LEFT(DTOS(DOC_DATE),4)+DOC_NUM INTO TIT_DELTA

SELECT ST_DELTA

clc = 1
sc0 = SECONDS()
IF is_win
  @ 0, 0 SAY PADC("��������: 蹍��Θ",WCOLS()) COLOR SCHEME 5
ENDIF

SCAN
  IF is_win
    clc = clc+1
    w = SECOND()-sc0
    w1 = FLOOR(w/3600)
    w2 = FLOOR(w/60)
    w3 = w%60
    w  = RIGHT(STR(100+w1,3),2)+"."+RIGHT(STR(100+w2,3),2)+"."+RIGHT(STR(100+w3,3),2)
    @ 4, 2 SAY w
    @ 4,11 SAY STR(clc,6)
    @ 4,18 SAY PREFIX+" "+NAME
  ENDIF
  IF .NOT. SEEK(CODE, "DET_DELTA")
    LOOP
  ENDIF
  STORE 0 TO w1, w2, w3
  SELECT DET_DELTA
  SCAN REST WHILE CODE = ST_DELTA.CODE FOR BETWEEN(DOC_DATE,date_s,date_e)
    IF TIT_DELTA.CUS_TYPE = 4 .AND. EMPTY(TIT_DELTA.LIST_NUM)
      w2 = w2+QNT
    ELSE
      w1 = w1+QNT
    ENDIF
    w3 = w3+QNT_REP
  ENDSCAN
  SELECT DELTA_Q
  IF .NOT. SEEK(ST_DELTA.CODE)
    APPEND BLANK
  ENDIF
  REPLACE CODE   WITH ST_DELTA.CODE, ;
          EXP_Q  WITH w1,    ;
          EXP_QR WITH w3,    ;
          EXP_QF WITH w2
  SELECT ST_DELTA

ENDSCAN

SELECT TIT_DELTA
USE

SELECT DET_DELTA
USE

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                     蹍������ ��瘍モ� �爬絎��.                        �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Clc_Inc
PRIVATE w, w1, w2, w3

SELECT 0
USE ACT_S     ORDER TAG CODE    ALIAS INC_DELTA AGAIN

SELECT ST_DELTA

clc = 1
sc0 = SECONDS()
IF is_win
  @ 0, 0 SAY PADC("��������: 踸絎�",WCOLS()) COLOR SCHEME 5
ENDIF

SCAN
  IF is_win
    clc = clc+1
    w = SECOND()-sc0
    w1 = FLOOR(w/3600)
    w2 = FLOOR(w/60)
    w3 = w%60
    w  = RIGHT(STR(100+w1,3),2)+"."+RIGHT(STR(100+w2,3),2)+"."+RIGHT(STR(100+w3,3),2)
    @ 4, 2 SAY w
    @ 4,11 SAY STR(clc,6)
    @ 4,18 SAY PREFIX+" "+NAME
  ENDIF
  IF .NOT. SEEK(CODE, "INC_DELTA")
    LOOP
  ENDIF
  STORE 0 TO w1, w2
  SELECT INC_DELTA
  SCAN REST WHILE CODE = ST_DELTA.CODE FOR BETWEEN(DOC_DATE,date_s,date_e)
    w1 = w1+QNT
    w2 = w2+QNT_REP
  ENDSCAN
  SELECT DELTA_Q
  IF .NOT. SEEK(ST_DELTA.CODE)
    APPEND BLANK
  ENDIF
  REPLACE CODE   WITH ST_DELTA.CODE, ;
          INC_Q  WITH w1,    ;
          INC_QR WITH w2
  SELECT ST_DELTA

ENDSCAN

SELECT TIT_DELTA
USE

SELECT DET_DELTA
USE

RETURN
