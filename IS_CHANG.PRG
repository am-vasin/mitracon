*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Is_Chang      ��о�｀砒┴ ��むォ ��瓱�                     �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                  蹍痰�←キ┘ �爬Л���� ����腮� ���キ.                 �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 09/24/1999 様�
PROCEDURE Is_Chang

PRIVATE s_sav, r_part, is_c, is_r

s_sav = SELECT()
SELECT 0
is_c = .F.
IF File_O(base_path+"ANALOG.DBF")
  is_c = .T.
  USE (base_path+"ANALOG") ORDER TAG CODE ALIAS AN_9924 AGAIN
ENDIF
select 0
is_r = .F.
IF File_O(path_comm+"SUBJECT.DBF")
  is_r = .T.
  USE (path_comm+"SUBJECT") ORDER TAG STO_CODE AGAIN ALIAS RT_9924
ENDIF
IF .NOT. is_r .AND. .NOT. is_c
  SELECT (s_sav)
  RETURN
ENDIF
PRIVATE cnt, mrk, r_part
SELECT 0
USE (base_path+"STOCK") ALIAS ST_9924 AGAIN
SET RELATION TO CODE INTO AN_9924, CODE INTO RT_9924
cnt = 0
DO Wt_Mess WITH "蹍痰�←錺� ���モ��..."

SCAN
  mrk = " "
  cnt = cnt+1
  @ WROW()-1, 0 SAY PADC(ALLTRIM(STR(cnt)), WCOLS())
  IF is_c .AND. FOUND("AN_9924")
    mrk = "�"
  ENDIF
  IF EMPTY(mrk) .AND. is_r .AND. FOUND("RT_9924")
    SELECT RT_9924
    r_part = ROOT
    SET ORDER TO TAG ROOT
    SET FILTER TO STO_CODE # ST_9924.CODE .AND. STO_CODE # 0  && �皰° �� 礫���碎 �� 瓮￥
    IF .NOT. EMPTY(r_part)    && � °� �� ��爛��!?
      IF SEEK(r_part)
        mrk = "�"
      ENDIF
    ENDIF
    SET ORDER TO STO_CODE
    SET FILTER TO
    SELECT ST_9924
  ENDIF
  REPLACE IS_CHANGE WITH mrk
ENDSCAN

DO Wt_Mess

USE
IF is_r
  SELECT RT_9924
  USE
ENDIF
IF is_c
  SELECT AN_9924
  USE
ENDIF

SELECT (s_sav)

RETURN
