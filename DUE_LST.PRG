*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Due_Lst      ��о�｀砒┴ ��むォ ��瓱�           04.03.98 18:20:54 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                        ��ぃ�皰→� 甎�瓷� ぎ���.                        �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Due_Lst
PARAMETERS full_name  && ������ ━� ����� 爛с�赳��� ．� ��瘉�爛���
PRIVATE sel_sav

sel_sav = SELECT()

SELECT 0
IF File_O(full_name)
  DELETE FILE (full_name)
  DELETE FILE (full_name+".CDX")
ENDIF

CREATE DBF (full_name) ;
  ( CODE N( 7),     ;
    QNT  N( 6),     ;
    PRICE_R N(8,2) )
USE (full_name) ALIAS DUE_LST EXCLUSIVE
INDEX ON CODE TAG CODE

SELECT 0
USE (base_path+"LIST_TIT") ORDER TAG LST_NUM ALIAS LT_000 AGAIN

SELECT 0
USE (base_path+"LIST_DET") ORDER TAG DUE_CODE ALIAS LD_000 AGAIN
SET RELATION TO VAL(ALLTRIM(LIST_NUM)) INTO LT_000

SCAN FOR EMPTY(LT_000.SAL_NUM)
  SELECT DUE_LST
  IF .NOT. SEEK(LD_000.CODE)
    APPEND BLANK
  ENDIF
  REPLACE CODE    WITH LD_000.CODE, ;
          QNT     WITH QNT+LD_000.QNT_DUE, ;
          PRICE_R WITH MAX(PRICE_R, LD_000.SAL_PRICE)
  SELECT LD_000
ENDSCAN

USE

SELECT LT_000
USE

SELECT DUE_LST
DELETE TAG CODE
USE

SELECT (sel_sav)

RETURN
