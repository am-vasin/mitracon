*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� BigOrd       ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�           ��爼�牀���┘ ����� � 痺����� �� �爬絎き覓 �爐ム��.           �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 20.09.2000 様�
PROCEDURE BigOrd
PARAMETERS pFirm, pDoc, pDate

PRIVATE s000

IF .NOT. USED("BIGORD")
  SELECT 0
  DELETE FILE (tmpo_path+"BIGORD.DBF")
  CREATE DBF (tmpo_path+"BIGORD") ;
   ( DOC_REF    N(10),  ;
     DOC_DATE   D,      ;
     DOC_SUM    N(15,2),;
     DAY_SUM    N(15,2),;
     CUS_CODE   N( 8)   )
  SELECT 0
  USE (base_path+"INCMONEY") AGAIN ORDER TAG DOC_NUM ALIAS BIGDOC
  SELECT 0
  USE (base_path+"INCMONEY") AGAIN ORDER TAG CUS_CODE ALIAS CUS_SUM
ENDIF
SELECT BIGDOC
SEEK pFirm+" "+LEFT(DTOS(pDate),4)+pDoc
s000 = Cus_Sum(BIGDOC.CUS_CODE, pDate, pFirm)
SELECT BIGORD
APPEND BLANK
REPLACE DOC_REF  WITH RECNO("BIGDOC"),  ;
        DOC_DATE WITH pDate,            ;
        DOC_SUM  WITH BIGDOC.MONEY_R,   ;
        CUS_CODE WITH BIGDOC.CUS_CODE,  ;
        DAY_SUM  WITH s000
RETURN
