*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Pers_Slr     ��о�｀砒┴ ��むォ ��瓱�           08.11.97 17:37:44 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                   ����→�: ��牆���� �� 甌矗磴�┴��.                    �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Pers_Slr

PRIVATE date_1, date_2

STORE {} TO date_1, date_2
IF .NOT. Get_Int()
  RETURN
ENDIF
DO Prep_Tmp



*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                     ��ぃ�皰→� ∇ガキ����� �����.                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 08.11.97 17:39:04 夕
PROCEDURE Prep_Tmp
PRIVATE w

SET EXCLUSIVE ON
DELETE FILE (tmpo_path+"TMP_SLR1.DBF")
DELETE FILE (tmpo_path+"TMP_SLR1.CDX")
CREATE DBF (tmpo_path+"TMP_SLR1.DBF") ;
  (PS_CODE N( 5),  ;
   PS_NAME C(62),  ;
   SUM_ST  N(12,2),;
   SUM_EN  N(12,2) )
INDEX ON PS_CODE TAG PS_CODE
SET EXCLUSIVE OFF

SELECT 0
USE (base_path+"PS_STAT") ORDER TAG CODE
SELECT 0
USE (base_path+"PERSONS") ORDER TAG NAME
SET RELATION TO CODE INTO PS_STAT
SCAN
  w = ROUND(Crpt_2(PS_STAT.START),2)
  SELECT TMP_SLR1
  APPEND BLANK
  REPLACE PS_CODE WITH PERSONS.CODE,  ;
          PS_NAME WITH ALLTRIM(PERSONS.FAMILY)+" "+  ;
                       ALLTRIM(PERSONS.NAME)+" "+    ;
                       ALLTRIM(PERSONS.S_NAME),  ;
          SUM_ST  WITH w,   ;
          SUM_EN  WITH w
  SELECT PERSONS
ENDSCAN
USE
SELECT PS_STAT
USE

SELECT 0
SET EXCLUSIVE ON
DELETE FILE (tmpo_path+"TMP_SLR2.DBF")
DELETE FILE (tmpo_path+"TMP_SLR2.CDX")
CREATE DBF (tmpo_path+"TMP_SLR2.DBF") ;
  (PS_CODE N( 5),  ;
   DOC_NUM C( 7),  ;
   DOC_DAT D,      ;
   PAY_NAM C(10),  ;
   SUM_ST  N(12,2),;
   SUM_EN  N(12,2) )
INDEX ON PS_CODE TAG PS_CODE
SET EXCLUSIVE OFF
