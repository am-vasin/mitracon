*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� To_URM       ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�       ��爼�牀���┘ ����� ├ 甎�瓷� か� 磴��キ���� ��｀腑�� �メ��       �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 24.10.2007 様�
PROCEDURE To_URM
PARAMETERS l_num

PRIVATE tmpNm, s_sav, mss, s_list

m.s_list = Seg_Lst()
IF EMPTY(m.s_list)
  RETURN
ENDIF
m.s_sav = SELECT()
SELECT 0
USE m.path_comm+"SUBJECT" SHARED AGAIN ORDER TAG STO_CODE ALIAS SJ_07A24
SELECT 0
USE m.base_path+"LIST_DET" SHARED AGAIN ORDER TAG L_NAME ALIAS LS_07A24
SET RELATION TO CODE INTO SJ_07A24
SEEK m.l_num
LOCATE REST WHILE m.l_num = LIST_NUM FOR QNT # 0 .AND. CODE # 0 .AND. ;
                  FOUND("SJ_07A24") .AND. SJ_07A24.SEGMENT $ m.s_list
IF .NOT. FOUND()
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���甌� �竅�..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE IN SJ_07A24
  USE
  SELECT (m.s_sav)
  RETURN
ENDIF
m.tmpNm = New_File("", "DBF", "�ぅ 甌縲���碎 �����")
IF EMPTY(m.tmpNm)
  USE IN SJ_07A24
  USE
  SELECT (m.s_sav)
  RETURN
ENDIF

COPY REST WHILE m.l_num = LIST_NUM FOR QNT # 0 .AND. CODE # 0 .AND. ;
                FOUND("SJ_07A24") .AND. SJ_07A24.SEGMENT $ m.s_list ;
       TO (m.tmpNm) FIELDS CODE, QNT

USE IN SJ_07A24
USE
SELECT (m.s_sav)
RETURN
