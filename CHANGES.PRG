*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Changes      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                        ���瓷 ���キ か� ��Ж罔�.                        �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 09/24/1999 様�
PROCEDURE Changes
PARAMETERS position

PRIVATE s_sav, r_part

s_sav = SELECT()
SELECT 0
DELETE FILE (tmpo_path+"CHANGES.DBF")
CREATE DBF (tmpo_path+"CHANGES.DBF") ;
  ( CODE     N( 7), ;
    SOURCE   C(40) )
SELECT 0
*
*    鴜�ガ瘴 甌÷�碎 ������� �� ��爿鍖
*
IF File_O(path_comm+"SUBJECT.DBF")
  USE (path_comm+"SUBJECT") ORDER TAG STO_CODE AGAIN ALIAS RT_9924
  SET FILTER TO STO_CODE # 0
  IF SEEK(position)  && ��Ж罔� メ碎 � �����
    r_part = ROOT
    SET ORDER TO TAG ROOT
    SET FILTER TO STO_CODE # position .AND. STO_CODE # 0  && �皰° �� 礫���碎 �� 瓮￥
    IF .NOT. EMPTY(r_part)    && � °� �� ��爛��!?
      SEEK r_part
      SCAN REST WHILE r_part = ROOT
        SELECT CHANGES
        APPEND BLANK
        REPLACE CODE   WITH RT_9924.STO_CODE, ;
                SOURCE WITH "��爛��"
        SELECT RT_9924
      ENDSCAN
    ENDIF
  ENDIF
  USE
ENDIF
*
*   �薀� � 甎�瓷� ��������
*
USE (base_path+"ANALOG") ORDER TAG CODE ALIAS AN_9924 AGAIN
IF SEEK(position)
  SCAN REST WHILE position = CODE_1
    SELECT CHANGES
    APPEND BLANK
    REPLACE CODE   WITH AN_9924.CODE_2, ;
            SOURCE WITH AN_9924.SOURCE
    SELECT AN_9924
  ENDSCAN
ENDIF
USE

SELECT CHANGES
USE

SELECT (s_sav)

RETURN
