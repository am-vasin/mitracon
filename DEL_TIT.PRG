*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Del_Tit      ��о�｀砒┴ ��むォ ��瓱�           12.05.97 14:30:59 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                   ����キ┘ ����皰→� �爬絎�� 皰����.                   �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Del_Tit
PRIVATE c_num, buff

c_num = STR(RECNO("INC_TIT"),5)
SELECT INC_LIST
SET ORDER TO TAG L_CODE

DO WHILE SEEK(c_num) && NEW_COMP 18.04.2002
  BLANK             && NEW_COMP 18.04.2002
ENDDO                && NEW_COMP 18.04.2002
IF File_O(tmp_path+"DL"+ALLTRIM(STR(RECNO("INC_TIT")))+".DBF")
  SELECT 0
  USE (log_path+"DUE_LOG") ORDER TAG HRON
  SELECT 0
  USE (tmp_path+"DL"+ALLTRIM(STR(RECNO("INC_TIT")))) ALIAS LOG_TMP
  SCAN
    SCATTER TO buff
    SELECT DUE_LOG
    GO TOP
    GATHER FROM buff
    SELECT LOG_TMP
  ENDSCAN
  USE
  SELECT DUE_LOG
  USE
ENDIF
SELECT INC_NEW
SET ORDER TO TAG L_NAME

DO WHILE SEEK(c_num)
  BLANK
ENDDO

SELECT INC_DUE
SET ORDER TO TAG L_CODE

SEEK c_num
DELETE REST WHILE INC_LIST = c_num

SELECT INC_TIT
BLANK

DELETE FILE (tmp_path+"DL"+ALLTRIM(c_num)+".DBF")

RETURN
