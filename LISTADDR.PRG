*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� ListAddr     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                       �矗��� �むメ�� か� 甎�瓷�.                       �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 13.11.2001 様�
PROCEDURE ListAddr
PARAMETERS l_num, str_len

PRIVATE s_sav, tmpStr, tmpList

s_sav = SELECT()
IF TYPE("l_num") = "C"
  tmpList = l_num
ELSE
  tmpList = STR(l_num, 6)
ENDIF

SELECT 0
USE (base_path+"BOXES") SHARED AGAIN ALIAS BOX_1A13 ORDER TAG BOX_NO

SELECT 0
USE (base_path+"BOXCONT") SHARED AGAIN ALIAS ADDR_1A13 ORDER TAG LIST_IN
SET RELATION TO BOX_NO INTO BOX_1A13
tmpStr = ""

IF SEEK(tmpList)
  SCAN REST WHILE LIST_NUM == tmpList
    tmpStr = tmpStr+ALLTRIM(BOX_1A13.BOXADDRESS)+","
  ENDSCAN
ENDIF

IF LEN(tmpStr) > 1
  tmpStr = LEFT(tmpStr, LEN(tmpStr)-1)
ENDIF

USE
SELECT BOX_1A13
USE
SELECT (s_sav)

IF TYPE("str_len") = "N" 
  IF LEN(tmpStr) > 4
    IF LEN(tmpStr) > str_len
      tmpStr = LEFT(tmpStr, str_len-4)+" !!!"
    ENDIF
  ENDIF
  tmpStr = PADR(tmpStr, str_len)
ENDIF

RETURN tmpStr
