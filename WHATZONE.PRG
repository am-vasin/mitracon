*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� WhatZone     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                   ｯ爛ぅ�キ┘ М�� 瓷���� �� �むメ�.                   �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 06.01.2000 様�
PROCEDURE WhatZone
PARAMETERS addr

IF .NOT. USED("ZONES")
  IF FILE_O(base_path+"ZONES.DBF")
    USE (base_path+"ZONES.DBF") ORDER TAG ZONE AGAIN IN 0
  ELSE
    RETURN "  "
  ENDIF
ENDIF

IF SEEK(LEFT(addr,2), "ZONES")
*IF SEEK(LEFT(addr,1), "ZONES")
  RETURN ZONES.CHAR
ENDIF

RETURN "**"
