*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Seek_Key     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                   蹍������ ���瓷� �� �ム〓� ＜�▲.                    �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 31.12.1999 様�
PROCEDURE Seek_Key
PARAMETERS prmChar

PRIVATE nRec

IF .NOT. SEEK(prmChar)
  nRec = RECNO(0)
  IF BETWEEN(nRec, 1, RECCOUNT())
    GO nRec
  ENDIF
ENDIF

RETURN
