*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� LINKPOS     ��о�｀砒┴ ��むォ ��瓱�                       �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�           ���瓷 � ���キ���矣爛 ��─��ゥ ��ゅ�わ薀� ��Ж罔�.            �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 02/10/2000 様�
PROCEDURE Link_Pos
PARAMETERS pPref, pName, pProd, als

PRIVATE tmpMetric, tmpName, tmpProd, lenName
PRIVATE i, tmpRec, tmpCnt, tmpVal, s_sav
STORE 0 TO curMetric, tmpMetric

s_sav = ALIAS()
IF TYPE("als") $ "NC"
  SELECT (als)
ENDIF

tmpName = SeekName(pPref)+SeekName(pName)
tmpProd = SeekName(pProd)
lenName = LEN(tmpName)
tmpMetric = 0
i = MIN(lenName,3)
DO WHILE SEEK(LEFT(tmpName,i))
  tmpMetric = i
  tmpRec = RECNO()
  i = i+1
  IF i > lenName
    EXIT
  ENDIF
ENDDO
IF tmpMetric > 0
  DIMENSION arrlink(1,2)
  GO tmpRec
  tmpCnt = 0
  tmpVal = LEFT(tmpName, tmpMetric)
  SCAN REST WHILE tmpCnt < 11 .AND. tmpVal = LEFT(ALLTRIM(PREFIX1)+NAME1, tmpMetric)
    DIMENSION arrlink(ALEN(arrLink,1)+1,2)
    arrlink(ALEN(arrLink,1),1) = CODE
    arrlink(ALEN(arrLink,1),2) = 1
    IF ALLTRIM(PRODUCER1) == tmpProd
      arrlink(ALEN(arrLink,1),2) = arrlink(ALEN(arrLink,1),2)-1
    ENDIF
    tmpCnt = tmpCnt+1
  ENDSCAN
ENDIF

SELECT (s_sav)

RETURN
