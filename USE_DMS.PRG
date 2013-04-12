*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Use_DMS      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                           ����⨥ 䠩�� ���                           �
*�                                                                        �
*����������������������������������������������������������� 13.02.2009 �ͼ
PROCEDURE Use_DMS
PARAMETERS prmAlias

PRIVATE tmpMacro, tmpArr, n, i, tmpName, tmpTime, tmpDate

m.n = ADIR(tmpArr, m.base_path+"DMS_*.DBF")

IF m.n = 0
  RETURN .F.
ENDIF

= ASORT(tmpArr, 3, -1, 1)
m.tmpName = tmpArr[1, 1]
m.tmpTime = tmpArr[1, 4]
m.tmpDate = tmpArr[1, 3]
FOR m.i = 1 TO m.n
  IF m.tmpDate # tmpArr[m.i, 3]
    EXIT
  ENDIF
  IF m.tmpTime < tmpArr[m.i, 4]
    m.tmpName = tmpArr[m.i, 1]
    m.tmpTime = tmpArr[m.i, 4]
  ENDIF
ENDFOR

m.tmpMacro = " ALIAS "+IIF(TYPE("m.prmAlias")="C", m.prmAlias, "DMS")
USE (m.base_path+m.tmpName) SHARED AGAIN &tmpMacro

RETURN .T.