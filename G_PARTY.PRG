********************************************************************************
*    ���� G_Party        ���ࠡ��稪 ���३ ��ᨭ                13.02.2013    *
********************************************************************************
*                                                                              *
*                           ����祭�� ����� ���⨨                            *
*                                                                              *
********************************************************************************
PROCEDURE G_Party

PRIVATE tmpYear, nn

m.tmpYear = STR(YEAR(DATE()), 4)
m.nn = Doc_Num("PARTY", .F., .F., m.tmpYear)
RETURN RIGHT(m.tmpYear, 2)+STR(m.nn, 8)
