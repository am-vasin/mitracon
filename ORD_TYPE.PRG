*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Ord_Type     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                        ｯ爛ぅ�キ┘ 皋�� ������                         �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 17.06.2007 様�
PROCEDURE Ord_Type
PARAMETERS prmCode, prmReal
*
*  prmCode - ��� ��Ж罔�;
*  prmReal - メ�� .T. - 爛��├�罔�, ┃�腑 ���祚��.
*  �襷�� �牀������ ．� �����モ牀� - ���琺皋� ������.
*

IF PARAMETERS() = 0
  IF USED("_OT_STOCK")
    USE IN _OT_STOCK
    USE IN _OT_SUBJ
    USE IN _OT_SEG
    USE IN _OT_PRO
  ENDIF
  RETURN 0
ENDIF

IF .NOT. USED("_OT_STOCK")
  USE (base_path+"STOCK") SHARED AGAIN ORDER TAG CODE ALIAS _OT_STOCK IN 0
  USE (path_comm+"SUBJECT") SHARED AGAIN ORDER TAG STO_CODE ALIAS _OT_SUBJ IN 0
  USE (base_path+"ORD_PRO") SHARED AGAIN ORDER TAG PRO_CODE ALIAS _OT_PRO IN 0
  USE (base_path+"ORD_SEG") SHARED AGAIN ORDER TAG SEGMENT ALIAS _OT_SEG IN 0
ENDIF

= SEEK(m.prmCode, "_OT_STOCK")
IF m.prmReal
  IF .NOT. EMPTY(_OT_STOCK.ORD_REAL)
    RETURN _OT_STOCK.ORD_REAL
  ENDIF
ELSE
  IF .NOT. EMPTY(_OT_STOCK.ORD_BUY)
    RETURN _OT_STOCK.ORD_BUY
  ENDIF
ENDIF

IF .NOT. EMPTY(_OT_STOCK.PRO_CODE)
  IF SEEK(IIF(m.prmReal, "R", "B")+STR(_OT_STOCK.PRO_CODE), "_OT_PRO")
    RETURN _OT_PRO.ORD_CODE
  ENDIF
ENDIF

IF SEEK(m.prmCode, "_OT_SUBJ")
  = SEEK(UPPER(_OT_SUBJ.SEGMENT), "_OT_SEG")
  IF m.prmReal
    RETURN _OT_SEG.ORD_REAL
  ELSE
    RETURN _OT_SEG.ORD_BUY
  ENDIF
ENDIF

RETURN 0
