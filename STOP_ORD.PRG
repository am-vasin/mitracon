*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Stop_Ord     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                       �����牀→� ������ �� ぎ�ｃ                       �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 21.04.2006 様�
PROCEDURE Stop_Ord
PARAMETERS l_num, p_code, tmpQnt

*
* l_num   - ���ム 甎�瓷� (C),
* p_code  - ��� ��Ж罔�,
* tmp_qnt - ����腑痰〓.
*

PRIVATE qnt0, s_sav, tmpRec

m.s_sav = SELECT()
SELECT 0
USE (m.base_path+"LINK_ORD") SHARED AGAIN ORDER TAG LIST_CODE ALIAS LO_6421
m.qnt0 = 0
SEEK m.l_num+STR(m.p_code)
m.tmpRec = 0
SCAN REST WHILE m.l_num = LIST_NUM .AND. m.p_code = CODE
  IF "**********" = ORD_NUM
    m.tmpRec = RECNO()
  ENDIF
  m.qnt0 = m.qnt0+QNT
ENDSCAN

IF m.tmpQnt - m.qnt0 > 0
  IF m.tmpRec # 0
    GO m.tmpRec
  ELSE
    APPEND BLANK
    REPLACE	CODE		WITH m.p_code,		;
    		LIST_NUM	WITH m.l_num,		;
    		ORD_FIRM	WITH "***",			;
    		ORD_YEAR	WITH "****",		;
    		ORD_NUM		WITH "**********",	;
    		MANAGER		WITH m.user
  ENDIF
  REPLACE QNT WITH QNT+m.tmpQnt-m.qnt0
ENDIF

USE
SELECT (m.s_sav)

RETURN
