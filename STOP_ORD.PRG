*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Stop_Ord     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                       �����஢�� ������ �� �����                       �
*�                                                                        �
*����������������������������������������������������������� 21.04.2006 �ͼ
PROCEDURE Stop_Ord
PARAMETERS l_num, p_code, tmpQnt

*
* l_num   - ����� ᯨ᪠ (C),
* p_code  - ��� ����樨,
* tmp_qnt - ������⢮.
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