*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� V_Addr       ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                    ��爼�牀���┘ �むメ� か� �ョ�皋.                     �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 03.07.2000 様�
PROCEDURE V_Addr
PARAMETERS parmAddr

IF sale_mode = "Compel"
  IF EMPTY(SUBSTR(parmAddr, 2, 1))
    RETURN parmAddr+"   "
  ELSE
    RETURN LEFT(parmAddr,2)+"-"+SUBSTR(parmAddr, 3, 2)+"-"+  ;
           SUBSTR(parmAddr, 5, 2)+"-"+SUBSTR(parmAddr, 7, 2)
  ENDIF
ENDIF

RETURN parmAddr
