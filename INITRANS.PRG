*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� IniTrans     ��о�｀砒┴ ��むォ ��瓱�           06.03.98 11:03:05 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                  ���罔��├�罔� ����� ��痳��� か� ��.                   �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE IniTrans
PARAMETER n_word
PRIVATE nf, bf, i, p

IF n_word > 1
  IF .NOT. File_O(path_comm+"STO_LET.DBF")
    RETURN ""
  ELSE
    RETURN path_comm+"STO_LET.DBF"
  ENDIF
ELSE
  IF .NOT. EMPTY(Get_Sys("COM_SALE"))
    IF .NOT. File_O(path_comm+"SALE.DBF")
      RETURN ""
    ELSE
      RETURN path_comm+"SALE.DBF"
    ENDIF
  ELSE
    RETURN ""
  ENDIF
ENDIF
