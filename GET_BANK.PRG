*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Get_Bank     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                        �キム�罔� GET か� �����.                        �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 15.01.1999 様�
PROCEDURE Get_Bank
PARAMETERS l_01, c_01, is_enab
PRIVATE s_sav

IF EMPTY(b_c000)
  b_n000 = SPACE(16)
ELSE
  s_sav = SELECT()
  SELECT 0
  USE (base_path+"BANKS") ORDER TAG BANK ALIAS BNK_9120 AGAIN
  IF SEEK(f_c000+b_c000)
    b_n000 = B_NAME
  ELSE
    b_c000 = "   "
    b_n000 = SPACE(16)
  ENDIF
  USE
  SELECT (s_sav)
ENDIF

IF is_enab
  @ l_01, c_01 GET b_sw000 PICTURE "@*C ����" VALID G_Bank()
  @ ROW(), COL()+1 GET b_n000 WHEN .F.
ELSE
  @ l_01, c_01+4 SAY "����"
  @ ROW(), COL()+1 GET b_n000 DISABLE
ENDIF

RETURN
