*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Choose_F     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                        �襦�� �爛く爬閧�� �� F1.                        �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 15.01.1999 様�
PROCEDURE Choose_F
PARAMETER n_f
PRIVATE nm_w
nm_w = ""

DO Ch_Firm WITH f_c000, f_n000, nm_w
IF f_c_old # f_c000
  b_c000 = "   "
  b_n000 = SPACE(16)
  SHOW GET b_n000
  f_c_old = f_c000
ENDIF
SHOW GET f_c000
SHOW GET f_n000
IF .NOT. EMPTY(n_f)
  DO (n_f)
ENDIF
IF EMPTY(ON("KEY","Tab"))
  KEYBOARD "{Tab}"
ELSE
  IF EMPTY(ON("KEY","DnArrow"))
    KEYBOARD "{DnArrow}"
  ENDIF
ENDIF
RETURN
