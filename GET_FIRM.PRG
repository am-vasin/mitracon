*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Get_Firm     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                        �キム�罔� GET か� 筥爼�.                        �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 15.01.1999 様�
PROCEDURE Get_Firm
PARAMETERS l_01, c_01, is_enab, exit_proc
PRIVATE s_sav, n_f
n_f = IIF(EMPTY(exit_proc), "", '"'+exit_proc+'"')

IF EMPTY(f_c000)
  f_n000 = SPACE(16)
ELSE
  s_sav = SELECT()
  SELECT 0
  USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS FRM_9120 AGAIN
  IF SEEK(f_c000)
    f_n000 = B_NAME
  ELSE
    f_c000 = "   "
    f_n000 = SPACE(16)
  ENDIF
  USE
  SELECT (s_sav)
ENDIF

@ l_01, c_01 SAY "踳く爬閧┘ "
IF is_enab
  @ ROW(), COL() GET f_c000 WHEN S_F1_(&n_f) VALID C_F1_(&n_f)
  @ ROW(), COL()+1 GET f_n000 WHEN .F.
  @ ROW(), COL()+1 SAY "(F1)"
ELSE
  @ ROW(), COL() GET f_c000 DISABLE
  @ ROW(), COL()+1 GET f_n000 DISABLE
ENDIF

RETURN
