*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� View_Nt      ��о�｀砒┴ ��むォ ��瓱�           18.06.98 16:00:08 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                    蹍甃�矗 �爬�ョ���� か� ��Ж罔�.                    �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE View_Nt
PARAMETER s_alias

PRIVATE win_name      && ��� ���� ( ���� か� BROWSE ).
PRIVATE s_sav

*
*   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
*
win_name   = PROGRAM()
s_sav = SELECT()
IF EMPTY(s_alias)
  SELECT STOCK
ELSE
  SELECT (s_alias)
ENDIF

*------------------------------------------------------------------------
*    ��ぅ爨�皀�讚�� ��痰� �牀������:
*

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�                                          �
*�                                          �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE lin_1, lin_2, n, i
lin_1 = ""
lin_2 = ""
n = MEMLINES(NOTE)
IF n >= 1
  lin_1 = MLINE(NOTE,1)
ENDIF
IF n >= 2
  lin_2 = MLINE(NOTE,2)
ENDIF
IF EMPTY(lin_1) .AND. EMPTY(lin_2)
  SELECT (s_sav)
  RETURN
ENDIF
DO D_Win_N WITH 5, 46, ALLTRIM(PREFIX)+" "+ALLTRIM(NAME)+ ;
   IIF(.NOT. EMPTY(PRODUCER), " /"+ALLTRIM(PRODUCER)+"/", "")
@ 2, 3 SAY lin_1
@ 3, 3 SAY lin_2

n = FLOOR((WCOLS()-23)/2)
@ WROWS()-1, n SAY " ��Μ�皀 艪�-�─磴�... "
= INKEY(0, "HM")

RELEASE WINDOW (win_name)
SELECT (s_sav)
RETURN
