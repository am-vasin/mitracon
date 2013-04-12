*…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª
*∫ î†©´ View_Nt      ê†ß‡†°Æ‚Á®™ Ä≠§‡•© Ç†·®≠           18.06.98 16:00:08 ∫
*«ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ∂
*∫                                                                        ∫
*∫                    è‡Æ·¨Æ‚‡ Ø‡®¨•Á†≠®Ô §´Ô ØÆß®Ê®®.                    ∫
*∫                                                                        ∫
*»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº
PROCEDURE View_Nt
PARAMETER s_alias

PRIVATE win_name      && à¨Ô Æ™≠† ( Æ™Æ≠ §´Ô BROWSE ).
PRIVATE s_sav

*
*   á†ØÆ´≠Ô•¨ ß≠†Á•≠®Ô¨® Ø•‡•¨•≠≠Î• ·Æ·‚ÆÔ≠®Ô...
*
win_name   = PROGRAM()
s_sav = SELECT()
IF EMPTY(s_alias)
  SELECT STOCK
ELSE
  SELECT (s_alias)
ENDIF

*------------------------------------------------------------------------
*    ëÆ§•‡¶†‚•´Ï≠†Ô Á†·‚Ï Ø‡Æ£‡†¨¨Î:
*

*⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
*≥                                          ≥
*≥                                          ≥
*¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ

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
@ WROWS()-1, n SAY " ç†¶¨®‚• Á‚Æ-≠®°„§Ï... "
= INKEY(0, "HM")

RELEASE WINDOW (win_name)
SELECT (s_sav)
RETURN
