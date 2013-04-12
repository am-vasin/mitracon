*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                      บ
*บ         เฎฃเ ฌฌ  ฏเจข๏งชจ ฏเจซฎฆฅญจ๏ ช HARD คจแชใ.                  บ
*บ                                                                      บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE ClrPrice
PARAMETERS s1, s2

PRIVATE sw, i, n
sw = ""
n = LEN(s2)
DO WHILE LEN(sw) < n
  sw = sw+s1
ENDDO

s2 = STUFF(s2,1,1,CHR(MOD(ASC(SUBSTR(s2,1,1))-ASC(RIGHT(s2,1)),256)))

FOR i = n TO 1 STEP -1
  s2 = STUFF(s2,i,1,CHR(MOD(ASC(SUBSTR(s2,i,1))-ASC(SUBSTR(sw,i,1))-IIF(i#1, ASC(SUBSTR(s2,i-1,1)), 0)+1024,256)))
ENDFOR

RETURN
