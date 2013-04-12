*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ            เฎฃเ ฌฌ  ฏฎคฃฎโฎขชจ ฎชญ  ไจซ์โเ  แฏจแชฎข.                 บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Prp_FltL
PRIVATE l, c, ln, wd


*
*   ”ฎเฌจเใฅฌ ฎชญฎ ไจซ์โเ 
*
ln = 10
wd = 68
l = WROWS("")-ln-1
c = FLOOR((WCOLS("")-wd)/2)

DEFINE WINDOW FltL FROM l, c TO l+ln-1, c+wd-1 NONE SHADOW COLOR SCHEME 13
ACTIVATE WINDOW FltL
@ 0, 0 SAY PADC("”จซ์โเ แฏจแชฎข",WCOLS()) COLOR SCHEME 5
@ 1, 1 TO 5, 44
@ 1,46 TO 5, 66
@ 6, 1 TO 9, 66
@ 1, 3 SAY " ‘ฏจแฎช "
@ 1,48 SAY " ฅฏฎงจโ "
@ 6, 3 SAY " ซจฅญโ "

DIMENSION fltl(13, 4)

fltl( 1, 1) = .T.
fltl( 1, 2) = "ฎซฃ"
fltl( 1, 3) = 2
fltl( 1, 4) = 7

fltl( 2, 1) = .T.
fltl( 2, 2) = "‘ชจคช "
fltl( 2, 3) = 3
fltl( 2, 4) = 7

fltl( 3, 1) = .T.
fltl( 3, 2) = "‘็ฅโ"
fltl( 3, 3) = 4
fltl( 3, 4) = 7

fltl( 4, 1) = .T.
fltl( 4, 2) = "ฅไจๆจโ"
fltl( 4, 3) = 2
fltl( 4, 4) = 18

fltl( 5, 1) = .T.
fltl( 5, 2) = "‘ฏฅๆๆฅญ "
fltl( 5, 3) = 3
fltl( 5, 4) = 18

fltl( 6, 1) = .T.
fltl( 6, 2) = " ชซ คญ ๏"
fltl( 6, 3) = 4
fltl( 6, 4) = 18

fltl( 7, 1) = .T.
fltl( 7, 2) = " ๏ขช "
fltl( 7, 3) = 2
fltl( 7, 4) = 32

fltl( 8, 1) = .T.
fltl( 8, 2) = "“โขฅเฆคฅญจฅ"
fltl( 8, 3) = 3
fltl( 8, 4) = 32

fltl( 9, 1) = .T.
fltl( 9, 2) = "ก๋็ญ๋ฉ"
fltl( 9, 3) = 2
fltl( 9, 4) = 52

fltl(10, 1) = .T.
fltl(10, 2) = "ญคฅชแจเใฅฌ๋ฉ"
fltl(10, 3) = 3
fltl(10, 4) = 52

fltl(11, 1) = .T.
fltl(11, 2) = "ฎค เฎช"
fltl(11, 3) = 4
fltl(11, 4) = 52

fltl(12, 1) = .T.
fltl(12, 2) = "ฎแโฎ๏ญญ๋ฉ"
fltl(12, 3) = 7
fltl(12, 4) = 7

fltl(13, 1) = .T.
fltl(13, 2) = " งฎข๋ฉ"
fltl(13, 3) = 7
fltl(13, 4) = 24

ln = ALEN(fltl,1)

FOR l = 1 TO ln
  @ fltl(l,3), fltl(l,4)-4 SAY "[X]"
  @ fltl(l,3), fltl(l,4)   SAY fltl(l,2)
ENDFOR

@ WROWS()-2, 3 SAY "ฌ๏"

RETURN
