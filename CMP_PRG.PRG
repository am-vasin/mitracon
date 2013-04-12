*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ Cmp_Prg       งเ กฎโ็จช €ญคเฅฉ  แจญ           03.04.98 11:48:41 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                      ‘เ ขญฅญจฅ โฅชแโฎข๋ๅ ไ ฉซฎข.                       บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Cmp_Prg

PRIVATE name1, name2, nf1, nf2, buff1, buff2, cnt_lin, cnt_err, ex

name1 = SPACE(60)
name2 = SPACE(60)

CLEAR

@ 10, 6 SAY "” ฉซ 1" GET name1 WHEN Ini(1) VALID Clr_K()
@ 12, 6 SAY "” ฉซ 2" GET name2 WHEN Ini(2) VALID Clr_K()
READ CYCLE

IF READKEY() % 256 = 12
  RETURN
ENDIF

cnt_lin = 0
cnt_err = 0
name1 = ALLTRIM(name1)
nf1 = FOPEN(name1)
IF nf1 <= 0
  WAIT WINDOW "ฅใค ็  ฏเจ ฎโชเ๋โจจ ไ ฉซ  1"
  RETURN
ENDIF

name2 = ALLTRIM(name2)
nf2 = FOPEN(name2)
IF nf2 <= 0
  WAIT WINDOW "ฅใค ็  ฏเจ ฎโชเ๋โจจ ไ ฉซ  2"
  = FCLOSE(nf1)
  RETURN
ENDIF
ex = 1
DO WHILE .NOT. FEOF(nf1) .AND. .NOT. FEOF(nf2)
  cnt_lin = cnt_lin+1
  buff1 = FGETS(nf1, 500)
  buff2 = FGETS(nf2, 500)
  IF .NOT. buff1 == buff2
    CLEAR
    ? "‘โเฎช  "+ALLTRIM(STR(cnt_lin))+":"
    ? "” ฉซ "+name1+":"
    ? buff1
    ? "” ฉซ "+name2+":"
    ? buff2
    cnt_err = cnt_err+1
    ex = 1
    @ ROW()+1, 5 GET ex PICTURE ;
       "@*HT \ เฎคฎซฆจโ์ ;\ ” ฉซ 1 ;\ ” ฉซ 2 ;\ ๋ๅฎค "
    READ CYCLE
    DO CASE
    CASE ex = 2
      = FCLOSE(nf1)
      = FCLOSE(nf2)
      MODIFY COMMAND (name1)
      EXIT
    CASE ex = 3
      = FCLOSE(nf1)
      = FCLOSE(nf2)
      MODIFY COMMAND (name2)
      EXIT
    CASE ex = 4
      = FCLOSE(nf1)
      = FCLOSE(nf2)
      EXIT
    ENDCASE
  ENDIF
ENDDO

CLEAR
IF cnt_err = 0
  IF .NOT. FEOF(nf1) .OR. .NOT. FEOF(nf2)
    ? "” ฉซ  เ งญฎฉ คซจญ๋!"
  ELSE
    ? " แๅฎฆคฅญจฉ ญฅโ!"
  ENDIF
  = FCLOSE(nf1)
  = FCLOSE(nf2)
ELSE
  IF ex = 1
    ? "แฅ!!"
    = FCLOSE(nf1)
    = FCLOSE(nf2)
  ENDIF
ENDIF

RETURN

***************************************************************
PROCEDURE Ini
PARAMETERS n

IF n = 1
  ON KEY LABEL F1 DO G_File WITH 1
ELSE
  ON KEY LABEL F1 DO G_File WITH 2
ENDIF

RETURN .T.

***************************************************************
PROCEDURE Clr_K
ON KEY LABEL F1
RETURN .T.

***************************************************************
PROCEDURE G_File
PARAMETER w_f

PRIVATE w

w = GETFILE()
IF EMPTY(w)
  RETURN
ENDIF

IF w_f = 1
  name1 = PADR(w, 60)
  SHOW GET name1
ELSE
  name2 = PADR(w, 60)
  SHOW GET name2
ENDIF
