*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º   ˆ¬ï ä ©«  P_Req         §à ¡®âç¨ª €­¤à¥© ‚ á¨­                      º
*ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
*º                                                                        º
*º             ¥ç âì âà¥¡®¢ ­¨ï ­  ¢ë¤ çã â®¢ à  á® áª« ¤ .              º
*º                                                                        º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ 10/27/1998 ÍÍ¼
PROCEDURE P_Req
PARAMETERS f_code, n_doc, d_doc

*
*  n_doc - ­®¬¥à ­ ª« ¤­®© ( ¢­ãâà¥­­¨© ä®à¬ â );
*  d_doc - «¨¡® ¤ â  ­ ª« ¤­®© «¨¡® £®¤ ¢ á¨¬¢®«ì­®¬ ¨«¨ ç¨á«®¢®¬ ä®à¬ â¥;
*
PRIVATE p_ln, fnt, n_cp, lft, l_num, n, i, j
PRIVATE t_d, f_p, f_d, d_year, ffeed, p_drctry
PRIVATE sel_rem, last_c
PRIVATE q_p_n, q_pos, q_neg

sel_rem = SELECT()

IF TYPE("d_doc") = "D"
  d_year = LEFT(DTOS(d_doc),4)
ENDIF
IF TYPE("d_doc") = "N"
  d_year = STR(d_doc,4)
ENDIF
IF TYPE("d_doc") = "C"
  d_year = d_doc
ENDIF
*---------------------------------------------------------

q_p_n = 0
q_pos = 0
q_neg = 0

SELECT 0
USE (base_path+"ORD_T") ORDER TAG DOC_NUM ALIAS T_0130 AGAIN
IF SEEK(STR(LIST_TIT.LST_NUM,6))
  SCAN REST WHILE LIST_NUM = STR(LIST_TIT.LST_NUM,6) 
    IF .NOT. EMPTY(SIZE_POS) .AND. .NOT. EMPTY(SIZE_NEG)
      q_p_n = q_p_n + 1
    ELSE
      IF SIZE_POS # 0 
        q_pos = q_pos + 1
      ENDIF
      IF SIZE_NEG # 0
        q_neg = q_neg + 1
      ENDIF
    ENDIF
  ENDSCAN
ENDIF
USE
*---------------------------------------------------------
DIMENSION sb(10,2)
sb( 1,1) = "{List_Num}"
sb( 1,2) = ""

sb( 2,1) = "{Cus_Name}"
sb( 2,2) = ""

sb( 3,1) = "{Qnt}"
sb( 3,2) = ""

sb( 4,1) = "{Date}"
sb( 4,2) = DTOC(DATE())

sb( 5,1) = "{Autor                       }"
SELECT 0
USE (base_path+"PERSONS.DBF") ORDER TAG CODE ALIAS C9903 AGAIN
IF user = 0
  sb(5,2) = "‘€Œ!"
ELSE
  IF SEEK (user)
    sb(5,2) = ALLTRIM(FAMILY)+" "+LEFT(NAME,1)+" "+LEFT(S_NAME,1)    
  ELSE
    sb(5,2) = "?!!"
  ENDIF
ENDIF         
sb(5,2) = PADR(sb(5,2),LEN(sb(5,1)))

sb( 6,1) = "{Address                       }"
*sb( 6,2) = LIST_TIT.ADDRESS
sb( 6,2) = ListAddr(LIST_TIT.LST_NUM, LEN(sb( 6,1)))

sb( 7,1) = "{Q01}"       && ¢á¥£® § ï¢®ª
sb( 7,2) = STR(LIST_TIT.ORD_NUM,3)

sb( 8,1) = "{Q02}"       && ¢á¥£® "á¬¥è ­­ëå" § ï¢®ª
sb( 8,2) = STR(q_p_n,3)

sb( 9,1) = "{Q03}"       && ¢á¥£® "¯®«®¦¨â¥«ì­ëå" § ï¢®ª
sb( 9,2) =  STR(q_pos,3)

sb(10,1) = "{Q04}"       && ¢á¥£® "®âà¨æ â¥«ì­ëå" § ï¢®ª
sb(10,2) = STR(q_neg,3)

USE

SELECT 0
IF LEFT(n_doc, 1) = " "
  USE (base_path+"RYN_M_T") ORDER TAG DOC_NUM ALIAS ST8A28 AGAIN
  IF .NOT. SEEK(d_year+RIGHT(n_doc,5))
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"¥â ¤®ªã¬¥­â  á â ª¨¬ ­®¬¥à®¬!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    USE
    SELECT (sel_rem)
    RETURN
  ENDIF
  sb(1,2) = ALLTRIM(STR(LIST_TIT.LST_NUM))

  SELECT 0
  USE (base_path+"RYN_M") ORDER TAG DOC_NUM AGAIN ALIAS S8A28
  SEEK d_year+RIGHT(n_doc,5)
  last_c = 0
  sb(3,2) = 0
  SCAN REST WHILE DOC_NUM = ST8A28.DOC_NUM .AND. DOC_DATE = ST8A28.DOC_DATE
    IF last_c # CODE
      sb(3,2) = sb(3,2) + 1
      last_c = CODE
    ENDIF
  ENDSCAN
  sb(3,2) = ALLTRIM(STR(sb(3,2)))
  USE
ELSE
  USE (base_path+"SALE_TIT") ORDER TAG DOC_NUM ALIAS ST8A28 AGAIN
  IF .NOT. SEEK(f_code+d_year+n_doc)
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"¥â ¤®ªã¬¥­â  á â ª¨¬ ­®¬¥à®¬!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    USE
    SELECT (sel_rem)
    RETURN
  ENDIF
  sb(1,2) = ALLTRIM(LIST_NUM)

  SELECT 0
  USE (base_path+"SALE") ORDER TAG DOC_NUM AGAIN ALIAS S8A28
  SEEK f_code+d_year+n_doc
  last_c = 0
  sb(3,2) = 0
  SCAN REST WHILE FIRM = ST8A28.FIRM .AND. DOC_NUM = ST8A28.DOC_NUM .AND. DOC_DATE = ST8A28.DOC_DATE
    IF last_c # CODE
      sb(3,2) = sb(3,2) + 1
      last_c = CODE
    ENDIF
  ENDSCAN
  sb(3,2) = ALLTRIM(STR(sb(3,2)))
  USE
ENDIF

IF ST8A28.CUS_CODE = 0
  sb(2,2) = ST8A28.CUS_NAME
ELSE
  USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE AGAIN ALIAS A8A28
  SEEK ST8A28.CUS_CODE
  sb(2,2) = CUS_NAME
  USE
ENDIF
SELECT ST8A28

USE (base_path+"DOC_FORM")
LOCATE FOR "REQUIRE" == ALLTRIM(UPPER(DOC_NAME))
p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed =DOC_FORM.F_FEED
p_drctry = DOC_FORM.P_DIR

n = MEMLINES(DOC_H)
IF n > 0
  DIMENSION t_d(n)
  FOR i = 1 TO n
    t_d(i) = MLINE(DOC_H,i)
  ENDFOR
  FOR i = 1 TO n
    FOR j = 1 TO ALEN(sb,1)
      t_d(i) = STRTRAN(t_d(i), sb(j,1), sb(j,2) )
    ENDFOR
  ENDFOR
ELSE
  DIMENSION t_d(1)
  t_d(1) = ""
ENDIF

DO Ini_Prn WITH "’à¥¡®¢ ­¨¥ ­  ¢ë¤ çã â®¢ à ", p_ln, lft, n_cp, fnt, ffeed, p_drctry

FOR i = 1 TO n
  @ IIF(i=1, PROW(), PROW()+1), IIF(i=1, PCOL(), 0) SAY t_d(i)
ENDFOR

DO Term_Prn WITH ""
USE

SELECT (sel_rem)

RETURN
