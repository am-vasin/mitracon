*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ          เฎฃเ ฌฌ  ไฎเฌจเฎข ญจ๏ แฏจแช  จง ญฎฌฅญชซ โใเ๋                บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Sto_Seek

PRIVATE l_n, l_br0, c_br0, ln_br0, fl_br0, recs_m, recs_c, ;
        imag_br, vals_br, foot_br, l_tit, o_exist
PRIVATE w_ln, w_wd, c, fnd, c1_pos, c2_pos, how_ex, l_mnt, pic_w, tmp_rec
PRIVATE keyClass, ClassName, pageLen
keyClass = ""
ClassName = "แฅ ชซ แแ๋"
pageLen = 0

l_mnt = IIF(LIST_TIT.IS_USD, 4, 5)
pic_w = IIF(LIST_TIT.IS_USD, "@Z 99999.9999", "@Z 999999.99999")

DEFINE WINDOW Not_Sto FROM 1,WCOLS("")-18 TO 3,WCOLS("")-2 NONE SHADOW COLOR SCHEME 7
ACTIVATE WINDOW Not_Sto NOSHOW
@ 1, 0 SAY PADC("ฅ แชซ คแช ๏!",WCOLS())

o_exist = .F.
fnd = .F.
w_ln = MIN(WROWS("")-5, RECCOUNT()+5)
c1_pos = LEN(Br_Line2())
c2_pos = LEN(Br_Line1())

w_wd = c1_pos+c2_pos+5  &&+IIF(LIST_TIT.IS_USD, 5, 9)
c1_pos = c2_pos+3
c2_pos = c1_pos+7
l_tit = "‘ฏจแฎช N "+ALLTRIM(lc)+" ฎโ "+TRANSFORM(LIST_TIT.CR_DAT,"@D")+" "+ALLTRIM(LIST_TIT.LST_NAME)
DO D_W WITH "W_Seek", w_ln, w_wd, l_tit, 0

*@ 1, 43 SAY "‘ใฌฌ "
@ 3, 1 TO WROWS()-IIF(sale_mode = "Mitracon", 3, 2), WCOLS()-2  && ชซ แแจไจช ๆจ๏ 29.10.2005
IF LIST_TIT.IS_USD
  c = "Aฤ"+PADC(" จฌฅญฎข ญจฅฤฤฤฤฤฤฤฤ", FSIZE("PREFIX","STOCK")+FSIZE("PRODUCER","STOCK")+25,"ฤ")+ ;
      "ฤฤฤฤฤฤ–ฅญ (‘ช)ฤฎซ-ขฎฤฤฤ–ฅญ (เ)"
ELSE
  c = "Aฤ"+PADC(" จฌฅญฎข ญจฅฤฤฤฤฤฤฤฤ", FSIZE("PREFIX","STOCK")+FSIZE("PRODUCER","STOCK")+25,"ฤ")+ ;
      "ฤฤฤฤฤฤฤ–ฅญ (‘ช)ฤฤฎซ-ขฎฤฤฤฤฤ–ฅญ (เ)"
ENDIF
@ 3, 2 SAY c    && ชซ แแจไจช ๆจ๏ 29.10.2005
IF is_cor
  @ 1, WCOLS()-25 SAY "ฎเฏใแ"
ENDIF
*@ 1, WCOLS()-48 SAY ". โฏใแช "
@ 1, WCOLS()-48 SAY "       ‘"
@ 2, 2 SAY "ซ แแ "+ClassName

*@ 1, 49 SAY STR(sum_list,12,l_mnt) COLOR SCHEME 1

l_br0  = 4           && ฅเข ๏ แโเฎช  BROWSE && ชซ แแจไจช ๆจ๏ 29.10.2005
c_br0  = 2           && ฅเข ๏ ฏฎงจๆจ๏ BROWSE
*ln_br0 = WROWS()-IIF(sale_mode = "Mitracon", 6, 5)   && ฎซจ็ฅแโขฎ แโเฎช BROWSE
ln_br0 = WROWS()-IIF(sale_mode = "Mitracon", 7, 6) && ชซ แแจไจช ๆจ๏ 29.10.2005
fl_br0 = 1           && “ช ง โฅซ์ BROWSE

recs_m = ""          && ฎฌฅเ  ง ฏจแฅฉ ฎแญฎขญฎฃฎ ไ ฉซ 
recs_c = ""          && ฎฌฅเ  ง ฏจแฅฉ แข๏ง ญญฎฃฎ ไ ฉซ 
imag_br = ""         && เฅคแโ ขซฅญจฅ แโเฎช BROWSE
foot_br = ""         && ญไฎเฌ ๆจฎญญ ๏ แโเฎช  BROWSE
vals_br = ""         && ญ ็ฅญจ๏ ฎกเ ก โ๋ข ฅฌ๋ๅ ฏฎซฅฉ 

GO TOP
IF TYPE("ent_rec") = "N"
  IF ent_rec > 0 .AND. ent_rec < RECCOUNT()
    GO ent_rec
  ENDIF
ENDIF
DO Md_Image
DO Draw_Br
how_ex = Locator(1, 2, 25)
RELEASE WINDOW Not_Sto
RELEASE WINDOW W_Seek

RETURN how_ex

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                เฎฃเ ฌฌ  ฏฎคฃฎโฎขชจ ฌ แแจขฎข BROWSE                   บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Md_Image
PARAMETER stop_
PRIVATE i, dr, s, fnd, s0, s1, s2, s3, s4, rt

*l_br0  = 3           && ฅเข ๏ แโเฎช  BROWSE
*c_br0  = 2           && ฅเข ๏ ฏฎงจๆจ๏ BROWSE
*ln_br0 = WROWS()-4   && ฎซจ็ฅแโขฎ แโเฎช BROWSE

fl_br0 = 1           && “ช ง โฅซ์ BROWSE

recs_m = ""          && ฎฌฅเ  ง ฏจแฅฉ ฎแญฎขญฎฃฎ ไ ฉซ 
recs_c = ""          && ฎฌฅเ  ง ฏจแฅฉ แข๏ง ญญฎฃฎ ไ ฉซ 
imag_br = ""         && เฅคแโ ขซฅญจฅ แโเฎช BROWSE
vals_br = ""         && ญ ็ฅญจ๏ ฎกเ ก โ๋ข ฅฌ๋ๅ ฏฎซฅฉ 
foot_br = ""         && ญไฎเฌ ๆจฎญญ ๏ แโเฎช  BROWSE

rt = 0
dr = 1
pageLen = 0

FOR i = 1 TO ln_br0
  pageLen = pageLen+1
  IF stop_ .AND. rt = 0
    rt = INKEY()
    IF rt > 0
      IF c_t = SYS(15, chr_type, CHR(rt)) = "*"
        RETURN rt
      ENDIF
    ENDIF
  ENDIF
  fnd = SEEK(lc+STR(CODE,7),"LD")
  s0 = Br_Line3()
  s1 = STR(RECNO(),9)+CHR(13)
  s2 = IIF(fnd,STR(RECNO("LD"),9), SPACE(9))+CHR(13)
  s4 = Br_Line2()+CHR(13)
  s3 = Br_Line1()+" "+s4
  IF dr = 1
    foot_br = foot_br+s0
    recs_m  = recs_m+s1
    recs_c  = recs_c+s2
    imag_br = imag_br+s3
    vals_br = vals_br+s4
  ELSE
    foot_br = s0+foot_br
    recs_m  = s1+recs_m
    recs_c  = s2+recs_c
    imag_br = s3+imag_br
    vals_br = s4+vals_br
    fl_br0  = fl_br0+1
  ENDIF
  SKIP dr
  IF EOF() .OR. .NOT. EMPTY(keyClass) .AND. CLASS > keyClass
    s1 = VAL(ALLTRIM(LEFT(recs_m,9)))
    GO s1
    dr = -1
    SKIP dr
  ENDIF
  IF BOF() .OR. .NOT. EMPTY(keyClass) .AND. CLASS < keyClass
    EXIT
  ENDIF
ENDFOR

RETURN rt

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                    เฎฃเ ฌฌ  ฏเฎเจแฎขชจ BROWSE                        บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Draw_Br
PRIVATE i

*l_br0  = 3           && ฅเข ๏ แโเฎช  BROWSE
*c_br0  = 2           && ฅเข ๏ ฏฎงจๆจ๏ BROWSE
*ln_br0 = WROWS()-4   && ฎซจ็ฅแโขฎ แโเฎช BROWSE

*fl_br0 = 1           && “ช ง โฅซ์ BROWSE

*recs_m = ""          && ฎฌฅเ  ง ฏจแฅฉ ฎแญฎขญฎฃฎ ไ ฉซ 
*recs_c = ""          && ฎฌฅเ  ง ฏจแฅฉ แข๏ง ญญฎฃฎ ไ ฉซ 
*imag_br = ""         && เฅคแโ ขซฅญจฅ แโเฎช BROWSE
*vals_br = ""         && ญ ็ฅญจ๏ ฎกเ ก โ๋ข ฅฌ๋ๅ ฏฎซฅฉ 

@ l_br0, c_br0 CLEAR TO l_br0+ln_br0-1, WCOLS()-3 COLOR SCHEME 1

FOR i = 1 TO ln_br0
  @ l_br0+i-1, c_br0 SAY Get_Part(imag_br,i)
ENDFOR
i = l_br0+fl_br0-1
@ i, c_br0 FILL TO i, WCOLS()-3 COLOR SCHEME 1

DO Draw_Foot

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                    เฎฃเ ฌฌ  ข๋คฅซฅญจ๏ ํซฅฌฅญโ .                      บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Get_Part
PARAMETERS shab, n_part
PRIVATE i0, i1

IF n_part = 1
  i0 = 1
ELSE
  i0 = AT(CHR(13), shab, n_part-1)+1
ENDIF
i1 = AT(CHR(13), shab, n_part)

RETURN SUBSTR(shab, i0, i1-i0)

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ           เฎฃเ ฌฌ  ฏฎจแช  ฏฎงจๆจจ จ ขขฎค  ขฅซจ็จญ๋ ง ๏ขชจ            บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Locator
PARAMETERS l001, c001, ln

PRIVATE prf_flt, is_ins, c_flt, chr_type, c, c_c, c_t, i
PRIVATE prf_rem, rec_rem, is_sto, ex_req, seek_m, mss, w_sav

is_ins = INSMODE()   && ฅฆจฌ ขแโ ขช /ง ฌฅญ 
is_sto = .T.

*
*   "*" - ชฎค๋, แฎฎโขฅโแโขใ๎้จฅ ฏฅ็ โญ๋ฌ แจฌขฎซ ฌ
*
chr_type = SPACE(31)+     REPLICATE("*",126-31 )+ ;
           SPACE(127-126)+REPLICATE("*",175-127)+ ;
           SPACE(223-175)+REPLICATE("*",241-223)+ ;
           SPACE(255-223)

chr_type = STUFF(chr_type,   9, 1, "F")  &&  Tab - ๏ชฎก๋ ไใญชๆ. ชซ ขจ่ 
*
*   "I" - ชฎค๋, แฎฎโขฅโแโขใ๎้จฅ แจฌขฎซ ฌ ขญใโเฅญญฅฉ ญ ขจฃ ๆจจ
*
chr_type = STUFF(chr_type,   1, 1, "I")  &&  Home
chr_type = STUFF(chr_type,   4, 1, "I")  &&  ‘โเฅซช  ขฏเ ขฎ
chr_type = STUFF(chr_type,   6, 1, "I")  &&  End
chr_type = STUFF(chr_type,   7, 1, "I")  &&  Del
chr_type = STUFF(chr_type,  10, 1, "I")  &&  Ctrl-Enter
chr_type = STUFF(chr_type,  13, 1, "I")  &&  Enter
chr_type = STUFF(chr_type,  19, 1, "I")  &&  ‘โเฅซช  ขซฅขฎ
chr_type = STUFF(chr_type,  22, 1, "I")  &&  Ins
chr_type = STUFF(chr_type, 127, 1, "I")  &&  BackSpace

*
*   "E" - ชฎค๋, แฎฎโขฅโแโขใ๎้จฅ แจฌขฎซ ฌ ขญฅ่ญฅฉ ญ ขจฃ ๆจจ ( ฏฎ ก งฅ )
*

chr_type = STUFF(chr_type,  3, 1, "E")  &&  PgDn
chr_type = STUFF(chr_type,  5, 1, "E")  &&  ‘โเฅซช  ขขฅเๅ
chr_type = STUFF(chr_type, 18, 1, "E")  &&  PgUp
chr_type = STUFF(chr_type, 24, 1, "E")  &&  ‘โเฅซช  ขญจง

prf_flt = ""    && เฅไจชแ
c_flt   = 0     && ’ฅชใ้ ๏ ฏฎงจๆจ๏ ชใเแฎเ 

ex_req = "exit"

seek_m  = "nothing"

DO WHILE .T.
  prf_rem = prf_flt
  w_sav = WOUTPUT()
  ACTIVATE SCREEN
  @ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 2
  SHOW MENU Stock
  ACTIVATE WINDOW (w_sav)
  
  rec_rem = Get_Part(recs_m, fl_br0)
  rec_rem = VAL(ALLTRIM(rec_rem))

  @ l001, c001 SAY PADR(prf_flt, ln, "ฑ") COLOR SCHEME 1
  @ l001, c001+c_flt SAY ""

  c_c = INKEY(0,"M")            && ฎค แจฌขฎซ 
  IF c_c = 94                   && Ctrl-F1
    c_c = -21
  ENDIF
  IF c_c = 151 .AND. LASTKEY() # 151  && €ฃ ! จขฎโญฎฅ!
    IF MROW("") = 0
      c_c = -9                      && ฎคแฎข๋ข ฅฌ F10
    ELSE
      IF MCOL() < 0                 && ญฅ ฎชญ ...
        LOOP
      ENDIF
      c = MROW()
      DO CASE
      CASE c = 2                    && ฎคแฎข๋ข ฅฌ PgUp
        c_c = 3
      CASE c = WROWS()-2            && ฎคแฎข๋ข ฅฌ PgDn
        c_c = 18
      CASE BETWEEN(c, 3, WROWS()-3) &&  ไฎเโฎ็ชฅ...
        IF c = l_br0+fl_br0-1
          c_c = 13                  && Enter
        ELSE
          i = l_br0+fl_br0-1
          @ i, c_br0 FILL TO i, WCOLS()-3 COLOR SCHEME 13
          fl_br0 = c-l_br0+1
          i = l_br0+fl_br0-1
          @ i, c_br0 FILL TO i, WCOLS()-3 COLOR SCHEME 1
          DO Draw_Foot
          LOOP
        ENDIF
      OTHERWISE                     && จฌฎ ไฎเโฎ็ชจ...
        LOOP
      ENDCASE
    ENDIF
  ENDIF
  IF c_c >= 0
    c = CHR(c_c)                && ‘จฌขฎซ
    c_t = SYS(15, chr_type, c)  && ’จฏ แจฌขฎซ 
  ENDIF
  DO CASE
  CASE c_c < 0 .OR. c_c = 28 .OR. c_t = "F"  && ”ใญชๆจฎญ ซ์ญ ๏ ชซ ขจ่ 
    IF c_c = -9
      ACTIVATE MENU Stock
    ENDIF
    DO CASE
    CASE c_c = 28 .OR. seek_m = "l_sum"     && F1 - แใฌฌ  ฏฎ แฏจแชใ
      seek_m = "nothing"
      = Go_Base(fl_br0)
      DO Lst_Sum WITH LIST_TIT.LST_NUM
      LOOP
    CASE c_c = -8 .OR. seek_m = "p_rep"     &&  แฏเ ขช  ฏฎ ฏฎงจๆจจ
      = Go_Base(fl_br0)
      seek_m = WOUTPUT()
      DO Move_S WITH CODE
      ACTIVATE SCREEN
      @ 0, 0 SAY SPACE(WCOLS()) COLOR (SCHEME(1,2))
      SHOW MENU STOCK
      ACTIVATE WINDOW (seek_m)
      seek_m = "nothing"
      LOOP
    CASE c_c = -5 .OR. seek_m = "cor_note"       && F6 - ฏเจฌฅ็ ญจฅ ช ชฎเฏใแใ
      = Go_Base(fl_br0)
      seek_m = WOUTPUT()
      DO Cor_Note
      ACTIVATE SCREEN
      @ 0, 0 SAY SPACE(WCOLS()) COLOR (SCHEME(1,2))
      SHOW MENU STOCK
      ACTIVATE WINDOW (seek_m)
      seek_m = "nothing"
      LOOP
    CASE seek_m = "period"                      && ‘เฎช ฏฎแโ ขชจ
      = Go_Base(fl_br0)
      seek_m = WOUTPUT()
      DO Time_Inc WITH STOCK.CODE
      ACTIVATE SCREEN
      @ 0, 0 SAY SPACE(WCOLS()) COLOR (SCHEME(1,2))
      SHOW MENU STOCK
      ACTIVATE WINDOW (seek_m)
      seek_m = "nothing"
      LOOP
    CASE seek_m = "Rest_DMS"       && แโ โชจ ‘
      = Go_Base(fl_br0)
      seek_m = WOUTPUT()
      DO Rest_DMS WITH STOCK.CODE
      ACTIVATE SCREEN
      @ 0, 0 SAY SPACE(WCOLS()) COLOR (SCHEME(1,2))
      SHOW MENU STOCK
      ACTIVATE WINDOW (seek_m)
      seek_m = "nothing"
      LOOP
    CASE seek_m = "bPrice"                      &&  ชใฏฎ็ญ๋ฅ ๆฅญ๋
      = Go_Base(fl_br0)
      seek_m = WOUTPUT()
      DO V_B_Prc WITH STOCK.CODE
      ACTIVATE SCREEN
      @ 0, 0 SAY SPACE(WCOLS()) COLOR (SCHEME(1,2))
      SHOW MENU STOCK
      ACTIVATE WINDOW (seek_m)
      seek_m = "nothing"
      LOOP
    CASE c_c = -4 .OR. seek_m = "pr_manager"     && F5 - productmanager
      = Go_Base(fl_br0)
      seek_m = WOUTPUT()
      DO Autor WITH CODE
      ACTIVATE SCREEN
      @ 0, 0 SAY SPACE(WCOLS()) COLOR (SCHEME(1,2))
      SHOW MENU STOCK
      ACTIVATE WINDOW (seek_m)
      seek_m = "nothing"
      LOOP
    CASE c_c = -1 .OR. seek_m = "list"      && F2 - ฏฅเฅๅฎค ช แฏจแชใ
      ex_req = "list"
      EXIT
    CASE c_c = -2 .OR. seek_m = "refuse"    && F3 - ข๋แฎช ๏ ๆฅญ 
      seek_m = "nothing"
      = Go_Base(fl_br0)
      DO Ref_Qnt WITH 1, IIF(LIST_TIT.IS_USD, STOCK.PRICE, STOCK.PRICE_R)
      LOOP
    CASE c_c = -3 .OR. seek_m = "new_info"  && F4 - จญไฎเฌ. ฎ ฏฎงจๆจจ
      seek_m = "nothing"
      = Go_Base(fl_br0)
      DO Sto_Add WITH CODE, .T.
      LOOP
    CASE c_c = 9 .OR. seek_m = "dem_line"  && Tab - ข แฏเฎแ
      seek_m = "nothing"
      = Go_Base(fl_br0)
      DO Dem_Line WITH CODE
      LOOP
    CASE seek_m = "ClearClass"  && ็จแโช  ชซ แแ 
      seek_m = "nothing"
      = Go_Base(fl_br0)
      SET ORDER TO TAG NAME
      keyClass = ""
      ClassName = PADR("แฅ ชซ แแ๋", LEN(ClassName))
      GO TOP
      @ 2, 8 SAY ClassName
      prf_flt = ""
      c_flt = 0
      DO Md_Image
      DO Draw_Br
      LOOP
    CASE seek_m = "GetClass"  && ๋กฎเ ชซ แแ 
      seek_m = "nothing"
      = Go_Base(fl_br0)
      DO Ch_Class WITH keyClass, ClassName
      IF .NOT. EMPTY(keyClass)
        SET ORDER TO TAG CLASS
        IF .NOT. SEEK(keyClass)
          DIMENSION mss[3]
          mss[1] = ""
          mss[2] = CHR(0)+"๋กเ ญญ๋ฉ  ฌจ ชซ แแ ฏใแโ..."
          mss[3] = ""
          DO Out_Mess WITH 7, "mss"
          SET ORDER TO TAG NAME
          keyClass = ""
          ClassName = PADR("แฅ ชซ แแ๋", LEN(ClassName))
          GO TOP
        ENDIF
      ENDIF
      @ 2, 8 SAY ClassName
      prf_flt = ""
      c_flt = 0
      DO Md_Image
      DO Draw_Br
      LOOP
    CASE c_c = -21 .OR. seek_m = "bron"      && เฎญ์
      seek_m = "nothing"
      = Go_Base(fl_br0)
      DO View_Br WITH CODE
      LOOP
    CASE seek_m = "Add_Sto"    && ฎฏ. ญฎฌฅญชซ โใเ 
      seek_m = "nothing"
      DO Add_Sto WITH STR(LIST_TIT.LST_NUM, 6)
      LOOP
    CASE c_c = -6 .OR. seek_m = "sub_seek"  && F7 - ชฎญโฅชแโญ๋ฉ ฏฎจแช
      seek_m = "nothing"
      SELECT STOCK
      DO C_Text WITH "S"
      LOOP
    CASE seek_m = "dig_seek"  && ฏฎจแช ฏฎ ๆจไเฎขฎฉ ็ แโจ
      SELECT STOCK
      DO C_Text WITH "D"
      seek_m = "nothing"
      LOOP
    CASE seek_m = "rep_seek"  && F9 - ฏฎขโฎเ ฏฎจแช 
      seek_m = "nothing"
      SELECT STOCK
      DO C_Text WITH "R"
      LOOP
    CASE c_c = -7 .OR. seek_m = "cha_seek"  && F8 - ฏฎจแช ง ฌฅญ๋
      seek_m = "nothing"
      = Go_Base(fl_br0)
      DO C_Text WITH "A"
      LOOP
    CASE seek_m = "exit"                    && ๋ๅฎค
      EXIT
    ENDCASE
  CASE c_t = "*"                && ฅ็ โญ๋ฉ แจฌขฎซ
    prf_flt = LEFT(prf_flt, c_flt)+c+  ;
              SUBSTR(prf_flt, c_flt+IIF(is_ins,1,2))
    prf_flt = LEFT(prf_flt, ln)
    c_flt = c_flt+1
    c_flt = MIN(c_flt, ln)
  CASE c_t = "E"                &&  ขจฃ ๆจ๏ ฏฎ ก งฅ
    DO CASE
    CASE c_c = 5                && ‘โเฅซช  ขขฅเๅ
      DO Move_Up
    CASE c_c = 24               && ‘โเฅซช  ขญจง
      DO Move_Dn
    CASE c_c = 18               && PgUp
      DO Page_Up
    CASE c_c = 3                && PgDn
      DO Page_Dn
    ENDCASE
  CASE c_t = "I"                && ญใโเฅญญ๏๏ ญ ขจฃ ๆจ๏
    DO CASE
    CASE c_c = 7                && Del
      prf_flt = LEFT(prf_flt, c_flt)+SUBSTR(prf_flt, c_flt+2)
    CASE c_c = 127              && BackSpace
      prf_flt = LEFT(prf_flt, c_flt-1)+SUBSTR(prf_flt, c_flt+1)
      c_flt = MAX(c_flt-1,0)
    CASE c_c = 19               && ‘โเฅซช  ขซฅขฎ
      c_flt = MAX(c_flt-1,0)
    CASE c_c = 4                && ‘โเฅซช  ขฏเ ขฎ
      c_flt = MIN(c_flt+1,LEN(prf_flt))
    CASE c_c = 22               && Ins
      is_ins = .NOT. is_ins
      = INSMODE(is_ins)
    CASE c_c = 10               && Ctrl-Enter
      = Go_Base(fl_br0)
      prf_flt = ALLTRIM(NAME)
      prf_sav = PREFIX
      c_flt = LEN(prf_flt)
    CASE c_c = 13               && Enter
      DO Get_Qnt
      LOOP
    ENDCASE
  OTHERWISE
    EXIT
  ENDCASE
  DO Seek_    && ฎจแช ข ก งฅ
ENDDO

s = Get_Part(recs_c, fl_br0)
IF .NOT. EMPTY(s)
  ent_rec = VAL(ALLTRIM(s))
ELSE
  ent_rec = 0
ENDIF

RETURN ex_req

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ     เฎฃเ ฌฌ  ไฎเฌจเฎข ญจ๏ แชซ คแชฎฉ ็ แโจ ฎกเ ง  แโเฎชจ BROWSE       บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Br_Line1
PRIVATE s, tmpNm
*s = An_Ex()+" "+PREFIX+" "+NAME+" "+Show_Rest(1)+ " "
s = " "
*IF is_cor
*  DO CASE
*  CASE SUBJECT.DEP_COM = "1"
*    s = "ฑ"
*  CASE SUBJECT.DEP_COM = "2"
*    s = "Û"
*  ENDCASE
*ENDIF
IF .NOT. EMPTY(SUBJECT.NODISC)
  s = "Û"
ENDIF

IF EMPTY(SUBSTR(NAME, 26))
  m.tmpNm = LEFT(NAME, 25)
ELSE
  m.tmpNm = LEFT(NAME, 24)+""
ENDIF
s = s+IS_CHANGE+" "+PREFIX+" "+m.tmpNm+" "+PRODUCER+" "+Show_Rest(1)+ " "
IF LIST_TIT.IS_USD
  s = s+IIF(PRICE = 0, SPACE(10), STR(PRICE,10,l_mnt) )
ELSE
  s = s+IIF(PRICE_R = 0, SPACE(12), STR(PRICE_R,12,l_mnt) )
ENDIF

RETURN s

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ       เฎฃเ ฌฌ  ไฎเฌจเฎข ญจ๏ ็ แโจ ง ช ง  ฎกเ ง  แโเฎชจ BROWSE        บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Br_Line2
PRIVATE s
IF fnd
  s = IIF(LD.QNT=0, "      ", STR(LD.QNT,6) )
  IF LIST_TIT.IS_USD
    s = s+IIF(LD.USD_PRICE=0, "           ", STR(LD.USD_PRICE,11,l_mnt) )
  ELSE
    s = s+IIF(LD.SAL_PRICE=0, "             ", STR(LD.SAL_PRICE,13,l_mnt) )
  ENDIF
ELSE
  s = IIF(LIST_TIT.IS_USD, SPACE(17), SPACE(19))
ENDIF
RETURN s

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ         เฎฃเ ฌฌ  จญไฎเฌ ๆจฎญญฎฉ ็ แโจ ฎกเ ง  แโเฎชจ BROWSE           บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Br_Line3
PRIVATE s, s0, s1, tmpC, tmpQnt, s_sav
IF mode_fil
  s0 = 0
  IF SEEK(STR(ACCOUNT.CUS_CODE,5)+STR(STOCK.CODE,7), "FILIAL")
    s0 = FILIAL.QNT
  ENDIF
  s0 = "  ซ. ข ไจซ.:"+STR(s0,6)
ELSE
  IF STOCK.QNT_STORE = 0
    s0 = "      "
  ELSE
    IF STOCK.QNT_STORE = -99999
      s0 = "…แโ์"
    ELSE
      s0 = STR(STOCK.QNT_STORE,6)
    ENDIF
  ENDIF
  s0 = "  ฎฏ. ญ ซจ็จฅ:"+s0
ENDIF
s = "€คเ.:"+TRANSFORM(STOCK.ADDRESS,Addr_Pic())+"  ซจ็จฅ:"+STR(STOCK.QNT,6)+  ;
    " ใฏซฅญฎ "+STR(STOCK.QNT+STOCK.QNT_SAVE-STOCK.QNT_REP,6)+  ;
    s0+" เฎญ์"+STR(STOCK.QNT_SAVE,6)+CHR(13)
s1 = ""
IF is_cor
  s1 = SPACE(FSIZE("NAME","CORPUS"))
  IF SEEK(STOCK.CODE, "SUBJECT")
    IF FOUND("CORPUS")
      s1 = CORPUS.NAME
    ENDIF
  ENDIF
ENDIF
s = s+s1+CHR(13)
*s = s+STR(STOCK.QNT_PACK,6)+CHR(13)
IF USED("DMS")
*  m.tmpC = ALLTRIM(STR(STOCK.CODE))
*  m.tmpC = PADR(m.tmpC, FSIZE("CODE", "DMS"))
  m.tmpQnt = 0
*  IF SEEK(m.tmpC, "DMS")
  IF SEEK(STOCK.CODE, "DMS")
    m.s_sav = SELECT()
    SELECT DMS
    SCAN REST WHILE CODE = STOCK.CODE
      IF .NOT. EMPTY(m.mtk_period)
        IF QNT = STOCK.QNT .AND. ALLTRIM(SYS(15, m.lwr, DMS.SUP_DATE)) = m.mtk_period
          LOOP
        ENDIF
      ENDIF
      m.tmpQnt = m.tmpQnt+DMS.QNT
    ENDSCAN
    SELECT (m.s_sav)
  ENDIF
  s = s+STR(MIN(m.tmpQnt, 999999),6)+CHR(13)
ELSE
  s = s+CHR(13)
ENDIF
RETURN s

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ               เฎฃเ ฌฌ  ฏฅเฅฌฅ้ฅญจ๏ ญ  แโเฎชใ ขขฅเๅ                   บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Move_Up
PRIVATE i, s0, s1, s2, s2, s3, fnd

i = l_br0+fl_br0-1
@ i, c_br0 FILL TO i, WCOLS()-3 COLOR SCHEME 13

IF fl_br0 > 1
  fl_br0 = fl_br0-1
ELSE
*  DO Go_Base WITH 1
  = Go_Base(1)
  SKIP -1
  IF BOF() .OR. .NOT. EMPTY(keyClass) .AND. keyClass > CLASS
    IF BOF()
      GO TOP
    ELSE
      SKIP -1
    ENDIF
  ELSE
    fnd = SEEK(lc+STR(CODE,7),"LD")
    s0 = Br_Line3()
    s1 = STR(RECNO(),9)+CHR(13)
    s2 = IIF(fnd,STR(RECNO("LD"),9), SPACE(9))+CHR(13)
    s4 = Br_Line2()+CHR(13)
    s3 = Br_Line1()+" "+s4
    foot_br = s0+LEFT(foot_br,LEN(foot_br)-LEN(s0))
    recs_m  = s1+LEFT(recs_m,LEN(recs_m)-LEN(s1))
    recs_c  = s2+LEFT(recs_c,LEN(recs_c)-LEN(s2))
    imag_br = s3+LEFT(imag_br,LEN(imag_br)-LEN(s3))
    vals_br = s4+LEFT(vals_br,LEN(vals_br)-LEN(s4))
*    SCROLL 3, 2, WROWS()-IIF(sale_mode = "Mitracon", 4, 3), WCOLS()-3, -1
    SCROLL l_br0, 2, l_br0+ln_br0-1, WCOLS()-3, -1 && ชซ แแจไจช ๆจ๏ 29.10.2005
    @ l_br0, 2 SAY LEFT(s3,LEN(s3)-1)
  ENDIF
ENDIF
i = l_br0+fl_br0-1
@ i, c_br0 FILL TO i, WCOLS()-3 COLOR SCHEME 1
DO Draw_Foot
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ               เฎฃเ ฌฌ  ฏฅเฅฌฅ้ฅญจ๏ ญ  แโเฎชใ ขญจง                    บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Move_Dn
PRIVATE i, s1, s2, s2, s3, fnd

i = l_br0+fl_br0-1
@ i, c_br0 FILL TO i, WCOLS()-3 COLOR SCHEME 13

IF fl_br0 < pageLen
  fl_br0 = fl_br0+1
ELSE
  = Go_Base(pageLen)
  SKIP
  IF EOF() .OR. .NOT. EMPTY(keyClass) .AND. keyClass < CLASS
    IF EOF()
      GO BOTTOM
    ELSE
      SKIP -1
    ENDIF
  ELSE
    fnd = SEEK(lc+STR(CODE,7),"LD")
    s0 = Br_Line3()
    s1 = STR(RECNO(),9)+CHR(13)
    s2 = IIF(fnd,STR(RECNO("LD"),9), SPACE(9))+CHR(13)
    s4 = Br_Line2()+CHR(13)
    s3 = Br_Line1()+" "+s4
    foot_br = RIGHT(foot_br,LEN(foot_br)-LEN(s0))+s0
    recs_m  = RIGHT(recs_m,LEN(recs_m)-LEN(s1))+s1
    recs_c  = RIGHT(recs_c,LEN(recs_c)-LEN(s2))+s2
    imag_br = RIGHT(imag_br,LEN(imag_br)-LEN(s3))+s3
    vals_br = RIGHT(vals_br,LEN(vals_br)-LEN(s4))+s4
*    SCROLL 3, 2, WROWS()-IIF(sale_mode = "Mitracon", 4, 3), WCOLS()-3, 1
    SCROLL l_br0, 2, l_br0+ln_br0-1, WCOLS()-3, 1  && ชซ แแจไจช ๆจ๏ 29.10.2005
    @ l_br0+ln_br0-1, 2 SAY LEFT(s3,LEN(s3)-1)
  ENDIF
ENDIF
i = l_br0+fl_br0-1
@ i, c_br0 FILL TO i, WCOLS()-3 COLOR SCHEME 1
DO Draw_Foot
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                         ฅเฅๅฎค  ข ก งฅ ค ญญ๋ๅ                        บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Go_Base
PARAMETER point
PRIVATE s, is_conn

s = Get_Part(recs_m, point)
GO VAL(ALLTRIM(s))

s = Get_Part(recs_c, point)
is_conn = .F.
IF .NOT. EMPTY(s)
  is_conn = .T.
  GO VAL(ALLTRIM(s)) IN LD
ENDIF

RETURN is_conn

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                     เฎฃเ ฌฌ  ญ  แโเ ญจๆใ ขญจง                        บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Page_Dn
PRIVATE i, fnd

i = l_br0+fl_br0-1
@ i, c_br0 FILL TO i, WCOLS()-3 COLOR SCHEME 13

IF fl_br0 # pageLen
  fl_br0 = pageLen
  i = l_br0+fl_br0-1
  @ i, c_br0 FILL TO i, WCOLS()-3 COLOR SCHEME 1
ELSE
*  DO Go_Base WITH ln_br0
  = Go_Base(pageLen)
  DO Md_Image
  DO Draw_Br
  i = l_br0+fl_br0-1
  @ i, c_br0 FILL TO i, WCOLS()-3 COLOR SCHEME 13
  fl_br0 = pageLen
  i = l_br0+fl_br0-1
  @ i, c_br0 FILL TO i, WCOLS()-3 COLOR SCHEME 1
ENDIF
DO Draw_Foot
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                     เฎฃเ ฌฌ  ญ  แโเ ญจๆใ ขญจง                        บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Page_Up
PRIVATE i, fnd

i = l_br0+fl_br0-1
@ i, c_br0 FILL TO i, WCOLS()-3 COLOR SCHEME 13

IF fl_br0 # 1
  fl_br0 = 1
  i = l_br0+fl_br0-1
  @ i, c_br0 FILL TO i, WCOLS()-3 COLOR SCHEME 1
ELSE
*  DO Go_Base WITH 1
  = Go_Base(1)
  FOR i = 1 TO pageLen-1
    SKIP -1
    IF BOF() .OR. .NOT. EMPTY(keyClass) .AND. keyClass > CLASS
      IF BOF()
        GO TOP
      ELSE
        SKIP 1
      ENDIF
      EXIT
    ENDIF
  ENDFOR
  DO Md_Image
  DO Draw_Br
ENDIF
DO Draw_Foot
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                  เฎฃเ ฌฌ  ฏฅเฅฌฅ้ฅญจ๏ ฏฎ ก งฅ                        บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Seek_
PRIVATE fnd, i, rc, rc_s, new_pos

new_pos = 0

IF prf_rem == prf_flt
  RETURN
ENDIF

fnd = SEEK(keyClass+prf_flt)
IF fnd
  rc = RECNO()
  IF .NOT. is_sto
    HIDE WINDOW Not_Sto
    is_sto = .T.
  ENDIF
ELSE
  rc = RECNO(0)
  DO CASE
  CASE rc <= 0
    GO TOP
    rc = RECNO()
  CASE rc > RECCOUNT()
    GO BOTTOM
    rc = RECNO()
  OTHERWISE
    GO rc
  ENDCASE
  IF .NOT. EMPTY(keyClass)
    DO CASE
    CASE CLASS < keyClass
      SEEK keyClass
    CASE CLASS > keyClass
      SET ORDER TO TAG CLASS DESCENDING
      SEEK keyClass
      SET ORDER TO TAG CLASS ASCENDING
    ENDCASE
    rc = RECNO()
  ENDIF
  IF is_sto
    SHOW WINDOW Not_Sto
    is_sto = .F.
  ENDIF
ENDIF

IF rec_rem = rc
  RETURN
ENDIF

rc_s = STR(rc,9)+CHR(13)
i    = AT(rc_s, recs_m)
IF i # 0
  new_pos = (i+9)/10
ELSE
  new_pos = 1
  FOR i = 1 TO 4
    SKIP -1
    IF BOF() .OR. .NOT. EMPTY(keyClass) .AND. keyClass > CLASS
      IF BOF()
        GO TOP
      ELSE
        SKIP
      ENDIF
      EXIT
    ENDIF
    new_pos = new_pos+1
  ENDFOR
  DO Md_Image
  DO Draw_Br
  new_pos = fl_br0+new_pos-1
ENDIF

IF new_pos > 0
  i = l_br0+fl_br0-1
  @ i, c_br0 FILL TO i, WCOLS()-3 COLOR SCHEME 13
  fl_br0 = new_pos
  i = l_br0+fl_br0-1
  @ i, c_br0 FILL TO i, WCOLS()-3 COLOR SCHEME 1
ENDIF
DO Draw_Foot
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                  เฎฃเ ฌฌ  ขขฎค  ชฎซจ็ฅแโข                            บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Get_Qnt
PRIVATE q_d, q_r, q_w, p_w, q_w0, p_w0, i, q_d0, ;
        q_dem, q_ref, is_d, ref_reas

IF .NOT. is_sto
  DO Add_Pos1
  is_sto = .T.
  HIDE WINDOW Not_Sto
  prf_flt = ""
  c_flt   = 0
  RETURN
ENDIF

o_exist = Go_Base(fl_br0)
IF o_exist
  q_w =  LD.QNT
  p_w =  IIF(LIST_TIT.IS_USD, LD.USD_PRICE, LD.SAL_PRICE)
  q_d =  LD.QNT_DUE
  q_r =  LD.QNT_REQ
  is_d = LD.IS_DEF
ELSE
  q_w = 0
  p_w = 0
  q_d = 0
  q_r = 0
  is_d = " "
ENDIF
IF EMPTY(STOCK.ADDRESS)
  WAIT " €…! ใแโฎฉ  คเฅแ " WINDOW NOWAIT
ENDIF
IF .NOT. EMPTY(STOCK.NOTE)
  DO View_Nt
ENDIF
q_dem = 0
q_ref = 0
q_w0 = q_w
q_d0 = q_d
p_w0 = p_w
ref_reas = 0

i = l_br0+fl_br0-1
@ i, c_br0 FILL TO i, WCOLS()-3 COLOR (SCHEME(14,2))

ON KEY LABEL F3 DO Ref_Qnt WITH q_w, IIF(LIST_TIT.IS_USD, STOCK.PRICE, STOCK.PRICE_R)

@ l_br0+fl_br0-1, c1_pos GET q_w PICTURE "@Z 999999" VALID Exit_Qnt()
IF mode_spec
  @ l_br0+fl_br0-1, c2_pos GET p_w    ;
      WHEN q_w # 0 PICTURE pic_w VALID Exit_Prc()
ENDIF
READ VALID Corr_Base()

ON KEY LABEL F3

@ i, c_br0 FILL TO i, WCOLS()-3 COLOR SCHEME 1

c_flt   = 0

*
*   ชฎก๋ ฎกเ กฎโช ...
*

prf_flt = ""
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ          เฎฃเ ฌฌ  ฏเฎเจแฎขชจ จญไฎเฌ ๆจฎญญฎฉ แโเฎชจ                   บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Draw_Foot
PRIVATE s, tmpNm

s = Get_Part(foot_br, fl_br0*3-2)
@ WROWS()-1, 0 SAY PADC(s, WCOLS())
s = Get_Part(foot_br, fl_br0*3-1)
@ 1, WCOLS()-18 SAY s
s = Get_Part(foot_br, fl_br0*3)
@ 1, WCOLS()-38 SAY s
= Go_Base(fl_br0)
= SEEK(STOCK.PRO_CODE, "DEVELOP")
IF EMPTY(DEVELOP.PERIOD)
  @ 2, WCOLS()-5 SAY "    "
ELSE
  @ 2, WCOLS()-5 SAY "จเโ" COLOR SCHEME 7
ENDIF
DO Show_Opt WITH WROWS()-2
*IF .NOT. EMPTY(STOCK.NOTE)
*  m.s = STOCK.NOTE
*  WAIT WINDOW m.s NOWAIT
*ENDIF
IF EMPTY(SUBSTR(STOCK.NAME, 26))
  @ m.l_br0+m.ln_br0, 2 SAY REPLICATE("ฤ", WCOLS()-4)
ELSE
  @ m.l_br0+m.ln_br0, 2 SAY LEFT(STOCK.NAME, WCOLS()-18) COLOR SCHEME 1
*  @ m.l_br0+m.ln_br0, col()+1 SAY STOCK.NAME COLOR SCHEME 1
*  @ m.l_br0+m.ln_br0, col()+1 SAY STOCK.PRODUCER COLOR SCHEME 1
ENDIF
@ m.l_br0+m.ln_br0, WCOLS()-16 SAY "€เโ."
@ m.l_br0+m.ln_br0, COL() SAY Articul(STOCK.CODE) COLOR SCHEME 1

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                  เฎฃเ ฌฌ  แฌฅญ๋ เฅฆจฌ .                              บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Sw_Mod
PARAMETERS m, m_nam
PRIVATE w

w = IIF(TYPE("m_nam") = "C", m_nam, "")

seek_m = m

IF .NOT. EMPTY(w)
  DEACTIVATE MENU (w)
ENDIF

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                 เฎฃเ ฌฌ  ชฎญโฅชแโญฎฃฎ ฏฎจแช                          บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE C_Text
PARAMETERS t_seek

PRIVATE fnd, i, rc, rc_s, new_pos, tmpFnd, tmpShab

IF m.t_seek = "D"
  m.tmpFnd = .F.
  tmpShab = Get_Shab("", "ฎจแช ฏฎ ๆจไเฎขฎฉ ็ แโจ")
  IF .NOT. EMPTY(tmpShab)
    rc = Seek_Dig(tmpShab)
    IF .NOT. EMPTY(rc)
      GO rc
      m.tmpFnd = .T.
    ENDIF
  ENDIF
ELSE
  m.tmpFnd = Seek_Pos(t_seek)
ENDIF
IF m.tmpFnd
  is_sto = .T.
  HIDE WINDOW Not_Sto
  prf_flt = ""
  c_flt   = 0
  ACTIVATE WINDOW W_Seek BOTTOM
  new_pos = 0
  rc_s = STR(RECNO(),9)+CHR(13)
  i    = AT(rc_s, recs_m)
  IF i # 0
    new_pos = (i+9)/10
  ELSE
    new_pos = 1
    FOR i = 1 TO 4
      SKIP -1
      IF BOF()
        GO TOP
        EXIT
      ENDIF
      new_pos = new_pos+1
    ENDFOR
    DO Md_Image
    DO Draw_Br
  ENDIF

  IF new_pos > 1
    i = l_br0+fl_br0-1
    @ i, c_br0 FILL TO i, WCOLS()-3 COLOR SCHEME 13
    fl_br0 = new_pos
    i = l_br0+fl_br0-1
    @ i, c_br0 FILL TO i, WCOLS()-3 COLOR SCHEME 1
  ENDIF
  DO Draw_Foot
ENDIF
ACTIVATE SCREEN
@ 0, 0 SAY PADL("ฅญ๎ F10 ",WCOLS()) COLOR SCHEME 13
SHOW MENU Stock
DEACTIVATE MENU Stock
ACTIVATE WINDOW W_Seek BOTTOM
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ       ใคญ ๏ จ ขฅโขจแโ ๏ ฏเฎฃเ ฌฌ  ข๋ๅฎค  จง ฏฎซ๏ ชฎซจ็ฅแโข .         บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Exit_Qnt
PRIVATE v1, pr, delta, i, q00
IF .NOT. sale_mode = "Mitracon"
  IF q_w < 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"ฎงขเ โ ฎแใ้ฅแโขซ๏ฅโแ๏ ็ฅเฅง แฏฅๆจ ซ์ญใ๎ ฏเฎๆฅคใเใ!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
ENDIF
SHOW GETS

IF q_w = q_w0    && ฅ ฌฅญ๏ซจ!
  RETURN .T.
ENDIF

q00 = q_w
*IF q00 % MAX(1, STOCK.QNT_ROUND) # 0 .AND. q00 > 0
  q_w = Round_P(STOCK.PREFIX+" "+STOCK.NAME+" "+STOCK.PRODUCER, q00, STOCK.QNT_ROUND, STOCK.QNT_PACK, STOCK.PRICE)
*ENDIF

i = q_w-MAX(STOCK.QNT,0)-q_r  && ฅ ๅข โ ฅโ ญ  แชซ คฅ
IF i > 0 .AND. mode_defA      &&  ... จ  ขโฎฌ โจ็ฅแช ๏ ฎกเ กฎโช 
  IF mode_firm
    DO Other_F
  ENDIF
  IF i > 0
    WAIT "ฅไจๆจโ!" WINDOW NOWAIT
    IF mode_defD
      q_d = q_d+i
    ENDIF
    IF mode_defS
      q_dem = i
    ENDIF
    q_w = MAX(STOCK.QNT,0)+q_r
    is_d = " "
  ENDIF
ELSE
  IF q_w < q_w0  .AND. q_w >= 0 && “ฌฅญ์่จซจ ชฎซจ็ฅแโขฎ: ฏฎ็ฅฌใ?
    ref_reas = Reason_D()
    DO CASE
    CASE ref_reas = 1   && ๋แฎช ๏ ๆฅญ 
      q_ref = q_w0-q_w
    CASE ref_reas = 3
*      IF File_O(base_path+"ABSENT.DBF")
*        DO Absent WITH STOCK.CODE
*      ENDIF
      q_dem = q_w0-q_w
    ENDCASE
  ENDIF
ENDIF

IF .NOT. o_exist .AND. q_w # 0 .AND. p_w = 0
  p_w = IIF(LIST_TIT.IS_USD, PRICE, Round_6(PRICE_R))
  IF mode_spec
    SHOW GET p_w
  ELSE
    @ l_br0+fl_br0-1, c2_pos SAY p_w PICTURE pic_w COLOR (SCHEME(14,2))
  ENDIF
ENDIF
  
RETURN .T.

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                  เฎฃเ ฌฌ  ข๋ๅฎค  จง ฏฎซ๏ ๆฅญ๋.                       บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Exit_Prc
PRIVATE delta

IF p_w < 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"โฎ ็โฎ ง  ๆฅญ  โ ช ๏? คใฌ ฉโฅแ์!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  p_w = p_w0
  SHOW GET p_w
  RETURN .F.
ENDIF

IF .NOT. LIST_TIT.IS_USD
  p_w = Round_6(p_w)
ENDIF

SHOW GET p_w

RETURN .T.

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                    เฎฃเ ฌฌ  ชฎเเฅๆจจ ก ง๋.                           บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Corr_Base
PRIVATE fnd, s1, s2, i

*
*    ฎก ขซ๏ฅฌ ( จซจ ชฎเเฅชโจเใฅฌ ) ง ช ง
*
IF .NOT. LIST_TIT.IS_USD
  p_w = Round_6(p_w)
ENDIF

IF q_w # q_w0 .OR. q_d # q_d0 .OR. p_w # p_w0  && —โฎ-โฎ จงฌฅญจซฎแ์!
  SELECT LD
  IF .NOT. o_exist
    APPEND BLANK
    REPLACE LIST_NUM  WITH lc,             ;
            CODE      WITH STOCK.CODE,     ;
            PREFIX    WITH STOCK.PREFIX,   ;
            NAME      WITH STOCK.NAME,     ;
            PRODUCER  WITH STOCK.PRODUCER, ;
            PRO_CODE  WITH STOCK.PRO_CODE, ;
            QNT       WITH q_w,            ;
            QNT_DUE   WITH q_d,            ;
            ADDRESS   WITH STOCK.ADDRESS,  ;
            DEF_PRICE WITH STOCK.PRICE_R,  ;
            USD_DEF   WITH STOCK.PRICE,    ;
            IS_SPEC   WITH IIF(IIF(LIST_TIT.IS_USD, p_w = STOCK.PRICE, p_w = ROUND_6(STOCK.PRICE_R)), " ", "*"), ;
            SAL_PRICE WITH IIF(LIST_TIT.IS_USD, 0, p_w), ;
            USD_PRICE WITH IIF(LIST_TIT.IS_USD, p_w, 0)
  ELSE
    REPLACE QNT       WITH q_w,  ;
            QNT_DUE   WITH q_d,  ;
            IS_SPEC   WITH IIF(IIF(LIST_TIT.IS_USD, p_w = USD_DEF, p_w = ROUND_6(DEF_PRICE)), " ", "*"), ;
            SAL_PRICE WITH IIF(LIST_TIT.IS_USD, 0, p_w), ;
            USD_PRICE WITH IIF(LIST_TIT.IS_USD, p_w, 0)
  ENDIF

  SELECT STOCK

*
*    ฎคแโ ขซ๏ฅฌ งญ ็ฅญจ๏ ่ กซฎญฎข
*
  SELECT STOCK
  fnd = SEEK(lc+STR(CODE,7),"LD")
  s1 = IIF(fnd,STR(RECNO("LD"),9), SPACE(9))
  s2 = Br_Line2()
  i = IIF(fl_br0 = 1, 1, AT(CHR(13), recs_c, fl_br0-1)+1 )
  recs_c = STUFF(recs_c,i,LEN(s1), s1)
  i = IIF(fl_br0 = 1, 1, AT(CHR(13), vals_br, fl_br0-1)+1 )
  vals_br = STUFF(vals_br,i,LEN(s2), s2)

  REPLACE LIST_TIT.WHO_LET WITH 0

*  sum_list = sum_list-q_w0*p_w0+q_w*p_w
*  @ 1, 49 SAY STR(sum_list,12,l_mnt) COLOR SCHEME 1

  DO Show_Opt WITH WROWS()-2
  p_w = IIF(LIST_TIT.IS_USD, LD.USD_PRICE, LD.SAL_PRICE)


ENDIF  

IF q_dem # 0     && ‘ฏเฎแ
  DO To_Dem0 WITH q_dem
ENDIF
IF q_ref # 0    && โช ง ฏฎ ข๋แฎชฎฉ ๆฅญฅ
  DO To_Refus WITH q_ref
ENDIF

IF mode_spec
  SHOW GET p_w
ELSE
  @ l_br0+fl_br0-1, c2_pos SAY p_w PICTURE pic_w COLOR (SCHEME(14,2))
ENDIF

RETURN .T.

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ             เฎฃเ ฌฌ  ง ญฅแฅญจ๏ ง ฏจแจ ข แฏเฎแ จง แฏจแช .             บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE To_Dem0
PARAMETER q    && ฎซจ็ฅแโขฎ
PRIVATE sel_sav, p_w1

IF .NOT. USED("DEMAND")
  RETURN
ENDIF

IF LIST_TIT.IS_USD
  p_w1 = IIF(p_w = 0, STOCK.PRICE, p_w)
ELSE
  p_w1 = IIF(p_w = 0, STOCK.PRICE_R, p_w)
ENDIF
sel_sav = SELECT()
SELECT DEMAND
APPEND BLANK
REPLACE STO_CODE WITH STOCK.CODE,      ;
        PREFIX   WITH STOCK.PREFIX,    ;
        NAME     WITH STOCK.NAME,      ;
        PRODUCER  WITH STOCK.PRODUCER, ;
        PRO_CODE  WITH STOCK.PRO_CODE, ;
        DATE     WITH DATE(),          ;
        QTY      WITH q,               ;
        PRICE    WITH IIF(LIST_TIT.IS_USD, 0, p_w1), ;
        PRICE_D  WITH IIF(LIST_TIT.IS_USD, p_w1, 0), ;
        CLI_CODE WITH LIST_TIT.CLI_CODE, ;
        CLIENT   WITH LIST_TIT.LST_NAME, ;
        WHO      WITH user,            ;
        WHERE    WITH arm
IF USED("DEM_LINK")
  SELECT DEM_LINK
  GO TOP
  IF .NOT. EMPTY(LST_NUM) .OR. BOF() .OR. EOF()
    APPEND BLANK
  ENDIF
  REPLACE LST_NUM WITH LIST_TIT.LST_NUM, LINK WITH RECNO("DEMAND")
ENDIF
SELECT (sel_sav)
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ             เฎฃเ ฌฌ  ง ญฅแฅญจ๏ ฎโช ง  ฏฎ ข๋แฎชฎฉ ๆฅญฅ.               บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE To_Refus
PARAMETER q    && ฎซจ็ฅแโขฎ
PRIVATE sel_sav

IF .NOT. USED("REFUSAL")
  RETURN
ENDIF

sel_sav = SELECT()
SELECT 0
SELECT REFUSAL
APPEND BLANK
REPLACE STO_CODE WITH STOCK.CODE,   ;
        PREFIX   WITH STOCK.PREFIX, ;
        NAME     WITH STOCK.NAME,   ;
        PRODUCER WITH STOCK.PRODUCER,;
        PRO_CODE WITH STOCK.PRO_CODE,;
        DATE     WITH DATE(),       ;
        QTY      WITH q,            ;
        PRICE    WITH IIF(LIST_TIT.IS_USD, 0, p_w), ;
        PRICE_D  WITH IIF(LIST_TIT.IS_USD, p_w, 0), ;
        CUS_CODE WITH LIST_TIT.CLI_CODE, ;
        CLIENT   WITH LIST_TIT.LST_NAME, ;
        WHO      WITH user,      ;
        WHERE    WITH arm
SELECT (sel_sav)
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ             เฎฃเ ฌฌ  ฎกเ กฎโชจ ็ใฆฎฉ ฏฎงจๆจจ.                        บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Add_Pos1
PRIVATE l, c, ln, wd, prf_w, nam_w, qnt_w, prc_w, ex, pic, model_w, func_w
PRIVATE next_p, rc_rem, fc, fn

= Go_Base(fl_br0)
IF mode_defD .OR. mode_defS .OR. .NOT. mode_defA  && € ฅแโ์ ซจ แฌ๋แซ ข ฎกเ กฎโชฅ?
  nam_w   = PADR(prf_flt, FSIZE("NAME", "STOCK"))
  prf_w   = prf_sav
  fc      = 1
  fn      = "        "
*  prf_w   = "    "
*  prf_w   = STOCK.PREFIX
  qnt_w   = 1
  prc_w   = 0
  model_w = SPACE(24)
  func_w  = SPACE(32)
  ex = 1
  pic = " ‘ฎๅเ ญจโ์ Ctrl-W ;\? โช ง โ์แ๏ Esc "
  wd = 64
  ln = 14
  l  = CEILING((WROWS("")-ln)/2)
  c  = FLOOR((WCOLS("")-wd)/2)
  DEFINE WINDOW Add_Pos1 FROM l, c TO l+ln-1, c+wd-1 NONE SHADOW COLOR SCHEME 13
  ACTIVATE WINDOW Add_Pos1 TOP
  @  0, 0 SAY PADC("ฎข ๏ ฏฎงจๆจ๏",WCOLS()) COLOR SCHEME 5
  @  1, 0 SAY " ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ"
  @  2, 0 SAY " ณฺฤฤ  จฌฅญฎข ญจฅ ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ      ฺฤฤฤฤฤฤฤฤฤฤฟณ"
  @  3, 0 SAY " ณณ1234 12345678901234567890123456789012345ณ ”จเฌ ณ          ณณ"
  @  4, 0 SAY " ณภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู      ภฤฤฤฤฤฤฤฤฤฤูณ"
  @  5, 0 SAY " ณ          ฎซจ็ฅแโขฎ 999999          –ฅญ  99999.99          ณ"
  @  5,53 SAY IIF(LIST_TIT.IS_USD, "$", "เ")
  @  6, 0 SAY " ณ                                                            ณ"
  @  7, 0 SAY " ณ          ฎคฅซ์  ฏฏ เ โ  123456789012345678901234          ณ"
  @  8, 0 SAY " ณ                                                            ณ"
  @  9, 0 SAY " ณ ”ใญชๆจฎญ ซ์ญ. ญ งญ ็ฅญจฅ 12345678901234567890123456789012  ณ"
  @ 10, 0 SAY " ณ                                                            ณ"
  @ 11, 0 SAY " ณ          < ‘ฎๅเ ญจโ์ Ctrl-W > < โช ง โ์แ๏ Esc >           ณ"
  @ 12, 0 SAY " ณ                                                            ณ"
  @ 13, 0 SAY " ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู"
  @ 5, 23 GET qnt_w PICTURE "@Z 999999"
  @ 5, 44 GET prc_w PICTURE pic_w
  @ 7, 28 GET model_w
  @ 9, 28 GET func_w
  @11, 12 GET ex PICTURE  "@*HT \!"+pic
  @ 3,  3 GET prf_w
  @ 3,  8 GET nam_w SIZE 1, 35 VALID Tst_Exi()
  @ 3, 52 GET fn VALID Develop() .AND. Tst_Exi()
  READ CYCLE VALID To_Base()
  RELEASE WINDOW Add_Pos1
  ACTIVATE WINDOW W_Seek
  prf_sav = "    "
ENDIF
RETURN
*
*                  ฅแโญ๋ฅ ฏเฎๆฅคใเ๋
*************************************************************************
*
*      ๋ๅฎค จง ฏฎซ๏ จฌฅญจ
*
PROCEDURE Tst_Exi
rc_rem = RECNO()
IF SEEK(nam_w+prf_w+STR(fc,5))
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"’ ช ๏ ฏฎงจๆจ๏ ใฆฅ ฅแโ์ ข ญฎฌฅญชซ โใเฅ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .T.
ENDIF
GO rc_rem
RETURN .T.
*************************************************************************
*
*      ๋ๅฎค จง กซ ญช 
*
PROCEDURE To_Base
PRIVATE s, delta

s = READKEY() % 256
IF s = 12 .OR. ex = 2
  RETURN .T.
ENDIF
rc_rem = RECNO()
IF SEEK(nam_w+prf_w)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"’ ช ๏ ฏฎงจๆจ๏ ใฆฅ ฅแโ์ ข ญฎฌฅญชซ โใเฅ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .T.
ENDIF
GO rc_rem
IF mode_defS .AND. mode_defA       &&  แฏเฎแ
  IF USED("DEMAND")
    SELECT DEMAND
    APPEND BLANK
    REPLACE PREFIX   WITH prf_w,     ;
            NAME     WITH nam_w,     ;
            PRO_CODE WITH fc,        ;
            PRODUCER WITH fn,        ;
            DATE     WITH DATE(),    ;
            QTY      WITH qnt_w,     ;
            PRICE    WITH IIF(LIST_TIT.IS_USD, 0, prc_w), ;
            PRICE_D  WITH IIF(LIST_TIT.IS_USD, prc_w, 0), ;
            CLI_CODE WITH LIST_TIT.CLI_CODE, ;
            CLIENT   WITH LIST_TIT.LST_NAME, ;
            FUNCTION WITH func_w,    ;
            AMODEL   WITH model_w,   ;
            WHO      WITH user,      ;
            WHERE    WITH arm
    IF USED("DEM_LINK")
      SELECT DEM_LINK
      GO TOP
      IF .NOT. EMPTY(LST_NUM)
        APPEND BLANK
      ENDIF
      REPLACE LST_NUM WITH LIST_TIT.LST_NUM, LINK WITH RECNO("DEMAND")
    ENDIF
    SELECT STOCK
  ENDIF
ENDIF

IF mode_defD .OR. .NOT. mode_defA        &&  คฎซฃ จซจ ง ช ง
  SELECT LD
  rc_rem = RECNO()
  SEEK lc
  LOCATE REST WHILE LIST_NUM = lc FOR NAME = nam_w .AND. PREFIX = prf_w .AND. CODE = 0
  IF .NOT. FOUND()
    APPEND BLANK
  ENDIF
  delta = IIF(LIST_TIT.IS_USD, USD_PRICE*QNT, SAL_PRICE*QNT)
  IF mode_defD .AND. mode_defA
    REPLACE LIST_NUM  WITH lc,        ;
            PREFIX    WITH prf_w,     ;
            NAME      WITH nam_w,     ;
            PRO_CODE  WITH fc,        ;
            PRODUCER  WITH fn,        ;
            QNT_DUE   WITH QNT_DUE+qnt_w,;
            IS_SPEC   WITH IIF(IIF(LIST_TIT.IS_USD, prc_w = STOCK.PRICE, Round_6(prc_w) = Round_6(STOCK.PRICE_R)), " ", "*"), ;
            SAL_PRICE WITH IIF(LIST_TIT.IS_USD, 0, Round_6(prc_w)), ;
            USD_PRICE WITH IIF(LIST_TIT.IS_USD, prc_w, 0),          ;
            FUNCTION  WITH func_w,    ;
            AMODEL    WITH model_w
  ELSE
    REPLACE LIST_NUM  WITH lc,        ;
            PREFIX    WITH prf_w,     ;
            NAME      WITH nam_w,     ;
            PRO_CODE  WITH fc,        ;
            PRODUCER  WITH fn,        ;
            QNT       WITH qnt_w,     ;
            IS_SPEC   WITH IIF(IIF(LIST_TIT.IS_USD, prc_w = STOCK.PRICE, Round_6(prc_w) = Round_6(STOCK.PRICE_R)), " ", "*"), ;
            SAL_PRICE WITH IIF(LIST_TIT.IS_USD, 0, Round_6(prc_w)), ;
            USD_PRICE WITH IIF(LIST_TIT.IS_USD, prc_w, 0),          ;
            FUNCTION  WITH func_w,    ;
            AMODEL    WITH model_w

*    sum_list = sum_list-delta+IIF(LIST_TIT.IS_USD, USD_PRICE, SAL_PRICE)*QNT 
*    ACTIVATE WINDOW L_Det_M BOTTOM
     ACTIVATE WINDOW W_Seek  BOTTOM
*    @ 1, 49 SAY STR(sum_list,12,l_mnt) COLOR SCHEME 1
  ENDIF
  IF rc_rem > 0 .AND. rc_rem <= RECCOUNT()
    GO rc_rem
  ENDIF
  SELECT STOCK
ENDIF

RETURN .T.

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ             เฎฃเ ฌฌ  ฎกเ กฎโชจ ็ใฆฎฉ ฏฎงจๆจจ.                        บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Ref_Qnt
PARAMETERS q, p

PRIVATE q_w
IF TYPE("p_w") = "U"
  PRIVATE p_w
  p_w = p
ELSE
  IF p_w = 0
    p_w = p
  ENDIF
ENDIF
q_w = q

*
*  ฅเฅฌฅญญ๋ฅ แฎแโฎ๏ญจ๏ คซ๏ ญ ขจฃ ๆจจ
*

PRIVATE stat_type     && ’จฏ ญ ขจฃ ๆจจ: 0 - ญฅแโ ญค เโญ๋ฉ;
                                        1 - กซ ญช;
                                        2 - BROWSE - โ กซจๆ ;
                                        3 - BROWSE - แฏจแฎช.
PRIVATE what_do       && ฌ๏ เฅฆจฌ .
PRIVATE menu_name     && ฌ๏  แจญๅเฎญญฎฃฎ ฌฅญ๎.
PRIVATE last_mouse    && เฅฌ๏ ฏฎแซฅคญฅฃฎ ญ ฆ โจ๏ งขฅเจญฎฉ ชญฎฏชจ.
PRIVATE win_name      && ฌ๏ ฎชญ  ( ฎชฎญ คซ๏ BROWSE ).

*
*    ฏฎซญ๏ฅฌ งญ ็ฅญจ๏ฌจ ฏฅเฅฌฅญญ๋ฅ แฎแโฎ๏ญจ๏...
*
IF .F. &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ‘ฎคฅเฆ โฅซ์ญ ๏ ็ แโ์ ฏเฎฃเ ฌฌ๋:
*

PRIVATE ex
                     &&   ก๊๏ขซ๏ฅฌ จ ง ฏฎซญ๏ฅฌ ฏฎซ๏ กซ ญช 
ex   = 1             &&
*------------------------------------------------------------------------

PUSH KEY CLEAR       &&   ขแ๏ชจฉ ฏฎฆ เญ๋ฉ แซใ็ ฉ!
*
*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ ฺฤ ฎงจๆจ๏ ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ ณ
*ณ ณ .... ....:....!....:....!....: ....:... ณ ณ
*ณ ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู ณ
*ณ      ฎซจ็ฅแโขฎ 999999   –ฅญ  99999.99      ณ
*ณ      < OK Ctrl-W > < โช ง โ์แ๏ Esc >       ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

DO Prp_Nav_2
DO D_Win_N WITH 8, 49, "โช ง ฏฎ ข๋แฎชฎฉ ๆฅญฅ"
DO Sun_Bord WITH 2, 3, 4, 45
@ 2, 5 SAY " ฎงจๆจ๏ "
@ 5, 42 SAY IIF(LIST_TIT.IS_USD, "$", "เ")
@ 3, 5 SAY STOCK.PREFIX
@ 3,10 SAY LEFT(STOCK.NAME, 25)
@ 3,36 SAY STOCK.PRODUCER
@ 5, 8 SAY "ฎซจ็ฅแโขฎ" GET q_w PICTURE "@Z 999999"
@ 5,28 SAY "–ฅญ " GET p_w PICTURE pic_w
@ 6, 8 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? โช ง โ์แ๏ Esc "
READ CYCLE

IF ex = 2
  q_w = 0
ENDIF

POP KEY
RELEASE WINDOW (win_name)

*IF q_w > 0
ENDIF &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  DO To_Refus WITH MAX(q_w,1)
  WAIT WINDOW "โช ง, OK!" NOWAIT
*ENDIF
*q = 0
SELECT LD
SELECT STOCK

*SHOW MENU STOCK
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ          เฎฃเ ฌฌ  ฏเฎแฌฎโเ  โฎฉ ฆฅ ฏฎงจๆจจ คเใฃจๅ ไจเฌ.              บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Other_F

PRIVATE r_sav, q, pr, nm, mss

r_sav = RECNO()
nm = NAME
pr = PREFIX
q = 0

SKIP
SCAN REST WHILE NAME = nm .AND. PREFIX = pr
  q = q+MAX(QNT-QNT_SAVE-QNT_REZ, 0)
ENDSCAN

IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF

IF q > 0
  DIMENSION mss(5)
  mss(1) = ""
  mss(2) = CHR(0)+"€…!   แชซ คฅ ญฅ ๅข โ ฅโ "+ALLTRIM(STR(i))+" ่โใช!"
  mss(3) = CHR(0)+"ฎ ฅแโ์ "+ALLTRIM(STR(q))+" ่โใช คเใฃจๅ ไจเฌ.  ช ฏฎแโใฏจฌ?"
  mss(4) = CHR(0)+"  [ แโ ขจฌ คฅไจๆจโ ] [ ฎแฌฎโเจฌ ญ  ํโจ ฏฎงจๆจจ ]"
  mss(5) = ""
  ?? REPLICATE(CHR(7),3)
  r_sav = Out_Mess(5, "mss")
  IF r_sav = 2
    q_w = MAX(STOCK.QNT,0)+q_r
    i = MAX(0, i-q)
  ENDIF
ENDIF

RETURN
