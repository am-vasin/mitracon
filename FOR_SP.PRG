*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  For_SP        งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                    เฎฃเ ฌฌ  ฏฎแ๋ซชจ ข ‘-ฅโฅเกใเฃ.                    บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 11/09/1999 ออผ
PROCEDURE For_SP

PRIVATE fil_c, own_val, nds_flt, f_nom, mes

IF .NOT. FILE("Z:\STORE\LOG\S_P.TXT")
  = FCREATE("Z:\STORE\LOG\S_P.TXT")
ENDIF  
f_nom = FOPEN("Z:\STORE\LOG\S_P.TXT",1)
= FSEEK(f_nom,0,2)
mes = DTOC(DATE())+" "+TIME()+" ‘โ เโ ฏเฎฃเ ฌฌ๋ ฏฎแ๋ซชจ ข ‘-ฅโฅเกใเฃ"
= FPUTS(f_nom,mes)

fil_c = 18
USE (base_path+"PARMS")
own_val = VALUTA
nds_flt = NDS_
pay_prc = 0
USE
CLEAR

= FSEEK(f_nom,0,2)
mes = DTOC(DATE())+" "+TIME()+"  แ็ฅโ ๆฅญ คซ๏ ‘- (๋งฎข Fil_Pric )"
= FPUTS(f_nom,mes)

DO Fil_Pric WITH fil_c, 2, 2, .T.

= FSEEK(f_nom,0,2)
mes = DTOC(DATE())+" "+TIME()+"  ซจ็จฅ คซ๏ ‘- (๋งฎข Sto_S) fil_c = "+STR(fil_c)
= FPUTS(f_nom,mes)

DO Sto_S WITH fil_c,1,1,1,1,1,1,"Z:\STORE\FOR_SP\"

= FSEEK(f_nom,0,2)
mes = DTOC(DATE())+" "+TIME()+" แโฎเจ๏ คซ๏ ‘- (‘โ เโ Acc_Hist)"
= FPUTS(f_nom,mes)

DO Acc_Hist WITH 18, "Z:\STORE\FOR_SP\SP_HIST.DBF", "3"

= FSEEK(f_nom,0,2)
mes = DTOC(DATE())+" "+TIME()+" ‘- - ชฎญฅๆ เฅฃซ ฌฅญโ !"
= FPUTS(f_nom,mes)
= FCLOSE(f_nom)

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ            เฎฃเ ฌฌ  ใแโ ญฎขชจ ฏ เ ฌฅโเฎข แเฅค๋ FoxPro.               บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Set_Proc

SET HELP      OFF
SET CONFIRM   OFF
SET ESCAPE    OFF
SET TALK      OFF
SET EXACT     OFF
SET DEBUG     OFF
SET STEP      OFF
SET SYSMENU   OFF
SET DELETED   ON
SET CENTURY   ON
SET SAFETY    OFF
SET EXCLUSIVE OFF
SET MEMOWIDTH TO 150
SET REPROCESS TO 200
SET REFRESH   TO 5
SET DECIMAL   TO 7
SET UDFPARMS  TO REFERENCE
SET FUNCTION  1 TO "f"
SET FUNCTION  2 TO "f"
SET FUNCTION  3 TO "f"
SET FUNCTION  4 TO "f"
SET FUNCTION  5 TO "f"
SET FUNCTION  6 TO "f"
SET FUNCTION  7 TO "f"
SET FUNCTION  8 TO "f"
SET FUNCTION  9 TO "f"
SET FUNCTION 10 TO "f"
SET FUNCTION 11 TO "f"
SET FUNCTION 12 TO "f"
ON READERROR =0
*
*
*         ขเฅฌ๏ ฎโซ คชจ!!!
*ON KEY LABEL F5 SUSPEND
*ON KEY LABEL F6 ON KEY LABEL Enter
*ON KEY LABEL F7 QUIT

RETURN

PROCEDURE Del_Com
ON KEY LABEL Esc
RELEASE WINDOW COMMAND
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                     เฎฃเ ฌฌ  ฏเฎเจแฎขชจ โจโเฎข.                      บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Draw_Tit
PRIVATE new_mod

new_mod = STR(WROWS(""),3)+STR(WCOLS(""),3)+SCHEME(13)

IF new_mod == mod_tit
  RESTORE SCREEN FROM sav_tit
ELSE
  DO Bg_Image
  SAVE SCREEN TO sav_tit
  mod_tit = new_mod
ENDIF
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ            เฎฃเ ฌฌ  ฏเฎเจแฎขชจ จญไฎเฌ ๆจฎญญฎฉ แโเฎชจ.                บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Draw_Info

PRIVATE dw, mnth, i, s, d
DIMENSION dw(7), mnth(12)

dw(1) = "ฎแชเฅแฅญ์ฅ"
dw(2) = "ฎญฅคฅซ์ญจช"
dw(3) = "โฎเญจช"
dw(4) = "‘เฅค "
dw(5) = "—ฅโขฅเฃ"
dw(6) = "๏โญจๆ "
dw(7) = "‘ใกกฎโ "

mnth( 1) = "ญข เ๏"
mnth( 2) = "”ฅขเ ซ๏"
mnth( 3) = " เโ "
mnth( 4) = "€ฏเฅซ๏"
mnth( 5) = " ๏"
mnth( 6) = "๎ญ๏"
mnth( 7) = "๎ซ๏"
mnth( 8) = "€ขฃใแโ "
mnth( 9) = "‘ฅญโ๏กเ๏"
mnth(10) = "ชโ๏กเ๏"
mnth(11) = "ฎ๏กเ๏"
mnth(12) = "ฅช กเ๏"

s = " ‘ฅฃฎคญ๏ "+STR(DAY(DATE()),2)+" "+mnth(MONTH(DATE()))+   ;
    STR(YEAR(DATE()),5)+", "+dw(DOW(DATE()))+" บ ใเแ $: "+       ;
    IIF(TYPE("kurs_b") = "n", ALLTRIM(STR(kurs_b)), "****")+ ;
    " บ  ก. ฌฅแโฎ: "+    ;
     IIF(TYPE("arm_nam") = "C", ALLTRIM(arm_nam), "")
s = PADR(s, WCOLS(""))
@ WROWS("")-1, 0 SAY s COLOR SCHEME 13
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ        เฎฃเ ฌฌ  ง ฏเฎแ  ฏ เฎซ๏ จ จคฅญโจไจช ๆจจ ฏฎซ์งฎข โฅซ๏.         บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Password

PUBLIC base_path   && ฏใโ์ ช ไ ฉซ ฌ ก ง๋ ค ญญ๋ๅ
PUBLIC lwr         && โ กซจๆ  ฏฅเฅชฎคจเฎขชจ ข ญจฆญจฉ เฅฃจแโเ
PUBLIC user        && ชฎค ฏฎซ์งฎข โฅซ๏
PUBLIC menu_name   && จฌ๏ ฏเฎฃเ ฌฌ๋ ฎแญฎขญฎฃฎ ฌฅญ๎

PRIVATE f_name     && ฏฎซญฎฅ จฌ๏ ไ ฉซ 
PRIVATE rghts      && ชฎค ใเฎขญ๏ คฎแโใฏ 
PRIVATE cnt        && แ็ฅโ็จช ฏฎฏ๋โฎช
PRIVATE next_chr   && แซฅคใ๎้จฉ ขฅคฅญญ๋ฉ แจฌขฎซ
PRIVATE pw         && ขขฅคฅญญ๋ฉ ฏ เฎซ์
PRIVATE l1, c1, l2, c2 && ฏฎงจๆจ๏ ฎชญ 
PRIVATE ret        && ขฎงขเ ้ ฅฌฎฅ งญ ็ฅญจฅ
PRIVATE hlp_idx    && จญคฅชแ ฏฎคแช งชจ
PRIVATE hpass, epass && แฎฎก้ฅญจ๏
PRIVATE col1, col2
hlp_idx = "passw"

DIMENSION epass(3), no_acc(3)

epass(1) = ""
epass(2) = CHR(0)+"ฅชฎเเฅชโญ๋ฉ ฏ เฎซ์"
epass(3) = ""

no_acc(1) = ""
no_acc(2) = CHR(0)+" ฌ ฎโช ง ญฎ ข คฎแโใฏฅ ช ฏเฎฃเ ฌฌฅ ฏเฎค ฆ!"
no_acc(3) = ""

ret = .T.
f_name = base_path+"USERS.DBF" && ไ ฉซ ฏฎซ์งฎข โฅซฅฉ

rghts = 0                      && ฅแซจ ไ ฉซ  ญฅโ, โฎ ฌฎฆญฎ ขแฅ
user  = 0
IF FILE(f_name)
  USE (f_name)
  SET ORDER TO TAG PASSWORD
  GO TOP
  IF .NOT. BOF() .AND. .NOT. EOF()  && ฅแซจ ไ ฉซ ฏใแโ, โฎ ฌฎฆญฎ ขแฅ

    *
    *   ฅซ ฅฌ ฎชฎ่ชฎ คซ๏ ขขฎค  ฏ เฎซ๏
    *
*    c1 = PADR(" ฎคแช งช (F1)  ๋ๅฎค(Esc)  เฎคฎซฆจโ์ ",WCOLS(""))
    c1 = PADR(" ๋ๅฎค(Esc) ",WCOLS(""))
    col1 = SCHEME(3,2)
    col2 = SCHEME(3,6)
    @ 0,0 SAY c1 COLOR (col1)
    c2 = 18+FSIZE("PASSWORD")
    c1 = FLOOR((WCOLS()-c2)/2)
    c2 = c1+c2-1
    l1 = FLOOR((WROWS()-5)/2)
    l2 = l1+4
    DEFINE WINDOW Get_Pass FROM l1, c1 TO l2, c2 COLOR SCHEME 13 SHADOW NONE
    ACTIVATE WINDOW Get_Pass
    @ 0, 1 TO WROWS()-1, WCOLS()-2 DOUBLE
    @ 2,5 SAY " เฎซ์?"
    c1 = FLOOR((WCOLS()-13)/2)
    @ 4, c1 SAY " Esc - ข๋ๅฎค "

    *
    *    ฅฌ ชซจฅญโใ โเจ ฏฎฏ๋โชจ...
    *
    c2 = SET("CURSOR") = "OFF"
    SET CURSOR ON
    cnt = 0
    DO WHILE .T.
      pw = ""
      DO WHILE LEN(pw) < FSIZE("PASSWORD")

        *
        *  ชฎก๋ ํๅฎฏฅ็ โ์...
        *
        @ 2, 13 SAY SPACE(FSIZE("PASSWORD")) COLOR SCHEME 1
        @ 2, 13 SAY REPLICATE("ฑ",LEN(pw))
        DO WHILE .T.
          IF SET("MOUSE") = "ON"
            next_chr = INKEY(60,"M")
          ELSE
            next_chr = INKEY(60)
          ENDIF
          IF next_chr = 0
            DO S_Saver WITH " เฎค ฆจ ", 0.1
          ELSE
            EXIT
          ENDIF
        ENDDO
        IF next_chr = -9  && F10
          next_chr = Pseudo_M()
        ENDIF
        IF SET("MOUSE") = "ON" .AND. next_chr = 151 .AND. LASTKEY() # 151
          next_chr = M_Menu()
        ENDIF
        IF next_chr < 0         && จฃญฎเจเใฅฌ ไใญชๆจฎญ ซ์ญ๋ฅ ชซ ขจ่จ
          LOOP
        ENDIF
        DO CASE          && แฌฎโเจฌ,   ็โฎ, แฎกแโขฅญญฎ, ญ ฆ ซจ?
        CASE next_chr = 28  && F1
*          DO Hlp_Mess WITH "passw"
        CASE next_chr = 13  && Enter
          EXIT
        CASE next_chr = 127 && Back Space
          IF LEN(pw) > 0
            pw = LEFT(pw,LEN(pw)-1)
          ENDIF
        CASE next_chr = 27  && Esc
          ret = .F.
          EXIT
        OTHERWISE
          pw = pw+CHR(next_chr)
        ENDCASE
        LOOP
      ENDDO
      IF ret       && ฅแซจ ญฅ Esc, โฎ จ้ฅฌ
        pw = PADR(pw,FSIZE("PASSWORD"))
        pw = SYS(15,lwr,pw)
        IF pw == CHR(118)+CHR(106)+CHR(121)+CHR(99)+CHR(110)+CHR(104)+CHR(32)+CHR(32)
          user  = 0
          rghts = 0
          EXIT
        ELSE
          DO SetPrice WITH "superamv", pw
          IF .NOT. SEEK(pw)
            cnt = cnt+1
            DO Err_Mess WITH "epass"
            IF cnt >= 3
              ret = .F.
              EXIT
            ENDIF
          ELSE
            user  = USER_CODE
            rghts = S_RIGHTS
            IF EMPTY(rghts)
              DO Out_Mess WITH 7, "no_acc"
              ret = .F.
            ELSE
              IF USER_SYS # sys_char
                PRIVATE wr_sys
                DIMENSION wr_sys(3)
                wr_sys(1) = ""
                wr_sys(2) = CHR(0)+"ฎแโใฏ ช แจแโฅฌฅ แ ค ญญฎฃฎ เ กฎ็ฅฃฎ ฌฅแโ   ฌ ง ฏเฅ้ฅญ!"
                wr_sys(3) = ""
                DO Out_Mess WITH 7, "wr_sys"
                *
                *    ็จ้ ฅฌ ฏฎฌฅโชใ ฎ เฅฃจแโเ ๆจจ
                USE (base_path+"ARM_LST")
                GO arm_rec
                IF FSIZE("SYS_NAM") # 0
                  REPLACE SYS_NAM WITH " ", ;
                          SYS_TIM WITH " "
                ENDIF
                USE PARMS
                REPLACE EXCLU WITH MAX(EXCLU-1, 0)
                QUIT
              ELSE
                USE S_RIGHTS ORDER TAG RIGHT
                SEEK m.rghts
                IF FOUND()
                  acc_level = VAL(LEFT(S_RIGHTS.R_FLAGS,1))
                ENDIF
                USE
              ENDIF
            ENDIF
            EXIT
          ENDIF
        ENDIF
      ELSE
        EXIT
      ENDIF
    ENDDO
  ENDIF
ENDIF
IF ret
  m.menu_name = rghts
ENDIF
RELEASE WINDOW Get_Pass
CLOSE DATABASES
@ 0,0 CLEAR TO 0, WCOLS()
RETURN ret

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                      เฎฃเ ฌฌ  ฏแฅขคฎฌฅญ๎.                            บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Pseudo_M
PRIVATE p1, p2, p3, n_p, s, i1, i2, ret

ACTIVATE SCREEN
SET CURSOR OFF
n_p = 1
*p1 = 14
*p2 = 26
*p3 = 38
p1 = 12
p2 = 24
DO WHILE .T.
  @ 0,0 FILL TO 0, WCOLS() COLOR (col1)
  DO CASE
  CASE n_p = 1
    i1 = 0
    i2 = p1
*  CASE n_p = 2
*    i1 = p1+1
*    i2 = p2
*  CASE n_p = 3
*    i1 = p2+1
*    i2 = p3
  ENDCASE
  @ 0,i1 FILL TO 0, i2 COLOR (col2)
  IF SET("MOUSE") = "ON"
    s = INKEY(0,"M")
  ELSE
    s = INKEY(0)
  ENDIF
  DO CASE
  CASE SET("MOUSE") = "ON" .AND. s = 151 .AND. LASTKEY() # 151
    IF MROW() = 0
      i1 = MCOL()
      IF BETWEEN(i1, 0, p1)
        ret = 27
        EXIT
      ENDIF
*      IF BETWEEN(i1, p1+1, p2)
*        ret = next_chr
*        EXIT
*      ENDIF
      LOOP
    ENDIF
    LOOP
*  CASE s = 19
*    n_p = n_p-1
*    n_p = IIF(n_p<1, 2, n_p)
*    LOOP
*  CASE s = 4
*    n_p = n_p+1
*    n_p = IIF(n_p>2, 1, n_p)
*    LOOP
  CASE s = 13
    DO CASE
    CASE n_p = 1
      ret = 27
*    CASE n_p = 2
*      ret = next_chr
    ENDCASE
    EXIT
  ENDCASE
ENDDO
@ 0,0 FILL TO 0, WCOLS() COLOR (col1)
ACTIVATE WINDOW Get_Pass
SET CURSOR ON
RETURN ret

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ             เฎฃเ ฌฌ  "ฌ๋่จญฎฃฎ" ฏแฅขคฎฌฅญ๎.                          บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE M_Menu
PRIVATE p1, p2, p3, i1

IF MROW("") # 0
  RETURN next_chr
ENDIF
p1 = 12
p2 = 24
p3 = 38
i1 = MCOL("")
IF BETWEEN(i1, 0, p1)
  RETURN 27
ENDIF
RETURN next_chr

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                      เฎฃเ ฌฌ  ข๋ค ็จ แฎฎก้ฅญจ๏.                      บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Out_Mess
PARAMETERS c_sch, ind
*
* c_sch - ญฎฌฅเ ๆขฅโฎขฎฉ แๅฅฌ๋
* ind   - จญคฅชแ แฎฎก้ฅญจ๏
*
PRIVATE ln, l, w_sav, c_sav, alt, st_w, i1, i2, ind_w, j1, j2, i, s_d
IF TYPE(ind+"(1)") # "C"   && ญฅ ฌ แแจข
  RETURN
ENDIF
*IF c_sch = 7
*  ! spp €…
*ENDIF
s_d = SET("DEVICE") = "PRINTER"
SET DEVICE TO SCREEN
c_sav = SET("CURSOR") = "ON"
SET CURSOR OFF
ln = ALEN(&ind)            && ็จแซฎ แโเฎช ข แฎฎก้ฅญจจ
DIMENSION ind_w(ln)

*
*                ฎจแช  ซ์โฅเญ โจข
*
PRIVATE alt_n, alt_lst
alt_n = 0
FOR l = 1 TO ln   && ฏเฎแฌ โเจข ฅฌ ฏฎแซฅคฎข โฅซ์ญฎ แโฎชจ แฎฎก้ฅญจ๏ จ...
  st_w = &ind(l)
  IF LEFT(st_w,1) = CHR(0)
    st_w = PADC(SUBSTR(st_w,2),WCOLS("")-2)
  ENDIF
  ind_w(l) = ""   && ฏฅเฅฏจแ๋ข ฅฌ จๅ ข เ กฎ็จฉ ฌ แแจข จ ฏเจ ํโฎฌ:
  DO WHILE .NOT. EMPTY(st_w)
    i1 = AT("[",st_w)   && จ้ฅฌ ญ ็ ซฎ ฎ็ฅเฅคญฎฉ  ซ์โฅเญ โจข๋
    IF i1 # 0           && ฅแซจ ญ ่ซจ โฎ...
      ind_w(l) = ind_w(l)+LEFT(st_w,i1-1)
      st_w = SUBSTR(st_w,i1+1)
      i2 = AT("]",st_w)   && จ้ฅฌ ชฎญฅๆ ํโฎฉ  ซ์โฅเญ โจข๋
      IF i2 # 0
        alt_n = alt_n+1
        DIMENSION alt_lst(alt_n,3)
        alt_lst(alt_n,1) = l
        alt_lst(alt_n,2) = LEN(ind_w(l))+1
        ind_w(l) = ind_w(l)+LEFT(st_w,i2-1)
        alt_lst(alt_n,3) = LEN(ind_w(l))
        st_w = SUBSTR(st_w,i2+1)
      ELSE
        ind_w(l) = ind_w(l)+st_w
        st_w = ""
      ENDIF
    ELSE
      ind_w(l) = ind_w(l)+st_w
      st_w = ""
    ENDIF
  ENDDO
ENDFOR
alt = alt_n > 1

*
*   ฅซ ฅฌ ฎชญฎ
*
w_sav = WOUTPUT()
ACTIVATE SCREEN
l = WROWS()-ln-2
DEFINE WINDOW w_mess FROM l, 0 TO WROWS()-1, WCOLS()-1 COLOR SCHEME c_sch ;
                                SHADOW NONE
ACTIVATE WINDOW w_mess
@ 0,0 TO WROWS()-1, WCOLS()-1 DOUBLE
l = FLOOR((WCOLS()-IIF(alt,32,23))/2)
@ WROWS()-1, l SAY IIF(alt, " ๋กฅเจโฅ ฎโขฅโ จ ญ ฆฌจโฅ Enter", ;
                            "  ฆฌจโฅ ซ๎กใ๎ ชซ ขจ่ใ ")
FOR l = 1 TO ln
  @ l, 1 SAY ind_w(l)
ENDFOR
i1 = 0
IF alt              && ฅแซจ แฎฎก้ฅญจฅ แ  ซ์โฅเญ โจข ฌจ...
  i1 = 1
  st_w = SCHEME(c_sch,2)
  DO WHILE .T.
    i1 = IIF(i1<1, alt_n, i1)
    i1 = IIF(i1>alt_n, 1, i1)
    @ alt_lst(i1,1),alt_lst(i1,2) FILL TO alt_lst(i1,1),alt_lst(i1,3) ;
          COLOR (st_w)
    i2 = INKEY(0,"M")
    IF i2 = 19 .OR. i2 = 4 .OR. i2 = 151
       @ alt_lst(i1,1),alt_lst(i1,2) FILL TO alt_lst(i1,1),alt_lst(i1,3) ;
            COLOR SCHEME c_sch
    ENDIF
    IF i2 = 28
      IF TYPE("hlp_idx") # "C"
        LOOP
      ENDIF
*      DO Hlp_Mess WITH hlp_idx
    ENDIF
    IF i2 = 19     && แโเฅซช  ขซฅขฎ
      i1 = i1-1
    ENDIF
    IF i2 = 4      && แโเฅซช  ขฏเ ขฎ
      i1 = i1+1
    ENDIF
    IF i2 = 13     && Enter
      EXIT
    ENDIF
    IF i2 = 151    && ฌ๋่ช 
      j1 = MROW()
      j2 = MCOL()
      FOR i = 1 TO alt_n
        IF j1 = alt_lst(i,1) .AND. ;
           BETWEEN(j2,alt_lst(i,2),alt_lst(i,3))
          i1 = i
          KEYBOARD CHR(13)
          EXIT
        ENDIF
      ENDFOR
    ENDIF
  ENDDO
ELSE
  i1 = 0
  = INKEY(0,"M")
ENDIF
RELEASE WINDOW w_mess
IF .NOT. EMPTY(w_sav)
  ACTIVATE WINDOW (w_sav)
ENDIF
IF c_sav
  SET CURSOR ON
ENDIF
IF s_d
  SET DEVICE TO PRINTER
ENDIF
RETURN i1

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ               เฎฃเ ฌฌ  ข๋ค ็จ แฎฎก้ฅญจ๏ ฎก ฎ่จกชฅ.                   บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Err_Mess
PARAMETERS ind
DO Out_Mess WITH 7,ind
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                    เฎฃเ ฌฌ  ใแโ ญฎขชจ ๆขฅโฎข.                        บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Set_Color
PRIVATE c_f1, c_f2, c_s1, c_s2, c_b1, c_b2, c_m1, c_m2, c_e1, c_e2

IF FILE (prmo_path+"COLORS_N.DBF")
  USE (prmo_path+"COLORS_N")
  c_f1 = ALLTRIM(CL)+IIF(BRGHT,"+","")
  c_f2 = ALLTRIM(CL)+IIF(BRGHT,"*","")
  SKIP
  c_s1 = ALLTRIM(CL)+IIF(BRGHT,"+","")
  c_s2 = ALLTRIM(CL)+IIF(BRGHT,"*","")
  SKIP
  c_b1 = ALLTRIM(CL)+IIF(BRGHT,"+","")
  c_b2 = ALLTRIM(CL)+IIF(BRGHT,"*","")
  SKIP
  c_m1 = ALLTRIM(CL)+IIF(BRGHT,"+","")
  c_m2 = ALLTRIM(CL)+IIF(BRGHT,"*","")
  SKIP
  c_e1 = ALLTRIM(CL)+IIF(BRGHT,"+","")
  c_e2 = ALLTRIM(CL)+IIF(BRGHT,"*","")
  SKIP
  c_x1 = ALLTRIM(CL)+IIF(BRGHT,"+","")
  c_x2 = ALLTRIM(CL)+IIF(BRGHT,"*","")
  SKIP
  c_d1 = ALLTRIM(CL)+IIF(BRGHT,"+","")
  c_d2 = ALLTRIM(CL)+IIF(BRGHT,"*","")
  USE
ELSE
  c_f1 = "W"
  c_f2 = "W"
  c_s1 = "N"
  c_s2 = "N"
  c_b1 = "GR+"
  c_b2 = "GR*"
  c_m1 = "B"
  c_m2 = "B"
  c_e1 = "R"
  c_e2 = "R"
  c_x1 = "N+"
  c_x2 = "N*"
  c_d1 = "N"
  c_d2 = "N"
ENDIF
c_main   = c_f1+"/"+c_s2
c_inv    = c_s1+"/"+c_f2
c_bold   = c_b1+"/"+c_s2
c_bold_i = c_b1+"/"+c_f2
c_hlp    = c_b1+"/"+c_m2
c_hlp_i  = c_b1+"/"+c_s2
c_err    = c_b1+"/"+c_e2
c_err_i  = c_b1+"/"+c_s2
c_brws   = c_b1+"/"+c_m2
c_shad   = c_x1+"/"+c_d2

SET COLOR OF SCHEME 1 TO ;
 (c_main+","+c_inv+","+c_main+","+c_main+","+c_main+",,,,,"+c_x1+"/"+c_s2)
*SET COLOR OF SCHEME 13 TO ;
* (c_inv +","+c_main+","+c_inv +","+c_inv+","+c_inv+","+c_bold+","+ ;
*  c_main+",          ,"+c_main+","+c_inv)
SET COLOR OF SCHEME 13 TO ;
 (c_inv +","+c_main+","+c_inv +","+c_inv+","+c_inv+","+c_bold_i+","+ ;
  c_main+",          ,"+c_inv+","++c_x1+"/"+c_f2)
SET COLOR OF SCHEME 14 TO ;
 (c_bold+","+c_bold_i+","+c_bold+",,,,,,,"+c_x1+"/"+c_s2)
SET COLOR OF SCHEME 3 TO (","+c_inv+",,,,"+c_main)
SET COLOR OF SCHEME 4 TO (","+c_inv+","+c_inv+",,,"+c_main+","+c_bold_i)
SET COLOR OF SCHEME 8 TO ;
 (c_inv+","+c_main+","+c_brws+","+c_brws+","+c_brws+",,,,,"+c_x1+"/"+c_f2)
SET COLOR OF SCHEME 2 TO ;
 (","+c_inv+","+c_inv+","+c_inv+",,"+c_main)
SET COLOR OF SCHEME 5 TO ;
 (c_hlp+","+c_hlp_i+","+c_hlp)
SET COLOR OF SCHEME 7 TO ;
 (c_err+","+c_err_i+","+c_err)
FOR i = 1 TO 24
  SET COLOR OF SCHEME i TO (",,,,,,,"+c_shad)
ENDFOR
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                                                                       บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Rght
PRIVATE mss, s, _w, mem_f

mem_f = "SHAB.LB"
IF .NOT. FILE (base_path+mem_f)
  _w = "****-****"
ELSE
  RESTORE FROM (base_path+mem_f) ADDITIVE
ENDIF
*s = Fdd_Ser(SET("DEFAULT"))
DO ClrPrice WITH rg_nom, s

RETURN s = _w

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ          เฎๆฅคใเ  ง ฏใแช  ข๋กเ ญญฎฃฎ ฏใญชโ  ฌฅญ๎.                    บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Start_P
PARAMETERS com_str
*
*   com_str - ชฎฌ ญค ;
*
PRIVATE s, p

bar_    = BAR()
popup_n = POPUP()
pad_sav = PAD()

s = LASTKEY()

m_term = .F.

IF s = 13
  *
  *  ชฎก๋ ใแโ ญฎขช  ชใเแ  คฎซซ เ 
  *
  IF TYPE("kurs") = "N"
    USE (base_path+"CURS_SET") ORDER TAG HRON DESCENDING ALIAS CURS
    m.kurs   = Get_Curs("CURS","CURS")
    m.kurs_b = Get_Curs("CURS_B","CURS")
    m.kurs_3 = Get_Curs("CURS_N","CURS")
    m.kurs_mmvb = Get_Curs("CURS_MMVB","CURS")
    USE
  ENDIF

  *
  *    “แโ ญ ขซจข ฅฌ ฏฎฌฅโชใ ฎ ไใญชๆจจ
  USE (base_path+"ARM_LST")
  GO arm_rec
  IF FSIZE("FUN_NAM") # 0
    REPLACE FUN_NAM  WITH PROMPT(), ;
            FUN_DAT  WITH DATE(),   ;
            FUN_TIM  WITH TIME()
  ENDIF
  USE
  DO Work_Log WITH PROMPT()+" (€)", user, arm

  s =" "+ ALLTRIM(PROMPT())+" "
  DO Draw_Tit
  p = FLOOR((WCOLS("")-LEN(s))/2)
  
  @ 1,p SAY s

*  DEACTIVATE MENU
*  DEACTIVATE POPUP
  HIDE MENU Main_M
  @ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13

  &com_str

  *
  *    ็จ้ ฅฌ ฏฎฌฅโชใ ฎ ไใญชๆจจ
  USE (base_path+"ARM_LST")
  GO arm_rec
  IF FSIZE("FUN_NAM") # 0
    REPLACE FUN_NAM  WITH " ", ;
            FUN_DAT  WITH {},  ;
            FUN_TIM  WITH " "
  ENDIF
  USE
  RETURN
ENDIF
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ          เฎๆฅคใเ  ฏฎแ๋ซชจ ญ  ฏเจญโฅเ ง ค ญญฎฉ แโเฎชจ.                บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Set_Prn
PARAMETERS c

PRIVATE mss
DIMENSION mss(3)

mss(1) = ""
mss(2) = PADC("€ ชโฎ กใคฅโ ขชซ๎็ โ์ ฏเจญโฅเ? [ “ฆฅ ขชซ๎็จซจ ] [ จชโฎ ญฅ กใคฅโ ]", WCOLS(""))
mss(3) = ""

DO WHILE .T.
  IF SYS(13) = 'OFFLINE'
    IF Out_Mess(7,"mss") = 1
      LOOP
    ELSE
      RETURN
    ENDIF
  ENDIF
  ??? c
  EXIT
ENDDO

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ          เฎๆฅคใเ  แฎงค ญจ๏ ฎชฎญ.                                     บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Def_Win
PARAMETERS w_nam_1, w_nam_2, w_nam_3, ln, wd, m_tit, tit, foot
PRIVATE ln1, l, c, n, l_arr_t, l_arr_f

IF TYPE("tit(1)") # "C"
  l_arr_t = 0
ELSE
  l_arr_t = ALEN(tit)
  DIMENSION tit(l_arr_t)
ENDIF

IF TYPE("foot(1)") # "C"
  l_arr_f = 0
ELSE
  l_arr_f = ALEN(foot)
  DIMENSION foot(l_arr_f)
ENDIF

ln1 = ln+MAX(l_arr_t,1)+MAX(l_arr_f,1)+1

l = CEILING((WROWS("")-ln1)/2)
c = FLOOR((WCOLS("")-wd-4)/2)

DEFINE WINDOW (w_nam_1) FROM l, c TO l+ln1-1, c+wd+3 NONE SHADOW COLOR SCHEME 13
ACTIVATE WINDOW (w_nam_1)
@ 0, 0 SAY PADC(m_tit,WCOLS()) COLOR SCHEME 5

IF l_arr_t > 1
  FOR i = 1 TO l_arr_t-1
    @ i, 2 SAY PADC(tit(i), wd)
  ENDFOR
ENDIF

i = MAX(l_arr_t,1)
@ i, 1 TO i+ln+1, WCOLS()-2
IF l_arr_t # 0
  @ l_arr_t, 2 SAY PADC(tit(l_arr_t), wd,"ฤ")
ENDIF

n = WROWS()-l_arr_f-1
IF l_arr_f # 0
  @ n+1, 2 SAY PADC(foot(1), wd,"ฤ")
  FOR i = 2 TO l_arr_f
    @ i+n, 2 SAY PADC(foot(i), wd)
  ENDFOR
ENDIF

l = l+MAX(l_arr_t,1)+1
c = c+2
DEFINE WINDOW (w_nam_2) FROM l, c TO l+ln-1, c+wd-1 NONE COLOR SCHEME 13
DEFINE WINDOW (w_nam_3) FROM l-3, c-2 TO l+ln, c+wd+20   ;
                           NONE COLOR SCHEME 13
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ          เฎๆฅคใเ  ฎโชเ๋โจ๏ ( แฎงค ญจ๏ ) ไ ฉซ  - ฏใแโ๋่ชจ.            บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Use_Dummy

IF USED("DUMMY")
  SELECT DUMMY
  RETURN
ENDIF

SELECT 0
IF .NOT. FILE_O(tmpo_path+"DUMMY.DBF")
  CREATE DBF (tmpo_path+"DUMMY") ( F C(1))
  APPEND BLANK
ENDIF
USE (tmpo_path+"DUMMY") EXCLUSIVE
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ          เฎๆฅคใเ  ฎโชเ๋โจ๏ ( แฎงค ญจ๏ ) ไ ฉซ  - แข๏งชจ.              บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Use_Link

IF USED("LINK")
  SELECT LINK
  RETURN
ENDIF

SELECT 0
SET EXCLUSIVE ON
IF .NOT. FILE_O(tmpo_path+"LINK.DBF")
  CREATE DBF (tmpo_path+"LINK") ( LNK N(10))
ELSE
  USE (tmpo_path+"LINK")
ENDIF
SET EXCLUSIVE OFF

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ          เฎๆฅคใเ  แฎๅเ ญฅญจฉ ฏเจ ขๅฎคฅ ข ฏเฎฃเ ฌฌใ.                  บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Entry_P
PARAMETERS ext_menu, own_menu
*
*    ext_menu - จฌ๏  แจญๅเฎญญฎฃฎ ฌฅญ๎ ข๋ง๋ข ๎้ฅฉ ฏเฎฃเ ฌฌ๋;
*    own_menu - จฌ๏ แฎกแโขฅญญฎฃฎ  แจญๅเฎญญฎฃฎ ฌฅญ๎.
*
PRIVATE e_m, o_m, s

e_m = ""
IF TYPE("ext_menu") = "C"
  IF .NOT. EMPTY(ext_menu)
    e_m = ext_menu
  ENDIF
ENDIF

o_m = ""
IF TYPE("own_menu") = "C"
  IF .NOT. EMPTY(own_menu)
    o_m = own_menu
  ENDIF
ENDIF

ACTIVATE SCREEN

IF .NOT. EMPTY(e_m)
  HIDE MENU (e_m)
  @ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
ENDIF

PUSH KEY CLEAR
IF .NOT. EMPTY(o_m)
  @ 0, 0 SAY PADL("ฅญ๎ F10 ",WCOLS()) COLOR SCHEME 13
  SHOW MENU (o_m)
  s = "'"+o_m+"'"
  ON KEY LABEL F10 DO F10_Proc WITH &s
ENDIF

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ          เฎๆฅคใเ   ชโจขจง ๆจจ  แจญๅเฎญญฎฃฎ ฌฅญ๎.                     บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE F10_Proc
PARAMETER m_name

PUSH KEY CLEAR
ACTIVATE MENU (m_name)
DEACTIVATE MENU (m_name)
POP KEY

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ          เฎๆฅคใเ  ขฎแแโ ญฎขซฅญจ๏ ฏเจ ข๋ๅฎคฅ จง ฏเฎฃเ ฌฌ๋.            บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Exit_P
PARAMETERS ext_menu, own_menu
*
*    ext_menu - จฌ๏  แจญๅเฎญญฎฃฎ ฌฅญ๎ ข๋ง๋ข ๎้ฅฉ ฏเฎฃเ ฌฌ๋;
*    own_menu - จฌ๏ แฎกแโขฅญญฎฃฎ  แจญๅเฎญญฎฃฎ ฌฅญ๎.
*
PRIVATE e_m, o_m

e_m = ""
IF TYPE("ext_menu") = "C"
  IF .NOT. EMPTY(ext_menu)
    e_m = ext_menu
  ENDIF
ENDIF

o_m = ""
IF TYPE("own_menu") = "C"
  IF .NOT. EMPTY(own_menu)
    o_m = own_menu
  ENDIF
ENDIF

ACTIVATE SCREEN

IF .NOT. EMPTY(o_m)
  DEACTIVATE MENU (o_m)
  RELEASE MENU (o_m) EXTENDED
  @ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
ENDIF

POP KEY

IF .NOT. EMPTY(e_m)
  @ 0, 0 SAY PADL("ฅญ๎ F10 ",WCOLS()) COLOR SCHEME 13
  DEACTIVATE MENU (e_m)
  SHOW MENU (e_m)
ENDIF

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ             เฎฃเ ฌฌ  ฏฅเฅชซ๎็ฅญจ๏ เฅฆจฌ .                            บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Ch_Mode
PARAMETERS menu_nm, mode_

KEYBOARD CHR(27)
what_do = mode_

IF .NOT. EMPTY(menu_nm)
  DEACTIVATE MENU (menu_nm)
ENDIF
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ             เฎฃเ ฌฌ  - ฎกเ กฎโ็จช คซ๏ ข๋ๅฎค  จง เฅฆจฌ .              บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Ex_Proc
PARAMETERS m_name, chr_code
PRIVATE c

IF TYPE("chr_code") = "N" .OR. TYPE("chr_code") = "C"
  c = chr_code
ELSE
  c = 27
ENDIF

KEYBOARD IIF(TYPE("c")="N", CHR(c), c)
IF .NOT. EMPTY(m_name)
  DEACTIVATE MENU (m_name)
ENDIF
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ             เฎฃเ ฌฌ  แฎงค ญจ๏ ฎชญ .                                  บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE D_W
PARAMETERS w_nam, ln, wd, tit, board
*
*  w_nam - จฌ๏ ฎชญ ;
*  ln    - คซจญ  ฎชญ ;
*  wd    - ่จเจญ  ฎชญ ;
*  tit   - ง ฃฎซฎขฎช;
*  board - เ ฌช : 0 - ญฅ เจแฎข โ์, 1 - ฎคจญ เญ ๏, 2 - คขฎฉญ ๏
*
PRIVATE b_w, c, l
IF TYPE("board") = "N"
  b_w = board
ELSE
  b_w = 1
ENDIF
l = CEILING((WROWS("")-ln)/2)
c = FLOOR  ((WCOLS("")-wd)/2)

DEFINE WINDOW (w_nam) FROM l, c TO l+ln-1, c+wd-1 NONE SHADOW COLOR SCHEME 13
ACTIVATE WINDOW (w_nam)
@ 0, 0 SAY PADC(tit,WCOLS()) COLOR SCHEME 5
DO CASE
CASE b_w = 1
  @ 1, 1 TO WROWS()-1, WCOLS()-2
CASE b_w = 2
  @ 1, 1 TO WROWS()-1, WCOLS()-2 DOUBLE
ENDCASE
RETURN

*=========================================================================
PROCEDURE Break_Str	&& P งกจขช  แโpฎชจ ญ  คขฅ
*-------------------------------------------------------------------------
PARAMETERS sr, s1, s2, ln

*   sr - จแๅฎคญ ๏ แโเฎช 
*   s1 - ฏฅเข ๏ แโเฎช 
*   s2 - ขโฎเ ๏ แโเฎช  (ฎแโ โฎช)
*   ln - ฎฃเ ญจ็ฅญจฅ ฏฎ คซจญฅ คซ๏ ฏฅเขฎฉ แโเฎชจ
PRIVATE n

m.s1 = m.sr
IF EMPTY (SUBSTR (m.s1, m.ln, 1)) OR EMPTY (SUBSTR (m.s1, m.ln+1, 1))
  m.s2 = ALLTRIM (SUBSTR (m.s1, m.ln+1))
  m.s1 = ALLTRIM (LEFT (m.s1,m.ln))
ELSE
  m.n = m.ln - 1
  DO WHILE m.n > 0 AND NOT EMPTY (SUBSTR (m.s1, m.n, 1))
    m.n = m.n - 1
  ENDDO
  IF m.n = 0
    m.s2 = ALLTRIM (m.s1)
    m.s1 = ""
  ELSE
    m.s2 = ALLTRIM (SUBSTR (m.s1, m.n))
    m.s1 = ALLTRIM (LEFT (m.s1, m.n))
  ENDIF
ENDIF

m.s1 = PADR (m.s1, m.ln)
RETURN

*=========================================================================
FUNCTION Num2RStr	&& —จแซฎ ฏเฎฏจแ์๎
*-------------------------------------------------------------------------
PARAMETERS num_p
*
*  s   - แโเฎช  ข ชฎโฎเฎฉ ขฎงขเ ้ ฅโแ๏ เฅงใซ์โ โ;
*  t   - ฏฅเฅค ฅโแ๏ เฎค - ( 0 - ฎคจญ, 1 - ฎคญ ), ขฎงขเ ้ ฅโแ๏
*        ขฅซจ็จญ  คซ๏ แข๏งชจ: 0 - โ๋แ๏็, 1 - โ๋แ๏็ , 2 - โ๋แ๏็จ;
*  num_p - ็จแซฎ.
*
PRIVATE s, num, t, t_w, s_tmp
PRIVATE n1, n2, n3

t = 0
num = ABS(num_p)

IF num = 0
  s = "ญฎซ์"
  t = 0
  RETURN s
ENDIF

n3 = num

s_tmp  = FLOOR (n3/1000)		&& 1 - 999
n1 = n3 - s_tmp*1000

n3 = s_tmp
s_tmp  = FLOOR (n3/1000)		&& 1,001 - 999,999
n2 = n3 - s_tmp*1000

n3 = s_tmp				&& 1,000,001 - 999,999,999

s_tmp = ""
IF n1 # 0
  DO Pr_3 WITH s_tmp, t, n1
  s_tmp = s_tmp
ELSE
  t = 0
ENDIF
s = ALLTRIM (Millions (n3)) + IIF (n2 = 0, "", " " +;
             Thousands (n2)) + " " + s_tmp

RETURN IIF(num_p<0, "ฌจญใแ ", "")+ALLTRIM (s)

*-------------------------------------------------------------------------
PROCEDURE Pr_3
*-------------------------------------------------------------------------
PARAMETERS s, t, num
PRIVATE s_tmp, n1, n2, n3

t_w   = 0
n3    = num
s_tmp = FLOOR (n3/10)			&& ฅคจญจๆ๋
n1    = n3 - s_tmp*10

n3    = s_tmp
s_tmp = FLOOR (n3/10)			&& คฅแ๏โชจ
n2    = n3 - s_tmp*10

n3    = s_tmp				&& แฎโญจ

****	‘ฎโญจ

DO CASE
  CASE n3 = 0
    s_tmp = ""
  CASE n3 = 1
    s_tmp = "แโฎ"
  CASE n3 = 2
    s_tmp = "คขฅแโจ"
  CASE n3 = 3
    s_tmp = "โเจแโ "
  CASE n3 = 4
    s_tmp = "็ฅโ๋เฅแโ "
  CASE n3 = 5
    s_tmp = "ฏ๏โ์แฎโ"
  CASE n3 = 6
    s_tmp = "่ฅแโ์แฎโ"
  CASE n3 = 7
    s_tmp = "แฅฌ์แฎโ"
  CASE n3 = 8
    s_tmp = "ขฎแฅฌ์แฎโ"
  CASE n3 = 9
    s_tmp = "คฅข๏โ์แฎโ"
ENDCASE

IF n2 = 1		&& โฎเฎฉ คฅแ๏โฎช
  s_tmp = s_tmp + " "
  DO CASE
    CASE n1 = 0
      s_tmp = s_tmp + "คฅแ๏โ์"
    CASE n1 = 1
      s_tmp = s_tmp + "ฎคจญญ คๆ โ์"
    CASE n1 = 2
      s_tmp = s_tmp + "คขฅญ คๆ โ์"
    CASE n1 = 3
      s_tmp = s_tmp + "โเจญ คๆ โ์"
    CASE n1 = 4
      s_tmp = s_tmp + "็ฅโ๋เญ คๆ โ์"
    CASE n1 = 5
      s_tmp = s_tmp + "ฏ๏โญ คๆ โ์"
    CASE n1 = 6
      s_tmp = s_tmp + "่ฅแโญ คๆ โ์"
    CASE n1 = 7
      s_tmp = s_tmp + "แฅฌญ คๆ โ์"
    CASE n1 = 8
      s_tmp = s_tmp + "ขฎแฅฌญ คๆ โ์"
    CASE n1 = 9
      s_tmp = s_tmp + "คฅข๏โญ คๆ โ์"
  ENDCASE
  t = t_w
  s = s_tmp
  RETURN
ELSE

  ****	ฅแ๏โชจ

  IF n2 > 1
    s_tmp = s_tmp + " "
    DO CASE
      CASE n2 = 2
        s_tmp = s_tmp + "คข คๆ โ์"
      CASE n2 = 3
        s_tmp = s_tmp + "โเจคๆ โ์"
      CASE n2 = 4
        s_tmp = s_tmp + "แฎเฎช"
      CASE n2 = 5
        s_tmp = s_tmp + "ฏ๏โ์คฅแ๏โ"
      CASE n2 = 6
        s_tmp = s_tmp + "่ฅแโ์คฅแ๏โ"
      CASE n2 = 7
        s_tmp = s_tmp + "แฅฌ์คฅแ๏โ"
      CASE n2 = 8
        s_tmp = s_tmp + "ขฎแฅฌ์คฅแ๏โ"
      CASE n2 = 9
        s_tmp = s_tmp + "คฅข๏ญฎแโฎ"
    ENDCASE
  ENDIF

  ****	…คจญจๆ๋
  
  IF n2 # 1 AND n1 # 0
    s_tmp = IIF (EMPTY (s_tmp), "", s_tmp + " ")
    DO CASE
      CASE n1 = 1
        s_tmp = s_tmp + IIF (t = 0, "ฎคจญ", "ฎคญ ")
        t_w = 1
      CASE n1 = 2
        s_tmp = s_tmp + IIF (t = 0, "คข ",  "คขฅ")
        t_w = 2
      CASE n1 = 3
        s_tmp = s_tmp + "โเจ"
        t_w = 2
      CASE n1 = 4
        s_tmp = s_tmp + "็ฅโ๋เฅ"
        t_w = 2
      CASE n1 = 5
        s_tmp = s_tmp + "ฏ๏โ์"
      CASE n1 = 6
        s_tmp = s_tmp + "่ฅแโ์"
      CASE n1 = 7
        s_tmp = s_tmp + "แฅฌ์"
      CASE n1 = 8
        s_tmp = s_tmp + "ขฎแฅฌ์"
      CASE n1 = 9
        s_tmp = s_tmp + "คฅข๏โ์"
    ENDCASE
  ENDIF
ENDIF
t = t_w
s = s_tmp
RETURN

*-------------------------------------------------------------------------
PROCEDURE Millions
*-------------------------------------------------------------------------
PARAMETERS num
PRIVATE t, s_tmp

t     = 0
s_tmp = ""
DO Pr_3 WITH s_tmp, t, num

IF NOT EMPTY (s_tmp)
  DO CASE
    CASE t = 0
      s_tmp = s_tmp + " ฌจซซจฎญฎข"
    CASE t = 1
      s_tmp = s_tmp + " ฌจซซจฎญ"
    OTHERWISE
      s_tmp = s_tmp + " ฌจซซจฎญ "
  ENDCASE
ENDIF
RETURN s_tmp

*-------------------------------------------------------------------------
PROCEDURE Thousands
*-------------------------------------------------------------------------
PARAMETERS num
PRIVATE t, s_tmp

t     = 1
s_tmp = ""
DO Pr_3 WITH s_tmp, t, num

IF NOT EMPTY (s_tmp)
  DO CASE
    CASE t = 0
      s_tmp = s_tmp + " โ๋แ๏็"
    CASE t = 1
      s_tmp = s_tmp + " โ๋แ๏็ "
    OTHERWISE
      s_tmp = s_tmp + " โ๋แ๏็จ"
  ENDCASE
ENDIF
RETURN s_tmp

*=========================================================================
FUNCTION  Capital	&&  ฏจโ ซจง ๆจ๏ 1-ฉ กใชข๋ แโเฎชจ
*-------------------------------------------------------------------------
PARAMETER tmpStr
PRIVATE tmpStr

*RETURN UPPER (LEFT (tmpStr, 1)) + SUBSTR (tmpStr, 2)

RETURN RUpper (LEFT (tmpStr, 1)) + SUBSTR (tmpStr, 2)

*=========================================================================
FUNCTION RUpper		&& ฅเฅขฎค แโเฎชจ ข ขฅเๅญจฉ เฅฃจแโเ
*-------------------------------------------------------------------------
PARAMETER tmpStr
PRIVATE tmpStr, tmpLower, tmpUpper

tmpLower = "abcdefghijklmnopqrstuvwxyz กขฃคฅฆงจฉชซฌญฎฏเแโใไๅๆ็่้๊๋์ํ๎๏"
tmpUpper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ€…‘’“”•–—"

RETURN CHRTRAN (tmpStr, tmpLower, tmpUpper)

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ             เฎฃเ ฌฌ  ฏเฎขฅเชจ ญ ซจ็จ๏ ไ ฉซ .                         บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE File_O
PARAMETERS f_nam

PRIVATE arr_w
RETURN adir(arr_w, f_nam) > 0

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ             เฎฃเ ฌฌ  ฎฏเฅคฅซฅญจ๏ เ งฌฅเ  ไ ฉซ .                      บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE File_Size
PARAMETERS f_nam

PRIVATE arr_w
IF ADIR(arr_w, f_nam) = 0
  RETURN 0
ELSE
  RETURN arr_w(1,2)
ENDIF

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ    เฎฃเ ฌฌ  ฎฏเฅคฅซฅญจ๏ แขฎกฎคญฎฃฎ จ ฎก้ฅฃฎ ฏเฎแโเ ญแโข  ญ  คจแชฅ.   บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Disk_Space
PARAMETERS drv, free_space, full_space
PRIVATE dir_sav, n_p, free_w
n_p = PARAMETERS()

dir_sav = SET("DEFAULT")+CURDIR()
SET DEFAULT TO (drv)

free_w = DISKSPACE()

IF n_p = 1
  SET DEFAULT TO (dir_sav)
  RETURN free_w
ENDIF

free_space = free_w

IF n_p = 3
  full_space = VAL(SYS(2020))
ENDIF

SET DEFAULT TO (dir_sav)

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ          เฎๆฅคใเ  แฎงค ญจ๏ ฎชฎญ คซ๏ BROWSE.                          บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE D_Wins
PARAMETERS ln, wd, m_tit, l_tit, l_foot
PRIVATE ln1, l, c, l_tit_w, l_foot_w

l_tit_w  = IIF(TYPE("l_tit")  = "N", l_tit,  0)
l_foot_w = IIF(TYPE("l_foot") = "N", l_foot, 0)
l_foot_w = IIF(l_foot_w = 0, 0, l_foot_w+2)

ln1 = ln+3+l_tit_w+l_foot_w
ln1 = MIN(ln1, WROWS("")-3)

l = CEILING((WROWS("")-ln1)/2)
c = FLOOR((WCOLS("")-wd-4)/2)

*
*    ‘ฎงค ฅฌ จ ง ฏฎซญ๏ฅฌ ฎชญฎ - ฏฎคซฎฆชใ
*
DEFINE WINDOW (win_name+"_M") FROM l, c TO l+ln1-1, c+wd+3 NONE SHADOW COLOR SCHEME 13
ACTIVATE WINDOW (win_name+"_M")
@ 0, 0 SAY PADC(m_tit,WCOLS()) COLOR SCHEME 5

@ l_tit_w+1, 1 TO WROWS()-l_foot_w-1, WCOLS()-2
@ WROWS()-l_foot_w-1, 2 FILL TO WROWS()-l_foot_w-1, WCOLS()-2 COLOR (SCHEME(13,6))
@ l_tit_w+1, WCOLS()-2 FILL TO WROWS()-l_foot_w-1, WCOLS()-2 COLOR (SCHEME(13,6))

IF l_foot_w # 0
  @ WROWS()-l_foot_w, 1 TO WROWS()-1, WCOLS()-2
  @ WROWS()-1, 2 FILL TO WROWS()-1, WCOLS()-2 COLOR (SCHEME(13,6))
  @ WROWS()-l_foot_w, WCOLS()-2 FILL TO WROWS()-1, WCOLS()-2 COLOR (SCHEME(13,6))
ENDIF

*
*  ‘ฎงค ฅฌ ขญฅ่ญฅฅ จ ขญใโเฅญญฅฅ ฎชญ  คซ๏ BROWSE.
*
l   = l+2+l_tit_w
c   = c+2
ln1 = ln1-3-l_foot_w-l_tit_w

DEFINE WINDOW (win_name+"_E") FROM l, c TO l+ln1-1, c+wd-1 NONE COLOR SCHEME 13
DEFINE WINDOW (win_name+"_I") FROM l-3, c-2 TO l+ln1, c+wd+20   ;
                           NONE COLOR SCHEME 13
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ          เฎๆฅคใเ  แฎงค ญจ๏ ญ ขจฃ ๆจฎญญฎฉ แเฅค๋ คซ๏ BROWSE.           บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Prp_Nav_1

ACTIVATE SCREEN
IF EMPTY(menu_name)
  @ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
ELSE
  @ 0, 0 SAY PADL("ฅญ๎ - F10 ",WCOLS()) COLOR SCHEME 13
  SHOW MENU (menu_name)
  ON KEY LABEL F10 DO To_Menu
ENDIF

ON KEY LABEL LeftMouse DO Mouse
ACTIVATE WINDOW (win_name+"_E")

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                เฎๆฅคใเ   ชโจขจง ๆจจ  แจญๅเฎญญฎฃฎ ฌฅญ๎.               บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE To_Menu

IF EMPTY(menu_name)
  RETURN
ENDIF

PUSH KEY CLEAR
ACTIVATE MENU (menu_name)
DEACTIVATE MENU (menu_name)
POP KEY

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                    เฎๆฅคใเ  - ฎกเ กฎโ็จช ฌ๋่ชจ.                      บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Mouse
PRIVATE ml, mc, s

PUSH KEY CLEAR

IF INLIST(stat_type, 2, 3)   && BROWSE
  *
  *     ฎคญฎฉ จง แโเฎช Browse
  *
  ml = MROW(win_name+"_E")
  mc = MCOL(win_name+"_E")
  IF ml >= 0 .AND. mc >= 0
    IF stat_type = 3         && ซ๏ ฌฅญ๎ ฏเฎขฅเ๏ฅฌ ญ  คขฎฉญฎฅ ญ ฆ โจฅ
      s = SECONDS()
      IF s - last_mouse < _DBLCLICK
        KEYBOARD CHR(13)
      ELSE
        last_mouse = s
      ENDIF
    ENDIF
    POP KEY
    RETURN
  ENDIF

  ml = MROW("")
  mc = MCOL("")
  IF BETWEEN(mc, WLCOL(win_name+"_E"), WLCOL(win_name+"_E")+WCOLS(win_name+"_E")-1)
    IF ml = WLROW(win_name+"_E")-1
      = INKEY("M")
      KEYBOARD "{PgUp}"
      POP KEY
      RETURN
    ENDIF
    IF ml = WLROW(win_name+"_E")+WROWS(win_name+"_E")
      = INKEY("M")
      KEYBOARD "{PgDn}"
      POP KEY
      RETURN
    ENDIF
  ENDIF
  IF ml = 0 .AND. .NOT. EMPTY(menu_name)
    DO To_Menu
  ELSE
    = INKEY("M")
  ENDIF
  POP KEY
  RETURN
ENDIF
IF stat_type = 1       && ซ ญช
  IF MROW() < 0 .OR. MCOL() < 0
    = INKEY("M")
  ENDIF
ENDIF
POP KEY
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                    เฎๆฅคใเ  ฏฅเฅชซ๎็ฅญจ๏ เฅฆจฌ .                     บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Sw_Mode
PARAMETERS mode

what_do = mode
KEYBOARD CHR(23)
IF .NOT. EMPTY(menu_name)
  DEACTIVATE MENU (menu_name)
ENDIF

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ          เฎๆฅคใเ  แฎงค ญจ๏ ฎชญ  คซ๏ กซ ญช .                          บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE D_Win_N
PARAMETERS ln, wd, m_tit
PRIVATE l, c

l = CEILING((WROWS("")-ln)/2)
c = FLOOR((WCOLS("")-wd)/2)

*
*    ‘ฎงค ฅฌ จ ง ฏฎซญ๏ฅฌ ฎชญฎ
*
DEFINE WINDOW (win_name) FROM l, c TO l+ln-1, c+wd-1 NONE SHADOW COLOR SCHEME 13
ACTIVATE WINDOW (win_name)
@ 0, 0 SAY PADC(m_tit,WCOLS()) COLOR SCHEME 5

@ 1, 1 TO WROWS()-1, WCOLS()-2
@ WROWS()-1, 2 FILL TO WROWS()-1, WCOLS()-2 COLOR (SCHEME(13,6))
@ 1, WCOLS()-2 FILL TO WROWS()-1, WCOLS()-2 COLOR (SCHEME(13,6))

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ          เฎๆฅคใเ  แฎงค ญจ๏ ญ ขจฃ ๆจฎญญฎฉ แเฅค๋ คซ๏ กซ ญช .           บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Prp_Nav_2

ACTIVATE SCREEN
@ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
ON KEY LABEL LeftMouse DO Mouse

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ              เฎๆฅคใเ  ฏเฎเจแฎขชจ เ ฌชจ แ ง ฃฎซฎขชฎฌ.                 บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Draw_Box
PARAMETERS l1, c1, l2, c2, tit

@ l1, c1        TO l2, c2
@ l2, c1+1 FILL TO l2, c2 COLOR (SCHEME(13,6))
@ l1, c2   FILL TO l2, c2 COLOR (SCHEME(13,6))

IF TYPE("tit") = "C"
  @ l1, c1+2 SAY tit
ENDIF

RETURN
*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Tst_Date      งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                      เฎขฅเช  ชฎเเฅชโญฎแโจ ค โ๋.                       บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 01/11/1999 ออผ
PROCEDURE Tst_Date
PRIVATE mss

DO CASE
CASE EMPTY(SYS_DAT)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"  ‘ฅฃฎคญ๏ "+DTOC(DATE())+". โฎ ฏเ ขจซ์ญฎ? [   ] [ ฅโ ]"
  mss(3) = ""
  RETURN Out_Mess(7,"mss") = 1
CASE BETWEEN(DATE()-SYS_DAT, 0, 40)
  RETURN .T.
OTHERWISE
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"ฎแซฅคญจฉ เ ง ๋ เ กฎโ ซจ "+DTOC(SYS_DAT)+",   แฅฃฎคญ๏ "+DTOC(DATE())+"."
  mss(3) = CHR(0)+"  โฎ ฏเ ขจซ์ญฎ? [   ] [ ฅโ ]"
  mss(4) = ""
  RETURN Out_Mess(7,"mss") = 1
ENDCASE



&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ Sto_S         งเ กฎโ็จช €ญคเฅฉ  แจญ           25.05.98 15:52:42 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ            ฅเฅค ็  ญฎฌฅญชซ โใเ๋ ข ไจซจ ซ (จซจ ฅ้ฅ ชใค ...).           บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Sto_S
PARAMETER f_code, _s_usd0, _s_rub0, _abs_d0, _abs_r0, _abs_t0, ;
          _s_qnt0, path0
                           && ฎค ไจซจ ซ  จซจ 0, ฅแซจ กฅง ไจซจ ซ , ฅแซจ 
                           && ฏ เ ฌฅโเ ฎโแใโแโขใฅโ, โฎ แฏเฎแจโ์...
PRIVATE ask_f, is_f, code_wk, name_w, s_sav, __abs_r, __abs_d, __abs_t
PRIVATE f_nom, mes
__abs_r = .F.

f_nom = FOPEN("Z:\STORE\LOG\S_P.TXT",1)
= FSEEK(f_nom,0,2)
mes = DTOC(DATE())+" "+TIME()+" ‘โ เโ ฏเฎฃเ ฌฌ๋ Sto_S "
= FPUTS(f_nom,mes)

IF TYPE("_abs_r0") = "N"
  __abs_r = _abs_r0 = 1
ENDIF

__abs_d = .F.
IF TYPE("_abs_d0") = "N"
  __abs_d = _abs_d0 = 1
ENDIF
__abs_t = 0
IF TYPE("_abs_r0") = "N"
  __abs_t = _abs_t0
ENDIF
IF TYPE("f_code") = "N"
  ask_f = .F.
  is_f  = f_code # 0
  code_wk = f_code
ELSE
  ask_f = .T.
  is_f  = .F.
  code_wk = 0
ENDIF

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
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ‘ฎคฅเฆ โฅซ์ญ ๏ ็ แโ์ ฏเฎฃเ ฌฌ๋:
*
PRIVATE c_list, n_list, l_list
s_sav = SELECT()
SELECT 0
USE (base_path+"RYN_L_T") ORDER TAG CUS_CODE ALIAS TIT25598

= FSEEK(f_nom,0,2)
mes = DTOC(DATE())+" "+TIME()+" โชเ๋โ ไ ฉซ RYN_L_T"
= FPUTS(f_nom,mes)

l_list = 0
name_w = "                "
SCAN
  l_list = l_list + 1
  DIMENSION c_list(l_list), n_list(l_list)
  c_list(l_list) = CUS_CODE
  n_list(l_list) = CUS_NAME
  IF code_wk = CUS_CODE
    name_w = CUS_NAME
  ENDIF
ENDSCAN
IF l_list = 0    && ฅโ แฏจแชฎข? ’ ช โฎฌใ จ ก๋โ์!
  ask_f = .F.
  is_f  = .F.
  code_wk = 0
ENDIF
IF EMPTY(name_w)
  is_f = .F.
  code_wk = 0
ENDIF

*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ ฺฤ ”จซจ ซ ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ ณ
*ณ ณ       [ ] …แโ์   ....:....!....:.       ณ ณ
*ณ ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู ณ
*ณ ฺฤ –ฅญ๋ ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ ณ
*ณ ณ     [ ] ฎซซ เฎข๋ฅ     [ ] ใกซฅข๋ฅ     ณ ณ
*ณ ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู ณ
*ณ ฺฤ –ฅญ๋ ญ  ฎโแใแโข. ฏฎงจๆจจ ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ ณ
*ณ ณ     [ ] ฎซซ เฎข๋ฅ     [ ] ใกซฅข๋ฅ     ณ ณ
*ณ ณ       ( ) ฏโฎข๋ฅ  ( ) ฎงญจ็ญ๋ฅ        ณ ณ
*ณ ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู ณ
*ณ ฺฤ  ซจ็จฅ ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ ณ
*ณ ณ ( ) ฎซจ็ฅแโขฎ  ( ) ฎฌฅโชจ  ( ) จ็ฅฃฎ ณ ณ
*ณ ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู ณ
*ณ ฺฤ ฎฌฅญชซ โใเ  ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ ณ
*ณ ณ   [ ] ”จซ์โเ ฏฎ เ งคฅซ ฌ (  ‘ญ๏โ    )   ณ ณ
*ณ ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู ณ
*ณ      < OK Ctrl-W > < โช ง โ์แ๏ Esc >       ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

PRIVATE ex, __s_usd, __s_rub, __s_qnt, __s_div, ch_div, div_lst, div_cod
*
*   งคฅซ๋ ญฎฌฅญชซ โใเ๋
*

__s_div = ""   && แฅ เ งคฅซ๋
SELECT 0
USE (base_path+"DIVISION") ALIAS DIV28598 AGAIN

= FSEEK(f_nom,0,2)
mes = DTOC(DATE())+" "+TIME()+" โชเ๋โ ไ ฉซ DIVISION"
= FPUTS(f_nom,mes)

ex = 0
SCAN
  ex = ex+1
  DIMENSION div_lst(ex), div_cod(ex)
  div_lst(ex) = "๛"+B_NAME
  div_cod(ex) = CODE
ENDSCAN
USE
__s_usd = .F.
IF TYPE("_s_usd0") = "N"
  __s_usd = _s_usd0 = 1
ENDIF
__s_rub = .F.
IF TYPE("_s_rub0") = "N"
  __s_rub = _s_rub0 = 1
ENDIF
__s_qnt = 2
IF TYPE("_s_qnt0") = "N"
  __s_qnt = _s_qnt0
ENDIF
ch_div = .F.
ex   = 1

*------------------------------------------------------------------------

IF TYPE("path0") # "C"
  PUSH KEY CLEAR       &&   ขแ๏ชจฉ ฏฎฆ เญ๋ฉ แซใ็ ฉ!
  DO Prp_Nav_2
  DO D_Win_N WITH 20, 49, "ฅเฅค ็  ญฎฌฅญชซ โใเ๋ จ ญ ซจ็จ๏"
  DO Sun_Bord WITH  2, 3,  4, 45, " ”จซจ ซ "
  DO Sun_Bord WITH  5, 3,  7, 45, " –ฅญ๋ "
  DO Sun_Bord WITH  8, 3, 11, 45, " –ฅญ๋ ญ  ฎโแใแโข. ฏฎงจๆจจ "
  DO Sun_Bord WITH 12, 3, 14, 45, "  ซจ็จฅ "
  DO Sun_Bord WITH 15, 3, 17, 45, " ฎฌฅญชซ โใเ  "
  @ 16, 30 SAY "(  ‘ญ๏โ    )"
  @ 3, 22 SAY name_w
  *------------------------------------------------------------------------
  *      ขฎค ฏฎซฅฉ กซ ญช 
  *
  @  3, 11 GET is_f PICTURE "@*C …แโ์" WHEN ask_f VALID Ch_Fil()
  @  6,  9 GET __s_usd PICTURE "@*C ฎซซ เฎข๋ฅ"
  @  6, 28 GET __s_rub PICTURE "@*C ใกซฅข๋ฅ"
  @  9,  9 GET __abs_d PICTURE "@*C ฎซซ เฎข๋ฅ"
  @  9, 28 GET __abs_r PICTURE "@*C ใกซฅข๋ฅ"
  @ 10, 11 GET __abs_t  PICTURE "@*RH ฏโฎข๋ฅ ;ฎงญจ็ญ๋ฅ"
  @ 13,  5 GET __s_qnt PICTURE "@*RH ฎซจ็ฅแโขฎ ;ฎฌฅโชจ ;จ็ฅฃฎ"
  @ 16,  7 GET ch_div  PICTURE "@*C ”จซ์โเ ฏฎ เ งคฅซ ฌ" VALID Ch_Div()
  @ 18,  8 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? โช ง โ์แ๏ Esc "

  READ CYCLE
ENDIF

= FSEEK(f_nom,0,2)
mes = DTOC(DATE())+" "+TIME()+" ๋งฎข Send (ฅแซจ ฅๅ=1), ฅๅ = "+STR(ex)
= FPUTS(f_nom,mes)
= FCLOSE(f_nom)

IF ex = 1
  DO Send
ENDIF
*--------------------------------------------------------------------------

IF TYPE("path0") # "C"
  POP KEY
  RELEASE WINDOW (win_name)
ENDIF
SELECT TIT25598
USE
SELECT (s_sav)

RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                           ฅเฅ็ฅญ์ เ งคฅซฎข.                           บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 26.05.98 13:56:22 อผ
PROCEDURE Ch_Div
*
*  ฅเฅฌฅญญ๋ฅ แฎแโฎ๏ญจ๏ คซ๏ ญ ขจฃ ๆจจ
*
PRIVATE i
IF .NOT. ch_div
  RETURN .T.
ENDIF

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
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ‘ฎคฅเฆ โฅซ์ญ ๏ ็ แโ์ ฏเฎฃเ ฌฌ๋:
*

PRIVATE point, sw, arr_w
DIMENSION arr_w(ALEN(div_lst))
FOR i = 1 TO ALEN(div_lst)
  arr_w(i) = div_lst(i)
ENDFOR
*------------------------------------------------------------------------

PUSH KEY CLEAR       &&   ขแ๏ชจฉ ฏฎฆ เญ๋ฉ แซใ็ ฉ!
DO Prp_Nav_2
DO D_Win_N WITH LEN(arr_w(1))+4, MIN(ALEN(arr_w)+3, 15), " งคฅซ๋"

*------------------------------------------------------------------------
*      ขฎค ฏฎซฅฉ กซ ญช 
*
point = 1
DO WHILE .T.
  point = MAX(1, point)
  point = MIN(point, ALEN(arr_w))
  @ 1, 1 MENU arr_w, ALEN(arr_w), WROWS()-3
  READ MENU TO point
  IF READKEY() = 15
    IF LASTKEY() = 13
      point = 1
      EXIT
    ENDIF
    IF LEFT(arr_w(point),1) = " "
      arr_w(point) = "๛"+SUBSTR(arr_w(point), 2)
    ELSE
      arr_w(point) = " "+SUBSTR(arr_w(point), 2)
    ENDIF
    point = point+1
    LOOP
  ENDIF
  IF READKEY() = 12
    point = 0
    EXIT
  ENDIF
ENDDO  

IF point = 1
  __s_div = "!"
  sw = .T.
  FOR i = 1 TO ALEN(arr_w)
    IF LEFT(arr_w(i),1) = " "
      sw = .F.
    ELSE
      __s_div = __s_div+ALLTRIM(STR(div_cod(i)))+"!"
    ENDIF
  ENDFOR
  IF sw
    __s_div = ""
  ENDIF
  FOR i = 1 TO ALEN(div_lst)
    div_lst(i) = arr_w(i)
  ENDFOR
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
ch_div = .F.
SHOW GET ch_div
@ 16, 30 SAY IIF(EMPTY(__s_div), "(  ‘ญ๏โ    )", "(“แโ ญฎขซฅญ)")
RETURN .T.

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                           ฅเฅ็ฅญ์ ไจซจ ซฎข.                           บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 26.05.98 13:56:22 อผ
PROCEDURE Ch_Fil
*
*  ฅเฅฌฅญญ๋ฅ แฎแโฎ๏ญจ๏ คซ๏ ญ ขจฃ ๆจจ
*
PRIVATE i
IF .NOT. is_f
  name_w = SPACE(LEN(n_list(1)))
  code_wk = 0
  @ 3, 22 SAY name_w
  RETURN .T.
ENDIF

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
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ‘ฎคฅเฆ โฅซ์ญ ๏ ็ แโ์ ฏเฎฃเ ฌฌ๋:
*

PRIVATE point, sw
*------------------------------------------------------------------------

point = MAX(1, ASCAN(c_list,code_wk))

PUSH KEY CLEAR       &&   ขแ๏ชจฉ ฏฎฆ เญ๋ฉ แซใ็ ฉ!
DO Prp_Nav_2
DO D_Win_N WITH MIN(ALEN(n_list)+3, 15), LEN(n_list(1))+4, "”จซจ ซ๋"

*------------------------------------------------------------------------
*      ขฎค ฏฎซฅฉ กซ ญช 
*
point = 1
DO WHILE .T.
  point = MAX(1, point)
  point = MIN(point, ALEN(n_list))
  @ 1, 1 MENU n_list, ALEN(n_list), WROWS()-3
  READ MENU TO point
  IF READKEY() = 15
    name_w = n_list(point)
    code_wk = c_list(point)
    EXIT
  ENDIF
  IF READKEY() = 12
    is_f = .F.
    SHOW GET is_f
    EXIT
  ENDIF
ENDDO  

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
@ 3, 22 SAY name_w

RETURN .T.

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ            ฅฏฎแเฅคแโขฅญญฎ ฏฎคฃฎโฎขช  จ ฏฅเฅแ๋ซช  ญ ซจ็จ๏.             บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 28.05.98 17:36:40 อผ
PROCEDURE Send

PRIVATE cnt, pr_d, pr_r, pr_df, pr_rf, q_f, tmpw_path, f_nom, mes

f_nom = FOPEN("Z:\STORE\LOG\S_P.TXT",1)
= FSEEK(f_nom,0,2)
mes = DTOC(DATE())+" "+TIME()+" ‘โ เโ Send: ’จฏ path0 = "+TYPE("path0")
= FPUTS(f_nom,mes)

IF TYPE("path0") = "C"
  tmpw_path = path0
ELSE
  tmpw_path = tmpo_path
ENDIF
USE (base_path+"RYN_L") ALIAS RL28598 AGAIN ORDER TAG C_CODE
SELECT 0
DELETE FILE (tmpw_path+"STO_S.DBF")
DELETE FILE (tmpw_path+"STO_S.FPT")
CREATE DBF (tmpw_path+"STO_S")  ;
  ( CODE      N( 7),;
    PRO_CODE  N( 5),;
    PRODUCER  C( 8),;
    CLASS     C(12),;
    PREFIX    C( 4),;
    NAME      C(35),;
    NUM_PART  C(35),;
    DIV_CODE  N( 3),;
    QNT       N( 6),;
    QNT_STORE N( 6),;
    PRICE_B   N(10,4),;
    PRICE_B_R N(8,2),;
    PRICE     N(10,4),;
    PRICE_R   N(8,2),;
    QNT_ROUND N( 6),;
    DATE_ON   D,    ;
    DATE_OFF  D,    ;
    NOTE      M,    ;
    SPEC_PRC  C( 1) )
SELECT 0
USE (path_comm+"SUBJECT") ORDER TAG STO_CODE ALIAS SJ2607 AGAIN
SELECT 0
USE (base_path+"STOCK") ALIAS ST28598 AGAIN
SET RELATION TO CODE INTO SJ2607
IF code_wk # 0
  SET RELATION TO STR(code_wk,5)+STR(CODE,7) INTO RL28598 ADDITIVE
ENDIF

cnt = 0
IF TYPE("path0") # "C"
  DO Wt_Mess WITH "ฎคฃฎโฎขช  ญฎฌฅญชซ โใเ๋."
ENDIF
SCAN
  IF TYPE("path0") # "C"
    cnt = cnt+1
    @ WROWS()-1, 0 SAY PADC(ALLTRIM(STR(cnt)), WCOLS())
  ENDIF
  IF .NOT. EMPTY(__s_div)
    IF EMPTY(code_wk)
      IF .NOT. ("!"+ALLTRIM(STR(ST28598.DIV_CODE))+"!" $ __s_div)
        LOOP
      ENDIF
    ELSE
      IF .NOT. ("!"+ALLTRIM(STR(ST28598.DIV_CODE))+"!" $ __s_div) .AND. ;
         .NOT. FOUND("RL28598")
        LOOP
      ENDIF
    ENDIF
  ENDIF
  pr_d  = IIF(__s_usd, PRICE, 0)
  pr_r  = IIF(__s_rub, PRICE_R, 0)
  pr_df = 0
  pr_rf = 0
  q_f   = 0
  IF code_wk # 0
    IF FOUND("RL28598")
      pr_df = RL28598.PRICE_D
      pr_rf = RL28598.PRICE_R
      q_f   = RL28598.QNT
    ENDIF
  ENDIF
  IF .NOT. FOUND("RL28598")
    IF __abs_d .OR. __abs_r
      IF __abs_t = 1
        pr_df = ST28598.PRICE
      ELSE
        pr_df = RetPrice(ST28598.NAME, ST28598.PRICE, EMPTY(ST28598.STOP_PUR), IIF(EMPTY(ST28598.CLASS), ST28598.DIV_CODE, ST28598.CLASS))
      ENDIF
      IF __abs_r
        pr_rf = Clc_Rub(pr_df)
      ENDIF
      IF .NOT. __abs_d
        pr_df = 0
      ENDIF
    ENDIF
  ENDIF
  q = 0
  IF __s_qnt = 1
    q = QNT
  ELSE
    IF __s_qnt = 2
      q = IIF(QNT > 0, -99999, 0)
    ENDIF
  ENDIF
  SELECT STO_S
  APPEND BLANK
  REPLACE CODE      WITH ST28598.CODE,     ;
          PRO_CODE  WITH ST28598.PRO_CODE, ;
          PRODUCER  WITH ST28598.PRODUCER, ;
          CLASS     WITH ST28598.CLASS,    ;
          PREFIX    WITH ST28598.PREFIX,   ;
          NAME      WITH ST28598.NAME,     ;
          NUM_PART  WITH ST28598.NUM_PART, ;
          DIV_CODE  WITH ST28598.DIV_CODE, ;
          QNT       WITH q_f,              ;
          QNT_STORE WITH q,                ;
          PRICE_B   WITH pr_df,            ;
          PRICE_B_R WITH pr_rf,            ;
          PRICE     WITH pr_d,             ;
          PRICE_R   WITH pr_r,             ;
          QNT_ROUND WITH ST28598.QNT_ROUND,;
          DATE_ON   WITH ST28598.DATE_ON,  ;
          DATE_OFF  WITH ST28598.DATE_OFF, ;
          NOTE      WITH ST28598.NOTE,     ;
          SPEC_PRC  WITH SJ2607.NODISC
  SELECT ST28598
ENDSCAN
IF TYPE("path0") # "C"
  DO Wt_Mess
ENDIF
USE
SELECT STO_S
USE
SELECT SJ2607
USE
SELECT RL28598
USE
PRIVATE pth, mss, i000, sz
sz = File_Size(tmpw_path+"STO_S.DBF")+  ;
     File_Size(tmpw_path+"DIVISION.DBF")+  ;
     File_Size(tmpw_path+"DEVELOP.DBF")

= FSEEK(f_nom,0,2)
mes = DTOC(DATE())+" "+TIME()+"  ซจ็จฅ คซ๏ ‘- แไฎเฌจเฎข ญฎ!"
= FPUTS(f_nom,mes)

DO WHILE .T. .AND. TYPE("path0") # "C"

  pth = "\"
  pth = Int_Dir(pth, "๋กฅเจโฅ คจเฅชโฎเจ๎", sz)
  IF EMPTY(pth)
    EXIT
  ENDIF
  IF File_O(pth+"STO_S.DBF") .OR. ;
     File_O(pth+"STO_S_T.DBF") .OR. ;
     File_O(pth+"DIV_S.DBF") .OR. ;
     File_O(pth+"DEV_S.DBF")
    DIMENSION mss(4)
    mss(1) = ""
    mss(2) = CHR(0)+"€ โ ฌ ใฆฅ ฅแโ์ ไ ฉซ๋ แ ญฎฌฅญชซ โใเฎฉ คซ๏ ไจซจ ซ ! —โฎ กใคฅฌ คฅซ โ์?"
    mss(3) = CHR(0)+"   [  ฌฅญจฌ ไ ฉซ๋ ] [ ‘ฌฅญจฌ คจเฅชโฎเจ๎ ] [ โช ฆฅฌแ๏ ]"
    mss(4) = ""
    i000 = Out_Mess(7, "mss")
    IF i000 = 3
      EXIT
    ENDIF
    IF i000 = 2
      LOOP
    ENDIF
  ENDIF
  DO Wt_Mess WITH " ฏจแ์..."
  DELETE FILE (pth+"STO_S.DBF")
  DELETE FILE (pth+"STO_S.FPT")
  COPY FILE (tmpw_path+"STO_S.DBF") TO (pth+"STO_S.DBF")
  COPY FILE (tmpw_path+"STO_S.FPT") TO (pth+"STO_S.FPT")
  USE (base_path+"DIVISION") ALIAS TMP29598
  DELETE FILE (pth+"DIV_S.DBF")
  COPY TO (pth+"DIV_S.DBF")

  = FSEEK(f_nom,0,2)
  mes = DTOC(DATE())+" "+TIME()+" ‘ไฎเฌจเฎข ญ DIV_S.DBF! # C"
  = FPUTS(f_nom,mes)
  
  USE (base_path+"DEVELOP") ALIAS TMP29598
  DELETE FILE (pth+"DEV_S.DBF")
  COPY TO (pth+"DEV_S.DBF")

  = FSEEK(f_nom,0,2)
  mes = DTOC(DATE())+" "+TIME()+" ‘ไฎเฌจเฎข ญ DEV_S.DBF! # C"
  = FPUTS(f_nom,mes)

  USE (base_path+"CLASSES") ALIAS TMP29598
  DELETE FILE (pth+"CLASSES.DBF")
  COPY TO (pth+"CLASSES.DBF")

  = FSEEK(f_nom,0,2)
  mes = DTOC(DATE())+" "+TIME()+" ‘ไฎเฌจเฎข ญ CASS_S.DBF! # C"
  = FPUTS(f_nom,mes)

  USE
  IF code_wk # 0
    SELECT TIT25598
    SEEK code_wk
    DELETE FILE (pth+"STO_S_T.DBF")
    COPY NEXT 1 TO (pth+"STO_S_T.DBF")
  ENDIF
  DO Wt_Mess
  EXIT
ENDDO
IF TYPE("path0") = "C"
  USE (base_path+"DIVISION") ALIAS TMP29598
  DELETE FILE (path0+"DIV_S.DBF")
  COPY TO (path0+"DIV_S.DBF")

  = FSEEK(f_nom,0,2)
  mes = DTOC(DATE())+" "+TIME()+" ‘ไฎเฌจเฎข ญ DIV_S.DBF! = C"
  = FPUTS(f_nom,mes)

  USE (base_path+"DEVELOP") ALIAS TMP29598
  DELETE FILE (path0+"DEV_S.DBF")
  COPY TO (path0+"DEV_S.DBF")

  = FSEEK(f_nom,0,2)
  mes = DTOC(DATE())+" "+TIME()+" ‘ไฎเฌจเฎข ญ DEV_S.DBF! = C"
  = FPUTS(f_nom,mes)

  USE (base_path+"CLASSES") ALIAS TMP29598
  DELETE FILE (path0+"CLASSES.DBF")
  COPY TO (path0+"CLASSES.DBF")

  = FSEEK(f_nom,0,2)
  mes = DTOC(DATE())+" "+TIME()+" ‘ไฎเฌจเฎข ญ DEV_S.DBF! = C"
  = FPUTS(f_nom,mes)

  USE
  IF code_wk # 0
    SELECT TIT25598
    SEEK code_wk
    DELETE FILE (path0+"STO_S_T.DBF")
    COPY NEXT 1 TO (path0+"STO_S_T.DBF")
  ENDIF

  = FSEEK(f_nom,0,2)
  mes = DTOC(DATE())+" "+TIME()+"  ซจ็จฅ คซ๏ ‘- ฏฅเฅค ญฎ!"
  = FPUTS(f_nom,mes)
  = FCLOSE(f_nom)

ENDIF

RETURN