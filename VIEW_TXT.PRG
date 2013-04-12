*╔═══════════════════════════════════════════════════════════════════╗
*║                                                                   ║
*║                   Программа просмотра файла.                      ║
*║                                                                   ║
*╚═══════════════════════════════════════════════════════════════════╝
PROCEDURE View_txt
PARAMETER f_name, d_titl, prn_p_len, lft_f, fnt
*
*   f_name    - полное имя файла с расширением и, если требуется с путем;
*   d_titl    - имя документа;
*   prn_p_len - длина страницы;
*   lft_f     - левое поле;
*   fnt       - шрифт.
*
PRIVATE nf, buf, fl_p, j, i, k, n_pgs, scale_h, scale_v, sav_sel, lf, fp

PRIVATE font    && заголовок документа и фонт: 0 - норм.,
                        &&       1 - condenced
sav_sel = SELECT()

IF TYPE("fnt") = "C"
  font = IIF(fnt=" ", 0, 1)
ELSE
  font = 0
ENDIF

nf = FOPEN(f_name)

IF FILE (tmpo_path+"DOC_STRU.DBF")
  DELETE FILE (tmpo_path+"DOC_STRU.DBF")
ENDIF

CREATE DBF (tmpo_path+"DOC_STRU") ( P_POS N(10) )

APPEND BLANK

lf = 0   && строка на странице
fp = 0   && Позиция в файле
k = 0
DO WHILE .NOT. FEOF(nf)
  buf = FGET(nf, 1024)
  lf = lf+1
  fp = fp+LEN(buf)+IIF(RIGHT(buf,1) = "", 1, 2)
  IF RIGHT(buf,1) = "" .OR. lf = prn_p_len
    lf = 0
    APPEND BLANK
    REPLACE P_POS WITH fp
  ENDIF
  k = k+1
ENDDO

n_pgs = RECCOUNT()
IF RIGHT(buf,2) = CHR(12)+CHR(13)
  DELETE
  n_pgs = n_pgs-1
ENDIF
PRIVATE ln_w, l_w, c_w, wd_w, prn_p_wd

prn_p_wd = IIF(font = 0, 80, 132)
ln_w = MIN(WROWS("")-5, prn_p_len+4)
wd_w = MIN(WCOLS("")-4, prn_p_wd+4)

l_w = CEILING((WROWS("")-ln_w)/2)
c_w = FLOOR((WCOLS("")-wd_w)/2)

DEFINE WINDOW View_M FROM l_w, c_w TO l_w+ln_w-1, c_w+wd_w-1    ;
                                      NONE SHADOW COLOR SCHEME 13
DEFINE WINDOW View_E FROM l_w+2, c_w+2 TO l_w+ln_w-3, c_w+wd_w-3   ;
                                      NONE COLOR SCHEME 13
*DEFINE WINDOW View_I FROM l_w+1, c_w+2 TO l_w+prn_p_len+1, c_w+prn_p_wd+1   ;
*                                      NONE SHADOW COLOR SCHEME 10
DEFINE WINDOW View_I FROM 0, 0 TO prn_p_len-1, prn_p_wd-1   ;
                                      NONE SHADOW COLOR SCHEME 13
ACTIVATE WINDOW View_M
@ 0, 0 SAY PADC(ALLTRIM(d_titl),WCOLS()) COLOR SCHEME 5
@ 1, 1 TO WROWS()-2, WCOLS()-2
k = "PgDn/PgUp - след./пред. страница, стрелки - перемещ. текста, Esc - выход."
@ WROWS()-1, 0 SAY PADC(k, WCOLS())
ACTIVATE WINDOW View_E
ACTIVATE WINDOW View_I IN WINDOW View_E
scale_h = ""
k = 1
DO WHILE LEN(scale_h) < WCOLS()
  scale_h = scale_h+"∙∙∙∙∙∙∙∙"+RIGHT(STR(k),1)
  k = k+1
ENDDO
DIMENSION scale_v(WROWS())
FOR k = 1 TO WROWS()
  IF MOD(k,5) # 0
    scale_v(k) = " ∙"
  ELSE
    IF MOD(k,10) # 0
      scale_v(k) = " "
    ELSE
      scale_v(k) = LEFT(STR(k,3),2)
    ENDIF
  ENDIF
ENDFOR
fl_p = 1
DO Prep_Pg
PRIVATE s, crsr
crsr = SET("CURSOR") = "ON"
SET CURSOR OFF

DO WHILE .T.
  s = INKEY(0,"M")
  DO CASE
  CASE s = 27  && Esc
    RELEASE WINDOW View_M
    RELEASE WINDOW View_E
    RELEASE WINDOW View_I
    = FCLOSE(nf)
    SELECT DOC_STRU
    USE
    SELECT (sav_sel)
    IF crsr
      SET CURSOR ON
    ENDIF
    EXIT
  CASE s = 5   && Up
    IF WLROW("View_I") # WLROW("View_E")
      MOVE WINDOW View_I BY 1, 0
      ACTIVATE WINDOW View_M
      SCROLL 2, 0, 1+WROWS("View_E"), 1, -1
      @ 2, 0 SAY scale_V(WLROW("View_E")-WLROW("View_I")+1) COLOR SCHEME 13
      ACTIVATE WINDOW View_I
    ENDIF
  CASE s = 24  && Dn
    IF WLROW("View_I")+WROWS("View_I") # WLROW("View_E")+WROWS("View_E")
      MOVE WINDOW View_I BY -1, 0
      ACTIVATE WINDOW View_M
      SCROLL 2, 0, 1+WROWS("View_E"), 1, 1
      @ 2+WROWS("View_E")-1, 0 SAY  ;
          scale_v(WLROW("View_E")-WLROW("View_I")+WROWS("View_E")) COLOR SCHEME 13
      ACTIVATE WINDOW View_I
    ENDIF
  CASE s = 19  && Left
    IF WLCOL("View_I") # WLCOL("View_E")
      MOVE WINDOW View_I BY 0, 1
      ACTIVATE WINDOW View_M
      @ 1, 2 SAY SUBSTR(scale_h,WLCOL("View_E")-WLCOL("View_I")+1,WCOLS("View_E"))  ;
                          COLOR SCHEME 13
      ACTIVATE WINDOW View_I
    ENDIF
  CASE s = 4   && Right
    IF WLCOL("View_I")+WCOLS("View_I") # WLCOL("View_E")+WCOLS("View_E")
      MOVE WINDOW View_I BY 0, -1
      ACTIVATE WINDOW View_M
      @ 1, 2 SAY SUBSTR(scale_h,WLCOL("View_E")-WLCOL("View_I")+1,WCOLS("View_E"))  ;
                      COLOR SCHEME 13
      ACTIVATE WINDOW View_I
    ENDIF
  CASE s = 18  && PgUp
    IF fl_p # 1
      fl_p = fl_p-1
      DO Prep_Pg
    ENDIF 
  CASE s = 3   && PgDn
    IF fl_p # n_pgs
      fl_p = fl_p+1
      DO Prep_Pg
    ENDIF 
  ENDCASE
ENDDO

SELECT (sav_sel)
RETURN


PROCEDURE Prep_Pg
PRIVATE s, p, i

CLEAR
GO fl_p
= FSEEK(nf,P_POS)
p = 0
s = FGETS(nf)

DO WHILE .T.    && .NOT. FEOF(nf)
  IF p > WROWS()-1
    EXIT
  ENDIF
  IF RIGHT(s,1) = CHR(12)
    s = LEFT(s,LEN(s)-1)
    @ p, lft_f SAY LEFT(s,WCOLS()-lft_f)
    EXIT
  ENDIF
  @ p, lft_f SAY LEFT(s,WCOLS()-lft_f)
  p = p+1
  IF FEOF(nf)
    EXIT
  ENDIF
  s = FGETS(nf)
ENDDO

ACTIVATE WINDOW View_M
i = PADR("Стр. "+ALLTRIM(STR(fl_p,5))+   ;
                        "/"+ALLTRIM(STR(n_pgs,5)), 12)
@ 0, WCOLS()-14 SAY i COLOR SCHEME 5
@ 1, 1 SAY "┌" COLOR SCHEME 13
@ 1, 2 SAY LEFT(scale_h,WCOLS("View_E")) COLOR SCHEME 13
FOR i = 1 TO WROWS("View_E")
  @ 1+i, 0 SAY scale_v(i) COLOR SCHEME 13
ENDFOR

ACTIVATE WINDOW View_I
MOVE WINDOW View_I TO 0, 0

RETURN
