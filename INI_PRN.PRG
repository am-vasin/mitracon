*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                   เฎฃเ ฌฌ  ฏฎคฃฎโฎขชจ ฏเจญโฅเ .                      บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Ini_Prn
PARAMETERS dc_n, p_l, l_f, n_c, fnt, d_term, p_d
*
*  dc_n - จฌ๏ คฎชใฌฅญโ ;
*  p_l  - คซจญ  แโเ ญจๆ๋;
*  l_f  - ซฅขฎฅ ฏฎซฅ;
*  n_c  - ็จแซฎ ชฎฏจฉ;
*  fnt  - ่เจไโ;
*  d_term - ฏเฎฃฎญ แโเฎช ข ชฎญๆฅ คฎชใฌฅญโ  ( -1 - FF ).
*  p_d  - ฏเจญโฅเ
*

PRIVATE i, sel_rem, p_file, w1, w2, w3, w4, w5, w6, w7

IF TYPE("dc_n") = "C"
  w1 = dc_n
ELSE
  w1 = "<< ฅง ญ งข ญจ๏ >>"
ENDIF

IF TYPE("p_l") = "N"
  w2 = p_l
ELSE
  w2 = 66
ENDIF

IF TYPE("l_f") = "N"
  w3 = l_f
ELSE
  w3 = 0
ENDIF

IF TYPE("n_c") = "N"
  w4 = MAX(n_c,1)
ELSE
  w4 = 1
ENDIF

IF TYPE("fnt") = "C"
  w5 = fnt
ELSE
  w5 = "  "
ENDIF

IF TYPE("d_term") = "N"
  w6 = d_term
ELSE
  w6 = -1
ENDIF

IF TYPE("p_d") = "N"
  w7 = p_d
ELSE
  w7 = 1
ENDIF

sel_rem = SELECT()
SELECT 0

DO WHILE .T.
  IF .NOT. FILE(prmo_path+"PRINT.DBF")
    DELETE FILE (prmo_path+"PRINT.CDX")
    CREATE DBF (prmo_path+"PRINT")  ;
               ( PRN_DATE    D( 8), ;
                 PRN_TIME    C( 8), ;
                 DOC_NAME    C(40), ;
                 PG_LENG     N( 3), ;
                 COPY_NUM    N( 2), ;
                 L_FIELD     N( 2), ;
                 FONT        C( 2), ;
                 F_END       N( 3), ;
                 P_UNIT      N( 2)  )
    INDEX ON DTOS(PRN_DATE)+PRN_TIME TAG HRON
    FOR i = 1 TO 10
      APPEND BLANK
    ENDFOR
  ELSE
    USE (prmo_path+"PRINT.DBF") ORDER TAG HRON
  ENDIF
  IF FSIZE("P_UNIT") = 0 .OR. FSIZE("FONT") < 2
    USE
    DELETE FILE (prmo_path+"PRINT.CDX")
    DELETE FILE (prmo_path+"PRINT.DBF")
    LOOP
  ENDIF
  EXIT
ENDDO

GO TOP

REPLACE PRN_DATE WITH DATE(), ;
        PRN_TIME WITH TIME(), ;
        DOC_NAME WITH w1,     ;
        PG_LENG  WITH w2, ;
        COPY_NUM WITH w4, ;
        L_FIELD  WITH w3, ;
        FONT     WITH w5, ;
        F_END    WITH w6, ;
        P_UNIT   WITH w7
p_file = tmpo_path+"PRINT_"+RIGHT(STR(1000+RECNO(),4),2)+".TXT"
SET PRINTER TO (p_file)
SET DEVICE TO PRINTER

SELECT (sel_rem)

RETURN
