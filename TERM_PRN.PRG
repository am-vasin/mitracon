*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                   บ
*บ                 เฎฃเ ฌฌ  ง ขฅเ่ฅญจ๏ ฏฅ็ โจ.                      บ
*บ                                                                   บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Term_Prn
PARAMETERS ext_menu, dbf_name, c_code, wType, wFirm, wDoc, wDate
*
*  ext_menu - จฌ๏ ขญฅ่ญฅฃฎ ฌฅญ๎;
*  dbf_name - จฌ๏ DBF-ไ ฉซ  - ฅแซจ ญฅโ จซจ ฏใแโฎ - ญฅ ฏเฅคใแฌฎโเฅญ.
*

PRIVATE sel_rem, dbf_n

SET DEVICE TO SCREEN
SET PRINTER TO PRN:

sel_rem = SELECT()
IF TYPE("dbf_name") = "C"
  dbf_n = dbf_name
ELSE
  dbf_n = ""
ENDIF

SELECT PRINT

PRIVATE f_nam, d_nm, pg_l, flt, fnt, ncp, f_term, p_d

f_nam = tmpo_path+"PRINT_"+RIGHT(STR(1000+RECNO(),4),2)+".TXT"
d_nm = DOC_NAME
pg_l = PG_LENG
ncp = COPY_NUM
flt = L_FIELD
fnt = FONT
f_term = F_END
p_d    = MAX(P_UNIT,1)

USE
SELECT (sel_rem)

DO Print_D WITH ext_menu, f_nam, d_nm, pg_l, flt, fnt, ncp, f_term, dbf_n, p_d, ;
                c_code, wType, wFirm, wDoc, wDate

RETURN
