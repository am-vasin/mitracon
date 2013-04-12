*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Debug_R       งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                   เฎฃเ ฌฌ  เฅฃจแโเ ๆจจ โเ ญง ชๆจฉ.                    บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 04/03/1999 ออผ
PROCEDURE Debug_R
PARAMETERS als, is_add, vl, p_name, p_line, note

* als    -  ซจ แ;
* is_add - ฏเจงญ ช คฎก ขซฅญจฅ/ชฎเเฅชๆจ๏;
* vl     - แใฌฌ ;
* p_name - จฌ๏ ฏเฎฃเ ฌฌ๋;
* p_line - ญฎฌฅเ แโเฎชจ ฏเฎฃเ ฌฌ๋;
* note   - ฏเจฌฅ็ ญจฅ (ญฅฎก๏ง.).

PRIVATE s_sav, db_name, i

IF .NOT. USED("DEBUG_R")
  RETURN
ENDIF

s_sav = SELECT()
SELECT DEBUG_R

GO TOP
IF BOF() .OR. EOF()
  SELECT (s_sav)
  RETURN
ENDIF

db_name = DBF(als)
i = RAT("\", db_name)
IF i # 0
  db_name = SUBSTR(db_name, i+1)
ENDIF

REPLACE D_DATE   WITH DATE(),    ;
        D_TIME   WITH TIME(),    ;
        DBF_NAME WITH db_name,   ;
        MOD_REC  WITH RECNO(als),;
        TOTAL    WITH vl,        ;
        PRG_NAME WITH p_name,    ;
        PRG_LINE WITH p_line,    ;
        REM      WITH IIF(TYPE("note") = "C", note, ""), ;
        WHERE    WITH arm
SELECT (s_sav)

RETURN
