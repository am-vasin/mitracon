PROCEDURE Emp_Act
PARAMETERS f_code, n_doc, d_doc

PRIVATE s_sav, tmpYear, is_act, is_inv

DO CASE
CASE TYPE("d_doc") = "D"
  tmpYear = LEFT(DTOS(d_doc), 4)
CASE TYPE("d_doc") = "C"
  tmpYear = d_doc
CASE TYPE("d_doc") = "N"
  tmpYear = STR(d_doc, 4)
OTHERWISE
  tmpYear = LEFT(DTOS(DATE()), 4)
ENDCASE

s_sav = SELECT()
SELECT 0
USE (base_path+"SALE_TIT") SHARED AGAIN ALIAS tmp_Title
IF ISDIGIT(SUBSTR(n_doc, 2, 1))
  SET ORDER TO TAG DOC_NUM
ELSE
  SET ORDER TO TAG NEW_DOC
ENDIF

IF .NOT. SEEK(f_code+tmpYear+n_doc)
  USE
  SELECT (s_sav)
  RETURN "  "
ENDIF

SELECT 0
USE (m.base_path+"STOCK") SHARED AGAIN ORDER TAG CODE ALIAS STO_TEST
SELECT 0
USE (m.base_path+"SALE") SHARED AGAIN ORDER TAG DOC_NUM ALIAS SALE_TEST
SEEK tmp_Title.Firm+LEFT(DTOS(tmp_Title.DOC_DATE), 4)+tmp_Title.DOC_NUM
m.is_act = .F.
m.is_inv = .F.
SCAN REST WHILE tmp_Title.Firm=FIRM .AND. tmp_Title.DOC_DATE=DOC_DATE .AND. tmp_Title.DOC_NUM=DOC_NUM
  = SEEK(CODE, "STO_TEST")
  IF .NOT. EMPTY(STO_TEST.WK_SW2)
    m.is_act = .T.
  ELSE
    m.is_inv = .T.
  ENDIF
  IF m.is_inv .AND. m.is_act
    EXIT
  ENDIF
ENDSCAN
USE IN tmp_Title
USE IN SALE_TEST
USE IN STO_TEST
SELECT (s_sav)
RETURN IIF(m.is_inv, "*", " ")+IIF(m.is_act, "*", " ")
