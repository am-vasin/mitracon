*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл S_Ord1       Разработчик Андрей Васин           19.05.98 15:51:55 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                 Запись заказа в DBF-файл для передачи.                 ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE S_Ord1
PARAMETERS frm00, ord_n, ord_d, fileName, prmPth
*
*  frm00    - предприятие;
*  ord_n    - номер заказа ( внутренний формат );
*  ord_d    - либо дата заказа либо год в символьном или числовом формате.
*  fileName - имя файла заказа;
*  prmPth   - путь к файлу заказа.
*

PRIVATE sel_rem, d_year, n, mss, tmpName, pth, tmpStru, tmpBuff, tmpN, newNote, i

sel_rem = SELECT()

m.tmpName = IIF(.NOT. EMPTY(m.fileName), m.fileName, "ord_s")
m.pth     = IIF(.NOT. EMPTY(m.prmPth),   m.prmPth,   "")
IF TYPE("ord_d") = "D"
  d_year = LEFT(DTOS(ord_d),4)
ENDIF
IF TYPE("ord_d") = "N"
  d_year = STR(ord_d,4)
ENDIF
IF TYPE("ord_d") = "C"
  d_year = ord_d
ENDIF

PRIVATE d_n, d_d, sup_n, cus_n
d_n = IIF(TYPE("ord_n") = "C", PADL(ALLTRIM(ord_n), 10), STR(ord_n,10))

SELECT 0
USE (base_path+"ORD_TIT") ORDER TAG ORD_NUM ALIAS INV_TTT AGAIN
IF .NOT. SEEK(frm00+d_year+d_n)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет заявки с номером "+ALLTRIM(ord_n)+"!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (sel_rem)
  RETURN
ENDIF

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS PRS_TTT AGAIN
SELECT 0
USE (base_path+"LINK_ORD") ORDER TAG ORD_CODE ALIAS LINK_TTT AGAIN
SET RELATION TO MANAGER INTO PRS_TTT
SELECT 0
USE (base_path+"ORDER") ORDER TAG ORD_NUM ALIAS INV_DET AGAIN
*SEEK d_year+d_n
SEEK frm00+d_year+d_n

IF EMPTY(m.pth)
  DO WHILE .T.
    pth = Int_Dir(pth, "Файл заказа ", 0)
    IF EMPTY(pth)
      USE
      SELECT PRS_TTT
      USE
      SELECT LINK_TTT
      USE
      IF USED ("INV_TTT")
        SELECT INV_TTT
        USE
      ENDIF
      SELECT(sel_rem)
      RETURN
    ENDIF
    pth = ALLTRIM(pth)
    IF File_O(pth+m.tmpName+".dbf") .OR. File_O(pth+m.tmpName+"_t.dbf")
      DIMENSION mss(4)
      mss(1) = ""
      mss(2) = CHR(0)+"Там уже есть заявка!, Как поступим?"
      mss(3) = CHR(0)+"   [ Запишем поверх ] [ Запишем в другое место ] [ Откажемся ]"
      mss(4) = ""
      n = Out_Mess(7, "mss")
      IF n = 3
        USE
        SELECT INV_TTT
        USE
        SELECT PRS_TTT
        USE
        SELECT LINK_TTT
        USE
        SELECT(sel_rem)
        RETURN
      ENDIF
      IF n = 2
        LOOP
      ENDIF
    ENDIF
    EXIT
  ENDDO
ENDIF
DELETE FILE (pth+m.tmpName+".dbf")
DELETE FILE (pth+m.tmpName+"_t.dbf")

m.tmpN = AFIELDS(tmpStru)
newNote = .T.
FOR m.i = 1 TO m.tmpN
  IF UPPER(ALLTRIM(tmpStru[m.i, 1])) == "NOTE"
    newNote = .F.
    EXIT
  ENDIF
ENDFOR
IF m.newNote
  m.tmpN = m.tmpN+1
  DIMENSION tmpStru[m.tmpN, 4]
  tmpStru[m.tmpN, 1] = "NOTE"
  tmpStru[m.tmpN, 2] = "C"
  tmpStru[m.tmpN, 3] = 32
  tmpStru[m.tmpN, 4] = 0
ENDIF

SELECT 0
CREATE DBF (pth+m.tmpName+".dbf") FROM ARRAY tmpStru
USE (pth+m.tmpName+".dbf") ALIAS TMP_DET

SELECT INV_DET
SCAN REST WHILE ORD_NUM = d_n .AND. FIRM = frm00 .AND. LEFT(DTOS(ORD_DATE),4) = d_year
  SCATTER TO tmpBuff
  SELECT TMP_DET
  APPEND FROM ARRAY tmpBuff
  IF m.newNote
    IF SEEK(FIRM+LEFT(DTOS(ORD_DATE), 4)+ORD_NUM+STR(CODE), "LINK_TTT")
      IF LINK_TTT.MANAGER # 0 .AND. FOUND("PRS_TTT")
        REPLACE NOTE WITH ALLTRIM(PRS_TTT.FAMILY)+" "+LEFT(PRS_TTT.NAME, 1)+" "+LEFT(PRS_TTT.S_NAME, 1)
      ENDIF
    ENDIF
  ENDIF
  SELECT INV_DET
ENDSCAN

USE
USE IN PRS_TTT
USE IN LINK_TTT
USE IN TMP_DET
SELECT INV_TTT
COPY NEXT 1 TO (pth+m.tmpName+"_t.dbf")
USE

SELECT (sel_rem)

RETURN
