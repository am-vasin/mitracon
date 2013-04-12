*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Prep_Tel     Разработчик Андрей Васин           21.07.98 13:44:23 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                  Подготовка номенклатуры для справки.                  ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Prep_Tel
PRIVATE c000, f_name

f_code = "   11"  &&&&&&&&&&&&&&&&&  Внимание! Код розничного магазина!

IF File_O(base_path+"STO_DEM.DBF")
  PRIVATE err_lst, err_cod
  DIMENSION err_lst(1)
  err_lst(1) = 1705
  err_cod = 0
  USE (base_path+"STO_DEM")
  IF err_cod # 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Файл номенклатуры для телефона занят, обновление запрещено!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN
  ENDIF
  USE
ENDIF

f_name = Int_File("", "DBF", "Номенклатура")

IF EMPTY(f_name)
  RETURN
ENDIF
DELETE FILE (base_path+"STO_DEM.DBF")
DELETE FILE (base_path+"STO_DEM.CDX")

USE (base_path+"STOCK") ORDER TAG NAME
COPY STRUCTURE TO (base_path+"STO_DEM") WITH CDX
SELECT 0
USE (base_path+"STO_DEM") EXCLUSIVE
REINDEX  && На всякий пожарный случай: COPY ... WITH CDX иногда шалит...
SELECT 0
USE (f_name) EXCLUSIVE ALIAS SOURCE
INDEX ON CODE TAG CODE OF (tmpo_path+"TMP_CDX")

SELECT STOCK
SET RELATION TO CODE INTO SOURCE
c000 = 0
DO Wt_Mess WITH "Подготовка номенклатуры..."
SCAN
  c000 = c000+1
  @ WROWS()-1, 0 SAY PADC(ALLTRIM(STR(c000)),WCOLS())
  SCATTER MEMVAR
  m.qnt_store = m.qnt
  m.price_b   = m.price
  m.price_b_r = m.price_r
  IF FOUND("SOURCE")
    m.qnt = SOURCE.QNT
    m.price = SOURCE.PRICE
    m.price_r = SOURCE.PRICE_R
  ELSE
    m.qnt = 0
    m.price = RETPRICE(m.name, m.price_b, EMPTY(m.stop_pur), IIF(EMPTY(m.class), m.div_code, m.class))
    m.price_r = Clc_Rub(m.price)
  ENDIF
  SELECT STO_DEM
  APPEND BLANK
  GATHER MEMVAR
  SELECT STOCK
ENDSCAN
DO Wt_Mess
CLOSE DATA
DELETE FILE (tmpo_path+"TMP_CDX.CDX")
RETURN
