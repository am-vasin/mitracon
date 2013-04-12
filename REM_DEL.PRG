*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Rem_del      Разработчик Андрей Васин           14.01.98 16:24:14 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                      Удаление и упаковка файлов.                       ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Rem_Del

USE (base_path+"PARMS")
d_s = DAT_START

CLEAR
? "SALE - удаление..."
USE (base_path+"SALE") EXCLUSIVE
DELETE ALL FOR DOC_DATE < d_s
?? "   упаковка..."
PACK

? "SALE_TIT - удаление..."
USE (base_path+"SALE_TIT") EXCLUSIVE
DELETE ALL FOR DOC_DATE < d_s
?? "   упаковка..."
PACK

? "RYN_M - удаление..."
USE (base_path+"RYN_M") EXCLUSIVE
DELETE ALL FOR DOC_DATE < d_s
?? "   упаковка..."
PACK

? "RYN_M_T - удаление..."
USE (base_path+"RYN_M_T") EXCLUSIVE
DELETE ALL FOR DOC_DATE < d_s
?? "   упаковка..."
PACK

? "KASSA - удаление..."
USE (base_path+"KASSA") EXCLUSIVE
DELETE ALL FOR DOC_DATE < d_s
?? "   упаковка..."
PACK

? "KASSA_M - удаление..."
USE (base_path+"KASSA_M") EXCLUSIVE
DELETE ALL FOR DOC_DATE < d_s
?? "   упаковка..."
PACK

? "INCMONEY - удаление..."
USE (base_path+"INCMONEY") EXCLUSIVE
DELETE ALL FOR DOC_DATE < d_s
?? "   упаковка..."
PACK

? "ORD_D, ORD_T - удаление..."
USE (base_path+"ORD_D") EXCLUSIVE ORDER TAG DOC_NUM
SELECT 0
USE (base_path+"ORD_T") EXCLUSIVE
SCAN FOR ORD_DATE < d_s .AND. STO_MARK # 0
  SELECT ORD_D
  SEEK ORD_T.LIST_NUM+STR(ORD_T.ORD_NUM,3)
  DELETE REST WHILE LIST_NUM = ORD_T.LIST_NUM .AND. ORD_NUM = ORD_T.ORD_NUM
  SELECT ORD_T
  DELETE
ENDSCAN
?? "   упаковка..."
PACK
USE
SELECT ORD_D
PACK

? "ORDER - удаление..."
USE (base_path+"ORDER") EXCLUSIVE
DELETE ALL FOR ORD_DATE < d_s
?? "   упаковка..."
PACK

? "ORD_TIT - удаление..."
USE (base_path+"ORD_TIT") EXCLUSIVE
DELETE ALL FOR ORD_DATE < d_s
?? "   упаковка..."
PACK

? "ACT_S - удаление..."
USE (base_path+"ACT_S") EXCLUSIVE
DELETE ALL FOR DOC_DATE < d_s .AND. QNT_REST = 0 .AND. REST_REP = 0 .AND. LAST_SALE < d_s
?? "   упаковка..."
PACK

SET ORDER TO DOC_NUM
SELECT 0

? "ACT_S_T - удаление..."
USE (base_path+"ACT_S_T") EXCLUSIVE
SET RELATION TO LEFT(DTOS(DOC_DATE),4)+DOC_NUM INTO ACT_S
DELETE ALL FOR DOC_DATE < d_s .AND. .NOT. FOUND("ACT_S")
?? "   упаковка..."
PACK

CLOSE DATABASES
