*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Rem_del      ��о�｀砒┴ ��むォ ��瓱�           14.01.98 16:24:14 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                      ����キ┘ � 祚���→� ������.                       �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Rem_Del

USE (base_path+"PARMS")
d_s = DAT_START

CLEAR
? "SALE - 磴��キ┘..."
USE (base_path+"SALE") EXCLUSIVE
DELETE ALL FOR DOC_DATE < d_s
?? "   祚���→�..."
PACK

? "SALE_TIT - 磴��キ┘..."
USE (base_path+"SALE_TIT") EXCLUSIVE
DELETE ALL FOR DOC_DATE < d_s
?? "   祚���→�..."
PACK

? "RYN_M - 磴��キ┘..."
USE (base_path+"RYN_M") EXCLUSIVE
DELETE ALL FOR DOC_DATE < d_s
?? "   祚���→�..."
PACK

? "RYN_M_T - 磴��キ┘..."
USE (base_path+"RYN_M_T") EXCLUSIVE
DELETE ALL FOR DOC_DATE < d_s
?? "   祚���→�..."
PACK

? "KASSA - 磴��キ┘..."
USE (base_path+"KASSA") EXCLUSIVE
DELETE ALL FOR DOC_DATE < d_s
?? "   祚���→�..."
PACK

? "KASSA_M - 磴��キ┘..."
USE (base_path+"KASSA_M") EXCLUSIVE
DELETE ALL FOR DOC_DATE < d_s
?? "   祚���→�..."
PACK

? "INCMONEY - 磴��キ┘..."
USE (base_path+"INCMONEY") EXCLUSIVE
DELETE ALL FOR DOC_DATE < d_s
?? "   祚���→�..."
PACK

? "ORD_D, ORD_T - 磴��キ┘..."
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
?? "   祚���→�..."
PACK
USE
SELECT ORD_D
PACK

? "ORDER - 磴��キ┘..."
USE (base_path+"ORDER") EXCLUSIVE
DELETE ALL FOR ORD_DATE < d_s
?? "   祚���→�..."
PACK

? "ORD_TIT - 磴��キ┘..."
USE (base_path+"ORD_TIT") EXCLUSIVE
DELETE ALL FOR ORD_DATE < d_s
?? "   祚���→�..."
PACK

? "ACT_S - 磴��キ┘..."
USE (base_path+"ACT_S") EXCLUSIVE
DELETE ALL FOR DOC_DATE < d_s .AND. QNT_REST = 0 .AND. REST_REP = 0 .AND. LAST_SALE < d_s
?? "   祚���→�..."
PACK

SET ORDER TO DOC_NUM
SELECT 0

? "ACT_S_T - 磴��キ┘..."
USE (base_path+"ACT_S_T") EXCLUSIVE
SET RELATION TO LEFT(DTOS(DOC_DATE),4)+DOC_NUM INTO ACT_S
DELETE ALL FOR DOC_DATE < d_s .AND. .NOT. FOUND("ACT_S")
?? "   祚���→�..."
PACK

CLOSE DATABASES
