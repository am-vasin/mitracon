*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                      ����キ┘ 竅��爛∵�� ����諷.                      �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Del_Mov

PRIVATE c00, wd, mss0, dt_e, w, arch_path, c_dir

IF .NOT. G_Exclu()    && �� 磴���瘡 ��紜�皋碎 �. �.
  RETURN
ENDIF

dt_e = Get_Dt()

IF .NOT. Is_Dir(base_path+"ARCHIV")
  w = "! MKDIR "+base_path+"ARCHIV"
  &w
ENDIF
arch_path = base_path+"ARCHIV\"
c_dir = arch_path+"a"+SUBSTR(DTOS(dt_e),3)
IF Is_Dir(c_dir)
  DIMENSION mss0(3)
  mss0(1) = ""
  mss0(2) = CHR(0)+"��������! �Ε °�� ����� ���謔��, ��Аム�皀瘡 � 腑� ぅ��!"
  mss0(3) = ""
  DO Out_Mess WITH 7, "mss0"
  DO C_Exclu
  RETURN
ENDIF
w = "! MKDIR "+c_dir
&w
c_dir = c_dir+"\"

IF EMPTY(dt_e)
  DO C_Exclu
  RETURN
ENDIF

************************************************************************
mss0 = "        ��瘍モ ��あ└オ        "

wd = LEN(mss0)+4
c00 = FLOOR((WCOLS("")-wd)/2)

DEFINE WINDOW Title_W FROM 2, c00 TO 4, c00+wd-1 NONE SHADOW COLOR SCHEME 13
ACTIVATE WINDOW Title_W
DO Sun_Bord WITH 0, 1, WROWS()-1, WCOLS()-2
@ 1, 2 SAY mss0
ACTIVATE SCREEN

DO Delta WITH {}, dt_e

RELEASE WINDOW Title_W

************************************************************************
mss0 = "        �璢│�牀���┘ � 磴��キ┘        "

wd = LEN(mss0)+4
c00 = FLOOR((WCOLS("")-wd)/2)

DEFINE WINDOW Title_W FROM 2, c00 TO 6, c00+wd-1 NONE SHADOW COLOR SCHEME 13
ACTIVATE WINDOW Title_W
DO Sun_Bord WITH 0, 1, WROWS()-1, WCOLS()-2
@ 1, 2 SAY mss0

DO Del_Arch WITH c_dir

RELEASE WINDOW Title_W

************************************************************************
mss0 = "        �ムメ����→� �����讚諷 Л�腑�┤        "

wd = LEN(mss0)+4
c00 = FLOOR((WCOLS("")-wd)/2)

DEFINE WINDOW Title_W FROM 2, c00 TO 5, c00+wd-1 NONE SHADOW COLOR SCHEME 13
ACTIVATE WINDOW Title_W
DO Sun_Bord WITH 0, 1, WROWS()-1, WCOLS()-2
@ 1, 2 SAY mss0

DO Mov_Start


RELEASE WINDOW Title_W

************************************************************************
mss0 = "        �����→� ������        "

wd = LEN(mss0)+4
c00 = FLOOR((WCOLS("")-wd)/2)

DEFINE WINDOW Title_W FROM 2, c00 TO 5, c00+wd-1 NONE SHADOW COLOR SCHEME 13
ACTIVATE WINDOW Title_W
DO Sun_Bord WITH 0, 1, WROWS()-1, WCOLS()-2
@ 1, 2 SAY mss0

USE PARMS
REPLACE DAT_START WITH dt_e+1
USE

DO Pk_Files

RELEASE WINDOW Title_W

************************************************************************

DO C_Exclu

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                      ���� 磴��キ�� 竅��爛∵�� ����諷.                 �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Get_Dt

PRIVATE dt, dt_s, ex, win_name

USE PARMS
dt_s = DAT_START
dt = {}
USE

*        ����襯 � あ└キ┬ 縲��閧瘴 � ��/��/��
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�                                                        �
*�    ���Θ皀 ��矣 �� ��皰珮� 磴��錺� ����襯 ��/��/��     �
*�                                                        �
*�            < OK Ctrl-W > < �皖���碎瘴 Esc >            �
*�                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

ex = 1
win_name = PROGRAM()

DO D_Win_N WITH 8, 60, "����襯 � あ└キ┬ 縲��閧瘴 � "+TRANSFORM(dt_s, "@D ")

@ 3, 6 SAY "���Θ皀 ��矣 �� ��皰珮� 磴��錺� ����襯" GET dt PICTURE "@D "
@ 5,14 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "
READ CYCLE VALID Tst_Dt()

RELEASE WINDOW (win_name)

RETURN dt

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                      蹍▲爲� ��玻オ皚�痰� ��硅.                      �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Tst_Dt
PRIVATE mss

IF ex = 2 .OR. EMPTY(dt)
  dt = {}
  RETURN .T.
ENDIF

IF dt < dt_s
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� 轤�� ��硅 �モ ����諷, �ョィ� � 磴��閧�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF dt = DATE()
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"､祠��皀瘡, �� 絎皋皀 磴���碎 ��� ぎ�祠キ硅!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF dt > DATE()
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�皰� ぅ�� ラ� �� ��痰祚┼!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN.T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                      �璢│�牀���┘ � 磴��キ┘.                        �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Del_Arch
PARAMETERS pth    && 鉘� � �璢│��� ぎ爛�皰爬�
PRIVATE mss000, l_date, arr1, arr2


***********************************************************************
mss000 = "踸ガ�襯 ��硅 筥������"
@ WROWS()-3, 2 SAY PADC(mss000, WCOLS()-4)

USE (base_path+"ACT_F") ORDER TAG DOC_NUM ALIAS OLD_D

COPY STRUCTURE TO (pth+"ACT_F")
SELECT 0
USE (pth+"ACT_F") ALIAS NEW_D

SELECT 0
USE (base_path+"ACT_F_T") ALIAS OLD_T

COPY STRUCTURE TO (pth+"ACT_F_T")
SET RELATION TO STR(YEAR(DOC_DATE),4)+STR(CUS_CODE,5)+STR(DOC_NUM,5) INTO OLD_D

SELECT 0
USE (pth+"ACT_F_T") ALIAS NEW_T

SELECT OLD_T
GO TOP
l_date = {}

SCAN FOR DOC_DATE <= dt_e
  IF DOC_DATE # l_date
    l_date = DOC_DATE
    @ WROWS()-2, 2 SAY PADC("���� "+DTOC(l_date), WCOLS()-4)
  ENDIF
  SCATTER TO arr1
  SELECT NEW_T
  APPEND FROM ARRAY arr1
  
  SELECT OLD_D
  SCAN REST WHILE OLD_T.DOC_DATE = DOC_DATE .AND.   ;
                  OLD_T.DOC_NUM  = DOC_NUM .AND.    ;
                  OLD_T.CUS_CODE = CUS_CODE
    
    SCATTER TO arr2
    SELECT NEW_D
    APPEND FROM ARRAY arr2
    SELECT OLD_D

    DELETE
  ENDSCAN
  SELECT OLD_T
  
  DELETE

ENDSCAN

***********************************************************************
mss000 = "踸ガ�襯 ��硅"
@ WROWS()-3, 2 SAY PADC(mss000, WCOLS()-4)

SELECT OLD_D
USE (base_path+"ACT_S") ORDER TAG DOC_NUM ALIAS OLD_D

COPY STRUCTURE TO (pth+"ACT_S")
SELECT NEW_D
USE (pth+"ACT_S") ALIAS NEW_D

SELECT OLD_T
USE (base_path+"ACT_S_T") ALIAS OLD_T

COPY STRUCTURE TO (pth+"ACT_S_T")
SET RELATION TO STR(YEAR(DOC_DATE),4)+STR(DOC_NUM,5) INTO OLD_D

SELECT NEW_T
USE (pth+"ACT_S_T") ALIAS NEW_T

SELECT OLD_T
GO TOP
l_date = {}

SCAN FOR DOC_DATE <= dt_e
  IF DOC_DATE # l_date
    l_date = DOC_DATE
    @ WROWS()-2, 2 SAY PADC("���� "+DTOC(l_date), WCOLS()-4)
  ENDIF
  SCATTER TO arr1
  SELECT NEW_T
  APPEND FROM ARRAY arr1
  
  SELECT OLD_D
  SCAN REST WHILE OLD_T.DOC_DATE = DOC_DATE .AND.   ;
                  OLD_T.DOC_NUM  = DOC_NUM
    
    SCATTER TO arr2
    SELECT NEW_D
    APPEND FROM ARRAY arr2
    SELECT OLD_D

    DELETE
  ENDSCAN
  SELECT OLD_T
  
  DELETE

ENDSCAN

***********************************************************************
mss000 = "��錢�� �� 瓷���"
@ WROWS()-3, 2 SAY PADC(mss000, WCOLS()-4)

SELECT OLD_D
USE (base_path+"ORD_D") ORDER TAG DOC_NUM ALIAS OLD_D

COPY STRUCTURE TO (pth+"ORD_D")
SELECT NEW_D
USE (pth+"ORD_D") ALIAS NEW_D

SELECT OLD_T
USE (base_path+"ORD_T") ALIAS OLD_T

COPY STRUCTURE TO (pth+"ORD_T")
SET RELATION TO LIST_NUM+STR(ORD_NUM,3) INTO OLD_D

SELECT NEW_T
USE (pth+"ORD_T") ALIAS NEW_T

SELECT OLD_T
GO TOP
l_date = {}

SCAN FOR ORD_DATE <= dt_e .AND. .NOT. EMPTY(STO_MARK)
  IF ORD_DATE # l_date
    l_date = ORD_DATE
    @ WROWS()-2, 2 SAY PADC("���� "+DTOC(l_date), WCOLS()-4)
  ENDIF
  SCATTER TO arr1
  SELECT NEW_T
  APPEND FROM ARRAY arr1
  
  SELECT OLD_D
  SCAN REST WHILE OLD_T.LIST_NUM = LIST_NUM .AND.   ;
                  OLD_T.ORD_NUM  = ORD_NUM
    
    SCATTER TO arr2
    SELECT NEW_D
    APPEND FROM ARRAY arr2
    SELECT OLD_D

    DELETE
  ENDSCAN
  SELECT OLD_T
  
  DELETE

ENDSCAN

***********************************************************************
mss000 = "����щ �� ��痰�→�"
@ WROWS()-3, 2 SAY PADC(mss000, WCOLS()-4)

SELECT OLD_D
USE (base_path+"ORDER") ORDER TAG ORD_NUM ALIAS OLD_D

COPY STRUCTURE TO (pth+"ORDER")
SELECT NEW_D
USE (pth+"ORDER") ALIAS NEW_D

SELECT OLD_T
USE (base_path+"ORD_TIT") ALIAS OLD_T

COPY STRUCTURE TO (pth+"ORD_TIT")
SET RELATION TO STR(YEAR(ORD_DATE),4)+STR(ORD_NUM,5) INTO OLD_D

SELECT NEW_T
USE (pth+"ORD_TIT") ALIAS NEW_T

SELECT OLD_T
GO TOP
l_date = {}

SCAN FOR ORD_DATE <= dt_e
  IF ORD_DATE # l_date
    l_date = ORD_DATE
    @ WROWS()-2, 2 SAY PADC("���� "+DTOC(l_date), WCOLS()-4)
  ENDIF
  SCATTER TO arr1
  SELECT NEW_T
  APPEND FROM ARRAY arr1
  
  SELECT OLD_D
  SCAN REST WHILE OLD_T.ORD_DATE = ORD_DATE .AND.   ;
                  OLD_T.ORD_NUM  = ORD_NUM
    
    SCATTER TO arr2
    SELECT NEW_D
    APPEND FROM ARRAY arr2
    SELECT OLD_D

    DELETE
  ENDSCAN
  SELECT OLD_T
  
  DELETE

ENDSCAN

***********************************************************************
mss000 = "�����き襯 �ムイ�腮 �� 爛��├�罔�"
@ WROWS()-3, 2 SAY PADC(mss000, WCOLS()-4)

SELECT OLD_D
USE (base_path+"RYN_M") ORDER TAG DOC_NUM ALIAS OLD_D

COPY STRUCTURE TO (pth+"RYN_M")
SELECT NEW_D
USE (pth+"RYN_M") ALIAS NEW_D

SELECT OLD_T
USE (base_path+"RYN_M_T") ALIAS OLD_T

COPY STRUCTURE TO (pth+"RYN_M_T")
SET RELATION TO STR(YEAR(DOC_DATE),4)+STR(DOC_NUM,5) INTO OLD_D

SELECT NEW_T
USE (pth+"RYN_M_T") ALIAS NEW_T

SELECT OLD_T
GO TOP
l_date = {}

SCAN FOR DOC_DATE <= dt_e
  IF DOC_DATE # l_date
    l_date = DOC_DATE
    @ WROWS()-2, 2 SAY PADC("���� "+DTOC(l_date), WCOLS()-4)
  ENDIF
  SCATTER TO arr1
  SELECT NEW_T
  APPEND FROM ARRAY arr1
  
  SELECT OLD_D
  SCAN REST WHILE OLD_T.DOC_DATE = DOC_DATE .AND.   ;
                  OLD_T.DOC_NUM  = DOC_NUM
    
    SCATTER TO arr2
    SELECT NEW_D
    APPEND FROM ARRAY arr2
    SELECT OLD_D

    DELETE
  ENDSCAN
  SELECT OLD_T
  
  DELETE

ENDSCAN

***********************************************************************
mss000 = "�����き襯 �牀��Θ"
@ WROWS()-3, 2 SAY PADC(mss000, WCOLS()-4)

SELECT OLD_D
USE (base_path+"SALE") ORDER TAG DOC_NUM ALIAS OLD_D

COPY STRUCTURE TO (pth+"SALE")
SELECT NEW_D
USE (pth+"SALE") ALIAS NEW_D

SELECT OLD_T
USE (base_path+"SALE_TIT") ORDER TAG HRON ALIAS OLD_T

COPY STRUCTURE TO (pth+"SALE_TIT")
SET RELATION TO LEFT(DTOS(DOC_DATE),4)+DOC_NUM INTO OLD_D

SELECT NEW_T
USE (pth+"SALE_TIT") ALIAS NEW_T

SELECT OLD_T
GO TOP
l_date = {}

SCAN REST WHILE DOC_DATE <= dt_e
  IF DOC_DATE # l_date
    l_date = DOC_DATE
    @ WROWS()-2, 2 SAY PADC("���� "+DTOC(l_date), WCOLS()-4)
  ENDIF
  SCATTER TO arr1
  SELECT NEW_T
  APPEND FROM ARRAY arr1
  
  SELECT OLD_D
  SCAN REST WHILE OLD_T.DOC_DATE = DOC_DATE .AND.   ;
                  OLD_T.DOC_NUM  = DOC_NUM
    
    SCATTER TO arr2
    SELECT NEW_D
    APPEND FROM ARRAY arr2
    SELECT OLD_D

    DELETE
  ENDSCAN
  SELECT OLD_T
  
  DELETE

ENDSCAN

***********************************************************************
mss000 = "�腑��"
@ WROWS()-3, 2 SAY PADC(mss000, WCOLS()-4)

SELECT OLD_D
USE

SELECT NEW_D
USE

SELECT OLD_T
USE (base_path+"BILLS") ORDER TAG HRON ALIAS OLD_T

COPY STRUCTURE TO (pth+"BILLS")
SELECT NEW_T
USE (pth+"BILLS") ALIAS NEW_T

SELECT OLD_T
GO TOP
l_date = {}

SCAN REST WHILE DOC_DATE <= dt_e
  IF DOC_DATE # l_date
    l_date = DOC_DATE
    @ WROWS()-2, 2 SAY PADC("���� "+DTOC(l_date), WCOLS()-4)
  ENDIF
  SCATTER TO arr1
  SELECT NEW_T
  APPEND FROM ARRAY arr1
  SELECT OLD_T
  
  DELETE

ENDSCAN

***********************************************************************
mss000 = "踸絎� ぅ�ィ"
@ WROWS()-3, 2 SAY PADC(mss000, WCOLS()-4)

SELECT OLD_T
USE (base_path+"INCMONEY") ORDER TAG HRON ALIAS OLD_T

COPY STRUCTURE TO (pth+"INCMONEY")
SELECT NEW_T
USE (pth+"INCMONEY") ALIAS NEW_T

SELECT OLD_T
GO TOP
l_date = {}

SCAN REST WHILE DOC_DATE <= dt_e
  IF DOC_DATE # l_date
    l_date = DOC_DATE
    @ WROWS()-2, 2 SAY PADC("���� "+DTOC(l_date), WCOLS()-4)
  ENDIF
  SCATTER TO arr1
  SELECT NEW_T
  APPEND FROM ARRAY arr1
  SELECT OLD_T
  
  DELETE

ENDSCAN

***********************************************************************
mss000 = "�砒モ� ��｀腮� �メ�"
@ WROWS()-3, 2 SAY PADC(mss000, WCOLS()-4)


SELECT OLD_T
USE (base_path+"KASSA") ORDER TAG HRON ALIAS OLD_T

COPY STRUCTURE TO (pth+"KASSA")
SELECT NEW_T
USE (pth+"KASSA") ALIAS NEW_T

SELECT OLD_T
GO TOP
l_date = {}

SCAN REST WHILE DOC_DATE <= dt_e FOR .NOT. EMPTY(WHO_GET)
  IF DOC_DATE # l_date
    l_date = DOC_DATE
    @ WROWS()-2, 2 SAY PADC("���� "+DTOC(l_date), WCOLS()-4)
  ENDIF
  SCATTER TO arr1
  SELECT NEW_T
  APPEND FROM ARRAY arr1
  SELECT OLD_T
  
  DELETE

ENDSCAN

***********************************************************************
mss000 = "��瘁� �爛く爬閧��"
@ WROWS()-3, 2 SAY PADC(mss000, WCOLS()-4)

SELECT OLD_T
USE (base_path+"KASSA_M") ORDER TAG HRON ALIAS OLD_T

COPY STRUCTURE TO (pth+"KASSA_M")
SELECT NEW_T
USE (pth+"KASSA_M") ALIAS NEW_T

SELECT OLD_T
GO TOP
l_date = {}

SCAN REST WHILE DOC_DATE <= dt_e
  IF DOC_DATE # l_date
    l_date = DOC_DATE
    @ WROWS()-2, 2 SAY PADC("���� "+DTOC(l_date), WCOLS()-4)
  ENDIF
  SCATTER TO arr1
  SELECT NEW_T
  APPEND FROM ARRAY arr1
  SELECT OLD_T
  
  DELETE

ENDSCAN

***********************************************************************
PRIVATE rc_n
mss000 = "�窶� USD"
@ WROWS()-3, 2 SAY PADC(mss000, WCOLS()-4)

SELECT OLD_T
USE (base_path+"CURS") ORDER TAG HRON ALIAS OLD_T

COPY STRUCTURE TO (pth+"CURS")
SELECT NEW_T
USE (pth+"CURS") ALIAS NEW_T

SELECT OLD_T
GO TOP
l_date = {}

rc_n = 0
SCAN REST WHILE DATE_SET <= dt_e
  rc_n = RECNO()
  IF DATE_SET # l_date
    l_date = DATE_SET
    @ WROWS()-2, 2 SAY PADC("���� "+DTOC(l_date), WCOLS()-4)
  ENDIF
  SCATTER TO arr1
  SELECT NEW_T
  APPEND FROM ARRAY arr1
  SELECT OLD_T
  
  DELETE

ENDSCAN

IF rc_n # 0
  GO rc_n
  RECALL
ENDIF

CLOSE DATABASES

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                      �ムメ����→� �����讚諷 Л�腑�┤.                 �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Mov_Start

***********************************************************************
mss000 = "���キ���矣��"
@ WROWS()-2, 2 SAY PADC(mss000, WCOLS()-4)

USE S_DELTA ORDER TAG CODE ALIAS DL
SELECT 0
USE STOCK ALIAS BAS
SET RELATION TO CODE INTO DL

REPLACE ALL FOR FOUND("DL")  ;
      QNT_START WITH QNT_START+DL.QNT,      ;
      QNT_S_REP WITH QNT_S_REP+DL.QNT_REP,  ;
      QNT_S_RYN WITH QNT_S_RYN+DL.QNT_REAL

***********************************************************************
mss000 = "��瘁� ��｀腮� �メ�"
@ WROWS()-2, 2 SAY PADC(mss000, WCOLS()-4)

SELECT DL
USE A_DELTA ORDER TAG ARM_C ALIAS DL
DELETE FILE (tmpo_path+"S_DELTA.DBF")
DELETE FILE (tmpo_path+"S_DELTA.CDX")
SELECT BAS
USE ARM_LST ALIAS BAS
SET RELATION TO ARM_C INTO DL

REPLACE ALL FOR FOUND("DL")  ;
      S_KASSA WITH S_KASSA+DL.KASSA

***********************************************************************
mss000 = "��瘁� �爛く爬閧��"
@ WROWS()-2, 2 SAY PADC(mss000, WCOLS()-4)

SELECT DL
USE K_DELTA ALIAS DL
DELETE FILE (tmpo_path+"A_DELTA.DBF")
DELETE FILE (tmpo_path+"A_DELTA.CDX")
SELECT BAS
USE KASSA_S ALIAS BAS

REPLACE SUM_R_S WITH SUM_R_S+DL.DELTA_R,  ;
        SUM_D_S WITH SUM_D_S+DL.DELTA_D

***********************************************************************
mss000 = "��┘�硅"
@ WROWS()-2, 2 SAY PADC(mss000, WCOLS()-4)

SELECT DL
USE C_DELTA ORDER TAG CUS_CODE ALIAS DL
DELETE FILE (tmpo_path+"K_DELTA.DBF")
SELECT BAS
USE ACCOUNT ALIAS BAS
SET RELATION TO CUS_CODE INTO DL

SCAN
  IF .NOT. FOUND("DL")
    LOOP
  ENDIF
  IF ACC_TYPE = 2
    REPLACE ACC_SD_S WITH ACC_SD_S+DL.DELTA_D,  ;
            ACC_S_S  WITH 0
  ELSE
    REPLACE ACC_S_S  WITH ACC_S_S+DL.DELTA_R,  ;
            ACC_SD_S WITH 0
  ENDIF
ENDSCAN

CLOSE DATABASES
DELETE FILE (tmpo_path+"C_DELTA.DBF")
DELETE FILE (tmpo_path+"C_DELTA.CDX")

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                        �����→� ������.                               �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Pk_Files

PRIVATE w

***********************************************************************
mss000 = "踳��襯 ��硅"
@ WROWS()-2, 2 SAY PADC(mss000, WCOLS()-4)

USE (base_path+"ACT_S_T") EXCLUSIVE
PACK
USE (base_path+"ACT_S") EXCLUSIVE
PACK

***********************************************************************
mss000 = "踳��襯 ��硅 筥������"
@ WROWS()-2, 2 SAY PADC(mss000, WCOLS()-4)

USE (base_path+"ACT_F_T") EXCLUSIVE
PACK
USE (base_path+"ACT_F") EXCLUSIVE
PACK

***********************************************************************
mss000 = "��錢�� �� 瓷���"
@ WROWS()-2, 2 SAY PADC(mss000, WCOLS()-4)

USE (base_path+"ORD_T") EXCLUSIVE
PACK
USE (base_path+"ORD_D") EXCLUSIVE
PACK

***********************************************************************
mss000 = "��錢�� �� ��痰�→�"
@ WROWS()-2, 2 SAY PADC(mss000, WCOLS()-4)

USE (base_path+"ORD_TIT") EXCLUSIVE
PACK
USE (base_path+"ORDER") EXCLUSIVE
PACK

***********************************************************************
mss000 = "�����き襯 �� 爛��├�罔�"
@ WROWS()-2, 2 SAY PADC(mss000, WCOLS()-4)

USE (base_path+"RYN_M") EXCLUSIVE
PACK
USE (base_path+"RYN_M_T") EXCLUSIVE
PACK

***********************************************************************
mss000 = "�����き襯 �牀��Θ"
@ WROWS()-2, 2 SAY PADC(mss000, WCOLS()-4)

USE (base_path+"SALE_TIT") EXCLUSIVE
PACK
USE (base_path+"SALE") EXCLUSIVE
PACK

***********************************************************************
mss000 = "�腑��"
@ WROWS()-2, 2 SAY PADC(mss000, WCOLS()-4)

USE (base_path+"BILLS") EXCLUSIVE
PACK

***********************************************************************
mss000 = "�痰�爬� �窶�� USD"
@ WROWS()-2, 2 SAY PADC(mss000, WCOLS()-4)

USE (base_path+"CURS") EXCLUSIVE
PACK

***********************************************************************
mss000 = "��痰祚�キ┘ ぅ�ィ"
@ WROWS()-2, 2 SAY PADC(mss000, WCOLS()-4)

USE (base_path+"INCMONEY") EXCLUSIVE
PACK

***********************************************************************
mss000 = "��瘁� �爛く爬閧��"
@ WROWS()-2, 2 SAY PADC(mss000, WCOLS()-4)

USE (base_path+"KASSA_M") EXCLUSIVE
PACK

***********************************************************************
mss000 = "�砒モ� �牀��∽��"
@ WROWS()-2, 2 SAY PADC(mss000, WCOLS()-4)

USE (base_path+"KASSA") EXCLUSIVE
PACK

CLOSE DATABASES

IF ADIR(ww, base_path+"*.BAK") # 0

  mss000 = "! DEL "+base_path+"*.BAK"
  &mss000

ENDIF

RETURN
