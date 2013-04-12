USE (base_path+"SALE") EXCLU
ZAP
USE (base_path+"SALE_TIT") EXCLU
ZAP
USE (base_path+"ACT_S") EXCLU
ZAP
USE (base_path+"ACT_S_T") EXCLU
ZAP
USE (base_path+"RYN_M") EXCLU
ZAP
USE (base_path+"RYN_M_T") EXCLU
ZAP
USE (base_path+"STOCK") EXCLU
REPLACE ALL QNT        WITH 0,;
            QNT_START  WITH 0,;
            QNT_S_REP  WITH 0,;
            QNT_S_SPEC WITH 0,;
            QNT_S_RYN  WITH 0,;
            QNT_REP    WITH 0,;
            QNT_REAL   WITH 0,;
            QNT_SAVE   WITH 0,;
            QNT_SPEC   WITH 0
USE (base_path+"RYN_L") EXCLU
ZAP
USE (base_path+"INCMONEY") EXCLU
ZAP
USE (base_path+"SPEC_SUM") EXCLU
ZAP
USE (base_path+"LIST_TIT") EXCLU
ZAP
USE (base_path+"LIST_DET") EXCLU
ZAP
USE (base_path+"LET_SP_T") EXCLU
ZAP
USE (base_path+"LET_SPEC") EXCLU
ZAP
USE (base_path+"CUS_REST") EXCLU
ZAP
USE (base_path+"KASSA_M") EXCLU
ZAP
USE (base_path+"KASSA") EXCLU
ZAP
USE (base_path+"ARM_LST") EXCLU
REPLACE ALL S_KASSA   WITH 0, ;
            S_KASSA_U WITH 0, ;
            KASSA     WITH 0, ;
            KASSA_USD WITH 0
USE (base_path+"KASSA_S") EXCLU
REPLACE SUM_R_S WITH 0, ;
        SUM_D_S WITH 0, ;
        SUM_R   WITH 0, ;
        SUM_D   WITH 0
USE (base_path+"FIRMS") EXCLU
REPLACE ALL NALOG WITH 0
USE
