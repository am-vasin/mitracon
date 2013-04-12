*
*   Очищаем базу данных и приводим ее к начальному состоянию.
*

*     Товар
*
USE BASE\STOCK
REPLACE ALL  ;
        QNT_START  WITH 0,  ;
        QNT_S_REP  WITH 0,  ;
        QNT_S_RYN  WITH 0,  ;
        QNT_S_SPEC WITH 0,  ;
        QNT        WITH 0,  ;
        QNT_REP    WITH 0,  ;
        QNT_STORE  WITH 0,  ;
        QNT_REAL   WITH 0,  ;
        QNT_SAVE   WITH 0,  ;
        QNT_SPEC   WITH 0
USE BASE\sale exclu
zap
 
USE BASE\sale_tit exclu
zap

USE BASE\act_s exclu
zap

USE BASE\act_s_t exclu
zap

USE BASE\ryn_l exclu
zap

USE BASE\ryn_l_t exclu
zap

USE BASE\ryn_m exclu
zap

USE BASE\ryn_m_t exclu
zap

USE BASE\spec_sum exclu
zap

USE BASE\let_spec exclu
zap

USE BASE\let_sp_t exclu
zap

*   Деньги
*
USE BASE\cus_rest exclu
zap

USE BASE\kassa_s exclu
zap
append blank

USE BASE\arm_lst exclu
REPLACE ALL s_kassa    WITH 0,  ;
            s_kassa_u  WITH 0,  ;
            kassa      WITH 0,  ;
            kassa_usd  WITH 0

USE BASE\FIRMS exclu
REPLACE ALL NALOG WITH 0

USE BASE\incmoney exclu
zap

USE BASE\kassa_m exclu
zap

USE BASE\kassa exclu
zap

USE BASE\bills exclu
zap

USE BASE\bills_d exclu
zap

USE BASE\bill_f exclu
zap

USE BASE\list_tit exclu
zap

USE BASE\list_det exclu
zap

CLOSE DATA
