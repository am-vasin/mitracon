*
*     Расстановка закупочных цен в приемных актах по накладным АС
*
procedure pricedc1

use base_path+"act_s" order tag inc_list
select 0
use path_comm+"invoiced" order tag exp_num
select 0
use path_comm+"invoicet" order tag exp_num
select 0
use base_path+"act_s_t"
set filter to .not. empty(exp_num) .and. inv_type = "2"
set relation to LEFT(DTOS(EXP_DATE),4)+EXP_NUM into invoicet
n_hole = 0
n_full = 0
n_docs = 0
scan for found("invoicet")
  wait window dtoc(doc_date)+" "+doc_num+" всего:"+str(n_hole)+"  заполнено"+STR(n_full) nowait
  n_docs = n_docs+1
  if n_docs = 100
    exit
  endif
  select invoiced
  seek LEFT(DTOS(invoicet.EXP_DATE),4)+invoicet.EXP_NUM
  scan rest while invoicet.EXP_DATE = EXP_DATE .AND. invoicet.EXP_NUM = EXP_NUM
    n_hole = n_nole +1
    if empty(sto_code)
      loop
    endif
    if .not. seek(STR(STO_CODE,7)+ACT_S_T.FIRM+DTOS(ACT_S_T.DOC_DATE)+ACT_S_T.DOC_NUM, "act_s")
      loop
    endif
    n_full = n_full+1
    select act_s
*    replace PRICE_DC WITH Crpt_1(invoiced.price, 10, 4)
    select invoiced
  endscan
  select act_s_t
endscan
