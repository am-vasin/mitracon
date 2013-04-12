use customer order tag code
sele 0
use act_s_t order tag doc_num
set rela to SUP_code into customer
sele 0
use act_s
sele 0
use stock order tag code
sele act_s
set rela to code into stock
set rela to left(dtos(doc_date),4)+doc_num into act_s_t addit
brow for rest_rep # 0 .and. act_s_t.sup_code = 11 fields stock.name, rest_rep, customer.b_name
