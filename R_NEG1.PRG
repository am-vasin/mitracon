*
*  Уносим остатки из возвратных актов
*
close data
use act_s order tag code
select 0
use neg_inc
set filter to qnt_rest#0 .OR. rest_rep#0
n = 0
scan
  n = n+1
  clear
  ? n, doc_num, flg_buy, code, qnt_rep
  select act_s
  go neg_inc.link
  q_rep = qnt_rep
  q_rest = qnt_rest
  q_r_rep = rest_rep
  replace qnt_rep with 0, qnt_rest with 0, rest_rep with 0
  do while "!"$doc_num or qnt_inc <=0 or flg_buy#"R"
    skip -1
  enddo
  replace qnt_rep with qnt_rep+q_rep, ;
          qnt_rest with qnt_rest+q_rest, ;
          rest_rep with rest_rep+q_r_rep 
  select neg_inc
endscan
close data
