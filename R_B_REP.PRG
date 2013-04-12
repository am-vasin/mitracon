close data
use base_path+"act_s" alias neg
select 0
use base_path+"act_s" alias big again
set rela to recno()+1 into neg
nn = 0
scan
  nn = nn+1
  if nn%10000 = 0
    wait window str(nn) nowait
  endif
  if rest_rep <= qnt_rep
    loop
  endif
  if neg.code = big.code .and. neg.rest_rep = -big.rest_rep
*     brow fields rrb = recno("big"), big.code, big.rest_rep, rrn = recno("neg"), neg.code, neg.rest_rep
    select neg
    replace rest_rep with 0
    select big
    replace rest_rep with 0
  endif
endscan

close data
