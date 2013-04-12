PROCEDURE Stru_N
PARAMETER pth
close data
private curName
use stru_n
do while .not. eof()
  clear
  m.curName = ALLTRIM(T_NAME)
  ? m.curName
  select 0
  use (m.base_path+m.curName) shared again alias src
  copy structure extended to tmpstru
  use tmpstru in 0
  select stru_n
  scan rest while m.curName == ALLTRIM(T_NAME)
    select tmpstru
    locate for upper(alltrim(field_name)) == upper(alltrim(stru_n.f_name))
    if found()
      replace field_len with 35
    endif
    select stru_n
  endscan
  use in tmpstru
  select 0
  create (pth+m.curName) from tmpstru
  delete file tmpstru.dbf
  use (pth+m.curName) alias dst exclu
  nn = 0
  select src
  mm = reccount()
  scan
    nn = nn+1
    if nn%1000 = 0
      wait wind str(nn)+"/"+str(mm) nowait
    endif
    scatter to tmpArr
    select dst
    append from array tmpArr
    select src
  endscan

  ex = 1
  t_name = TAG(ex)
  DO WHILE .NOT. EMPTY(t_name)
    i_expr = SYS(14, ex)
    f_expr = SYS(2021, ex)
    IF .NOT. EMPTY(f_expr)
      f_expr = "FOR "+f_expr
    ENDIF
    SELECT dst
    set talk on
    INDEX ON &i_expr TAG (t_name) &f_expr ADDITIVE
    set talk off
    SELECT src
    ex = ex+1
    t_name = TAG(ex)
  ENDDO
  use in src
  use in dst
  select stru_n
enddo
close data

use pth+"stock" order tag code alias dst
select 0
use base_path+"stock" order tag code alias src
scan for .not. empty(note)
  nn = NOTE
  select dst
  seek src.code
  replace note with nn
  select src
endscan

use pth+"list_det" order tag l_code alias dst in dst
use base_path+"list_det" order tag code alias src
scan for .not. empty(note)
  nn = NOTE
  select dst
  if seek(src.list_num+str(src.code, 7))
    replace note with nn
  endif
  select src
endscan
close data
