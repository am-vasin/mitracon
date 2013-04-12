private n_arr, err, n, i, err_sav, cs
close database
delete file base_path+"tmp_log.dbf"
create dbf (base_path+"tmp_log.dbf") ;
  ( nm  c(15), er n( 5) )
use (base_path+"tmp_log.dbf") shared again
n = adir(n_arr, base_path+"*.dbf")
err_sav = on("error")
on error err = error()
select 0
cs = select()
for i = 1 to n
  err = 0
  use base_path+n_arr(i,1) shared again alias ttt
  if err # 0
    select tmp_log
    append blank
    replace nm with n_arr(i,1), ;
            er with err
    select (cs)
  endif
endfor
on error &err_sav
close data
