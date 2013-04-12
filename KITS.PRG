set deleted on
set exclusive off
set talk off
use base\stock order tag code
select 0
use kits
set relation to sto_code into stock
replace all stock.ord_buy with 2

close data
wait window "That's all. Press any key"
quit
