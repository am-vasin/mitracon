close data
use cus_bil order tag cus_code
edit nowait
select 0
use account order tag cus_name
set relation to cus_code into cus_bil
brow fields cus_code, cus_name nowait
on key label f1 do cus_bANK with account.cus_code, "11"
