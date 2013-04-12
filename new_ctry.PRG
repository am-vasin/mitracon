close data
clear
on key label f1 keyboard "COUNTRYC"

clear
? "after country" 
use base_path+"cmp_docs" exclusive
modi stru

clear
? "after country" 
use base_path+"act_s" exclusive
modi stru

clear
? "after country" 
use base_path+"corr_lst" exclusive
modi stru
clear

? "after country" 
use base_path+"sale" exclusive
modi stru

clear
? "after country" 
use base_path+"ext_ord" exclusive
modi stru

clear
? "after country" 
use base_path+"inc_list" exclusive
modi stru

clear
? "after country" 
use base_path+"ryn_m" exclusive
modi stru

clear
? "after country" 
use base_path+"inc_new" exclusive
modi stru

clear
? "after country" 
use path_comm+"gtd" exclusive
modi stru

clear
? "after country" 
use path_comm+"gtd_c" exclusive
modi stru

on key label f1
