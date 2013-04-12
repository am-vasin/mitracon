procedure cnt_line
parameter f_name
private f_code, cnt, bf
f_code = FOPEN(f_name)
cnt = 0
do while .not. feof(f_code)
  bf = fgets(f_code)
  cnt = cnt+1
enddo
= fclose(f_code)
return cnt
