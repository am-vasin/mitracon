procedure tst_as
parameters variant
close databases
*use decl_tit
*select 0
*use decl_lst exclu
*zap
*use decl_lst order tag doc_num

DELETE FILE decl_lst.dbf
CREATE DBF decl_lst  ;
  ( sto_code N(7),   ;
    prefix   C(4),   ;
    name     C(35),  ;
    doc_date  D,     ;
    doc_num   N(5),  ;
    producer C(8),   ;
    qty      N(6),   ;
    qty_RPL  N(6),   ;
    price    N(8,3), ;
    price_r  N(8) )
INDEX ON STR(YEAR(doc_date),4)+STR(doc_num,5) TAG DOC_NUM

SELECT 0
DELETE FILE decl_tit.dbf
CREATE DBF decl_tit ;
  ( code C( 1),      ;
    cus_code  N( 8), ;
    spec_num  N( 5), ;
    spec_date D,     ;
    doc_date  D,     ;
    doc_num   N(5),  ;
    lst_num   c(6),  ;
    lst_date  D,     ;
    STOC_NUM  C(6),  ;
    STOC_DATE D,     ;
    MAIL      C(1),  ;
    FLAG      C(1),  ;
    who       N(3),  ;
    where     C(1)  )
APPEND BLANK

do case
case variant = 1    && Создание списка
  select decl_tit
  scatter to arr_w blank
  gather from arr_w
  replace CODE      with "2",      ;
          cus_code  with 279,      ;
          spec_num  with 1,        ;
          spec_date with date()-1, ;
          lst_date  with date(),   ;
          who       with 1,        ;
          where     with chr(1)
  SELECT 0
  use stock order tag name
  i = 1
  scan next 10
    select decl_lst
    append blank
    replace sto_code with stock.code, ;
            prefix   with stock.prefix, ;
            name     with ALLTRIM(stock.name), ;
            producer with stock.producer, ;
            qty      with 10,   ;
            price    with mod(recno(),1000), ;
            price_r  with mod(recno(),1000)*5000
    select stock
    i = i+1
  endscan
case variant = 2     && Коррекция долга
  select decl_tit
  scatter to arr_w blank
  gather from arr_w
  replace CODE      with "1",      ;
          cus_code  with 279,      ;
          spec_num  with 1,        ;
          spec_date with date()-1, ;
          lst_num   with 207,      ;
          lst_date  with date(),   ;
          who       with 1,        ;
          where     with chr(1)
  SELECT 0
  use stock order tag name
  scan next 2
    select decl_lst
    append blank
    replace sto_code with stock.code, ;
            prefix   with stock.prefix, ;
            name     with stock.name,   ;
            producer with stock.producer, ;
            qty      with -20,   ;
            price    with mod(recno(),1000), ;
            price_r  with mod(recno(),1000)*5000
    select stock
  endscan
case variant = 3  && Забронировать
  select decl_tit
  scatter to arr_w blank
  gather from arr_w
  replace CODE      with "2",      ;
          cus_code  with 279,      ;
          spec_num  with 1,        ;
          spec_date with date()-1, ;
          lst_num   with 207,      ;
          lst_date  with date(),   ;
          who       with 1,        ;
          where     with chr(1)
  SELECT 0
  use stock order tag name
  scan next 10
    select decl_lst
    append blank
    replace sto_code with stock.code, ;
            prefix   with stock.prefix, ;
            name     with stock.name,   ;
            producer with stock.producer, ;
            qty      with 7,   ;
            price    with mod(recno(),1000), ;
            price_r  with mod(recno(),1000)*5000
    select stock
  endscan
case variant = 4  && Удалить позиции
  select decl_tit
  scatter to arr_w blank
  gather from arr_w
  replace CODE      with "1",      ;
          cus_code  with 279,      ;
          spec_num  with 1,        ;
          spec_date with date()-1, ;
          lst_num   with 199,      ;
          lst_date  with date(),   ;
          who       with 1,        ;
          where     with chr(1)
  SELECT 0
  use stock order tag name
  scan next 5
    select decl_lst
    append blank
    replace sto_code with stock.code, ;
            prefix   with stock.prefix, ;
            name     with stock.name,   ;
            producer with stock.producer, ;
            qty      with 0,   ;
            price    with mod(recno(),1000), ;
            price_r  with mod(recno(),1000)*5000
    select stock
  endscan
case variant = 5   && Вынести список
  select decl_tit
  scatter to arr_w blank
  gather from arr_w
  replace CODE      with "3",      ;
          cus_code  with 279,      ;
          spec_num  with 1,        ;
          doc_date  with date(),   ;
          spec_date with date()-1, ;
          lst_num   with "   216", ;
          lst_date  with date(),   ;
          who       with 1,        ;
          where     with chr(1)
  SELECT 0
  use stock order tag name
  scan next 10
    select decl_lst
    append blank
    replace sto_code with stock.code, ;
            prefix   with stock.prefix, ;
            name     with stock.name,   ;
            producer with stock.producer, ;
            doc_date with date(),   ;
            qty      with 10,   ;
            price    with mod(recno(),1000), ;
            price_r  with mod(recno(),1000)*5000
    select stock
  endscan
case variant = 6
case variant = 7
case variant = 8
case variant = 9
case variant = 10
case variant = 11
case variant = 12
case variant = 13
case variant = 14
case variant = 15
case variant = 16
case variant = 17
endcase

base_store = base_path
do for_as

select decl_tit
edit

return
