scatter to arr_w
s001 = doc_h
s002 = doc_f
s003 = page_h
s004 = page_f
s005 = diction
APPEND BLANK
GATHER FROM arr_w
REPLACE doc_h WITH s001, ;
        doc_f WITH s002, ;
        page_h WITH s003, ;
        page_f WITH s004, ;
        diction WITH s005
