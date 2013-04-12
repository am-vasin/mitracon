use tmp
private n1, n2, n3
store "" to n1, n2, n3
scan
  WAIT WIND STR(RECNO()) NOWAIT
  DO Set_Gtd WITH code, prefix, name, producer, n1, n2, n3
  REPLACE GTD_NUM  WITH n1, ;
          COUNTRY  WITH n2, ;
          GTD_LINE WITH n3
ENDSCAN
CLOSE DATA
