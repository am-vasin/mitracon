*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Seek_Pos     ��о�｀砒┴ ��むォ ��瓱�           13.01.98 09:51:44 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                           蹍������ ����皰�.                           �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Seek_Pos
PARAMETERS lin_no, col_no, name_l, win_l, als, new_rec, strange
*
* lin_no  - 痰牀�� □��� ━キ�;
* col_no  - ��Ж罔� □��� ━キ�;
* name_l  - か┃� ━キ�;
* win_l   - 腮甄� 痰牀� � BROWSE;
* als     - �����;
* new_rec - ━� �牀������ か� �ムュ��� �� ��≪� ����瘡;
* strange - �爬Л�� ��оヨキ�� "艫Θ� ��Ж罔�";
*

PRIVATE symb_n, symb_c, lin_s, flt_p, ins_m, wk, rc_sav, flt_w

ACTIVATE WINDOW (win_name+"_M") BOTTOM

lin_s = ""
ins_m = INSMODE()
flt_p = 1

DO WHILE .T.
  @ lin_no, col_no+1 SAY PADR(lin_s, name_l, "�") COLOR SCHEME 1
  @ lin_no, col_no+flt_p SAY ""
  symb_n = INKEY(0)
  DO CASE

  CASE BETWEEN(symb_n,  32, 126) .OR. ;
       BETWEEN(symb_n, 128, 175) .OR. ;
       BETWEEN(symb_n, 224, 241) .OR. ;
       INLIST(symb_n, 7, 127)   && �━〓�, BS ┼� Del
    DO CASE

    CASE symb_n = 7     && Del
      IF flt_p <= LEN(lin_s)
        wk = STUFF(lin_s, flt_p, 1, "")
        flt_w = flt_p
      ELSE
        LOOP
      ENDIF

    CASE symb_n = 127   && BS
      IF flt_p > 1
        wk = STUFF(lin_s, flt_p-1, 1, "")
        flt_w = flt_p-1
      ELSE
        LOOP
      ENDIF

    OTHERWISE
      IF flt_p > name_l
        LOOP
      ENDIF
      IF ins_m
        wk = LEFT(lin_s, flt_p-1)+CHR(symb_n)+SUBSTR(lin_s, flt_p)
      ELSE
        IF flt_p <= LEN(lin_s)
          wk = STUFF(lin_s, flt_p, 1, CHR(symb_n))
        ELSE
          wk = lin_s+CHR(symb_n)
        ENDIF
      ENDIF
      wk = LEFT(wk,25)
      flt_w = MIN(flt_p+1, name_l)

  ENDCASE

  rc_sav = RECNO()
  IF SEEK(wk)
    lin_s = wk
    flt_p = flt_w
    rc_no = RECNO()
    SHOW WINDOW (als) SAME REFRESH
  ELSE
    GO rc_sav
    IF strange
      rc_no = 0
      lin_s = wk
      flt_p = flt_w
      SHOW WINDOW (als) SAME REFRESH
    ENDIF
  ENDIF
  LOOP

  CASE symb_n = 27    && Esc
    EXIT

  CASE symb_n = 19    && <-  �������� ! �絎� � む磽�� ����!
    flt_p = MAX(flt_p-1, 1)

  CASE symb_n = 4     && ->  �������� ! �絎� � む磽�� ����!
    flt_p = MIN(flt_p+1, LEN(lin_s)+1)

  CASE symb_n = 5 .AND. rc_no # 0    && �矗カ�� □ム�
    SKIP -1
    IF BOF()
      GO TOP
    ELSE
      rc_no = RECNO()
      SHOW WINDOW (als) SAME REFRESH
    ENDIF
    LOOP

  CASE symb_n = 24 .AND. rc_no # 0    && �矗カ�� ↓├
    SKIP
    IF EOF()
      GO BOTTOM
    ELSE
      rc_no = RECNO()
      SHOW WINDOW (als) SAME REFRESH
    ENDIF
    LOOP

  CASE symb_n = 18 .AND. rc_no # 0    && PgUp
    SKIP 1-win_l
    IF BOF()
      GO TOP
    ENDIF
    rc_no = RECNO()
    SHOW WINDOW (als) SAME REFRESH
    LOOP

  CASE symb_n = 3 .AND. rc_no # 0     && PgDn
    SKIP win_l-1
    IF EOF()
      GO BOTTOM
    ENDIF
    rc_no = RECNO()
    SHOW WINDOW (als) SAME REFRESH
    LOOP

  CASE symb_n = 22    && Ins

    ins_m = .NOT. ins_m
    = INSMODE(ins_m)

  CASE symb_n = 10    && Ctrl+Enter

    wk = EVALUATE(SYS(14))
    lin_s = TRIM(LEFT(wk, name_l))
    flt_p = LEN(lin_s)+1

  ENDCASE
ENDDO

RETURN
