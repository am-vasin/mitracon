*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Ord_Hist     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�              ��甎爛ぅ�キ┘ ��錢�� �� ∇ガキ� ��皰↓�痰�.               �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 19.01.2000 様�
PROCEDURE Ord_Hist
PRIVATE table, today, p, max_o, max_p
DIMENSION table(53, 8)
today = DATE()
table = 0
PRIVATE s_sav
s_sav = SELECT()
SELECT 0
USE (base_path+"ORD_T") ORDER TAG REQUIRE_1 ALIAS _0119 AGAIN
SCAN
  DO CASE
  CASE REQ_DATE < today
    p = 1
  CASE REQ_DATE = today
    p = VAL(ALLTRIM(LEFT(REQ_TIME,2)))+2
  CASE REQ_DATE = today+1
    p = VAL(ALLTRIM(LEFT(REQ_TIME,2)))+26
  CASE REQ_DATE = today+2
    p = 50
  CASE REQ_DATE = today+3
    p = 51
  CASE REQ_DATE = today+4
    p = 52
  OTHERWISE
    p = 53
  ENDCASE
  table(p, 1) = table(p,1)+1
  table(p, 5) = table(p,5)+ABS(SIZE_NEG)+ABS(SIZE_POS)
  IF .NOT. EMPTY(CHECK_S)
    table(p, 2) = table(p,2)+1
    table(p, 6) = table(p,6)+ABS(SIZE_NEG)+ABS(SIZE_POS)
  ENDIF
ENDSCAN
USE
SELECT (s_sav)

STORE 0 TO max_o, max_p
FOR p = 1 TO 53
  IF table(p,1) > max_o
    max_o = table(p,1)
  ENDIF
  IF table(p,5) > max_p
    max_p = table(p,5)
  ENDIF
ENDFOR
FOR p = 1 TO 53
  table(p,3) = ROUND(table(p,1)/MAX(max_o,1)*100, 0)
  table(p,4) = ROUND(table(p,2)/MAX(max_o,1)*100, 0)
  table(p,7) = ROUND(table(p,5)/MAX(max_p,1)*100, 0)
  table(p,8) = ROUND(table(p,6)/MAX(max_p,1)*100, 0)
ENDFOR
DO Prep_Hist WITH 0
DO Prep_Hist WITH 1
DO Main_Wind
PRIVATE w_nm
w_nm = "Hist_0"
DO WHILE .T.
  IF Show_Hist(w_nm) = 0
    EXIT
  ENDIF
  w_nm = IIF("0" $ w_nm, "Hist_1", "Hist_0")
ENDDO

RELEASE WINDOW Hist_0
RELEASE WINDOW Hist_1
RELEASE WINDOW Main_1
RELEASE WINDOW Main_2

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Prep_Hist    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                   ��爼�牀���┘ ���� � ��痰��������.                    �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 19.01.2000 陳�
PROCEDURE Prep_Hist
PARAMETERS w_type
PRIVATE w_nm, p, dt, cl, cur_h
w_nm = "Hist_"+STR(w_type,1)
DEFINE WINDOW (w_nm) FROM 0, 0 TO 52, 67 NONE SHADOW COLOR SCHEME 13
ACTIVATE WINDOW (w_nm) NOSHOW

cur_h = VAL(ALLTRIM(LEFT(TIME(),2)))+1
@ 0, 0 SAY "....."
dt = STR(DAY(DATE()),2)
FOR p = 1 TO 24
  IF cur_h = p
    @ p, 0 SAY dt+" "+RIGHT(STR(100+p-1),2) COLOR SCHEME 1
  ELSE
    @ p, 0 SAY dt+" "+RIGHT(STR(100+p-1),2)
  ENDIF
ENDFOR
dt = STR(DAY(DATE()+1),2)
FOR p = 1 TO 24
  @ p+24, 0 SAY dt+" "+RIGHT(STR(100+p-1),2)
ENDFOR
@ 49, 0 SAY STR(DAY(DATE()+2),2)+" .."
@ 50, 0 SAY STR(DAY(DATE()+3),2)+" .."
@ 51, 0 SAY STR(DAY(DATE()+4),2)+" .."
@ 52, 0 SAY "....."
cl = SCHEME(13,1)
p = AT("/", cl)
cl = SUBSTR(cl, p+1)
FOR p = 1 TO 53
  DO Draw_Line WITH p-1, 5, table(p, 4+w_type*4), table(p, 3+w_type*4), ;
               "B+", "B", cl
  IF table(p, 2+w_type*4) # 0 .OR. table(p, 1+w_type*4) # 0
    @ ROW(), COL() SAY " "+ALLTRIM(STR(table(p, 2+w_type*4)))+", "+ ;
                           ALLTRIM(STR(table(p, 1+w_type*4)))
  ENDIF
ENDFOR

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Draw_Line    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                     蹍爬甌→� �┃┬ ��痰�������.                      �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 19.01.2000 陳�
PROCEDURE Draw_Line
PARAMETERS c0, l0, p0, p1, col0, col1, colb
PRIVATE str_w, p

IF p1 = 0
  @ c0, l0 SAY ""
  RETURN
ENDIF
str_w = REPLICATE("�", FLOOR(p1/2))+IIF(p1 % 2 = 1, "�", "")
@ c0, l0 SAY str_w COLOR (col1+"/"+colb)
p = COL()
str_w = REPLICATE("�", FLOOR(p0/2))+IIF(p0 % 2 = 1, "�", "")
IF p0 = p1
  @ c0, l0 SAY str_w COLOR (col0+"/"+colb)
ELSE
  @ c0, l0 SAY str_w COLOR (col0+"/"+col1)
ENDIF

@ ROW(), p SAY ""

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Main_Nind    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                        ��Г��┘ �甅�↓諷 ����.                         �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 19.01.2000 陳�
PROCEDURE Main_Wind
*
*  �ムガキ�襯 甌痰�鎰�� か� ��※��罔�
*

PRIVATE win_name      && ��� ����

win_name = "Main_1"

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

DO D_Win_N WITH WROWS("")-3, 72, ""
@ 1, 1 CLEAR
DO Sun_Bord WITH  1, 1,  WROWS()-2, WCOLS()-2, ""

DEFINE WINDOW Main_2 FROM WLROW("Main_1")+2, WLCOL("Main_1")+2 ;
                     TO WLROW("Main_1")+WROWS("Main_1")-3,     ;
                        WLCOL("Main_1")+WCOLS("Main_1")-3      ;
NONE && COLOR SCHEME 13
ACTIVATE WINDOW Main_2
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Show_Hist    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                         蹍甃�矗 ��痰�������.                          �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 19.01.2000 陳�
PROCEDURE Show_Hist
PARAMETERS w_name
PRIVATE s, txt_h, txt_f

ACTIVATE WINDOW Main_1 SAME
IF "0" $ w_name
  txt_h = "��甎爛ぅ�キ┘ ��錢�� �� ∇ガキ�"
  txt_f = " - �ムガラキ┘;   Tab - ��甎爛ぅ�キ┘ ��Ж罔�;    Esc - �諷��"
ELSE
  txt_h = "��甎爛ぅ�キ┘ ��Ж罔� � ��錢��� �� ∇ガキ�"
  txt_f = " - �ムガラキ┘;   Tab - ��甎爛ぅ�キ┘ ��錢��;    Esc - �諷��"
ENDIF
@ 0, 0 SAY PADC(txt_h, WCOLS()) COLOR SCHEME 5
@ WROWS()-1, 0 SAY PADC(txt_f, WCOLS())
ACTIVATE WINDOW Main_2
SHOW WINDOW (w_name) IN WINDOW Main_2
DO WHILE .T.
  s = INKEY(0, "MH")
  DO CASE
  CASE s = 5
    IF WLROW(w_name)+WROWS(w_name) >= WLROW("Main_2")+WROWS("Main_2")+1
      MOVE WINDOW (w_name) BY -1, 0
    ENDIF
  CASE s = 24
    IF WLROW(w_name) <= WLROW("Main_2")-1
      MOVE WINDOW (w_name) BY 1, 0
    ENDIF
  CASE s = 27
    s = 0
    EXIT
  CASE s = 9
    s = 1
    EXIT
  ENDCASE  
ENDDO

RETURN s
