*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� V_Pos                                                             �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                 �ョ�碎 �痰�爬� あ└キ�� (�牀���) ��Ж罔�.              �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 16.03.2001 様�
PROCEDURE V_Pos
PARAMETERS p_code

*
*  �ムガキ�襯 甌痰�鎰�� か� ��※��罔�
*

PRIVATE stat_type     && �┓ ��※��罔�: 0 - �メ�����珥�覃;
                                        1 - ゛���;
                                        2 - BROWSE - ��゛���;
                                        3 - BROWSE - 甎�甌�.
PRIVATE what_do       && ��� 爛Θ��.
PRIVATE menu_name     && ��� �瓱�縲������ �キ�.
PRIVATE last_mouse    && �爛�� ��甄イ�ィ� ����皋� Бム┃�� ������.
PRIVATE win_name      && ��� ���� ( ���� か� BROWSE ).

*
*   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
*
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

PRIVATE sel_sav, is_dummy, d01, d02, delta_d, delta_r

PUSH KEY CLEAR
sel_sav = SELECT()

IF USED("DUMMY")
  is_dummy = .T.
ELSE
  is_dummy = .F.
  DO Use_Dummy
ENDIF

d01 = DATE() - 30
d02 = DATE()
delta_d = 0.01
delta_r = 0.01
IF .NOT. G_Period(d01, d02)
  RETURN
ENDIF

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD P_Doc OF (menu_name) PROMPT "�ョ�碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD P_Doc OF (menu_name) DO Sw_Mode WITH "Hist"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

PRIVATE w_who, w_cus, w_doc, w_name, w_sp, w_prc
STORE "" TO w_who, w_cus, w_doc, w_name, w_sp

DELETE FILE (tmpo_path+"V_SALE.DBF")
DELETE FILE (tmpo_path+"V_SALE.CDX")

CREATE DBF (tmpo_path+"V_SALE.DBF")  ;
   ( FIRM      C(   3), ;
     DOC_DATE  D,       ;
     DOC_NUM   C(  10), ;
     PRICE_R   N(12,2), ;
     PRICE_D   N(10,4), ;
     SPEC      C(   1), ;
     PRC       N( 7,2), ;
     QNT       N(   8), ;
     C_NAME    C(  50), ;
     WHO       C(  29)  )

INDEX ON DTOS(DOC_DATE) TAG HRON

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS STO_1631 AGAIN
SEEK p_code
w_name = ALLTRIM(PREFIX+" "+NAME+" "+PRODUCER)

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS P_1631 AGAIN

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS ACC_1631 AGAIN

SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG DOC_NUM ALIAS ST_1631 AGAIN
SET FILTER TO CUS_CODE >= 0 .AND. .NOT. EMPTY(LIST_NUM)
SET RELATION TO CUS_CODE INTO ACC_1631, ;
                WHO      INTO P_1631

SELECT 0
USE (base_path+"SALE") ORDER TAG CODE ALIAS S_1631 AGAIN
SET RELATION TO FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM INTO ST_1631

SEEK STO_1631.CODE
SCAN REST WHILE CODE = STO_1631.CODE FOR DOC_DATE >= d01 .AND. DOC_DATE <= d02
  IF .NOT. FOUND("ST_1631")
    LOOP
  ENDIF
  w_doc = ALLTRIM(LEFT(DOC_NUM,4))+"-"+ALLTRIM(SUBSTR(DOC_NUM,5))
  w_sp = " "
  w_prc = 0 
  IF ABS(PRICE_R_L - PRICE_R) > delta_r .OR. ABS(PRICE_D_L - PRICE_D) > delta_d
    IF .NOT. EMPTY(PRICE_TYPE)
      w_sp = IIF(PRICE_TYPE = "D","�","�")
    ENDIF
    w_prc = (PRICE_R_L-PRICE_R)/PRICE_R_L*100
  ENDIF
  IF FOUND("P_1631")
    w_who = ALLTRIM(P_1631.FAMILY)+" "+  ;
            LEFT(P_1631.NAME,1)+". "+    ;
            LEFT(P_1631.S_NAME,1)+"."
  ELSE
    w_who = ""
  ENDIF
  IF FOUND("ACC_1631")
    w_cus = ACC_1631.CUS_NAME
  ELSE
     w_cus = ST_1631.CUS_NAME
  ENDIF
  SELECT V_SALE
  APPEND BLANK
  REPLACE FIRM     WITH ST_1631.FIRM,    ;
          DOC_DATE WITH ST_1631.DOC_DATE,;
          DOC_NUM  WITH w_doc,           ;
          C_NAME   WITH w_cus,           ;
          SPEC     WITH w_sp,            ;
          PRC      WITH w_prc,           ;
          QNT      WITH S_1631.QNT,      ;
          PRICE_R  WITH S_1631.PRICE_R,  ;
          PRICE_D  WITH S_1631.PRICE_D,  ;
          WHO      WITH w_who
  SELECT S_1631
ENDSCAN

USE

what_do = "List"
SELECT V_SALE
IF RECCOUNT() = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� 礫�����覃 �ム┏� �牀��� �� ��Ж罔� �� °��!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  what_do = "Exit"
ENDIF

SELECT ACC_1631
USE

SELECT P_1631
USE

SELECT STO_1631
USE

SELECT ST_1631
USE

PRIVATE r_sum, d_sum
r_sum = 0
d_sum = 0
 
SELECT V_SALE
SCAN
  r_sum = r_sum + PRICE_R * QNT
  d_sum = d_sum + PRICE_D * QNT
ENDSCAN
*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 77

DO D_Wins WITH ln, wd, w_name, 1, 2

@ 2, 2 SAY "�爼"+"陳�"+"���祠キ�"+"陳陳"+ ;
     "����陳陳��┘�眥陳陳陳陳����-〓陳��キ�,$陳��キ�,狡C���┐��,%"

@ 1, 20 SAY "蹍��Θ � "+DTOC(d01)+" �� "+DTOC(d02)

@ WROWS()-3, 2 SAY "��┘��"
@ WROWS()-2, 2 SAY "蹍��▲�"


DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter DO Sw_Mode WITH "Hist"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS FIRM:H="",         ;
                  DOC_NUM:H="",      ;
                  DOC_DATE:H="",     ;
                  C_NAME:15:H="",    ;
                  QNT:H="":P="@Z 999999",       ; 
                  PRICE_D:H="":P="@Z 99999.99", ;
                  PRICE_R:H="":P="@Z 99999.99", ;
                  SPEC:H="",         ;
                  PRC:H="":P="@Z 99999.99",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Hist")
    ENDIF

  CASE what_do = "Hist"    && �ョ�碎 ぎ�祠キ��

    DO Prn_Doc WITH w_name, d01, d02
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

USE

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")

DELETE FILE (tmpo_path+"V_SALE.DBF")
DELETE FILE (tmpo_path+"V_SALE.CDX")

RELEASE MENU (menu_name) EXTENDED


IF .NOT. is_dummy
  SELECT DUMMY
  USE
ENDIF

SELECT (sel_sav)
POP KEY

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�                                                                        �
*�                   ��痺ガ ぎ�����皀�讚竡 ┃筮爼�罔�.                    �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 60.03.2001 陳�
PROCEDURE Draw_Foot

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� Гメ�, 爬痺ガ...
*
@ WROWS()-3, 13 SAY V_SALE.C_NAME
@ WROWS()-2, 13 SAY V_SALE.WHO

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                      蹍������ �ョ�皋 ぎ�祠キ皰�.                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様 06.03.2001 夕
PROCEDURE Prn_Doc
PARAMETERS w_name, d_1, d_2

PRIVATE sb, p_ln, fnt, n_cp, lft, t_p, f_p, f_d, n, i, ffeed, w_pos, n_pos
PRIVATE n_pg, f_pg, n_det, j, stw, p_drctry

*{Pgs}         - �矗�����/≡ィ� 痰�����
*{Dat1  }      - �����讚�� ����
*{Dat2  }      - ���ョ��� ����
*{POS                                    } - ��Ж罔�
*{SUM_R      } - �祠�� �牀��� � 珮�.
*{SUM_D      } - �祠�� �牀��� � $

DIMENSION sb(6,2)

sb(1,1) = "{Pgs}"
sb(1,2) = ""

sb(2,1) = "{Dat1  }"
sb(2,2) = TRANSFORM(d_1,"@D")

sb(3,1) = "{Dat2  }"
sb(3,2) = TRANSFORM(d_2,"@D")

sb(4,1) = "{POS                                    }"
sb(4,2) = w_name

sb(5,1) = "{SUM_R      }"
sb(5,2) = TRANSFORM(r_sum, "99999999.99")

sb(6,1) = "{SUM_D      }"
sb(6,2) = TRANSFORM(d_sum, "999999.9999")

SELECT 0
USE (base_path+"DOC_FORM")
LOCATE FOR "SALE_POS" == ALLTRIM(UPPER(DOC_NAME))

p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed = DOC_FORM.F_FEED
p_drctry = DOC_FORM.P_DIR

n = MEMLINES(PAGE_H)         &&     ������〓� 痰����肓 (�� �ム〓�)
IF n > 0
  DIMENSION t_p(n)
  FOR i = 1 TO n
      t_p(i) = MLINE(PAGE_H,i)
  ENDFOR
ELSE
  DIMENSION t_p(1)
  t_p(1) = ""
ENDIF

n = MEMLINES(DOC_H)          &&     ������〓� �ム〓� 痰����肓
IF n > 0
  DIMENSION t_d(n)
  FOR i = 1 TO n
    t_d(i) = MLINE(DOC_H,i)
  ENDFOR
ELSE
  DIMENSION t_d(ALEN(t_p))
  FOR j = 1 TO ALEN(t_p)
    t_d(j) = t_p(j)
  ENDFOR
ENDIF

n = MEMLINES(PAGE_F)
IF n > 0
  DIMENSION f_p(n)
  FOR i = 1 TO n
    f_p(i) = MLINE(PAGE_F,i)
  ENDFOR
ELSE
  DIMENSION f_p(1)
  f_p(1) = ""
ENDIF

n = MEMLINES(PAGE_F)         &&     ��あ�� 痰����肓 (�� ��甄イ�ォ!)
IF n > 0
  DIMENSION f_p(n)
  FOR i = 1 TO n
    f_p(i) = MLINE(PAGE_F,i)
  ENDFOR
ELSE
  DIMENSION f_p(1)
  f_p(1) = ""
ENDIF

n = MEMLINES(DOC_F)          &&     ��あ�� ��甄イ�ォ 痰����肓
IF n > 0
  DIMENSION f_d(n)
  FOR i = 1 TO n
    f_d(i) = MLINE(DOC_F,i)
  ENDFOR
ELSE
  DIMENSION f_d(ALEN(f_p))
  FOR j = 1 TO ALEN(f_p)
    f_d(j) = f_p(j)
  ENDFOR
ENDIF

USE

SELECT V_SALE

DO Ini_Prn WITH "", p_ln, lft, n_cp, fnt, ffeed, p_drctry

GO TOP
nn = 0

n_det = p_ln-MAX(ALEN(f_p),ALEN(f_d))-MAX(ALEN(t_p),ALEN(t_d))
IF n_det <= 0
  n_det = RECCOUNT()
  n_pg = 1
ELSE
  n_pg = MAX(CEILING(RECCOUNT()/n_det),1)
ENDIF
*
*  ������〓� ぎ�祠キ��
sb( 1,2) = "1/"+ALLTRIM(STR(n_pg))
sb( 1,2) = PADL(sb(1,2), LEN(sb(1,1)))
FOR i = 1 TO ALEN(t_d)
  str_w = t_d(i)
  FOR j = 1 TO ALEN(sb,1)
    str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
  ENDFOR
  IF i = 1
    @ PROW(), PCOL() SAY str_w
  ELSE
    @ PROW()+1, 0 SAY str_w
  ENDIF
ENDFOR

FOR f_pg = 1 TO n_pg
  IF f_pg # 1
    sb(1,2) = ALLTRIM(STR(f_pg))+"/"+ALLTRIM(STR(n_pg))
    sb( 1,2) = PADR(sb(1,2), LEN(sb(1,1)))
    EJECT
    FOR i = 1 TO ALEN(t_d)
      stw = t_d(i)
      FOR j = 1 TO ALEN(sb,1)
        stw = STRTRAN(stw, sb(j,1), sb(j,2))
      ENDFOR
      IF i = 1
        @ PROW(), PCOL() SAY stw
      ELSE
        @ PROW()+1, 0 SAY stw
      ENDIF
    ENDFOR
  ENDIF
  IF RECCOUNT() # 0
    FOR i = 1 TO n_det
      stw = FIRM+" "+DOC_NUM+" "+TRANSFORM(DOC_DATE,"@D")+" "+  ;
            LEFT(C_NAME,30)+" "+STR(QNT,6)+" "+STR(PRICE_D,10,4)+;
            STR(PRICE_R,12,2)+" "+SPEC+"   "+STR(PRC,6,2)+" "+WHO
      @ PROW()+1, 0 SAY stw
      SKIP
      IF EOF()
        EXIT
      ENDIF
    ENDFOR
  ENDIF
  
  IF f_pg # n_pg
    FOR i = 1 TO ALEN(f_p)
      str_w = f_p(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      @ PROW()+1, 0 SAY str_w
    ENDFOR
  ENDIF
ENDFOR

FOR i = 1 TO ALEN(f_d)
  str_w = f_d(i)
  FOR j = 1 TO ALEN(sb,1)
    str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
  ENDFOR
  @ PROW()+1, 0 SAY str_w
ENDFOR
  
COPY TO (tmpo_path+"V0001.DBF")

DO Term_Prn WITH "", tmpo_path+"V0001.DBF"

DELETE FILE (tmpo_path+"V0001.DBF") 

RETURN