*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Cus_StoD     ��о�｀砒┴ ��むォ ��瓱�           22.10.97 10:22:32 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                     ���キ���矣�� ��┘��� (USD).                        �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Cus_StoD

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
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ��ぅ爨�皀�讚�� ��痰� �牀������:
*
PRIVATE ex, c_cod, c_nam, dt_0, dt_1, d0, d1
DO Use_Link
DO Use_Dummy
SELECT 0
USE (base_path+"PARMS")
STORE DAT_START TO dt_0, d0
STORE DATE() TO dt_1, d1
USE (base_path+"ACCOUNT") ORDER TAG CUS_NAME
ex    = 1
c_cod = 0
c_nam = SPACE(50)

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� 敖 ��┘�� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕 �
*� � 旭旭碓旭異旭旭碓旭異旭旭碓旭異旭旭碓旭異旭旭碓旭異 � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�        敖 ��皀燿�� ��� 陳陳陳陳陳陳陳陳陳陳陳朕        �
*�        �      � ��.��.���� �� ��.��.����      �        �
*�        青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰        �
*�            < OK Ctrl-W > < �皖���碎瘴 Esc >            �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 10, 60, "���祚�� ��┘��� �� �ム┏�"
DO Sun_Bord WITH 2,  3,  4, 56, " ��┘�� "
DO Sun_Bord WITH 5, 10,  7, 49, " ��皀燿�� ��� "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 3,  5 GET c_nam WHEN G_Cust()
@ 6, 17 SAY "�"  GET dt_0 PICTURE "@D" VALID Tst_d0()
@ 6, 30 SAY "��" GET dt_1 PICTURE "@D" VALID Tst_d1()
@ 8, 14 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Tst_Blank()

IF ex = 1 .AND. .NOT. EMPTY(c_cod)
  DO P_Doc
ENDIF
*--------------------------------------------------------------------------

POP KEY
CLOSE DATABASES
RELEASE WINDOW (win_name)
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                             �襦�� ��┘���.                             �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 22.10.97 10:47:32 夕
PROCEDURE G_Cust

IF Custs("", .F., .F., .F.)
  c_cod = ACCOUNT.CUS_CODE
  c_nam = ACCOUNT.CUS_NAME
  SHOW GET c_nam
ENDIF
RETURN .F.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                 蹍▲爲� ��玻オ皚�痰� �����讚�� ��硅.                  �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 22.10.97 10:49:55 夕
PROCEDURE Tst_D0
PRIVATE mss

IF ex = 2 .OR. READKEY() % 256 = 12
  RETURN .T.
ENDIF

IF dt_0 < d0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��ゥ� ����襯 皰�讓� � "+DTOC(d0)+"!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                 蹍▲爲� ��玻オ皚�痰� ���ョ��� ��硅.                   �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 22.10.97 10:49:55 夕
PROCEDURE Tst_D1
PRIVATE mss

IF ex = 2 .OR. READKEY() % 256 = 12
  RETURN .T.
ENDIF

IF dt_1 > d1
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�ィ�き� 皰�讓� "+DTOC(d1)+"!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                        蹍▲爲� ゛���� � 罐���.                        �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 22.10.97 10:53:30 夕
PROCEDURE Tst_Blank

PRIVATE mss

IF ex = 2 .OR. READKEY() % 256 = 12
  RETURN .T.
ENDIF

IF dt_1 < dt_0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� 礫����� �����讚竡 ��矣 ｀�跏� ���ョ���!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF c_cod = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� ��� � �� ��爛ぅ�┼�瘡 � ��┘�皰�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                           �ョ�碎 ぎ�祠キ��.                            �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 22.10.97 11:15:35 夕
PROCEDURE P_Doc
PRIVATE dt_w, arr_w, n, s_w

DO Prep_Tmp
SELECT CUS_STO
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��┘�� ��腑�� �� �祚┼ �� 礫�����覃 �ム┏�..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  DELETE FILE (tmpo_path+"CUS_STO.DBF")
  DELETE FILE (tmpo_path+"CUS_STO.CDX")
  RETURN
ENDIF
SET ORDER TO NAME
GO TOP
dt_w = SALE_DATE
n = 0
s_w = 0
SCAN
  IF dt_w # SALE_DATE
    n = n+1
    DIMENSION arr_w(n,2)
    arr_w(n,1) = dt_w
    arr_w(n,2) = s_w
    dt_w = SALE_DATE
    s_w  = 0
  ENDIF
  s_w = s_w + TOTAL
ENDSCAN
n = n+1
DIMENSION arr_w(n,2)
arr_w(n,1) = dt_w
arr_w(n,2) = s_w

FOR s_w = 1 TO n
  APPEND BLANK
  REPLACE SALE_DATE WITH arr_w(s_w,1), ;
          TOTAL     WITH arr_w(s_w,2)
ENDFOR

DO Print_W

CLOSE DATABASES
DELETE FILE (tmpo_path+"CUS_STO.DBF")
DELETE FILE (tmpo_path+"CUS_STO.CDX")
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                      ��ぃ�皰→� ∇ガキ���� �����.                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 22.10.97 11:16:04 夕
PROCEDURE Prep_Tmp

DO Wt_Mess WITH "��ぃ�皰→� ����諷..."
SELECT 0
DELETE FILE (tmpo_path+"CUS_STO.DBF")
DELETE FILE (tmpo_path+"CUS_STO.CDX")
CREATE DBF (tmpo_path+"CUS_STO.DBF") ;
  ( SALE_DATE  D,     ;
    CODE       N( 7), ;
    PREFIX     C( 4), ;
    NAME       C(35), ;
    PRODUCER   C( 8), ;
    QNT        N( 6), ;
    PRICE      N(10,4), ;
    TOTAL      N(15,4)  )
USE (tmpo_path+"CUS_STO.DBF") EXCLUSIVE
INDEX ON DTOS(SALE_DATE)+IIF(EMPTY(NAME), REPLICATE(CHR(255),35), NAME)+  ;
                         PREFIX+PRODUCER TAG NAME ADDITIVE
INDEX ON DTOS(SALE_DATE)+STR(CODE,7) TAG CODE ADDITIVE

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE

SELECT 0
USE (base_path+"SALE") ORDER TAG DOC_NUM
SET RELATION TO CODE INTO STOCK

SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG CUS_CODE

SEEK c_cod
SCAN REST WHILE CUS_CODE = c_cod FOR BETWEEN(DOC_DATE, dt_0, dt_1)
  SELECT SALE
  SEEK SALE_TIT.FIRM+LEFT(DTOS(SALE_TIT.DOC_DATE),4)+SALE_TIT.DOC_NUM
  SCAN REST WHILE SALE_TIT.FIRM = FIRM .AND. SALE_TIT.DOC_NUM = DOC_NUM .AND. SALE_TIT.DOC_DATE = DOC_DATE
    IF .NOT. SEEK(DTOS(DOC_DATE)+STR(CODE,7), "CUS_STO") .OR. ;
       CUS_STO.PRICE # PRICE_R
      SELECT CUS_STO
      APPEND BLANK
      REPLACE SALE_DATE WITH SALE.DOC_DATE, ;
              CODE      WITH SALE.CODE,     ;
              PREFIX    WITH STOCK.PREFIX,  ;
              NAME      WITH STOCK.NAME,    ;
              PRODUCER  WITH STOCK.PRODUCER,;
              QNT       WITH SALE.QNT,      ;
              PRICE     WITH SALE.PRICE_D,  ;
              TOTAL     WITH SALE.QNT*SALE.PRICE_D
    ELSE
      SELECT CUS_STO
      REPLACE QNT       WITH QNT+SALE.QNT, ;
              PRICE     WITH SALE.PRICE_D, ;
              TOTAL     WITH TOTAL+SALE.QNT*SALE.PRICE_D
    ENDIF
    SELECT SALE
  ENDSCAN
  SELECT SALE_TIT
ENDSCAN
DO Wt_Mess

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                   �ク�痼イ痰▲��� �ョ�碎 ぎ�祠キ��.                    �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 22.10.97 12:06:35 夕
PROCEDURE Print_W

*
*  doc_n  - ���ム �爬ガ���� ����;
*  doc_d  - �─� ���� �����き�� �─� ��� � 瓱�〓�讚�� ┼� 腮甄�〓� 筮爼�皀;
*  d_type - 皋� ぎ�祠キ��: "H" - �爬ガ�覃 ���, "B" - 皰�讓� �爬絎�,
*                          "D" - 皰�讓� ��痳�Δキ��. �� 祠������� �ョ���モ瘴
*                           皰�讓� �爬絎�, メ�� ���ム �����き�� �竅� ┼�
*                           �爬ガ�覃 ��� � �牀皋↓�� 甄竍��.
*

PRIVATE p_ln, fnt, n_cp, lft, n, i, j, t_p, f_p
PRIVATE t_d, f_d, d_year, ffeed, sw, p_drctry
PRIVATE sel_rem, f_name, sb, n_p_det, n_pg, f_pg, s_pg, str_w, nds, doc_tit
PRIVATE k00

sel_rem = SELECT()

DIMENSION sb(5,2)
sb( 1,1) = "{Pg/Pgs}"
sb( 1,2) = ""

sb( 2,1) = "{Date_0}"
sb( 2,2) = DTOC(dt_0)

sb( 3,1) = "{Date_1}"
sb( 3,2) = DTOC(dt_1)

sb( 4,1) = "{Customer}"
sb( 4,2) = ALLTRIM(c_nam)

sb( 5,1) = "{Total_Doc    }"
sb( 5,2) = ""

n = ALEN(sb,1)

SELECT 0
USE DOC_FORM

LOCATE FOR "CUS_STO" = ALLTRIM(UPPER(DOC_NAME))
p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed =DOC_FORM.F_FEED
p_drctry = DOC_FORM.P_DIR

n = MEMLINES(PAGE_H)
DIMENSION t_p(n)
FOR i = 1 TO n
  t_p(i) = MLINE(PAGE_H,i)
ENDFOR

n = MEMLINES(PAGE_F)
DIMENSION f_p(n)
FOR i = 1 TO n
  f_p(i) = MLINE(PAGE_F,i)
ENDFOR

n = MEMLINES(DOC_H)
DIMENSION t_d(n)
FOR i = 1 TO n
  t_d(i) = MLINE(DOC_H,i)
ENDFOR

n = MEMLINES(DOC_F)
DIMENSION f_d(n)
FOR i = 1 TO n
  f_d(i) = MLINE(DOC_F,i)
ENDFOR
USE

SELECT CUS_STO

DO Ini_Prn WITH "���甌� �牀��� ��┘�矣", p_ln, lft, n_cp, fnt, ffeed, p_drctry

GO TOP

n_p_det = p_ln-MAX(ALEN(t_p),ALEN(t_d))-MAX(ALEN(f_p),ALEN(f_d))
IF n_p_det <= 0
  n_p_det = RECCOUNT()
  n_pg = 1
ELSE
  n_pg = CEILING(RECCOUNT()/n_p_det)
ENDIF

PRIVATE s_d
STORE 0 TO s_d

FOR f_pg = 1 TO n_pg
  sb( 1,2) = ALLTRIM(STR(f_pg))+"/"+ALLTRIM(STR(n_pg))
  sb( 1,2) = PADR(sb(1,2), LEN(sb(1,1)))
  IF f_pg # 1
    EJECT
    FOR i = 1 TO ALEN(t_p)
      str_w = t_p(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      IF i = 1
        @ PROW(), PCOL() SAY str_w
      ELSE
        @ PROW()+1, 0 SAY str_w
      ENDIF
    ENDFOR
  ELSE
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
  ENDIF

  FOR i = 1 TO n_p_det
    IF .NOT. EMPTY(NAME)
      str_w = DTOC(SALE_DATE)+" "+PREFIX+" "+NAME+" "+PRODUCER+ ;
              STR(QNT,7)+STR(PRICE,11,4)+STR(TOTAL,16,4)
    ELSE
      str_w = PADL(" �皰�� �� ", LEN(PREFIX+NAME+PRODUCER)+19, "-")+  ;
                   DTOC(SALE_DATE)+STR(TOTAL,16,4)
      s_d = s_d+TOTAL
    ENDIF
    @ PROW()+1,0 SAY str_w
    SKIP
    IF EOF()
      EXIT
    ENDIF
  ENDFOR

  IF f_pg = n_pg
    sb(5, 2) = STR(s_d, 15, 4)
    FOR i = 1 TO ALEN(f_d)
      str_w = f_d(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      @ PROW()+1, 0 SAY str_w
    ENDFOR
  ELSE
    FOR i = 1 TO ALEN(f_p)
      str_w = f_p(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      @ PROW()+1, 0 SAY str_w
    ENDFOR
  ENDIF
ENDFOR

f_name = SYS(3)
COPY FOR .NOT. EMPTY(NAME) TO (tmpo_path+f_name)

USE

DO Term_Prn WITH "", tmpo_path+f_name

DELETE FILE (tmpo_path+f_name+".DBF")
SELECT (sel_rem)
RETURN
