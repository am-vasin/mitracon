*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� BillFree     ��о�｀砒┴ ��むォ ��瓱�           20.03.97 22:25:05 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                        �〓｀き�� 瘍モ-���矣��.                         �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE BillFree
PARAMETERS cnt_name

PRIVATE f_c000, f_n000, f_c_old, enable_f
PRIVATE shab1, shab2
shab1 = "abcdefghijklmnopqrstuvwxyz�、�ぅ颱Ж�������珀矣粤肄蓍裨跋鈿"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZ���������������������������������"
*
*   ���Κ�, ���瘠��鉗┘ 爛Θ� ��｀硅 �牀������
*

IF TYPE("cnt_name")  = "C"
  IF cnt_name = "Modify" .AND. .NOT. EMPTY(BILL_F.HOLD_DATE)
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"�皰� ぎ�祠キ� 礒� 〓茱� � ��┌� �牀���..."
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN
  ENDIF
ENDIF

IF TYPE("cnt_name")  = "C"
  IF cnt_name = "Modify" .AND. .NOT. EMPTY(BILL_F.SEND_DATE) .AND. (sys_char # "A" .AND. sale_mode # "Mitin")
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"�皰� ぎ�祠キ� 礒� �ムイ�� �� �甅�↓竡 �����お�..."
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN
  ENDIF
ENDIF

PRIVATE type_flg   && ぎ�竅皋�襯 皋�� ぎ�祠キ皰�:
                   && 1 - ��� 皋��,
                   && 2 - 皰�讓� ���,
                   && 3 - 皰�讓� �/�.
PRIVATE base_flg   && ����腮� ��щ ��┘�皰�
PRIVATE addc_flg   && ��оヨキ┘ ぎ��←閧� ��┘�皰� � ��с (base_flg = .T.)
PRIVATE attr_flg   && ����腮� ����� �矗─竄�� ��┘���
PRIVATE gds_flg    && ����腮� 甎�瓷� 皰��牀�
PRIVATE addg_flg   && ��оヨキ┘ □��� 皰���� ．� ����
PRIVATE g_codes, g_names, n

type_flg = 1
base_flg = File_O(base_path+"ACCOUNT.DBF")
addc_flg = .F.
attr_flg = File_O(base_path+"CUS_BIL.DBF")
gds_flg  = .F.
addg_flg = .T.

PRIVATE sel
sel = SELECT()
SELECT 0
n = 0
IF File_O(base_path+"GOODS.DBF")
  USE (base_path+"GOODS.DBF") ORDER TAG NAME
  IF addg_flg
    n = 1
    DIMENSION g_codes(n), g_names(n)
    g_codes(n) = 0
    g_names(n) = SPACE(24)
  ENDIF
  SCAN
    n = n+1
    DIMENSION g_codes(n), g_names(n)
    g_codes(n) = CODE
    g_names(n) = PADR(NAME, 24)
  ENDSCAN
  USE
ENDIF  
IF n = 0
  n = 1
  DIMENSION g_codes(n), g_names(n)
  g_codes(n) = 0
  g_names(n) = SPACE(24)
ENDIF

IF attr_flg
  USE (base_path+"CUS_BIL") ORDER TAG CUS_CODE
ENDIF

IF base_flg
  SELECT 0
  USE (base_path+"ACCOUNT") ORDER TAG CUS_NAME
  DO Use_Dummy
  DO Use_Link
ENDIF

PRIVATE i_zero, i_real, j, nnn, mode_w
IF cnt_name = "Modify"
  mode_w = "modify"
ELSE
  mode_w = "add"
ENDIF
DO Get_Form WITH mode_w
IF USED("CUS_BIL")
  SELECT CUS_BIL
  USE
ENDIF
IF USED("ACCOUNT")
  SELECT ACCOUNT
  USE
ENDIF

SELECT (sel)

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                ��皀���皋↓�� ��痰� □��� 瘍モ�-���矣琺.                �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 20.03.97 22:39:12 夕
PROCEDURE Get_Form
PARAMETER mode_w
PRIVATE ln, wd, c_type, c_code, c_name
PRIVATE p01, p02, p03, p04, p05, p06, p07, p08, p09, p10, p11, dt_000
PRIVATE g_code, g_name, inv, money0, money_nds, money, prc_nds, sav_val
PRIVATE n_f, buff

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�                           ��┘��: [ ] ��痰�鎰�覃                         �
*崖        ��� ....:....!....:....!....:....!....:....!....:....!          崖
*崖                                                                        崖
*崖  �むメ ....:....!....:....!....:....!....:....!....:....!....:....!    崖
*崖                                                                        崖
*崖 ��牀� ....:....!....:. �カ. ....:....!....:.  �/� ....:....!....:....! 崖
*崖                                                                        崖
*崖 � ....:....!....:....!....:....!....:....!   �/� ....:....!....:....!  崖
*崖                                                                        崖
*崖  ��� ....:....!....:....! ��� ....:....!.. ����� ....: ���� ....:...   崖
*崖                                                                        崖
*崖....:....!....:....!....:....!....:....!....:....!....:....!....:....!..崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳抒
*�                  踳く爬閧┘ 999 9999999999999999 (F1)                   �
*楽� �皴竕キ� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕�
*崖 ����� ....:....!....:....!....:. ���祠キ� N ....:....! ���� 99.99.9999 崖
*崖 �祠�� ．� ��� 9999999.99  %��� 999.9  ��� 9999999.99  �皰�� 9999999.99 崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳抒
*�                  < OK Ctrl-W > < �皖���碎瘴 Esc >                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
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

PRIVATE ex
&&   ������錺� ���� ゛����
ex   = 1             &&
SELECT BILL_F
IF mode_w = "add"
  IF File_O(prmo_path+"BILLFREE.SAV")
    n_f = FOPEN(prmo_path+"BILLFREE.SAV")
    buff = FGETS(n_f)
    c_type = .NOT. EMPTY(buff)
*    c_type = .F.                 && �┓ ��┘���: �エБ., ��痰�鎰�覃
    buff = FGETS(n_f)
    c_code = VAL(ALLTRIM(buff))
*    c_code = 0                   && �エБ.
    c_name = FGETS(n_f)
*    c_name = SPACE(50)           && ��� ��┘���
    p01 = FGETS(n_f)   && �むメ
    p02 = FGETS(n_f)   && �カヤ��
    p03 = FGETS(n_f)   && ��瘍モ�覃 瘍モ
    p04 = FGETS(n_f)   && ����
    p05 = FGETS(n_f)   && ��玻. 瘍モ
    p06 = FGETS(n_f)   && ���
    p07 = FGETS(n_f)   && ��牀�
    p08 = FGETS(n_f)   && ���
    p09 = FGETS(n_f)   && �����
    p10 = FGETS(n_f)   && ����
    p11 = FGETS(n_f)   && 踸�ョ��┘
    dt_000 = CTOD(FGETS(n_f))
    buff = FGETS(n_f)
    g_code = VAL(ALLTRIM(buff))
*    g_code = g_codes(1)  && ��� 皰����
    g_name = FGETS(n_f)
*    g_name = g_names(1)  && ��━キ����┘ 皰����
    inv    = FGETS(n_f)
*    inv    = SPACE(10)   && �����き��
    money0 = VAL(ALLTRIM(FGETS(n_f)))
*    money0 = 0
    prc_nds = VAL(ALLTRIM(FGETS(n_f)))
*    prc_nds= nds_flt
    money_nds = VAL(ALLTRIM(FGETS(n_f)))
*    money_nds = 0
    money = VAL(ALLTRIM(FGETS(n_f)))
*    money  = 0           && �祠��
    f_c000 = FGETS(n_f)
    f_n000 = FGETS(n_f)
    f_c_old= f_c000
    = FCLOSE(n_f)
  ELSE
    c_type = .F.                 && �┓ ��┘���: �エБ., ��痰�鎰�覃
    c_code = 0                   && �エБ.
    c_name = SPACE(50)           && ��� ��┘���
    p01 = SPACE(60)   && �むメ
    p02 = SPACE(16)   && �カヤ��
    p03 = SPACE(20)   && ��瘍モ�覃 瘍モ
    p04 = SPACE(40)   && ����
    p05 = SPACE(20)   && ��玻. 瘍モ
    p06 = SPACE(20)   && ���
    p07 = SPACE(16)   && ��牀�
    p08 = SPACE(12)   && ���
    p09 = SPACE(5)    && �����
    p10 = SPACE(8)    && ����
    p11 = SPACE(72)   && 踸�ョ��┘
    dt_000 = DATE()
    g_code = g_codes(1)  && ��� 皰����
    g_name = g_names(1)  && ��━キ����┘ 皰����
    inv    = SPACE(10)   && �����き��
    money0 = 0
    prc_nds= nds_flt
    money_nds = 0
    money  = 0           && �祠��
    f_c000 = def_firm
    f_n000 = ""
    f_c_old= def_firm
  ENDIF
  enable_f = enab_firm
  ln = 21
ELSE
  c_type = BILL_F.CUS_CODE # 0
  c_code = BILL_F.CUS_CODE
  c_name = BILL_F.CUS_NAME
  p01    = BILL_F.ADDRESS
  p02    = BILL_F.PHONE
  p03    = BILL_F.ACC_BILL
  p04    = BILL_F.BANK
  p05    = BILL_F.CORR_BILL
  p06    = BILL_F.BIK
  p07    = BILL_F.CITY
  p08    = BILL_F.INN
  p09    = BILL_F.OKOHX
  p10    = BILL_F.OKPO
  p11    = BILL_F.NOTE
  inv    = BILL_F.INV_NUM
  dt_000 = BILL_F.DOC_DATE
  g_code = BILL_F.GOODS_COD
  g_name = BILL_F.GOODS
  money0 = BILL_F.TOTAL_0
  prc_nds= BILL_F.NDS_
  money_nds = BILL_F.TOTAL_NDS
  money  = BILL_F.TOTAL_R
  f_c000 = BILL_F.FIRM
  f_n000 = ""
  f_c_old= BILL_F.FIRM
  enable_f = .F.
  ln = 21
ENDIF
wd = 78
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH ln, wd, IIF(mode_w = "add", "�〓� 瘍モ�-���矣琺",  ;
                            "��玻オ罔� 瘍モ�-���矣琺 N "+LEFT(DOC_NUM,1)+  ;
                            ALLTRIM(SUBSTR(DOC_NUM,2))+" �� "+TRANSFORM(DOC_DATE,"@D"))
ln = 2
DO Sun_Bord WITH ln+13, 2, ln+16, 75
@ ln+13, 4 SAY " �皴竕キ� "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
sav_val = 0
@ ln, 29 SAY "��┘��:"
IF base_flg
  @ ln, 37 GET c_type PICTURE "@*C ��痰�鎰�覃 " VALID Get_Cust()
ENDIF
@ ln+1, 11 SAY "���" GET c_name WHEN c_code = 0
@ ln+3,  5 SAY "�むメ" GET p01
@ ln+5,  4 SAY "��牀�" GET p07
@ ln+5, 27 SAY "�カ." GET p02
@ ln+5, 50 SAY "�/�" GET p03
@ ln+7,  4 SAY "�" GET p04
@ ln+7, 49 SAY "�/�" GET p05
@ ln+9,  5 SAY "���" GET p06
@ ln+9, 30 SAY "���" GET p08
@ ln+9, 47 SAY "�����" GET p09
@ ln+9, 59 SAY "����" GET p10
@ ln+11,  3 GET p11
DO Get_Firm WITH ln+12, 20, enable_f
@ ln+14,  4 SAY "�����" GET g_name WHEN G_Goods()
@ ln+14, 37 SAY "���祠キ�" GET inv
@ ln+14, 59 SAY "����" GET dt_000
@ ln+15,  4 SAY "�祠�� ．� ���" GET money0 PICTURE "@Z 9999999.99"  ;
       WHEN Sav_Ent(money0) VALID Recalc(money0 = sav_val,1)
@ ln+15, 30 SAY "%���" GET prc_nds PICTURE "@Z 999.9"  ;
       WHEN Sav_Ent(prc_nds) VALID Recalc(prc_nds = sav_val,1)
@ ln+15, 42 SAY "���" GET money_nds PICTURE "@Z 9999999.99" WHEN .F.
@ ln+15, 58 SAY "�皰��" GET money PICTURE "@Z 9999999.99"  ;
       WHEN Sav_Ent(money) VALID Recalc(money = sav_val,2)
@ ln+17, 20 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Term_Bl()

IF ex # 2
  DELETE FILE (prmo_path+"BILLFREE.SAV")
  n_f = FCREATE(prmo_path+"BILLFREE.SAV")
  = FPUTS(n_f, IIF(c_type, "*", " "))
*    c_type = .F.                 && �┓ ��┘���: �エБ., ��痰�鎰�覃
  = FPUTS(n_f, STR(c_code))
*    c_code = 0                   && �エБ.
  = FPUTS(n_f, c_name)
*    c_name = SPACE(50)           && ��� ��┘���
  = FPUTS(n_f, p01)
*    p01 = FGETS(n_f)   && �むメ
  = FPUTS(n_f, p02)
*    p02 = FGETS(n_f)   && �カヤ��
  = FPUTS(n_f, p03)
*    p03 = FGETS(n_f)   && ��瘍モ�覃 瘍モ
  = FPUTS(n_f, p04)
*    p04 = FGETS(n_f)   && ����
  = FPUTS(n_f, p05)
*    p05 = FGETS(n_f)   && ��玻. 瘍モ
  = FPUTS(n_f, p06)
*    p06 = FGETS(n_f)   && ���
  = FPUTS(n_f, p07)
*    p07 = FGETS(n_f)   && ��牀�
  = FPUTS(n_f, p08)
*    p08 = FGETS(n_f)   && ���
  = FPUTS(n_f, p09)
*    p09 = FGETS(n_f)   && �����
  = FPUTS(n_f, p10)
*    p10 = FGETS(n_f)   && ����
  = FPUTS(n_f, p11)
*    p11 = FGETS(n_f)   && 踸�ョ��┘

  = FPUTS(n_f, DTOC(dt_000))
*    dt_000 = CTOD(FGETS(n_f))
  = FPUTS(n_f, STR(g_code))
*    g_code = g_codes(1)  && ��� 皰����
  = FPUTS(n_f, g_name)
*    g_name = g_names(1)  && ��━キ����┘ 皰����
  = FPUTS(n_f, inv)
*    inv    = SPACE(10)   && �����き��
  = FPUTS(n_f, STR(money0,15,3))
*    money0 = 0
  = FPUTS(n_f, STR(prc_nds,15,3))
*    prc_nds= nds_flt
  = FPUTS(n_f, STR(money_nds,15,3))
*    money_nds = 0
  = FPUTS(n_f, STR(money,15,3))
*    money  = 0           && �祠��
  = FPUTS(n_f, f_c000)
  = FPUTS(n_f, f_n000)
    = FCLOSE(n_f)
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
SELECT TMP_LINK
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                      ��縲��キ┘ √�わ薀�� Л�腑���.                    �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 23.06.97 21:34:37 夕
PROCEDURE Sav_Ent
PARAMETER v
sav_val = v
RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                         �ムメ腑� Л�腑�┤ 痺��.                        �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 23.06.97 21:40:37 夕
PROCEDURE Recalc
PARAMETER pass, mode
IF pass
  RETURN .T.
ENDIF
IF mode = 1
  money_nds = ROUND(money0*prc_nds/100,2)
  money     = money0+money_nds
ELSE
  money_nds = ROUND(money*prc_nds/(100+prc_nds),2)
  money0    = money-money_nds
ENDIF
SHOW GET money0
SHOW GET money_nds
SHOW GET money

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                         �襦�� ��┘��� ├ ��щ.                         �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 20.03.97 23:34:37 夕
PROCEDURE Get_Cust

IF c_type
  c_type = Custs("", addc_flg, .F., .T.)
ENDIF

SHOW GET c_type
IF c_type
  c_code = ACCOUNT.CUS_CODE
  c_name = ACCOUNT.CUS_NAME
  IF attr_flg
    IF SEEK(c_code, "CUS_BIL")
      c_name = CUS_BIL.CUS_NAME
      p01 = CUS_BIL.ADDRESS
      p02 = CUS_BIL.PHONE
      p03 = CUS_BIL.ACC_BILL
      p04 = CUS_BIL.BANK
      p05 = CUS_BIL.CORR_BILL
      p06 = CUS_BIL.BIK
      p07 = CUS_BIL.CITY
      p08 = CUS_BIL.INN
      p09 = CUS_BIL.OKOHX
      p10 = CUS_BIL.OKPO
      SHOW GET p01
      SHOW GET p02
      SHOW GET p03
      SHOW GET p04
      SHOW GET p05
      SHOW GET p06
      SHOW GET p07
      SHOW GET p08
      SHOW GET p09
      SHOW GET p10
    ENDIF
  ENDIF
  SHOW GET c_name
ELSE
  c_code = 0
ENDIF

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                        �襦�� 皰���� ├ 甎�瓷�.                         �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 21.03.97 00:05:27 夕
PROCEDURE G_Goods
PRIVATE new_cod, i

IF n > 1
  new_cod = ReadMenu(g_code, g_codes, g_names, "�ムョキ� 皰��牀�")
  IF new_cod # g_code
    i = ASCAN(g_codes, new_cod)
    g_name = g_names(i)
    g_code = new_cod
    SHOW GET g_name
  ENDIF
ENDIF

RETURN g_code = 0

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                            �諷�� ├ ゛����.                            �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 21.03.97 10:45:53 夕
PROCEDURE Term_Bl

PRIVATE mss, arr_w, doc_n, doc_d, doc_t, rc_w

IF ex = 2 .OR. READKEY() % 256 = 12
  RETURN .T.
ENDIF

IF EMPTY(c_name)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"����む�´ゥ, �����礬痰�, � ��┘�皀, ��, 絎硼 °, ィ� ━�..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF EMPTY(g_name)
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"�痰�モ瘴 �ク��閧�覓, �� 艪� Ε, 甌＝癶キ��,"
  mss(3) = CHR(0)+ALLTRIM(c_name)+" ��� �����皋�..."
  mss(4) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF money = 0
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"�痰�モ瘴 �ク��閧�覓, 瓷��讓�, 甌＝癶キ��,"
  mss(3) = CHR(0)+ALLTRIM(c_name)+" ��� �����皋�..."
  mss(4) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF c_type
  SELECT CUS_BIL
  IF CUS_CODE # ACCOUNT.CUS_CODE
    APPEND BLANK
  ENDIF
  REPLACE CUS_CODE  WITH c_code,    ;
          CUS_NAME  WITH c_name,    ;
          ADDRESS   WITH p01,       ;
          PHONE     WITH p02,       ;
          ACC_BILL  WITH p03,       ;
          BANK      WITH p04,       ;
          CORR_BILL WITH p05,       ;
          BIK       WITH p06,       ;
          CITY      WITH p07,       ;
          INN       WITH p08,       ;
          OKOHX     WITH p09,       ;
          OKPO      WITH p10
ENDIF

SELECT BILL_F
IF mode_w = "add"
  doc_n = Doc_Num("BILL_F    ", f_c000, arm, DATE())
  doc_n = sys_char+STR(doc_n, FSIZE("DOC_NUM", "BILL_F")-1)
  doc_d = DATE()
  doc_t = TIME()
  APPEND BLANK
  REPLACE SUB_SYS   WITH sys_char,    ;
          FIRM      WITH f_c000,      ;
          DOC_NUM   WITH doc_n,       ;
          DOC_TIME  WITH doc_t,       ;
          WHO       WITH user,        ;
          WHERE     WITH arm
  rc_w = RECNO()
  SELECT TMP_LINK
  APPEND BLANK
  REPLACE RC_NO WITH rc_w
  GO RECNO()
ENDIF
SELECT BILL_F
REPLACE CUS_CODE  WITH c_code,    ;
        CUS_NAME  WITH c_name,    ;
        ADDRESS   WITH p01,       ;
        PHONE     WITH p02,       ;
        ACC_BILL  WITH p03,       ;
        DOC_DATE  WITH dt_000,    ;
        BANK      WITH p04,       ;
        CORR_BILL WITH p05,       ;
        BIK       WITH p06,       ;
        CITY      WITH p07,       ;
        INN       WITH p08,       ;
        OKOHX     WITH p09,       ;
        OKPO      WITH p10,       ;
        NOTE      WITH p11,       ;
        INV_NUM   WITH inv,       ;
        GOODS_COD WITH g_code,    ;
        GOODS     WITH g_name,    ;
        TOTAL_0   WITH money0,    ;
        NDS_      WITH prc_nds,   ;
        TOTAL_NDS WITH money_nds, ;
        TOTAL_R   WITH money

RETURN .T.
