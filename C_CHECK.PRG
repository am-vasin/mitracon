*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� C_Check      ��о�｀砒┴ ��むォ ��瓱�           31.07.97 09:47:50 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                       蹍������ ��玻オ罔� 腑���.                       �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE C_Check
PRIVATE dat_0, dat_1, dat_v0, dat_v1, ex, mss, p_numb, p_line
PRIVATE f_c000, f_n000, f_c_old
PRIVATE shab1, shab2
PRIVATE dt_w, s01, s02, s03, s04
STORE 0 TO s01, s02, s03, s04
shab1 = "abcdefghijklmnopqrstuvwxyz�、�ぅ颱Ж�������珀矣粤肄蓍裨跋鈿"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZ���������������������������������"
f_c000 = def_firm
f_n000 = ""
f_c_old= def_firm

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

PRIVATE ln, ln1, l_ex, wk_list
USE PLACES
p_numb = RECCOUNT()
p_line = ""
FOR dat_1 = 1 TO p_numb
  p_line = p_line+CHR(ASC("A")+dat_1-1)
ENDFOR

USE PARMS
*dat_0  = "01"+SUBSTR(DTOC(DATE()),3)
*dat_0  = MAX(DAT_START, CTOD(dat_0))
dat_v0 = DAT_START
USE
dat_0  = DATE()
dat_1  = DATE()
dat_v1 = DATE()

DIMENSION wk_list(p_numb)
FOR ex = 1 TO p_numb
  wk_list(ex) = CHR(ASC("A")+ex-1) $ p_line
ENDFOR

ex   = 1

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
*
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� 敖 ��皀燿�� 陳陳陳陳陳陳陳陳� 敖 ç��お� 陳陳陳陳陳陳陳陳陳朕 �
*� �                           � �                              � �
*� � �����讚�� ���� ��.��.���� � � ( ) ....:....!....:....!.... � �
*� �                           � �                              � �
*� � ���ョ��� ����  ��.��.���� � �                              � �
*� �                           � �                              � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�             踳く爬閧┘ 999 9999999999999999 (F1)              �
*�               < OK Ctrl-W > < �皖���碎瘴 Esc >                 �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

ln1  = p_numb+4
ln   = ln1+5
l_ex = 6+p_numb

DO D_Win_N WITH ln, 68, "�┼赳�"

DO Sun_Bord WITH 2, 3, 8, 31
@  2, 5 SAY " ��皀燿�� "

DO Sun_Bord WITH 2, 32, 2+ln1-1, 64
@  2, 34 SAY " ç��お� "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 4,  5 SAY "�����讚�� ����" GET dat_0 PICTURE "@D"
@ 6,  5 SAY "���ョ��� ���� " GET dat_1 PICTURE "@D"
DO G_Place WITH 4, 35, wk_list
DO Get_Firm WITH WROWS()-3, 15, enab_firm
@ WROWS()-2, 17 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID V_Dat()

POP KEY
RELEASE WINDOW (win_name)

IF ex = 2
  RETURN
ENDIF

PRIVATE sum_h, sum_c, s_goods, s_money
STORE 0 TO sum_h, sum_c, s_goods, s_money
*DO Wt_Mess WITH "��ぁ�爲�..."
DO Prep_Dat
*DO Wt_Mess
IF RECCOUNT() = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� ���ぅ�� �� �き��� 腑��!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  DELETE FILE (tmpo_path+"C_CHECK0.DBF")
  DELETE FILE (tmpo_path+"C_CHECK.DBF")
  DELETE FILE (tmpo_path+"C_CHECK.CDX")
  RETURN
ENDIF

DO View_0

CLOSE DATABASES
DELETE FILE (tmpo_path+"C_CHECK0.DBF")
DELETE FILE (tmpo_path+"C_CHECK.DBF")
DELETE FILE (tmpo_path+"C_CHECK.CDX")

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                     蹍▲爲� ��玻オ皚�痰� ゛����.                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 31.07.97 09:49:45 夕
PROCEDURE V_Dat
PRIVATE i

IF ex = 2
  RETURN .T.
ENDIF

IF dat_0 < dat_v0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��ゥ� ����襯 皰�讓� � "+TRANSFORM(dat_v0,"@D")+"."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  dat_0 = dat_v0
  SHOW GETS
  RETURN .F.
ENDIF

IF dat_1 > dat_v1
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�ィ�き� 皰�讓� "+TRANSFORM(dat_v1,"@D")+"."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  dat_1 = dat_v1
  SHOW GETS
  RETURN .F.
ENDIF
PRIVATE l_tmp, i

l_tmp = ""
FOR i = 1 TO p_numb
  IF wk_list(i)
    l_tmp = l_tmp+CHR(ASC("A")+i-1)
  ENDIF
ENDFOR

IF EMPTY(l_tmp)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�甄� ��� �� ┃皀爛痺モ �� �き� �����お�?!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

p_line = l_tmp

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                 蹍������ ��ぃ�皰→� ∇ガキ���� �����.                 �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 31.07.97 09:54:04 夕
PROCEDURE Prep_Dat
PRIVATE rc, c_nam, p_nam

DELETE FILE (tmpo_path+"C_CHECK.DBF")

CREATE DBF (tmpo_path+"C_CHECK")  ;
   ( CH_DATE D,     ;
     REF_SAL N(10), ;
     CH_NUM  C(11), ;
     SAL_NUM C(11), ;
     REF_MON N(10), ;
     S_MAN   C(28), ;
     CLIENT  C(50)  )
INDEX ON DTOS(CH_DATE) TAG HRON
SELECT 0
CREATE DBF (tmpo_path+"C_CHECK0")  ;
   ( CH_DATE D,       ;
     SUM_M0  N(15,2), ;
     SUM_MN  N(15,2), ;
     SUM_I0  N(15,2), ;
     SUM_IN  N(15,2)  )
SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE
SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG DOC_NUM
SELECT 0
USE (base_path+"ACCOUNT")  ORDER TAG CUS_CODE
SELECT 0
USE (base_path+"INCMONEY") ORDER TAG DOC_NUM
SET RELATION TO FIRM+LEFT(DTOS(SALE_DATE),4)+SALE_NUM INTO SALE_TIT, ;
                WHO INTO PERSONS

SEEK f_c000+"�"
SCAN REST WHILE FIRM = f_c000 .AND. DOC_TYPE = "�" FOR BETWEEN(DOC_DATE, dat_0, dat_1)
  c_nam = CUS_NAME
  IF CUS_CODE # 0
    IF SEEK(CUS_CODE, "ACCOUNT")
      c_nam = ACCOUNT.CUS_NAME
    ENDIF
  ENDIF
  IF .NOT. FOUND("SALE_TIT") .OR. EMPTY(SALE_TIT.NEW_DOC)
    rc = ""
  ELSE
    rc = LEFT(SALE_TIT.DOC_NUM,1)+"-"+ALLTRIM(SUBSTR(SALE_TIT.DOC_NUM,2))
  ENDIF
  IF EMPTY(WHO)
    p_nam = "���!"
  ELSE
    IF FOUND("PERSONS")
      p_nam = ALLTRIM(PERSONS.FAMILY)+" "+LEFT(PERSONS.NAME,1)+" "+LEFT(PERSONS.S_NAME,1)
    ELSE
      p_nam = "??????"
    ENDIF
  ENDIF
  SELECT C_CHECK
  APPEND BLANK
  REPLACE CH_DATE WITH INCMONEY.DOC_DATE, ;
          REF_SAL WITH IIF(EMPTY(INCMONEY.SALE_NUM), 0, RECNO("SALE_TIT")), ;
          SAL_NUM WITH rc,                ;
          CH_NUM  WITH LEFT(INCMONEY.DOC_NUM,1)+ALLTRIM(RIGHT(INCMONEY.DOC_NUM,9)), ;
          REF_MON WITH RECNO("INCMONEY"), ;
          CLIENT  WITH c_nam,             ;
          S_MAN   WITH p_nam
  SELECT INCMONEY
  sum_c = sum_c+INCMONEY.MONEY_R
  sum_h = sum_h+INCMONEY.N_OF_SALE
  IF FOUND("SALE_TIT")
    s_goods = s_goods+SALE_TIT.TOTAL_R
    s_money = s_money+ROUND(SALE_TIT.KASSA*SALE_TIT.NSP_/100,2)
  ENDIF
ENDSCAN

SET RELATION TO
SELECT ACCOUNT
USE
SELECT SALE_TIT
SET ORDER TO

SELECT INCMONEY
SET ORDER TO 

SELECT C_CHECK
SET RELATION TO REF_SAL INTO SALE_TIT,  ;
                REF_MON INTO INCMONEY
PRIVATE dt_w
STORE 0 TO s01, s02, s03, s04
dt_w = dat_0
SCAN
  IF dt_w # C_CHECK.CH_DATE
    SELECT C_CHECK0
    APPEND BLANK
    REPLACE CH_DATE WITH dt_w,;
            SUM_M0  WITH s01, ;
            SUM_MN  WITH s02, ;
            SUM_I0  WITH s03, ;
            SUM_IN  WITH s04
    STORE 0 TO s01, s02, s03, s04
    dt_w = dt_w+1
    DO WHILE dt_w < C_CHECK.CH_DATE
      APPEND BLANK
      REPLACE CH_DATE WITH dt_w
      dt_w = dt_w+1
    ENDDO
    dt_w = C_CHECK.CH_DATE
    SELECT C_CHECK
  ENDIF
  s01 = s01+INCMONEY.MONEY_R
  s02 = s02+INCMONEY.N_OF_SALE
  IF FOUND("SALE_TIT")
    s03 = s03+SALE_TIT.TOTAL_R
    s04 = s04+ROUND(SALE_TIT.KASSA*SALE_TIT.NSP_/100,2)
  ENDIF
  SELECT C_CHECK
ENDSCAN
IF .NOT. EMPTY(dt_w)
  SELECT C_CHECK0
  APPEND BLANK
  REPLACE CH_DATE WITH dt_w,;
          SUM_M0  WITH s01, ;
          SUM_MN  WITH s02, ;
          SUM_I0  WITH s03, ;
          SUM_IN  WITH s04
  STORE 0 TO s01, s02, s03, s04
  dt_w = dt_w+1
  DO WHILE dt_w <= dat_1 
    APPEND BLANK
    REPLACE CH_DATE WITH dt_w
    dt_w = dt_w+1
  ENDDO
ENDIF

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                           ��玻オ罔� ���モ��.                           �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 31.07.97 12:15:01 夕
PROCEDURE View_D

*
*  �ムガキ�襯 甌痰�鎰�� か� ��※��罔�
*
PRIVATE k000
k000 = DTOS(C_CHECK0.CH_DATE)
SELECT C_CHECK
IF .NOT. SEEK(k000)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� "+DTOC(C_CHECK0.CH_DATE)+" �モ �� �き��� 腑��!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

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

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*

DO Use_Dummy
SELECT C_CHECK
*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = MIN(100, RECCOUNT())   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 66

*敖陳����陳�����爿覃 腑�陳�オ陳陳陳陳陳�祠�� �/��楉陳��祠�� � ��楉�
*���.��.���� NNNNNNNNNNN NNNNNNNNNNN 999999999999.99 999999999999.99�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�                                                                  �
*�                                                                  �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
DO D_Wins WITH ln, wd, "踳く爬閧┘ "+ALLTRIM(f_n000)+" �� "+DTOC(C_CHECK0.CH_DATE), 1, 2

@ 2, 2 SAY "陳�����陳�����爿覃 腑�陳�オ陳陳陳陳陳�祠�� �/��楉陳��祠�� � ���"

@ 1, 1 SAY "�祠�� ．� ���"
@ 1, 32 SAY "���"
@ 1, 15 SAY TRANSFORM(C_CHECK0.SUM_M0, "@Z 999999999999.99") COLOR SCHEME 1
@ 1, 36 SAY TRANSFORM(C_CHECK0.SUM_MN, "@Z 999999999999.99") COLOR SCHEME 1
@ 1, COL()+1 SAY TRANSFORM(C_CHECK0.SUM_M0+C_CHECK0.SUM_MN, "@Z 999999999999.99") COLOR SCHEME 1
@ WROWS()-4, 37 SAY TRANSFORM(C_CHECK0.SUM_I0, "@Z 999999999999.99")
@ WROWS()-4, COL()+1 SAY TRANSFORM(C_CHECK0.SUM_M0+C_CHECK0.SUM_MN, "@Z 999999999999.99")

@ WROWS()-3, 2 SAY " ��┘��   "
@ WROWS()-2, 2 SAY " 蹍��▲� "
what_do = "List"
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE KEY k000 ;
           FIELDS INCMONEY.DOC_DATE:H="",  ;
                  SAL_NUM:H="",  ;
                  CH_NUM:H="",   ;
                  s001=INCMONEY.MONEY_R:P="@Z 999999999999.99":H="", ;
                  s000=INCMONEY.MONEY_R+INCMONEY.N_OF_SALE:P="@Z 999999999999.99":H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Show_Cus()   ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Exit")
    ENDIF

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                            �ョ�碎 ��┘���.                             �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 31.07.97 12:59:01 夕
PROCEDURE Show_Cus

ACTIVATE WINDOW (win_name+"_M") SAME
@ WROWS()-3, 12 SAY CLIENT
@ WROWS()-2, 12 SAY S_MAN
ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� View_0       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                             �オ� �� き鍖.                              �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 11.11.1999 陳�
PROCEDURE View_0

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
PRIVATE s_sav         && ���ム ��｀腑� �゛�痰� か� 甌縲��キ�� � 〓瘁����←キ��!

*
*   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
*
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*
s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT C_CHECK0

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Detail OF (menu_name) PROMPT "���祠キ硅" KEY Ctrl-A, "Enter"
ON SELECTION PAD Detail OF (menu_name) DO Sw_Mode WITH "Detail"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ��瘍モ ��Кム�� ����
*
*敖陳����陳陳��祠�� ．� ��楉陳陳��祠�� ��楉陳��祠�� � ��楉朕
*���.��.���� 999999999999.99 999999999999.99 999999999999.99 *�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
*    �皰��   999999999999.99 999999999999.99 999999999999.99
*敖����爿襯 腑��陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�           999999999999.99 999999999999.99 999999999999.99  �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ln, wd
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 60
DO D_Wins WITH ln, wd, "�オ� �� �ム┏� � "+DTOC(dat_0)+" �� "+DTOC(dat_1), 1, 2
@ 1, 3 SAY "踳く爬閧┘"
@ 1, COL()+1 SAY f_n000 COLOR SCHEME 1
@ 2, 2 SAY "陳�����陳陳��祠�� ．� ��楉陳陳��祠�� ��楉陳��祠�� � ���"
@ WROWS()-4, 0 CLEAR TO WROWS()-1, WCOLS()-1
DO Sun_Bord WITH WROWS()-3, 1, WROWS()-1, WCOLS()-2, "����爿襯 腑��"
@ WROWS()-4, 5 SAY "�皰��"
@ WROWS()-4, 13 SAY TRANSFORM(sum_c,"999999999999.99") COLOR SCHEME 1
@ ROW(), COL()+1 SAY TRANSFORM(sum_h,"999999999999.99") COLOR SCHEME 1
@ ROW(), COL()+1 SAY TRANSFORM(sum_c+sum_h,"999999999999.99") COLOR SCHEME 1

what_do = "List"

DO WHILE .T.

  SELECT C_CHECK0
  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE FIELDS CH_DATE:H="", ;
                  SUM_M0:H="":P="@Z 999999999999.99", ;
                  SUM_MN:H="":P="@Z 999999999999.99", ;
                  s000 = SUM_M0+SUM_MN:H="":P="@Z 999999999999.99", ;
                  s001 = IIF(SUM_M0 # SUM_I0 .OR. SUM_MN # SUM_IN, "�", " "):H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_F()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Detail")
    ENDIF

  CASE what_do = "Detail"    && ���祠キ硅

    DO View_D
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   ���窶�皚キ讓� (���窶�皚キ讓�!) ���琺��ガ DBF-�����.
*
SELECT (s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Draw_Foot    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                   ��痺ガ ぎ�����皀�讚竡 ┃筮爼�罔�.                    �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 20.10.1998 陳�
PROCEDURE Draw_F

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� Гメ�, 爬痺ガ...
*
@ WROWS()-2, 13 SAY SUM_I0 PICTURE "999999999999.99" COLOR SCHEME 1
@ WROWS()-2, COL()+1 SAY SUM_IN PICTURE "999999999999.99" COLOR SCHEME 1
@ WROWS()-2, COL()+1 SAY SUM_I0+SUM_IN PICTURE "999999999999.99" COLOR SCHEME 1

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.
