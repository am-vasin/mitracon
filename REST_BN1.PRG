*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                ����→� �痰�皖� �� ぅ��Ж��� ( �/� ).                  �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Rest_Bn1

PRIVATE dat_0, dat_1, dat_v0, dat_v1, ex, acc_t_c, sw, p_numb, p_line

PRIVATE f_c000, f_n000, f_c_old
PRIVATE shab1, shab2
shab1 = "abcdefghijklmnopqrstuvwxyz�、�ぅ颱Ж�������珀矣粤肄蓍裨跋鈿"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZ���������������������������������"
f_c000 = "   "
f_n000 = SPACE(16)
f_c_old = f_c000

PRIVATE mss

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

USE (base_path+"PARMS")
dat_0  = DAT_START
dat_v0 = DAT_START
USE
dat_1  = DATE()
dat_v1 = DATE()
p_line = "   "
acc_t_c = 1
IF .NOT. Fill_Bl()
  RETURN
ENDIF

DO Wt_Mess WITH "��ぁ�爲� ����諷..."

SELECT 0
USE (base_path+"CUS_REST") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_NAME
SET RELATION TO STR(CUS_CODE)+f_c000 INTO CUS_REST

SELECT 0
USE (base_path+"INCMONEY") ORDER TAG CUS_CODE
SET FILTER TO DOC_DATE >= dat_v0

SELECT 0
USE (base_path+"ACT_S_T") ORDER TAG SUP_CODE
SET FILTER TO DOC_DATE >= dat_v0

SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG CUS_CODE
SET FILTER TO DOC_DATE >= dat_v0

DELETE FILE (tmpo_path+"REST_BN.DBF")
DELETE FILE (tmpo_path+"REST_BN.CDX")
SELECT 0
SET EXCLUSIVE ON
CREATE DBF (tmpo_path+"REST_BN.DBF")  ;
   ( CUS_NAME C(FSIZE("CUS_NAME","ACCOUNT")),  ;
     ST_MONEY N(10,2), ;
     INC_BN   N(10,2), ;
     INC      N(10,2), ;
     EXP      N(10,2), ;
     REST     N(10,2)   )

INDEX ON SYS(15,lwr,CUS_NAME) TAG NAME
SET EXCLUSIVE OFF
PRIVATE v_inc_bn, v_inc, v_exp, v_nam, v_start
SELECT ACCOUNT

SCAN FOR FOUND("CUS_REST")
  v_start = CUS_REST.ACC_S_S
  v_namn  = CUS_NAME
  v_inc   = 0
  v_inc_bn= 0
  v_exp   = 0
  IF SEEK(ACCOUNT.CUS_CODE,"INCMONEY")
    SELECT INCMONEY
    SCAN REST WHILE CUS_CODE = ACCOUNT.CUS_CODE FOR FIRM = f_c000
      IF DOC_TYPE $ " �" .AND. DOC_DATE < dat_0 .OR. DOC_TYPE $ "1�" .AND. ENTER_DAT < dat_0
        v_start = v_start+MONEY_R
      ELSE
        IF DOC_TYPE $ " �" .AND. DOC_DATE <= dat_1 .OR. DOC_TYPE $ "1�" .AND. ENTER_DAT <= dat_1
          IF DOC_TYPE $ "1�"
            v_inc_bn = v_inc_bn+MONEY_R
          ELSE
            v_inc = v_inc+MONEY_R
          ENDIF
        ENDIF
      ENDIF
    ENDSCAN
  ENDIF
  IF SEEK(ACCOUNT.CUS_CODE,"SALE_TIT")
    SELECT SALE_TIT
    SCAN REST WHILE CUS_CODE = ACCOUNT.CUS_CODE FOR FIRM = f_c000
      IF NEW_TYPE # " "
        LOOP
      ENDIF
      IF DOC_DATE < dat_0
        v_start = v_start-TOTAL_R
      ELSE
        IF DOC_DATE <= dat_1
          v_exp = v_exp+TOTAL_R
        ENDIF
      ENDIF
    ENDSCAN
  ENDIF

  IF SEEK(ACCOUNT.CUS_CODE,"ACT_S_T")
    SELECT ACT_S_T
    SCAN REST WHILE SUP_CODE = ACCOUNT.CUS_CODE FOR FIRM = f_c000
      IF VALUTA = "$"
        LOOP
      ENDIF
      IF DOC_DATE < dat_0
        v_start = v_start+TOTAL_H
      ELSE
        IF DOC_DATE <= dat_1
          v_inc = v_inc+TOTAL_H
        ENDIF
      ENDIF
    ENDSCAN
  ENDIF

  IF v_inc_bn # 0 .OR. v_inc # 0 .OR. v_exp # 0 .OR. v_start # 0
    SELECT REST_BN
    APPEND BLANK
    REPLACE CUS_NAME WITH ACCOUNT.CUS_NAME, ;
            ST_MONEY WITH v_start, ;
            INC_BN   WITH v_inc_bn, ;
            INC      WITH v_inc, ;
            EXP      WITH v_exp, ;
            REST     WITH v_start+v_inc_bn+v_inc-v_exp
  ENDIF
  SELECT ACCOUNT
ENDSCAN

SELECT REST_BN
DELETE FILE (tmpo_path+"REST_BN0.DBF")
COPY TO (tmpo_path+"REST_BN0")

PRIVATE sb, p_ln, fnt, n_cp, lft, t_p, f_p, f_d, n, i, ffeed
DIMENSION sb(15,2)

*{Pgs}      - �矗�����/≡ィ� 痰�����
*{Type}     - �┓ ぅ��Ж皰�
*{Dat1  }   - �����讚�� ����
*{Dat2  }   - ���ョ��� ����
*{Firm_Name}- ��Б��┘ �爛く爬閧��
*{T001    } - �祠�� �痰�皖�� �� �����讚竡 ��矣 �� 痰����罐
*{T002    } - �祠�� �/� �爬絎�� �� �ム┏� �� 痰����罐
*{T003    } - �祠�� �爬絎�� �� �ム┏� �� 痰����罐
*{T004    } - �祠�� ��痳��� �� �ム┏� �� 痰����罐
*{T005    } - �祠�� �痰�皖�� �� ���ョ�竡 ��矣 �� 痰����罐
*{T101    } - �祠�� �痰�皖�� �� �����讚竡 ��矣 �� ぎ�祠キ矣
*{T102    } - �祠�� �/� �爬絎�� �� �ム┏� �� ぎ�祠キ矣
*{T103    } - �祠�� �爬絎�� �� �ム┏� �� ぎ�祠キ矣
*{T104    } - �祠�� ��痳��� �� �ム┏� �� ぎ�祠キ矣
*{T105    } - �祠�� �痰�皖�� �� ���ョ�竡 ��矣 �� ぎ�祠キ矣

sb(1,1) = "{Pgs}"
sb(1,2) = ""

sb(2,1) = "{Type}"
sb(2,2) = ""

sb(3,1) = "{Dat1  }"
sb(3,2) = TRANSFORM(dat_0,"@D ")

sb(4,1) = "{Dat2  }"
sb(4,2) = TRANSFORM(dat_1,"@D ")

sb(5,1) = "{T001    }"
sb(5,2) = "          "

sb(6,1) = "{T002    }"
sb(6,2) = "          "

sb(7,1) = "{T003    }"
sb(7,2) = "          "

sb(8,1) = "{T004    }"
sb(8,2) = "          "

sb(9,1) = "{T005    }"
sb(9,2) = "          "

sb(10,1) = "{T101    }"
sb(10,2) = "          "

sb(11,1) = "{T102    }"
sb(11,2) = "          "

sb(12,1) = "{T103    }"
sb(12,2) = "          "

sb(13,1) = "{T104    }"
sb(13,2) = "          "

sb(14,1) = "{T105    }"
sb(14,2) = "          "

sb(15,1) = "{Firm_Name}"

SELECT CUS_REST
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE
SEEK f_c000
sb(15,2) = L_NAME

SELECT ACCOUNT
USE

SELECT INCMONEY
USE

SELECT SALE_TIT

USE (base_path+"DOC_FORM")
LOCATE FOR "REP_BN" == ALLTRIM(UPPER(DOC_NAME))

p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed = DOC_FORM.F_FEED
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

n = MEMLINES(DOC_F)
DIMENSION f_d(n)
FOR i = 1 TO n
  f_d(i) = MLINE(DOC_F,i)
ENDFOR

USE
SELECT REST_BN
GO TOP
DO Wt_Mess

PRIVATE s001, s002, s003, s004, s005, s101, s102, s103, s104, s105
PRIVATE n_pg, f_pg, n_det, j, stw, p_drctry

n_det = p_ln-ALEN(t_p)-ALEN(f_p)-ALEN(f_d)
n_pg  = CEILING(RECCOUNT()/n_det)

STORE 0 TO s101, s102, s103, s104, s105
DO Ini_Prn WITH "����→� �� �痰�皖�� �� ぅ��Ж���", p_ln, lft, n_cp, fnt, ffeed, p_drctry
FOR f_pg = 1 TO n_pg
  IF f_pg # 1
    EJECT
  ENDIF
  STORE 0 TO s001, s002, s003, s004, s005
  sb(1,2) = ALLTRIM(STR(f_pg))+"/"+ALLTRIM(STR(n_pg))
  FOR i = 1 TO ALEN(t_p)
    stw = t_p(i)
    FOR j = 1 TO 15
      stw = STRTRAN(stw, sb(j,1), sb(j,2))
    ENDFOR
    @ PROW()+1, 0 SAY stw
  ENDFOR
  FOR i = 1 TO n_det
    DO CASE
    CASE acc_t_c = 2 
      stw = CUS_NAME+STR(ST_MONEY,11,4)+STR(INC,11,4)+STR(EXP,11,4)+  ;
            STR(ST_MONEY+INC-EXP,11,4)
    CASE acc_t_c = 1
      stw = CUS_NAME+STR(ST_MONEY,11,2)+STR(INC_BN,11,2)+  ;
            STR(INC,11,2)+STR(EXP,11,2)+STR(ST_MONEY+INC_BN+INC-EXP,11,2)
    OTHERWISE
      stw = CUS_NAME+STR(ST_MONEY,11,2)+  ;
            STR(INC,11,2)+STR(EXP,11,2)+STR(ST_MONEY+INC-EXP,11,2)
    ENDCASE
    s001 = s001+ST_MONEY
    s002 = s002+INC_BN
    s003 = s003+INC
    s004 = s004+EXP
    s005 = s005+ST_MONEY+INC+INC_BN-EXP
    @ PROW()+1, 0 SAY stw
    SKIP
    IF EOF()
      EXIT
    ENDIF
  ENDFOR

  IF acc_t_c = 2
    sb(5,2) = STR(s001, 10, 4)
    sb(6,2) = STR(s002, 10, 4)
    sb(7,2) = STR(s003, 10, 4)
    sb(8,2) = STR(s004, 10, 4)
    sb(9,2) = STR(s005, 10, 4)
  ELSE
    sb(5,2) = STR(s001, 10, 2)
    sb(6,2) = STR(s002, 10, 2)
    sb(7,2) = STR(s003, 10, 2)
    sb(8,2) = STR(s004, 10, 2)
    sb(9,2) = STR(s005, 10, 2)
  ENDIF

  s101 = s101+s001
  s102 = s102+s002
  s103 = s103+s003
  s104 = s104+s004
  s105 = s105+s005

  FOR i = 1 TO ALEN(f_p)
    stw = f_p(i)
    FOR j = 1 TO 15
      stw = STRTRAN(stw, sb(j,1), sb(j,2))
    ENDFOR
    @ PROW()+1, 0 SAY stw
  ENDFOR
  IF EOF()
    EXIT
  ENDIF
ENDFOR

IF acc_t_c = 2
  sb(10,2) = STR(s101, 10, 4)
  sb(11,2) = STR(s102, 10, 4)
  sb(12,2) = STR(s103, 10, 4)
  sb(13,2) = STR(s104, 10, 4)
  sb(14,2) = STR(s105, 10, 4)
ELSE
  sb(10,2) = STR(s101, 10, 2)
  sb(11,2) = STR(s102, 10, 2)
  sb(12,2) = STR(s103, 10, 2)
  sb(13,2) = STR(s104, 10, 2)
  sb(14,2) = STR(s105, 10, 2)
ENDIF

FOR i = 1 TO ALEN(f_d)
  stw = f_d(i)
  FOR j = 1 TO 15
    stw = STRTRAN(stw, sb(j,1), sb(j,2))
  ENDFOR
  @ PROW()+1, 0 SAY stw
ENDFOR

USE

DO Term_Prn WITH "", tmpo_path+"REST_BN0.DBF"
DELETE FILE (tmpo_path+"REST_BN0.DBF")

CLOSE DATABASES
DELETE FILE (tmpo_path+"REST_BN.DBF")
DELETE FILE (tmpo_path+"REST_BN.CDX")

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Fill_Bl      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           ������キ┘ ゛����.                           �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 03/15/1999 陳�
PROCEDURE Fill_Bl
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   敖 ��皀燿�� ��� 陳陳陳陳陳陳陳朕   �
*�   �  � ��.��.���� �� ��.��.����  �   �
*�   青陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰   �
*�踳く爬閧┘ .... ....:....!....:. (F1)�
*�   < OK Ctrl-W > < �皖���碎瘴 Esc >   �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 8, 42, "�������皀 ゛���"
DO Sun_Bord WITH  2,  5,  4, 36, " ��皀燿�� ��� "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@  3,  8 SAY "�" GET dat_0
@  3, 21 SAY "��" GET dat_1
DO Get_Firm WITH 5, 2, .T.
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID T_Blank()

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� T_Blank      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                蹍▲爲� ��玻オ皚�痰� ������キ�� ゛����.                �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 03/15/1999 陳�
PROCEDURE T_Blank
PRIVATE mss

IF ex = 2
  RETURN .T.
ENDIF

IF EMPTY(f_c000)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� ��°�� 礫���碎 �爛く爬閧┘!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF dat_0 < dat_v0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��ゥ� ����襯 皰�讓� � "+DTOC(dat_v0)+"!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF .F. && dat_1 > dat_v1
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�ィ�き� 皰�讓� "+DTOC(dat_v1)+"!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF dat_1 < dat_0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� 礫����� �����讚竡 ��矣 ｀�跏� ���ョ���!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.
