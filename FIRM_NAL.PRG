*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Firm_Nal     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�               踸絎き襯/��痳�き襯 �爐ム� �� �爛く爬閧��.               �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 20.04.1999 様�
PROCEDURE Firm_Nal
PARAMETER is_bank
PRIVATE dat_0, dat_1, dat_v0, dat_v1, mss, s000, check0, checkn

USE (base_path+"PARMS")
dat_0  = DATE()
dat_v0 = DAT_START
USE
dat_1  = DATE()
dat_v1 = DATE()
PRIVATE f_c000, f_n000, f_c_old
PRIVATE shab1, shab2
shab1 = "abcdefghijklmnopqrstuvwxyz�、�ぅ颱Ж�������珀矣粤肄蓍裨跋鈿"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZ���������������������������������"
f_c000 = "   "
f_n000 = SPACE(16)
f_c_old = f_c000
IF .NOT. Fill_Bl()
  RETURN
ENDIF

DO Wt_Mess WITH "��ぁ�爲� ����諷..."

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"INCMONEY") ORDER TAG HRON
SET RELATION TO CUS_CODE INTO ACCOUNT

DELETE FILE (tmpo_path+"FIRM_NAL.DBF")
SELECT 0
SET EXCLUSIVE ON
CREATE DBF (tmpo_path+"FIRM_NAL.DBF")  ;
   ( DOC_NUM  C(10), ;
     DOC_DATE D,     ;
     CUS_CODE N( 8), ;
     CUS_NAME C(50), ;
     SUM_INC  N(10,2), ;
     NALOG    N(10,2), ;
     IS_CHECK C(1) )   

SET EXCLUSIVE OFF
SELECT INCMONEY

IF .NOT. SEEK(DTOS(dat_0))
  mss = RECNO(0)
  IF mss > 0 .AND. mss < RECCOUNT()
    GO mss
  ELSE
    GO BOTTOM
  ENDIF
ENDIF
STORE 0 TO check0, checkn
SCAN REST WHILE DOC_DATE <= dat_1 FOR FIRM = f_c000
  IF DOC_TYPE = "�"
    check0 = check0+MONEY_R
    checkn = checkn+N_OF_SALE
  ENDIF
  IF .NOT. DOC_TYPE $ IIF(is_bank, "�", " �")
    LOOP
  ENDIF
  SELECT FIRM_NAL
  APPEND BLANK
  REPLACE DOC_NUM  WITH INCMONEY.DOC_NUM,  ;
          DOC_DATE WITH INCMONEY.DOC_DATE, ;
          CUS_CODE WITH INCMONEY.CUS_CODE, ;
          CUS_NAME WITH ACCOUNT.CUS_NAME,  ;
          SUM_INC  WITH INCMONEY.MONEY_R,  ;
          NALOG    WITH INCMONEY.N_OF_SALE,;
          IS_CHECK WITH IIF(LEFT(INCMONEY.ENTER_DOC,3)="�オ" .OR. INCMONEY.DOC_TYPE = "�", "�", " ")
  SELECT INCMONEY
ENDSCAN

SELECT FIRM_NAL
IF RECCOUNT() = 0
  DIMENSION mss(3)
  mss(1) = ""
  IF is_bank
    mss(2) = CHR(0)+"�� 礫�����覃 �ム┏� �� °�� ���皀Ε� 腑爛� ��!"
  ELSE
    mss(2) = CHR(0)+"�� 礫�����覃 �ム┏� �� ���ぅ�� �� �き��� �爬絎き���/��痳�き��� �爐ム�!"
  ENDIF
  mss(3) = ""
  DO Wt_Mess 
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  DELETE FILE (tmpo_path+"FIRM_NAL.DBF")
  RETURN
ENDIF

PRIVATE sb, p_ln, fnt, n_cp, lft, t_p, f_p, f_d, n, i, ffeed
DIMENSION sb(28,2)

*{Pgs}         - �矗�����/≡ィ� 痰�����
*{Dat1  }      - �����讚�� ����
*{Dat2  }      - ���ョ��� ����
*{Firm_Name}   - 踳く爬閧┘
*{_Pos_P  } - �祠�� �爬絎�� �� 痰����罐
*{_Pos_P_N} - �祠�� ������ �� 痰����罐
*{_Pos_P_H} - �祠�� �♂ィ� �爬絎�� �� 痰����罐
*{_Neg_P  } - �祠�� ��痳��� �� 痰����罐
*{_Neg_P_N} - �祠�� ������ �� 痰����罐
*{_Neg_P_H} - �祠�� �♂ィ� ��痳��� �� 痰����罐
*{_Tot_P  } - ｡��� 痺��� �� 痰����罐
*{_Tot_P_N} - ｡��� 痺��� ������ �� 痰����罐
*{_Tot_P_H} - ｡��� 痺��� � ������� �� 痰����罐
*{_Pos_D  } - �祠�� �爬絎�� �� ぎ�祠キ矣
*{_Pos_D_N} - �祠�� �爬絎�� ������ �� ぎ�祠キ矣
*{_Pos_D_H} - �祠�� �爬絎�� � ������� �� ぎ�祠キ矣
*{_Neg_D  } - �祠�� ��痳��� �� ぎ�祠キ矣
*{_Neg_D_N} - �祠�� ��痳��� ������ �� ぎ�祠キ矣
*{_Neg_D_H} - �祠�� ��痳��� � ������� �� ぎ�祠キ矣
*{_Tot_D  } - ｡��� 痺��� �� ぎ�祠キ矣
*{_Tot_D_N} - ｡��� 痺��� ������ �� ぎ�祠キ矣
*{_Tot_D_H} - ｡��� 痺��� � ������� �� ぎ�祠キ矣
*{_Tot_C_N} - ｡��� 痺��� �� 腑��� ．� ��� �� ぎ�祠キ矣
*{_Tot_C_H} - ｡��� 痺��� �� 腑��� ��� �� ぎ�祠キ矣
*{_Tot_C  } - ｡��� 痺��� �� 腑��� � ��� �� ぎ�祠キ矣

sb(1,1) = "{Pgs}"
sb(1,2) = ""

sb(2,1) = "{Dat1  }"
sb(2,2) = TRANSFORM(dat_0,"@D ")

sb(3,1) = "{Dat2  }"
sb(3,2) = TRANSFORM(dat_1,"@D ")

sb(4,1) = "{_Pos_P  }"
sb(4,2) = "               "

sb(5,1) = "{_Neg_P  }"
sb(5,2) = "               "

sb(6,1) = "{_Tot_P  }"
sb(6,2) = "               "

sb(7,1) = "{_Pos_D  }"
sb(7,2) = "               "

sb(8,1) = "{_Neg_D  }"
sb(8,2) = "               "

sb(9,1) = "{_Tot_D  }"
sb(9,2) = "               "

sb(10,1) = "{Firm_Name}"
sb(10,2) = ""

sb(11,1) = "{_Pos_P_N}" && �祠�� ������ �� 痰����罐
sb(11,2) = ""

sb(12,1) = "{_Pos_P_H}" && �祠�� �♂ィ� �爬絎�� �� 痰����罐
sb(12,2) = ""

sb(13,1) = "{_Neg_P_N}" && �祠�� ������ �� 痰����罐
sb(13,2) = ""

sb(14,1) = "{_Neg_P_H}" && �祠�� �♂ィ� ��痳��� �� 痰����罐
sb(14,2) = ""

sb(15,1) = "{_Tot_P_N}" && ｡��� 痺��� ������ �� 痰����罐
sb(15,2) = ""

sb(16,1) = "{_Tot_P_H}" && ｡��� 痺��� � ������� �� 痰����罐
sb(16,2) = ""

sb(17,1) = "{_Pos_D_N}" && �祠�� �爬絎�� ������ �� ぎ�祠キ矣
sb(17,2) = ""

sb(18,1) = "{_Pos_D_H}" && �祠�� �爬絎�� � ������� �� ぎ�祠キ矣
sb(18,2) = ""

sb(19,1) = "{_Neg_D_N}" && �祠�� ��痳��� ������ �� ぎ�祠キ矣
sb(19,2) = ""

sb(20,1) = "{_Neg_D_H}" && �祠�� ��痳��� � ������� �� ぎ�祠キ矣
sb(20,2) = ""

sb(21,1) = "{_Tot_D_N}" && ｡��� 痺��� ������ �� ぎ�祠キ矣
sb(21,2) = ""

sb(22,1) = "{_Tot_D_H}" && ｡��� 痺��� � ������� �� ぎ�祠キ矣
sb(22,2) = ""

sb(23,1) = "{_Tot_C_N}" && ｡��� 痺��� �� 腑��� ．� ��� �� ぎ�祠キ矣
sb(23,2) = STR(check0, 10, 2)

sb(24,1) = "{_Tot_C_H}" && ｡��� 痺��� �� 腑��� ��� �� ぎ�祠キ矣
sb(24,2) = STR(checkn, 10, 2)

sb(25,1) = "{_Tot_C  }" && ｡��� 痺��� �� 腑��� � ��� �� ぎ�祠キ矣
sb(25,2) = STR(checkn+check0, 10, 2)

SELECT ACCOUNT
USE

SELECT INCMONEY
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE
SEEK f_c000
sb(26, 1) = "{�礫�〓え皀��          }"
sb(26, 2) = BOSS
sb(27, 1) = "{��. ＜紕��皀�         }"
sb(27, 2) = ABAK
sb(28, 1) = "{��瘁��                }"
sb(28, 2) = KASSIR

sb(10,2) = L_NAME

USE (base_path+"DOC_FORM")
LOCATE FOR IIF(is_bank, "FIRM_SB", "FIRM_NAL") == ALLTRIM(UPPER(DOC_NAME))

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
SELECT FIRM_NAL
INDEX ON DTOS(DOC_DATE)+DOC_NUM TAG DOC_NUM
COPY TO (tmpo_path+"FN00000.DBF")
USE (tmpo_path+"FN00000.DBF")
DELETE FILE (tmpo_path+"FIRM_NAL.DBF")
DO Wt_Mess

PRIVATE s001, s002, s101, s102
PRIVATE s0011, s0021, s1011, s1021
PRIVATE s0012, s0022, s1012, s1022
PRIVATE n_pg, f_pg, n_det, j, stw, p_drctry

n_det = p_ln-ALEN(t_p)-MAX(ALEN(f_p),ALEN(f_d))
n_pg  = CEILING(RECCOUNT()/n_det)

STORE 0 TO s101, s102
STORE 0 TO s1011, s1021
STORE 0 TO s1012, s1022
DO Ini_Prn WITH "��┌� �牀���", p_ln, lft, n_cp, fnt, ffeed, p_drctry
FOR f_pg = 1 TO n_pg
  IF f_pg # 1
    EJECT
  ENDIF
  STORE 0 TO s001, s002
  STORE 0 TO s0011, s0021
  STORE 0 TO s0012, s0022
  sb(1,2) = ALLTRIM(STR(f_pg))+"/"+ALLTRIM(STR(n_pg))
  FOR i = 1 TO ALEN(t_p)
    stw = t_p(i)
    FOR j = 1 TO ALEN(sb,1)
      stw = STRTRAN(stw, sb(j,1), sb(j,2))
    ENDFOR
    @ PROW()+1, 0 SAY stw
  ENDFOR
  FOR i = 1 TO n_det
    stw = PADR(DTOC(DOC_DATE),11)+ ;
          PADR(LEFT(DOC_NUM,1)+ALLTRIM(SUBSTR(DOC_NUM,2)),11)+ ;
          LEFT(CUS_NAME,40)+" "+ ;
          IIF(SUM_INC > 0, TRANSFORM(SUM_INC, "9999999.99"), SPACE(10))+ ;
          IIF(SUM_INC > 0, TRANSFORM(NALOG, " 999999.99"), SPACE(11))+ ;
          IIF(SUM_INC > 0, TRANSFORM(SUM_INC+NALOG, " 9999999.99"), SPACE(11))+ ;
          IIF(SUM_INC < 0, TRANSFORM(-SUM_INC, " 9999999.99"), SPACE(11))+ ;
          IIF(SUM_INC < 0, TRANSFORM(-NALOG, " 9999999.99"), SPACE(11))+ ;
          IIF(SUM_INC < 0, TRANSFORM(-SUM_INC-NALOG, " 9999999.99"), SPACE(11))+ ;
          " "+IS_CHECK
    IF SUM_INC > 0
      s001 = s001+SUM_INC
      s0011 = s0011+NALOG
      s0012 = s0012+NALOG+SUM_INC
    ELSE
      s002 = s002-SUM_INC
      s0021 = s0021-NALOG
      s0022 = s0022-NALOG-SUM_INC
    ENDIF
    @ PROW()+1, 0 SAY stw
    SKIP
    IF EOF()
      EXIT
    ENDIF
  ENDFOR
  sb(4,2) = STR(s001, 10, 2)
  sb(5,2) = STR(s002, 10, 2)
  sb(6,2) = STR(s001-s002, 10, 2)
  sb(11,2) = STR(s0011, 10, 2)
  sb(12,2) = STR(s0012, 10, 2)
  sb(13,2) = STR(s0021, 10, 2)
  sb(14,2) = STR(s0022, 10, 2)
  sb(15,2) = STR(s0011-s0021, 10, 2)
  sb(16,2) = STR(s0012-s0022, 10, 2)

  s101 = s101+s001
  s102 = s102+s002
  s1011 = s1011+s0011
  s1021 = s1021+s0021
  s1012 = s1012+s0012
  s1022 = s1022+s0022

  IF f_pg # n_pg
    FOR i = 1 TO ALEN(f_p)
      stw = f_p(i)
      FOR j = 1 TO ALEN(sb,1)
        stw = STRTRAN(stw, sb(j,1), sb(j,2))
      ENDFOR
      @ PROW()+1, 0 SAY stw
    ENDFOR
  ENDIF
  IF EOF()
    EXIT
  ENDIF
ENDFOR

sb(7,2) = STR(s101, 10, 2)
sb(8,2) = STR(s102, 10, 2)
sb(9,2) = STR(s101-s102, 10, 2)
sb(17,2) = STR(s1011, 10, 2)
sb(18,2) = STR(s1012, 10, 2)
sb(19,2) = STR(s1021, 10, 2)
sb(20,2) = STR(s1022, 10, 2)

sb(21,2) = STR(s1011-s1021, 10, 2)
sb(22,2) = STR(s1012-s1022, 10, 2)

FOR i = 1 TO ALEN(f_d)
  stw = f_d(i)
  FOR j = 1 TO ALEN(sb,1)
    stw = STRTRAN(stw, sb(j,1), sb(j,2))
  ENDFOR
  @ PROW()+1, 0 SAY stw
ENDFOR

SELECT FN00000
USE
DO Term_Prn WITH "", tmpo_path+"FN00000.DBF"
CLOSE DATABASES

DELETE FILE (tmpo_path+"FN00000.DBF")

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
