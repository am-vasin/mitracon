*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� V_Buhg       ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                         ��錢�� � ＜紕��皀爬�.                          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 22.12.1998 様�
PROCEDURE V_Buhg
PARAMETERS def_type, l_num

PRIVATE is_cr, put_r, put_d, put_cr, mov_d
PRIVATE cource, nt, f_name, f_code, close_cr
PRIVATE d_type, s__dep, s__depd, s__cr, s__inv, s__invd
PRIVATE s_sav
s_sav = SELECT()
*
*     ������〓� 甎�瓷�
*
d_type = def_type
SELECT 0
DO Use_Dummy
SELECT 0
USE (base_path+"LIST_TIT") ORDER TAG LST_NUM ALIAS LT_8C22 AGAIN
SEEK l_num    && �薀�!
IF CLI_CODE = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"� 甎�瓷� �� 礫���� ��┘��!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  RETURN
ENDIF

SELECT 0
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS FR_8C22 AGAIN
IF .NOT. SEEK(LT_8C22.FIRM)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"� 甎�瓷� �� 礫����� 筥爼�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT LT_8C22
  USE
  SELECT (s_sav)
  RETURN
ENDIF
f_name = ALLTRIM(B_NAME)
f_code = FIRM_CODE
USE

*
*  ��ぅ爨━�� 甎�瓷�
*
SELECT 0
USE (base_path+"LIST_DET") ORDER TAG LIST ALIAS LD_8C22 AGAIN
SEEK STR(l_num,6)   && �薀�... � ��め腮硅��ガ 痺���:
s__inv  = 0
s__invd = 0
IF LT_8C22.IS_USD     && か� ぎ���牀�諷 罐� �...
  SCAN REST WHILE LIST_NUM = STR(l_num,6)
    s__inv  = s__inv+ROUND(Clc_Rub(USD_PRICE)*QNT,2)
    s__invd = s__invd+USD_PRICE*QNT
  ENDSCAN
  IF s__invd # 0
    cource = ROUND(s__inv/s__invd, 3)
  ELSE
    cource = kurs
  ENDIF
ELSE                 && か� 珮゛ア諷 罐�
  SCAN REST WHILE LIST_NUM = STR(l_num,6)
    s__inv  = s__inv+ROUND(SAL_PRICE*QNT,2)
  ENDSCAN
  s__invd = ROUND(s__inv/kurs,4)
  cource = kurs
ENDIF

*
*   �ェガ �� �爬�ョ��┘�...
*
USE (base_path+"CLI_INFO") ORDER TAG CUS_CODE ALIAS CUS_8C22 AGAIN
nt = ""
IF SEEK(LT_8C22.CLI_CODE)
  nt = NOTE
ENDIF

USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS CUS_9201 AGAIN
SEEK LT_8C22.CLI_CODE

SELECT 0
USE (base_path+"CUS_REST") ORDER TAG CUS_CODE ALIAS CUS_8C22 AGAIN
IF SEEK(STR(LT_8C22.CLI_CODE)+LT_8C22.FIRM)
  s__depd = ACC_SUMD
  s__dep  = IIF(d_type # 3, ACC_SUM, ACC_SUMD)
  s__cr   = IIF(d_type # 3, CR_SUM,  CRD_SUM)
ELSE
  s__depd = 0
  s__dep  = 0
  s__cr   = 0
ENDIF

STORE 0 TO put_r, put_d, put_cr, mov_d
is_cr = .F.
close_cr = {}

*DO Prep_V WITH l_num

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
*  ��┘��
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*楽��┓陳陳��祠��陳陳���ぅ爨.陳陳�爛え眥陳踸�ョ��┘陳陳陳陳陳陳陳陳陳陳陳審
*崖 �磧  9999999.99 9999999.99 9999999.99 ....:....!....:....!....:....!..崖
*崖 ���. 99999.9999 99999.9999 99999.9999 ....:....!....:....!....:....!..崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰�
*�   ��ぅ��. ぅ��Ж� 999 999 999.99珮� 99 999 999.9999$      �窶� 999.999  �
*�  敖 [ ] �爛え� 陳陳陳陳陳陳陳陳朕                                       �
*�  � �ク�Ж�    999 999 999.99    �     踸�閧� ($)      99 999 999.9999  �
*�  � �爛え�     999 999 999.99    �                     (999 999 999.99)  �
*�  � �����き��  999 999 999.99    �     踸�閧� (珮�)    999 999 999.99   �
*�  �           (99 999 999.9999$) �     �ムアメ皋 � ┃�. 99 999 999.9999  �
*�  � �痰�皰�    999 999 999.99    �                     (999 999 999.99)  �
*�  青陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰     �皖琺碎 �爛え�   999 999 999.99   �
*�                    ���� ����茱��� �爛え�� ��.��.����                    �
*�  敖 踸�ョ��┘ 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕   �
*�  � ....:....!....:....!....:....!....:....!....:....!....:....!.... �   �
*�  青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰   �
*�   < OK Ctrl-W > < �皖���碎瘴 Esc > < �┓ ぅ��Ж�� Tab > < ��瘡� F1 >    �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
DO Calc_V
PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 20, 77, "���甌� N "+ALLTRIM(STR(l_num))+",   ��┘��: "+ALLTRIM(CUS_9201.CUS_NAME)
DO Sun_Bord WITH  2,  2,  5, 74
@ 1, FLOOR((WCOLS()-LEN(f_name)-2)/2) SAY " "+f_name+" " COLOR (SCHEME(13,6))
@ 2, 3 SAY "��┓陳陳陳��祠��陳陳���ぅ爨.陳陳�爛え眥踸�ョ��┘"
DO Sun_Bord WITH  7,  4, 13, 35, "       "
DO Sun_Bord WITH 15,  4, 17, 71, " 踸�ョ��┘ "
@  6,  5 SAY "��ぅ��. ぅ��Ж�"
@  6, 61 SAY "�窶�"
@  8,  6 SAY "�ク�Ж�"
@  9,  6 SAY "�爛え�"
@ 10,  6 SAY "�����き��"
@ 12,  6 SAY "�痰�皰�"
@  3,  4 SAY "�/�."+STR(ACC_SUM,11,2)+STR(ACC_S_H,11,2)+STR(CR_SUM,11,2)+" "+CUS_9201.ACC_NOTE
@  4,  4 SAY "���."+STR(ACC_SUMD,11,4)+STR(ACC_SD_H,11,4)+STR(CRD_SUM,11,4)+" "+CUS_9201.ACC_NOTED
@  3,  3 SAY IIF(d_type # 3, "", " ")
@  4,  3 SAY IIF(d_type # 3, " ", "")
@  6, 66 SAY cource PICTURE "999.999"

DO Calc_V
*ON KEY LABEL Tab DO Sw_Type
ON KEY LABEL F1 DO Dossier WITH CUS_8C22.CUS_CODE
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@  7,  7 GET is_cr PICTURE "@*C �爛え� " VALID Clr_Crd()
@  8, 41 SAY "踸�閧� ($)     " GET put_d PICTURE "@Z 99 999 999.9999"  ;
      VALID Calc_V()
@ 10, 41 SAY "踸�閧� (珮�)   " GET put_r PICTURE "@Z 999 999 999.99"  ;
      VALID Calc_V()
@ 11, 41 SAY "�ムアメ皋 � ┃�." GET mov_d PICTURE "@Z 999 999 999.99"  ;
      VALID Calc_V()
@ 13, 41 SAY "�皖琺碎 �爛え�  " GET put_cr PICTURE "@Z 999 999 999.99"  ;
      VALID Calc_V() WHEN is_cr
@ 14, 22 SAY "���� ����茱��� �爛え��" GET close_cr
@ WROWS()-2, 5 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc ;\\ �┓ ぅ��Ж�� Tab ;\ ��瘡� F1 "

READ CYCLE VALID Tst_Blank()

IF ex = 1
  *
  * �矗���硅��ガ ゛���
  SELECT 0
*  PRIVATE dn_buhg
  USE (base_path+"BUHG_REQ") ORDER TAG DOC_NUM DESCENDING ALIAS REQ_8C22 AGAIN
  DO WHILE .NOT. FLOCK()
  ENDDO
  IF YEAR(DOC_DATE) = YEAR(DATE())
    dn_buhg = STR(VAL(ALLTRIM(DOC_NUM))+1,10)
  ELSE
    dn_buhg = "         1"
  ENDIF
  APPEND BLANK
  REPLACE DOC_NUM   WITH dn_buhg,    ;
          DOC_DATE  WITH DATE(), ;
          LST_NUM   WITH l_num,  ;
          FIRM      WITH f_code, ;
          CUS_CODE  WITH CUS_9201.CUS_CODE, ;
          DEP_TYPE  WITH d_type, ;
          W_COURCE  WITH cource, ;
          USD_INC   WITH put_d,  ;
          USD_INC_R WITH ROUND(put_d*cource,2), ;
          R_INC     WITH put_r,  ;
          IND_EXP   WITH ROUND(mov_d/cource,4), ;
          IND_INC   WITH mov_d,  ;
          IS_INC    WITH put_d # 0 .OR. put_r # 0, ;
          IS_CONV   WITH mov_d # 0, ;
          CREDIT    WITH put_cr,    ;
          CREDIT_D  WITH close_cr,  ;
          WHO_REQ   WITH user,      ;
          WHERE_REQ WITH arm
  UNLOCK
  USE
ENDIF
SELECT LT_8C22
USE
SELECT CUS_8C22
USE
SELECT CUS_9201
USE
SELECT (s_sav)
IF TYPE("dn_buhg") = "C"
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"  �筮爼�牀���� ��錢�� N "+ALLTRIM(dn_buhg)+". �ョ���ガ? [ �� ] [ �モ ]"
  mss(3) = ""
  IF Out_Mess(5, "mss") = 1
    DO P_B_Req WITH dn_buhg, DATE()
  ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Calc_V       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                     ��瘍モ � �襤�� ���� Л�腑�┤.                      �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 22.12.1998 陳�
PROCEDURE Calc_V

@  6, 21 SAY TRANSFORM(ROUND(s__depd*cource,2)-mov_d,"999 999 999.99")+"珮�"
@  6, 40 SAY TRANSFORM(s__depd-ROUND(mov_d/cource,4),"99 999 999.9999")+"$"
@  8, 17 SAY s__dep+mov_d+ROUND(put_d*cource,2)+put_r PICTURE "999 999 999.99"
@  9, 17 SAY IIF(is_cr, s__cr+put_cr, 0) PICTURE "999 999 999.99"
@ 10, 17 SAY s__inv PICTURE "999 999 999.99"
@ 11, 16 SAY "("+TRANSFORM(s__invd,"999 999 999.99")+"$)"
@ 12, 17 SAY s__dep+mov_d+ROUND(put_d*cource,2)+put_r + ;
             IIF(is_cr, s__cr+put_cr, 0)- ;
             s__inv PICTURE "999 999 999.99"
@  9, 57 SAY "("+TRANSFORM(ROUND(put_d*cource,2),"999 999 999.99")+"�)"
@ 12, 57 SAY "("+TRANSFORM(ROUND(mov_d/cource,4),"9 999 999.9999")+"$)"

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Tst_Blank    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      蹍������ �諷��� ├ ゛����.                       �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 22.12.1998 陳�
PROCEDURE Tst_Blank

DO CASE
CASE ex = 3
*  DO Sw_Type
  RETURN .F.
CASE ex = 4
  DO Dossier WITH CUS_8C22.CUS_CODE
  RETURN .F.
CASE ex = 1
  IF put_d = 0 .AND. put_r = 0 .AND. mov_d = 0 .AND. put_cr = 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"��� 腑�� Ε, 甌＝癶キ��, �� 絎皋皀 �� ＜紕��皀爬�?!"
    mss(3) = ""
    DO Out_Mess WITH 7,"mss"
    RETURN .F.
  ENDIF
  IF ROUND(s__depd*cource,2)-mov_d < 0 .AND. mov_d > 0 .AND. EMPTY(close_cr)
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"�� ��°�� 礫���碎 ��矣 ����茱��� �爛え��!"
    mss(3) = ""
    DO Out_Mess WITH 7,"mss"
    RETURN .F.
  ENDIF
ENDCASE  
RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Sw_Type      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                          ��キ� 皋�� ぅ��Ж��.                          �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 22.12.1998 陳�
PROCEDURE Sw_Type

d_type = IIF(d_type # 3, IIF(LIST_TIT.IS_CHECK, 4, 1), 3)
*s__dep  = IIF(d_type = 1, ACC_SUM, ACC_SUM0)
s__cr   = IIF(d_type = 1, CR_SUM,  CR0_SUM)
@  3,  3 SAY IIF(d_type = 1, "", " ")
@  5,  3 SAY IIF(d_type = 1, " ", "")
put_d  = 0
put_r  = 0
mov_d  = 0
put_cr = 0
SHOW GETS
DO Calc_V
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Clr_Crd      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                     �諷�� ├ ���� 筰�Κ� �爛え��.                      �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 23.12.1998 陳�
PROCEDURE Clr_Crd

IF .NOT. is_cr
  put_cr = 0
ENDIF

DO Calc_V

RETURN .T.
