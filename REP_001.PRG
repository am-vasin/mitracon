*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                    ����→�: �諤竍�� �� �ム┏�.                        �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Rep_001

PRIVATE ln, wd, c1, c2, c3, dat_0, dat_1, dat_v0, dat_v1, ex, r_sav

PRIVATE f_list, f_codes, f_count, f_point
f_count = 1
f_point = 1
USE (base_path+"FIRMS")
DIMENSION f_list(f_count), f_codes(f_count)
f_list(f_count) = "�瓮 �爛く爬閧�� "
f_codes(f_count)= "   "
SCAN
  f_count = f_count+1
  DIMENSION f_list(f_count), f_codes(f_count)
  f_list(f_count) = B_NAME
  f_codes(f_count)= FIRM_CODE
ENDSCAN

HIDE MENU Main_M
USE (base_path+"PARMS")

dat_v0 = DAT_START   && ���珥���� ���� 縲��キ�� ����諷
dat_v1 = DATE()      && �オ竕�� ����
USE

dat_0 = dat_v1
dat_1 = dat_v1
ex    = 1

*                          �ム┏�
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�                                                            �
*�   �����讚�� ���� ��.��.����     ���ョ��� ���� ��.��.����   �
*�                                                            �
*�              < OK Ctrl-W > < �皖���碎瘴 Esc >              �
*�                                                            �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
*
ln = 8
IF SET("CENTURY") = "ON"
  wd = 64
  c1 = 6
  c2 = 35
ELSE
  wd = 60
  c1 = 6
  c2 = 33
ENDIF
c3 = FLOOR((wd-32)/2)

DEFINE WINDOW W_H_Bnds FROM 0, 0 TO 0, WCOLS("") NONE COLOR SCHEME 13
SHOW WINDOW W_H_Bnds

IF .NOT. S_Bounds()
  RELEASE WINDOW W_H_Bnds
  RETURN
ENDIF
DO Wt_Mess WITH "��ぃ�皰→� ����諷..."

USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG DOC_NUM

SELECT 0
DELETE FILE (tmpo_path+"sal_001.dbf")
DELETE FILE (tmpo_path+"sal_001.cdx")
CREATE DBF (tmpo_path+"sal_001") ;
      ( FIRM     C( 3),   ;
        DOC_NUM  C(10),   ;
        DOC_DATE D    ,   ;
        DOC_TIME N( 5),   ;
        NEW_DOC  C(10),   ;
        NEW_TYPE C( 1),   ;
        DOC_NAME C(FSIZE("CUS_NAME", "SALE_TIT")), ;
        CUS_CODE N(FSIZE("CUS_CODE", "SALE_TIT")), ;
        CUS_NAME C(FSIZE("CUS_NAME", "ACCOUNT")), ;
        SUM_T    N(10,2), ;
        SUM_D    N(10,2), ;
        SUM_K    N(10,2), ;
        WHERE    C( 3) )
USE (tmpo_path+"sal_001") EXCLUSIVE
INDEX ON CHR(VAL(SUBSTR(DOC_NUM,2,4)))+DTOS(DOC_DATE) TAG HRON
INDEX ON CHR(VAL(SUBSTR(DOC_NUM,2,4)))+FIRM+DTOS(DOC_DATE)+STR(DOC_TIME,5) TAG FIRM ADDITIVE

SELECT 0
DELETE FILE (tmpo_path+"rep_001.dbf")
CREATE DBF (tmpo_path+"rep_001") ;
      ( ARM_C  C( 3) ,  ;
        ARM_N  C(16) ,  ;
        ARM_S  C( 1) ,  ;
        SUM_T  N(10,2), ;
        SUM_D  N(10,2), ;
        SUM_K  N(10,2)   )
USE (tmpo_path+"rep_001") EXCLUSIVE

PRIVATE s0, s1, s2, ss0, ss1, ss2, i, j, s
PRIVATE sw1

dat_v0 = YEAR(dat_0)
dat_v1 = YEAR(dat_1)

SELECT SALE_TIT
SCAN FOR BETWEEN(DOC_DATE, dat_0, dat_1)
  cus_w = ""
  IF CUS_CODE # 0
    IF SEEK(CUS_CODE, "ACCOUNT")
      cus_w = ACCOUNT.CUS_NAME
    ENDIF
  ENDIF 
  SELECT SAL_001
  APPEND BLANK
  REPLACE FIRM     WITH SALE_TIT.FIRM,     ;
          DOC_NUM  WITH SALE_TIT.DOC_NUM,  ;
          DOC_DATE WITH SALE_TIT.DOC_DATE, ;
          DOC_TIME WITH SALE_TIT.DOC_TIME, ;
          NEW_DOC  WITH SALE_TIT.NEW_DOC,  ;
          NEW_TYPE WITH SALE_TIT.NEW_TYPE, ;
          DOC_NAME WITH SALE_TIT.CUS_NAME, ;
          CUS_CODE WITH SALE_TIT.CUS_CODE, ;
          CUS_NAME WITH cus_w,             ;
          SUM_T    WITH SALE_TIT.TOTAL_R,  ;
          SUM_D    WITH SALE_TIT.TOTAL_R-SALE_TIT.KASSA, ;
          SUM_K    WITH SALE_TIT.KASSA,    ;
          WHERE    WITH SALE_TIT.WHERE
  SELECT SALE_TIT
ENDSCAN
USE
SELECT ACCOUNT
USE
SELECT SAL_001
sw1 = "***"
SCAN
  SELECT REP_001
  IF SAL_001.WHERE # sw1
    sw1 = SAL_001.WHERE
    APPEND BLANK
    REPLACE ARM_C WITH sw1
  ENDIF
  REPLACE SUM_T WITH SUM_T+SAL_001.SUM_T, ;
          SUM_D WITH SUM_D+SAL_001.SUM_D, ;
          SUM_K WITH SUM_K+SAL_001.SUM_K
  SELECT SAL_001
ENDSCAN
SELECT 0
USE (base_path+"ARM_LST") ORDER TAG ARM_C
SELECT REP_001
SET RELATION TO ARM_C INTO ARM_LST
REPLACE ALL ARM_N WITH ARM_LST.ARM_N, ARM_S WITH ARM_LST.ARM_S
DO Wt_Mess
SELECT ARM_LST
USE
DO Use_Dummy
SELECT SAL_001
wd = FSIZE("DOC_NUM" )+13+ ;
     FSIZE("FIRM")+1+  ;
     FSIZE("SUM_T")+1+ ;
     FSIZE("SUM_D")+1+ ;
     FSIZE("SUM_K")
GO TOP
ln = WROWS("")-7
PRIVATE tt, ft, tt_m
tt_m = ""
DIMENSION tt(1), ft(2)
tt(1) = "�爼�"+PADC("N ���-��",FSIZE("DOC_NUM" ),"�")+"�"+   ;
        PADC(" ���� ",IIF(SET("CENTURY") = "ON", 10, 8),"�")+"�"+ ;
        PADC(" �祠�� ",FSIZE("SUM_T" ),"�")+ ;
        PADC(" ��瘁� ",FSIZE("SUM_K" ),"�")+ ;
        PADC("陳� �ク ",FSIZE("SUM_D"),"�")

ft(1) = ""
ft(2) = ""

DO Def_Win WITH "Sal_M", "Sal_E", "Sal_I", ln, wd, tt_m, tt, ft
HIDE WINDOW Sal_M, Sal_E, Sal_I

SELECT REP_001
wd = FSIZE("ARM_N")+ ;
     FSIZE("SUM_T")+ ;
     FSIZE("SUM_K")+ ;
     FSIZE("SUM_D")+2
SUM SUM_T, SUM_K, SUM_D TO ss0, ss1, ss2
GO TOP
ln = MIN(RECCOUNT(), WROWS("")-6)
tt_m = "�諤竍�� �� �ム┏� � "+TRANSFORM(dat_0,"@D")+" �� "+TRANSFORM(dat_1,"@D")
DIMENSION tt(1), ft(2)
tt(1) = PADC("��｀腑� �メ皰",FSIZE("ARM_N" ),"�")+   ;
        PADC("陳 �祠�� ",FSIZE("SUM_T" ),"�")+ ;
        PADC("陳 ��瘁� ",FSIZE("SUM_T" ),"�")+ ;
        PADC("陳� �ク ",FSIZE("SUM_D"),"�")
ft(1) = ""
ft(2) = PADL("�皰�� ",FSIZE("ARM_N" ))+   ;
        STR(ss0,FSIZE("SUM_T")+1,2)+      ;
        STR(ss1,FSIZE("SUM_K")+1,2)+      ;
        STR(ss2,FSIZE("SUM_D")+1,2)
ft(2) = PADR(ft(2),wd)
GO TOP

DO Def_Win WITH "Vyr_M", "Vyr_E", "Vyr_I", ln, wd, tt_m, tt, ft

DEFINE MENU List_A IN SCREEN COLOR SCHEME 4
DEFINE PAD List_I OF List_A PROMPT "�����き襯" KEY Ctrl-A, "Enter"
DEFINE PAD Exit OF List_A PROMPT "�諷��" KEY Ctrl-A, "Esc"

ON SELECTION PAD List_I OF List_A  DO Ch_Mode WITH "List_A","sal_lst"
ON SELECTION PAD Exit   OF List_A  DO Ch_Mode WITH "List_A","���!"

DEFINE MENU List_I IN SCREEN COLOR SCHEME 4
DEFINE PAD Prn_Inv OF List_I PROMPT "�ョ�碎 �����き��" KEY Ctrl-A, "Enter"
DEFINE PAD Cus_Rep OF List_I PROMPT "����→� �� ��┘�矣" KEY Ctrl-A, "Tab"
DEFINE PAD Firm OF List_I PROMPT "踳く�." KEY Ctrl-A, "F1"
DEFINE PAD Return  OF List_I PROMPT "��Б���" KEY Ctrl-A, "Esc"

ON SELECTION PAD Prn_Inv OF List_I DO Ch_Mode WITH "List_I","prn_inv"
ON SELECTION PAD Cus_Rep OF List_I DO Ch_Mode WITH "List_I","cus_rep"
ON SELECTION PAD Firm OF List_I DO Ch_Mode WITH "List_I","ch_firm"
ON SELECTION PAD Return  OF List_I DO Ch_Mode WITH "List_I","arm_lst"

PRIVATE what_do, rst

what_do = "arm_lst"

DO WHILE .T.
  DO CASE
  CASE what_do = "arm_lst"  && �諤竍�� �� ��｀腮� �メ���
    f_point = 1
    r_sav = 0
    rst = .F.
    HIDE WINDOW Sal_I, Sal_E, Sal_M, W_H_Bnds
    SELECT REP_001
    ACTIVATE SCREEN
    @ 0, 0 SAY PADL("�キ� F10 ",WCOLS()) COLOR SCHEME 13
    SHOW MENU List_A
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F10 ACTIVATE MENU List_A
    ON KEY LABEL LeftMouse DO M_Proc_1 WITH "List_A", "Vyr_"
    ln = "@Z "+REPLICATE("9",FSIZE("SUM_D1"))
    ACTIVATE WINDOW Vyr_E
    BROWSE FIELDS ARM_N:H="", ;
                  SUM_T:H="", ;
                  SUM_K:H="", ;
                  SUM_D:H="", ;
                  DUMMY.F:H=""  ;
           FREEZE DUMMY.F NOMODIFY NOCLEAR WINDOW Vyr_I IN WINDOW Vyr_E
    ON KEY LABEL Enter
    ON KEY LABEL F10
    ON KEY LABEL LeftMouse
    HIDE MENU List_A
    IF READKEY() % 256 # 12 .AND. what_do = "arm_lst"
      what_do = "sal_lst"
    ENDIF
    IF what_do = "arm_lst"
      what_do = "���!"
    ENDIF
  CASE what_do = "sal_lst"   && �����き襯 �� ��｀腑� �メ皀
    HIDE WINDOW W_H_Bnds
    ACTIVATE SCREEN
    @ 0, 0 SAY PADL("�キ� F10 ",WCOLS()) COLOR SCHEME 13
    SHOW MENU List_I
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Tab   DO Ch_Mode WITH "List_I","cus_rep"
    ON KEY LABEL F1    DO Ch_Mode WITH "List_I","ch_firm"
    ON KEY LABEL F10 ACTIVATE MENU List_I
    ON KEY LABEL LeftMouse DO M_Proc_1 WITH "List_I", "Sal_"
    DO Show_Docs WITH REP_001.ARM_C+IIF(f_point=1, "", f_codes(f_point))
    ON KEY LABEL LeftMouse
    ON KEY LABEL Enter
    ON KEY LABEL Tab
    ON KEY LABEL F10
    rst = .T.
    IF what_do = "sal_lst"
      what_do = IIF(READKEY()%256 = 12, "arm_lst", "prn_inv")
    ENDIF
  CASE what_do = "prn_inv"   && �ョ�碎 �����き��
    SHOW WINDOW W_H_Bnds
    IF What_P() = "N"
      DO Prn_Inv WITH FIRM, DOC_NUM, DOC_DATE
    ELSE
      DO P_Bill_F WITH FIRM, NEW_DOC, DOC_DATE, NEW_TYPE
    ENDIF
    what_do = "sal_lst"
  CASE what_do = "cus_rep"   && �痰�爬� ��┘���
    DO Acc_Hist WITH CUS_CODE
    what_do = "sal_lst"
  CASE what_do = "ch_firm"   && �襦�� �爛く爬閧��
    DO Ch_Firm
    what_do = "sal_lst"
  OTHERWISE
    EXIT
  ENDCASE
ENDDO

CLOSE DATA
RELEASE WINDOWS W_H_Bnds, Vyr_M, Vyr_E, Vyr_I, Sal_M, Sal_E, Sal_I
RELEASE MENU List_A
RELEASE MENU List_I
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                    ��皀���皋↓�� 竅����→� ���.                       �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE S_Bounds
PRIVATE mss

DO D_W WITH "S_Bounds", ln, wd, "�ム┏�", 1

@ 3, c1 SAY "�����讚�� ����" GET dat_0 PICTURE "@D"
@ 3, c2 SAY "���ョ��� ����"  GET dat_1 PICTURE "@D"
@ 5, c3 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "
READ CYCLE VALID V_Dat()

RELEASE WINDOW S_Bounds
RETURN ex = 1

*********************************************************************
PROCEDURE V_Dat

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

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�              蹍甃�矗 �����き諷 � ��｀腑�� �メ��.                     �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Show_Docs
PARAMETER k
PRIVATE mss, pic

SELECT SAL_001

IF LEN(k) = 1
  SET ORDER TO TAG HRON
  IF .NOT. SEEK(k)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"�モ �����き諷 � 轤��� ��｀腑�� �メ��!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    what_do = "arm_lst"
    RETURN
  ENDIF
ELSE
  SET ORDER TO TAG FIRM
  IF .NOT. SEEK(k)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"�モ �����き諷 � 轤��� ��｀腑�� �メ�� �� 礫�������� �爛く爬閧��!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    what_do = "arm_lst"
    RETURN
  ENDIF
ENDIF

ACTIVATE WINDOW SAL_M
@ 0, 0 SAY PADC(ALLTRIM(REP_001.ARM_N),WCOLS()) COLOR SCHEME 5

ACTIVATE WINDOW SAL_E
pic = "@Z 9999999.99"

IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ELSE
  GO TOP
ENDIF
BROWSE FIELDS FIRM:H="", ;
              f0001 = LEFT(DOC_NUM,4)+"-"+ALLTRIM(SUBSTR(DOC_NUM,5)):11:H="", ;
              DOC_DATE:H="":P="@D",   ;
              SUM_T:P=pic:H="",   ;
              SUM_K:P=pic:H="",   ;
              SUM_D:P=pic:H="",   ;
              DUMMY.F:H=""  ;
       FREEZE DUMMY.F     ;
       WHEN Sh_Cus()      ;
       KEY k              ;
       NOMODIFY NOCLEAR WINDOW Sal_I IN WINDOW Sal_E
r_sav = RECNO()
RETURN

*********************************************************************
PROCEDURE Sh_Cus
PRIVATE s

IF EMPTY(CUS_NAME)
  s = DOC_NAME
ELSE
  s = CUS_NAME
ENDIF

ACTIVATE WINDOW Sal_M SAME
s = PADR(s,WCOLS()-4)
@ WROWS()-1, 2 SAY s
ACTIVATE WINDOW (ALIAS())

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�        蹍������ - �÷�｀砒┴ �謌�� か� 甎�瓷��.                      �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE M_Proc_1
PARAMETER menu_nm, wind_nm
PRIVATE mc, mr, cr

*
*  �瓱�縲����� �キ�
*
IF MENU() == UPPER(menu_nm)
  RETURN
ENDIF

IF MROW("") = 0
  IF .NOT. MENU() == UPPER(menu_nm)
    ACTIVATE MENU (menu_nm)
  ENDIF
  RETURN
ENDIF

*
*  ｪ�� BROWSE
*
mc = MCOL(wind_nm+"E")
mr = MROW(wind_nm+"E")
IF mc >= 0 .AND. mr >= 0
  KEYBOARD CHR(13)
  RETURN
ENDIF

= INKEY("M")

*
*  �甅�↓�� ����
*
mc = MCOL(wind_nm+"M")
mr = MROW("")
IF mc >= 0
  IF mr = WLROW(wind_nm+"E")-1
    KEYBOARD "{PgUP}"
  ENDIF
  IF mr = WLROW(wind_nm+"E")+WROWS(wind_nm+"E")
    KEYBOARD "{PgDn}"
  ENDIF
ENDIF
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                �皰膈錺� 皋� �ョ���ガ��� ぎ�祠キ��.                    �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE What_P
PRIVATE ex

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

*      ����� ぎ�祠キ� �ョ���碎?
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�                                   �
*�  < �����き竡 > < �腑�-���矣珮 >   �
*�                                   �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
PRIVATE ex
                     &&   ｡褊←錺� � ������錺� ���� ゛����
ex   = 1             &&
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 6, 39, "����� ぎ�祠キ� �ョ���碎?"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 3, 4 GET ex PICTURE "@*HT \ �����き竡 ; �腑�-���矣珮 "

READ CYCLE

*--------------------------------------------------------------------------
ex = IIF(ex = 1, "N", "B")
POP KEY
RELEASE WINDOW (win_name)
RETURN ex

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Ch_Firm      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           �襦�� �爛く爬閧��.                           �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 08.06.1999 陳�
PROCEDURE Ch_Firm
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

PRIVATE p, ln, wd

p  = f_point
ln = MIN(WROWS("")-5, f_count+3)
wd = LEN(f_list(1))+4
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH ln, wd, "踳く爬閧��"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
DO WHILE .T.
  p = MIN(p,ALEN(f_list))
  p = MAX(p, 1)
  @ 1, 1 MENU f_list, f_count, WROWS()-3
  READ MENU TO p
  IF READKEY() = 12
    EXIT
  ENDIF
  IF READKEY() = 15
    f_point = p
    EXIT
  ENDIF
ENDDO

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
