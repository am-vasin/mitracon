*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� prp_cust     ��о�｀砒┴ ��むォ ��瓱�           14.04.97 16:43:53 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�               ��ぃ�皰→� ゛����� ��┘�皰� か� �����ぎ�.                �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Prp_Cust

PRIVATE mss, pl_list, pl_chr, pl_num, cnt
IF (sys_char # "A" .AND. sale_mode # "Mitin")
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��Г���碎 ゛���� ��Ν� 皰�讓� �� �甅�↓�� �����お�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

pl_num = 0
pl_chr = ""

USE (base_path+"PLACES")
SCAN
  IF P_CHAR = "A"
    LOOP
  ENDIF
  pl_num = pl_num+1
  DIMENSION pl_list(pl_num)
  pl_list(pl_num) = P_NAME
  pl_chr = pl_chr+P_CHAR
ENDSCAN
USE (base_path+"ACCOUNT") ORDER TAG EMP_NAME

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

PRIVATE ex, point, n, c, i, n_flt
ex   = 1
point = 1
n = 0
*------------------------------------------------------------------------
*
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�          ç��お�                              �
*� 敖陳陳陳陳陳陳陳陳陳陳陳陳� � ���-〓 ゛����� � �
*� � ....:....!....:....!....� � ��Г��� 99999  � �
*� � ....:....!....:....!....� � ����※碎 9999  � �
*� � ....:....!....:....!....� 青陳陳陳陳陳陳陳潰 �
*� � ....:....!....:....!....�                    �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳�                    �
*�                                                �
*�        < OK Ctrl-W > < �皖���碎瘴 Esc >        �
*�                                                �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH MAX(9+MIN(pl_num,4),11), 52, "������ ��┘�皰�"
@ 2, 12 SAY "ç��お�"
DO Sun_Bord WITH 3, 31, 6, 48
@ 3, 32 SAY " ���-〓 ゛����� "
@ 4, 34 SAY "��Г���"
= Cnt_Emp()
@ 5, 34 SAY "����※碎"
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 3,  3 GET point PICTURE "@&" FROM pl_list SIZE pl_num+2, 27 VALID Cnt_Emp()
@ 5, 43 GET n PICTURE "@Z 999"
@ WROWS()-3, 10 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

cnt = 0
IF ex = 1 .AND. n > 0
  SELECT ACCOUNT
  c = SUBSTR(pl_chr, point,1)
  DO Wt_Mess WITH "��Г�ガ..."
  FOR i = 1 TO n
    n_flt = Doc_Num("CUS_CODE", def_firm, arm, DATE())
    APPEND BLANK
    REPLACE SUBSYS WITH c, CUS_CODE WITH n_flt
    cnt = cnt+1
    @ WROWS()-1, 0 SAY PADC(ALLTRIM(STR(cnt)), WCOLS())
  ENDFOR
  DO Wt_Mess
ENDIF
USE
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                     ��め腑� ゛����� か� �����お�.                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 15.04.97 10:52:28 夕
PROCEDURE Cnt_Emp

PRIVATE i, c

i = 0
c = SUBSTR(pl_chr, point,1)
IF SEEK(c)
  COUNT REST WHILE c = SUBSYS TO i
ENDIF

@ 4, 41 SAY STR(i,5)

RETURN .T.
