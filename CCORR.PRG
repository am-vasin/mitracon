*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�               蹍������ ��玻オ罔� ━キ� ��┘���.                      �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE CCorr
PRIVATE v, pl

PUSH KEY CLEAR
HIDE MENU Main_M

pl = Choose_P()

DO Use_Dummy
DO Use_Link
SELECT 0
USE (base_path+"CUSTOMER") ORDER TAG CODE
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_NAME
DO WHILE .T.
  SELECT ACCOUNT
  IF Custs("", .T., .F., .F., pl)
    DO WHILE .T.
      IF INLIST(Corr_Name(), 1, 3)
        EXIT
      ELSE
        DO Acc_Hist WITH CUS_CODE
      ENDIF
    ENDDO
  ELSE
    EXIT
  ENDIF
ENDDO

CLOSE DATABASES
POP KEY

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                  蹍������ ��玻オ罔� ━キ� ��┘���.                   �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Corr_Name

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

PRIVATE ex, nm_w_b, nm_w_l, cus_exi, is_fil, is_sup
ex   = 1
nm_w_l = CUS_NAME
nm_w_b = SPACE(16)
cus_exi = SEEK(CUS_CODE, "CUSTOMER")
is_fil = ACC_TYPE = 4
is_sup = .F.
IF cus_exi
  nm_w_b = CUSTOMER.B_NAME
  is_sup = .NOT. EMPTY(CUSTOMER.SUP)
ENDIF
PUSH KEY CLEAR
*
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*楽� ��� ��┘��� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳審
*崖 ....:....!....:....!....:....!....:....!....:....! 崖
*崖                                                    崖
*崖                ��牀皖�� ....:....!....:.           崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳抒
*楽陳 ���Κ� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳審
*崖          [ ] ����├�皰�     [ ] ��痰�∫┴          崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳抒
*�   < OK Ctrl-W > <  ��筮爼�罔� > < �皖���碎瘴 Esc >   �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 12, 58, "��┘��"
DO Sun_Bord WITH  2,  2,  6, 55, " ��� ��┘��� "
DO Sun_Bord WITH  7,  2,  9, 55, " ���Κ� "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@  3,  4 GET nm_w_l
@  5, 19 SAY "��牀皖��" GET nm_w_b WHEN is_sup .OR. is_fil .OR. cus_exi
@  8, 13 GET is_fil PICTURE "@*C ����├�皰�" VALID Tst_Fil()
@  8, 32 GET is_sup PICTURE "@*C ��痰�∫┴"
@ 10,  5 GET ex PICTURE "@*HT \! OK Ctrl-W ;\  ��筮爼�罔� ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Tst_Blank()

IF ex = 1
  *
  * �矗���硅��ガ ゛���
  REPLACE CUS_NAME   WITH nm_w_l, ;
          ACC_TYPE   WITH IIF(is_fil, 4, 0), ;
          DATE_CORR  WITH DATE(), ;
          WHO_CORR   WITH user,   ;
          WHERE_CORR WITH arm

  IF cus_exi
    IF EMPTY(CUSTOMER.RYN) .OR. .NOT. EMPTY(CUSTOMER.RYN) .AND. .NOT. is_fil
      *
      *  ����� 紲矗�� 竅��※� か� 筥������ ���ョキ�諷 �� Бェぎ腦��!
      *
      REPLACE CUSTOMER.RYN WITH IIF(is_fil, "*", " ")
    ENDIF
    REPLACE CUSTOMER.B_NAME WITH nm_w_b, ;
            CUSTOMER.L_NAME WITH nm_w_l, ;
            CUSTOMER.SUP    WITH IIF(is_sup, "*", " "), ;
            CUSTOMER.CUS    WITH "*"
  ELSE
    IF is_fil .OR. is_sup
      SELECT CUSTOMER
      APPEND BLANK
      REPLACE CUSTOMER.CODE    WITH ACCOUNT.CUS_CODE, ;
              CUSTOMER.B_NAME  WITH nm_w_b, ;
              CUSTOMER.L_NAME  WITH nm_w_l, ;
              CUSTOMER.RYN     WITH IIF(is_fil, "*", " "), ;
              CUSTOMER.SUP     WITH IIF(is_sup, "*", " "), ;
              CUSTOMER.CUS     WITH "*", ;
              CUSTOMER.DATE_ON WITH ACCOUNT.DATE_ON, ;
              CUSTOMER.WHO_ADD WITH ACCOUNT.WHO, ;
              CUSTOMER.WHERE_ADD WITH ACCOUNT.WHERE
      SELECT ACCOUNT
    ENDIF
  ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Tst_Blank    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                            �諷�� ├ ゛����.                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 04/16/1999 陳�
PROCEDURE Tst_Blank

PRIVATE mss
IF ex > 1
  RETURN .T.
ENDIF

IF (is_sup .OR. is_fil .OR. cus_exi) .AND. EMPTY(nm_w_b)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"� ������ 甄竍�� ��牀皖�� ━� �� ��Ε� °碎 �竅硅�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF EMPTY(nm_w_l)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"������ ━� �� ��Ε� °碎 �竅硅�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
  
RETURN .T.


*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Tst_Fil                                                    �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                       ���モ�� "爛��├�皰�".                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 12.04.2001 陳�
PROCEDURE Tst_Fil

PRIVATE mss, s_sav
s_sav = SELECT()

IF .NOT. is_fil
  SELECT 0
  USE (base_path+"RYN_L") ORDER TAG CUS_CODE ALIAS R_1412
  IF SEEK(CUSTOMER.CODE)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"����├�皰� - ��� ぎ�Ν┴ ! "
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    is_fil = .T.
    SHOW GET is_fil
  ENDIF
  USE
ENDIF

SELECT(s_sav)
RETURN .T.