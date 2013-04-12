*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�               �ணࠬ�� ���४樨 ����� ������.                      �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
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

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                  �ணࠬ�� ���४樨 ����� ������.                   �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Corr_Name

*
*  ��६���� ���ﭨ� ��� ������樨
*

PRIVATE stat_type     && ��� ������樨: 0 - ���⠭�����;
                                        1 - �����;
                                        2 - BROWSE - ⠡���;
                                        3 - BROWSE - ᯨ᮪.
PRIVATE what_do       && ��� ०���.
PRIVATE menu_name     && ��� �ᨭ�஭���� ����.
PRIVATE last_mouse    && �६� ��᫥����� ������ ���ਭ�� ������.
PRIVATE win_name      && ��� ���� ( ���� ��� BROWSE ).

*
*   ������塞 ���祭�ﬨ ��६���� ���ﭨ�...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ����ঠ⥫쭠� ���� �ணࠬ��:
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
*������������������������������������������������������Ŀ
*��� ��� ������ �������������������������������������Ŀ�
*�� ....:....!....:....!....:....!....:....!....:....! ��
*��                                                    ��
*��                ���⪮� ....:....!....:.           ��
*������������������������������������������������������ٳ
*���� ������ �����������������������������������������Ŀ�
*��          [ ] ���������     [ ] ���⠢騪          ��
*������������������������������������������������������ٳ
*�   < OK Ctrl-W > <  ���ଠ�� > < �⪠������ Esc >   �
*��������������������������������������������������������

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 12, 58, "������"
DO Sun_Bord WITH  2,  2,  6, 55, " ��� ������ "
DO Sun_Bord WITH  7,  2,  9, 55, " ������ "

*------------------------------------------------------------------------
*      ���� ����� ������
*
@  3,  4 GET nm_w_l
@  5, 19 SAY "���⪮�" GET nm_w_b WHEN is_sup .OR. is_fil .OR. cus_exi
@  8, 13 GET is_fil PICTURE "@*C ���������" VALID Tst_Fil()
@  8, 32 GET is_sup PICTURE "@*C ���⠢騪"
@ 10,  5 GET ex PICTURE "@*HT \! OK Ctrl-W ;\  ���ଠ�� ;\? �⪠������ Esc "

READ CYCLE VALID Tst_Blank()

IF ex = 1
  *
  * ��ࠡ��뢠�� �����
  REPLACE CUS_NAME   WITH nm_w_l, ;
          ACC_TYPE   WITH IIF(is_fil, 4, 0), ;
          DATE_CORR  WITH DATE(), ;
          WHO_CORR   WITH user,   ;
          WHERE_CORR WITH arm

  IF cus_exi
    IF EMPTY(CUSTOMER.RYN) .OR. .NOT. EMPTY(CUSTOMER.RYN) .AND. .NOT. is_fil
      *
      *  ����� ��஥ �᫮��� ��� 䨫����� ����祭��� �� ������窮�!
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

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Tst_Blank    ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                            ��室 �� ������.                            �
*�                                                                        �
*����������������������������������������������������������� 04/16/1999 ���
PROCEDURE Tst_Blank

PRIVATE mss
IF ex > 1
  RETURN .T.
ENDIF

IF (is_sup .OR. is_fil .OR. cus_exi) .AND. EMPTY(nm_w_b)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"� ������ ��砥 ���⪮� ��� �� ����� ���� �����!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF EMPTY(nm_w_l)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"������ ��� �� ����� ���� �����!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
  
RETURN .T.


*������������������������������������������������������������������������Ŀ
*�   ��楤�� Tst_Fil                                                    �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                       ����⪠ "ॠ������".                            �
*�                                                                        �
*����������������������������������������������������������� 12.04.2001 ���
PROCEDURE Tst_Fil

PRIVATE mss, s_sav
s_sav = SELECT()

IF .NOT. is_fil
  SELECT 0
  USE (base_path+"RYN_L") ORDER TAG CUS_CODE ALIAS R_1412
  IF SEEK(CUSTOMER.CODE)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"��������� - ��� ������� ! "
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    is_fil = .T.
    SHOW GET is_fil
  ENDIF
  USE
ENDIF

SELECT(s_sav)
RETURN .T.