*������������������������������������������������������������������������ͻ
*� ���� Rep_Sup      ���ࠡ��稪 ���३ ��ᨭ           07.10.97 15:03:09 �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�         ��ନ஢���� ���� �� ⮢���, ���⮬� �� ॠ������.          �
*�                                                                        �
*������������������������������������������������������������������������ͼ
PROCEDURE Rep_Sup
PARAMETERS tmpCus, tmpType

PRIVATE c_code, c_name, pr_t, stru_, n_filelds
PRIVATE sw_nal, pr_r, pr_d, is_usd, dt_0
PRIVATE f_c000, f_n000, f_c_old
PRIVATE shab1, shab2, sw_zer, non_zer, toAsk
shab1 = "abcdefghijklmnopqrstuvwxyz������񦧨�����������������������"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZ���������������������������������"
f_c000 = ""
f_n000 = ""
f_c_old= f_c000
non_zer = ""
sw_zer = .F.
IF File_O(base_path+"NON_ZER.MEM")
  RESTORE FROM (base_path+"NON_ZER.MEM") ADDITIVE
ENDIF
c_code  = 0
c_name  = SPACE(50)
pr_t = 1
toAsk = .T.
IF TYPE("tmpCus") = "N"
  toAsk = .F.
  c_code = tmpCus
  IF TYPE("tmpType") = "N"
    pr_t = tmpType
  ENDIF
ENDIF

USE (base_path+"PARMS")
dt_0 = DAT_START

USE (base_path+"CUSTOMER") ORDER TAG CODE

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_NAME

DO Use_Link

DO Use_Dummy

SELECT ACCOUNT

IF toAsk
  IF .NOT. Rep_Blnk()
    CLOSE DATABASES
    RETURN
  ENDIF
ENDIF

CLOSE DATABASES
USE (base_path+"PARMS")
is_usd = VALUTA = "$"

*USE (base_path+"SALE") ORDER TAG CODE_ACT
USE (base_path+"SALE") ORDER TAG PARTY

SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG DOC_NUM

SELECT 0
USE (base_path+"REPORT")
n_fields = AFIELDS(stru_)
DIMENSION stru_(n_fields+2, 4)
stru_(n_fields+1, 1) = "S_REF"
stru_(n_fields+1, 2) = "N"
stru_(n_fields+1, 3) = 10
stru_(n_fields+1, 4) = 0
stru_(n_fields+2, 1) = "A_REF"
stru_(n_fields+2, 2) = "N"
stru_(n_fields+2, 3) = 10
stru_(n_fields+2, 4) = 0

SELECT 0
DELETE FILE (tmpo_path+"REP_TMP.DBF")
DELETE FILE (tmpo_path+"REP_TMP.CDX")
CREATE DBF (tmpo_path+"REP_TMP") FROM ARRAY stru_

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE

SELECT 0
USE (base_path+"ACT_S") ORDER TAG DOC_NUM

SELECT 0
USE (base_path+"ACT_S_T") ORDER TAG SUP_CODE

SEEK c_code
SCAN REST WHILE SUP_CODE = c_code FOR FLG_BUY = "R"  && �ਥ��� ���� ���⠢騪�
  SELECT ACT_S
  SEEK ACT_S_T.FIRM+LEFT(DTOS(ACT_S_T.DOC_DATE),4)+ACT_S_T.DOC_NUM
  SCAN REST WHILE FIRM = ACT_S_T.FIRM .AND. DOC_DATE = ACT_S_T.DOC_DATE .AND. DOC_NUM = ACT_S_T.DOC_NUM ;
                   FOR QNT_REP # REST_REP  && ����樨 �ਥ����� ���
    SELECT SALE
*    SEEK STR(ACT_S.CODE,7)+ACT_S.FIRM+LEFT(DTOS(ACT_S.DOC_DATE),4)+ACT_S.DOC_NUM
    SEEK ACT_S.PARTY
    SCAN REST WHILE PARTY = ACT_S.PARTY FOR QNT_REP # 0
      IF pr_t = 1
        pr_r = 0
        pr_d = 0
        IF SEEK(CODE,"STOCK")
          IF is_usd
            pr_d = STOCK.PRICE_B
            pr_r = ROUND(pr_d*kurs,2)
          ELSE
            pr_r = STOCK.PRICE_B_R
            pr_d = ROUND(pr_r/kurs,4)
          ENDIF
        ENDIF
      ELSE
        pr_r = ACT_S.PRICE_R
        pr_d = ROUND(pr_r/kurs,4)
      ENDIF
      sw_nal = .T.
      *
      *   sw_nal = .T., �᫨ �த��� �� �������� �।�����!
      *
      IF SEEK(FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM,"SALE_TIT")
        sw_nal = SALE_TIT.FIRM $ non_zer
      ENDIF
      SELECT REP_TMP
      APPEND BLANK
      REPLACE INC_N   WITH ACT_S.DOC_NUM,  ;
              INC_F   WITH ACT_S.FIRM,     ;
              INC_D   WITH ACT_S.DOC_DATE, ;
              EXP_N   WITH SALE.DOC_NUM,   ;
              EXP_F   WITH SALE.FIRM,      ;
              EXP_D   WITH SALE.DOC_DATE,  ;
              IS_NAL  WITH sw_nal,         ;
              CODE    WITH SALE.CODE,      ;
              QNT     WITH SALE.QNT_REP,   ;
              PRICE_R WITH pr_r,           ;
              PRICE_D WITH pr_d,           ;
              A_REF   WITH RECNO("ACT_S"), ;
              S_REF   WITH RECNO("SALE")
      SELECT SALE
    ENDSCAN
    SELECT ACT_S
  ENDSCAN
  SELECT ACT_S_T
ENDSCAN

SELECT REP_TMP
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��祣� �� �த�������!"
  mss(3) = ""
  IF toAsk
    DO Out_Mess WITH 7, "mss"
  ENDIF
  CLOSE DATABASES
  DELETE FILE (tmpo_path+"REP_TMP.DBF")
  DELETE FILE (tmpo_path+"REP_TMP.CDX")
  RETURN
ENDIF

PRIVATE n_r, d_r, i_n, i_d, i
SELECT 0
USE (base_path+"REP_TIT") ORDER TAG N_REP
GO BOTTOM
IF EOF() .OR. BOF()
  n_r = 1
ELSE
  n_r = IIF(N_REP = 99999, 1, N_REP+1)
ENDIF
d_r = DATE()
APPEND BLANK
REPLACE N_REP    WITH n_r,    ;
        D_REP    WITH d_r,    ;
        SUP_CODE WITH c_code, ;
        VALUTA   WITH IIF(pr_t = 1 .AND. is_usd, "$", "�"), ;
        PR_TYPE  WITH pr_t    ;
        WHO      WITH user,   ;
        WHERE    WITH arm
SELECT REP_TMP

FOR i = 1 TO FCOUNT()
  IF FIELD(i) == "N_REP"
    i_r = i
  ENDIF
  IF FIELD(i) == "D_REP"
    i_d = i
  ENDIF
ENDFOR

SCAN
  GO REP_TMP.S_REF IN SALE
  GO REP_TMP.A_REF IN ACT_S
  SCATTER TO stru_
  DIMENSION stru_(FCOUNT("REPORT"))
  stru_(i_r) = n_r
  stru_(i_d) = d_r
  SELECT REPORT
  APPEND FROM ARRAY stru_
  SELECT SALE
  REPLACE SALE.QNT_REP WITH SALE.QNT_REP-REP_TMP.QNT
  SELECT ACT_S
  REPLACE ACT_S.QNT_REP WITH ACT_S.QNT_REP-REP_TMP.QNT
  IF ACT_S.DOC_DATE < dt_0
    SELECT STOCK
    SEEK ACT_S.CODE
    REPLACE QNT_S_REP WITH QNT_S_REP - REP_TMP.QNT
  ENDIF
  SELECT REP_TMP
ENDSCAN
SELECT ACT_S_T
SEEK c_code
REPLACE REST WHILE SUP_CODE = c_code FOR FLG_BUY = "R" .AND. EMPTY(REP_SUP);
  REP_SUP WITH n_r
CLOSE DATABASES
DELETE FILE (tmpo_path+"REP_TMP.DBF")
DELETE FILE (tmpo_path+"REP_TMP.CDX")

DO Out_R_S WITH n_r, d_r  && ����� �/��� ������ ���� � DBF-�ଠ�

RETURN

*������������������������������������������������������������������������ͻ
*�                                                                        �
*�                       ���������� ������ ����.                        �
*�                                                                        �
*����������������������������������������������������� 07.10.97 15:07:42 ͼ
PROCEDURE Rep_Blnk

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

*��������������������������������������������������������Ŀ
*�        [ ] ���᮪ �।���⨩ "�㫥����" ⨯�          �
*� �� [ ] ���⠢騪 �����������������������������������Ŀ �
*� � ....:....!....:....!....:....!....:....!....:....! � �
*� ������������������������������������������������������ �
*� �� ���� ��������������������������������������������Ŀ �
*� �     ( ) �� ������������  ( ) �� �ਥ����� ���     � �
*� ������������������������������������������������������ �
*�            < OK Ctrl-W > < �⪠������ Esc >            �
*����������������������������������������������������������

PRIVATE ex, sup_sw
ex   = 1
sup_sw = .F.
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 11, 60, "���� � ⮢��, ���⮬ �� ॠ������"
DO Sun_Bord WITH 3, 3, 5, 56, " ���⠢騪 "
DO Sun_Bord WITH 6, 3, 8, 56, " ���� "

*------------------------------------------------------------------------
*      ���� ����� ������
*
@ 3,  6 GET sup_sw PICTURE "@*C ���⠢騪 " VALID Ch_Sup()
@ 4,  5 GET c_name WHEN .F.
@ 7,  9 GET pr_t PICTURE "@*RH �� ������������ ;�� �ਥ����� ���"
@ 9, 14 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "
@ 2, 10 GET sw_zer PICTURE '@*C ���᮪ �।���⨩ "�㫥����" ⨯�' VALID Ch_Zer()
READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1 .AND. c_code # 0

*������������������������������������������������������������������������ͻ
*�                                                                        �
*�                           �롮� ���⠢騪�.                            �
*�                                                                        �
*����������������������������������������������������� 08.10.97 09:51:53 ͼ
PROCEDURE Ch_Sup

IF .NOT. sup_sw
  RETURN .T.
ENDIF

IF c_code = 0
  GO TOP
ELSE
  SELECT ACCOUNT
  SET ORDER TO TAG CUS_CODE
  SEEK c_code
  SET ORDER TO TAG CUS_NAME
ENDIF

IF Custs("")
  c_code = ACCOUNT.CUS_CODE
  c_name = ACCOUNT.CUS_NAME
  SHOW GET c_name
  IF .NOT. SEEK(c_code,"CUSTOMER") .OR. EMPTY(CUSTOMER.SUP)
    WAIT WINDOW '��������! � ������ ��� ����⪨ "���⠢騪"' NOWAIT
  ENDIF
  SHOW GET c_name
ENDIF

sup_sw = .F.
SHOW GET sup_sw

RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Ch_Zer       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�              ��ନ஢���� �।���⨩ "���㫥����" ⨯�.               �
*�                                                                        �
*����������������������������������������������������������� 06/03/1999 ���
PROCEDURE Ch_Zer

*
*  ��६���� ���ﭨ� ��� ������樨
*
IF .NOT. sw_zer
  RETURN .T.
ENDIF

PRIVATE stat_type     && ��� ������樨: 0 - ���⠭�����;
                                        1 - �����;
                                        2 - BROWSE - ⠡���;
                                        3 - BROWSE - ᯨ᮪.
PRIVATE what_do       && ��� ०���.
PRIVATE menu_name     && ��� �ᨭ�஭���� ����.
PRIVATE last_mouse    && �६� ��᫥����� ������ ���ਭ�� ������.
PRIVATE win_name      && ��� ���� ( ���� ��� BROWSE ).
PRIVATE s_sav         && ����� ࠡ�祩 ������ ��� ��࠭���� � ����⠭�������!

*
*   ������塞 ���祭�ﬨ ��६���� ���ﭨ�...
*
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ���뢠�� 䠩�� �.�., ��⠭�������� �裡 � �. �.
*
s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT 0
USE (base_path+"FIRMS") ORDER TAG B_NAME ALIAS F_9603 AGAIN

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD OK OF (menu_name) PROMPT "���࠭���" KEY Ctrl-A, "Enter"
ON SELECTION PAD OK OF (menu_name) DO Sw_Mode WITH "OK"

DEFINE PAD Mark OF (menu_name) PROMPT "�⬥⪠" KEY Ctrl-A, "�஡��"
ON SELECTION PAD Mark OF (menu_name) DO Sw_Mode WITH "Mark"

DEFINE PAD Exit OF (menu_name) PROMPT "�⪠������" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
ln = MAX(WROWS("")-11, RECCOUNT())   && ������⢮ ������� ��ப BROWSE
wd = 22
DO D_Wins WITH ln, wd, "�।�����", 0, 0
@ 1, 2 SAY "���č����������������0"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL SpaceBar DO Mark

*
*    BROWSE - ����
*
    BROWSE FIELDS FIRM_CODE:3:H="",  ;
                  B_NAME:H="",       ;
                  F000 = IIF(FIRM_CODE+"�" $ non_zer, " ", "�"):1:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "OK")
    ENDIF

  CASE what_do = "OK"    && ��室

    SAVE ALL LIKE non_zer TO (base_path+"NON_ZER.MEM")
    EXIT

  CASE what_do = "Mark"  && �⬥���

    DO Mark
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
*   �����⭥�쪮 (�����⭥�쪮!) ����뢠�� DBF-䠩��.
*
USE
SELECT (s_sav)

sw_zer = .F.
SHOW GET sw_zer

RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Mark         ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                       ��⠭�����/����� �⬥��.                        �
*�                                                                        �
*����������������������������������������������������������� 06/03/1999 ���
PROCEDURE Mark
PRIVATE p

p = AT(FIRM_CODE+"�", non_zer)
IF p = 0
  non_zer = non_zer+FIRM_CODE+"�"
ELSE
  non_zer = STUFF(non_zer, p, 4, "")
ENDIF

KEYBOARD "{DnArrow}"

RETURN