*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�             蹍������ □��� ┃ぅ�瓱珮ガ��� ���皀��.                   �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Inp_Ind

PRIVATE new_cli, cli_nm, cli_cd, pay_dat, pay_sum, ex, p_doc
PRIVATE ln, wd, c1, c2

HIDE MENU Main_M
ACTIVATE SCREEN
@ 0,0 SAY SPACE(WCOLS()) COLOR SCHEME 13

new_cli = .T.
cli_nm  = SPACE(50)
cli_cd  = 0
pay_dat = DATE()
pay_sum = 0

p_doc = .T.
ex = 1
DO Use_Dummy
SELECT 0
USE ACCOUNT ORDER 
*
*            踸絎� ぅ�ィ �� ┃ぅ�瓱珮ガ覃 ぅ��Ж�
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� 敖陳 ��┘�� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕 �
*� � [ ] ���覃                                              � �
*� � ��� ....:....1....:....2....:....3....:....4....:....5 � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�         �祠�� 9999999.99      [ ] �ョ�碎 ぎ�祠キ��         �
*�              < OK Ctrl-W > < �皖���碎瘴 Esc >              �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
*
ln = 10
wd = FSIZE("CUS_NAME")+14
DO D_W WITH "Inp_Pay", ln, wd, "踸絎� ぅ�ィ �� ┃ぅ�瓱珮ガ覃 ぅ��Ж�", 1
c1 = FLOOR((wd-42)/2)
c2 = FLOOR((wd-32)/2)

@  2, 3 TO 5, WCOLS()-4
@  2, 6 SAY " ��┘�� "
@  6,c1 SAY "�祠��" 

@  3, 5 GET new_cli PICTURE "@*C ���覃" VALID Ch_Cust()
@  4, 5 SAY "���" GET cli_nm WHEN new_cli
@  6,c1+ 6 GET pay_sum PICTURE "@Z 9999999.99"
@  6,c1+22 GET p_doc PICTURE "@*C �ョ�碎 ぎ�祠キ��"
@  8,c2 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "
READ CYCLE VALID Tst_Blank()

RELEASE WINDOW Inp_Pay
CLOSE DATABASES

*DO Exit_P WITH ext_menu, ""

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                蹍������ �襦��� ��痰�鎰���� ��┘���.                  �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Ch_Cust

IF new_cli
  RETURN .T.
ENDIF

IF Custs("", .F., .F., 2)
  cli_nm = ACCOUNT.CUS_NAME
  SHOW GET cli_nm
ELSE
  new_cli = .T.
  SHOW GET new_cli
ENDIF
RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                蹍������ �諷��� ├ 蹣������ 筮爼�.                    �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Tst_Blank

PRIVATE mss

IF ex = 2
  RETURN .T.
ENDIF

IF new_cli .AND. EMPTY(cli_nm)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��┘�� � �竅硅� ━キガ? �� ��Ε� °碎!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF pay_sum <= 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�皰膈�皀, 瓷��讓� Ε �� ����皋�."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
PRIVATE sel_sv

sel_sv = SELECT()
IF new_cli
  SELECT 0
  USE PARMS
  cli_cd = LAST_CUS+1
  REPLACE LAST_CUS WITH cli_cd
  USE
  SELECT ACCOUNT
  APPEND BLANK
  REPLACE CUS_CODE WITH cli_cd, ;
          CUS_NAME WITH cli_nm, ;
          ACC_TYPE WITH 2,      ;
          DATE_ON  WITH DATE(), ;
          WHO      WITH user,   ;
          WHERE    WITH arm
ENDIF  
SELECT ACCOUNT

REPLACE ACC_SUM  WITH ACC_SUM+pay_sum, ;
        ACC_SUMD WITH ACC_SUMD+ROUND(pay_sum/kurs,4)
cli_cd  = CUS_CODE
cli_nam = CUS_NAME

SELECT 0
USE ARM_LST
GO arm_rec
ln = LAST_IND+1
REPLACE LAST_IND WITH ln, KASSA WITH KASSA+pay_sum
USE INCMONEY
APPEND BLANK
REPLACE DOC_TYPE  WITH "2",         ;  
        DOC_NUM   WITH sys_char+CHRTRAN(arm," ","0")+STR(ln,6), ;
        DOC_DATE  WITH pay_dat,   ;
        ENTER_DAT WITH pay_dat,   ;
        CUS_CODE  WITH ACCOUNT.CUS_CODE,    ;
        MONEY_R   WITH pay_sum,   ;
        MONEY_D   WITH ROUND(pay_sum/kurs,4), ;
        WHO       WITH user,   ;
        WHERE     WITH arm

ln = DOC_NUM
USE

IF p_doc
  DO Prn_Pay1 WITH ln, pay_dat
ENDIF
SELECT (sel_sv)

RETURN .T.
