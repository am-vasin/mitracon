*������������������������������������������������������������������������ͻ
*� ���� F_Ord        ���ࠡ��稪 ���३ ��ᨭ           16.04.98 15:51:22 �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                           ��� �� 䨫����.                           �
*�                                                                        �
*������������������������������������������������������������������������ͼ
PROCEDURE F_Ord
PARAMETER a_code
PRIVATE pth_w, mss, s_sav, c_name, code_w, w_num, p_usd, s000
PRIVATE f_c000, f_n000, f_c_old
PRIVATE shab1, shab2, q_w
shab1 = "abcdefghijklmnopqrstuvwxyz������񦧨�����������������������"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZ���������������������������������"
f_c000 = def_firm
f_n000 = SPACE(16)
f_c_old = def_firm
w_num = ""
c_name = ""
pth_w = ""
DO Get_Path WITH pth_w, "INC_ORD"
DO WHILE .T.
  pth_w = Int_Dir(pth_w, "��� �饬 �����?", 0)
  IF EMPTY(pth_w)
    RETURN
  ENDIF
  IF .NOT. File_O(pth_w+"ORD_TIT.DBF") .OR. .NOT. File_O(pth_w+"ORDER.DBF")
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"��� ⠬ ������!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    LOOP
  ENDIF
  EXIT
ENDDO

s_sav = SELECT()
SELECT 0
USE (pth_w+"ORD_TIT") ALIAS ORDT98
SELECT 0
USE (pth_w+"ORDER") ALIAS ORD98

IF .NOT. Tst_Ord()
  SELECT ORDT98
  USE
  SELECT ORD98
  USE
  SELECT (s_sav)
  RETURN
ENDIF
p_usd = What_V()
IF p_usd = 3
  RETURN
ENDIF
p_usd = p_usd = 2
*
*   ������� ��������� ᯨ᪠
*
SELECT 0
USE (base_path+"LIST_TIT") ORDER TAG LST_NUM ALIAS W_LIST_0 AGAIN
code_w = Doc_Num("LISTS", "", a_code, DATE())
SELECT W_LIST_0
APPEND BLANK
REPLACE LST_ARM   WITH a_code, ;
        LST_NUM   WITH code_w, ;
        CR_DAT    WITH DATE(), ;
        LST_NAME  WITH c_name, ;
        CLI_CODE  WITH ORDT98.CUS_CODE, ;
        FIRM      WITH f_c000, ;
        BANK      WITH def_bank, ;
        REQ_DOC   WITH w_num,  ;
        REQ_DATE  WITH ORDT98.ORD_DATE,  ;
        ORD_QUEUE WITH queuetype, ;
        IS_USD    WITH p_usd,  ;
        WHO_CR    WITH user,   ;
        WHERE_CR  WITH a_code
GO RECNO() IN ACCOUNT
SELECT 0
USE (base_path+"LIST_DET") ALIAS LD9804 AGAIN
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS ST9804 AGAIN
SET FILTER TO PRICE_R # 0 .AND. EMPTY(STOP_SALE)
SELECT ORD98
s000 = 0
SCAN
  IF .NOT. SEEK(CODE, "ST9804")
    LOOP
  ENDIF
  SELECT LD9804
  APPEND BLANK
  q_w = ORD98.QNT
*  IF q_w % MAX(ST9804.QNT_ROUND,1) # 0 .AND. q_w > 0
    q_w = Round_P(ST9804.PREFIX+" "+ST9804.NAME+" "+ST9804.PRODUCER, q_w, ST9804.QNT_ROUND, ST9804.QNT_PACK, ST9804.PRICE)
*  ENDIF
  REPLACE LIST_NUM  WITH STR(code_w, 6),  ;
          CODE      WITH ORD98.CODE,      ;
          PREFIX    WITH ST9804.PREFIX,   ;
          NAME      WITH ST9804.NAME,     ;
          PRODUCER  WITH ST9804.PRODUCER, ;
          ADDRESS   WITH ST9804.ADDRESS,  ;
          DEF_PRICE WITH ST9804.PRICE_R,  ;
          USD_DEF   WITH ST9804.PRICE,    ;
          SAL_PRICE WITH IIF(.NOT. p_usd, ST9804.PRICE_R, 0),  ;
          USD_PRICE WITH IIF(p_usd, ST9804.PRICE, 0),          ;
          QNT       WITH q_w
  s000 = s000+QNT*IIF(p_usd, USD_PRICE, SAL_PRICE)
  SELECT ORD98
  IF .NOT. EMPTY(ST9804.NOTE)
    DO View_Nt WITH "ST9804"
  ENDIF
ENDSCAN

USE

SELECT W_LST_0
REPLACE R_TOTAL WITH s000
USE

SELECT ORDT98
USE

SELECT LD9804
USE

SELECT ST9804
USE

SELECT (s_sav)
RETURN

*������������������������������������������������������������������������ͻ
*�                                                                        �
*�                    �஢�ઠ ��� �� ���४⭮���.                    �
*�                                                                        �
*����������������������������������������������������� 16.04.98 16:14:02 ͼ
PROCEDURE Tst_Ord
PRIVATE mss, s_sav, c

s_sav = SELECT()

IF ORDT98.SUP_CODE # sup_own
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��� �� ��� ���!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS ACC98 AGAIN
IF .NOT. SEEK(ORDT98.CUS_CODE) .OR. ORDT98.CUS_CODE = sup_own
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�⮣� �����稪� �� �� �����!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  RETURN .F.
ENDIF
c_name = ALLTRIM(ACC98.CUS_NAME)

USE (base_path+"RYN_M_T") ALIAS RYN98 AGAIN
w_num = ORDT98.ORD_NUM

IF TYPE("w_num") = "C"
  w_num = ALLTRIM(w_num)
ELSE
  w_num = ALLTRIM(STR(w_num))
ENDIF

LOCATE FOR CUS_CODE = ORDT98.CUS_CODE .AND. ;
           ALLTRIM(ORD_NUM) == w_num  .AND. ;
           ORD_DATE = ORDT98.ORD_DATE
IF FOUND()
  DIMENSION mss(5)
  mss(1) = ""
  mss(2) = CHR(0)+"�����稪 "+c_name+"."
  mss(3) = CHR(0)+"��� N "+w_num+" �� "+DTOC(ORDT98.ORD_DATE)+" 㦥 �뫠!"
  mss(4) = CHR(0)+"  [ ��� �� ����� �⠥�! ] [ �⪠������ ]"
  mss(5) = ""
  c = 7
ELSE
  DIMENSION mss(5)
  mss(1) = ""
  mss(2) = CHR(0)+"�����稪 "+c_name+"."
  mss(3) = CHR(0)+"��� N "+w_num+" �� "+DTOC(ORDT98.ORD_DATE)+"."
  mss(4) = CHR(0)+"  [ ����⠥�! ] [ �⪠������ ]"
  mss(5) = ""
  c = 5
ENDIF
USE

RETURN Out_Mess(c, "mss") = 1

*������������������������������������������������������������������������Ŀ
*�   ��楤�� What_V       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                             �롮� ������.                              �
*�                                                                        �
*����������������������������������������������������������� 12.04.1999 ���
PROCEDURE What_V

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

*����������������������������������������Ŀ
*�                                        �
*� �।���⨥ 999 9999999999999999 (F1)  �
*�                                        �
*�  < �㡫� > < USD > < �⪠������ Esc >  �
*�                                        �
*������������������������������������������

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 8, 44, "������ ������"

*------------------------------------------------------------------------
*      ���� ����� ������
*

DO Get_Firm WITH 3, 3, enab_firm
@ 5, 4 GET ex PICTURE "@*HT \ �㡫� ;\ USD ;\? �⪠������ Esc "

READ CYCLE VALID T_Firm()

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex

*������������������������������������������������������������������������Ŀ
*�   ��楤�� T_Firm       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                     �஢�ઠ ������ �।�����.                      �
*�                                                                        �
*����������������������������������������������������������� 04/13/1999 ���
PROCEDURE T_Firm

IF ex = 3
  RETURN .T.
ENDIF
IF EMPTY(f_c000)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� �� 㪠���� �।���⨥!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.