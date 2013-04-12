*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Delivery     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                        �ॡ������ �� ���⠢��.                         �
*�                                                                        �
*����������������������������������������������������������� 26.03.2000 �ͼ
PROCEDURE Delivery
PARAMETERS d_num, d_date, modi

PRIVATE doc_num, doc_date, lst_num, dlv_date, cus_code, stat_code, stat_name
PRIVATE train_no, train_nam, arrive, leave, carriage, carr_name, conductor
PRIVATE cond_code, cond_text, to_get, cource, text01, text02, text03
PRIVATE exa_wght, waight, call_time, to_pay, pay_sum, to_mess1, to_mess2, is_deliv
PRIVATE salesman, enter_dat, enter_tim, who_mod, mod_date, mod_time
PRIVATE who_read, read_date, read_time, who_call, call_date, call_time
PRIVATE who_gave, gave_date, gave_time, who_term, term_date, term_time, new_list
PRIVATE tmpWght

PRIVATE view_only, sw1, sw2, stat_sw
PRIVATE s_sav, mss, d_year, ex_pic, to_print
PRIVATE tmpFrm
tmpFrm = ""
STORE .F. TO sw1, sw2
to_print = .F.
stat_sw = .F.
view_only = .T.

s_sav = SELECT()
SELECT 0
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS FRM_0326 AGAIN

SELECT 0
USE (base_path+"LIST_TIT") ORDER TAG LST_NUM ALIAS LST_0326 AGAIN
SET RELATION TO FIRM INTO FRM_0326

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS CUS_0326 AGAIN

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS PERS_0326 AGAIN

SELECT 0
USE (base_path+"DELIVERY") ORDER TAG DOC_NUM ALIAS DLV_0326 AGAIN
IF TYPE("d_num") = "N"      && ����� ᯨ᪠ - �ਧ��� ����� �����
  view_only = .F.
  SCATTER MEMVAR BLANK
  IF .NOT. SEEK(d_num, "LST_0326")
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"��� ᯨ᪠ � ����஬ "+ALLTRIM(STR(d_num))+"!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    SELECT DLV_0326
    USE
    SELECT LST_0326
    USE
    SELECT CUS_0326
    USE
    SELECT PERS_0326
    USE
    SELECT (s_sav)
    RETURN
  ENDIF
  IF .NOT. SEEK(LST_0326.CLI_CODE, "CUS_0326")
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"�� 㤠���� ���� ������..."
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    SELECT DLV_0326
    USE
    SELECT LST_0326
    USE
    SELECT CUS_0326
    USE
    SELECT PERS_0326
    USE
    SELECT (s_sav)
    RETURN
  ENDIF
  tmpWght = Weight(d_num)
  tmpFrm = FRM_0326.L_NAME
  tit = "�������� �ॡ������ �� ���⠢�� � ᯨ�� N "+ALLTRIM(STR(d_num))
  m.dlv_date = {}
ELSE
  IF TYPE("d_date") = "D"
    d_year = LEFT(DTOS(d_date),4)
  ENDIF
  IF TYPE("d_date") = "N"
    d_year = STR(d_date,4)
  ENDIF
  IF TYPE("d_date") = "C"
    d_year = d_date
  ENDIF
  IF .NOT. SEEK(d_year+d_num, "DLV_0326")
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"�ॡ������ �� �������!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    SELECT DLV_0326
    USE
    SELECT LST_0326
    USE
    SELECT CUS_0326
    USE
    SELECT PERS_0326
    USE
    SELECT (s_sav)
    RETURN
  ENDIF
  IF .NOT. SEEK(DLV_0326.CUS_CODE, "CUS_0326")
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"�� 㤠���� ���� ������..."
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    SELECT DLV_0326
    USE
    SELECT LST_0326
    USE
    SELECT CUS_0326
    USE
    SELECT PERS_0326
    USE
    SELECT (s_sav)
    RETURN
  ENDIF
  tmpWght = Weight(DLV_0326.LST_NUM)
  IF .NOT. SEEK(DLV_0326.LST_NUM, "LST_0326")
    SELECT LST_0326
    USE (base_path+"SALE_TIT") ORDER TAG LIST_NUM ALIAS LST_0326 AGAIN
    SET RELATION TO FIRM INTO FRM_0326
    IF SEEK(STR(DLV_0326.LST_NUM, 6), "LST_0326")
      tmpFrm = FRM_0326.L_NAME
    ENDIF
    USE (base_path+"LIST_TIT") ORDER TAG LST_NUM ALIAS LST_0326 AGAIN
    SET RELATION TO FIRM INTO FRM_0326
    SELECT DLV_0326
  ELSE
    tmpFrm = FRM_0326.L_NAME
  ENDIF
  SCATTER MEMVAR
  view_only = .NOT. modi .OR. .NOT. EMPTY(TERM_DATE)
  tit = "�ॡ������ �� ���⠢�� N "+ALLTRIM(DLV_0326.DOC_NUM)+  ;
                               " �� "+DTOC(DLV_0326.DOC_DATE)
  sw1 = .NOT. EMPTY(m.stat_code)
  sw2 = .NOT. EMPTY(m.cond_code)
ENDIF

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

*������������������������������������������������������������������������Ŀ
*�                    ��� ��.��.���� (F1) �������쭨�                    �
*�   ���⠢騪 ....:....!....:....!....:....!....:....!....:....!         �
*�      ������ ....:....!....:....!....:....!....:....!....:....!         �
*�[ ] ������ ....:....!....:....!....  ����� ....:... ....:....!....:....!�
*�            �६� �ਡ��� ....:     �६� ��ࠢ����� ....:            �
*�   ����� 99   ����. N ....:...   ��� �஢������ ....:....!....:.        �
*� [ ] ��।��� �஢. ....:....!....:.  ������� ....:....!  ���� ....:.. �
*�    �� �� ����, ��� ���� �����������������������������������������Ŀ    �
*�    � ....:....!....:....!....:....!....:....!....:....!....:....! �    �
*�    � ....:....!....:....!....:....!....:....!....:....!....:....! �    �
*�    � ....:....!....:....!....:....!....:....!....:....!....:....! �    �
*�    ����������������������������������������������������������������    �
*�        �।�������. ��� (��) ....:      ����� �맢��� � ....:        �
*�         ���. 㪠�����: [ ] ������ �஢������ � �।���� ....:         �
*�    �� ���� ᮮ���� �� ��ࠢ�� (���, ⥫) ����������������������Ŀ    �
*�    � ....:....!....:....!....:....!....:....!....:....!....:....! �    �
*�    � ....:....!....:....!....:....!....:....!....:....!....:....! �    �
*�    ����������������������������������������������������������������    �
*�    [ ] ���⠢�� �������   [ ] ����ﭨ� (F7)   [ ] ����� ���㬥��    �
*�                    < OK Ctrl-W > < �⪠������ Esc >                    �
*��������������������������������������������������������������������������
*�                            < OK Ctrl-W >                               �

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 23, 76, tit
@ 2, 38 SAY "(F1)"
DO Sun_Bord WITH  9,  6, 13, 69, " �� ����, ��� ���� "
DO Sun_Bord WITH 16,  6, 19, 69, " ���� ᮮ���� �� ��ࠢ�� (���, ⥫.) "
ex_pic = IIF(view_only, "@*HT \! OK Ctrl-W ", "@*HT \! OK Ctrl-W ;\? �⪠������ Esc ")
*------------------------------------------------------------------------
*      ���� ����� ������
*
IF TYPE("d_num") # "N"
  ON KEY LABEL F7 DO Cur_Status
ENDIF
tmpWght = ROUND(tmpWght/1000, 0)
DO Say_DOW
@  2, 22 SAY "���" GET m.dlv_date WHEN Set_F1() VALID Say_DOW()
@  3,  5 SAY "���⠢騪 "+tmpFrm
@  4,  8 SAY "������ "+CUS_0326.CUS_NAME
@  5,  2 GET sw1 PICTURE "@*C ������" VALID Get_01()
@  5, 13 GET m.stat_name WHEN .NOT. sw1
@  5, 39 SAY "�����" GET m.train_no
@  5, 54 GET m.train_nam
@  6, 14 SAY "�६� �ਡ���" GET m.arrive
@  6, 39 SAY "�६� ��ࠢ�����" GET m.leave
@  7,  5 SAY "�����" GET m.carriage
@  7, 16 SAY "����. N" GET m.carr_name
@  7, 35 SAY "��� �஢������" GET m.conductor
@  8,  3 GET sw2 PICTURE "@*C ��।��� �஢." VALID Get_02()
@  8, 22 GET m.cond_text WHEN .NOT. sw2
@  8, 40 SAY "�������" GET m.to_get
@  8, 61 SAY "����" GET m.cource
@ 10,  8 GET m.text01
@ 11,  8 GET m.text02
@ 12,  8 GET m.text03
@ 14, 10 SAY "�।�������. ��� (��)" GET tmpWght PICTURE "99999" WHEN .F.
@ 14, 43 SAY "����� �맢��� �" GET m.call_time
@ 15, 11 SAY "���. 㪠�����:"
@ 15, 26 GET m.to_pay PICTURE "@*C ������ �஢������" VALID Clr_S()
@ 15, 49 SAY "� �।����" GET m.pay_sum WHEN m.to_pay
@ 17,  8 GET m.to_mess1
@ 18,  8 GET m.to_mess2
@ 20,  6 GET m.is_deliv PICTURE "@*C ���⠢�� �������"
IF view_only
  CLEAR GETS
ENDIF
@ 20, 29 GET m.stat_sw PICTURE "@*C ����ﭨ� (F7)" ;
            WHEN TYPE("d_num") # "N" VALID Cur_Status()
@ 20, 50 GET m.to_print PICTURE "@*C ����� ���㬥��" VALID P_Doc()
@ WROWS()-2, FLOOR(WCOLS()/2)-IIF(view_only, 6, 16) GET ex PICTURE ex_pic

READ CYCLE VALID Tst_Blank()

IF ex = 1 .AND. .NOT. view_only
  *
  * ��ࠡ��뢠�� �����
  IF TYPE("d_num") = "N"      && ����� ᯨ᪠ - �ਧ��� ����� �����
    ex = Doc_Num("DELIVERY", LST_0326.FIRM, arm, DATE())
    m.doc_num  = PADR(sys_char+ALLTRIM(STR(ex)), LEN(m.doc_num))
    m.doc_date = DATE()
    m.lst_num  = LST_0326.LST_NUM
    m.cus_code = LST_0326.CLI_CODE
    m.salesman = user
    m.enter_dat= DATE()
    m.enter_tim= TIME()
    m.where    = arm
    APPEND BLANK
  ELSE
    m.who_mod = user
    mod_date  = DATE()
    mod_time  = TIME()
    m.where   = arm
  ENDIF
  GATHER MEMVAR
  IF to_print
    DO P_Dlv WITH m.doc_num, m.doc_date
  ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
SELECT DLV_0326
USE
SELECT LST_0326
USE
SELECT CUS_0326
USE
SELECT FRM_0326
USE
SELECT PERS_0326
USE
SELECT (s_sav)
RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Say_DOW      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                           ����� ��� ������.                           �
*�                                                                        �
*����������������������������������������������������������� 26.03.2000 ���
PROCEDURE Say_DOW
PARAMETER no_clr

PRIVATE n

IF EMPTY(m.dlv_date)
  @ 2, 43 SAY "           "
ELSE
  n = DOW(m.dlv_date)
  DO CASE
  CASE n = 1
    @ 2, 43 SAY "����ᥭ�"
  CASE n = 2
    @ 2, 43 SAY "�������쭨�"
  CASE n = 3
    @ 2, 43 SAY "��୨�    "
  CASE n = 4
    @ 2, 43 SAY "�।�      "
  CASE n = 5
    @ 2, 43 SAY "��⢥�    "
  CASE n = 6
    @ 2, 43 SAY "��⭨�    "
  CASE n = 7
    @ 2, 43 SAY "�㡡��    "
  ENDCASE
ENDIF

IF .NOT. no_clr
  ON KEY LABEL F1
ENDIF

RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Get_01       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                             �롮� �������.                             �
*�                                                                        �
*����������������������������������������������������������� 26.03.2000 ���
PROCEDURE Get_01

IF sw1
  DO Stations WITH m.stat_code, m.stat_name
ELSE
  m.stat_code = SPACE(LEN(m.stat_code))
  m.stat_name = SPACE(LEN(m.stat_name))
ENDIF
sw1 = .NOT. EMPTY(m.stat_code)
SHOW GET sw1
SHOW GET m.stat_code
SHOW GET m.stat_name

RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Get_02       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                             �롮� ???????.                             �
*�                                                                        �
*����������������������������������������������������������� 26.03.2000 ���
PROCEDURE Get_02

IF sw2
  DO For_Cond WITH m.cond_code, m.cond_text
ELSE
  m.cond_code = SPACE(LEN(m.cond_code))
  m.cond_name = SPACE(LEN(m.cond_text))
ENDIF
sw2 = .NOT. EMPTY(m.cond_text)
SHOW GET sw2
SHOW GET m.cond_code
SHOW GET m.cond_text

RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Set_F1       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                       ��⠭���� ��ࠡ��稪� F1.                        �
*�                                                                        �
*����������������������������������������������������������� 26.03.2000 ���
PROCEDURE Set_F1

ON KEY LABEL F1 DO Int_Date

RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Int_Date     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                      ��⠭���� ���� �� ���������.                      �
*�                                                                        �
*����������������������������������������������������������� 26.03.2000 ���
PROCEDURE Int_Date

m.dlv_date = Get_Date(m.dlv_date)
SHOW GET m.dlv_date
= Say_DOW(.T.)

RETURN
*������������������������������������������������������������������������Ŀ
*�   ��楤�� Clr_S        ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                             ���⪠ �㬬�.                             �
*�                                                                        �
*����������������������������������������������������������� 27.03.2000 ���
PROCEDURE Clr_S

IF .NOT. m.to_pay
  m.pay_sum = SPACE(LEN(m.pay_sum))
  SHOW GET m.pay_sum
ENDIF

RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Cur_Status   ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                    C����� �ॡ������ �� ���⠢��.                     �
*�                                                                        �
*����������������������������������������������������������� 28.03.2000 ���
PROCEDURE Cur_Status

DO Dlv_Stat WITH m.doc_num, m.doc_date
m.stat_sw = .F.
SHOW GET m.stat_sw

RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� P_Doc        ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                           ����� ���㬥��.                            �
*�                                                                        �
*����������������������������������������������������������� 30.03.2000 ���
PROCEDURE P_Doc

IF EMPTY(m.doc_num)
  RETURN .T.
ENDIF

IF m.to_print
  DO P_Dlv WITH m.doc_num, m.doc_date
  m.p_doc = .F.
  SHOW GET m.p_doc
ENDIF

RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Tst_Blank    ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                �஢�ઠ ���४⭮�� ���������� ������.                �
*�                                                                        �
*����������������������������������������������������������� 22.08.2000 ���
PROCEDURE Tst_Blank

PRIVATE mss

IF ex = 2
  RETURN .T.
ENDIF

IF EMPTY(m.dlv_date)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� �� 㪠���� ����..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.