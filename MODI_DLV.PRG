*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Modi_Dlv     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                     ��｀�� � ��錢���� �� ぎ痰�→�.                     �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 29.03.2000 様�
PROCEDURE Modi_Dlv
PARAMETER W_Only

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
PRIVATE s_sav         && ���ム ��｀腑� �゛�痰� か� 甌縲��キ�� � 〓瘁����←キ��!

*
*   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
*
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*

s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS PSS_0329 AGAIN
SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS PLS_0329 AGAIN
SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS PSN_0329 AGAIN
SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS PLN_0329 AGAIN
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS CUS_0329 AGAIN
SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG LIST_NUM ALIAS SALS_0329 AGAIN
SET FILTER TO LEFT(DOC_NUM,1) = sys_char
SET RELATION TO WHO INTO PSS_0329
SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG LIST_NUM ALIAS SALN_0329 AGAIN
SET FILTER TO LEFT(DOC_NUM,1) = sys_char
SET RELATION TO WHO INTO PSN_0329
SELECT 0
USE (base_path+"LIST_TIT") ORDER TAG LST_NUM ALIAS LSTS_0329 AGAIN
SET RELATION TO WHO_CR INTO PLS_0329
SELECT 0
USE (base_path+"LIST_TIT") ORDER TAG LST_NUM ALIAS LSTN_0329 AGAIN
SET RELATION TO WHO_CR INTO PLN_0329
SELECT 0
USE (base_path+"DELIVERY") ORDER TAG HRON1 ALIAS DLV_0329 AGAIN
SET RELATION TO STR(LST_NUM,6)  INTO SALS_0329, ;
                LST_NUM         INTO LSTS_0329, ;
                STR(NEW_LIST,6) INTO SALN_0329, ;
                NEW_LIST        INTO LSTN_0329, ;
                CUS_CODE        INTO CUS_0329

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Work OF (menu_name) PROMPT "��｀��"
ON PAD Work OF (menu_name) ACTIVATE POPUP Work

  DEFINE POPUP Work MARGIN RELATIVE SHADOW COLOR SCHEME 4

  IF .NOT. W_Only
    DEFINE BAR  1 OF Work PROMPT "蹍甃�矗, �ョ�碎, ��玻オ罔�..." KEY Ctrl-A, "Enter"
    ON SELECTION BAR 1 OF Work DO Sw_Mode WITH "View"
  ENDIF

  DEFINE BAR  2 OF Work PROMPT "��痰�鎰┘" KEY Ctrl-A, "F1"
  ON SELECTION BAR 2 OF Work DO Sw_Mode WITH "Status"

  IF .NOT. W_Only
    DEFINE BAR  3 OF Work PROMPT "���キ���矣�� ぎ痰�→�" KEY Ctrl-A, "F9"
    ON SELECTION BAR 3 OF Work DO Sw_Mode WITH "Dlv_Sto"

    DEFINE BAR  4 OF Work PROMPT "�痰���→� 甌痰�鎰��" KEY Ctrl-A, "Tab"
    ON SELECTION BAR 4 OF Work DO Sw_Mode WITH "Set_Stat"
  ENDIF

  DEFINE BAR  5 OF Work PROMPT "��膈覃 ▲� ��瘠���" KEY Ctrl-A, "F2"
  ON SELECTION BAR 5 OF Work DO Sw_Mode WITH "Waight"

  IF .NOT. W_Only
    DEFINE BAR  6 OF Work PROMPT "���. ��痳�る �� ぎ痰�→�" KEY Ctrl-A, "F3"
    ON SELECTION BAR 6 OF Work DO Sw_Mode WITH "Invoice"
  ENDIF

DEFINE PAD All OF (menu_name) PROMPT "�瓮 ��錢��" KEY Ctrl-A, "F7"
ON SELECTION PAD All OF (menu_name) DO Sw_Mode WITH "All"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 72

DO D_Wins WITH ln, wd, "��錢�� �� ぎ痰�→�", 0, 5
what_do = "List"
*敖N ��錢��陳陳����陳陳���┘�眥陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳��痰.�
*�....:....! ��.��.���� ....:....!....:....!....:....!....:....!.... ....:�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� ���甌� 1 999999 ��Г�� ....:....!....:....!....:... ��.��.����         �
*� ���. 1 99999999999/CCC 蹍��� ....:....!....:....!....:... ��.��.����  �
*� ���甌� 2 999999 ��Г�� ....:....!....:....!....:... ��.��.����         �
*� ���. 2 99999999999/CCC 蹍��� ....:....!....:....!....:... ��.��.����  �
*�                             �メ 99999 [��膈�]                          �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
@ 1, 2 SAY "�N ��錢��陳陳����陳陳���┘�眥陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳��痰."
@ WROWS()-6, 3 SAY "���甌� 1        ��Г��"
@ WROWS()-5, 3 SAY "���. 1                 蹍���"
@ WROWS()-4, 3 SAY "���甌� 2        ��Г��"
@ WROWS()-3, 3 SAY "���. 2                 蹍���"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    IF .NOT. W_Only
      ON KEY LABEL Enter KEYBOARD CHR(23)
      ON KEY LABEL F9    DO Sw_Mode WITH "Dlv_Sto"
      ON KEY LABEL Tab   DO Sw_Mode WITH "Set_Stat"
      ON KEY LABEL F3    DO Sw_Mode WITH "Invoice"
    ENDIF
    ON KEY LABEL F2    DO Sw_Mode WITH "Waight"
    ON KEY LABEL F7    DO Sw_Mode WITH "All"
    ON KEY LABEL F1    DO Sw_Mode WITH "Status"
*
*    BROWSE - �キ�
*
    BROWSE FIELDS DOC_NUM:10:H="",     ;
                  DOC_DATE:H="",       ;
                  CUS_0329.CUS_NAME:44:H="", ;
                  STAT = Stat_Str():5:H="",  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "View")
    ENDIF

  CASE what_do = "View"       && 蹍甃�矗 甎�瓷�

    IF .NOT. BOF() .AND. .NOT. EOF()
      IF EMPTY(READ_DATE)
        REPLACE READ_DATE WITH DATE(),  ;
                READ_TIME WITH TIME(),  ;
                WHO_READ  WITH user
      ENDIF
      DO Delivery WITH DOC_NUM, DOC_DATE, .T.
    ENDIF
    what_do = "List"

  CASE what_do = "All"        && �┼赳�

    IF "1" $ TAG()
      DEFINE PAD All OF (menu_name) PROMPT "�ェ��琺硅� ��錢��" KEY Ctrl-A, "F7" AFTER Work
      SET ORDER TO TAG HRON
    ELSE
      DEFINE PAD All OF (menu_name) PROMPT "�瓮 ��錢��" KEY Ctrl-A, "F7" AFTER Work
      SET ORDER TO TAG HRON1
    ENDIF
    what_do = "List"

  CASE what_do = "Status"       && ��痰�鎰┘

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO Dlv_Stat WITH DOC_NUM, DOC_DATE
    ENDIF
    what_do = "List"

  CASE what_do = "Invoice"       && ���. ��痳�る

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO Prep_Inv WITH NEW_LIST, CUS_0329.CUS_CODE, CUS_0329.CUS_NAME, WHERE
    ENDIF
    what_do = "List"

  CASE what_do = "Waight"       && ��膈覃 �メ

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO E_Waight
    ENDIF
    what_do = "List"

  CASE what_do = "Set_Stat"      && �痰���→� 甌痰�鎰��

    DO Set_Stat WITH DOC_NUM, DOC_DATE
    what_do = "List"

  CASE what_do = "Dlv_Sto"       && ���キ���矣�� ぎ痰�→�

    DO Dlv_Sto
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
*   ���窶�皚キ讓� (���窶�皚キ讓�!) ���琺��ガ DBF-�����.
*
SELECT PSS_0329
USE
SELECT PLS_0329
USE
SELECT PSN_0329
USE
SELECT PLN_0329
USE
SELECT CUS_0329
USE
SELECT SALS_0329
USE
SELECT SALN_0329
USE
SELECT LSTS_0329
USE
SELECT LSTN_0329
USE
SELECT DLV_0329
USE
SELECT (s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Draw_Foot    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                   ��痺ガ ぎ�����皀�讚竡 ┃筮爼�罔�.                    �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 20.10.1998 陳�
PROCEDURE Draw_Foot

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� Гメ�, 爬痺ガ...
*
@ WROWS()-6, 26 SAY SPACE(28)
IF FOUND("LSTS_0329")
  @ WROWS()-6, 12 SAY STR(LSTS_0329.LST_NUM,6)
  @ WROWS()-6, 55 SAY DTOC(LSTS_0329.CR_DAT)
  IF FOUND("PLS_0329")
    @ WROWS()-6, 26 SAY PADR(ALLTRIM(PLS_0329.FAMILY)+" "+ ;
                             LEFT(PLS_0329.NAME,1)+" "+ ;
                             LEFT(PLS_0329.S_NAME,1), 28)
  ENDIF
ELSE
  @ WROWS()-6, 12 SAY "����キ"
  @ WROWS()-6, 55 SAY SPACE(10)
ENDIF

@ WROWS()-5, 33 SAY SPACE(28)
IF FOUND("SALS_0329")
  @ WROWS()-5, 10 SAY PADR(LEFT(SALS_0329.DOC_NUM,4)+"-"+ALLTRIM(SUBSTR(SALS_0329.DOC_NUM,5))+ ;
                           "/"+SALS_0329.FIRM,15)
  @ WROWS()-5, 62 SAY DTOC(SALS_0329.DOC_DATE)
  IF FOUND("PSS_0329")
    @ WROWS()-5, 33 SAY PADR(ALLTRIM(PSS_0329.FAMILY)+" "+ ;
                             LEFT(PSS_0329.NAME,1)+" "+ ;
                             LEFT(PSS_0329.S_NAME,1), 28)
  ENDIF
ELSE
  @ WROWS()-5, 10 SAY SPACE(15)
  @ WROWS()-5, 62 SAY SPACE(10)
ENDIF

@ WROWS()-4, 26 SAY SPACE(28)
IF FOUND("LSTN_0329")
  @ WROWS()-4, 12 SAY STR(LSTN_0329.LST_NUM,6)
  @ WROWS()-4, 55 SAY DTOC(LSTN_0329.CR_DAT)
  IF FOUND("PLN_0329")
    @ WROWS()-4, 26 SAY PADR(ALLTRIM(PLN_0329.FAMILY)+" "+ ;
                             LEFT(PLN_0329.NAME,1)+" "+ ;
                             LEFT(PLN_0329.S_NAME,1), 28)
  ENDIF
ELSE
  @ WROWS()-4, 12 SAY SPACE(6)
  @ WROWS()-4, 55 SAY SPACE(10)
ENDIF

@ WROWS()-3, 33 SAY SPACE(28)
IF FOUND("SALN_0329")
  @ WROWS()-3, 10 SAY PADR(LEFT(SALN_0329.DOC_NUM,4)+"-"+ALLTRIM(SUBSTR(SALN_0329.DOC_NUM,5))+ ;
                           "/"+SALN_0329.FIRM,15)
  @ WROWS()-3, 62 SAY DTOC(SALN_0329.DOC_DATE)
  IF FOUND("PSN_0329")
    @ WROWS()-3, 33 SAY PADR(ALLTRIM(PLN_0329.FAMILY)+" "+ ;
                             LEFT(PLN_0329.NAME,1)+" "+ ;
                             LEFT(PLN_0329.S_NAME,1), 28)
  ENDIF
ELSE
  @ WROWS()-3, 10 SAY SPACE(15)
  @ WROWS()-3, 62 SAY SPACE(10)
ENDIF
@ WROWS()-2, 31 SAY "�メ "+WAIGHT+IIF(EXA_WGHT, " (皰膈�)", "        ")

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Stat_Str     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                        �矗��� 甌痰�鎰�� ��錢��.                        �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 29.03.2000 陳�
PROCEDURE Stat_Str
PRIVATE s

s = IIF(EMPTY(MOD_DATE),  " ", "�")+ ;
    IIF(EMPTY(READ_DATE), " ", "�")+ ;
    IIF(EMPTY(CALL_DATE), " ", "�")+ ;
    IIF(EMPTY(GAVE_DATE), " ", "�")+ ;
    IIF(EMPTY(TERM_DATE), " ", "�")
RETURN s

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Set_Stat     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                          �痰���→� 甌痰�鎰��.                          �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 29.03.2000 陳�
PROCEDURE Set_Stat
PARAMETERS d_num, d_date

PRIVATE d_year, s_sav, self

IF TYPE("d_date") = "D"
  d_year = LEFT(DTOS(d_date),4)
ENDIF
IF TYPE("d_date") = "N"
  d_year = STR(d_date,4)
ENDIF
IF TYPE("d_date") = "C"
  d_year = d_date
ENDIF

s_sav = SELECT()
SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS WHO_0328 AGAIN
IF .NOT. SEEK(user, "WHO_0328")
  self = "?"
ELSE
  self = ALLTRIM(WHO_0328.FAMILY)+" "+LEFT(WHO_0328.NAME,1)+" "+LEFT(WHO_0328.S_NAME,1)
ENDIF
SELECT 0
USE (base_path+"DELIVERY") ORDER TAG DOC_NUM ALIAS DLV_0328 AGAIN

IF .NOT. SEEK(d_year+d_num)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���祠キ� �� ���ぅ�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT WHO_0328
  USE
  SELECT (s_sav)
  RETURN
ENDIF

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

*敖陳陳陳陳陳陳陳陳堕陳陳陳陳陳陳陳陳陳陳陳陳陳陳賃陳陳陳陳陳陳堕陳陳陳陳朕
*� �ュ�. 轤��      �  ��矗磴�┴                   �    ����    �  �爛��   �
*団陳陳陳陳陳陳陳陳田陳陳陳陳陳陳陳陳陳陳陳陳陳陳津陳陳陳陳陳陳田陳陳陳陳調
*� �〓� ��錢��     � ....:....!....:....!....:... � ��.��.���� � ��:��:�� �
*団陳陳陳陳陳陳陳陳田陳陳陳陳陳陳陳陳陳陳陳陳陳陳津陳陳陳陳陳陳田陳陳陳陳調
*� ��玻.��錢��     � ....:....!....:....!....:... � ��.��.���� � ��:��:�� �
*団陳陳陳陳陳陳陳陳田陳陳陳陳陳陳陳陳陳陳陳陳陳陳津陳陳陳陳陳陳田陳陳陳陳調
*� �ョ�碎 ��錢��   � ....:....!....:....!....:... � ��.��.���� � ��:��:�� �
*団陳陳陳陳陳陳陳陳田陳陳陳陳陳陳陳陳陳陳陳陳陳陳津陳陳陳陳陳陳田陳陳陳陳調
*� �襷�� �窶譽��   � ....:....!....:....!....:... � ��.��.���� � ��:��:�� �
*団陳陳陳陳陳陳陳陳田陳陳陳陳陳陳陳陳陳陳陳陳陳陳津陳陳陳陳陳陳田陳陳陳陳調
*� �皴��→� �窶譽��� ....:....!....:....!....:... � ��.��.���� � ��:��:�� �
*団陳陳陳陳陳陳陳陳田陳陳陳陳陳陳陳陳陳陳陳陳陳陳津陳陳陳陳陳陳田陳陳陳陳調
*� ���琺皋� ��錢�� � ....:....!....:....!....:... � ��.��.���� � ��:��:�� �
*青陳陳陳陳陳陳陳陳祖陳陳陳陳陳陳陳陳陳陳陳陳陳陳珍陳陳陳陳陳陳祖陳陳陳陳潰
*                                    < OK >                    

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 17, 77, "��痰�鎰┘ ��錢�� N "+ALLTRIM(DOC_NUM)+" "+DTOC(DOC_DATE)

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 1, 0 CLEAR TO WROWS()-1, WCOLS()-1
@ 0, 0 SAY ""
DIMENSION tmpVal(4, 4), sw0(4)
sw0(1) = 1
sw0(2) = 1
sw0(3) = 1
sw0(4) = 1
FOR ex = 1 TO 4
  tmpVal(ex, 1) = 0
  tmpVal(ex, 2) = SPACE(28)
  tmpVal(ex, 3) = {}
  tmpVal(ex, 4) = SPACE(8)
ENDFOR

TEXT
 敖陳陳陳陳陳陳陳陳賃陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳堕陳陳陳陳陳賃陳陳陳陳陳�
 � �ュ�. 轤��       �  ��矗磴�┴                   �    ����    �  �爛��   �
 団陳陳陳陳陳陳陳陳津陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳田陳陳陳陳陳津陳陳陳陳陳�
 � �〓� ��錢��      �                              �            �          �
 団陳陳陳陳陳陳陳陳津陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳田陳陳陳陳陳津陳陳陳陳陳�
 � ��玻.��錢��      �                              �            �          �
 団陳陳陳陳陳陳陳陳津陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳田陳陳陳陳陳津陳陳陳陳陳�
 � �ョ�碎 ��錢��    �                              �            �          �
 団陳陳陳陳陳陳陳陳津陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳田陳陳陳陳陳津陳陳陳陳陳�
 � �襷�� �窶譽��    �                              �            �          �
 団陳陳陳陳陳陳陳陳津陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳田陳陳陳陳陳津陳陳陳陳陳�
 � �皴��→� �窶譽�� �                              �            �          �
 団陳陳陳陳陳陳陳陳津陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳田陳陳陳陳陳津陳陳陳陳陳�
 � ���琺皋� ��錢��  �                              �            �          �
 青陳陳陳陳陳陳陳陳珍陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳祖陳陳陳陳陳珍陳陳陳陳陳�
ENDTEXT
PRIVATE nm_w
IF .NOT. SEEK(SALESMAN, "WHO_0328")
  nm_w = "?"
ELSE
  nm_w = ALLTRIM(WHO_0328.FAMILY)+" "+LEFT(WHO_0328.NAME,1)+" "+LEFT(WHO_0328.S_NAME,1)
ENDIF
@  4, 21 SAY nm_w
@  4, 52 SAY DTOC(ENTER_DAT)
@  4, 65 SAY ENTER_TIM

IF .NOT. EMPTY(MOD_DATE)
  IF .NOT. SEEK(WHO_MOD, "WHO_0328")
    nm_w = "?"
  ELSE
    nm_w = ALLTRIM(WHO_0328.FAMILY)+" "+LEFT(WHO_0328.NAME,1)+" "+LEFT(WHO_0328.S_NAME,1)
  ENDIF
  @  6, 21 SAY nm_w
  @  6, 52 SAY DTOC(MOD_DATE)
  @  6, 65 SAY MOD_TIME
ENDIF

IF .NOT. EMPTY(READ_DATE)
  tmpVal(1,1) = WHO_READ
  IF .NOT. SEEK(WHO_READ, "WHO_0328")
    tmpVal(1,2) = "?"
  ELSE
    tmpVal(1,2) = ALLTRIM(WHO_0328.FAMILY)+" "+LEFT(WHO_0328.NAME,1)+" "+LEFT(WHO_0328.S_NAME,1)
  ENDIF
  tmpVal(1,2) = PADR(tmpVal(1,2), 28)
  tmpVal(1,3) = READ_DATE
  tmpVal(1,4) = READ_TIME
ENDIF

IF .NOT. EMPTY(CALL_DATE)
  tmpVal(2,1) = WHO_CALL
  IF .NOT. SEEK(WHO_CALL, "WHO_0328")
    tmpVal(2,2) = "?"
  ELSE
    tmpVal(2,2) = ALLTRIM(WHO_0328.FAMILY)+" "+LEFT(WHO_0328.NAME,1)+" "+LEFT(WHO_0328.S_NAME,1)
  ENDIF
  tmpVal(2,2) = PADR(tmpVal(2,2), 28)
  tmpVal(2,3) = CALL_DATE
  tmpVal(2,4) = CALL_TIM
ENDIF

IF .NOT. EMPTY(GAVE_DATE)
  tmpVal(3,1) = WHO_GAVE
  IF .NOT. SEEK(WHO_GAVE, "WHO_0328")
    tmpVal(3,2) = "?"
  ELSE
    tmpVal(3,2) = ALLTRIM(WHO_0328.FAMILY)+" "+LEFT(WHO_0328.NAME,1)+" "+LEFT(WHO_0328.S_NAME,1)
  ENDIF
  tmpVal(3,2) = PADR(tmpVal(3,2), 28)
  tmpVal(3,3) = GAVE_DATE
  tmpVal(3,4) = GAVE_TIME
ENDIF

IF .NOT. EMPTY(TERM_DATE)
  tmpVal(4,1) = WHO_TERM
  IF .NOT. SEEK(WHO_TERM, "WHO_0328")
    tmpVal(4,2) = "?"
  ELSE
    tmpVal(4,2) = ALLTRIM(WHO_0328.FAMILY)+" "+LEFT(WHO_0328.NAME,1)+" "+LEFT(WHO_0328.S_NAME,1)
  ENDIF
  tmpVal(4,2) = PADR(tmpVal(4,2), 28)
  tmpVal(4,3) = TERM_DATE
  tmpVal(4,4) = TERM_TIME
ENDIF
FOR ex = 1 TO 4
  DO Show_Lin WITH ex
ENDFOR

ex = 1
@  8, 2 GET sw0(1) PICTURE "@*HN �ョ�碎 ��錢��   " VALID Sw_Flag(1)
@ 10, 2 GET sw0(2) PICTURE "@*HN �襷�� �窶譽��   " VALID Sw_Flag(2)
@ 12, 2 GET sw0(3) PICTURE "@*HN �皴��→� �窶譽��" VALID Sw_Flag(3)
@ 14, 2 GET sw0(4) PICTURE "@*HN ���琺皋� ��錢�� " VALID Sw_Flag(4)
@ WROWS()-1, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

*--------------------------------------------------------------------------
IF ex = 1
  REPLACE WHO_READ  WITH tmpVal(1,1),  ;
          READ_DATE WITH tmpVal(1,3),  ;
          READ_TIME WITH tmpVal(1,4),  ;
          WHO_CALL  WITH tmpVal(2,1),  ;
          CALL_DATE WITH tmpVal(2,3),  ;
          CALL_TIM  WITH tmpVal(2,4),  ;
          WHO_GAVE  WITH tmpVal(3,1),  ;
          GAVE_DATE WITH tmpVal(3,3),  ;
          GAVE_TIME WITH tmpVal(3,4),  ;
          WHO_TERM  WITH tmpVal(4,1),  ;
          TERM_DATE WITH tmpVal(4,3),  ;
          TERM_TIME WITH tmpVal(4,4)
ENDIF

POP KEY
RELEASE WINDOW (win_name)
USE
SELECT WHO_0328
USE
SELECT (s_sav)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Show_Lin     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                             �襤�� 痰牀��.                              �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 30.03.2000 陳�
PROCEDURE Show_Lin
PARAMETERS nLine

@ nLine*2+6, 22 SAY tmpVal(nLine, 2)
@ nLine*2+6, 53 SAY IIF(EMPTY(tmpVal(nLine, 3)), SPACE(10), DTOC(tmpVal(nLine, 3)))
@ nLine*2+6, 66 SAY tmpVal(nLine, 4)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Sw_Flag      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                     �ムオ�鈑キ┘ 筰�Κ� 甌痰�鎰��.                     �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 30.03.2000 陳�
PROCEDURE Sw_Flag

PARAMETERS nLine

IF EMPTY(tmpVal(nLine, 1))
  tmpVal(nLine, 1) = user
  tmpVal(nLine, 2) = self
  tmpVal(nLine, 3) = DATE()
  tmpVal(nLine, 4) = TIME()
ELSE
  tmpVal(nLine, 1) = 0
  tmpVal(nLine, 2) = SPACE(28)
  tmpVal(nLine, 3) = {}
  tmpVal(nLine, 4) = SPACE(8)
ENDIF

DO Show_Lin WITH nLine

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� E_Waight     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                              ��膈覃 ▲�.                               �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 30.03.2000 陳�
PROCEDURE E_Waight

IF EXA_WGHT
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��膈覃 ▲� 礒� 竅����←キ, ＜ぅ� 竄�膈閧�? [ �� ] [ �モ ]"
  mss(3) = ""
  IF Out_Mess(7, "mss") = 2
    RETURN
  ENDIF
ENDIF

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
*   ��膈覃 ▲� ��瘠���
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   ���Θ皀 皰膈覃 ▲� 99999 ��    �
*�                                  �
*� < OK Ctrl-W > < �皖���碎瘴 Esc > �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, w_wght
ex = 1
w_wght = WAIGHT
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 6, 38, "��膈覃 ▲� ��瘠���"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 2, 5 SAY "���Θ皀 皰膈覃 ▲�       ��"
@ 2, 24 GET w_wght
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

IF ex = 1
  *
  * �矗���硅��ガ ゛���
  REPLACE WAIGHT   WITH w_wght,  ;
          EXA_WGHT WITH .T.
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
