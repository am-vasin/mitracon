*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Inp_Req      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                   �〓� ぅ�ィ �� ��錢�� �� �牀��∽�.                    �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 22.07.1999 様�
PROCEDURE Inp_Req

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
*IF File_O(base_path+"MAX_ORD.MEM")
*  RESTORE FROM (base_path+"MAX_ORD.MEM") ADDITIVE
*ENDIF
PRIVATE pl_code
pl_code = sys_char

s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE AGAIN ALIAS CS_9722

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE AGAIN ALIAS WS_9722

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE AGAIN ALIAS WK_9722

SELECT 0
USE (base_path+"BUHG_REQ") ORDER TAG DOC_NUM AGAIN
GO BOTTOM
SET RELATION TO WHO_REQ INTO WS_9722
SET RELATION TO WHO_INP INTO WK_9722 ADDITIVE
SET RELATION TO CUS_CODE INTO CS_9722 ADDITIVE

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Seek OF (menu_name) PROMPT "���瓷" KEY Ctrl-A, "F7"
ON SELECTION PAD Seek OF (menu_name) DO Sw_Mode WITH "Seek"

DEFINE PAD Inp_M OF (menu_name) PROMPT "踸絎�/���▲珥�罔�" KEY Ctrl-A, "Enter"
ON SELECTION PAD Inp_M OF (menu_name) DO Sw_Mode WITH "Inp_M"

DEFINE PAD Print OF (menu_name) PROMPT "�ョ�碎" KEY Ctrl-A, "Tab"
ON SELECTION PAD Print OF (menu_name) DO Sw_Mode WITH "Print"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
*敖N �爛｀�陳陳����陳陳���┘�眥陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳���
*�NNNNNNNNNN ��.��.���� ���������������������������������������������� **�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*敖蹍��▲翊陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳��瘁�狡陳陳陳陳陳陳陳陳陳陳陳�
*� ....:....!....:....!.... . .             ....:....!....:....!.... . . �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
PRIVATE ln, wd
PRIVATE mss
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 72

DO D_Wins WITH ln, wd, "��錢�� �� �牀��∽��", 0, 1
@ 1, 3 SAY "N �爛｀�陳陳����陳陳���┘�眥陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳���"
@ WROWS()-3, 3 SAY "蹍��▲翊陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳��瘁��"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�
    SET RELATION TO WHO_REQ INTO WS_9722
    SET RELATION TO WHO_INP INTO WK_9722 ADDITIVE
    SET RELATION TO CUS_CODE INTO CS_9722 ADDITIVE

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F7 DO Sw_Mode WITH "Seek"
    ON KEY LABEL Tab DO Sw_Mode WITH "Print"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS DOC_NUM:10:H="", DOC_DATE:P="@D":H="", CS_9722.CUS_NAME:47:H="", ;
                  S00 = IIF(IS_INC, "�", " ")+IIF(IS_CONV, "�", " "):2:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Inp_M")
    ENDIF

  CASE what_do = "Print"    && �ョ�碎

    DO P_B_Req WITH DOC_NUM, DOC_DATE
    what_do = "List"

  CASE what_do = "Inp_M"    && 踸絎� ぅ�ィ

    DO CASE
    CASE IS_CONV
      IF BOF() .OR. EOF()
        what_do = "List"
        LOOP
      ENDIF
      IF CUS_CODE = 0
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"蹍��▲� �� 礫���� ��┘���!"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        what_do = "List"
        LOOP
      ENDIF
      DO Conv_dep WITH CUS_CODE
      SELECT BUHG_REQ
      what_do = "List"
    CASE IS_INC
      IF BOF() .OR. EOF()
        what_do = "List"
        LOOP
      ENDIF
      IF CUS_CODE = 0
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"蹍��▲� �� 礫���� ��┘���!"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        what_do = "List"
        LOOP
      ENDIF
      DO IncMoney WITH FIRM, DOC_NUM, DOC_DATE, "Req"
      SELECT BUHG_REQ
      what_do = "List"

    OTHERWISE
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"�瓮 ��ム�罔� 礒� �覩���キ�!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      what_do = "List"
      LOOP
    ENDCASE

  CASE what_do = "Seek"    && ���瓷

    DO Seek_Num
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
USE
SELECT CS_9722
USE
SELECT WS_9722
USE
SELECT WK_9722
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
@ WROWS()-2,  3 SAY WS_9722.FAMILY-(" "+LEFT(WS_9722.NAME,1))-(" "+LEFT(WS_9722.S_NAME,1))
@ WROWS()-2, 44 SAY WK_9722.FAMILY-(" "+LEFT(WK_9722.NAME,1))-(" "+LEFT(WK_9722.S_NAME,1))

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Seek_Num     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                            ���瓷 �� ���ム�.                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 22.07.1999 陳�
PROCEDURE Seek_Num
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�                                    �
*� ��� 9999   ���ム ��錢�� 9999999999 �
*�                                    �
*�  < OK Ctrl-W > < �皖���碎瘴 Esc >  �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, y, n
ex = 1
y = YEAR(DATE())
n = SPACE(10)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 7, 40, "���瓷 ��錢��"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@ 3, 14 SAY "���ム ��錢��" GET n
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "
@ 3,  3 SAY "���" GET y PICTURE "9999"

READ CYCLE

IF ex = 1
  *
  * �矗���硅��ガ ゛���
  PRIVATE r_sav
  r_sav = RECNO()
  n = PADL(ALLTRIM(n),10)
  IF .NOT. SEEK(STR(y,4)+n)
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"�モ ��錢�� � ���━ ���ム��!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    IF BETWEEN(r_sav, 1, RECCOUNT())
      GO r_sav
    ENDIF
  ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
