*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Mark_Ord     ��о�｀砒┴ ��むォ ��瓱�           15.03.98 17:11:20 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                     �癶ムΔキ┘ ��錢�� �牀��∽��.                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Mark_Ord

PRIVATE mss, s_sav

IF .NOT. EMPTY(LIST_TIT.SAL_NUM)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���┘ ��ｃ� °碎 ��錢��? ���甌� �牀���!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

IF EMPTY(LIST_TIT.ORD_NUM)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� 甎�瓷� �� °�� �� �き�� ��錢��!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  POP KEY
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
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Print OF (menu_name) PROMPT "�ョ�碎"
ON PAD Print OF (menu_name) ACTIVATE POPUP Print

  DEFINE POPUP Print MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Print PROMPT "��爼�� 甎�瓷�" KEY Ctrl-A, "F1"
  ON SELECTION BAR 1 OF Print DO Sw_Mode WITH "Print_1"

  DEFINE BAR  2 OF Print PROMPT "��爼�� ��錢��" KEY Ctrl-A, "F2"
  ON SELECTION BAR 2 OF Print DO Sw_Mode WITH "Print_2"

DEFINE PAD Mark OF (menu_name) PROMPT "�癶ムえ碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Mark OF (menu_name) DO Sw_Mode WITH "Mark"

DEFINE PAD Note OF (menu_name) PROMPT "踸�ョ��┘" KEY Ctrl-A, "Tab"
ON SELECTION PAD Note OF (menu_name) DO Sw_Mode WITH "Note"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*
s_sav = SELECT()

SELECT 0
DO Use_Dummy

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS PSL_000 AGAIN

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS PST_000 AGAIN

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS PWK_000 AGAIN

SELECT 0
USE (base_path+"ORD_T") ORDER TAG DOC_NUM ALIAS ORD_999 AGAIN
SET RELATION TO WHO      INTO PSL_000
SET RELATION TO STO_MARK INTO PST_000 ADDITIVE
SET RELATION TO VAL(ALLTRIM(CHECK_S)) INTO PWK_000 ADDITIVE

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd, tmpVal
ln = 8   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 65

IF EMPTY(LIST_TIT.DATE_REQ)
  tmpVal = "    (�� 甌；��碎)"
ELSE
  tmpVal = "    � "+DTOC(LIST_TIT.DATE_REQ)+" "+LIST_TIT.TIME_REQ
ENDIF
*敖N陳陳����陳陳陳��爛�鐵陳陳蹍�.陳陳陳�� ��｀皀 c陳陳陳�����つ陳朕
*�999 ��.��.���� ��:��:�� ....:....! ��.��.���� ��:��:�� ....:....!�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
DO D_Wins WITH ln, wd, "C��甌� N "+ALLTRIM(STR(LIST_TIT.LST_NUM))+tmpVal, 0, 4
what_do = "List"
@ 1, 2 SAY ;
"�N陳陳����陳陳陳��爛�鐵陳陳蹍�.陳陳陳�� ��｀皀 c陳陳陳������"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    SELECT ORD_999
    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL F1    DO Sw_Mode WITH "Print_1"
    ON KEY LABEL F2    DO Sw_Mode WITH "Print_2"
    ON KEY LABEL Tab   DO Sw_Mode WITH "Note"
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE KEY STR(LIST_TIT.LST_NUM, 6) ;
           FIELDS ORD_NUM:3:H="",     ;
                  ORD_DATE:10:H="",   ;
                  ORD_TIME:8:H="",    ;
                  Sale_Stat  = IIF(EMPTY(WHO), " ", "�癶ムΔキ�"):10:H="", ;
                  In_Work = IIF(EMPTY(START_DATE), " ", DTOC(START_DATE)+" "+START_TIME):19:H="", ;
                  Stock_Stat = IIF(.NOT. EMPTY(STO_MARK), "�癶ムΔキ�", " "):10:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN V_Foot() ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Mark")
    ENDIF

  CASE what_do = "Mark"       && �癶ムえ碎

    DO P_D
    what_do = "List"

  CASE what_do = "Note"       && 踸�ョ��┘

    DO Modi_Nt
    what_do = "List"

  CASE what_do = "Print_1"    && �ョ�碎 � 筮爼�皀 甎�瓷�

    DO Prn_Ord1 WITH VAL(ALLTRIM(LIST_NUM)), ORD_NUM
    what_do = "List"

  CASE what_do = "Print_2"    && �ョ�碎 � 筮爼�皀 ��錢��

    IF new_ord
      DO Prn_OrdN WITH VAL(ALLTRIM(LIST_NUM)), ORD_NUM
    ELSE
      DO Prn_Ord WITH VAL(ALLTRIM(LIST_NUM)), ORD_NUM
    ENDIF
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

SELECT PSL_000
USE

SELECT PST_000
USE

SELECT PWK_000
USE

SELECT ORD_999

PRIVATE m_sl, m_st
STORE .T. TO m_sl, m_st
SEEK STR(LIST_TIT.LST_NUM, 6)
SCAN REST WHILE LIST_NUM = STR(LIST_TIT.LST_NUM, 6)
  IF EMPTY(WHO)
    m_sl = .F.
  ENDIF
  IF EMPTY(STO_MARK)
    m_st = .F.
  ENDIF
ENDSCAN
USE
SELECT LIST_TIT
REPLACE LIST_TIT.NOTE_STO WITH IIF(m_st, " ", "*"), ;
        LIST_TIT.NOTE_SAL WITH IIF(m_sl, " ", "*")

SELECT (s_sav)
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                          �癶ムΔキ┘ ��錢��.                           �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 15.03.98 17:31:53 夕
PROCEDURE P_D

PRIVATE mss
SELECT ORD_999
IF EMPTY(WHO)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"  �癶ムΔ�ガ? ��膈�? [ �� ] [ �モ ]"
  mss(3) = ""
  IF Out_Mess(5, "mss") = 1
    REPLACE WHO WITH user
    RETURN .T.
  ENDIF
ELSE
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��� ��錢�� 礒� 竄▲爨ぅ��!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
ENDIF

RETURN .F.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                    蹍������ �襤��� ぎ�. ┃筮爼�罔�.                  �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE V_Foot
PRIVATE vl1, vl2, vl3, vl4

ACTIVATE WINDOW (win_name+"_M") SAME
IF .NOT. EMPTY(WHO)
  vl1 = ALLTRIM(PSL_000.FAMILY)+" "+LEFT(PSL_000.NAME,1)+". "+LEFT(PSL_000.S_NAME,1)+"."
ELSE
  vl1 = ""
ENDIF

IF .NOT. EMPTY(CHECK_S)
  vl2 = ALLTRIM(PWK_000.FAMILY)+" "+LEFT(PWK_000.NAME,1)+". "+LEFT(PWK_000.S_NAME,1)+"."
ELSE
  vl2 = ""
ENDIF

IF .NOT. EMPTY(STO_MARK)
  vl3 = ALLTRIM(PST_000.FAMILY)+" "+LEFT(PST_000.NAME,1)+". "+LEFT(PST_000.S_NAME,1)+"."+ ;
        +"   "+DTOC(TERM_DATE)+" "+TERM_TIME
ELSE
  vl3 = ""
ENDIF

vl1 = "  蹍��▲�   "+PADR(vl1, 50)
vl2 = "  ��；��モ   "+PADR(vl2, 50)
vl3 = "  �癶ムえ�   "+PADR(vl3, 50)
vl4 = "  踸�ョ��┘ "+NOTE

@ WROWS()-5, 2 SAY vl1
@ WROWS()-4, 2 SAY vl2
@ WROWS()-3, 2 SAY vl3
@ WROWS()-2, 2 SAY vl4

ACTIVATE WINDOW (ALIAS())

RETURN .T.
