*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�        ��玻オ罔� �ムョ�� 皋��� あ└キ�� ぅ�ィ � ��瘁�.                �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Cr_M_T
PRIVATE what_do

USE MM_Types ORDER TAG CODE
GO TOP
DO Use_Dummy
SELECT MM_Types

DEFINE MENU Cr_M_T IN SCREEN COLOR SCHEME 4
DEFINE PAD Corr OF Cr_M_T PROMPT "��玻オ罔�" KEY Ctrl-A, "Enter"
ON SELECTION PAD Corr OF Cr_M_T DO Ch_Mode WITH "Cr_M_T", "Corr"
DEFINE PAD Add  OF Cr_M_T PROMPT "����←キ┘" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF Cr_M_T DO Ch_Mode WITH "Cr_M_T", "Add"
DEFINE PAD Exit OF Cr_M_T PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF Cr_M_T DO Ch_Mode WITH "Cr_M_T", "Exit"

PRIVATE ln, wd, tt, point
DIMENSION tt(1)
ln = WROWS("")-7
tt(1) = PADC("��━キ����┘",FSIZE("NAME"),"�")+"�"+  ;
        "��┓陳����.�"
wd = LEN(tt(1))
DO Def_Win WITH "Types_M", "Types_E", "Types_I", ln, wd, "�┓� あ└キ�� ぅ�ィ", tt

what_do = IIF(RECCOUNT() = 0, "Add", "List")

DO WHILE .T.
  DO CASE
  CASE what_do = "List"
    DO Prp_Menu WITH "Cr_M_T"
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins   DO Ch_Mode WITH "Cr_M_T", "Add"
    ON KEY LABEL F10 ACTIVATE MENU Cr_M_T
    ACTIVATE WINDOW Types_E
    BROWSE FIELDS MM_TYPES.NAME:H="",   ;
                  F001=IIF(MM_TYPES.M_DIR = 1, "��痳��", "踸絎�"):6:H="", ;
                  F002=IIF(MM_TYPES.M_USD, "USD", "�磧��"):5:H="", ;
                  DUMMY.F:H=""  ;
           FREEZE DUMMY.F NOLGRID NOMENU NOCLEAR NODELETE NOAPPEND ;
           WINDOW Types_I IN WINDOW Types_E
    ON KEY
    HIDE MENU Cr_M_T
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Corr")
    ENDIF
  CASE what_do = "Add"
    DO Corr WITH "A"
    what_do = IIF(RECCOUNT() = 0, "Exit", "List")
  CASE what_do = "Corr"
    DO Corr
    what_do = "List"
  OTHERWISE
    EXIT
  ENDCASE
ENDDO

DEACTIVATE MENU Cr_M_T

RELEASE WINDOW Types_M
RELEASE WINDOW Types_E
RELEASE WINDOW Types_I

CLOSE DATABASES

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�        ��玻オ罔�/ぎ��←キ┘ 皋�� あ└キ�� ぅ�ィ � ��瘁�.              �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Corr
PARAMETER add

PRIVATE add_w, ln, wd, ex, rc, mss, m_val, wk_div, nm_w, s_s

rc = RECNO()
add_w = TYPE("add") # "L"
ex = 1
wk_div = .F.
nm_w = SPACE(24)
IF add_w
  SCATTER MEMVAR BLANK
  m.m_dir = 1
  m.m_val = 1
  GO BOTTOM
  m.code = CODE+1
ELSE
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"  �甌Л�モ� �� �� ≡� �ム� �爬�━�ガ�� �� 瓮￥ �癶モ痰▲���痰�? [ �� ] [ �モ ]"
  mss(3) = ""
  IF Out_Mess(7, "mss") = 2
    RETURN
  ENDIF
  SCATTER MEMVAR
  m.m_dir = m.m_dir+1
  m.m_val = IIF(M_RUB, 1, 2)
  s_s = SELECT()
  SELECT 0
  USE (base_path+"OPERSTRU") ORDER TAG CODE ALIAS W000 AGAIN
  IF SEEK(m.group)
    nm_w = NAME
  ENDIF
  USE
  SELECT (s_s)
ENDIF

*           ｯ����┘ ��ム�罔�
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�   ��━キ����┘ ...."....1....".   � 5
*�    ��Ж罔� ��瑙腑� ���※荐 99     �
*� 敖 �┓ ��ム�罔� 陳陳陳陳陳陳陳陳� � 3, 5, 35
*� �    ( ) 踸絎�  ( ) ��痳��     � � 8
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*� 敖 ��Гカ 陳陳陳陳陳陳陳陳陳陳陳� � 3, 5, 35
*� � [ ]  ....:....!....:....!.... � � 8
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*� 敖 ���鈞� 陳陳陳陳陳陳陳陳陳陳陳� �
*� �    ( ) �磧��   ( ) USD        � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*� < OK Ctrl-W > < �皖���碎瘴 Esc >  � 3
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

ln = 15
wd = 38

DO D_W WITH "Corr", ln, wd, "ｯ����┘ ��ム�罔� "+  ;
             IIF(add_w, "( ぎ��←キ┘ )", "( ��玻オ罔� )"), 1
@ 4,  3 TO 6, 35
@ 4,  5 SAY " �┓ ��ム�罔� "
@ 7,  3 TO 9, 35
@ 7,  5 SAY " ��Гカ "
@ 10, 3 TO 12, 35
@ 10, 5 SAY " ���鈞� "
@ 8, 10 SAY nm_w

@ 2,  5 SAY "��━キ����┘" GET m.name
@ 3,  6 SAY "��Ж罔� ��瑙腑� ���※荐" GET m.hot_key PICTURE "@Z 99"  WHEN Sh_Hot() VALID Sh_Hot()
@ 5,  8 GET m.m_dir PICTURE "@*RH 踸絎�;��痳��" WHEN add_w
@ 8,  5 GET wk_div PICTURE "@*C " VALID G_Div()
@ 11,  8 GET m.m_val PICTURE "@*RH �磧�� ;USD" WHEN add_w
@ 13, 3 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc " WHEN Sh_Hot()
= Sh_Hot()
READ CYCLE VALID Sav_Sha()

RELEASE WINDOW Corr

IF ex # 1 .AND. BETWEEN(rc, 1, RECCOUNT())
  GO rc
ENDIF

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                            �襦�� ��Гカ�.                             �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE G_Div

IF .NOT. wk_div
  RETURN .T.
ENDIF

DO Ch_OperG WITH m.group, nm_w
@ 8, 10 SAY nm_w
SHOW GET wk_div

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                  �矗�｀皖� ├�キキ┤ � ��Д ����諷.                   �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Sav_Sha
PRIVATE mss

IF ex # 1
  RETURN .T.
ENDIF

IF EMPTY(m.name)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"鉑皰� ━� ��ム�罔� - ��ぎ ���-�─磴� ��Б�碎!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF EMPTY(m.group)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� �� 礫����� ��Гカ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF add_w
  APPEND BLANK
  m.m_rub = m.m_val = 1
  m.m_usd = m.m_val = 2
ENDIF
m.m_dir = m.m_dir-1

GATHER MEMVAR

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                       ��め▲皖� ��瑙腑� ���※荐.                      �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Sh_Hot

SHOW GET m.name
IF m.hot_key > 0 .AND. m.hot_key <= LEN(m.name)
  @ 2, 17+m.hot_key FILL TO 2, 17+m.hot_key COLOR (SCHEME(14,1))
ENDIF

RETURN .T.
