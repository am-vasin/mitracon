*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Seek_INN     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                         ���瓷 ��┘��� �� ���.                          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 02.09.2005 様�
PROCEDURE Seek_INN
PARAMETERS prmINN, prmKPP

PRIVATE s_sav, tmpINN, tmpKPP, res, tmpKey, tmpRec, is_list

m.is_list = .F.
m.res = 0
m.tmpINN = ""
m.tmpKPP = ""
DO Get_INN WITH 0, m.tmpINN, m.tmpKPP
IF .NOT. Fill_Blank()
  RETURN m.res
ENDIF
m.tmpKey = m.tmpINN+IIF(.NOT. EMPTY(m.tmpKPP), m.tmpKPP, "")
m.s_sav = SELECT()
SELECT 0
USE (m.base_path+"CUS_BIL") SHARED AGAIN ALIAS CB_5902 ORDER TAG INN
IF .NOT. SEEK(m.tmpKey)
  PRIVATE mss
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"��┃モ �� ���ぅ�!"
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  USE IN CB_5902
  SELECT (m.s_sav)
  RETURN m.res
ENDIF
m.tmpRec = RECNO()
SKIP
IF .NOT. EOF()
  m.is_list = CB_5902.INN+CB_5902.KPP = m.tmpKey
ENDIF
GO m.tmpRec

IF m.is_list
  m.res = Choose_C()
ELSE
  USE (m.base_path+"ACCOUNT") SHARED AGAIN ALIAS CUS_5902 ORDER TAG CUS_CODE IN 0
  IF SEEK(CB_5902.CUS_CODE, "CUS_5902")
    IF EMPTY(CUS_5902.DATE_OFF)
      m.res = RECNO("CUS_5902")
    ELSE
      PRIVATE mss
      DIMENSION mss[3]
      mss[1] = ""
      mss[2] = CHR(0)+"��┃モ �� ���ぅ�!"
      mss[3] = ""
      DO Out_Mess WITH 7, "mss"
    ENDIF
  ENDIF
  USE IN CUS_5902
ENDIF

USE IN CB_5902
SELECT (m.s_sav)

RETURN m.res

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Fill_Blank   ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           ������キ┘ ゛����.                           �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 02.09.2005 陳�
PROCEDURE Fill_Blank
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�                                         �
*� ��� ....:....!..   ��� ....:....!....:. �
*�                                         �
*�     < OK Ctrl-W > < �皖���碎瘴 Esc >    �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 7, 45, "�皰 �薀�?"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 3,  3 SAY "���" GET m.tmpINN
@ 3, 22 SAY "���" GET m.tmpKPP
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1 .AND. .NOT. EMPTY(m.tmpINN)

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Choose_c     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                        �襦�� ��┘��� ├ 甎�瓷�                         �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 02.09.2005 陳�
PROCEDURE Choose_c

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
PRIVATE res, mss
DIMENSION mss[3]
mss[1] = ""
mss[2] = CHR(0)+"�皰� ��┘�� 磴��キ!"
mss[3] = ""

*
*   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
*
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()
m.res = 0

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*
m.s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT 0
USE (m.base_path+"ACCOUNT") SHARED AGAIN ALIAS CUS_5902 ORDER TAG CUS_CODE
SELECT CB_5902
SET RELATION TO CUS_CODE INTO CUS_5902

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�襦��碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Main OF (menu_name) PROMPT "����↓�� �爛く�." KEY Ctrl-A, "Ins"
ON SELECTION PAD Main OF (menu_name) DO Sw_Mode WITH "Main"

DEFINE PAD Exit OF (menu_name) PROMPT "�皖���碎瘴" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd, r_sav
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 74

DO D_Wins WITH ln, wd, "��┘�硅", 0, 0
what_do = "List"
@ 1, 2 SAY ;
"�陳陳��┘�眥陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳����陳陳陳陳���楉陳陳陳陳陳��"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins   DO Sw_Mode WITH "Main"

*
*    BROWSE - �キ�
*
    BROWSE KEY m.tmpKey  ;
           FIELDS f001 = IIF(CB_5902.MAIN_FIRM, "�", " "):H="", ;
                  f000 = LEFT(CUS_5902.CUS_NAME,40):H="",   ;
                  CB_5902.INN:H="", ;
                  CB_5902.KPP:H="", ;
                  d000 = IIF(EMPTY(CUS_5902.DATE_OFF), " ", "�"):H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Main"    && ����↓�� �爛く爬閧┘

    IF CB_5902.MAIN_FIRM
      REPLACE CB_5902.MAIN_FIRM WITH .F.
    ELSE
      m.r_sav = RECNO("CB_5902")
      SELECT CB_5902
      SEEK m.tmpINN
      REPLACE REST WHILE INN = m.tmpINN MAIN_FIRM WITH .F.
      GO m.r_sav
      REPLACE CB_5902.MAIN_FIRM WITH .T.
    ENDIF
    what_do = "List"

  CASE what_do = "Choose"    && 蹍甃�矗 甎�瓷�

    IF EMPTY(CUS_5902.DATE_OFF)
      m.res = RECNO("CUS_5902")
      EXIT
    ENDIF
    DO Out_Mess WITH 7, "mss"
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
USE IN CUS_5902
SELECT (s_sav)

RETURN m.res
