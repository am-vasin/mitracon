*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� B_Groups     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                    �珮��� 筥爼 - �牀├〓え皀�ォ ��.                    �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 16.03.2000 様�
PROCEDURE B_Groups

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
PRIVATE mss

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
USE (base_path+"B_GROUPS") ALIAS BG_0316 AGAIN

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Modify OF (menu_name) PROMPT "���" KEY Ctrl-A, "Enter"
ON SELECTION PAD Modify OF (menu_name) DO Sw_Mode WITH "Modify"

DEFINE PAD Ins OF (menu_name) PROMPT "����※碎" KEY Ctrl-A, "Ins"
ON SELECTION PAD Ins OF (menu_name) DO Sw_Mode WITH "Ins"

DEFINE PAD Del OF (menu_name) PROMPT "�����碎" KEY Ctrl-A, "Del"
ON SELECTION PAD Del OF (menu_name) DO Sw_Mode WITH "Del"

DEFINE PAD Firms OF (menu_name) PROMPT "��爼�" KEY Ctrl-A, "Tab"
ON SELECTION PAD Firms OF (menu_name) DO Sw_Mode WITH "Firms"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 68
*
*敖�珮���陳陳陳陳陳陳陳陳陳�N ���陳陳陳陳陳陳陳陳陳陳陳�矗���陳陳陳陳朕
*�....:....!....:....!.... ....:....!....:....!....:. ....:....!....:.�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

DO D_Wins WITH ln, wd, "�珮��� 筥爼 - �爬Б�え皀�ォ ��", 0, 0
@ 1, 2 SAY "��珮���陳陳陳陳陳陳陳陳陳陳陳陳�N ���陳陳陳陳陳陳陳陳�矗���"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins   DO Sw_Mode WITH "Ins"
    ON KEY LABEL Tab   DO Sw_Mode WITH "Firms"
    ON KEY LABEL Del   DO Sw_Mode WITH "Del"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS NAME:24:H="", GTD_NUM:26:H="", COUNTRY:16:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Modify")
    ENDIF

  CASE what_do = "Del"       && ����キ┘

    IF RECNO() = 1
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"�矣 �珮��� �カ讌� 磴��閧�!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
    ELSE
      DELETE
      SKIP
      IF EOF() .OR. BOF()
        GO BOTTOM
      ENDIF
    ENDIF
    what_do = "List"

  CASE what_do = "Modify"    && ��玻オ罔�

    DO Add_Line
    what_do = "List"

  CASE what_do = "Ins"       && ����←キ┘

    DO Add_Line WITH .T.
    what_do = "List"

  CASE what_do = "Firms"     && ���甌� 筥爼

    DO F_List
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
SELECT (s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Add_Line     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      ����←キ┘/��玻オ罔� �珮���.                      �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 16.03.2000 陳�
PROCEDURE Add_Line
PARAMETERS new

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�  ��� �珮��� ....:....!....:....!....  �
*�                                       �
*�    N ��� ....:....!....:....!....:.   �
*�                                       �
*�      �矗��� ....:....!....:.          �
*�                                       �
*�   < OK Ctrl-W > < �皖���碎瘴 Esc >    �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex, n1, n2, n3, d_sav
ex = 1
IF new
  n1 = SPACE(24)
  n2 = SPACE(26)
  n3 = SPACE(16)
ELSE
  n1 = NAME
  n2 = GTD_NUM
  n3 = COUNTRY
ENDIF
  
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 10, 43, IIF(new, "����←キ┘", "��玻オ罔�")+" �珮���"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 2, 4 SAY "��� �珮���" GET n1
@ 4, 6 SAY "N ���" GET n2
@ 6, 8 SAY "�矗���" GET n3
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

IF ex = 1
  *
  * �矗���硅��ガ ゛���
  IF new
    d_sav = SET("DELETED") = "ON"
    SET DELETED OFF
    LOCATE FOR DELETED()
    IF FOUND()
      RECALL
      REPLACE DEV_LIST WITH ""
    ELSE
      APPEND BLANK
    ENDIF
    IF d_sav
      SET DELETED ON
    ENDIF
  ENDIF
  REPLACE NAME   WITH  n1, ;
          GTD_NUM WITH n2, ;
          COUNTRY WITH n3
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� F_List       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                             ��痰�� �珮��.                              �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 16.03.2000 陳�
PROCEDURE F_List

PRIVATE s_sav         && ���ム ��｀腑� �゛�痰� か� 甌縲��キ�� � 〓瘁����←キ��!
PRIVATE r_sav, tmpList, tmpRec, i, n, w, mss
r_sav = RECNO()
s_sav = SELECT()

n = 0
SCAN
  n = n+1
  DIMENSION tmpList(n), tmpRec(n), tmpNames(n)
  tmpList(n)  = DEV_LIST
  tmpRec(n)     = RECNO()
  tmpNames(n) = NAME
ENDSCAN

SELECT 0
DELETE FILE (tmpo_path+"TMP_GR.DBF")
CREATE DBF (tmpo_path+"TMP_GR") ;
    ( F_CODE N( 5), ;
      F_NAME C( 8), ;
      G_REC  N( 3), ;
      G_NAME C(24) )

SELECT 0
USE (base_path+"DEVELOP") ORDER TAG B_NAME ALIAS DV_0316
SCAN
  w = 1
  IF n > 1
    FOR i = 1 TO n
      IF " "+ALLTRIM(STR(CODE))+" " $ tmpList(i)
        w = i
        EXIT
      ENDIF
    ENDFOR
  ENDIF
  SELECT TMP_GR
  APPEND BLANK
  REPLACE F_CODE WITH DV_0316.CODE,   ;
          F_NAME WITH DV_0316.B_NAME, ;
          G_REC  WITH tmpRec(w)
  SELECT DV_0316
ENDSCAN
USE
SELECT TMP_GR
SET RELATION TO G_REC INTO BG_0316

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
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Change OF (menu_name) PROMPT "�珮���" KEY Ctrl-A, "Enter"
ON SELECTION PAD Change OF (menu_name) DO Sw_Mode WITH "Change"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = WROWS("")-7   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 33
*敖��爼�陳陳�珮���陳陳陳陳陳陳陳陳朕
*�....:... ....:....!....:....!....�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

DO D_Wins WITH ln, wd, "�珮���", 0, 0
@ 1, 2 SAY "���爼�陳陳�珮���"
what_do = "List"

GO TOP
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    GO RECNO()   && ����爬��!!!
    BROWSE FIELDS F_NAME:8:H="", BG_0316.NAME:24:H="",  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY  ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Change")
    ENDIF

  CASE what_do = "Change"    && �襦�� �珮���

    DO Choose
    what_do = "List"

  OTHERWISE

    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"  ��縲���碎 ├�キキ��? [ �� ] [ �モ ]"
    mss(3) = ""
    IF Out_Mess(5, "mss") = 1
      FOR i = 1 TO n
        tmpList(i) = " "
      ENDFOR
      SCAN
        i = ASCAN(tmpRec, G_REC)
        IF i > 1
          tmpList(i) = tmpList(i)+ALLTRIM(STR(F_CODE))+" "
        ENDIF
      ENDSCAN
      FOR i = 2 TO n
        SELECT BG_0316
        GO tmpRec(i)
        REPLACE DEV_LIST WITH tmpList(i)
        SELECT TMP_GR
      ENDFOR
    ENDIF
    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
SET RELATION TO
USE
DELETE FILE (tmpo_path+"TMP_GR.DBF")
*
*   ���窶�皚キ讓� (���窶�皚キ讓�!) ���琺��ガ DBF-�����.
*
SELECT (s_sav)
GO r_sav

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Choose       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                        �襦�� �珮��� か� ÷キ��.                        �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 17.03.2000 陳�
PROCEDURE Choose

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

PRIVATE tmpPoint
tmpPoint = MAX(ASCAN(tmpRec, G_REC),1)

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH MIN(ALEN(tmpNames)+3, WROWS("")-11), LEN(tmpNames(1))+4, "�珮��� か� "+ALLTRIM(F_NAME)
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
DO WHILE .T.
  tmpPoint = MAX(tmpPoint,1)
  tmpPoint = MIN(tmpPoint,ALEN(tmpNames))
  @ 1, 1 MENU tmpNames, ALEN(tmpNames), WROWS("")-3
  READ MENU TO tmpPoint
  IF READKEY() % 256 = 12
    EXIT
  ENDIF
  IF READKEY() % 256 = 15
    REPLACE G_REC WITH tmpRec(tmpPoint)
    EXIT
  ENDIF
ENDDO

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
