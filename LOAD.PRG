*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Load     ��о�｀砒┴ ��むォ ��瓱�                          �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                            ｯ����┘ �珮М�.                            �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 20.01.2000 様�
PROCEDURE Load

PRIVATE path_as, mss

path_as = ALLTRIM(Get_Sys("PATH_AS"))
IF EMPTY(path_as)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� ��爛ぅ�キ� ���� ����諷 �珮М�."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF
mss = .F.
IF TYPE("path_comm") = "C"
  mss = .NOT. EMPTY(path_comm)
ENDIF
IF .NOT. mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� ��爛ぅ�キ� �♂�� ���� ����諷."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
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
USE (path_as+"LOAD") ORDER TAG LD_NUM ALIAS LT_0120 AGAIN
SELECT 0
USE (path_comm+"INVOICET") ORDER TAG LD_BOX ALIAS IT_0120 AGAIN
SELECT 0
USE (path_as+"BOXNO") ORDER TAG BOXNO ALIAS BX_0120 AGAIN
SELECT 0
USE (path_as+"LOAD_D") ORDER TAG LOAD_NUM ALIAS LD_0120 AGAIN
SET RELATION TO STR(YEAR(LD_DATE),4)+STR(LD_NUM,FSIZE("LD_NUM")) ;
    INTO LT_0120
SET RELATION TO ;
    STR(YEAR(LD_DATE),4)+STR(LD_NUM)+OFFICE+STR(BOX_YEAR,4)+STR(BOXNO) ;
    INTO IT_0120 ADDITIVE
SET RELATION TO ;
    OFFICE+STR(BOX_YEAR,4)+STR(BOXNO) ;
    INTO BX_0120 ADDITIVE
*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Order OF (menu_name) PROMPT "�┐"
ON PAD Order OF (menu_name) ACTIVATE POPUP Order

  DEFINE POPUP Order MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Order PROMPT "����瑙え腮碎 �� �珮���" KEY Ctrl-A, "Ctrl-F1"
  ON SELECTION BAR 1 OF Order DO Sw_Mode WITH "OrdLoad"

  DEFINE BAR  2 OF Order PROMPT " ���瑙え腮碎 �� ��牀！��" KEY Ctrl-A, "F1"
  ON SELECTION BAR 2 OF Order DO Sw_Mode WITH "OrdBox"

  DEFINE BAR  3 OF Order PROMPT "\-"

  DEFINE BAR  4 OF Order PROMPT "��瓮 ��牀！�" KEY Ctrl-A, "F6"
  ON SELECTION BAR 4 OF Order DO Sw_Mode WITH "AllBox"

  DEFINE BAR  5 OF Order PROMPT " ���讓� ��牀！� � �����き覓�" KEY Ctrl-A, "Shift-F6"
  ON SELECTION BAR 5 OF Order DO Sw_Mode WITH "InvBox"

  DEFINE BAR  6 OF Order PROMPT " ���讓� �爬�閧襯" KEY Ctrl-A, "Ctrl-F6"
  ON SELECTION BAR 6 OF Order DO Sw_Mode WITH "GotBox"

  DEFINE BAR  7 OF Order PROMPT " ���讓� �� �爬�閧襯" KEY Ctrl-A, "Alt-F6"
  ON SELECTION BAR 7 OF Order DO Sw_Mode WITH "NotGotBox"

DEFINE PAD View OF (menu_name) PROMPT "蹍甃�矗"
ON PAD View OF (menu_name) ACTIVATE POPUP View

  DEFINE POPUP View MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF View PROMPT "��ぅ爨━��� ��牀！�" KEY Ctrl-A, "F2"
  ON SELECTION BAR 1 OF View DO Sw_Mode WITH "ViewBox"

  DEFINE BAR  2 OF View PROMPT "踸ガ�諷 ��皰�" KEY Ctrl-A, "F3"
  ON SELECTION BAR 2 OF View DO Sw_Mode WITH "ViewActs"

DEFINE PAD Seek OF (menu_name) PROMPT "���瓷"
ON PAD Seek OF (menu_name) ACTIVATE POPUP Seek

  DEFINE POPUP Seek MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Seek PROMPT "�� ��爲�牀→�" KEY Ctrl-A, "F7"
  ON SELECTION BAR 1 OF Seek DO Sw_Mode WITH "SeekMask"

  DEFINE BAR  2 OF Seek PROMPT "�� ��Ж罔�" KEY Ctrl-A, "Ctrl-F7"
  ON SELECTION BAR 2 OF Seek DO Sw_Mode WITH "SeekPos"

DEFINE PAD Weight OF (menu_name) PROMPT "�メ" KEY Ctrl-A, "Tab"
ON SELECTION PAD Weight OF (menu_name) DO Sw_Mode WITH "Weight"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = WROWS("")-5   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 75
*
*敖�N �珮��陳���� �珮��陳�筥當陳�N �.���つ��陳ｦ┐�モ瘴������き�鐵陳�踸ガ��朕
*�XXXXXXXXXX ��.��.���� XXXXXXXX XXXX XXXX C ��.��.���� XXXXXXXXXX ��.��.�����
*青陳陳陳陳��爲�牀→� ��牀！� XXXXXXXXXX陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
DO D_Wins WITH ln, wd, "�ムョキ� ��牀｀�", 0, 1
@ 1, 2 SAY ;
"陳N �珮��陳���� �珮��陳�筥當陳�N �.���つ��陳ｦ┐�モ瘴������き�鐵陳�踸ガ��"
what_do = "List"

GO BOTTOM
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Ctrl+F1  DO Sw_Mode WITH "OrdLoad"
    ON KEY LABEL F1       DO Sw_Mode WITH "OrdBox"
    ON KEY LABEL F6       DO Sw_Mode WITH "AllBox"
    ON KEY LABEL Shift+F6 DO Sw_Mode WITH "InvBox"
    ON KEY LABEL Ctrl+F6  DO Sw_Mode WITH "GotBox"
    ON KEY LABEL Alt+F6   DO Sw_Mode WITH "NotGotBox"
    ON KEY LABEL F2       DO Sw_Mode WITH "ViewBox"
    ON KEY LABEL F3       DO Sw_Mode WITH "ViewActs"
    ON KEY LABEL F7       DO Sw_Mode WITH "SeekMask"
    ON KEY LABEL Ctrl+F7  DO Sw_Mode WITH "SeekPos"
    ON KEY LABEL Tab      DO Sw_Mode WITH "Weight"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS LD_0120.LD_NUM:H="",  ;
                  LD_0120.LD_DATE:H="", ;
                  LD_0120.OFFICE:H="",  ;
                  LD_0120.BOXNO:H="",   ;
                  LD_0120.BOX_YEAR:H="",;
                  URGENT = IIF(EMPTY(LT_0120.QUICKLY), " ", "�"):H="", ;
                  LT_0120.LEADTIME:H="",;
                  IT_0120.EXP_NUM:H="", ;
                  IT_0120.DATE_INC:H="",;
           DUMMY.F:H="" FREEZE DUMMY.F  ;
           WHEN Draw_Foot()    ;
           NOMODIFY ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")

    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Exit")
    ENDIF

  CASE what_do = "OrdLoad"    && ���瑙ぎ腮碎 �� �珮���

    SET ORDER TO LOAD_NUM
    DEFINE BAR  1 OF Order PROMPT "����瑙え腮碎 �� �珮���" KEY Ctrl-A, "Ctrl-F1"
    DEFINE BAR  2 OF Order PROMPT " ���瑙え腮碎 �� ��牀！��" KEY Ctrl-A, "F1"
    what_do = "List"

  CASE what_do = "OrdBox"     && ���瑙ぎ腮碎 �� ��牀！��

    SET ORDER TO BOXNO
    DEFINE BAR  1 OF Order PROMPT " ���瑙え腮碎 �� �珮���" KEY Ctrl-A, "Ctrl-F1"
    DEFINE BAR  2 OF Order PROMPT "����瑙え腮碎 �� ��牀！��" KEY Ctrl-A, "F1"
    what_do = "List"

  CASE what_do = "AllBox"     && �瓮 ��牀！�

    DEFINE BAR  4 OF Order PROMPT "��瓮 ��牀！�" KEY Ctrl-A, "F6"
    DEFINE BAR  5 OF Order PROMPT " ���讓� ��牀！� � �����き覓�" KEY Ctrl-A, "Shift-F6"
    DEFINE BAR  6 OF Order PROMPT " ���讓� �爬�閧襯" KEY Ctrl-A, "Ctrl-F6"
    DEFINE BAR  7 OF Order PROMPT " ���讓� �� �爬�閧襯" KEY Ctrl-A, "Alt-F6"
    SET FILTER TO
    what_do = "List"

  CASE what_do = "InvBox"     && ���讓� ��牀！� � �����き覓�

    DEFINE BAR  4 OF Order PROMPT " �瓮 ��牀！�" KEY Ctrl-A, "F6"
    DEFINE BAR  5 OF Order PROMPT "����讓� ��牀！� � �����き覓�" KEY Ctrl-A, "Shift-F6"
    DEFINE BAR  6 OF Order PROMPT " ���讓� �爬�閧襯" KEY Ctrl-A, "Ctrl-F6"
    DEFINE BAR  7 OF Order PROMPT " ���讓� �� �爬�閧襯" KEY Ctrl-A, "Alt-F6"
    SET FILTER TO FOUND("IT_0120")
    what_do = "List"

  CASE what_do = "GotBox"     && ���讓� �爬�閧襯

    DEFINE BAR  4 OF Order PROMPT " �瓮 ��牀！�" KEY Ctrl-A, "F6"
    DEFINE BAR  5 OF Order PROMPT " ���讓� ��牀！� � �����き覓�" KEY Ctrl-A, "Shift-F6"
    DEFINE BAR  6 OF Order PROMPT "����讓� �爬�閧襯" KEY Ctrl-A, "Ctrl-F6"
    DEFINE BAR  7 OF Order PROMPT " ���讓� �� �爬�閧襯" KEY Ctrl-A, "Alt-F6"
    SET FILTER TO FOUND("IT_0120") .AND. .NOT. EMPTY(IT_0120.DATE_INC)
    what_do = "List"

  CASE what_do = "NotGotBox"     && ���讓� �� �爬�閧襯

    DEFINE BAR  4 OF Order PROMPT " �瓮 ��牀！�" KEY Ctrl-A, "F6"
    DEFINE BAR  5 OF Order PROMPT " ���讓� ��牀！� � �����き覓�" KEY Ctrl-A, "Shift-F6"
    DEFINE BAR  6 OF Order PROMPT " ���讓� �爬�閧襯" KEY Ctrl-A, "Ctrl-F6"
    DEFINE BAR  7 OF Order PROMPT "����讓� �� �爬�閧襯" KEY Ctrl-A, "Alt-F6"
    SET FILTER TO FOUND("IT_0120") .AND. EMPTY(IT_0120.DATE_INC)
    what_do = "List"

  CASE what_do = "ViewBox"     && ��ぅ爨━�� ��牀！�

    DO ViewBox
    what_do = "List"

  CASE what_do = "ViewActs"    && 踸ガ�襯 ��硅

    DO ViewAct
    what_do = "List"

  CASE what_do = "SeekMask"    && ���瓷 �� ��爲ム�→�

    DO SeekMask
    what_do = "List"

  CASE what_do = "SeekPos"     && ���瓷 �� ��Ж罔�

    DO SeekPos
    what_do = "List"

  CASE what_do = "Weight"       && �メ

    IF FOUND("BX_0120")
      DO Get_Wght
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
*
*   ���窶�皚キ讓� (���窶�皚キ讓�!) ���琺��ガ DBF-�����.
*
USE
SELECT LT_0120
USE
SELECT BX_0120
USE
SELECT IT_0120
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
PRIVATE s, p
ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� Гメ�, 爬痺ガ...
*
s = " ��爲�牀→� ��牀！�: "+IIF(EMPTY(LD_0120.MASK), ;
        ALLTRIM(LD_0120.OFFICE)+ALLTRIM(STR(LD_0120.BOXNO)),ALLTRIM(LD_0120.MASK))
@ WROWS()-2, 3 SAY PADR(s, 35)
@ WROWS()-2, COL() SAY "�メ: �爛あ. "+TRANSFORM(BX_0120.WEIGHT,"@Z 999.9")+  ;
                       "   皰膈� "+TRANSFORM(BX_0120.WEIGHT_ST,"@Z 999.9")

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� ViewBox      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                          ��ぅ爨━�� ��牀！�.                           �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 21.01.2000 陳�
PROCEDURE ViewBox

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
DO Prep_Pos
IF BOF() .OR. EOF()
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��牀！� �竅��..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  DELETE FILE (tmpo_path+"TMP_POS.DBF")
  DELETE FILE (tmpo_path+"TMP_POS.CDX")
  SELECT (s_sav)
  RETURN
ENDIF
*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 48

*敖 ��Ж罔� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳 ���-〓朕
*�XXXX XXXXXXXXXXXXXXXXXXXXXXXXX XXXXXXXX 99999999�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

DO D_Wins WITH ln, wd, "��ぅ爨━�� ��牀！� "+  ;
       LD_0120.OFFICE+" "+ALLTRIM(STR(LD_0120.BOXNO)), 0, 0
@ 1, 2 SAY "� ��Ж罔� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳 ���-〓�"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1

*
*    BROWSE - �キ�
*
    BROWSE FIELDS PREFIX:H="", NAME:H="", PRODUCER:H="", QNT:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Exit")
    ENDIF

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
DELETE FILE (tmpo_path+"TMP_POS.DBF")
DELETE FILE (tmpo_path+"TMP_POS.CDX")

SELECT (s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Prep_Pos     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�            ��ぃ�皰→� ∇ガキ���� ����� 甌ぅ爨━��� ��牀！�.            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 21.01.2000 陳�
PROCEDURE Prep_Pos

SELECT 0
DELETE FILE (tmpo_path+"TMP_POS.DBF")
DELETE FILE (tmpo_path+"TMP_POS.CDX")
CREATE DBF (tmpo_path+"TMP_POS") ;
   (  ID     C(11),   ;
      PREFIX C( 4),   ;
      NAME   C(35),   ;
      PRODUCER C( 8), ;
      QNT    N( 8)    )
INDEX ON ID TAG ID

SELECT 0
USE (path_comm+"PURBASE") ALIAS ST_0121 ORDER TAG IDP AGAIN

SELECT 0
USE (path_comm+"ONHAND") ALIAS CNT_0121 ORDER TAG BOXNO AGAIN
SET RELATION TO PURSYS+STR(IDP) INTO ST_0121

SEEK LD_0120.OFFICE+STR(LD_0120.BOX_YEAR,4)+STR(LD_0120.BOXNO)
SCAN REST WHILE LD_0120.OFFICE = BOXOFFIC .AND. ;
                LD_0120.BOX_YEAR = BOXYEAR .AND. ;
                LD_0120.BOXNO = BOXNO
  SELECT TMP_POS
  IF .NOT. SEEK(CNT_0121.PURSYS+STR(CNT_0121.IDP))
    APPEND BLANK
    REPLACE ID       WITH CNT_0121.PURSYS+STR(CNT_0121.IDP), ;
            PREFIX   WITH ST_0121.EQVPREF, ;
            NAME     WITH ST_0121.EQVNAME, ;
            PRODUCER WITH ST_0121.EQVPROD
  ENDIF
  REPLACE QNT WITH QNT+CNT_0121.QTY
  SELECT CNT_0121
ENDSCAN
USE
SELECT ST_0121
USE
SELECT TMP_POS
INDEX ON NAME+PREFIX+PRODUCER TAG NAME FOR QNT # 0
GO TOP

RETURN


*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� ViewAct      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                     蹍甃�矗 �爬ガ�諷 ��皰�.                           �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 21.01.2000 陳�
PROCEDURE ViewAct

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
IF EMPTY(IT_0120.EXP_NUM) .OR. EMPTY(IT_0120.EXP_DATE)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"� ��牀！� �モ �����き��..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

s_sav = SELECT()

SELECT 0
DO Use_Dummy
DO Prp_Act
IF BOF() .OR. EOF()
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�モ �� �き��� �爬ガ���� ����..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  DELETE FILE (tmpo_path+"TMP_ACT.DBF")
  DELETE FILE (tmpo_path+"TMP_ACT.CDX")
  SELECT (s_sav)
  RETURN
ENDIF
*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Print OF (menu_name) PROMPT "�ョ�碎 �爬ガ���� ����" KEY Ctrl-A, "Enter"
ON SELECTION PAD Print OF (menu_name) DO Sw_Mode WITH "Print"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"
*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 25

*��爼陳N ぎ�-��陳陳�����陳朕
*�XXX NNNNNNNNNN ��.��.�����
*青陳陳陳陳陳陳陳陳陳陳陳陳�
GO TOP
DO D_Wins WITH ln, wd, "踸ガ�襯 ��硅", 0, 0
@ 1, 2 SAY "�爼陳N ぎ�-��陳陳�����"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE FIELDS FIRM:H="", D_NUM:H="", D_DATE:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Print")
    ENDIF

  CASE what_do = "Print"    && �ョ�碎 �爬ガ���� ����

    DO P_Act WITH FIRM, D_NUM, D_DATE
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
DELETE FILE (tmpo_path+"TMP_ACT.DBF")
DELETE FILE (tmpo_path+"TMP_ACT.CDX")

SELECT (s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Prp_Act      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�            ��ぃ�皰→� ∇ガキ���� ����� � �爬ガ�覓� ������.             �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 21.01.2000 陳�
PROCEDURE Prp_Act
PRIVATE p, tmpStr, tmpKey
SELECT 0
DELETE FILE (tmpo_path+"TMP_ACT.DBF")
DELETE FILE (tmpo_path+"TMP_ACT.CDX")
CREATE DBF (tmpo_path+"TMP_ACT") ;
   (  FIRM   C( 3),   ;
      D_NUM  C(10),   ;
      D_DATE D   )

INDEX ON FIRM+STR(YEAR(D_DATE),4)+D_NUM TAG D_NUM

SELECT 0
USE (base_path+"ACT_S_T") ALIAS A_0121 ORDER TAG DOC_NUM AGAIN

SELECT 0
USE (path_comm+"INVOICED") ALIAS ID_0121 ORDER TAG EXP_NUM AGAIN

SEEK STR(YEAR(IT_0120.EXP_DATE),4)+IT_0120.EXP_NUM
SCAN REST WHILE IT_0120.EXP_DATE = EXP_DATE .AND. EXP_NUM = IT_0120.EXP_NUM ;
       FOR .NOT. EMPTY(AKT_NUM)
  tmpStr = ALLTRIM(AKT_NUM)
  DO WHILE .NOT. EMPTY(tmpStr)
    p = AT(",", tmpStr)
    IF p = 0
      EXIT
    ENDIF
    tmpKey = LEFT(tmpStr,p-1)
    tmpStr = SUBSTR(tmpStr,p+1)
    tmpKey = LEFT(tmpKey,7)+PADL(SUBSTR(tmpKey,8), 10)
    IF SEEK(tmpKey, "TMP_ACT")
      LOOP
    ENDIF
    IF .NOT. SEEK(tmpKey, "A_0121")
      LOOP
    ENDIF
    SELECT TMP_ACT
    APPEND BLANK
    REPLACE FIRM   WITH A_0121.FIRM,    ;
            D_NUM  WITH A_0121.DOC_NUM, ;
            D_DATE WITH A_0121.DOC_DATE
  ENDDO
  SELECT ID_0121
ENDSCAN
SELECT TMP_ACT
SET ORDER TO
IF RECCOUNT() # 0
  SCAN WHILE .NOT. "!" $ D_NUM
    p = RECNO()
    = SEEK(FIRM+STR(YEAR(D_DATE),4)+D_NUM, "A_0121")
    SELECT A_0121
    tmpKey = FIRM+DTOS(DOC_DATE)+DOC_NUM
    SCAN REST FOR "!" $ DOC_NUM .AND. tmpKey = FIRM+DTOS(EXP_DATE)+EXP_NUM
      SELECT TMP_ACT
      APPEND BLANK
      REPLACE FIRM   WITH A_0121.FIRM,    ;
              D_NUM  WITH A_0121.DOC_NUM, ;
              D_DATE WITH A_0121.DOC_DATE
      SELECT A_0121
    ENDSCAN
    SELECT TMP_ACT
    GO p
  ENDSCAN
ENDIF
SELECT A_0121
USE
SELECT ID_0121
USE
SELECT TMP_ACT
INDEX ON DTOS(D_DATE) TAG HRON
GO TOP

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� SeekMask     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      ���瓷 ��牀｀� �� ��爲�牀→�.                      �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 22.01.2000 陳�
PROCEDURE SeekMask

PRIVATE shab, o_sav, r_sav, s_sav, n_rec
shab = GetMask()
IF EMPTY(shab)
  RETURN
ENDIF

s_sav = SELECT()
SELECT LD_0120
o_sav = TAG()
r_sav = RECNO()

DO Prp_Mask

n_rec = ViewMask()
SELECT LD_0120
SET ORDER TO (o_sav)
IF BETWEEN(n_rec, 1, RECCOUNT())
  GO n_rec
ELSE
  IF BETWEEN(r_sav, 1, RECCOUNT())
    GO r_sav
  ENDIF
ENDIF
SELECT (s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� GetMask      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                   �〓� ��Б���� ��牀！� か� ���瓷�.                    �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 22.01.2000 陳�
PROCEDURE GetMask
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
*���瓷 ��牀！� �� ��爲�牀→�
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� �����皀 ��゛�� か� ���瓷� 9999999999999999 �
*�                                            �
*�      < OK Ctrl-W > < �皖���碎瘴 Esc >      �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, sh
ex = 1
sh = SPACE(16)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 6, 48, "���瓷 ��牀！� �� ��爲�牀→�"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@ 2, 3 SAY "�����皀 ��゛�� か� ���瓷�" GET sh
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN IIF(ex = 1, ALLTRIM(sh), "")

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Prp_Mask     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                             ���瓷 ��牀｀�.                             �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 22.01.2000 陳�
PROCEDURE Prp_Mask

PRIVATE s

SELECT 0
DELETE FILE (tmpo_path+"LNK_0122.DBF")
CREATE DBF (tmpo_path+"LNK_0122") ( N_REF N(10))

shab = SYS(15, lwr, shab)

SELECT LD_0120
SCAN
  s = IIF(EMPTY(LD_0120.MASK),  ;
        ALLTRIM(LD_0120.OFFICE)+ALLTRIM(STR(LD_0120.BOXNO)),ALLTRIM(LD_0120.MASK))
  s = SYS(15, lwr, s)
  IF shab $ s
    SELECT LNK_0122
    APPEND BLANK
    REPLACE N_REF WITH RECNO("LD_0120")
    SELECT LD_0120
  ENDIF
ENDSCAN

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� ViewMask     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                       蹍甃�矗 � �襦�� ��牀｀�.                        �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 22.01.2000 陳�
PROCEDURE ViewMask

IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� ���ぅ�� �� �き�� ��牀！�..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  DELETE FILE (tmpo_path+"LNK_0122.DBF")
  RETURN
ENDIF

PRIVATE tmpRet
tmpRet = 0
  
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

SELECT LD_0120
SET ORDER TO
SELECT LNK_0122
SET RELATION TO N_REF INTO LD_0120
*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�襦��" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Cont OF (menu_name) PROMPT "��ぅ爨━�� ��牀！�" KEY Ctrl-A, "F2"
ON SELECTION PAD Cont OF (menu_name) DO Sw_Mode WITH "ViewBox"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = WROWS("")-5   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 75
*
*敖�N �珮��陳���� �珮��陳�筥當陳�N �.���つ��陳ｦ┐�モ瘴������き�鐵陳�踸ガ��朕
*�XXXXXXXXXX ��.��.���� XXXXXXXX XXXX XXXX C ��.��.���� XXXXXXXXXX ��.��.�����
*青陳陳陳陳��爲�牀→� ��牀！� XXXXXXXXXX陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
DO D_Wins WITH ln, wd, "�ムョキ� ��牀｀�", 0, 0
@ 1, 2 SAY ;
"陳N �珮��陳���� �珮��陳�筥當陳�N �.���つ��陳ｦ┐�モ瘴������き�鐵陳�踸ガ��"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F2       DO Sw_Mode WITH "ViewBox"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS LD_0120.LD_NUM:H="",  ;
                  LD_0120.LD_DATE:H="", ;
                  LD_0120.OFFICE:H="",  ;
                  LD_0120.BOXNO:H="",   ;
                  LD_0120.BOX_YEAR:H="",;
                  LD_0120.BOXNO:H="",   ;
                  URGENT = IIF(EMPTY(LT_0120.QUICKLY), " ", "�"):H="", ;
                  LT_0120.LEADTIME:H="",;
                  IT_0120.EXP_NUM:H="", ;
                  IT_0120.DATE_INC:H="",;
           DUMMY.F:H="" FREEZE DUMMY.F  ;
           WHEN Draw_Foot()    ;
           NOMODIFY ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")

    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && �襦��

    tmpRet = LNK_0122.N_REF
    EXIT

  CASE what_do = "ViewBox"     && ��ぅ爨━�� ��牀！�

    SELECT LD_0120
    DO ViewBox
    SELECT LNK_0122
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
DELETE FILE (tmpo_path+"LNK_0122.DBF")

RETURN tmpRet

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� SeekPos      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           ���瓷 �� ��Ж罔�.                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 22.01.2000 陳�
PROCEDURE SeekPos
PRIVATE shab, n_rec, s_sav, r_sav, o_sav

r_sav = RECNO()
o_sav = ORDER()
s_sav = SELECT()
SET ORDER TO TAG BOXNO
shab = GetPos()
IF EMPTY(shab)
  RETURN
ENDIF

DO Prp_Pos

n_rec = ViewPos()
SELECT LD_0120
IF BETWEEN(n_rec, 1, RECCOUNT())
  GO n_rec
ENDIF
SELECT (s_sav)
SET ORDER TO (o_sav)
IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� GetPos      ��о�｀砒┴ ��むォ ��瓱�                       �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                    �〓� 皀�痰� か� ���瓷� ��Ж罔�.                     �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 22.01.2000 陳�
PROCEDURE GetPos
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

*���瓷 ��Ж罔�
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*� �����皀 ��゛�� か� ���瓷� 9999999999999999999999999 �
*�                                                     �
*�          < OK Ctrl-W > < �皖���碎瘴 Esc >           �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex, sh
ex = 1
sh = SPACE(25)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 6, 57, "���瓷 ��Ж罔�"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@ 2, 3 SAY "�����皀 ��゛�� か� ���瓷�" GET sh
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN IIF(ex = 1, ALLTRIM(sh), "")

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Prp_pos      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                       ��ぃ�皰→� 甎�瓷� ��Ж罔�.                       �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 22.01.2000 陳�
PROCEDURE Prp_pos

DO Wt_Mess WITH "���瓷..."
DELETE FILE (tmpo_path+"TMP_LIST.DBF")
DELETE FILE (tmpo_path+"TMP_LIST.CDX")
CREATE DBF (tmpo_path+"TMP_LIST") ;
  (  PREFIX   C( 4), ;
     NAME     C(35), ;
     PRODUCER C( 8), ;
     BOXNO    N( 7), ;
     OFFICE   C( 8), ;
     BOXYEAR  N( 4), ;
     REC_NO   N(10)  )
INDEX ON NAME+PREFIX+PRODUCER+STR(BOXNO)+OFFICE+STR(BOXYEAR) TAG TMP
cnt = 0
SELECT 0
USE (path_comm+"PURBASE") ALIAS ST_0122 AGAIN ORDER TAG IDP
SELECT 0
USE (path_comm+"ONHAND") ALIAS ONH_0122 AGAIN
SET RELATION TO PURSYS+STR(IDP) INTO ST_0122

SCAN
  cnt = cnt+1
*  @ WROW()-1, 0 SAY STR(cnt)
  IF .NOT. shab $ ST_0122.EQVPREF+ST_0122.EQVNAME+ST_0122.EQVPROD
    LOOP
  ENDIF
  IF SEEK(ST_0122.EQVNAME+ST_0122.EQVPREF+ST_0122.EQVPROD+ ;
          +STR(BOXNO)+BOXOFFIC+STR(BOXYEAR), "TMP_LIST")
    LOOP
  ENDIF
  IF .NOT. SEEK(ONH_0122.BOXOFFIC+STR(ONH_0122.BOXYEAR,4)+  ;
                STR(ONH_0122.BOXNO), "LD_0120")
    LOOP
  ENDIF
  SELECT TMP_LIST
  APPEND BLANK
  REPLACE PREFIX   WITH ST_0122.EQVPREF,  ;
          NAME     WITH ST_0122.EQVNAME,  ;
          PRODUCER WITH ST_0122.EQVPROD,  ;
          BOXNO    WITH ONH_0122.BOXNO,   ;
          OFFICE   WITH ONH_0122.BOXOFFIC,;
          BOXYEAR  WITH ONH_0122.BOXYEAR, ;
          REC_NO   WITH RECNO("LD_0120")
  SELECT ONH_0122
ENDSCAN
DO Wt_Mess
USE
SELECT ST_0122
USE
SELECT TMP_LIST

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� ViewPos      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      蹍甃�矗 �皰÷���諷 ��Ж罔�.                      �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 24.01.2000 陳�
PROCEDURE ViewPos

PRIVATE retVal, k00
retVal = 0
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��腑�� �� 磴���瘡 ���皋..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  DELETE FILE (tmpo_path+"TMP_LIST.DBF")
  DELETE FILE (tmpo_path+"TMP_LIST.CDX")
  RETURN retVal
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
SELECT TMP_LIST
*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�襦��碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "�皖���碎瘴" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = MIN(WROWS("")-8,RECCOUNT())   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 61
*敖��Ж罔鐵陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳N ���.��筥當陳陳��つ�
*�XXXX xxxxxxxxxXxxxxxxxxxXxxxxx xxxxxxxx NNNNNNN XXXXXXXX �����
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

DO D_Wins WITH ln, wd, "���甌� �皰÷���諷 ��Ж罔�", 0, 0
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
    BROWSE FIELDS PREFIX:H="",   ;
                  NAME:H="",     ;
                  PRODUCER:H="", ;
                  BOXNO:H="",    ;
                  OFFICE:H="",   ;
                  BOXYEAR:H="",  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && �襦��

    retVal = REC_NO
    EXIT

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
DELETE FILE (tmpo_path+"TMP_LIST.DBF")
DELETE FILE (tmpo_path+"TMP_LIST.CDX")

SELECT (s_sav)

RETURN retVal


*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Get_Wght     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                         �痰���→� ▲�� �珮��.                          �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 21.04.2000 陳�
PROCEDURE Get_Wght

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� 敖 �メ 陳陳陳陳陳陳陳陳陳陳陳� �
*� � 踳あ. 99999  ��膈覃 999.9 � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�< OK Ctrl-W > < �皖���碎瘴 Esc >�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, tmpWeight
ex = 1
tmpWeight = BX_0120.WEIGHT_ST
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 7, 36, ALLTRIM(LD_0120.OFFICE)+ALLTRIM(STR(LD_0120.BOXNO))
DO Sun_Bord WITH  2,  3,  4, 32, " �メ "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@ 3,  5 SAY "踳あ." GET BX_0120.WEIGHT PICTURE "@Z 999.9" WHEN .F.
@ 3, 19 SAY "��膈覃" GET tmpWeight PICTURE "@Z 999.9"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

IF ex = 1
  *
  * �矗���硅��ガ ゛���
  REPLACE BX_0120.WEIGHT_ST WITH tmpWeight
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
