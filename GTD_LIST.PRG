*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� GTD_List     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                         �襦�� ├ �璢│� ���.                           �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 09/04/2002 様�
PROCEDURE GTD_List
PARAMETER s_Value
PRIVATE r_value

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
r_value = ""
IF TYPE("s_value") = "C"
  IF .NOT. EMPTY(s_value)
    r_value = s_value
  ENDIF
ENDIF


s_sav = SELECT()

SELECT 0
DO Use_Dummy

SELECT 0
USE (path_comm+"GTD") ORDER TAG GTD_NUM ALIAS DET_0303 AGAIN

SELECT 0
USE (path_comm+"GTD_C") ORDER TAG GTD_NUM ALIAS CLS_0303 AGAIN

SELECT 0
USE (path_comm+"GTD_TIT") ALIAS TIT_0302 AGAIN ORDER TAG GTD_NUM
SET RELATION TO GTD_NUM INTO DET_0303
SET RELATION TO GTD_NUM INTO CLS_0303 ADDITIVE
IF .NOT. EMPTY(r_value)
  IF .NOT. SEEK(r_value)
    SET ORDER TO TAG HRON
    GO TOP
    r_value = ""
    GO TOP
  ENDIF
ELSE
  SET ORDER TO TAG HRON
  GO TOP
ENDIF
SET ORDER TO TAG HRON
IF EMPTY(r_value)
  r_value = SPACE(FSIZE("GTD_NUM"))
ENDIF

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�襦��" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Modify OF (menu_name) PROMPT "��ぅ爨━��"
ON PAD Modify OF (menu_name) ACTIVATE POPUP Modify

  DEFINE POPUP Modify MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Modify PROMPT "��Ж罔� ���" KEY Ctrl-A, "Tab"
  ON SELECTION BAR 1 OF Modify DO Sw_Mode WITH "Modify"

  DEFINE BAR  2 OF Modify PROMPT "��玻ユ�� ���瘁��" KEY Ctrl-A, "Shift-Tab"
  ON SELECTION BAR 2 OF Modify DO Sw_Mode WITH "Classes"

DEFINE PAD Print OF (menu_name) PROMPT "�ョ�碎" KEY Ctrl-A, "Ctrl-P"
ON SELECTION PAD Print OF (menu_name) DO Sw_Mode WITH "Print"

DEFINE PAD Seek OF (menu_name) PROMPT "���瓷"
ON PAD Seek OF (menu_name) ACTIVATE POPUP Seek

  DEFINE POPUP Seek MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Seek PROMPT "���瓷 ���" KEY Ctrl-A, "F7"
  ON SELECTION BAR 1 OF Seek DO Sw_Mode WITH "Seek"

  DEFINE BAR  2 OF Seek PROMPT "��≒�� ���瓷�" KEY Ctrl-A, "Ctrl-Enter"
  ON SELECTION BAR 2 OF Seek DO Sw_Mode WITH "Again"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 43

*敖� ���� 陳陳 ���ム 陳陳陳陳陳陳陳陳陳陳�����
*���.��.���� NNNNNNNNNNNNNNNNNNNNNNNNNN X X X�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

DO D_Wins WITH ln, wd, "�ムョキ� ���", 0, 0
@ 1, 2 SAY "陳 ���� 陳陳 ���ム 陳陳陳陳陳陳陳陳陳陳����"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter   KEYBOARD CHR(23)
    ON KEY LABEL Tab     DO Sw_Mode WITH "Modify"
    ON KEY LABEL BackTab DO Sw_Mode WITH "Classes"
    ON KEY LABEL Ctrl+P  DO Sw_Mode WITH "Print"
    ON KEY LABEL F7      DO Sw_Mode WITH "Seek"
    ON KEY LABEL Ctrl+F7 DO Sw_Mode WITH "Again"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS GTD_DATE:H="", GTD_NUM:H="", GTD_TYPE:H="", ;
           M001 = IIF(FOUND("DET_0303"), "�", " "):1:H= "", ;
           M002 = IIF(FOUND("CLS_0303"), "�", " "):1:H= "", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && �襦�� ���

    IF .NOT. BOF() .AND. .NOT. EOF()
      r_value = GTD_NUM
    ENDIF
    EXIT

  CASE what_do = "Modify"    && 蹍甃�矗 甌ぅ爨━��� ���

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO View_GTD
    ENDIF
    what_do = "List"

  CASE what_do = "Classes"    && 蹍甃�矗 甌ぅ爨━��� ���

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO View_Cls
    ENDIF
    what_do = "List"

  CASE what_do = "Print"    && �ョ�碎

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO P_GTD WITH GTD_NUM
    ENDIF
    what_do = "List"

  CASE what_do = "Seek"     && ���瓷

    DO SeekGTD
    what_do = "List"

  CASE what_do = "Again"    && ��≒��

    DO SeekGTD WITH .T.
    what_do = "List"

  CASE what_do = "Ins"       && ����←キ┘ ������→� ���

    DO Add_GTD
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
SELECT DET_0303
USE
SELECT CLS_0303
USE
SELECT (s_sav)

RETURN r_value

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Add_GTD      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                       ����←キ┘ ������→� ���.                        �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 03/02/2000 陳�
PROCEDURE Add_GTD

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
*�  ���ム ��� ....:....!....:....!....:. �
*�                                       �
*�  ���� ���  ��.��.���� (F1)            �
*�                                       �
*�  [ ] ���Ξ�                           �
*�                                       �
*�   < OK Ctrl-W > < �皖���碎瘴 Esc >    �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex, tmpNum, tmpDate, tmpFlg
tmpNum  = SPACE(26)
tmpDate = DATE()
tmpFlg  = .F.
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 10, 34, "����←キ┘ ���"

@ 2, 4 SAY "���ム ���"
@ 4, 4 SAY "���� ���             (F1)"
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
ON KEY LABEL F1 DO Set_Dt
@ 2, 14 GET tmpNum
@ 4, 14 GET tmpDate
@ 6,  4 GET tmpFlg PICTURE "@*C ���Ξ�"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Tst_GTD()
ON KEY LABEL F1

IF ex = 1
  *
  * �矗���硅��ガ ゛���
  APPEND BLANK
  REPLACE GTD_NUM  WITH tmpNum,  ;
          GTD_DATE WITH tmpDate, ;
          GTD_TYPE WITH IIF(tmpFlg, "�", " ")
  
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Set_Dt       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      �痰���→� ��硅 �� ���キ��瑟.                      �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 02.03.2000 陳�
PROCEDURE Set_Dt

tmpDate = Get_Date(tmpDate)
SHOW GET tmpDate

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Tst_GTD      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                       蹍▲爲� ��玻オ皚�痰� ���.                       �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 03/02/2000 陳�
PROCEDURE Tst_GTD

PRIVATE mss, r_sav, o_sav, sw

IF ex = 2
  RETURN .T.
ENDIF

IF EMPTY(tmpNum)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���ム ��� ぎ�Ε� °碎! �� 絎碎 �����-�─磴�..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN 1
ENDIF

r_sav = RECNO()
o_sav = TAG()

SET ORDER TO GTD_NUM
sw =  SEEK(tmpNum)
SET ORDER TO (o_sav)
IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF
IF sw
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��� � ���━ ���ム�� 礒� 痺薀痰≪モ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN 1
ENDIF

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� View_GTD     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                        蹍甃�矗 甌ぅ爨━��� ���.                       �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 03/03/2000 陳�
PROCEDURE View_GTD

DO Prep_Tmp
DO Modi_Tmp
SELECT GTD_TMP
USE
DELETE FILE (tmpo_path+"GTD_TMP.DBF")
DELETE FILE (tmpo_path+"GTD_TMP.CDX")

SELECT TIT_0302

RETURN


*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Prep_Tmp     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      ��ぃ�皰→� ∇ガキ���� �����.                      �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 03/03/2000 陳�
PROCEDURE Prep_Tmp

DELETE FILE (tmpo_path+"GTD_TMP.DBF")
DELETE FILE (tmpo_path+"GTD_TMP.CDX")

SELECT 0
USE (path_comm+"GTD") ORDER TAG GTD_NUM ALIAS GTD_0303 AGAIN

COPY STRUCTURE FIELDS NAME, COUNTRY, GTD_LINE, QNT TO (tmpo_path+"GTD_TMP")
CREATE DBF (tmpo_path+"GTD_TMP") ;
  ( MARK C(1), NAME C(29), COUNTRY C(16), GTD_LINE C( 4), QNT N( 8) )

USE (tmpo_path+"GTD_TMP") EXCLUSIVE

SELECT GTD_0303
SEEK TIT_0302.GTD_NUM
SCAN REST WHILE GTD_NUM = TIT_0302.GTD_NUM
  SELECT GTD_TMP
  APPEND BLANK
  REPLACE NAME     WITH GTD_0303.NAME,    ;
          COUNTRY  WITH GTD_0303.COUNTRY, ;
          GTD_LINE WITH GTD_0303.GTD_LINE,;
          QNT      WITH GTD_0303.QNT
  SELECT GTD_0303
ENDSCAN

USE
SELECT GTD_TMP
INDEX ON NAME TAG NAME

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Modi_Tmp     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                          ��玻オ罔� ����皰→�.                          �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 03.03.2000 陳�
PROCEDURE Modi_Tmp


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
SELECT GTD_TMP

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE WINDOW (win_name+"_H") FROM 0, 0 TO 0, WCOLS("") NONE COLOR SCHEME 13
ACTIVATE WINDOW (win_name+"_H") NOSHOW
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
*敖���━キ����┘陳陳陳陳陳陳陳陳陳�矗���陳陳陳陳陳N 痰狡����-〓朕
*�M ....:....!....:....!....:.... ....:....!....:. .... ....:...�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�        ��━キ����┘ ....:....!....:....!....:....            �
*�                                                              �
*� [ ] �矗��� ....:....!....:.  �矗��� N 9999  ���-〓 99999999  �
*�                                                              �
*�              < OK Ctrl-W > < �皖���碎瘴 Esc >                �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
PRIVATE ln, wd
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 62

DO D_Wins WITH ln, wd, "��� N "+ALLTRIM(TIT_0302.GTD_NUM)+" �� "+DTOC(TIT_0302.GTD_DATE), 0, 5
@ 1, 2 SAY "陳��━キ����┘陳陳陳陳陳陳陳陳陳�矗���陳陳陳陳陳N 痰狡����-〓"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1

    BROWSE FIELDS MARK:H="",  ;
                  NAME:H="",  ;
                  COUNTRY:H="", ;
                  GTD_LINE:H="",;
                  QNT:H="":P="@Z 99999999", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")

*
*    BROWSE - �キ�
*
    ON KEY
    EXIT

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE WINDOW (win_name+"_H")

RELEASE MENU (menu_name) EXTENDED
*
*   ���窶�皚キ讓� (���窶�皚キ讓�!) ���琺��ガ DBF-�����.
*
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
@ WROWS()-6,  2 CLEAR TO WROWS()-2, WCOLS()-3
@ WROWS()-6, 10 SAY "��━キ����┘ " + NAME
@ WROWS()-4,  7 SAY "�矗��� " + COUNTRY
@ WROWS()-4, 32 SAY "�矗��� N " + GTD_LINE
@ WROWS()-4, 47 SAY "���-〓 " + STR(QNT,8)

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� SeekGTD      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                         ���皀�痰�覃 ���瓷 ���                          �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 12.03.2002 陳�
PROCEDURE SeekGTD
PARAMETERS repeat

PRIVATE tmpShab, o_sav, r_sav

SELECT TIT_0302
r_sav = RECNO()

IF .NOT. repeat
  tmpShab = Get_Shab()
  IF EMPTY(tmpShab)
    RETURN
  ENDIF
  tmpShab = UPPER(tmpShab)
  SELECT 0
  DELETE FILE (tmpo_path+"SeekGTD.DBF")
  CREATE DBF (tmpo_path+"SeekGTD.DBF") ( LINK N(10) )
  SELECT TIT_0302
  SCAN FOR tmpShab $ UPPER(GTD_NUM)
    SELECT SeekGTD
    APPEND BLANK
    REPLACE LINK WITH RECNO("TIT_0302")
    SELECT TIT_0302
  ENDSCAN
  SELECT SeekGTD
  USE
  SELECT TIT_0302
  IF BETWEEN(r_sav, 1, RECCOUNT())
    GO r_sav
  ENDIF
ENDIF

IF .NOT. File_O(tmpo_path+"SeekGTD.DBF")
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��腑�� �� ���ぅ��..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF
o_sav = ORDER()
SET ORDER TO
SELECT 0
USE (tmpo_path+"SeekGTD")
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��腑�� �� ���ぅ��..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT TIT_0302
  SET ORDER TO TAG (o_sav)
  RETURN  
ENDIF

SET RELATION TO LINK INTO TIT_0302

tmpShab = Ch_Link()

USE
SELECT TIT_0302
SET ORDER TO TAG (o_sav)

IF BETWEEN(tmpShab, 1, RECCOUNT())
  GO tmpShab
ELSE
  IF BETWEEN(r_sav, 1, RECCOUNT())
    GO r_sav
  ENDIF
ENDIF

RETURN
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Ch_Link      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                          �襦�� ├ �皰÷������                          �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 12.03.2002 陳�
PROCEDURE Ch_Link

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

PRIVATE tmpRet
tmpRet = 0

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
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 39

*敖� ���� 陳陳 ���ム 陳陳陳陳陳陳陳陳陳陳�
*���.��.���� NNNNNNNNNNNNNNNNNNNNNNNNNN X�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

DO D_Wins WITH ln, wd, "���ぅ�� ���...", 0, 0
@ 1, 2 SAY "陳 ���� 陳陳 ���ム "
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
    BROWSE FIELDS TIT_0302.GTD_DATE:H="", TIT_0302.GTD_NUM:H="", TIT_0302.GTD_TYPE:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && �襦��

    tmpRet = LINK
    EXIT

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

RETURN tmpRet

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� View_Cls     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                 蹍甃�矗 甌ぅ爨━��� ��� � ��痰� ���瘁��.              �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 03/03/2000 陳�
PROCEDURE View_Cls

DO Prep_Cls
DO Corr_Cls
SELECT GTD_TMP
USE
DELETE FILE (tmpo_path+"GTD_TMP.DBF")
DELETE FILE (tmpo_path+"GTD_TMP.CDX")

SELECT TIT_0302

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Prep_Cls     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                  ��ぃ�皰→� ∇ガキ���� ����� ���瘁��.                  �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 03/03/2000 陳�
PROCEDURE Prep_Cls

DELETE FILE (tmpo_path+"GTD_TMP.DBF")
DELETE FILE (tmpo_path+"GTD_TMP.CDX")

SELECT 0
USE (path_comm+"GTD_C") ORDER TAG GTD_NUM ALIAS GTD_0303 AGAIN

COPY STRUCTURE FIELDS NAME, COUNTRY, GTD_LINE, QNT TO (tmpo_path+"GTD_TMP")
CREATE DBF (tmpo_path+"GTD_TMP") ;
  ( MARK C(1), CLASS C(12), COUNTRY C(16), GTD_LINE C( 4), QNT N( 8) )

USE (tmpo_path+"GTD_TMP") EXCLUSIVE

SELECT GTD_0303
SEEK TIT_0302.GTD_NUM
SCAN REST WHILE GTD_NUM = TIT_0302.GTD_NUM
  SELECT GTD_TMP
  APPEND BLANK
  REPLACE CLASS    WITH GTD_0303.CLASS,   ;
          COUNTRY  WITH GTD_0303.COUNTRY, ;
          GTD_LINE WITH GTD_0303.GTD_LINE,;
          QNT      WITH GTD_0303.QNT
  SELECT GTD_0303
ENDSCAN

USE
SELECT GTD_TMP
INDEX ON CLASS TAG NAME
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Corr_Cls     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�              ��玻オ罔� ����皰→� � ��痰� ���瘁��.                      �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 03.03.2000 陳�
PROCEDURE Corr_Cls


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
USE (base_path+"CLASSES") SHARED AGAIN ALIAS CCC_0303 ORDER TAG CODE
DO Use_Dummy
SELECT GTD_TMP
SET RELATION TO CLASS INTO CCC_0303

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE WINDOW (win_name+"_H") FROM 0, 0 TO 0, WCOLS("") NONE COLOR SCHEME 13
ACTIVATE WINDOW (win_name+"_H") NOSHOW
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
*敖���━キ����┘陳陳陳陳陳陳陳陳陳�矗���陳陳陳陳陳N 痰狡����-〓朕
*�M ....:....!....:....!....:.... ....:....!....:. .... ....:...�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� ....:....!....:....!....:....!....:....!....:....!....:....! �
*�                                                              �
*� [ ] �矗��� ....:....!....:.  �矗��� N 9999  ���-〓 99999999  �
*�                                                              �
*�              < OK Ctrl-W > < �皖���碎瘴 Esc >                �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
PRIVATE ln, wd
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 62

DO D_Wins WITH ln, wd, "��� N "+ALLTRIM(TIT_0302.GTD_NUM)+" �� "+DTOC(TIT_0302.GTD_DATE), 0, 5
@ 1, 2 SAY "陳���瘁陳陳陳陳陳陳陳陳陳陳陳陳��矗���陳陳陳陳陳N 痰狡����-〓"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1

    BROWSE FIELDS MARK:H="",  ;
                  CCC_0303.NAME:29:H="",  ;
                  COUNTRY:H="", ;
                  GTD_LINE:H="",;
                  QNT:H="":P="@Z 99999999", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Dr_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")

*
*    BROWSE - �キ�
*
    ON KEY
    EXIT

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE WINDOW (win_name+"_H")

RELEASE MENU (menu_name) EXTENDED
*
*   ���窶�皚キ讓� (���窶�皚キ讓�!) ���琺��ガ DBF-�����.
*
SELECT CCC_0303
USE
SELECT (s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Dr_Foot      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                   ��痺ガ ぎ�����皀�讚竡 ┃筮爼�罔�.                    �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 20.10.1998 陳�
PROCEDURE Dr_Foot

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� Гメ�, 爬痺ガ...
*
@ WROWS()-6,  2 CLEAR TO WROWS()-2, WCOLS()-3
@ WROWS()-6,  3 SAY LEFT(CCC_0303.NAME, 60)
@ WROWS()-4,  7 SAY "�矗��� " + COUNTRY
@ WROWS()-4, 32 SAY "�矗��� N " + GTD_LINE
@ WROWS()-4, 47 SAY "���-〓 " + STR(QNT,8)

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.
