*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� View_Ch      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                            蹍甃�矗 ���キ.                             �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 12.09.2001 様�
PROCEDURE View_Ch
PARAMETERS tmpList
WAIT WIND "!!!!"
PRIVATE s_sav, f1_name, f2_name, lstName
PRIVATE show_dat
show_dat = .T.
lstName = ""
f1_name = SYS(3)
f2_name = SYS(3)
s_sav   = SELECT()
SELECT 0

IF .NOT. Prep_Tmp()
  SELECT (s_sav)
  RETURN
ENDIF
SELECT TMP_CHANGE
INDEX ON STR(S_CODE)+NAME+PREFIX+PRODUCER TAG CODE

SELECT 0
USE (base_path+"STOCK") AGAIN ALIAS STO_1913 ORDER TAG CODE
SELECT TMP_CHANGE
SET RELATION TO CODE INTO STO_1913
SELECT TEMP_LST
SET FILTER TO CODE # 0 .AND. (FOUND("TMP_CHANGE") .OR. STO_1912.QNT > 0 .OR. .NOT. EMPTY(WAIT))
GO TOP
DO View1

SELECT STO_1912
USE
SELECT STO_1913
USE
SELECT TEMP_LST
USE
SELECT TMP_CHANGE
USE
SELECT LTIT_1912
USE
SELECT LST_1912
USE
DELETE FILE (tmpo_path+f1_name+".dbf")
DELETE FILE (tmpo_path+f2_name+".dbf")
DELETE FILE (tmpo_path+f1_name+".cdx")
DELETE FILE (tmpo_path+f2_name+".cdx")

SELECT (s_sav)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Prep_Tmp     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      ��ぃ�皰→� ∇ガキ���� �����.                      �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 12.09.2001 陳�
PROCEDURE Prep_Tmp

PRIVATE tmpArr, j, dt_wait, tmpQnt, ch_flags
ch_flags = "!!!"
DIMENSION tmpArr(1,3)
SELECT 0
CREATE DBF (tmpo_path+f1_name) ;
  ( CODE     N( 7), ;
    LINK     N(10), ;
    PREFIX   C( 4), ;
    NAME     C(35), ;
    PRODUCER C( 8), ;
    QNT      N( 6), ;
    COR_CODE N( 6), ;
    CORPUS   C(16), ;
    WAIT     D,     ;
    NOTE     C(10)  )
USE (tmpo_path+f1_name) EXCLUSIVE ALIAS TEMP_LST

SELECT 0
CREATE DBF (tmpo_path+f2_name) ;
  ( S_CODE   N( 7), ;
    CODE     N( 7), ;
    PREFIX   C( 4), ;
    NAME     C(35), ;
    PRODUCER C( 8), ;
    QNT      N( 6), ;
    CH_TYPE  C( 1), ;
    COR_CODE N( 6), ;
    CORPUS   C(16), ;
    WAIT     D,     ;
    NOTE     C(10)  )
USE (tmpo_path+f2_name) EXCLUSIVE ALIAS TMP_CHANGE

SELECT 0
USE (path_comm+"PURCHASE") AGAIN ALIAS PCH_1912 ORDER TAG NAME_DATE
SET FILTER TO PURFLAG = "K"
SELECT 0
USE (path_comm+"CORPUS") AGAIN ALIAS COR_1912 ORDER TAG CODE
SELECT 0
USE (path_comm+"SUBJECT") AGAIN ALIAS SUBJ_1912 ORDER TAG STO_CODE
SET RELATION TO COR_CODE INTO COR_1912
SELECT 0
USE (base_path+"STOCK") AGAIN ALIAS STO_1912 ORDER TAG CODE
SELECT 0
USE (base_path+"LIST_TIT") AGAIN ALIAS LTIT_1912 ORDER TAG LST_NUM
IF .NOT. SEEK(tmpList)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���甌� �� ���ぅ�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT TEMP_LST
  USE
  SELECT TMP_CHANGE
  USE
  SELECT COR_1912
  USE
  SELECT SUBJ_1912
  USE
  SELECT STO_1912
  USE
  SELECT PCH_1912
  USE
  DELETE FILE (tmpo_path+f1_name+".dbf")
  DELETE FILE (tmpo_path+f2_name+".dbf")
  RETURN .F.
ENDIF
IF .NOT. EMPTY(SAL_NUM)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���甌� 礒� �牀���!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT TEMP_LST
  USE
  SELECT TMP_CHANGE
  USE
  SELECT COR_1912
  USE
  SELECT LTIT_1912
  USE
  SELECT SUBJ_1912
  USE
  SELECT STO_1912
  USE
  SELECT PCH_1912
  USE
  DELETE FILE (tmpo_path+f1_name+".dbf")
  DELETE FILE (tmpo_path+f2_name+".dbf")
  RETURN .F.
ENDIF
DO Get_Flags
DO Wt_Mess WITH "               ���瓷 ���キ               "
lstName = LST_NAME
SELECT 0
USE (base_path+"LIST_DET") AGAIN ALIAS LST_1912 ORDER TAG L_NAME
SET RELATION TO CODE INTO STO_1912
SET RELATION TO CODE INTO SUBJ_1912 ADDITIVE
SEEK STR(tmpList,6)
SCAN REST WHILE STR(tmpList,6) = LIST_NUM
  IF LST_1912.QNT_REQ # 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"���甌� 甌ぅ爨�� ÷���! ｡��｀皖� ���爛薀��!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    USE
    SELECT COR_1912
    USE
    SELECT SUBJ_1912
    USE
    SELECT STO_1912
    USE
    SELECT PCH_1912
    USE
    SELECT TEMP_LST
    USE
    SELECT LTIT_1912
    USE
    SELECT TMP_CHANGE
    USE
    DELETE FILE (tmpo_path+f1_name+".dbf")
    DELETE FILE (tmpo_path+f2_name+".dbf")
    DO Wt_Mess
    RETURN .F.
  ENDIF
  SELECT TEMP_LST
  @ WROWS()-1, 1 SAY STO_1912.PREFIX+" "+STO_1912.NAME+" "+STO_1912.PRODUCER
  IF LST_1912.QNT > STO_1912.QNT
    dt_wait = WhenCome(STO_1912.NAME+STO_1912.PREFIX+STO_1912.PRODUCER)
  ELSE
    dt_wait = {}
  ENDIF
  APPEND BLANK
  REPLACE CODE     WITH LST_1912.CODE,     ;
          LINK     WITH RECNO("LST_1912"), ;
          PREFIX   WITH STO_1912.PREFIX,   ;
          NAME     WITH STO_1912.NAME,     ;
          PRODUCER WITH STO_1912.PRODUCER, ;
          QNT      WITH LST_1912.QNT,      ;
          WAIT     WITH dt_wait,           ;
          COR_CODE WITH SUBJ_1912.COR_CODE,;
          CORPUS   WITH COR_1912.NAME,     ;
          NOTE     WITH SUBJ_1912.NODISC
*  tmpQnt = MAX(QNT-STO_1912.QNT, 0)
  tmpQnt = QNT
  IF .NOT. EMPTY(CODE)
    IF Changes1(CODE, tmpArr, ch_flags)
      SELECT TMP_CHANGE
      FOR j = 1 TO ALEN(tmpArr, 1)
        = SEEK(tmpArr(j, 1), "STO_1912")
        IF tmpQnt > STO_1912.QNT
          dt_wait = WhenCome(tmpArr(j, 1))
        ELSE
          dt_wait = {}
        ENDIF
        IF EMPTY(STO_1912.QNT) .AND. (EMPTY(dt_wait) .OR. .NOT. show_dat)
          LOOP
        ENDIF
        IF tmpArr(j, 4) < 0 && �珮��� ��牆竅
          LOOP
        ENDIF
        APPEND BLANK
        REPLACE S_CODE   WITH TEMP_LST.CODE,     ;
                CODE     WITH tmpArr(j, 1),      ;
                PREFIX   WITH STO_1912.PREFIX,   ;
                NAME     WITH STO_1912.NAME,     ;
                PRODUCER WITH STO_1912.PRODUCER, ;
                QNT      WITH tmpQnt,            ;
                CH_TYPE  WITH tmpArr(j, 2),      ;
                COR_CODE WITH ABS(tmpArr(j, 4)), ;
                WAIT     WITH dt_wait,           ;
                NOTE     WITH SUBJ_1912.NODISC
      ENDFOR
    ENDIF
  ENDIF
  SELECT LST_1912
ENDSCAN

DO Wt_Mess

SELECT SUBJ_1912
USE
SELECT PCH_1912
USE
SELECT TMP_CHANGE
SET RELATION TO COR_CODE INTO COR_1912
REPLACE ALL CORPUS WITH COR_1912.NAME
SELECT COR_1912
USE


RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� WhenCome     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                   ���� ��痰祚�キ�� ��Ж罔� �� 瓷���                    �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 13.09.2001 陳�
PROCEDURE WhenCome
PARAMETER posCode

PRIVATE tmpName, r_sav
IF TYPE("posCode") = "C"
  tmpName = posCode
ELSE
  r_sav = RECNO("STO_1912")
  IF SEEK(posCode, "STO_1912")
    tmpName = STO_1912.NAME+STO_1912.PREFIX+STO_1912.PRODUCER
  ELSE
    tmpName = "============================="
  ENDIF
  IF BETWEEN(r_sav, 1, RECCOUNT("STO_1912"))
    GO r_sav IN STO_1912
  ENDIF
ENDIF
IF SEEK(tmpName, "PCH_1912")
  RETURN PCH_1912.DATE
ENDIF

RETURN {}

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� View1        ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                             蹍甃�矗 ���キ                             �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 13.09.2001 陳�
PROCEDURE View1

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
PRIVATE f_dummy, listNo, l, c, l1

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
f_dummy = SYS(3)
COPY TO (tmpo_path+f_dummy)
SELECT 0
USE (tmpo_path+f_dummy) ALIAS DUMMY1

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Switch OF (menu_name) PROMPT "ｪ�� ���キ" KEY Ctrl-A, "Tab"
ON SELECTION PAD Switch OF (menu_name) DO Sw_Mode WITH "Switch"

DEFINE PAD Print OF (menu_name) PROMPT "�ョ�碎" KEY Ctrl-A, "Ctrl-P"
ON SELECTION PAD Print OF (menu_name) DO Sw_Mode WITH "Print"

DEFINE PAD Work OF (menu_name) PROMPT "｡��｀皖�" KEY Ctrl-A, "Shift-Tab"
ON SELECTION PAD Work OF (menu_name) DO Sw_Mode WITH "Work"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd, ln1, ln2
*��陳陳��━キ����┘陳陳陳陳陳陳陳陳陳陳陳陳����-〓������.陳���痰.陳朕
*�� pppp nnnnnnnnnNnnnnnnnnnNnnnnn ppppPppp 999999 999999 ��.��.�����
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
*PPPP nnnnnnnnnNnnnnnnnnnNnnnnn ppppPppp    ��牆竅 kkkkkkkkkKkkkkkk
*敖陳��━キ����┘陳陳陳陳陳陳陳陳陳陳陳陳����-〓������.陳���痰.陳陳��
*�pppp nnnnnnnnnNnnnnnnnnnNnnnnn ppppPppp 999999 999999 ��.��.���� ��
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
ln = WROWS("")-10   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 66
ln2 = CEILING(ln/3)
ln1 = ln-ln2
DO D_Wins WITH ln, wd, "�ムョキ� ���キ か� 甎�瓷� � "+ALLTRIM(STR(tmpList)), 0, ln2
@ 1, 2 SAY "�陳陳��━キ����┘陳陳陳陳陳陳陳陳陳陳陳陳����-〓������.陳���痰."
@ WROWS()-ln2-2, 0 SAY SPACE(WCOLS())
DO Sun_Bord WITH WROWS()-ln2-1, 1, WROWS()-1, WCOLS()-2
@ WROWS()-ln2-1, 2 SAY ;
"陳���━キ����┘陳陳陳陳陳陳陳陳陳陳陳陳����-〓������.陳���痰.陳陳�"

l = WROWS()-ln2+2 &&-2
c = WLCOL(win_name+"_E")
DEFINE WINDOW (win_name+"_E1") FROM l, c ;
                               TO   l+ln2-2, c+wd-1 NONE && COLOR SCHEME 13
DEFINE WINDOW (win_name+"_I1") FROM l-3, c-2 TO l+ln2-1, c+wd+20   ;
                           NONE COLOR SCHEME 13

what_do = "List"
listNo  = 1

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Tab     DO Sw_Mode WITH "Switch"
    ON KEY LABEL Ctrl+P  DO Sw_Mode WITH "Print"
    ON KEY LABEL BackTab DO Sw_Mode WITH "Work"
    SELECT TEMP_LST

*
*    BROWSE - �キ�
*

    IF listNo = 1
      ACTIVATE WINDOW (win_name+"_E1")
      SELECT TMP_CHANGE
      BROWSE FIELDS TMP_CHANGE.PREFIX:H="",    ;
                    TMP_CHANGE.NAME:H="",      ;
                    TMP_CHANGE.PRODUCER:H="",  ;
                    TEMP_LST.QNT:H="":P="999999",  ;
                    STO_QNT = MIN(STO_1913.QNT, TEMP_LST.QNT):H="":P="999999", ;
                    TMP_CHANGE.WAIT:H="",    ;
                    TMP_CHANGE.CH_TYPE:H="", ;
             DUMMY1.F:H="" FREEZE DUMMY1.F ;
             NOMODIFY   ;
             NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I1") IN WINDOW (win_name+"_E1") NOWAIT

      ACTIVATE WINDOW (win_name+"_E")
      SELECT TEMP_LST
      SET RELATION TO CODE INTO STO_1912
      SET RELATION TO STR(CODE) INTO TMP_CHANGE ADDITIVE
      BROWSE FIELDS IS_D = IIF(STO_1912.QNT < TEMP_LST.QNT, "�", " "):1:H="",  ;
                    TEMP_LST.PREFIX:H="",    ;
                    TEMP_LST.NAME:H="",      ;
                    TEMP_LST.PRODUCER:H="",  ;
                    TEMP_LST.QNT:H="":P="999999",  ;
                    STO_QNT = MIN(STO_1912.QNT, TEMP_LST.QNT):H="":P="999999", ;
                    TEMP_LST.WAIT:H="", ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             NOMODIFY   ;
             WHEN Draw_Foot()    ;
             NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ELSE
      ACTIVATE WINDOW (win_name+"_E")
      SELECT TEMP_LST
      SET RELATION TO CODE INTO STO_1912
      BROWSE FIELDS IS_D = IIF(STO_1912.QNT < TEMP_LST.QNT, "�", " "):1:H="",  ;
                    TEMP_LST.PREFIX:H="",    ;
                    TEMP_LST.NAME:H="",      ;
                    TEMP_LST.PRODUCER:H="",  ;
                    TEMP_LST.QNT:H="":P="999999",  ;
                    STO_QNT = MIN(STO_1912.QNT, TEMP_LST.QNT):H="":P="999999", ;
                    TEMP_LST.WAIT:H="", ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             NOMODIFY   ;
             WHEN Draw_Foot()    ;
             NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E") NOWAIT

      ACTIVATE WINDOW (win_name+"_E1")
      SELECT TMP_CHANGE
      BROWSE KEY STR(TEMP_LST.CODE)    ;
             FIELDS TMP_CHANGE.PREFIX:H="",    ;
                    TMP_CHANGE.NAME:H="",      ;
                    TMP_CHANGE.PRODUCER:H="",  ;
                    TEMP_LST.QNT:H="":P="999999",  ;
                    STO_QNT = MIN(STO_1913.QNT, TEMP_LST.QNT):H="":P="999999", ;
                    TMP_CHANGE.WAIT:H="",    ;
                    TMP_CHANGE.CH_TYPE:H="", ;
             DUMMY1.F:H="" FREEZE DUMMY1.F ;
             NOMODIFY   ;
             NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I1") IN WINDOW (win_name+"_E1")
    ENDIF
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "List")
    ENDIF

  CASE what_do = "Switch"    && �ムオ�鈑キ┘ ����

    IF listNo = 1
      IF FOUND("TMP_CHANGE")
        DEFINE PAD Switch OF (menu_name) PROMPT "ｪ�� 甎�瓷�" KEY Ctrl-A, "Tab"
        listNo = 2
      ENDIF
    ELSE
      DEFINE PAD Switch OF (menu_name) PROMPT "ｪ�� ���キ" KEY Ctrl-A, "Tab"
      listNo = 1
    ENDIF
    what_do = "List"

  CASE what_do = "Print"    && �ョ�碎

    DO P_Doc
    what_do = "List"

  CASE what_do = "Work"    && ｡��｀皖�

    IF Work_1()
      EXIT
    ELSE
      what_do = "List"
    ENDIF

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE WINDOW (win_name+"_E1")
RELEASE WINDOW (win_name+"_I1")
RELEASE MENU (menu_name) EXTENDED
*
*   ���窶�皚キ讓� (���窶�皚キ讓�!) ���琺��ガ DBF-�����.
*
SELECT DUMMY1
USE
DELETE FILE (tmpo_path+f_dummy+".dbf")
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
@ WROWS()-ln2-2, 0 SAY TEMP_LST.PREFIX+" "+TEMP_LST.NAME+" "+TEMP_LST.PRODUCER+ ;
  "    ��牆竅 "+TEMP_LST.CORPUS

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Work         ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                               ｡��｀皖�                                �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 14.09.2001 陳�
PROCEDURE Work

PARAMETERS rest1, new1, dem1, rest2, new2, dem2

PRIVATE qnt_0, qnt_ch, qnt_exist
PRIVATE list_0, list_ch
PRIVATE qnt_dem, qnt_list1, qnt_list2, qnt_rest

STORE 0 TO list_0, list_ch
SELECT 0
USE (base_path+"LIST_DET") SHARED AGAIN ALIAS NEW_LISTS
SELECT 0
USE (base_path+"DEMAND") SHARED AGAIN ALIAS DEM_1914
SELECT LST_1912
SET ORDER TO
SELECT TEMP_LST
SET FILTER TO
SET RELATION TO CODE INTO STO_1912
SET RELATION TO STR(CODE) INTO TMP_CHANGE ADDITIVE
SCAN
  *
  *  ��瓷��お� ����腑痰�� �� 皋��� �÷�｀皖�
  *
  STORE 0 TO qnt_0, qnt_ch, qnt_exist
  DO CASE
  CASE CODE = 0      && �メ���め��� ��Ж罔� - ≡� ����腑痰〓 � ぅ筥罔� ．� ���キ
    qnt_0 = QNT
  CASE .NOT. FOUND("TMP_CHANGE")  && �モ ���キ
    qnt_exist = MIN(QNT, STO_1912.QNT)
    qnt_0     = QNT-qnt_exist
  OTHERWISE                       && �痰� ���キ�
    qnt_exist = MIN(QNT, STO_1912.QNT)
    qnt_ch    = QNT-qnt_exist
  ENDCASE
  *
  *   ��瓷��る��ガ ���-�� �� �爛く������
  *
  STORE 0 TO qnt_dem, qnt_list1, qnt_list2, qnt_rest
  IF rest1     && �ヤ�罔� ．� ���キ �痰�※碎 � 甎�瓷�
    qnt_rest = qnt_rest+qnt_0
  ENDIF
  IF new1     && �ヤ�罔� ．� ���キ 祗メ皋 � ���覃 甎�甌�
    qnt_list1 = qnt_list1+qnt_0
  ENDIF
  IF dem1     && �ヤ�罔� ．� ���キ 祗メ皋 � 甎牀�
    qnt_dem = qnt_dem+qnt_0
  ENDIF
  IF rest2     && �ヤ�罔� � ���キ��� �痰�※碎 � 甎�瓷�
    qnt_rest = qnt_rest+qnt_ch
  ENDIF
  IF new2     && �ヤ�罔� � ���キ��� 祗メ皋 � ���覃 甎�甌�
    qnt_list2 = qnt_list2+qnt_ch
  ENDIF
  IF dem2     && �ヤ�罔� � ���キ��� 祗メ皋 � 甎牀�
    qnt_dem = qnt_dem+qnt_ch
  ENDIF
  qnt_rest = qnt_rest+qnt_exist   && ��, 艪� メ碎 �� 瓷��ぅ - ≡ィ�� �痰�←錺�!
  GO TEMP_LST.LINK IN LST_1912
  SELECT NEW_LISTS
  IF qnt_list1 # 0
    IF list_0 = 0
      list_0 = Doc_Num("LISTS", "", arm, DATE())
    ENDIF
    APPEND BLANK
    REPLACE LIST_NUM  WITH STR(list_0,6),		;
            CODE      WITH LST_1912.CODE,		;
            PREFIX    WITH LST_1912.PREFIX,		;
            NAME      WITH LST_1912.NAME,		;
            PRODUCER  WITH LST_1912.PRODUCER,	;
            AMODEL    WITH LST_1912.AMODEL,		;
            FUNCTION  WITH LST_1912.FUNCTION,	;
            ADDRESS   WITH LST_1912.ADDRESS,	;
            QNT       WITH qnt_list1,			;
            USD_DEF   WITH LST_1912.USD_DEF,	;
            DEF_PRICE WITH LST_1912.DEF_PRICE,	;
            USD_PRICE WITH LST_1912.USD_PRICE,	;
            SAL_PRICE WITH LST_1912.SAL_PRICE
  ENDIF
  IF qnt_list2 # 0
    IF list_ch = 0
      list_ch = Doc_Num("LISTS", "", arm, DATE())
    ENDIF
    SELECT NEW_LISTS
    APPEND BLANK
    REPLACE LIST_NUM  WITH STR(list_ch,6),		;
            CODE      WITH LST_1912.CODE,		;
            PREFIX    WITH LST_1912.PREFIX,		;
            NAME      WITH LST_1912.NAME,		;
            PRODUCER  WITH LST_1912.PRODUCER,	;
            AMODEL    WITH LST_1912.AMODEL,		;
            FUNCTION  WITH LST_1912.FUNCTION,	;
            ADDRESS   WITH LST_1912.ADDRESS,	;
            QNT       WITH qnt_list2,			;
            USD_DEF   WITH LST_1912.USD_DEF,	;
            DEF_PRICE WITH LST_1912.DEF_PRICE,	;
            USD_PRICE WITH LST_1912.USD_PRICE,	;
            SAL_PRICE WITH LST_1912.SAL_PRICE
  ENDIF
  IF qnt_dem # 0
    SELECT DEM_1914
    APPEND BLANK
    REPLACE STO_CODE  WITH LST_1912.CODE,		;
            PREFIX    WITH LST_1912.PREFIX,		;
            NAME      WITH LST_1912.NAME,		;
            PRODUCER  WITH LST_1912.PRODUCER,	;
            DATE      WITH DATE(),				;
            AMODEL    WITH LST_1912.AMODEL,		;
            FUNCTION  WITH LST_1912.FUNCTION,	;
            QTY       WITH qnt_dem,				;
            PRICE_D   WITH LST_1912.USD_PRICE,	;
            PRICE     WITH LST_1912.SAL_PRICE,	;
            CLI_CODE  WITH LTIT_1912.CLI_CODE,	;
            CLIENT    WITH LTIT_1912.LST_NAME,	;
            WHO       WITH user,				;
            WHERE     WITH arm
  ENDIF
  SELECT LST_1912
  REPLACE QNT WITH qnt_rest
  IF qnt_rest = 0
    DELETE
  ENDIF
  SELECT TEMP_LST
ENDSCAN

SELECT NEW_LISTS
USE (base_path+"LIST_TIT") AGAIN ALIAS NEW_TIT
IF list_0 # 0
  APPEND BLANK
  REPLACE LST_ARM   WITH arm,		;
          LST_NUM   WITH list_0,	;
          CR_DAT    WITH DATE(),	;
          LST_NAME  WITH LEFT(LTIT_1912.LST_NAME, LEN(LTIT_1912.LST_NAME)-7)+" ���. 0", ;
          LST_FLAGS WITH LTIT_1912.LST_FLAGS,	;
          IS_USD    WITH LTIT_1912.IS_USD,		;
          IS_CHECK  WITH LTIT_1912.IS_CHECK,	;
          CLI_CODE  WITH LTIT_1912.CLI_CODE,	;
          CLI_TYPE  WITH LTIT_1912.CLI_TYPE,	;
          FIRM      WITH LTIT_1912.FIRM,		;
          BANK      WITH LTIT_1912.BANK,		;
          WHO_CR    WITH user,					;
          WHERE_CR  WITH arm,					;
          ORD_QUEUE WITH LTIT_1912.ORD_QUEUE
ENDIF
IF list_ch # 0
  APPEND BLANK
  REPLACE LST_ARM   WITH arm,		;
          LST_NUM   WITH list_ch,	;
          CR_DAT    WITH DATE(),	;
          LST_NAME  WITH LEFT(LTIT_1912.LST_NAME, LEN(LTIT_1912.LST_NAME)-7)+" ���キ�", ;
          LST_FLAGS WITH LTIT_1912.LST_FLAGS,	;
          IS_USD    WITH LTIT_1912.IS_USD,		;
          IS_CHECK  WITH LTIT_1912.IS_CHECK,	;
          CLI_CODE  WITH LTIT_1912.CLI_CODE,	;
          CLI_TYPE  WITH LTIT_1912.CLI_TYPE,	;
          FIRM      WITH LTIT_1912.FIRM,		;
          BANK      WITH LTIT_1912.BANK,		;
          WHO_CR    WITH user,					;
          WHERE_CR  WITH arm,					;
          ORD_QUEUE WITH LTIT_1912.ORD_QUEUE
ENDIF
USE
SELECT DEM_1914
USE
IF list_0 # 0
  DO LT_Refr WITH list_0
ENDIF
IF list_ch # 0
  DO LT_Refr WITH list_ch
ENDIF
IF list_0 # 0 .OR. list_ch # 0
  DO LT_Refr WITH tmpList
ENDIF

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Work_1       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                         �襦�� 皋�� �÷�｀皖�.                          �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 14.09.2001 陳�
PROCEDURE Work_1

PRIVATE rest1, new1, dem1, rest2, new2, dem2

rest1 = .F.
new1  = .F.
dem1  = .T.
rest2 = .F.
new2  = .T.
dem2  = .F.
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*� 敖 �ヤ�罔� ．� ���キ 陳陳陳陳陳陳陳陳陳陳陳陳朕 �
*� � [ ] �痰�※碎   [ ] ���覃 甎�甌�   [ ] ��牀� � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*� 敖 �ヤ�罔� � ���キ��� 陳陳陳陳陳陳陳陳陳陳陳陳� �
*� � [ ] �痰�※碎   [ ] ���覃 甎�甌�   [ ] ��牀� � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*�        < OK Ctrl-W > < �皖���碎瘴 Esc >         �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*� 敖 �ヤ�罔� ．� ���キ 陳陳陳陳陳陳朕 �
*� �  ( ) ���覃 甎�甌�    ( ) ��牀�  � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*� 敖 �ヤ�罔� � ���キ��� 陳陳陳陳陳陳� �
*� � ( ) �痰�※碎   ( ) ���覃 甎�甌� � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*�  < OK Ctrl-W > < �皖���碎瘴 Esc >   �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex, sw1, sw2
ex = 1
sw1 = 2
sw2 = 2
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
*DO D_Win_N WITH 10, 53, "�ウ━ �÷�｀皖� ぅ筥罔��"
*DO Sun_Bord WITH  2,  3,  4, 49, " �ヤ�罔� ．� ���キ "
*DO Sun_Bord WITH  5,  3,  7, 49, " �ヤ�罔� � ���キ��� "

DO D_Win_N WITH 10, 41, "�ウ━ �÷�｀皖� ぅ筥罔��"
DO Sun_Bord WITH  2,  3,  4, 37, " �ヤ�罔� ．� ���キ "
DO Sun_Bord WITH  5,  3,  7, 37, " �ヤ�罔� � ���キ��� "
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
*@ 3,  5 GET rest1 PICTURE "@*C �痰�※碎"
*@ 3, 20 GET new1 PICTURE "@*C ���覃 甎�甌�"
*@ 3, 39 GET dem1 PICTURE "@*C ��牀�"
*@ 6,  5 GET rest2 PICTURE "@*C �痰�※碎"
*@ 6, 20 GET new2 PICTURE "@*C ���覃 甎�甌�"
*@ 6, 39 GET dem2 PICTURE "@*C ��牀�"

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

@ 3, 6 GET sw1 PICTURE "@*RH ���覃 甎�甌�  ; ��牀�"
@ 6, 5 GET sw2 PICTURE "@*RH �痰�※碎 ; ���覃 甎�甌�"

READ CYCLE

IF ex # 1
  POP KEY
  RELEASE WINDOW (win_name)
  RETURN .F.
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)

IF sw1 = 1
  rest1 = .F.
  new1  = .T.
  dem1  = .F.
ELSE
  rest1 = .F.
  new1  = .F.
  dem1  = .T.
ENDIF

IF sw2 = 1
  rest2 = .T.
  new2  = .F.
  dem2  = .F.
ELSE
  rest2 = .F.
  new2  = .T.
  dem2  = .F.
ENDIF

DO Work WITH rest1, new1, dem1, rest2, new2, dem2

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Print_Tmp    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                ��ぃ�皰→� ∇ガキ���� ����� か� �ョ�皋.                 �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 19.09.2001 陳�
PROCEDURE Print_Tmp
PARAMETER tmpName

PRIVATE s_sav, r1_sav, r2_sav

s_sav = SELECT()
SELECT 0
IF LTIT_1912.IS_USD
  CREATE DBF (tmpo_path+tmpName)  ;
    ( MARK		C( 4), ; 
      PREFIX	C( 4), ;
      NAME		C(35), ;
      PRODUCER	C( 8), ;
      PRICE		N(10,4), ;
      QNT		N( 6), ;
      ROUND		N( 6), ;
      PACK		N( 6), ;
      CORPUS	C(16), ;
      DATE		D,     ;
      SUM       N(12,4),;
      ZAKAZ		N( 6), ;
      NOTE      C(10)  )
ELSE
  CREATE DBF (tmpo_path+tmpName)  ;
    ( MARK		C( 4), ; 
      PREFIX	C( 4), ;
      NAME		C(35), ;
      PRODUCER	C( 8), ;
      PRICE		N(10,2), ;
      QNT		N( 6), ;
      ROUND		N( 6), ;
      PACK		N( 6), ;
      CORPUS	C(16), ;
      DATE		D,     ;
      SUM       N(12,2),;
      ZAKAZ		N( 6), ;
      NOTE      C(10)  )
ENDIF

USE (tmpo_path+tmpName) ALIAS PRINT_TMP

r1_sav = RECNO("TEMP_LST")
r2_sav = RECNO("TMP_CHANGE")

SELECT TEMP_LST
SET RELATION TO CODE INTO STO_1912
SET RELATION TO STR(CODE) INTO TMP_CHANGE ADDITIVE

SCAN
  SELECT PRINT_TMP
  APPEND BLANK
  REPLACE PREFIX	WITH TEMP_LST.PREFIX, ;
          NAME		WITH TEMP_LST.NAME, ;
          PRODUCER	WITH TEMP_LST.PRODUCER, ;
          PRICE		WITH IIF(LTIT_1912.IS_USD, STO_1912.PRICE, ROUND(STO_1912.PRICE_R,2)), ;
          ZAKAZ		WITH TEMP_LST.QNT, ;
          QNT		WITH MIN(STO_1912.QNT, TEMP_LST.QNT), ;
          ROUND		WITH STO_1912.QNT_PACK, ;
          PACK		WITH STO_1912.QNT_ROUND, ;
          CORPUS	WITH TEMP_LST.CORPUS, ;
          DATE		WITH TEMP_LST.WAIT,   ;
          NOTE      WITH IIF(EMPTY(TEMP_LST.NOTE),"","��ユ. 罐��")
  REPLACE SUM WITH QNT*PRICE
  IF FOUND("TMP_CHANGE")
    SELECT TMP_CHANGE
    SCAN REST WHILE S_CODE = TEMP_LST.CODE
      SELECT PRINT_TMP
      APPEND BLANK
      REPLACE MARK		WITH IIF(TMP_CHANGE.CH_TYPE $ "PR", "�襦.", "���."), ;
              PREFIX	WITH TMP_CHANGE.PREFIX, ;
              NAME		WITH TMP_CHANGE.NAME, ;
              PRODUCER	WITH TMP_CHANGE.PRODUCER, ;
              PRICE		WITH IIF(LTIT_1912.IS_USD, STO_1913.PRICE, ROUND(STO_1913.PRICE_R,2)), ;
              ZAKAZ		WITH TMP_CHANGE.QNT, ;
              QNT		WITH MIN(STO_1913.QNT, TMP_CHANGE.QNT), ;
              ROUND		WITH STO_1913.QNT_PACK, ;
              PACK		WITH STO_1913.QNT_ROUND, ;
              CORPUS	WITH TMP_CHANGE.CORPUS, ;
              DATE		WITH TMP_CHANGE.WAIT,   ;
              NOTE      WITH IIF(EMPTY(TMP_CHANGE.NOTE),"","��ユ. 罐��")
      REPLACE SUM WITH QNT*PRICE
      SELECT TMP_CHANGE
    ENDSCAN
  ENDIF
  SELECT TEMP_LST
ENDSCAN

IF BETWEEN(r1_sav, 1, RECCOUNT("TEMP_LST"))
  GO r1_sav IN TEMP_LST
ENDIF

IF BETWEEN(r2_sav, 1, RECCOUNT("TMP_CHANGE"))
  GO r2_sav IN TMP_CHANGE
ENDIF

SELECT (s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� P_Doc        ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                            �ョ�碎 ぎ�祠キ��                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 19.09.2001 陳�
PROCEDURE P_Doc

PRIVATE s_sav				&& �ぅ瘡 甌縲��錺� ���ム ��｀腑� �゛�痰�
PRIVATE d_year				&& ��� ぎ�祠キ��
PRIVATE d_n					&& ���ム ぎ�-�� か� �ョ�皋
PRIVATE k00					&& ��鈑 か� ���瓷� ぎ�祠キ��
PRIVATE sb					&& 轒△����� ぎ�祠キ��
PRIVATE sb_det				&& 轒△����� ぅ���讚�� 痰牀��
PRIVATE p_ln				&& ��┃� 痰����肓 � 痰牀���
PRIVATE fnt					&& �爬籵+�爬キ��罔� 痰����肓
PRIVATE n_cp				&& ����腑痰〓 ���┤
PRIVATE lft					&& �ア�� ����
PRIVATE ffeed				&& ��▲琥キ┘ ぎ�-��
PRIVATE p_drctry			&& �����←キ┘ �ョ�皋
PRIVATE t_d					&& ������〓� ぎ�-��
PRIVATE f_d					&& ��あ�� ぎ�-��
PRIVATE t_p					&& ������〓� 痰����肓
PRIVATE f_p					&& ��あ�� 痰����肓
PRIVATE det					&& ｡��� ぅ���讚�� 痰牀��
PRIVATE n_p_det1			&& ���-〓 ぅ���讚諷 痰牀� �� 痰����罐
PRIVATE n_p_det				&& ���-〓 ぅ���讚諷 痰牀� �� 痰����罐
PRIVATE n_pg				&& ���-〓 痰����� � ぎ�祠キ皀
PRIVATE f_pg				&& �オ竕�� 痰������
PRIVATE dep_cod				&& ��� ��┘���

PRIVATE n, i, j, k, nn, str_w

s_sav = SELECT()

*
*   �皖琺��ガ ��゛�肓
*
SELECT 0
USE (base_path+"ACCOUNT") ALIAS CUS_1919 AGAIN ORDER TAG CUS_CODE
SEEK LTIT_1912.CLI_CODE
dep_cod = LTIT_1912.CLI_CODE

SELECT 0
USE (base_path+"PERSONS") ALIAS PRS_1919 AGAIN ORDER TAG CODE
SEEK user

f_name = SYS(3)
DO Wt_Mess WITH "��ぃ�皰→� ぎ�祠キ��"
DO Print_Tmp WITH f_name
DO Wt_Mess

DIMENSION sb(05,2)
*
sb(01,1) = "{Page   }"		&& �矗�����

*
*		�ぅ�皋筥��皰� ぎ�祠キ��
*
sb(02,1) = "{List}"			&& ���ム 甎�瓷�
sb(02,2) = STR(LTIT_1912.LST_NUM,6)

sb(03,1) = "{Date    }"		&& ���� ぎ�-��
sb(03,2) = DTOC(DATE())

sb(04,1) = "{Manager                     }"
							&& �≒�� ぎ�-��
sb(04,2) = ALLTRIM(PRS_1919.FAMILY)+" "+LEFT(PRS_1919.NAME, 1)+" "+LEFT(PRS_1919.S_NAME, 1)
sb(04,2) = PADR(sb(04,2), LEN(sb(04,1)))
*
*		��┘��
*
sb(05,1) = "{Customer                                        }"
							&& ��� ��┘���
sb(05,2) = CUS_1919.CUS_NAME
sb(05,2) = PADR(sb(05,2), LEN(sb(05,1)))

DIMENSION sb_det(12,2)
*
sb_det(01,1) = "{Type  }"			&& �┓ ���キ�
sb_det(02,1) = "{Pr}"				&& 踳筥��
sb_det(03,1) = "{Name                   }"
									&& ��━キ����┘
sb_det(04,1) = "{Prod  }"			&& ��爼� �牀├〓え皀��
sb_det(05,1) = "{Price   }"			&& �キ�
sb_det(06,1) = "{Qnt }"				&& ���-〓
sb_det(07,1) = "{Sto }"				&& ���-〓 �� 瓷��ぅ
sb_det(08,1) = "{Rnd }"				&& ��爼� �牀��Θ
sb_det(09,1) = "{Pack}"				&& ���-〓 � 祚���→�
sb_det(10,1) = "{Corpus        }"	&& ��牆竅
*sb_det(11,1) = "{Date    }"		&& ���� ��痰祚�キ��
sb_det(11,1) = "{SPr}"	     		&& ��ユ. 罐��
sb_det(12,1) = "{Sum       }"		&& �祠��

SELECT 0
USE (base_path+"DOC_FORM")
LOCATE FOR "CHANGES" == ALLTRIM(UPPER(DOC_NAME))

p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed = DOC_FORM.F_FEED
p_drctry = DOC_FORM.P_DIR

*
*		��゛�� ������→� ぎ�-��
*
n = MEMLINES(DOC_H)
IF n = 0
  DIMENSION t_d(1)
  t_d(1) = ""
ELSE
  DIMENSION t_d(n)
  FOR i = 1 TO n
    t_d(i) = MLINE(DOC_H,i)
  ENDFOR
ENDIF

*
*		��゛�� ������→� 痰����肓
*
n = MEMLINES(PAGE_H)
IF n = 0
  DIMENSION t_p(1)
  t_p(1) = ""
ELSE
  DIMENSION t_p(n)
  FOR i = 1 TO n
    t_p(i) = MLINE(PAGE_H,i)
  ENDFOR
ENDIF

*
*		��゛�� ��あ��� 痰����肓
*
n = MEMLINES(PAGE_F)
IF n = 0
  DIMENSION f_p(1)
  f_p(1) = ""
ELSE
  DIMENSION f_p(n)
  FOR i = 1 TO n
    f_p(i) = MLINE(PAGE_F,i)
  ENDFOR
ENDIF

*
*		��゛�� ��あ��� ぎ�-��
*
n = MEMLINES(DOC_F)
IF n = 0
  DIMENSION f_d(1)
  f_d(1) = ""
ELSE
  DIMENSION f_d(n)
  FOR i = 1 TO n
    f_d(i) = MLINE(DOC_F,i)
  ENDFOR
ENDIF

*
*		��゛�� ぅ���讚�� 痰牀��
*
n = MEMLINES(DETAIL)
IF n = 0
  DIMENSION det(1)
  det(1) = ""
ELSE
  DIMENSION det(n)
  FOR i = 1 TO n
    det(i) = MLINE(DETAIL,i)
  ENDFOR
ENDIF

USE

DO Ini_Prn WITH "���キ�", p_ln, lft, n_cp, fnt, ffeed, p_drctry

SELECT PRINT_TMP
GO TOP

nn = RECCOUNT()
n_p_det  = p_ln-ALEN(t_d)-MAX(ALEN(f_p),ALEN(f_d))
n_p_det1 = p_ln-ALEN(t_p)-MAX(ALEN(f_p),ALEN(f_d))
n_p_det  = FLOOR(n_p_det/ALEN(det))
n_p_det1 = FLOOR(n_p_det1/ALEN(det))
n_pg = 1
nn = nn-n_p_det

IF n_p_det <= 0  && ��；�� ≡� 痰����聶 ��� ������→�... �� ����� 痰�����!
  n_p_det = RECCOUNT()
ELSE
  nn = MAX(nn-n_p_det, 0)
  n_pg = 1+CEILING(nn/n_p_det1)
ENDIF
nn = 0
IF n_pg # 0
  FOR f_pg = 1 TO n_pg
    sb( 1,2) = ALLTRIM(STR(f_pg))+"/"+ALLTRIM(STR(n_pg))
    sb( 1,2) = PADR(sb(1,2), LEN(sb(1,1)))
    IF f_pg # 1   && ������〓� �� �ム〓� 痰����肓
      EJECT
      n_p_det = n_p_det1
      FOR i = 1 TO ALEN(t_p)
        str_w = t_p(i)
        FOR j = 1 TO ALEN(sb,1)
          str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
        ENDFOR
        IF i = 1
          @ PROW(), PCOL() SAY str_w
        ELSE
          @ PROW()+1, 0 SAY str_w
        ENDIF
      ENDFOR
    ELSE        && ������〓� �ム〓� 痰����肓
      FOR i = 1 TO ALEN(t_d)
        str_w = t_d(i)
        FOR j = 1 TO ALEN(sb,1)
          str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
        ENDFOR
        IF i = 1
          @ PROW(), PCOL() SAY str_w
        ELSE
          @ PROW()+1, 0 SAY str_w
        ENDIF
      ENDFOR
    ENDIF
*
*   �モ��讚�� ��痰� 痰����肓
* 
    i = 0
    DO WHILE .T.

sb_det(06,1) = "{Qnt }"				&& ���-〓
sb_det(07,1) = "{Sto }"				&& ���-〓 �� 瓷��ぅ
sb_det(08,1) = "{Rnd }"				&& ��爼� �牀��Θ
sb_det(09,1) = "{Pack}"				&& ���-〓 � 祚���→�
sb_det(10,1) = "{Corpus        }"	&& ��牆竅
sb_det(11,1) = "{SPr}"	         	&& ��ユ. 罐��


*     sb_det(01,1) = "{Type  }"			&& �┓ ���キ�
      sb_det(01,2) = PRINT_TMP.MARK
*     sb_det(02,1) = "{Pr}"			&& 踳筥��
      sb_det(02,2) = PRINT_TMP.PREFIX
*     sb_det(03,1) = "{Name                   }"
									&& ��━キ����┘
      sb_det(03,2) = PRINT_TMP.NAME
*     sb_det(04,1) = "{Prod  }"		&& ��爼� �牀├〓え皀��
      sb_det(04,2) = PRINT_TMP.PRODUCER
*     sb_det(05,1) = "{Price   }"	&& �キ�
      IF LTIT_1912.IS_USD
        sb_det(05,2) = STR(PRINT_TMP.PRICE, 10, 4)
      ELSE
        sb_det(05,2) = STR(PRINT_TMP.PRICE, 10, 2)
      ENDIF
*     sb_det(06,1) = "{Qnt }"		&& ���-〓
      sb_det(06,2) = STR(PRINT_TMP.ZAKAZ, 6)
*     sb_det(07,1) = "{Sto }"				&& ���-〓 �� 瓷��ぅ
      sb_det(07,2) = STR(PRINT_TMP.QNT, 6)
*     sb_det(08,1) = "{Rnd }"				&& ��爼� �牀��Θ
      sb_det(08,2) = STR(PRINT_TMP.ROUND, 6)
*     sb_det(09,1) = "{Pack}"				&& ���-〓 � 祚���→�
      sb_det(09,2) = STR(PRINT_TMP.PACK, 6)
*     sb_det(10,1) = "{Corpus        }"	&& ��牆竅
      sb_det(10,2) = PRINT_TMP.CORPUS
*     sb_det(11,1) = "{SPr}"			&& ��ユ. 罐��
      sb_det(11,2) = IIF(EMPTY(PRINT_TMP.NOTE),"   "," � ")
*     sb_det(12,1) = "{Sum       }"			&& �祠��
      IF LTIT_1912.IS_USD
        sb_det(12,2) = STR(PRINT_TMP.SUM, 12, 4)
      ELSE
        sb_det(12,2) = STR(PRINT_TMP.SUM, 12, 2)
      ENDIF
      FOR k = 1 TO ALEN(det)
        str_w = det(k)
        FOR j = 1 TO ALEN(sb_det,1)
          str_w = STRTRAN(str_w, sb_det(j,1), sb_det(j,2) )
        ENDFOR
        @ PROW()+1, 0 SAY str_w
      ENDFOR
      i = i+1
      SKIP
      IF EOF("PRINT_TMP")
        EXIT
      ENDIF
      IF  i >= n_p_det
        EXIT
      ENDIF
    ENDDO
*
*    ｪ�����┘ 痰����肓
*
    IF f_pg = n_pg
      FOR i = 1 TO ALEN(f_d)
        str_w = f_d(i)
        FOR j = 1 TO ALEN(sb,1)
          str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
        ENDFOR
        @ PROW()+1, 0 SAY str_w
      ENDFOR
    ELSE
      FOR i = 1 TO ALEN(f_p)
        str_w = f_p(i)
        FOR j = 1 TO ALEN(sb,1)
          str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
        ENDFOR
        @ PROW()+1, 0 SAY str_w
      ENDFOR
    ENDIF
  ENDFOR
ENDIF

USE

DO Term_Prn WITH "", (tmpo_path+f_name+".DBF"), dep_cod

DELETE FILE (tmpo_path+f_name+".DBF")
DELETE FILE (tmpo_path+f_name+".CDX")

SELECT CUS_1919
USE
SELECT PRS_1919
USE

SELECT (s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Get_Flags    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           ���Κ� か� ���キ.                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 09/20/2001 陳�
PROCEDURE Get_Flags
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

*敖陳陳陳陳陳陳陳陳陳陳�
*�  [ ] �� ��爿�       �
*�  [ ] �� ÷キゃ      �
*�  [ ] �� �����モ���  �
*�  [ ] � ��皰� ��痰.  �
*�       < OK >        �
*青陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex, swA, swR, swP
STORE .T. TO swA, swR, swP
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 8, 25, "�┓� ���キ"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ WROWS()-2, 9 GET ex PICTURE "@*HT \ OK "
@ 2, 4 GET swR PICTURE "@*C �� ��爿�"
@ 3, 4 GET swP PICTURE "@*C �� ÷キゃ"
@ 4, 4 GET swA PICTURE "@*C �� �����モ���"
@ 5, 4 GET show_dat PICTURE "@*C � ��皰� ��痰."

READ CYCLE

ch_flags = IIF(swA, "!", " ")+IIF(swR, "!", " ")+IIF(swP, "!", " ")
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
