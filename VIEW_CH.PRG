*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� View_Ch      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                            ��ᬮ�� �����.                             �
*�                                                                        �
*����������������������������������������������������������� 12.09.2001 �ͼ
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

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Prep_Tmp     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                      �����⮢�� �६������ 䠩��.                      �
*�                                                                        �
*����������������������������������������������������������� 12.09.2001 ���
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
  mss(2) = CHR(0)+"���᮪ �� ������!"
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
  mss(2) = CHR(0)+"���᮪ 㦥 �த��!"
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
DO Wt_Mess WITH "               ���� �����               "
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
    mss(2) = CHR(0)+"���᮪ ᮤ�ন� �஭�! ��ࠡ�⪠ ����饭�!"
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
        IF tmpArr(j, 4) < 0 && ��㣮� �����
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

*������������������������������������������������������������������������Ŀ
*�   ��楤�� WhenCome     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                   ��� ����㯫���� ����樨 �� ᪫��                    �
*�                                                                        �
*����������������������������������������������������������� 13.09.2001 ���
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

*������������������������������������������������������������������������Ŀ
*�   ��楤�� View1        ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                             ��ᬮ�� �����                             �
*�                                                                        �
*����������������������������������������������������������� 13.09.2001 ���
PROCEDURE View1

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
PRIVATE s_sav         && ����� ࠡ�祩 ������ ��� ��࠭���� � ����⠭�������!
PRIVATE f_dummy, listNo, l, c, l1

*
*   ������塞 ���祭�ﬨ ��६���� ���ﭨ�...
*
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ���뢠�� 䠩�� �.�., ��⠭�������� �裡 � �. �.
*
s_sav = SELECT()

SELECT 0
DO Use_Dummy
f_dummy = SYS(3)
COPY TO (tmpo_path+f_dummy)
SELECT 0
USE (tmpo_path+f_dummy) ALIAS DUMMY1

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Switch OF (menu_name) PROMPT "���� �����" KEY Ctrl-A, "Tab"
ON SELECTION PAD Switch OF (menu_name) DO Sw_Mode WITH "Switch"

DEFINE PAD Print OF (menu_name) PROMPT "�����" KEY Ctrl-A, "Ctrl-P"
ON SELECTION PAD Print OF (menu_name) DO Sw_Mode WITH "Print"

DEFINE PAD Work OF (menu_name) PROMPT "��ࠡ�⪠" KEY Ctrl-A, "Shift-Tab"
ON SELECTION PAD Work OF (menu_name) DO Sw_Mode WITH "Work"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd, ln1, ln2
*ڄ���č�����������������������������������Ċ��-��č����.��ď���.��Ŀ
*�� pppp nnnnnnnnnNnnnnnnnnnNnnnnn ppppPppp 999999 999999 ��.��.�����
*��������������������������������������������������������������������
*PPPP nnnnnnnnnNnnnnnnnnnNnnnnn ppppPppp    ����� kkkkkkkkkKkkkkkk
*���č�����������������������������������Ċ��-��č����.��ď���.���Ē�
*�pppp nnnnnnnnnNnnnnnnnnnNnnnnn ppppPppp 999999 999999 ��.��.���� ��
*��������������������������������������������������������������������
ln = WROWS("")-10   && ������⢮ ������� ��ப BROWSE
wd = 66
ln2 = CEILING(ln/3)
ln1 = ln-ln2
DO D_Wins WITH ln, wd, "���祭� ����� ��� ᯨ᪠ � "+ALLTRIM(STR(tmpList)), 0, ln2
@ 1, 2 SAY "����č�����������������������������������Ċ��-��č����.��ď���."
@ WROWS()-ln2-2, 0 SAY SPACE(WCOLS())
DO Sun_Bord WITH WROWS()-ln2-1, 1, WROWS()-1, WCOLS()-2
@ WROWS()-ln2-1, 2 SAY ;
"��č�����������������������������������Ċ��-��č����.��ď���.���Ē"

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

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Tab     DO Sw_Mode WITH "Switch"
    ON KEY LABEL Ctrl+P  DO Sw_Mode WITH "Print"
    ON KEY LABEL BackTab DO Sw_Mode WITH "Work"
    SELECT TEMP_LST

*
*    BROWSE - ����
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

  CASE what_do = "Switch"    && ��४��祭�� ����

    IF listNo = 1
      IF FOUND("TMP_CHANGE")
        DEFINE PAD Switch OF (menu_name) PROMPT "���� ᯨ᪠" KEY Ctrl-A, "Tab"
        listNo = 2
      ENDIF
    ELSE
      DEFINE PAD Switch OF (menu_name) PROMPT "���� �����" KEY Ctrl-A, "Tab"
      listNo = 1
    ENDIF
    what_do = "List"

  CASE what_do = "Print"    && �����

    DO P_Doc
    what_do = "List"

  CASE what_do = "Work"    && ��ࠡ�⪠

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
*   �����⭥�쪮 (�����⭥�쪮!) ����뢠�� DBF-䠩��.
*
SELECT DUMMY1
USE
DELETE FILE (tmpo_path+f_dummy+".dbf")
SELECT (s_sav)

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Draw_Foot    ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                   ���㥬 �������⥫��� ���ଠ��.                    �
*�                                                                        �
*����������������������������������������������������������� 20.10.1998 ���
PROCEDURE Draw_Foot

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� �����, ��㥬...
*
@ WROWS()-ln2-2, 0 SAY TEMP_LST.PREFIX+" "+TEMP_LST.NAME+" "+TEMP_LST.PRODUCER+ ;
  "    ����� "+TEMP_LST.CORPUS

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Work         ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                               ��ࠡ�⪠                                �
*�                                                                        �
*����������������������������������������������������������� 14.09.2001 ���
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
  *  ��᪫���� ������⢠ �� ⨯�� ��ࠡ�⪨
  *
  STORE 0 TO qnt_0, qnt_ch, qnt_exist
  DO CASE
  CASE CODE = 0      && ��᪫��᪠� ������ - �� ������⢮ � ����� ��� �����
    qnt_0 = QNT
  CASE .NOT. FOUND("TMP_CHANGE")  && ��� �����
    qnt_exist = MIN(QNT, STO_1912.QNT)
    qnt_0     = QNT-qnt_exist
  OTHERWISE                       && ���� ������
    qnt_exist = MIN(QNT, STO_1912.QNT)
    qnt_ch    = QNT-qnt_exist
  ENDCASE
  *
  *   ��᪫��뢠�� ���-�� �� �।��ᠭ��
  *
  STORE 0 TO qnt_dem, qnt_list1, qnt_list2, qnt_rest
  IF rest1     && ����� ��� ����� ��⠢��� � ᯨ᪥
    qnt_rest = qnt_rest+qnt_0
  ENDIF
  IF new1     && ����� ��� ����� 㭥�� � ���� ᯨ᮪
    qnt_list1 = qnt_list1+qnt_0
  ENDIF
  IF dem1     && ����� ��� ����� 㭥�� � ���
    qnt_dem = qnt_dem+qnt_0
  ENDIF
  IF rest2     && ����� � �������� ��⠢��� � ᯨ᪥
    qnt_rest = qnt_rest+qnt_ch
  ENDIF
  IF new2     && ����� � �������� 㭥�� � ���� ᯨ᮪
    qnt_list2 = qnt_list2+qnt_ch
  ENDIF
  IF dem2     && ����� � �������� 㭥�� � ���
    qnt_dem = qnt_dem+qnt_ch
  ENDIF
  qnt_rest = qnt_rest+qnt_exist   && ��, �� ���� �� ᪫��� - �ᥣ�� ��⠢�塞!
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
          LST_NAME  WITH LEFT(LTIT_1912.LST_NAME, LEN(LTIT_1912.LST_NAME)-7)+" ������", ;
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

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Work_1       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                         �롮� ⨯� ��ࠡ�⪨.                          �
*�                                                                        �
*����������������������������������������������������������� 14.09.2001 ���
PROCEDURE Work_1

PRIVATE rest1, new1, dem1, rest2, new2, dem2

rest1 = .F.
new1  = .F.
dem1  = .T.
rest2 = .F.
new2  = .T.
dem2  = .F.
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

*�������������������������������������������������Ŀ
*� �� ����� ��� ����� ������������������������Ŀ �
*� � [ ] ��⠢���   [ ] ���� ᯨ᮪   [ ] ���� � �
*� ����������������������������������������������� �
*� �� ����� � �������� �����������������������Ŀ �
*� � [ ] ��⠢���   [ ] ���� ᯨ᮪   [ ] ���� � �
*� ����������������������������������������������� �
*�        < OK Ctrl-W > < �⪠������ Esc >         �
*���������������������������������������������������

*�������������������������������������Ŀ
*� �� ����� ��� ����� ������������Ŀ �
*� �  ( ) ���� ᯨ᮪    ( ) ����  � �
*� ����������������������������������� �
*� �� ����� � �������� �����������Ŀ �
*� � ( ) ��⠢���   ( ) ���� ᯨ᮪ � �
*� ����������������������������������� �
*�  < OK Ctrl-W > < �⪠������ Esc >   �
*���������������������������������������

PRIVATE ex, sw1, sw2
ex = 1
sw1 = 2
sw2 = 2
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
*DO D_Win_N WITH 10, 53, "����� ��ࠡ�⪨ �����"
*DO Sun_Bord WITH  2,  3,  4, 49, " ����� ��� ����� "
*DO Sun_Bord WITH  5,  3,  7, 49, " ����� � �������� "

DO D_Win_N WITH 10, 41, "����� ��ࠡ�⪨ �����"
DO Sun_Bord WITH  2,  3,  4, 37, " ����� ��� ����� "
DO Sun_Bord WITH  5,  3,  7, 37, " ����� � �������� "
*------------------------------------------------------------------------
*      ���� ����� ������
*
*@ 3,  5 GET rest1 PICTURE "@*C ��⠢���"
*@ 3, 20 GET new1 PICTURE "@*C ���� ᯨ᮪"
*@ 3, 39 GET dem1 PICTURE "@*C ����"
*@ 6,  5 GET rest2 PICTURE "@*C ��⠢���"
*@ 6, 20 GET new2 PICTURE "@*C ���� ᯨ᮪"
*@ 6, 39 GET dem2 PICTURE "@*C ����"

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

@ 3, 6 GET sw1 PICTURE "@*RH ���� ᯨ᮪  ; ����"
@ 6, 5 GET sw2 PICTURE "@*RH ��⠢��� ; ���� ᯨ᮪"

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

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Print_Tmp    ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                �����⮢�� �६������ 䠩�� ��� ����.                 �
*�                                                                        �
*����������������������������������������������������������� 19.09.2001 ���
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
          NOTE      WITH IIF(EMPTY(TEMP_LST.NOTE),"","����. 業�")
  REPLACE SUM WITH QNT*PRICE
  IF FOUND("TMP_CHANGE")
    SELECT TMP_CHANGE
    SCAN REST WHILE S_CODE = TEMP_LST.CODE
      SELECT PRINT_TMP
      APPEND BLANK
      REPLACE MARK		WITH IIF(TMP_CHANGE.CH_TYPE $ "PR", "��.", "���."), ;
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
              NOTE      WITH IIF(EMPTY(TMP_CHANGE.NOTE),"","����. 業�")
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

*������������������������������������������������������������������������Ŀ
*�   ��楤�� P_Doc        ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                            ����� ���㬥��                            �
*�                                                                        �
*����������������������������������������������������������� 19.09.2001 ���
PROCEDURE P_Doc

PRIVATE s_sav				&& ����� ��࠭塞 ����� ࠡ�祩 ������
PRIVATE d_year				&& ��� ���㬥��
PRIVATE d_n					&& ����� ���-� ��� ����
PRIVATE k00					&& ���� ��� ���᪠ ���㬥��
PRIVATE sb					&& �ᥢ������ ���㬥��
PRIVATE sb_det				&& �ᥢ������ ��⠫쭮� ��ப�
PRIVATE p_ln				&& ����� ��࠭��� � ��ப��
PRIVATE fnt					&& ����+�ਥ���� ��࠭���
PRIVATE n_cp				&& ������⢮ �����
PRIVATE lft					&& ����� ����
PRIVATE ffeed				&& �����襭�� ���-�
PRIVATE p_drctry			&& ���ࠢ����� ����
PRIVATE t_d					&& ��������� ���-�
PRIVATE f_d					&& ������ ���-�
PRIVATE t_p					&& ��������� ��࠭���
PRIVATE f_p					&& ������ ��࠭���
PRIVATE det					&& ��ࠧ ��⠫쭮� ��ப�
PRIVATE n_p_det1			&& ���-�� ��⠫��� ��ப �� ��࠭��
PRIVATE n_p_det				&& ���-�� ��⠫��� ��ப �� ��࠭��
PRIVATE n_pg				&& ���-�� ��࠭�� � ���㬥��
PRIVATE f_pg				&& ������ ��࠭��
PRIVATE dep_cod				&& ��� ������

PRIVATE n, i, j, k, nn, str_w

s_sav = SELECT()

*
*   ���뢠�� ⠡����
*
SELECT 0
USE (base_path+"ACCOUNT") ALIAS CUS_1919 AGAIN ORDER TAG CUS_CODE
SEEK LTIT_1912.CLI_CODE
dep_cod = LTIT_1912.CLI_CODE

SELECT 0
USE (base_path+"PERSONS") ALIAS PRS_1919 AGAIN ORDER TAG CODE
SEEK user

f_name = SYS(3)
DO Wt_Mess WITH "�����⮢�� ���㬥��"
DO Print_Tmp WITH f_name
DO Wt_Mess

DIMENSION sb(05,2)
*
sb(01,1) = "{Page   }"		&& ��࠭��

*
*		�����䨪��� ���㬥��
*
sb(02,1) = "{List}"			&& ����� ᯨ᪠
sb(02,2) = STR(LTIT_1912.LST_NUM,6)

sb(03,1) = "{Date    }"		&& ��� ���-�
sb(03,2) = DTOC(DATE())

sb(04,1) = "{Manager                     }"
							&& ���� ���-�
sb(04,2) = ALLTRIM(PRS_1919.FAMILY)+" "+LEFT(PRS_1919.NAME, 1)+" "+LEFT(PRS_1919.S_NAME, 1)
sb(04,2) = PADR(sb(04,2), LEN(sb(04,1)))
*
*		������
*
sb(05,1) = "{Customer                                        }"
							&& ��� ������
sb(05,2) = CUS_1919.CUS_NAME
sb(05,2) = PADR(sb(05,2), LEN(sb(05,1)))

DIMENSION sb_det(12,2)
*
sb_det(01,1) = "{Type  }"			&& ��� ������
sb_det(02,1) = "{Pr}"				&& ��䨪�
sb_det(03,1) = "{Name                   }"
									&& ������������
sb_det(04,1) = "{Prod  }"			&& ��ଠ �ந�����⥫�
sb_det(05,1) = "{Price   }"			&& ����
sb_det(06,1) = "{Qnt }"				&& ���-��
sb_det(07,1) = "{Sto }"				&& ���-�� �� ᪫���
sb_det(08,1) = "{Rnd }"				&& ��ଠ �த���
sb_det(09,1) = "{Pack}"				&& ���-�� � 㯠�����
sb_det(10,1) = "{Corpus        }"	&& �����
*sb_det(11,1) = "{Date    }"		&& ��� ����㯫����
sb_det(11,1) = "{SPr}"	     		&& ����. 業�
sb_det(12,1) = "{Sum       }"		&& �㬬�

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
*		������ ��������� ���-�
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
*		������ ��������� ��࠭���
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
*		������ ������� ��࠭���
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
*		������ ������� ���-�
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
*		������ ��⠫쭮� ��ப�
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

DO Ini_Prn WITH "������", p_ln, lft, n_cp, fnt, ffeed, p_drctry

SELECT PRINT_TMP
GO TOP

nn = RECCOUNT()
n_p_det  = p_ln-ALEN(t_d)-MAX(ALEN(f_p),ALEN(f_d))
n_p_det1 = p_ln-ALEN(t_p)-MAX(ALEN(f_p),ALEN(f_d))
n_p_det  = FLOOR(n_p_det/ALEN(det))
n_p_det1 = FLOOR(n_p_det1/ALEN(det))
n_pg = 1
nn = nn-n_p_det

IF n_p_det <= 0  && ������ ��� ��࠭��� ��� ���������... �� ����� ��࠭��!
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
    IF f_pg # 1   && ��������� �� ��ࢮ� ��࠭���
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
    ELSE        && ��������� ��ࢮ� ��࠭���
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
*   ��⠫쭠� ���� ��࠭���
* 
    i = 0
    DO WHILE .T.

sb_det(06,1) = "{Qnt }"				&& ���-��
sb_det(07,1) = "{Sto }"				&& ���-�� �� ᪫���
sb_det(08,1) = "{Rnd }"				&& ��ଠ �த���
sb_det(09,1) = "{Pack}"				&& ���-�� � 㯠�����
sb_det(10,1) = "{Corpus        }"	&& �����
sb_det(11,1) = "{SPr}"	         	&& ����. 業�


*     sb_det(01,1) = "{Type  }"			&& ��� ������
      sb_det(01,2) = PRINT_TMP.MARK
*     sb_det(02,1) = "{Pr}"			&& ��䨪�
      sb_det(02,2) = PRINT_TMP.PREFIX
*     sb_det(03,1) = "{Name                   }"
									&& ������������
      sb_det(03,2) = PRINT_TMP.NAME
*     sb_det(04,1) = "{Prod  }"		&& ��ଠ �ந�����⥫�
      sb_det(04,2) = PRINT_TMP.PRODUCER
*     sb_det(05,1) = "{Price   }"	&& ����
      IF LTIT_1912.IS_USD
        sb_det(05,2) = STR(PRINT_TMP.PRICE, 10, 4)
      ELSE
        sb_det(05,2) = STR(PRINT_TMP.PRICE, 10, 2)
      ENDIF
*     sb_det(06,1) = "{Qnt }"		&& ���-��
      sb_det(06,2) = STR(PRINT_TMP.ZAKAZ, 6)
*     sb_det(07,1) = "{Sto }"				&& ���-�� �� ᪫���
      sb_det(07,2) = STR(PRINT_TMP.QNT, 6)
*     sb_det(08,1) = "{Rnd }"				&& ��ଠ �த���
      sb_det(08,2) = STR(PRINT_TMP.ROUND, 6)
*     sb_det(09,1) = "{Pack}"				&& ���-�� � 㯠�����
      sb_det(09,2) = STR(PRINT_TMP.PACK, 6)
*     sb_det(10,1) = "{Corpus        }"	&& �����
      sb_det(10,2) = PRINT_TMP.CORPUS
*     sb_det(11,1) = "{SPr}"			&& ����. 業�
      sb_det(11,2) = IIF(EMPTY(PRINT_TMP.NOTE),"   "," � ")
*     sb_det(12,1) = "{Sum       }"			&& �㬬�
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
*    ����砭�� ��࠭���
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

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Get_Flags    ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                           ������ ��� �����.                            �
*�                                                                        �
*����������������������������������������������������������� 09/20/2001 ���
PROCEDURE Get_Flags
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

*���������������������Ŀ
*�  [ ] �� ����       �
*�  [ ] �� �७��      �
*�  [ ] �� ��ࠬ��ࠬ  �
*�  [ ] � ��⮩ ����.  �
*�       < OK >        �
*�����������������������

PRIVATE ex, swA, swR, swP
STORE .T. TO swA, swR, swP
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 8, 25, "���� �����"

*------------------------------------------------------------------------
*      ���� ����� ������
*
@ WROWS()-2, 9 GET ex PICTURE "@*HT \ OK "
@ 2, 4 GET swR PICTURE "@*C �� ����"
@ 3, 4 GET swP PICTURE "@*C �� �७��"
@ 4, 4 GET swA PICTURE "@*C �� ��ࠬ��ࠬ"
@ 5, 4 GET show_dat PICTURE "@*C � ��⮩ ����."

READ CYCLE

ch_flags = IIF(swA, "!", " ")+IIF(swR, "!", " ")+IIF(swP, "!", " ")
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN