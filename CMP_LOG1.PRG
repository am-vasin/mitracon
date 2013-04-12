*������������������������������������������������������������������������ͻ
*� ���� Cmp_Log      ���ࠡ��稪 ���३ ��ᨭ           17.03.98 12:50:12 �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�               �ࠢ����� ��娢� ��� �� � ����묨 �த���.               �
*�                                                                        �
*������������������������������������������������������������������������ͼ
PROCEDURE Cmp_Log1

PRIVATE f_name, n, f, d, dt, s, dt_w, dt_e, n0

dt_w = DATE()
dt_e = DATE()

IF TYPE("vk_arch") # "C"
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"����� ᡮ� ���ଠ樨 �� ��⠭�����."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

*n = FOPEN(base_path+"FOR_VK.TXT")
*f_name = FGETS(n, 120)
*f_name = ALLTRIM(f_name)
*= FCLOSE(n)

f_name = vk_arch
IF .NOT. "." $ f_name
  f_name = f_name+".dbf"
ENDIF

IF .NOT. File_O(f_name)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��� 䠩�� ��娢�."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

IF .NOT. Get_Date()
  RETURN
ENDIF

DO Wt_Mess WITH "�����⮢�� ������..."

DELETE FILE (tmpo_path+"ARCH_S.DBF")
DELETE FILE (tmpo_path+"ARCH_S.CDX")
DELETE FILE (tmpo_path+"ARCH_T.DBF")
DELETE FILE (tmpo_path+"ARCH_T.CDX")

CREATE DBF (tmpo_path+"ARCH_S") ;
  ( FIRM     C( 3), ;
    DOC_NUM  C(10), ;
    DOC_DATE D,     ;
    CODE     N( 7), ;
    QNT      N( 6), ;
    PRICE_R  N(12,5)  )
INDEX ON FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM TAG DOC_NUM

SELECT 0
CREATE DBF (tmpo_path+"ARCH_T") ;
  ( FIRM     C( 3), ;
    DOC_NUM  C(10), ;
    DOC_DATE D,     ;
    TOTAL_A  C(12), ;
    TOTAL_S  C(12)   )
INDEX ON FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM TAG DOC_NUM
    
SELECT 0
USE (f_name) ORDER TAG DATE_CODE ALIAS ARCH
SEEK DTOS(dt_w)
IF .NOT. FOUND()
  n0 = RECNO(0)
  IF BETWEEN(n0, 1, RECCOUNT())
    GO n0
  ENDIF
ENDIF
SCAN REST WHILE DATE <= dt_e
  SELECT ARCH_S
  APPEND BLANK
  REPLACE FIRM     WITH ARCH.FIRM,     ;
          DOC_NUM  WITH ARCH.DOC_NUM,  ;
          DOC_DATE WITH ARCH.DATE,     ;
          CODE     WITH ARCH.STO_CODE, ;
          QNT      WITH ARCH.QTY,      ;
          PRICE_R  WITH ARCH.PRICE
  SELECT ARCH
ENDSCAN

USE
SELECT ARCH_S
s = 0
n = "          "
f = "   "
d = {}
SCAN
  IF DOC_NUM # n .OR. FIRM # f .OR. DOC_DATE # d
    IF .NOT. EMPTY(n)
      SELECT ARCH_T
      APPEND BLANK
      REPLACE FIRM     WITH f,   ;
              DOC_NUM  WITH n,   ;
              DOC_DATE WITH d,   ;
              TOTAL_A  WITH STR(s,12,2), ;
              TOTAL_S  WITH "������������"
      SELECT ARCH_S
    ENDIF
    n = DOC_NUM
    d = DOC_DATE
    f = FIRM
    s = 0
  ENDIF
  s = s+ROUND(QNT*PRICE_R,2)
ENDSCAN

IF .NOT. EMPTY(n)
  SELECT ARCH_T
  APPEND BLANK
  REPLACE FIRM     WITH f,  ;
          DOC_NUM  WITH n,  ;
          DOC_DATE WITH d,  ;
          TOTAL_A  WITH STR(s,12,2), ;
          TOTAL_S  WITH "������������"
ENDIF

SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG HRON

SEEK DTOS(dt_w)
IF .NOT. FOUND()
  n0 = RECNO(0)
  IF BETWEEN(n0, 1, RECCOUNT())
    GO n0
  ENDIF
ENDIF
SCAN REST WHILE DOC_DATE <= dt_e FOR CUS_CODE >= 0
  SELECT ARCH_T
  IF .NOT. SEEK(SALE_TIT.FIRM+LEFT(DTOS(SALE_TIT.DOC_DATE),4)+SALE_TIT.DOC_NUM)
    APPEND BLANK
    REPLACE FIRM     WITH SALE_TIT.FIRM,     ;
            DOC_NUM  WITH SALE_TIT.DOC_NUM,  ;
            DOC_DATE WITH SALE_TIT.DOC_DATE, ;
            TOTAL_A  WITH "������������",    ;
            TOTAL_S  WITH STR(SALE_TIT.TOTAL_R,12,2)
  ELSE
    REPLACE TOTAL_S WITH STR(SALE_TIT.TOTAL_R,12,2)
  ENDIF
  SELECT SALE_TIT
ENDSCAN

DO Wt_Mess

USE
SELECT ARCH_T

IF RECCOUNT() # 0
  DO View_Res
ELSE
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�த�� �� ��ਮ� � "+DTOC(dt_w)+" �� "+DTOC(dt_e)+" �� �����㦥��!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
ENDIF

CLOSE DATABASES

DELETE FILE (tmpo_path+"ARCH_S.DBF")
DELETE FILE (tmpo_path+"ARCH_S.CDX")
DELETE FILE (tmpo_path+"ARCH_T.DBF")
DELETE FILE (tmpo_path+"ARCH_T.CDX")
RETURN

*������������������������������������������������������������������������ͻ
*�                                                                        �
*�                               ���� ����.                               �
*�                                                                        �
*����������������������������������������������������� 17.03.98 15:53:14 ͼ
PROCEDURE Get_Date

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

*          �஢�ઠ ��娢�
*����������������������������������Ŀ
*�                                  �
*�    C DD.MM.YYYY �� DD.MM.YYYY    �
*�                                  �
*� < OK Ctrl-W > < �⪠������ Esc > �
*�                                  �
*������������������������������������

PRIVATE ex
ex   = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 8, 38, "�஢�ઠ ��娢�"

*------------------------------------------------------------------------
*      ���� ����� ������
*
@ 3,  6 SAY "�" GET dt_w
@ 3, 19 SAY "��" GET dt_e
@ 5,  3 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*������������������������������������������������������������������������ͻ
*�                                                                        �
*�                          ��ᬮ�� १����.                          �
*�                                                                        �
*����������������������������������������������������� 17.03.98 16:00:42 ͼ
PROCEDURE View_Res

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
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()
PRIVATE flt, rc_sav
flt = .F.

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD View OF (menu_name) PROMPT "��ᬮ��" KEY Ctrl-A, "Enter"
ON SELECTION PAD View OF (menu_name) DO Sw_Mode WITH "View"

DEFINE PAD Print OF (menu_name) PROMPT "����� ���-�" KEY Ctrl-A, "Tab"
ON SELECTION PAD Print OF (menu_name) DO Sw_Mode WITH "Print"

DEFINE PAD Filter OF (menu_name) PROMPT "���쪮 ���." KEY Ctrl-A, "F6"
ON SELECTION PAD Filter OF (menu_name) DO Sw_Mode WITH "Filter"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   ���뢠�� 䠩�� �.�., ��⠭�������� �裡 � �. �.
*
DO Use_Dummy
SELECT ARCH_T
*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
ln = MIN(100, RECCOUNT())   && ������⢮ ������� ��ப BROWSE
wd = IIF(SET("CENTURY") = "ON", 54, 52)

DO D_Wins WITH ln, wd, "�������� � "+DTOC(dt_w)+" �� "+DTOC(dt_e), 0, 0
what_do = "List"
@ 1, 2 SAY IIF(SET("CENTURY") = "ON",  ;
         "����N ���-����Ą�������������Ā�娢���č���������", ;
         "����N ���-���Ą�����������Ā�娢���č���������" )

GO TOP

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F6    DO Sw_Mode WITH "Filter"
    ON KEY LABEL Tab   DO Sw_Mode WITH "Print"

*
*    BROWSE - ����
*
    BROWSE FIELDS FIRM:H="", ;
                  F_Doc = LEFT(DOC_NUM,4)+"-"+ALLTRIM(SUBSTR(DOC_NUM,5)):11:H="", ;
                  DOC_DATE:P="@D", ;
                  TOTAL_A:H="":12, ;
                  TOTAL_S:H="":12, ;
                  Is_Eq = IIF(TOTAL_A = TOTAL_S, " ", "�"):1:H="" , ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() = 12, "Exit", "View")
    ENDIF

  CASE what_do = "Print"    && ����� ���㬥��

    DO Prn_Inv WITH FIRM, DOC_NUM, DOC_DATE
    what_do = "List"
    
  CASE what_do = "View"      && ��ᬮ�� ���㬥�⮢

    DO View
    what_do = "List"
    
  CASE what_do = "Filter"    && ��⠭����/���⨥ 䨫���

    IF .NOT. flt
      rc_sav = RECNO()
      SET FILTER TO TOTAL_A # TOTAL_S
      GO TOP
      IF EOF() .OR. BOF()
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"��� ��宦�����!"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        SET FILTER TO
        IF BETWEEN(rc_sav, 1, RECCOUNT("ARCH_T"))
          GO rc_sav IN ARCH_T
        ENDIF
      ELSE
        IF BETWEEN(rc_sav, 1, RECCOUNT("ARCH_T"))
          GO rc_sav IN ARCH_T
        ENDIF
        DEFINE PAD Filter OF (menu_name) PROMPT "��." BEFORE Exit KEY Ctrl-A, "F6"
        flt = .T.
      ENDIF
    ELSE
      SET FILTER TO
      DEFINE PAD Filter OF (menu_name) PROMPT "���쪮 ���." KEY Ctrl-A, "F6"
      flt = .F.
    ENDIF
    what_do = "List"
    
  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
CLOSE DATABASES
RELEASE MENU (menu_name) EXTENDED

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� View         ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                          ��ᬮ�� ���������.                           �
*�                                                                        �
*����������������������������������������������������������� 18.02.2000 ���
PROCEDURE View

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
DELETE FILE (tmpo_path+"TMP_VIEW.DBF")
DELETE FILE (tmpo_path+"TMP_VIEW.CDX")
CREATE DBF (tmpo_path+"TMP_VIEW") ;
  ( CODE     N( 7), ;
    PREFIX   C( 4), ;
    NAME     C(35), ;
    PRODUCER C( 8), ;
    QNT_S    N( 6), ;
    QNT_A    N( 6), ;
    PRICE_S  N(12,5),;
    PRICE_A  N(12,5)  )
INDEX ON CODE TAG CODE
    
SELECT 0
DO Use_Dummy
SELECT ARCH_S
SET ORDER TO TAG DOC_NUM
SEEK ARCH_T.FIRM+LEFT(DTOS(ARCH_T.DOC_DATE),4)+ARCH_T.DOC_NUM
SCAN REST WHILE FIRM = ARCH_T.FIRM .AND. DOC_DATE = ARCH_T.DOC_DATE .AND. DOC_NUM = ARCH_T.DOC_NUM
  SELECT TMP_VIEW
  IF .NOT. SEEK(ARCH_S.CODE)
    APPEND BLANK
  ENDIF
  REPLACE CODE    WITH ARCH_S.CODE, ;
          QNT_A   WITH QNT_A+ARCH_S.QNT, ;
          PRICE_A WITH ARCH_S.PRICE_R
  SELECT ARCH_S
ENDSCAN
SELECT 0
USE (base_path+"SALE") ORDER TAG DOC_NUM ALIAS SAL_0218 AGAIN
SEEK ARCH_T.FIRM+LEFT(DTOS(ARCH_T.DOC_DATE),4)+ARCH_T.DOC_NUM
SCAN REST WHILE FIRM = ARCH_T.FIRM .AND. DOC_DATE = ARCH_T.DOC_DATE .AND. DOC_NUM = ARCH_T.DOC_NUM
  SELECT TMP_VIEW
  IF .NOT. SEEK(SAL_0218.CODE)
    APPEND BLANK
  ENDIF
  REPLACE CODE    WITH SAL_0218.CODE, ;
          QNT_S   WITH QNT_S+SAL_0218.QNT, ;
          PRICE_S WITH SAL_0218.PRICE_D
  SELECT SAL_0218
ENDSCAN
USE (base_path+"STOCK") ORDER TAG CODE ALIAS ST_0218 AGAIN
SELECT TMP_VIEW
SET RELATION TO CODE INTO ST_0218
REPLACE ALL PREFIX   WITH ST_0218.PREFIX, ;
            NAME     WITH ST_0218.NAME,   ;
            PRODUCER WITH ST_0218.PRODUCER
SET RELATION TO
SELECT ST_0218
USE
SELECT TMP_VIEW
INDEX ON NAME+PREFIX+PRODUCER TAG NAME
GO TOP
*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
wd = 74
*�ď����������������������������Ċ �����Ċ �������Ė��� �������Ė��� ����Ŀ
*�NNNN NNNNNNNNNNNNNNNNNNNNNNNNN 999999 999999 V 999999999999 99999999999 V�
*���������������������������������������������������������������������������

DO D_Wins WITH ln, wd, "����ন��� ���������", 0, 1
@ 1, 2 SAY "ď����������������������������Ċ �����Ċ �������Ė��� �������Ė��� ���"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1

*
*    BROWSE - ����
*
    BROWSE FIELDS PREFIX:H="", Brw_Name=Nm_Part(NAME):25:H="",   ;
                  QNT_A:H="":P="@Z 999999", ;
                  QNT_S:H="":P="@Z 999999", ;
                  EQ_QNT=IIF(QNT_A=QNT_S, " ", "�"):1:H="", ;
                  PRICE_A:H="":P="@Z 999999.99999", ;
                  PRICE_S:H="":P="@Z 999999.99999", ;
                  EQ_PRICE=IIF(PRICE_A=PRICE_S, " ", "�"):1:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
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
*   �����⭥�쪮 (�����⭥�쪮!) ����뢠�� DBF-䠩��.
*
USE
DELETE FILE (tmpo_path+"TMP_VIEW.DBF")
DELETE FILE (tmpo_path+"TMP_VIEW.CDX")
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
@ WROWS()-2, 10 SAY "��ଠ "+PRODUCER

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.