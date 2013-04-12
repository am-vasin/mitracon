*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� RynBoxes     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�        ��᪫���� ��������� ��।�� �� ॠ������ �� ��஡���.         �
*�                                                                        �
*����������������������������������������������������������� 12.09.2000 �ͼ
PROCEDURE RynBoxes
PARAMETERS d_num, d_date

PRIVATE s_sav, mss, path_as, cus_sav
s_sav = SELECT()
path_as = ALLTRIM(Get_Sys("PATH_AS"))
IF EMPTY(path_as)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��� ����㯠 � ᯨ�� ��஡��"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

SELECT 0
USE (base_path+"RYN_M_T") AGAIN ALIAS SOURCE_T ORDER TAG DOC_NUM
= SEEK(STR(YEAR(d_date),4)+STR(d_num,5))
cus_sav = CUS_CODE
IF EMPTY(TECHNID)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"  �� 㢥७�, �� �� � ���������? [ �� ] [ ��� ]"
  mss(3) = ""
  IF Out_Mess(7, "mss") = 2
    USE
    SELECT (s_sav)
    RETURN
  ENDIF
ELSE
  SELECT 0
  USE (path_as+"BOXNO") ORDER TAG TECHNID
  IF SEEK(SOURCE_T.TECHNID)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"��஡�� ��� �⮩ ��������� 㦥 ��ନ஢���!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    USE
    SELECT SOURCE_T
    USE
    SELECT (s_sav)
    RETURN
  ENDIF
  USE
  SELECT SOURCE_T
ENDIF
DO BoxesTmp
SELECT BOXESTMP
IF RECCOUNT() = 0
  USE
  DELETE FILE (tmpo_path+"BOXESTMP.DBF")
  SELECT MATERS
  USE
  SELECT BOXNO
  USE
  SELECT SOURCE_T
  USE
  SELECT (s_sav)
  RETURN
ENDIF

DO Prp_List

DO F_Box

SELECT SOURCE_T
USE
SELECT MATERS
USE
SELECT BOXNO
USE
SELECT BOXESTMP
USE
SELECT BOXCONT
USE
SELECT BOXDISTR
USE
DELETE FILE (tmpo_path+"BOXESTMP.DBF")
DELETE FILE (tmpo_path+"BOXESTMP.CDX")
DELETE FILE (tmpo_path+"BOXCONT.DBF")
DELETE FILE (tmpo_path+"BOXCONT.CDX")
DELETE FILE (tmpo_path+"BOXDISTR.DBF")
DELETE FILE (tmpo_path+"BOXDISTR.CDX")

SELECT (s_sav)

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Prp_List     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                       �����⮢�� ࠡ�祣� 䠩��.                       �
*�                                                                        �
*����������������������������������������������������������� 12.09.2000 ���
PROCEDURE Prp_List

SELECT 0
DELETE FILE (tmpo_path+"BOXCONT.DBF")
DELETE FILE (tmpo_path+"BOXCONT.CDX")
CREATE DBF (tmpo_path+"BOXCONT") ;
  ( BOX_REF   N(10), ;
    CODE      N( 7), ;
    PURSYS    C( 1), ;
    IDL       N(10), ;
    QNT       N( 6) )
INDEX ON STR(BOX_REF)+STR(CODE) TAG CODE

SELECT 0
DELETE FILE (tmpo_path+"BOXDISTR.DBF")
DELETE FILE (tmpo_path+"BOXDISTR.CDX")
CREATE DBF (tmpo_path+"BOXDISTR") ;
  ( CODE      N( 7), ;
    PREFIX    C( 4), ;
    NAME      C(35), ;
    PRODUCER  C( 8), ;
    PURSYS    C( 1), ;
    IDL       N(10), ;
    QNT       N( 6), ;
    QNT_READY N( 6), ;
    QNT_BOX   N( 6)  )
INDEX ON CODE TAG CODE
INDEX ON NAME+PREFIX+PRODUCER TAG NAME ADDITIVE
SET ORDER TO TAG CODE

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS ST_0912 AGAIN
SELECT 0
USE (base_path+"RYN_M") ORDER TAG DOC_NUM ALIAS SOURCE_D AGAIN
= SEEK(STR(YEAR(SOURCE_T.DOC_DATE),4)+STR(SOURCE_T.DOC_NUM,5))
SCAN REST WHILE SOURCE_T.DOC_DATE = DOC_DATE .AND. SOURCE_T.DOC_NUM = DOC_NUM
  SELECT BOXDISTR
  IF .NOT. SEEK(SOURCE_D.CODE)
    = SEEK(SOURCE_D.CODE, "ST_0912")
    APPEND BLANK
    REPLACE CODE      WITH SOURCE_D.CODE,  ;
            PURSYS    WITH SOURCE_D.PURSYS,;
            IDL       WITH SOURCE_D.IDL,   ;
            PREFIX    WITH ST_0912.PREFIX, ;
            NAME      WITH ST_0912.NAME,   ;
            PRODUCER  WITH ST_0912.PRODUCER
  ENDIF
  REPLACE QNT  WITH QNT+SOURCE_D.QNT
  SELECT SOURCE_D
ENDSCAN

USE
SELECT ST_0912
USE
SELECT BOXDISTR

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Fil_Boxes    ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                          ���������� ��஡��.                           �
*�                                                                        �
*����������������������������������������������������������� 12.09.2000 ���
PROCEDURE F_Box

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
DO Use_Dummy
SELECT 0

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Box OF (menu_name) PROMPT "��஡��" KEY Ctrl-A, "Tab"
ON SELECTION PAD Box OF (menu_name) DO Sw_Mode WITH "Box"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
wd = 67

DO D_Wins WITH ln, wd, "��᪫���� �� ��஡���: ���-� N "+  ;
               ALLTRIM(STR(SOURCE_T.DOC_NUM))+" �� "+DTOC(SOURCE_T.DOC_DATE), 2, 0
what_do = "List"
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS ACC0912 AGAIN
= SEEK(SOURCE_T.CUS_CODE)
@ 1, 1 SAY CUS_NAME
USE
@ 2,  1 SAY "��஡��"
@ 2,  9 SAY ALLTRIM(BOXNO.OFFICE)+ALLTRIM(STR(BOXNO.BOXNO))+" "+BOXNO.MASK+SPACE(20)
@ 3,  2 SAY "��č�����������������������������������Ăᥣ��Đ���.�Ď���.Ă ���."

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    ACTIVATE WINDOW (win_name+"_M") SAME
    @ 2,  9 SAY ALLTRIM(BOXNO.OFFICE)+ALLTRIM(STR(BOXNO.BOXNO))+" "+BOXNO.MASK+SPACE(20)
    DO Prp_Nav_1
    SELECT BOXDISTR
    SET ORDER TO TAG NAME
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Tab   DO Sw_Mode WITH "Box"

*
*    BROWSE - ����
*
    BROWSE FIELDS PREFIX:H="",  ;
                  NAME:H="",    ;
                  PRODUCER:H="",;
                  QNT:H="":P="999999", ;
                  QNT001 = QNT_READY+QNT_BOX:H="":P="999999", ;
                  QNT002 = QNT-QNT_READY-QNT_BOX:H="":P="999999", ;
                  QNT_BOX:H="":P="999999":V=Tst_Qnt()    ;
           FREEZE QNT_BOX ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "List")
    ENDIF

  CASE what_do = "Box"    && ����� ��஡��

    DO Ch_Box    
    what_do = "List"

  OTHERWISE

    IF .NOT. SaveCont()
      EXIT
    ENDIF
    what_do = "List"

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   �����⭥�쪮 (�����⭥�쪮!) ����뢠�� DBF-䠩��.
*
SELECT (s_sav)

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Tst_Qnt      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                          �஢�ઠ ������⢠.                          �
*�                                                                        �
*����������������������������������������������������������� 12.09.2000 ���
PROCEDURE Tst_Qnt
PRIVATE mss

IF QNT-QNT_READY-QNT_BOX < 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"������ ������⢠ ��� � ���������!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF QNT_BOX < 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"������⢮ �� ����� ���� ����⥫��!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Ch_Box       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                             ����� ��஡��.                             �
*�                                                                        �
*����������������������������������������������������������� 12.09.2000 ���
PROCEDURE Ch_Box

DO To_Box
DO BoxesTmp
DO From_Box

*������������������������������������������������������������������������Ŀ
*�   ��楤�� To_Box       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                               � ��஡��.                               �
*�                                                                        �
*����������������������������������������������������������� 12.09.2000 ���
PROCEDURE To_Box

SELECT BOXDISTR
SCAN FOR QNT_BOX # 0
  SELECT BOXCONT
  IF .NOT. SEEK("         0")
    APPEND BLANK
  ENDIF
  REPLACE BOX_REF WITH BOXESTMP.REF_NO,;
          CODE    WITH BOXDISTR.CODE,  ;
          PURSYS  WITH BOXDISTR.PURSYS,;
          IDL     WITH BOXDISTR.IDL,   ;
          QNT     WITH BOXDISTR.QNT_BOX
  SELECT BOXDISTR
  REPLACE QNT_READY WITH QNT_READY+QNT_BOX
  REPLACE QNT_BOX   WITH 0
ENDSCAN

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� From_Box       ���ࠡ��稪 ���३ ��ᨭ                    �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                               �� ��஡��.                              �
*�                                                                        �
*����������������������������������������������������������� 12.09.2000 ���
PROCEDURE From_Box

SELECT BOXDISTR
SET ORDER TO CODE
SELECT BOXCONT
DO WHILE SEEK(STR(BOXESTMP.REF_NO))
  SELECT BOXDISTR
  SEEK BOXCONT.CODE
  REPLACE QNT_READY WITH QNT_READY-BOXCONT.QNT, ;
          QNT_BOX   WITH BOXCONT.QNT
  SELECT BOXCONT
  REPLACE BOX_REF WITH 0
ENDDO
SELECT BOXDISTR
SET ORDER TO NAME

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� SaveCont     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                    ���࠭��� �᪫���� �� ��஡���.                    �
*�                                                                        �
*����������������������������������������������������������� 13.09.2000 ���
PROCEDURE SaveCont

PRIVATE mss, s_sav, r_sav, to_work, cont_work

to_work   = .T.
cont_work = .F.
*
*  �஢��塞 �� �� ࠧ������?
*
s_sav = SELECT()
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE AGAIN ALIAS ST_0913
SELECT BOXDISTR
r_sav = RECNO()
SCAN
  IF QNT_READY+QNT_BOX # QNT
    DIMENSION mss(4)
    mss(1) = ""
    mss(2) = CHR(0)+"�� �� ����樨 ��������� ࠧ������. �� �㤥� ������?"
    mss(3) = CHR(0)+"  [ �த����� ] [ �����訬 ]"
    mss(4) = ""
    IF Out_Mess(7,"mss") = 1
      to_work   = .F.
      cont_work = .T.
    ELSE
      to_work   = .F.
      cont_work = .F.
    ENDIF
    EXIT
  ENDIF
ENDSCAN
IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF
SELECT (s_sav)

IF to_work
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"  ���࠭��� �᪫���� �� ��஡���? [ �� ] [ ��� ]"
  mss(3) = ""
  IF Out_Mess(5,"mss") = 1
    PRIVATE tmpIdl, tmpLgS
    to_work = .T.
    cont_work = .F.
    DO To_Box
    SELECT 0
    USE (path_as+"LOGSTOCK")
    SELECT BOXESTMP
    SET RELATION TO
    SELECT BOXCONT
    SCAN FOR BOX_REF # 0
      IF BOX_REF # RECNO("BOXNO")
        GO BOXCONT.BOX_REF IN BOXNO
      ENDIF
      SELECT ST_0913
      = SEEK(BOXCONT.CODE)
      SELECT MATERS
*      tmpIdl = IDL_MT
      tmpLgS = LGSTCODE
      REPLACE LGSTCODE WITH LGSTCODE+1   && IDL_MT WITH IDL_MT+1,
      SELECT LOGSTOCK
      APPEND BLANK
      REPLACE PURSYS    WITH BOXCONT.PURSYS,  ;
              SUBSYS    WITH MATERS.SUBSYS,   ;
              IDL       WITH BOXCONT.IDL,     ;
              LGCODE    WITH tmpLgS,          ;
              STO_CODE  WITH BOXCONT.CODE,    ;
              PREFIX    WITH ST_0913.PREFIX,  ;
              NAME      WITH ST_0913.NAME,    ;
              PRODUCER  WITH ST_0913.PRODUCER,;
              DATE_ON   WITH DATE(),          ;
              DATE_MOVE WITH DATE(),          ;
              QTY       WITH BOXCONT.QNT,     ;
              BOX_YEAR  WITH BOXNO.BOX_YEAR,  ;
              CUS_CODE  WITH cus_sav,         ;
              OFFICE    WITH BOXNO.OFFICE,    ;
              BOXNO     WITH BOXNO.BOXNO
      SELECT BOXCONT
    ENDSCAN
    SELECT LOGSTOCK
    USE
  ELSE
    cont_work = .F.
    to_work = .F.
  ENDIF
ENDIF

IF .NOT. cont_work .AND. .NOT. to_work
  SELECT BOXESTMP
  SCAN
    SELECT BOXNO
    REPLACE TECHNID WITH ""
    SELECT BOXESTMP
  ENDSCAN
ENDIF

SELECT ST_0913
USE
SELECT (s_sav)

RETURN cont_work