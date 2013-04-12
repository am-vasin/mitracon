*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Cmp_Docs     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                     �ࠢ����� 蠡����� ���㬥�⮢.                     �
*�                                                                        �
*����������������������������������������������������������� 11.10.1999 �ͼ
PROCEDURE Cmp_Docs

PRIVATE shab_path, is_det

shab_path = "\"

shab_path = Int_Dir(shab_path, "��४��� � �⠫����")
IF EMPTY(shab_path)
  RETURN
ENDIF

IF .NOT. File_O(shab_path+"DOC_FORM.DBF")
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��� ��� 蠡����� ���㬥�⮢!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

USE (shab_path+"DOC_FORM.DBF") ALIAS PATTERN
SELECT 0
USE (base_path+"DOC_FORM.DBF")
is_det = FSIZE("DETAIL") # 0

DO Prep_Tmp

DO View_D

USE
DELETE FILE (tmpo_path+"CMP_DOCS.DBF")
DELETE FILE (tmpo_path+"CMP_DOCS.CDX")
SELECT PATTERN
USE
SELECT DOC_FORM
USE

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Prep_Tmp     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                      �����⮢�� �६������ 䠩��.                      �
*�                                                                        �
*����������������������������������������������������������� 11.10.1999 ���
PROCEDURE Prep_Tmp

PRIVATE w0, w1, diff_f
SELECT 0
DELETE FILE (tmpo_path+"CMP_DOCS.DBF")
DELETE FILE (tmpo_path+"CMP_DOCS.CDX")

CREATE DBF (tmpo_path+"CMP_DOCS.DBF") ;
  ( DOC_NAME   C(10),  ;
    DOC_PROMPT C(40),  ;
    LINK_DOC   N(10),  ;
    LINK_SHAB  N(10),  ;
    EQ_FIELDS  C( 3),  ;
    EQ_DIC     C( 3),  ;
    EQ_DIC_D   C( 3)   )
SELECT PATTERN
SCAN
  SELECT CMP_DOCS
  APPEND BLANK
  REPLACE DOC_NAME   WITH PATTERN.DOC_NAME,    ;
          DOC_PROMPT WITH PATTERN.DOC_PROMPT,  ;
          LINK_SHAB  WITH RECNO("PATTERN"),    ;
          EQ_FIELDS  WITH " � ",               ;
          EQ_DIC     WITH " � ",               ;
          EQ_DIC_D   WITH " � "
  SELECT PATTERN
ENDSCAN
SELECT CMP_DOCS
INDEX ON DOC_NAME TAG DOC_NAME
SELECT DOC_FORM
SCAN
  SELECT CMP_DOCS
  IF .NOT. SEEK(DOC_FORM.DOC_NAME)
    APPEND BLANK
    REPLACE DOC_NAME   WITH DOC_FORM.DOC_NAME,    ;
            DOC_PROMPT WITH DOC_FORM.DOC_PROMPT,  ;
            LINK_DOC   WITH RECNO("DOC_FORM")
  ELSE
    w0 = LINK_SHAB
    GO w0 IN PATTERN
    w0 = DOC_FORM.DICTION
    w1 = PATTERN.DICTION
    diff_f = PATTERN.DOC_HF  # DOC_FORM.DOC_HF .OR. ;
             PATTERN.DOC_FF  # DOC_FORM.DOC_FF .OR. ;
             PATTERN.PAGE_HF # DOC_FORM.PAGE_HF .OR. ;
             PATTERN.PAGE_FF # DOC_FORM.PAGE_FF
    REPLACE LINK_DOC   WITH RECNO("DOC_FORM"), ;
            EQ_DIC     WITH IIF(w0==w1, "   ", " � ")

    IF is_det
      w0 = DOC_FORM.DIC_DET
      w1 = PATTERN.DIC_DET
      REPLACE EQ_DIC_D   WITH IIF(w0==w1, "   ", " � ")
      diff_f = diff_f .OR. PATTERN.DETAILF  # DOC_FORM.DETAILF .OR. ;
               PATTERN.DETAIL1F  # DOC_FORM.DETAIL1F
    ENDIF
    REPLACE EQ_FIELDS  WITH IIF(diff_f, " � ", "")
  ENDIF
  SELECT DOC_FORM
ENDSCAN
SELECT CMP_DOCS
SET ORDER TO
GO TOP

RETURN    

*������������������������������������������������������������������������Ŀ
*�   ��楤�� View_D       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                         ��ᬮ�� ��宦�����.                          �
*�                                                                        �
*����������������������������������������������������������� 11.10.1999 ���
PROCEDURE View_D

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
SELECT CMP_DOCS
*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Rem OF (menu_name) PROMPT "��ࠢ���" KEY Ctrl-A, "F1"
ON SELECTION PAD Rem OF (menu_name) DO Sw_Mode WITH "Rem"

DEFINE PAD View1 OF (menu_name) PROMPT "������" KEY Ctrl-A, "F2"
ON SELECTION PAD View1 OF (menu_name) DO Sw_Mode WITH "View1"

IF is_det
  DEFINE PAD View2 OF (menu_name) PROMPT "��⠫�� ��ப�" KEY Ctrl-A, "F3"
  ON SELECTION PAD View2 OF (menu_name) DO Sw_Mode WITH "View2"
ENDIF

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
ln = WROWS("")-8   && ������⢮ ������� ��ப BROWSE
wd = IIF(is_det, 71, 67)
*ڊ�� ���-�č������� ���㬥������������������������Ą��Ď������đ�.�
*�********** **************************************** *** *** *** ***�
*���������������������������������������������������������������������

DO D_Wins WITH ln, wd, "������� ���㬥�⮢", 0, 0
@ 1, 2 SAY "��� ���-�č������� ���㬥������������������������Ą��Ď������đ�."
IF is_det
  @ ROW(), COL() SAY "Ą��"
ENDIF
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL F1 DO Sw_Mode WITH "Rem"
    ON KEY LABEL F2 DO Sw_Mode WITH "View1"
    IF is_det
      ON KEY LABEL F3 DO Sw_Mode WITH "View2"
    ENDIF

*
*    BROWSE - ����
*
    IF is_det
      BROWSE FIELDS DOC_NAME:H="",  ;
                    DOC_PROMPT:H="",  ;
                    D0 = IIF(EMPTY(LINK_DOC), " � ", "   "):H="",  ;
                    D1 = IIF(EMPTY(LINK_SHAB), " � ", "   "):H="",  ;
                    EQ_FIELDS:H="",  ;
                    EQ_DIC:H="",  ;
                    EQ_DIC_D:H="",  ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ELSE
      BROWSE FIELDS DOC_NAME:H="",  ;
                    DOC_PROMPT:H="",  ;
                    D0 = IIF(EMPTY(LINK_DOC), " � ", "   "):H="",  ;
                    D1 = IIF(EMPTY(LINK_SHAB), " � ", "   "):H="",  ;
                    EQ_FIELDS:H="",  ;
                    EQ_DIC:H="",  ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ENDIF

    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "View1"    && ��ᬮ�� ᫮��३

    DO V_Dict
    what_do = "List"

  CASE what_do = "View2"    && ��ᬮ�� ᫮��३

    DO V_Dict WITH .T.
    what_do = "List"

  CASE what_do = "Rem"      && ����⠭�������

    DO Rem
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
*   �����⭥�쪮 (�����⭥�쪮!) ����뢠�� DBF-䠩��.
*
SELECT (s_sav)

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� V_Dict       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                           ��ᬮ�� ᫮��३.                           �
*�                                                                        �
*����������������������������������������������������������� 11.10.1999 ���
PROCEDURE V_Dict
PARAMETER md

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

*������������������������������������������������������������������������Ŀ
*��đ������ ���㬥�����������������Ŀ�đ������ 蠡���������������������Ŀ�
*��                                  ��                                  ��
*��                                  ��                                  ��
*��                                  ��                                  ��
*��                                  ��                                  ��
*��                                  ��                                  ��
*��                                  ��                                  ��
*��                                  ��                                  ��
*��                                  ��                                  ��
*��                                  ��                                  ��
*��                                  ��                                  ��
*��                                  ��                                  ��
*��                                  ��                                  ��
*��                                  ��                                  ��
*��                                  ��                                  ��
*������������������������������������������������������������������������ٳ
*�                                 < OK >                                 �
*��������������������������������������������������������������������������

PRIVATE ex, s0, s1
s0 = ""
s1 = ""
IF LINK_DOC # 0
  ex = LINK_DOC
  GO ex IN DOC_FORM
  IF md
    s0 = DOC_FORM.DIC_DET
  ELSE
    s0 = DOC_FORM.DICTION
  ENDIF
ENDIF
IF LINK_SHAB # 0
  ex = LINK_SHAB
  GO ex IN PATTERN
  IF md
    s1 = PATTERN.DIC_DET
  ELSE
    s1 = PATTERN.DICTION
  ENDIF
ENDIF

ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 20, 76, ALLTRIM(DOC_PROMPT)+IIF(md, "(���.)", "")
DO Sun_Bord WITH  2,  2,  WROWS()-3, 37, "������� ���㬥��"
DO Sun_Bord WITH  2, 38,  WROWS()-3, 73, "������� 蠡����"

*------------------------------------------------------------------------
*      ���� ����� ������
*
@ 3,  3 EDIT s0 SIZE 14, 34 NOMODIFY
@ 3, 39 EDIT s1 SIZE 14, 34 NOMODIFY

@ WROWS()-2, 35 GET ex PICTURE "@*HT \! OK "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Rem          ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                         ����⠭������� �����.                         �
*�                                                                        �
*����������������������������������������������������������� 11.10.1999 ���
PROCEDURE Rem

IF EMPTY(LINK_SHAB)
  RETURN
ENDIF

PRIVATE w0, w1, w2, w3, w4, w5, w6, w7, buff
PRIVATE sw0, sw1, sw2, sw3, sw4, sw5

IF EMPTY(LINK_DOC)
  SELECT PATTERN
  GO CMP_DOCS.LINK_SHAB
  SCATTER TO buff
  w0 = DOC_H
  w1 = DOC_F
  w2 = PAGE_H
  w3 = PAGE_F
  w4 = DICTION
  sw0 = DOC_HF
  sw1 = DOC_FF
  sw2 = PAGE_HF
  sw3 = PAGE_FF

  IF FSIZE("DETAIL") # 0
    w5 = DIC_DET
    w6 = DETAIL
    w7 = DETAIL1
    sw4 = DETAILF
    sw5 = DETAIL1F
  ENDIF

  SELECT DOC_FORM
  APPEND BLANK
  GATHER FROM buff
  REPLACE DOC_H    WITH w0,  ;
          DOC_F    WITH w1,  ; 
          PAGE_H   WITH w2,  ;
          PAGE_F   WITH w3,  ;
          DICTION  WITH w4,  ;
          DOC_HF   WITH sw0, ;
          DOC_FF   WITH sw1, ; 
          PAGE_HF  WITH sw2, ;
          PAGE_FF  WITH sw3

  IF FSIZE("DETAIL") # 0
    REPLACE DIC_DET  WITH w5, ;
            DETAIL   WITH w6, ;
            DETAIL1  WITH w7, ;
            DETAILF  WITH sw4, ;
            DETAIL1F WITH sw5
  ENDIF
  SELECT CMP_DOCS
  REPLACE LINK_DOC WITH RECNO("DOC_FORM"), ;
          EQ_DIC   WITH "   ", ;
          EQ_DIC_D WITH "   "
ELSE
  IF .NOT. EMPTY(LINK_SHAB)
    SELECT PATTERN
    GO CMP_DOCS.LINK_SHAB
    w0 = DOC_H
    w1 = DOC_F
    w2 = PAGE_H
    w3 = PAGE_F
    w4 = DICTION
    sw0 = DOC_HF
    sw1 = DOC_FF
    sw2 = PAGE_HF
    sw3 = PAGE_FF
    IF FSIZE("DETAIL") # 0
      w5 = DIC_DET
      sw4 = DETAILF
      sw5 = DETAIL1F
    ENDIF
    SELECT DOC_FORM
    GO CMP_DOCS.LINK_DOC
    REPLACE DICTION WITH w4,  ;
            DOC_HF  WITH sw0, ;
            DOC_FF  WITH sw1, ;
            PAGE_HF WITH sw2, ;
            PAGE_FF WITH sw3

    IF FSIZE("DETAIL") # 0
      REPLACE DIC_DET  WITH w5, ;
              DETAILF  WITH sw4,;
              DETAIL1F WITH sw5
              
    ENDIF

    SELECT CMP_DOCS
    REPLACE EQ_DIC    WITH "   ", ;
            EQ_FIELDS WITH "   ", ;
            EQ_DIC_D  WITH "   "
  ENDIF
ENDIF

RETURN