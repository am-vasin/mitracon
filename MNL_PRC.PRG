*������������������������������������������������������������������������ͻ
*� ���� Order        ���ࠡ��稪 ���३ ��ᨭ           24.04.98 17:20:08 �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                   ��筠� ���४�� 業 ��� 䨫����.                    �
*�                                                                        �
*������������������������������������������������������������������������ͼ
PROCEDURE Mnl_Prc
PARAMETER f_code

PRIVATE s_sav, mss, pic

s_sav = SELECT()
SELECT 0
USE (base_path+"RYN_L_T") ORDER TAG CUS_CODE AGAIN ALIAS RT
IF .NOT. SEEK(f_code)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� 䨫����� ��祣� �� �᫨���."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  RETURN
ENDIF
pic = IIF(VALUTA = "$", "99999.9999", "9999999.99")
SELECT 0
USE (base_path+"RYN_L") ORDER TAG CUS_CODE AGAIN ALIAS RL
IF .NOT. SEEK(f_code)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� 䨫����� ��祣� �� �᫨���."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT RT
  USE
  SELECT (s_sav)
  RETURN
ENDIF

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE AGAIN ALIAS ST

SELECT 0
DELETE FILE (tmpo_path+"MNL_PRC.DBF")
DELETE FILE (tmpo_path+"MNL_PRC.CDX")
CREATE DBF (tmpo_path+"MNL_PRC") ;
  ( CODE     N( 7), ;
    PREFIX   C( 4), ;
    NAME     C(35), ;
    PRODUCER C( 8), ;
    QNT      N( 6), ;
    PRICE    N(9,4) )
SELECT RL
SET RELATION TO CODE INTO ST
DO Wt_Mess WITH "�����ઠ ������������ 䨫����..."
SCAN REST WHILE CUS_CODE = f_code FOR QNT # 0
  SELECT MNL_PRC
  APPEND BLANK
  REPLACE CODE     WITH RL.CODE, ;
          PREFIX   WITH ST.PREFIX, ;
          NAME     WITH ST.NAME, ;
          PRODUCER WITH ST.PRODUCER, ;
          QNT      WITH RL.QNT, ;
          PRICE    WITH IIF(RT.VALUTA = "$", RL.PRICE_D, RL.PRICE_R)
  SELECT RL
ENDSCAN
DO Wt_Mess
SET RELATION TO
SELECT MNL_PRC
INDEX ON NAME+PREFIX+PRODUCER TAG NAME
SET RELATION TO CODE INTO ST

DO Modi_Pr    && �� ��⮢�, ����㯠�� � ���४樨
DIMENSION mss(3)
mss(1) = ""
mss(2) = CHR(0)+"  ���࠭�� ���������? [ �� ] [ ��� ]"
mss(3) = ""
IF Out_Mess(5, "mss") = 1
  SELECT RL
  SET ORDER TO TAG C_CODE
  pic = STR(f_code,5)
  SELECT MNL_PRC
  SCAN FOR SEEK(pic+STR(CODE, 7), "RL")
    SELECT RL
    IF RT.VALUTA = "$"
      REPLACE PRICE_D WITH MNL_PRC.PRICE
    ELSE
      REPLACE PRICE_R WITH MNL_PRC.PRICE
    ENDIF
    SELECT MNL_PRC
  ENDSCAN
ENDIF

SELECT MNL_PRC
USE
DELETE FILE (tmpo_path+"MNL_PRC.DBF")
DELETE FILE (tmpo_path+"MNL_PRC.CDX")

SELECT RL
USE

SELECT RT
USE

SELECT ST
USE

SELECT (s_sav)

RETURN

*������������������������������������������������������������������������ͻ
*�                                                                        �
*�                         ���४�� ᯨ᪠ 業.                          �
*�                                                                        �
*����������������������������������������������������� 20.06.98 14:14:24 ͼ
PROCEDURE Modi_Pr
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

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD C_Seek OF (menu_name) PROMPT "���⥪��. ����" KEY Ctrl-A, "F7"
ON SELECTION PAD C_Seek OF (menu_name) DO Sw_Mode WITH "C_Seek"

DEFINE PAD Discount OF (menu_name) PROMPT "������"
ON PAD Discount OF (menu_name) ACTIVATE POPUP Discount

  DEFINE POPUP Discount MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR 1 OF Discount PROMPT "�� �ᥬ� ������" KEY Ctrl-A, "F2"
  ON SELECTION BAR 1 OF Discount DO Sw_Mode WITH "Dis_H"

  DEFINE BAR 2 OF Discount PROMPT "�� ᯨ��" KEY Ctrl-A, "Ctrl-F2"
  ON SELECTION BAR 2 OF Discount DO Sw_Mode WITH "Dis_L"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
ln = 12   && ������⢮ ������� ��ப BROWSE
wd = 75

DO D_Wins WITH ln, wd, ALLTRIM(RT.CUS_NAME), 1, 1
@ 2, 3 SAY ;
"��������ď����������������������������đ�������đ�.業�Ĕ�������Ė��� 䨫"
@ 1, 2 SAY "���� Tab"

what_do = "List"
SELECT MNL_PRC
GO TOP

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL F7  DO Sw_Mode WITH "C_Seek"
    ON KEY LABEL F2  DO Sw_Mode WITH "Dis_H"
    ON KEY LABEL Ctrl+F2  DO Sw_Mode WITH "Dis_L"
    ON KEY LABEL Tab DO Seek_1

*
*    BROWSE - ����
*
    BROWSE FIELDS PREFIX:H="", ;
           Brw_Name=Nm_Part(NAME):25:H="", ;
           PRODUCER:H="",  ;
           ST.QNT:H="":P="@Z 999999", ;
           F000 = IIF(RT.VALUTA = "$", ST.PRICE, ST.PRICE_R):H="":P=pic, ;
           QNT:H="":P="@Z 999999", ;
           PRICE:H="":P=pic ;
           FREEZE PRICE ;
           WHEN Draw_Foot()    ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = "Exit"
    ENDIF

  CASE what_do = "C_Seek"    && ���⥪��� ����

    DO C_Seek
    what_do = "List"

  CASE what_do = "Dis_H"     && ������ �� �ᥬ� ������

    DO Dis_H
    what_do = "List"

  CASE what_do = "Dis_L"     && ������ �� ᯨ��

    DO Dis_L
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

RETURN

*������������������������������������������������������������������������ͻ
*�                                                                        �
*�                      ���� �� ��砫�� ᨬ�����!                      �
*�                                                                        �
*����������������������������������������������������� 13.04.98 11:51:11 ͼ
PROCEDURE Seek_1
PRIVATE flt_pos, max_len, str_flt, s_n, s_c, ins_flt
PRIVATE chr_shab, i, pos_new, str_new, to_seek, rc_sav

DEFINE WINDOW Seek_Hide FROM 0, 0 TO 0, WCOLS("")-1 NONE COLOR SCHEME 13
SHOW WINDOW Seek_Hide
flt_pos = 0
max_len = FSIZE("NAME")-1
str_flt = ""
s_n = 0
s_c = ""
ins_flt = INSMODE()
chr_shab = ""
FOR i = 32 TO 126
  chr_shab = chr_shab+CHR(i)
ENDFOR
FOR i = 128 TO 175
  chr_shab = chr_shab+CHR(i)
ENDFOR
FOR i = 224 TO 241
  chr_shab = chr_shab+CHR(i)
ENDFOR

ACTIVATE WINDOW (win_name+"_M") SAME

DO WHILE .T.
  to_seek = .F.
  @ 1, 8 SAY PADR(str_flt, max_len+1, "�") COLOR SCHEME 1
  @ 1, 8+flt_pos SAY ""
  s_n = INKEY(0)
  IF s_n < 0       && �㭪樮���쭠� ������ - ���� ⠧���!
    LOOP
  ENDIF
  s_c = CHR(s_n)

  IF s_c $ chr_shab    && ����� ᨬ���!
    str_new = LEFT(str_flt,flt_pos)+s_c+SUBSTR(str_flt,flt_pos+IIF(ins_flt,1,2))
    str_new = LEFT(str_new, max_len+1)
    pos_new = MIN(flt_pos+1, max_len)
    to_seek = .T.
  ENDIF

  IF s_n = 19          && ��५�� �����
    flt_pos = MAX(flt_pos-1,0)
  ENDIF

  IF s_n = 4           && ��५�� ��ࠢ�
    flt_pos = MIN(flt_pos+1, LEN(str_flt), max_len)
  ENDIF

  IF s_n = 7           && Del
    str_new = LEFT(str_flt,flt_pos)+SUBSTR(str_flt,flt_pos+2)
    pos_new = flt_pos
    to_seek = .T.
  ENDIF

  IF s_n = 127         && BackSpace
    str_new = LEFT(str_flt,flt_pos-1)+SUBSTR(str_flt,flt_pos+1)
    pos_new = MAX(flt_pos-1,0)
    to_seek = .T.
  ENDIF

  IF s_n = 22          && Ins
    ins_flt = .NOT. ins_flt
    = INSMODE(ins_flt)
  ENDIF

  IF s_n = 1           && Home
    flt_pos = 0
  ENDIF

  IF s_n = 6           && End
    flt_pos = MIN(LEN(str_flt), max_len)
  ENDIF

  IF s_n = 13          && Enter
    EXIT
  ENDIF

  IF INLIST(s_n, 24, 5, 3, 18)
    DO CASE
    CASE s_n = 24  && ��५�� ����
      i = 1
    CASE s_n = 5   && ��५�� �����
      i = -1
    CASE s_n = 3   && PgDn
      i = WROWS(win_name+"_E")-1
    OTHERWISE    && PgUp
      i = -WROWS(win_name+"_E")+1
    ENDCASE
    SKIP i
    IF BOF()
      GO TOP
    ENDIF
    IF EOF()
      GO BOTTOM
    ENDIF
    SHOW WINDOW (ALIAS()) REFRESH SAME
  ENDIF

  IF to_seek
    rc_sav = RECNO()
    IF SEEK(str_new)
      str_flt = str_new
      flt_pos = pos_new
      SHOW WINDOW (ALIAS()) REFRESH SAME
    ELSE
      IF BETWEEN(rc_sav, 1, RECCOUNT())
        GO rc_sav
      ENDIF
    ENDIF
  ENDIF

ENDDO

RELEASE WINDOW Seek_Hide

@ 1, 8 SAY PADR("Tab", max_len+1)

ACTIVATE WINDOW (ALIAS()) SAME

RETURN

*������������������������������������������������������������������������ͻ
*�                                                                        �
*�                           ���⥪��� ����!                           �
*�                                                                        �
*����������������������������������������������������� 20.06.98 11:51:11 ͼ
PROCEDURE C_Seek

PRIVATE s_sav, shab, rc_sav

shab = Get_Shab()
IF EMPTY(shab)
  RETURN
ENDIF

s_sav = SELECT()

SELECT 0
DELETE FILE(tmpo_path+"C_TMP.DBF")
CREATE DBF (tmpo_path+"C_TMP.DBF") ;
  ( N_REC    N(10), ;
    PREFIX   C( 4), ;
    NAME     C(35), ;
    PRODUCER C( 8), ;
    C_DUMMY  C( 1)  )
SELECT (s_sav)
rc_sav = RECNO()
DO Wt_Mess WITH "����..."
SCAN FOR shab $ NAME
  SELECT C_TMP
  APPEND BLANK
  REPLACE N_REC    WITH RECNO("MNL_PRC"), ;
          PREFIX   WITH MNL_PRC.PREFIX, ;
          NAME     WITH MNL_PRC.NAME, ;
          PRODUCER WITH MNL_PRC.PRODUCER
  SELECT MNL_PRC
ENDSCAN
DO Wt_Mess
SELECT C_TMP
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��祣� �� �������..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  DELETE FILE(tmpo_path+"C_TMP.DBF")
  SELECT (s_sav)
  IF BETWEEN(rc_sav, 1, RECCOUNT())
    GO rc_sav
  ENDIF
  RETURN
ENDIF

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

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�롮�" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
ln = MIN(100, RECCOUNT())   && ������⢮ ������� ��ப BROWSE
wd = 39

DO D_Wins WITH ln, wd, "�⮡࠭� �� ���⥪���", 0, 1
what_do = "List"

GO TOP
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - ����
*
    BROWSE FIELDS PREFIX:H="", ;
           Brw_Name=Nm_Part(NAME):25:H="", ;
           PRODUCER:H="",   ;
           C_DUMMY:H="" FREEZE C_DUMMY ;
           WHEN Draw_Foot(.T.)    ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && �롮�

    SELECT (s_sav)
    GO C_TMP.N_REC
    EXIT

  OTHERWISE

    SELECT (s_sav)
    IF BETWEEN(rc_sav, 1, RECCOUNT())
      GO rc_sav
    ENDIF
    EXIT

  ENDCASE

ENDDO

SELECT C_TMP
USE
DELETE FILE(tmpo_path+"C_TMP.DBF")
SELECT (s_sav)

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

RETURN

*������������������������������������������������������������������������ͻ
*�                                                                        �
*�                       ����� 蠡���� ��� ���᪠!                       �
*�                                                                        �
*����������������������������������������������������� 20.06.98 11:51:11 ͼ
PROCEDURE Get_Shab

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

*     ������ 蠡��� ��� ���᪠
*����������������������������������Ŀ
*�                                  �
*�    ....:....!....:....!....:     �
*�                                  �
*� < OK Ctrl-W > < �⪠������ Esc > �
*������������������������������������

PRIVATE ex, sb       &&
                     &&   ���塞 � ������塞 ���� ������
ex = 1
sb = ""
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 7, 38, "������ 蠡��� ��� ���᪠"

*------------------------------------------------------------------------
*      ���� ����� ������
*
@ 3, 6 EDIT sb SIZE 1, 25 COLOR (SCHEME(1,1))
@ 5, 3 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN IIF(ex = 1, sb, "")

*������������������������������������������������������������������������ͻ
*�                                                                        �
*�                   ��⠭���� ᪨��� �� �ᥬ� ������                    �
*�                                                                        �
*����������������������������������������������������� 20.06.98 11:51:11 ͼ
PROCEDURE Dis_H
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

* ������ �� �ᥬ� ������ � 䨫����
*����������������������������������Ŀ
*�                                  �
*� ��業� ᪨��� (��業��) 999.99  �
*�                                  �
*� < OK Ctrl-W > < �⪠������ Esc > �
*������������������������������������

PRIVATE ex, prc, rc_sav
ex  = 1
prc = 0
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 7, 38, "������ �� �ᥬ� ������ � 䨫����"

*------------------------------------------------------------------------
*      ���� ����� ������
*
@ 3, 3 SAY "��業� ᪨��� (��業��)" GET prc PICTURE "@Z 999.99"
@ 5, 3 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE

IF ex = 1 .AND. prc # 0
  rc_sav = RECNO()
  REPLACE ALL PRICE WITH PRICE*(100-prc)/100
  IF BETWEEN(rc_sav, 1, RECCOUNT())
    GO rc_sav
  ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*������������������������������������������������������������������������ͻ
*�                                                                        �
*�                        ��⠭���� ᪨��� ᯨ��                         �
*�                                                                        �
*����������������������������������������������������� 20.06.98 11:51:11 ͼ
PROCEDURE Dis_L
*
*  ��६���� ���ﭨ� ��� ������樨
*

PRIVATE s_sav, list_n, list_c, n, mss
s_sav = SELECT()
SELECT 0
n = 0
IF File_O(base_path+"WK_TIT.DBF")
  USE (base_path+"WK_TIT") ALIAS WK AGAIN
  SCAN
    n = n+1
    DIMENSION list_n(n), list_c(n)
    list_n(n) = L_NAME
    list_c(n) = RECNO()
  ENDSCAN
  USE
ENDIF

IF n = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��� �� ������ ᯨ᪠-����⮢��."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SELECT (s_sav)
  RETURN
ENDIF

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

*             ������ �� ᯨ��
*��������������������������������������������Ŀ
*� ����������������������������������������Ŀ �
*� �....:....!....:....!....:....!....:....!� �
*� ������������������������������������������ �
*�      ��業� ᪨��� (��業��) 999.99       �
*�      < OK Ctrl-W > < �⪠������ Esc >      �
*����������������������������������������������

PRIVATE ex, point, prc, rc_sav, www
point = 1
ex  = 1
prc = 0
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 8, 48, "������ �� ᯨ��"

*------------------------------------------------------------------------
*      ���� ����� ������
*
@ 2, 3 GET point PICTURE "@^" FROM list_n SIZE 1, 42
@ 5, 8 SAY "��業� ᪨��� (��業��)" GET prc PICTURE "@Z 999.99"
@ 6, 8 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE

SELECT (s_sav)
IF ex = 1 .AND. prc # 0
  rc_sav = RECNO()
  SELECT 0
  USE (base_path+"WK_LIST") ALIAS WL AGAIN ORDER TAG L_CODE
  SELECT (s_sav)
  www = STR(list_c(point),5)
  SCAN FOR SEEK(www+STR(CODE,7),"WL")
    REPLACE PRICE WITH PRICE*(100-prc)/100
  ENDSCAN
  SELECT WL
  USE
  SELECT (s_sav)
  IF BETWEEN(rc_sav, 1, RECCOUNT())
    GO rc_sav
  ENDIF
ENDIF

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
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
PARAMETER brif

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� �����, ��㥬...
*

ACTIVATE WINDOW (ALIAS()) SAME
IF brif
  @ WROWS()-2, 3 SAY NAME COLOR SCHEME 1
ELSE
  @ WROWS()-2, 3 SAY PREFIX COLOR SCHEME 1
  @ WROWS()-2, COL()+1 SAY NAME COLOR SCHEME 1
  @ WROWS()-2, COL()+1 SAY PRODUCER COLOR SCHEME 1
ENDIF

RETURN .T.