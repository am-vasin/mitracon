*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Rep_Flgs     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�     ���� �� ��த���� � �ᯮ�짮������ ���짮��⥫�᪨� 䫠����.      �
*�                                                                        �
*����������������������������������������������������������� 08.02.2000 �ͼ
PROCEDURE Rep_Flgs
PARAMETERS depParm

PRIVATE s_sav, dep_c, dep_n, dep_list, dep_codes, let_dep, n_deps, p_dep
PRIVATE mss, flg_str, d_0, d_1, n_flags

IF .NOT. File_O(base_path+"FS_FLAGS.MEM")
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� �� ���ᠫ� �� ������ 䫠���!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF
RESTORE FROM (base_path+"FS_FLAGS.MEM") ADDITIVE
IF EMPTY(fs_names(1))
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� �� ���ᠫ� �� ������ 䫠���!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF
n_flags = 0
FOR n_deps = 1 TO ALEN(fs_names)
  IF EMPTY(fs_names(n_deps))
    EXIT
  ENDIF
  n_flags = n_flags+1
ENDFOR
flg_str = SPACE(ALEN(fs_names))

IF TYPE("depParm") = "N"
  dep_c = depParm
ELSE
  dep_c = 0
ENDIF

s_sav = SELECT()
SELECT 0
USE (base_path+"DEPS") ORDER TAG CODE ALIAS DEP_0208
IF dep_c > 0
  IF .NOT. SEEK(dep_c)
    dep_c = -1
  ELSE
    dep_n = B_NAME
  ENDIF
ENDIF

SET ORDER TO TAG B_NAME
n_deps = 0
SCAN
  n_deps = n_deps+1
  DIMENSION dep_list(n_deps), dep_codes(n_deps)
  dep_list(n_deps)  = B_NAME
  dep_codes(n_deps) = CODE
ENDSCAN

USE
IF n_deps = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� �� ᮧ���� �� ������ �⤥�� �।�����!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SELECT (s_sav)
  RETURN
ENDIF

p_dep = 0
IF dep_c = 0      && �롨ࠥ� �⤥� �� ���� ���짮��⥫�
  USE (base_path+"PERSONS") ORDER TAG CODE
  IF SEEK(user)
    dep_c = DEP_NO
  ENDIF
  USE
ENDIF
IF dep_c > 0
  p_dep = ASCAN(dep_codes, dep_c)
ENDIF
n_dep = ""
IF p_dep > 0
  let_dep = .F.
  n_dep = dep_list(p_dep)
ELSE
  p_dep = 1
  let_dep = .T.
ENDIF

STORE DATE() TO d_0, d_1

IF .NOT. G_Parms()
  RETURN
ENDIF

DO Prep_Tmp

DO View_Rep

SELECT REP_FLGS
USE
DELETE FILE (tmpo_path+"REP_FLGS.DBF")
DELETE FILE (tmpo_path+"REP_FLGS.CDX")

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� G_Parms      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                         ���������� ��ࠬ��஢.                         �
*�                                                                        �
*����������������������������������������������������������� 08.02.2000 ���
PROCEDURE G_Parms
PRIVATE d0, d1
d0 = d_0
d1 = d_1
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

*��������������������������������Ŀ
*�          ����������������Ŀ    �
*�    �⤥� �....:....!....:.�    �
*�          ������������������    �
*� ��砫쭠� ��� ��.��.���� (F1) �
*� ����筠� ���  ��.��.���� (F1) �
*�    ����������������������Ŀ    �
*�    � [ ] ....:....!....:. �    �
*�    ������������������������    �
*�< OK Ctrl-W > < �⪠������ Esc >�
*����������������������������������

PRIVATE ex, tmp_flg, i
DIMENSION tmp_flg(ALEN(fs_names))

ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 11+n_flags, 36, "��������� �����"
DO Sun_Bord WITH 7, 6, WROWS()-3, 29, " ������ "
*------------------------------------------------------------------------
*      ���� ����� ������
*
@ 3, 6 SAY "�⤥�"
@ 5, 3 SAY "��砫쭠� ���            (F1)"
@ 6, 3 SAY "����筠� ���             (F1)"

@ 2, 12 GET p_dep PICTURE "@^" FROM dep_list SIZE 1, 18 WHEN let_dep
@ 5, 18 GET d0 PICTURE "@D" WHEN Set_F1(1) VALID Clr_F1()
@ 6, 18 GET d1 PICTURE "@D" WHEN Set_F1(2) VALID Clr_F1()
FOR i = 1 TO n_flags
  @ 7+i, 8 GET tmp_flg(i) PICTURE "@*C "+fs_names(i)
ENDFOR

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE VALID Tst_Blank()

*--------------------------------------------------------------------------
FOR i = 1 TO n_flags
  IF tmp_flg(i)
    flg_str = STUFF(flg_str,i,1,"*")
  ENDIF
ENDFOR
d_0 = d0
d_1 = d1
POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Set_F1       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                       ��⠭���� ��ࠡ��稪� F1.                        �
*�                                                                        �
*����������������������������������������������������������� 04/30/1999 ���
PROCEDURE Set_F1
PARAMETERS v

IF v = 1
  ON KEY LABEL F1 DO Int_D WITH 1
ELSE
  ON KEY LABEL F1 DO Int_D WITH 2
ENDIF
RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Clr_F1       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                        ���⪠ ��ࠡ��稪� F1.                         �
*�                                                                        �
*����������������������������������������������������������� 04/30/1999 ���
PROCEDURE Clr_F1

ON KEY LABEL F1

RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Int_D        ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                            �맮� ���������.                            �
*�                                                                        �
*����������������������������������������������������������� 30.04.1999 ���
PROCEDURE Int_D
PARAMETERS v

IF v = 1
  d0 = Get_Date(d0)
  SHOW GETS && d0
ELSE
  d1 = Get_Date(d1)
  SHOW GETS && d1
ENDIF

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Tst_Blank    ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                    �஢�ઠ ���४⭮�� ���ࢠ��.                    �
*�                                                                        �
*����������������������������������������������������������� 30.04.1999 ���
PROCEDURE Tst_Blank

PRIVATE mss, i, sw

IF ex # 1
  RETURN .T.
ENDIF

sw = .T.
FOR i = 1 TO n_flags
  IF tmp_flg(i)
    sw = .F.
    EXIT
  ENDIF
ENDFOR

IF sw
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� �� ��⠭����� �� ������ 䫠���!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF d0 > d1
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��砫쭠� ��� ����� ����筮�? ��� �� �뢠��!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Prep_Tmp     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                      �����⮢�� �६������ 䠩��.                      �
*�                                                                        �
*����������������������������������������������������������� 08.02.2000 ���
PROCEDURE Prep_Tmp

PRIVATE nr, q0, q1

DELETE FILE (tmpo_path+"REP_FLGS.DBF")
DELETE FILE (tmpo_path+"REP_FLGS.CDX")

CREATE DBF (tmpo_path+"REP_FLGS.DBF")  ;
  ( SAL_CODE N( 5), ;
    SAL_NAME C(30), ;
    QNT_INV  N( 6), ;
    QNT_POS  N( 6), ;
    QNT      N( 8), ;
    SUM_D    N(15,4), ;
    SUM_R    N(15,2)  )
INDEX ON SAL_CODE TAG SAL_CODE

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS P_0208 AGAIN

SELECT 0
USE (base_path+"SALE") ORDER TAG DOC_NUM ALIAS S_0208 AGAIN

SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG HRON ALIAS ST_0208 AGAIN
SET RELATION TO WHO INTO P_0208, ;
                FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM INTO S_0208

nr = SET("NEAR") = "OFF"
SET NEAR ON
SEEK DTOS(d_0)
SCAN REST WHILE DOC_DATE <= d_1
  IF P_0208.DEP_NO # dep_c
    LOOP
  ENDIF
  IF .NOT. Flag_OK()
    LOOP
  ENDIF
  q0 = 0
  q1 = 0
  SELECT S_0208
  SCAN REST WHILE FIRM = ST_0208.FIRM .AND. ;
                  DOC_NUM = ST_0208.DOC_NUM .AND. ;
                  DOC_DATE = ST_0208.DOC_DATE
    q0 = q0+1
    q1 = q1+QNT
  ENDSCAN
  SELECT REP_FLGS
  IF .NOT. SEEK(P_0208.CODE)
    APPEND BLANK
    REPLACE SAL_CODE WITH P_0208.CODE, ;
            SAL_NAME WITH ALLTRIM(P_0208.FAMILY)+" "+ ;
                          LEFT(P_0208.NAME,1)+" "+    ;
                          LEFT(P_0208.S_NAME,1)
  ENDIF
  REPLACE QNT_INV WITH QNT_INV+1, ;
          QNT_POS WITH QNT_POS+q0,;
          QNT     WITH QNT+q1,    ;
          SUM_D   WITH SUM_D+ST_0208.TOTAL_D, ;
          SUM_R   WITH SUM_R+ST_0208.TOTAL_R
  SELECT ST_0208
ENDSCAN

USE

SELECT S_0208
USE

SELECT P_0208
USE

SELECT REP_FLGS
INDEX ON SAL_NAME TAG SAL_NAME
IF nr
  SET NEAR OFF
ENDIF

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Flg_OK       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                           �஢�ઠ 䫠����.                            �
*�                                                                        �
*����������������������������������������������������������� 08.02.2000 ���
PROCEDURE Flag_OK
PRIVATE retVal, i

retVal = .F.

FOR i = 1 TO n_flags
  IF .NOT. EMPTY(SUBSTR(flg_str, i, 1)) .AND. ;
     .NOT. EMPTY(SUBSTR(SAL_FLAGS, i, 1))
    retVal = .T.
    EXIT
  ENDIF
ENDFOR

RETURN retVal

*������������������������������������������������������������������������Ŀ
*�   ��楤�� View_Rep     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                            ��ᬮ�� ����.                            �
*�                                                                        �
*����������������������������������������������������������� 09.02.2000 ���
PROCEDURE View_Rep


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
SELECT REP_FLGS

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Money OF (menu_name) PROMPT "�����" KEY Ctrl-A, "Tab"
ON SELECTION PAD Money OF (menu_name) DO Sw_Mode WITH "Money"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
wd = 71
*   �⤥� XXXXXXXXXXXXXXXX
*   [ ] XXXXXXXXXXXXXXXX  [ ] XXXXXXXXXXXXXXXX  [ ] XXXXXXXXXXXXXXXX
*   [ ] XXXXXXXXXXXXXXXX  [ ] XXXXXXXXXXXXXXXX  [ ] XXXXXXXXXXXXXXXX
*   [ ] XXXXXXXXXXXXXXXX  [ ] XXXXXXXXXXXXXXXX
*�ďத��������������������������č���.��ď��.��Ċ��-�����đ㬬�(� �)�Ŀ
*�XXXXXXXXXXXXXXXXXXXXXXXXXX X X 999999 999999 99999999 9999999999.9999�
*�����������������������������������������������������������������������
PRIVATE is_UE, tit, i, j, p
tit = "�த��� �� ��ਮ� � "+DTOC(d_0)+" �� "+DTOC(d_1)
is_UE = .T.
DO D_Wins WITH ln, wd, tit, 1+CEILING(n_flags/3), 0
p = 1
@ 1, 4 SAY "�⤥� "+n_dep
FOR i = 1 TO 3
  FOR j = 1 TO 3
    IF p > n_flags
      EXIT
    ENDIF
    @ 1+i, 4+(j-1)*22 SAY IIF(EMPTY(SUBSTR(flg_str, p, 1)), "[ ] ", "[X] ")+ ;
                          fs_names(p)
    p = p+1
  ENDFOR
ENDFOR
@ CEILING(n_flags/3)+2, 2 SAY ;
"ďத��������������������������č���.��ď��.��Ċ��-�����đ㬬�(� �)"

what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    ACTIVATE WINDOW (win_name+"_M") SAME
    @ CEILING(n_flags/3)+2, 64 SAY IIF(is_UE,"(� �)","(��)")
    DO Prp_Nav_1
    ON KEY LABEL Tab DO Sw_Mode WITH "Money"

*
*    BROWSE - ����
*

    IF is_UE
      BROWSE FIELDS SAL_NAME:H="",      ;
                    QNT_INV:P="999999", ;
                    QNT_POS:P="999999", ;
                    QNT:P="99999999",   ;
                    SUM_D:P="9999999999.9999", ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             NOMODIFY   ;
             NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ELSE
      BROWSE FIELDS SAL_NAME:H="",      ;
                    QNT_INV:P="999999", ;
                    QNT_POS:P="999999", ;
                    QNT:P="99999999",   ;
                    SUM_R:P="9999999999.9999", ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             NOMODIFY   ;
             NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ENDIF
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Exit")
    ENDIF

  CASE what_do = "Money"    && ����� ������

    is_UE = .NOT. is_UE
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
