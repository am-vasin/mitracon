*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Rep_Flgs     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�     �砒モ �� ��牀����� � �甎��讌����┘� ���讌���皀�赱��� 筰�Κ��.      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 08.02.2000 様�
PROCEDURE Rep_Flgs
PARAMETERS depParm

PRIVATE s_sav, dep_c, dep_n, dep_list, dep_codes, let_dep, n_deps, p_dep
PRIVATE mss, flg_str, d_0, d_1, n_flags

IF .NOT. File_O(base_path+"FS_FLAGS.MEM")
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� �� ������� �� �き��� 筰�Κ�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF
RESTORE FROM (base_path+"FS_FLAGS.MEM") ADDITIVE
IF EMPTY(fs_names(1))
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� �� ������� �� �き��� 筰�Κ�!"
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
  mss(2) = CHR(0)+"�� �� 甌Г��� �� �き��� �發カ� �爛く爬閧��!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SELECT (s_sav)
  RETURN
ENDIF

p_dep = 0
IF dep_c = 0      && �襦���ガ �發カ �� ��ゃ ���讌���皀��
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� G_Parms      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                         ������キ┘ �����モ牀�.                         �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 08.02.2000 陳�
PROCEDURE G_Parms
PRIVATE d0, d1
d0 = d_0
d1 = d_1
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
*�          敖陳陳陳陳陳陳陳朕    �
*�    �發カ �....:....!....:.�    �
*�          青陳陳陳陳陳陳陳潰    �
*� �����讚�� ���� ��.��.���� (F1) �
*� ���ョ��� ����  ��.��.���� (F1) �
*�    敖陳陳陳陳陳陳陳陳陳陳朕    �
*�    � [ ] ....:....!....:. �    �
*�    青陳陳陳陳陳陳陳陳陳陳潰    �
*�< OK Ctrl-W > < �皖���碎瘴 Esc >�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, tmp_flg, i
DIMENSION tmp_flg(ALEN(fs_names))

ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 11+n_flags, 36, "������┘� ゛���"
DO Sun_Bord WITH 7, 6, WROWS()-3, 29, " ���Κ� "
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 3, 6 SAY "�發カ"
@ 5, 3 SAY "�����讚�� ����            (F1)"
@ 6, 3 SAY "���ョ��� ����             (F1)"

@ 2, 12 GET p_dep PICTURE "@^" FROM dep_list SIZE 1, 18 WHEN let_dep
@ 5, 18 GET d0 PICTURE "@D" WHEN Set_F1(1) VALID Clr_F1()
@ 6, 18 GET d1 PICTURE "@D" WHEN Set_F1(2) VALID Clr_F1()
FOR i = 1 TO n_flags
  @ 7+i, 8 GET tmp_flg(i) PICTURE "@*C "+fs_names(i)
ENDFOR

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Set_F1       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                       �痰���→� �÷�｀砒┴� F1.                        �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 04/30/1999 陳�
PROCEDURE Set_F1
PARAMETERS v

IF v = 1
  ON KEY LABEL F1 DO Int_D WITH 1
ELSE
  ON KEY LABEL F1 DO Int_D WITH 2
ENDIF
RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Clr_F1       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                        �腮痰�� �÷�｀砒┴� F1.                         �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 04/30/1999 陳�
PROCEDURE Clr_F1

ON KEY LABEL F1

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Int_D        ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                            �襷�� ���キ��瑙.                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 30.04.1999 陳�
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Tst_Blank    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                    蹍▲爲� ��玻オ皚�痰� ┃皀燿���.                    �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 30.04.1999 陳�
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
  mss(2) = CHR(0)+"�� �� 竅����※�� �� �き��� 筰�Κ�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF d0 > d1
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�����讚�� ���� ｀�跏� ���ョ���? ��� �� °��モ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Prep_Tmp     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      ��ぃ�皰→� ∇ガキ���� �����.                      �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 08.02.2000 陳�
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Flg_OK       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           蹍▲爲� 筰�Κ��.                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 08.02.2000 陳�
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� View_Rep     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                            蹍甃�矗 �砒モ�.                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 09.02.2000 陳�
PROCEDURE View_Rep


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
SELECT REP_FLGS

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Money OF (menu_name) PROMPT "���鈞�" KEY Ctrl-A, "Tab"
ON SELECTION PAD Money OF (menu_name) DO Sw_Mode WITH "Money"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 71
*   �發カ XXXXXXXXXXXXXXXX
*   [ ] XXXXXXXXXXXXXXXX  [ ] XXXXXXXXXXXXXXXX  [ ] XXXXXXXXXXXXXXXX
*   [ ] XXXXXXXXXXXXXXXX  [ ] XXXXXXXXXXXXXXXX  [ ] XXXXXXXXXXXXXXXX
*   [ ] XXXXXXXXXXXXXXXX  [ ] XXXXXXXXXXXXXXXX
*敖蹍��▲翊陳陳陳陳陳陳陳陳陳陳陳����.陳����.陳����-〓陳陳�祠��(� �)陳�
*�XXXXXXXXXXXXXXXXXXXXXXXXXX X X 999999 999999 99999999 9999999999.9999�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
PRIVATE is_UE, tit, i, j, p
tit = "蹍��Θ �� �ム┏� � "+DTOC(d_0)+" �� "+DTOC(d_1)
is_UE = .T.
DO D_Wins WITH ln, wd, tit, 1+CEILING(n_flags/3), 0
p = 1
@ 1, 4 SAY "�發カ "+n_dep
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
"�蹍��▲翊陳陳陳陳陳陳陳陳陳陳陳����.陳����.陳����-〓陳陳�祠��(� �)"

what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    ACTIVATE WINDOW (win_name+"_M") SAME
    @ CEILING(n_flags/3)+2, 64 SAY IIF(is_UE,"(� �)","(�磧)")
    DO Prp_Nav_1
    ON KEY LABEL Tab DO Sw_Mode WITH "Money"

*
*    BROWSE - �キ�
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

  CASE what_do = "Money"    && ��キ� ���鈞�

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
*   ���窶�皚キ讓� (���窶�皚キ讓�!) ���琺��ガ DBF-�����.
*
SELECT (s_sav)

RETURN

