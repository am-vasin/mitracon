*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Sal_P_M      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�             ｡螢� �牀��� �� 甌矗磴�┴�� �發カ� �� �ム┏�.              �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 07/05/1999 様�
PROCEDURE Sal_P_M
PARAMETER own_dep
PRIVATE dep_c, d_0, d_1

dep_c = 0
IF own_dep
  USE (base_path+"PERSONS") SHARED AGAIN ALIAS TMP_2110 ORDER TAG CODE
  SEEK user
  dep_c = DEP_NO
  USE
ENDIF
d_0 = {}
d_1 = DATE()
IF .NOT. Fill_P()
  RETURN
ENDIF

DO Wt_Mess WITH "��ぁ�爲� ����諷"
DO Prep_Tmp
DO Wt_Mess

DO View_D

USE
DELETE FILE (tmpo_path+"SAL_P_M.DBF")
DELETE FILE (tmpo_path+"SAL_P_M.CDX")

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Fill_P       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           �襦�� �����モ牀�.                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 05.07.1999 陳�
PROCEDURE Fill_P
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�              敖陳陳陳陳陳陳陳朕        �
*�        �發カ �....:....!....:.�        �
*�              青陳陳陳陳陳陳陳潰        �
*� 敖 ��皀燿�� ��� 陳陳陳陳陳陳陳陳陳陳朕 �
*� � � ��.��.����(F1) �� ��.��.����(F1) � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�    < OK Ctrl-W > < �皖���碎瘴 Esc >    �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE d_names, d_codes, d_qnt, s_sav, let_dep, tmpDep
let_dep = dep_c = 0
tmpDep = dep_c
s_sav = SELECT()
SELECT 0
USE (base_path+"DEPS") ORDER TAG B_NAME ALIAS DP_9705 AGAIN
d_qnt = 0
SCAN
  d_qnt = d_qnt+1
  DIMENSION d_names(d_qnt), d_codes(d_qnt)
  d_names(d_qnt) = B_NAME
  d_codes(d_qnt) = CODE
  IF CODE = tmpDep
    dep_c = d_qnt
  ENDIF
ENDSCAN
IF let_dep
  dep_c = 1
ENDIF
USE
SELECT (s_sav)
IF d_qnt = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�モ �� �き��� ��Гカ�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 10, 44, "�������皀 ゛���"
@ 3, 10 SAY "�發カ"
DO Sun_Bord WITH  5,  3,  7, 41, " ��皀燿�� ��� "
@ 6, 5 SAY "�           (F1) ��           (F1)"
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@ 2, 16 GET dep_c PICTURE "@^" FROM d_names SIZE 3,18 WHEN let_dep
@ 6,  7 GET d_0 WHEN Set_F1() VALID Clr_F1()
@ 6, 25 GET d_1 WHEN Set_F1() VALID Clr_F1()

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE
dep_c = d_codes(dep_c)
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1
 
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Set_F1       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                       �痰���→� �÷�｀砒┴� F1.                        �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 05.07.1999 陳�
PROCEDURE Set_F1

ON KEY LABEL F1 DO Int_Date

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Clr_F1       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                        �腮痰�� �÷�｀砒┴� F1.                         �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 05.07.1999 陳�
PROCEDURE Clr_F1

ON KEY LABEL F1

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Int_Date     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                     �痰���→� ��硅 �� ���キ��瑟.                       �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 05.07.1999 陳�
PROCEDURE Int_Date

IF "D_0" $ OBJVAR()
  d_0 = Get_Date(d_0)
  SHOW GET d_0
ELSE
  d_1 = Get_Date(d_1)
  SHOW GET d_1
ENDIF

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Prep_Tmp     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      ��ぃ�皰→� ∇ガキ���� �����.                      �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 05.07.1999 陳�
PROCEDURE Prep_Tmp

PRIVATE m_list, s_sav, n, q0, q1, s
s_sav = SELECT()
SELECT 0
USE (base_path+"PERSONS") ALIAS P_9705 AGAIN
m_list = ""
SCAN FOR DEP_NO = dep_c
  m_list = m_list+STR(CODE,5)+"!"
ENDSCAN
SET ORDER TO TAG CODE
SELECT 0
DELETE FILE (tmpo_path+"SAL_P_M.DBF")
DELETE FILE (tmpo_path+"SAL_P_M.CDX")
CREATE DBF (tmpo_path+"SAL_P_M") ;
 ( CODE  N( 5),   ;
   NAME  C(28),   ;
   POS   N( 5),   ;
   QNT   N( 8),   ;
   SUM_D N(15,4), ;
   PRC   N( 7,3) )
INDEX ON CODE TAG CODE
SELECT 0
USE (base_path+"SALE") ORDER TAG DOC_NUM AGAIN ALIAS D_9705
SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG HRON AGAIN ALIAS T_9705
IF .NOT. SEEK(DTOS(d_0))
  n = RECNO(0)
  IF BETWEEN(n, 1, RECCOUNT())
    GO n
  ENDIF
ENDIF
s = 0
SCAN REST WHILE DOC_DATE <= d_1 FOR STR(WHO,5) $ m_list .AND. T_9705.TOTAL_D > 0
  q0 = 0
  q1 = 0
  SELECT D_9705
  SEEK T_9705.FIRM+LEFT(DTOS(T_9705.DOC_DATE),4)+T_9705.DOC_NUM
  SCAN REST WHILE FIRM=T_9705.FIRM .AND. DOC_DATE=T_9705.DOC_DATE .AND. DOC_NUM=T_9705.DOC_NUM
    q0 = q0+1
    q1 = q1+QNT
  ENDSCAN
  SELECT SAL_P_M
  IF .NOT. SEEK(T_9705.WHO)
    APPEND BLANK
    REPLACE CODE WITH T_9705.WHO
  ENDIF
  REPLACE POS   WITH POS+q0, ;
          QNT   WITH QNT+q1, ;
          SUM_D WITH SUM_D+T_9705.TOTAL_D
  SELECT T_9705
  s = s+T_9705.TOTAL_D
ENDSCAN
USE
SELECT D_9705
USE
SELECT SAL_P_M
SET RELATION TO CODE INTO P_9705

SCAN
  REPLACE NAME WITH P_9705.FAMILY-(" "+LEFT(P_9705.NAME,1))-(" "+LEFT(P_9705.S_NAME,1)), ;
          PRC  WITH SUM_D/s*100
ENDSCAN
SET RELATION TO
SELECT P_9705
USE
SELECT SAL_P_M
INDEX ON NAME TAG NAME
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� View_D       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                         蹍甃�矗 爛с�赳�皰�.                          �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 05.07.1999 陳�
PROCEDURE View_D

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
SELECT Sal_P_M

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"
*
*   ��瘍モ ��Кム�� ����
*
*�蹍��▲翊陳陳陳陳陳陳陳陳陳陳����.陳陳�矣�陳% �祠�訥��祠�� �牀�.�
*�....:....!....:....!....:... NNNNN NNNNNNNN NNNNNNN NNNNNNN.NNNN�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
PRIVATE ln, wd
ln = MIN(RECCOUNT(), WROWS("")-11)   && ����腑痰〓 ※え�諷 痰牀� BROWSE
ln = MAX(1, ln)
wd = 64

DO D_Wins WITH ln, wd, "｡螢� �牀��� �� �牀��∽��", 0, 0
@ 1, 2 SAY "蹍��▲翊陳陳陳陳陳陳陳陳陳陳����.陳陳�矣�陳% �祠�訥��祠�� �牀�."
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
*    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE FIELDS NAME:28:H="",   ;
                  POS:5:H="",     ;
                  QNT:8:H="",     ;
                  PRC:H="":P="999.999", ;
                  SUM_D:H="":P="9999999.9999",;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
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
SELECT (s_sav)

RETURN
