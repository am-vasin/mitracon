*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Rev_Inc      ��о�｀砒┴ ��むォ ��瓱�           06.05.97 14:58:44 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�              蹍������ �爬絎ぎ����� 皰���� �� ���祚�皀��.              �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Rev_Inc

PRIVATE mss, d_names, d_codes, n, w, i000

*
*   ���甌� ��Гカ�� ���キ���矣琺
*
USE (base_path+"DIVISION")
n = 0
SCAN
  n = n+1
  DIMENSION w(n,2)
  w(n,1) = B_NAME
  w(n,2) = CODE
ENDSCAN
= ASORT(w,1)

DIMENSION d_names(n), d_codes(n)
FOR i000 = 1 TO n
  d_codes(i000) = w(i000,2)
  d_names(i000) = w(i000,1)
ENDFOR
RELEASE w, i000

USE
DO Use_Dummy
DO Use_Link

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE

SELECT 0
USE (base_path+"INC_TIT")

SELECT 0
USE (base_path+"ACT_S")

SELECT 0
USE (base_path+"ACT_S_T")

SELECT 0
USE (base_path+"LIST_DET") ORDER TAG DUE_CODE

* NEW 04.01.2000 - � ≡��※� 甄イ. 2 痰牀�� 艪�° �� °�� �荐！�
* �爬 ���祚�� 皰���� ("�荐！� 13 Alias DUE_NAM not found
* � �牀������ INC_LIST, 痰牀�� 169") ��� 轤� 痰牀��:
* SET RELATION TO NAME+PREFIX+PRODUCER INTO DUE_NAM ADDITIVE

SELECT 0
USE (base_path+"LIST_DET") ORDER TAG DUE_NAM ALIAS DUE_NAM AGAIN

SELECT 0
USE (base_path+"INC_LIST") ORDER TAG L_CODE
DELETE FILE (tmpo_path+"TMP_INC.DBF")
COPY STRUCTURE TO (tmpo_path+"TMP_INC")
SELECT 0
USE (tmpo_path+"TMP_INC") EXCLUSIVE

SELECT 0
USE (base_path+"INC_DUE") ORDER TAG L_CODE

SELECT 0
USE (base_path+"INC_NEW") ORDER TAG L_NAME
DELETE FILE (tmpo_path+"TMP_NEW.DBF")
COPY STRUCTURE TO (tmpo_path+"TMP_NEW")
SELECT 0
USE (tmpo_path+"TMP_NEW") EXCLUSIVE
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
stat_type  = 3
what_do    = "list"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Corr OF (menu_name) PROMPT "��玻." KEY Ctrl-A, "Enter"
ON SELECTION PAD Corr OF (menu_name) Do Sw_Mode WITH "Corr"

DEFINE PAD Add OF (menu_name) PROMPT "���." KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) Do Sw_Mode WITH "Add"

DEFINE PAD Del OF (menu_name) PROMPT "����." KEY Ctrl-A, "Del"
ON SELECTION PAD Del OF (menu_name) Do Sw_Mode WITH "Del"

DEFINE PAD Price OF (menu_name) PROMPT "�キ�" KEY Ctrl-A, "F2"
ON SELECTION PAD Price OF (menu_name) Do Sw_Mode WITH "Price"

DEFINE PAD Act OF (menu_name) PROMPT "踸�." KEY Ctrl-A, "Tab"
ON SELECTION PAD Act OF (menu_name) Do Sw_Mode WITH "Act"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷." KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = WROWS("")-10   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 33+IIF(SET("CENTURY")="ON", 10, 8)

DO D_Wins WITH ln, wd, "����皰→� �爬ガ�諷 ��皰�", 0, 0
what_do = "List"
@ 1,  3 SAY " ��痰�∫┴ "
@ 1, 20 SAY " ����. N "
@ 1, 31 SAY " ���� "
@ 1, IIF(SET("CENTURY")="ON", 41, 39) SAY "�キ�"


DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    SELECT INC_TIT
    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins   Do Sw_Mode WITH "Add"
    ON KEY LABEL Del   Do Sw_Mode WITH "Del"
    ON KEY LABEL F2    Do Sw_Mode WITH "Price"
    ON KEY LABEL Tab   Do Sw_Mode WITH "Act"

*
*    BROWSE - �キ�
*
    BROWSE FOR SUP_CODE # 0   ;
           FIELDS INC_TIT.SUP_NAME:H="":16,     ;
                  INC_TIT.INC_DOC:H="":10,     ;
                  INC_TIT.INC_DATE:H="",     ;
                  IS_PRICE:H="":4,     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND REST NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "exit", "Corr")
    ENDIF

  CASE what_do = "Corr"    && ��玻オ罔� �爬ガ���� ����
    IF BETWEEN(RECNO(), 1, RECCOUNT())
      DO Inc_List WITH .T.
    ENDIF
    what_do = "List"

  CASE what_do = "Add"     && ����←キ┘ ����皰→�

    DO Rev_Tit
    what_do = "List"

  CASE what_do = "Del"     && ����キ┘ ����皰→�

    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"  �ォ痰※皀�讚� 磴���碎? [ �� ] [ �モ ]"
    mss(3) = ""
    IF Out_Mess(7, "mss") = 1
      DO Del_Tit
    ENDIF

    what_do = "List"

  CASE what_do = "Price"   && �痰���→� 爛Θ��

    DO Prices
    what_do = "List"

  CASE what_do = "Act"     && 踸絎�

    DO Prep_Act
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

CLEAR WINDOWS
CLOSE DATABASES
*DELETE FILE (tmpo_path+"TMP_INC.DBF")
*DELETE FILE (tmpo_path+"TMP_NEW.DBF")

RELEASE MENU (menu_name) EXTENDED

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                           �痰���→� 爛Θ��.                            �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 13.05.97 21:43:41 夕
PROCEDURE Prices
*
*  �ムガキ�襯 甌痰�鎰�� か� ��※��罔�
*

IF .NOT. Get_L()
  RETURN
ENDIF

REPLACE INC_TIT.IS_PRICE WITH IIF(EMPTY(INC_TIT.IS_PRICE), "�", " "),  ;
        INC_TIT.LOCKED   WITH "***"

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                           ��紜�皋碎 甎�甌�!                            �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 07.05.97 11:22:51 夕
PROCEDURE Get_L

PRIVATE mss, sel_sav, arm_name

IF INC_TIT.LOCKED # "***"
  sel_sav = SELECT()
  SELECT 0
  USE (base_path+"ARM_LST") ORDER TAG ARM_C ALIAS ARM000 AGAIN
  IF SEEK(INC_TIT.LOCKED)
    arm_name = ALLTRIM(ARM_N)
    DIMENSION mss(4)
    mss(1) = ""
    mss(2) = CHR(0)+"��������! �皰� 甎�甌� � ��｀皀, ��� "+'"'+arm_name+'"!'
    mss(3) = CHR(0)+"  ��� ��痰祚━? [ 鉑碎 ��｀��鈞 ] [ ����ア�碎! ]"
    mss(4) = ""
    IF Out_Mess(7, "mss") = 1
      USE
      SELECT (sel_sav)
      RETURN .F.
    ENDIF
  ENDIF
  USE
  SELECT (sel_sav)
ENDIF

REPLACE INC_TIT.LOCKED WITH arm

RETURN .T.
