*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�         蹍������ 筮爼�牀����� �皀����讚��� �爬ガ���� ����.           �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Sum_Acts
PRIVATE mkrs, d_list, i, n

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

DEFINE PAD Work OF (menu_name) PROMPT "��爼�牀���┘ 甎�瓷�" KEY Ctrl-A, "Enter"
ON SELECTION PAD Work OF (menu_name) Do Sw_Mode WITH "Work"

DEFINE PAD Mark OF (menu_name) PROMPT "�皙モ��" KEY Ctrl-A, "蹍．�"
ON SELECTION PAD Mark OF (menu_name) Do Sw_Mode WITH "Inv_Mark"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*

DO Use_Dummy

SELECT 0
USE ACT_S_T ORDER TAG DOC_NUM
GO BOTT
*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = MIN(100, RECCOUNT())   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = FSIZE("DOC_NUM")+IIF(SET("DATE")="ON", 13, 11)

DO D_Wins WITH ln, wd, "踸ガ�襯 ��硅", 0, 0
what_do = "list"

mrks = ""
DO WHILE .T.

  DO CASE

  CASE what_do = "list"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL SpaceBar DO Inv_Mark

*
*    BROWSE - �キ�
*
    BROWSE FIELDS M000 = IIF(STR(RECNO(),10) $ mrks, "�", " "):H="",     ;
           DOC_NUM:H="",     ;
           DOC_DATE:H="",    ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "list"
      what_do = IIF(READKEY() % 256 = 12, "exit", "Work")
    ENDIF

  CASE what_do = "Work"      && ��爼�牀���┘ 甎�瓷�

    CLEAR WINDOWS
    RELEASE MENU (menu_name) EXTENDED
    IF .NOT. EMPTY(mrks)
      SELECT ACT_S_T
      n = 0
      DO WHILE .NOT. EMPTY(mrks)
        i = VAL(ALLTRIM(LEFT(mrks,10)))
        mrks = SUBSTR(mrks, 11)
        GO i
        n = n+1
        DIMENSION d_list(n, 2)
        d_list(n, 1) = DOC_NUM
        d_list(n, 2) = DOC_DATE
      ENDDO
      CLOSE DATABASES
      SET CURSOR OFF
      DO Wt_Mess WITH "��ぃ�皰→� ����諷"
      DO Prep_F WITH d_list
      DO Wt_Mess
      SET CURSOR ON
    ENDIF
    EXIT

  CASE what_do = "Inv_Mark"  && �痰���→�/甅閧┘ ���モ��

    DO Inv_Mark
    what_do = "list"

  OTHERWISE

    CLEAR WINDOWS
    CLOSE DATABASES
    RELEASE MENU (menu_name) EXTENDED
    EXIT

  ENDCASE

ENDDO

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�             蹍������ ┃▲珥�牀����� �皙モ��.                         �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Inv_Mark
PRIVATE i, w

w = STR(RECNO(),10)
i = AT(w, mrks)
IF i = 0
  mrks = mrks + w
ELSE
  mrks = STUFF(mrks, i, 10, "")
ENDIF

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                     蹍������ 筮爼�牀����� �����.                     �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Prep_F

PARAMETERS act_list

PRIVATE n_acts, i
n_acts = ALEN(act_list,1)
DIMENSION act_list(n_acts,2)

SET EXCLU ON
DELETE FILE (tmpo_path+"SUM_ACTS.DBF")
DELETE FILE (tmpo_path+"SUM_ACTS.CDX")
CREATE DBF (tmpo_path+"SUM_ACTS")  ;
  ( CODE   N( 7), ;
    PREFIX C( 4), ;
    NAME   C(35), ;
    QNT    N( 6), ;
    NEW    L    , ;
    ACT_NO N( 5), ;
    INC_NO N( 5)  )
SET EXCLU OFF

SELECT 0
USE STOCK ORDER TAG CODE

SELECT 0
USE ACT_S_T ORDER TAG DOC_NUM

SELECT 0
USE ACT_S ORDER TAG DOC_NUM
SET RELATION TO CODE INTO STOCK

FOR i = 1 TO n_acts
  = SEEK(LEFT(DTOS(act_list(i,2)),4)+STR(act_list(i,1),5), "ACT_S_T")
  SEEK LEFT(DTOS(act_list(i,2)),4)+STR(act_list(i,1),5)
  SCAN REST WHILE DOC_NUM = act_list(i,1) .AND. DOC_DATE = act_list(i,2)
    SELECT SUM_ACTS
    APPEND BLANK
    REPLACE CODE   WITH ACT_S.CODE,   ;
            PREFIX WITH STOCK.PREFIX, ;
            NAME   WITH STOCK.NAME,   ;
            QNT    WITH QNT+ACT_S.QNT_INC, ;
            NEW    WITH STOCK.DATE_ON = act_list(i,2), ;
            ACT_NO WITH ACT_S.DOC_NUM, ;
            INC_NO WITH ACT_S_T.EXP_NUM
    SELECT ACT_S
  ENDSCAN
ENDFOR

CLOSE DATABASES

DO To_Disk WITH tmpo_path+"SUM_ACTS.DBF"
DELETE FILE (tmpo_path+"SUM_ACTS.DBF")

RETURN
