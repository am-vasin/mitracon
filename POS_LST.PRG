*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Pos_Lst      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                        ���瓷 ��Ж罔� � 甎�瓷��.                        �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 27.08.2005 様�
PROCEDURE Pos_Lst
PARAMETER repeate

PRIVATE t_shab, is_all, s_sav, tmpFound

IF m.repeate
  IF .NOT. File_O(m.tmpo_path+"POS_LST.DBF")
    PRIVATE tmpMess
    DIMENSION tmpMess[3]
    tmpMess[1] = ""
    tmpMess[2] = CHR(0)+"��腑�� �� ���ぅ��!"
    tmpMess[3] = ""
    DO Out_Mess WITH 7, "tmpMess"
    RETURN 0
  ENDIF
  m.s_sav = SELECT()
  SELECT 0
  USE (m.tmpo_path+"POS_LST.DBF") EXCLUSIVE ALIAS RES_LIST
  IF RECCOUNT() = 0
    PRIVATE tmpMess
    DIMENSION tmpMess[3]
    tmpMess[1] = ""
    tmpMess[2] = CHR(0)+"��腑�� �� ���ぅ��!"
    tmpMess[3] = ""
    DO Out_Mess WITH 7, "tmpMess"
    USE IN RES_LIST
    SELECT (m.s_sav)
    RETURN 0
  ENDIF
ELSE
  m.t_shab = ""

  m.t_shab = Get_Shb(m.t_shab, "�����皀 ��゛�� か� ���瓷�")

  IF EMPTY(m.t_shab)
    RETURN 0
  ENDIF
  m.is_all = .NOT. EMPTY(LEFT(m.t_shab, 1))
  m.t_shab = SUBSTR(m.t_shab, 2)
  m.s_sav = SELECT()
  SELECT 0
  IF File_O(m.tmpo_path+"POS_LST.DBF")
    USE (m.tmpo_path+"POS_LST.DBF") EXCLUSIVE ALIAS RES_LIST
    ZAP
  ELSE 
    CREATE DBF (m.tmpo_path+"POS_LST") ;
    ( REC_NO    N(10), ;
      LIST_NUM  C( 6), ;
      ARM_N     C(20), ;
      IS_STOCK  C( 1), ;
      PREFIX    C( 4), ;
      NAME      C(35), ;
      PRODUCER  C( 8), ;
      CUSTOMER  C(50), ;
      OWN_LIST  L,     ;
      SALED     L      )
    USE (m.tmpo_path+"POS_LST") EXCLUSIVE ALIAS RES_LIST
  ENDIF
  SELECT 0
  USE m.base_path+"ARM_LST" SHARED AGAIN ORDER TAG ARM_C ALIAS ARM_5827
  SELECT 0
  USE m.base_path+"STOCK" SHARED AGAIN ORDER TAG CODE ALIAS STO_5827
  SELECT 0
  USE m.base_path+"LIST_DET" SHARED AGAIN ORDER TAG LIST ALIAS LD_5827
  SET RELATION TO CODE INTO STO_5827
  SELECT 0
  USE m.base_path+"ACCOUNT" SHARED AGAIN ORDER TAG CUS_CODE ALIAS CUS_5827
  SELECT 0
  USE m.base_path+"LIST_TIT" SHARED AGAIN ORDER TAG LST_ARM ALIAS LT_5827
  SET RELATION TO LST_ARM INTO ARM_5827
  
  IF m.is_all
    GO TOP
  ELSE
    SEEK m.arm
  ENDIF
  SCAN REST WHILE LST_ARM = m.arm .OR. is_all FOR EMPTY(LT_5827.SAL_NUM)
    WAIT WIND ARM_5827.ARM_N+STR(LST_NUM) NOWAIT	&&&&&
    SELECT LD_5827
    SEEK STR(LT_5827.LST_NUM,6)
    SCAN REST WHILE STR(LT_5827.LST_NUM,6) = LD_5827.LIST_NUM
      IF FOUND("STO_5827")
        IF m.t_shab $ STO_5827.PREFIX+STO_5827.NAME+STO_5827.PRODUCER
          SELECT RES_LIST
          APPEND BLANK
          REPLACE REC_NO    WITH RECNO("LT_5827"),			;
                  LIST_NUM  WITH STR(LT_5827.LST_NUM, 6),	;
                  ARM_N     WITH ARM_5827.ARM_N,			;
                  IS_STOCK  WITH "�",						;
                  PREFIX    WITH STO_5827.PREFIX,			;
                  NAME      WITH STO_5827.NAME,				;
                  PRODUCER  WITH STO_5827.PRODUCER,			;
                  CUSTOMER  WITH CUS_5827.CUS_NAME,			;
                  OWN_LIST  WITH LT_5827.LST_ARM = m.arm,	;
                  SALED     WITH .NOT. EMPTY(LT_5827.SAL_NUM)
        ENDIF
      ELSE
        IF m.t_shab $ LD_5827.PREFIX+LD_5827.NAME+LD_5827.PRODUCER
          SELECT RES_LIST
          APPEND BLANK
          REPLACE REC_NO    WITH RECNO("LT_5827"),			;
                  LIST_NUM  WITH STR(LT_5827.LST_NUM, 6),	;
                  ARM_N     WITH ARM_5827.ARM_N,			;
                  IS_STOCK  WITH " ",						;
                  PREFIX    WITH LD_5827.PREFIX,			;
                  NAME      WITH LD_5827.NAME,				;
                  PRODUCER  WITH LD_5827.PRODUCER,			;
                  OWN_LIST  WITH LT_5827.LST_ARM = m.arm,	;
                  CUSTOMER  WITH CUS_5827.CUS_NAME
        ENDIF
      ENDIF
      SELECT LD_5827
    ENDSCAN
    SELECT LT_5827
  ENDSCAN

  USE IN LD_5827
  USE IN LT_5827
  USE IN STO_5827
  USE IN CUS_5827
  USE IN ARM_5827

  IF RECCOUNT("RES_LIST") = 0
    PRIVATE tmpMess
    DIMENSION tmpMess[3]
    tmpMess[1] = ""
    tmpMess[2] = CHR(0)+"��腑�� �� ���ぅ��!"
    tmpMess[3] = ""
    DO Out_Mess WITH 7, "tmpMess"
    USE IN RES_LIST
    SELECT (m.s_sav)
    RETURN 0
  ENDIF
ENDIF

m.tmpShab = Ch_List()

USE IN RES_LIST
SELECT (m.s_sav)

RETURN m.tmpShab

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Ch_List      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                          �襦�� ├ ���ぅ�����.                          �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 27.08.2005 陳�
PROCEDURE Ch_List

*
*  �ムガキ�襯 甌痰�鎰�� か� ��※��罔�
*

PRIVATE res
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
SELECT RES_LIST

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�襦��碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "�皖���碎瘴" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 60

DO D_Wins WITH ln, wd, "��, 艪� °�� ���ぅ��", 0, 3
@ 1, 2 SAY "�陳陳��Ж罔鐵陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳����甌���"
@ WROWS()-3, 3 SAY "��┘��" 
what_do = "List"
res = 0

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE FIELDS         ;
           IS_STOCK:H="", ;
           PREFIX:H="",   ;
           NAME:H="",     ;
           PRODUCER:H="", ;
           LIST_NUM:H="", ;
           F000 = IIF(SALED, "�", " "):H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && 蹍甃�矗 甎�瓷�

    IF .NOT. RES_LIST.OWN_LIST
      PRIVATE mss000
      DIMENSION mss000(3)
      mss000(1) = ""
      mss000(2) = CHR(0)+"�皰 艫Ξ� 甎�甌�, � �ガ� �カ讌� �ムォ皋"
      mss000(3) = ""
      DO Out_Mess  WITH 7, "mss000"
    ELSE
      m.res = RES_LIST.REC_NO
    ENDIF
    EXIT

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

RETURN m.res

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Draw_Foot    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                   ��痺ガ ぎ�����皀�讚竡 ┃筮爼�罔�.                    �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 20.10.1998 陳�
PROCEDURE Draw_Foot

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� Гメ�, 爬痺ガ...
*
@ WROWS()-4, 3 SAY "��｀腑� �メ皰 "+RES_LIST.ARM_N
@ WROWS()-2, 3 SAY RES_LIST.CUSTOMER

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.
