*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Inc_Due      ��о�｀砒┴ ��むォ ��瓱�           14.05.97 09:55:35 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                 蹍������ �÷�｀皖� ぎ��� �爬 �爬絎ぅ.                 �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Inc_Due
PRIVATE sel_sav, s_q

sel_sav = SELECT()
s_q = 0                && ����皰→� か� 痺���爿�� ÷���
IF .NOT. Prep_Due()    && ��ぃ�皰→� ぎ��� か� ��Ж罔�...
  SELECT (sel_sav)
  RETURN               &&  ...� �モ �┴����� ぎ���!
ENDIF

SELECT 0
USE (m.base_path+"LINK_ORD") SHARED AGAIN ALIAS LO_DUE ORDER TAG CODE
SELECT INC_DUE
SET RELATION TO CODE INTO LO_DUE

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
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD View OF (menu_name) PROMPT "����щ �キイΕ牀�" KEY Ctrl-A, "Tab"
ON SELECTION PAD View OF (menu_name) Do Sw_Mode WITH "view"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = 10  && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = FSIZE("ARM_N")+FSIZE("CUS_NAME")+;
     FSIZE("EXP_LIST")+FSIZE("QNT_DUE")+FSIZE("QNT")+6

DO D_Wins WITH ln, wd, "����: "+ALLTRIM(STOCK.PREFIX)+" "+  ;
                       ALLTRIM(STOCK.NAME)+" "+ALLTRIM(STOCK.PRODUCER), 0, 1

@ 1, 2 SAY PADC("���. �メ皰陳�",FSIZE("ARM_N")+1,"�")+     ;
           PADC("���甌�",FSIZE("EXP_LIST")+1,"�")+  ;
           PADC("��┘�眥陳陳陳",FSIZE("CUS_NAME")+1,"�")+  ;
           PADC("����",FSIZE("QNT_DUE")+1,"�")+   ;
           PADC("�牀��",FSIZE("QNT"),"�")+"��"
@ WROWS()-2,  3 SAY "��ゥ�:"
@ WROWS()-2, 10 SAY STR(STOCK.QNT+INC_LIST.QNT,6) COLOR SCHEME 1
@ WROWS()-2, 20 SAY "��甎爛ぅ�┼�:"
@ WROWS()-2, 34 SAY STR(s_q,6) COLOR SCHEME 1

what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Tab Do Sw_Mode WITH "view"

*
*    BROWSE - �キ�
*
    BROWSE KEY key_v+STR(STOCK.CODE,7)  ;
           FIELDS ARM_N:H="", EXP_LIST:H="", CUS_NAME:H="", QNT_DUE:H="", QNT:H="":V=Sh_Q(),  ;
                  f000 = IIF(.NOT. EMPTY(LO_DUE.CODE), "�", " "):H= "" ;
           FREEZE QNT ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "List")
    ENDIF
    ON KEY

  CASE what_do = "view" && ����щ �� ��痰�→� �� �キイΕ牀�

    DO Pos_Ord2 WITH CODE
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
USE IN LO_DUE

SELECT (sel_sav)

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                     ��ぃ�皰→� ぎ��� か� ��Ж罔�.                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 14.05.97 09:57:44 夕
PROCEDURE Prep_Due

PRIVATE mss, l_nam, arr_w, rc_sav

IF .NOT. FOUND("LIST_DET")
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��� �モ �┴����� ぎ��� �� 轤�� ��Ж罔�."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

SELECT INC_DUE
SCATTER TO arr_w BLANK

SELECT 0
USE LIST_TIT ORDER TAG LST_NUM

SELECT 0
USE ARM_LST ORDER TAG ARM_C

SELECT LIST_DET
SCAN REST WHILE CODE = STOCK.CODE
  = SEEK(VAL(ALLTRIM(LIST_NUM)), "LIST_TIT")
  IF LIST_TIT.LST_ARM = "***"
    l_nam = "｡��� �゛�痰�"
  ELSE
    = SEEK(LIST_TIT.LST_ARM, "ARM_LST")
    l_nam = ARM_LST.ARM_N
  ENDIF
  IF .NOT. SEEK(key_v+STR(CODE,7)+LIST_NUM, "INC_DUE")
    *
    *   鴜�ガ瘴 ��紜�皋碎 磴��キ�竡 ����瘡
    SET DELETED OFF
    SELECT INC_DUE
    SET ORDER TO TAG DEL
    GO TOP
    IF DELETED()
      RECALL
      GATHER FROM arr_w
    ELSE
      APPEND BLANK
    ENDIF
    SET DELETED ON
    SET ORDER TO TAG L_CODE
  ENDIF
  SELECT INC_DUE
  REPLACE INC_LIST WITH key_v,             ;
          EXP_LIST WITH LIST_DET.LIST_NUM, ;
          CUS_NAME WITH LIST_TIT.LST_NAME, ;
          ARM_N    WITH l_nam,             ;
          CODE     WITH STOCK.CODE,        ;
          QNT_DUE  WITH LIST_DET.QNT_DUE
  SELECT LIST_DET
ENDSCAN

SELECT LIST_TIT
USE

SELECT ARM_LST
USE

SELECT LIST_DET
SET ORDER TO TAG L_CODE
SELECT INC_DUE
SEEK key_v+STR(STOCK.CODE,7)
SCAN REST WHILE INC_LIST = key_v .AND. CODE = STOCK.CODE
  IF .NOT. SEEK(EXP_LIST+STR(CODE,7), "LIST_DET")
    DELETE
    LOOP
  ENDIF
  IF LIST_DET.QNT_DUE <= 0
    DELETE
    LOOP
  ENDIF
  REPLACE QNT_DUE WITH LIST_DET.QNT_DUE
  s_q = s_q+QNT
ENDSCAN
SELECT LIST_DET
SET ORDER TO TAG DUE_CODE

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                          �諷�� ├ ���� ÷���.                          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 14.05.97 12:48:42 夕
PROCEDURE Sh_Q

PRIVATE mss, rc_sav

IF QNT > QNT_DUE
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� ガ� 痰��讓� �� ぎ�Ν�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

rc_sav = RECNO()
SEEK key_v+STR(STOCK.CODE,7)
SUM REST WHILE key_v = INC_LIST .AND. STOCK.CODE = CODE QNT TO s_q
GO rc_sav
IF s_q > STOCK.QNT+INC_LIST.QNT
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"� ��� 痰��讓� �モ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

ACTIVATE WINDOW (win_name+"_M") SAME
@ WROWS()-2, 34 SAY STR(s_q,6) COLOR SCHEME 1
ACTIVATE WINDOW (ALIAS())

RETURN .T.
