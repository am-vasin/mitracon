*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� PosFound     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                       �襦�� ��Ж罔� ├ ���ぅ��諷                       �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 27.10.2005 様�
PROCEDURE PosFound


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
PRIVATE ch_mode		  && .F. - �襦���ガ ├ 磴��キ�諷

ch_mode = .T.
*
*   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
*
m.stat_type  = 3
m.menu_name  = PROGRAM()
m.last_mouse = 0
m.win_name   = PROGRAM()

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*
PRIVATE tmpRet
m.tmpRet = 0
m.s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT 0
USE (m.path_comm+"SUBJECT") SHARED AGAIN ALIAS SUB_5A27 ORDER TAG STO_CODE
SELECT 0
USE (m.base_path+"STOCK") SHARED AGAIN ALIAS STO_5A27 ORDER TAG CODE
SELECT TMP_5A27
SET RELATION TO CODE INTO SUB_5A27, CODE INTO STO_5A27

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
m.ln = MIN(WROWS("")-11, RECCOUNT())   && ����腑痰〓 ※え�諷 痰牀� BROWSE
m.wd = FSIZE("PREFIX")+FSIZE("NAME")+FSIZE("PRODUCER")+28

DO D_Wins WITH m.ln, m.wd, "�ェ祀赳�� ���瓷�", 0, 6
@ 1, 2 SAY ;
"陳陳���━キ����┘陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳���-〓陳���牆竅"
*pppp nnnnnnnnnNnnnnnnnnNnnnnnnnnnNnnnnn ppppPppp 99999999 ....:....!....:.
*"陳陳���━キ����┘陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳���-〓陳�����キ�"
*pppp nnnnnnnnnNnnnnnnnnNnnnnnnnnnNnnnnn ppppPppp 99999999 ��.��.����
SCROLL WROWS()-8, 0, WROWS()-7, WCOLS()-1, -1
@ WROWS()-7, 3 SAY "�祗�罔����讚�� ��Л�腑�┘"
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE FIELDS PREFIX:H="", NAME:H="", PRODUCER:H="",  ;
                  STO_5A27.QNT:H="":P="99999999",         ;
                  SUB_5A27.CORPUS:H="":16,				  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Choose"    && 蹍甃�矗 甎�瓷�

    IF TYPE("m.for_del") = "L"
      ch_mode = .NOT. m.for_del
    ENDIF
    IF ch_mode
      IF .NOT. EMPTY(STO_5A27.DATE_OFF)
        PRIVATE mss
        DIMENSION mss[3]
        mss[1] = ""
        mss[2] = CHR(0)+"��Ж罔� 磴��キ�..."
        mss[3] = ""
        DO Out_Mess WITH 7, "mss"
        what_do = "List"
        LOOP
      ENDIF
    ELSE
      IF EMPTY(STO_5A27.DATE_OFF)
        PRIVATE mss
        DIMENSION mss[3]
        mss[1] = ""
        mss[2] = CHR(0)+"��Ж罔� �� 磴��キ�..."
        mss[3] = ""
        DO Out_Mess WITH 7, "mss"
        what_do = "List"
        LOOP
      ENDIF
    ENDIF
    m.tmpRet = LINK
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

USE IN SUB_5A27
USE IN STO_5A27
SELECT (m.s_sav)

RETURN m.tmpRet

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Draw_Foot    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                   ��痺ガ ぎ�����皀�讚竡 ┃筮爼�罔�.                    �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 20.10.1998 陳�
PROCEDURE Draw_Foot
PRIVATE tmpDummy, lin, msDel

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� Гメ�, 爬痺ガ...
*
@ WROWS()-8,  2 SAY "�キ�" GET STO_5A27.PRICE_R
@ WROWS()-8, 25 SAY "���. � ��痰�∫┴�" GET STO_5A27.QNT_STORE

IF FOUND("SUB_5A27")
  @ WROWS()-6, 2 EDIT SUB_5A27.FUNCTION SIZE 5, WCOLS()-4
ELSE
  m.tmpDummy = ""
  @ WROWS()-6, 2 EDIT m.tmpDummy SIZE 5, WCOLS()-4
ENDIF
CLEAR GETS
IF .NOT. EMPTY(DATE_OFF)
  m.msDel = "����キ� "+DTOC(DATE_OFF)
  m.lin   = FLOOR((WCOLS()-LEN(m.msDel))/2)
  @ WROWS()-1, m.lin SAY m.msDel COLOR SCHEME 7
ELSE
   @ WROWS()-1, 3 SAY REPLICATE("�", WCOLS()-6) COLOR (SCHEME(14, 2))
ENDIF

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.
