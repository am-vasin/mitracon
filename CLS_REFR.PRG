*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Cls_Refr     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                         �▲爲� 甎�瓷� ���瘁��                          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 09.12.2005 様�
PROCEDURE Cls_Refr
PARAMETERS prmCus, prmFile, nameMode

*
* prmCus	- ��� ��痰�∫┴�,
* prmFile	- ��Б��┘ ����� ��痰�∫┴�,
* nameMode  - 0 - �痰�←錺� 瓣�� ━キ� (�� 祠�������),
*             1 - 竅����←│�ガ ━キ� ��痰�∫┴�,
*             2 - ≡鍼┤ ��� 甎��荐��ガ.
*

* �矗礫矣�� ����� ��痰�∫┴�
*	CODE - ��� ���瘁�
*   NAME - ��Б��┘ ���瘁�
*

PRIVATE s_sav, keyCus, tmpCode, tmpMode, mss, tmpName, ret_v

m.tmpName = SYS(3)

IF TYPE("m.nameMode") = "N"
  m.tmpMode = IIF(BETWEEN(m.nameMode, 0, 2), m.nameMode, 0)
ELSE
  m.tmpMode = 0
ENDIF
m.keyCus = STR(prmCus)
m.s_sav = SELECT()
SELECT 0
USE (m.base_path+"CLASSES") SHARED AGAIN ORDER TAG CODE ALIAS CL_5C09
SELECT 0
USE (m.base_path+"TEACH_U") SHARED AGAIN ORDER TAG CODE ALIAS TC_5C09
SELECT 0
CREATE DBF (m.tmpo_path+m.tmpName) ;
  ( NAME C(FSIZE("NAME_CUS", "TC_5C09")) )
USE (m.tmpo_path+m.tmpName) ALIAS TMP_LIST
SELECT 0
USE (m.prmFile) SHARED AGAIN ALIAS SRC_5C09
SCAN
  m.tmpCode = IIF(TYPE("CODE")="C", CODE, STR(CODE, 15))
  IF SEEK("C"+m.keyCus+m.tmpCode, "TC_5C09")
    SELECT TC_5C09
    REPLACE NAME_CUS WITH ALLTRIM(SRC_5C09.NAME)
    IF m.tmpMode = 0
      SELECT SRC_5C09
      LOOP
    ENDIF
    = SEEK(TC_5C09.CODE_OWN, "CL_5C09")
    IF .NOT. ALLTRIM(TC_5C09.NAME_CUS) == ALLTRIM(CL_5C09.NAME)
      SELECT CL_5C09
      IF m.tmpMode = 1
        REPLACE NAME WITH TC_5C09.NAME_CUS
      ELSE
        DIMENSION mss[5]
        mss[1] = ""
        mss[2] = CHR(0)+"���瘁 ��痰�∫┴�: "+ALLTRIM(TC_5C09.NAME_CUS)
        mss[3] = CHR(0)+"��茱 ��Б��┘: "+ALLTRIM(CL_5C09.NAME)
        mss[4] = CHR(0)+"  �ムエ�キ���碎 ���瘁? [ �� ] [ �モ ]"
        mss[5] = ""
        IF Out_Mess(5, "mss") = 1
          REPLACE NAME WITH TC_5C09.NAME_CUS
        ENDIF
      ENDIF
    ENDIF
    SELECT SRC_5C09
    LOOP
  ENDIF
  IF .NOT. Connect(m.tmpCode, NAME)
    SELECT TMP_LIST
    APPEND BLANK
    REPLACE NAME WITH ALLTRIM(SRC_5C09.NAME)
    SELECT SRC_5C09
  ENDIF
ENDSCAN

USE
USE IN TC_5C09
USE IN CL_5C09
m.ret_v = RECCOUNT("TMP_LIST") = 0
IF .NOT. m.ret_v
  DO View_Tmp
ENDIF
USE IN TMP_LIST
DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
SELECT (m.s_sav)
RETURN m.ret_v

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Connect      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                            踸�錻�� ���瘁�                             �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 09.12.2005 陳�
PROCEDURE Connect
PARAMETERS pCode, pName

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
m.stat_type  = 1
m.what_do    = ""
m.menu_name  = ""
m.last_mouse = 0
m.win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ��ぅ爨�皀�讚�� ��痰� �牀������:
*

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� 敖 ���瘁 ��痰�∫┴� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕 �
*� �....:....!....:....!....:....!....:....!....:....!....:....!....� �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�[ ] ....:....!....:....!....:....!....:....!....:....!....:....!....�
*�                    < OK Ctrl-W > < �皖���碎瘴 Esc >                �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, tmpCls, tmpNm, tmpSw, tmpRet, s_sav
m.ex = 1
m.tmpSw  = .F.
m.tmpRet = .F.
m.tmpCls = "            "
m.tmpNm  = SPACE(64)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 8, 72, "���Θ皀 甌�癶モ痰※�"
DO Sun_Bord WITH  2,  3,  4, 68, " ���瘁 ��痰�∫┴� "
@ 3,  4 SAY m.pName
@ WROWS()-1, 2 SAY PADC(" Ins - ��玻オ罔� 甎�瓷� ���瘁�� ", WCOLS()-4, "�")

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 5,  2 GET m.tmpSw PICTURE "@*C " VALID C_Cls()
@ 5,  6 GET m.tmpNm WHEN .F.

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

ON KEY LABEL Ins DO Classes
READ CYCLE
ON KEY

IF m.ex = 1
  *
  * �矗���硅��ガ ゛���
  IF .NOT. EMPTY(m.tmpCls)
    m.s_sav = SELECT()
    SELECT TC_5C09
    APPEND BLANK
    REPLACE TEACH_TYPE	WITH "C",      ;
            CUS_CODE	WITH m.prmCus, ;
            CODE_CUS	WITH m.pCode,  ;
            CODE_OWN	WITH m.tmpCls, ;
            NAME_CUS	WITH m.pName
    SELECT (m.s_sav)
    m.tmpRet = .T.
  ENDIF
  
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN m.tmpRet

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� C_Cls        ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                              �襦�� ���瘁�                              �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 09.12.2005 陳�
PROCEDURE C_Cls

DO Ch_Class WITH m.tmpCls, m.tmpNm
m.tmpSw = .F.
SHOW GET m.tmpSw
SHOW GET m.tmpNm

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� View_Tmp     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                       蹍甃�矗 ∇ガキ���� �����                        �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 14.01.2006 陳�
PROCEDURE View_Tmp

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
m.stat_type  = 3
m.menu_name  = PROGRAM()
m.last_mouse = 0
m.win_name   = PROGRAM()

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*
m.s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT TMP_LIST
GO TOP

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
m.ln = MIN(RECCOUNT(), WROWS("")-11)   && ����腑痰〓 ※え�諷 痰牀� BROWSE
m.wd = FSIZE("NAME")

DO D_Wins WITH m.ln, m.wd, "�� �� ��爛ぅ�┼� 甄イ竡薑� ���瘁� ��痰�∫┴�", 0, 0
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    m.statys_type = 3
    DO Prp_Nav_1
*    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE FIELDS NAME:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

*  CASE m.what_do = "Choose"    && 蹍甃�矗 甎�瓷�

*    DO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*    m.what_do = "List"

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
SELECT (m.s_sav)

RETURN
