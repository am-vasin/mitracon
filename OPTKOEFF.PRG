*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� OptKoeff     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�            �痰���→� ��辟筥罔キ皰� か� ��瘍モ� ��皰�諷 罐�             �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 08.05.2006 様�
PROCEDURE OptKoeff

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
m.win_name   = "kff"

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*
m.s_sav = SELECT()
PRIVATE tmpName, maxBound
m.maxBound = 9999999999.9999
m.tmpName = SYS(3)

SELECT 0
USE (m.base_path+"OPTKOEFF") SHARED AGAIN ALIAS KFF_6508 ORDER TAG PRC_BOUND
COPY TO (m.tmpo_path+m.tmpName) WITH CDX
USE (m.tmpo_path+m.tmpName) SHARED AGAIN ALIAS KFF_6508 ORDER TAG PRC_BOUND

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Modify OF (menu_name) PROMPT "��玻オ皋牀��碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Modify OF (menu_name) DO Sw_Mode WITH "Modify"

DEFINE PAD Add OF (menu_name) PROMPT "����※碎" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"

DEFINE PAD Del OF (menu_name) PROMPT "�����碎" KEY Ctrl-A, "Del"
ON SELECTION PAD Del OF (menu_name) DO Sw_Mode WITH "Del"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
m.ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
m.wd = 24

DO D_Wins WITH m.ln, m.wd, "��辟�. か� ��瘍. ���. 罐�", 0, 0
@ 1, 2 SAY ;
"陳��キ� ぎ ($)陳���辟�."
*9999999999.9999 999.9999
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins   DO Sw_Mode WITH "Add"
    ON KEY LABEL Del   DO Sw_Mode WITH "Del"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS PPP=PRC_B(PRC_BOUND):15:H="", ;
                  PRC_K:H="":P="999.9999" ;
                  FREEZE PRC_K ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Modify")
    ENDIF

  CASE m.what_do = "Modify"    && ��玻オ罔�

    DO ModiLine
    m.what_do = "List"

  CASE m.what_do = "Add"       && ����←キ┘

    DO ModiLine WITH .T.
    m.what_do = "List"

  CASE m.what_do = "Del"       && ����キ┘

    IF PRC_BOUND = m.maxBound
      DIMENSION mss[3]
      mss[1] = ""
      mss[2] = CHR(0)+"�矣 痰牀�� �カ讌� 磴��閧�"
      mss[3] = ""
      DO Out_Mess WITH 7, "mss"
    ELSE
      DELETE
    ENDIF
    m.what_do = "List"

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
PRIVATE tmpArr
SELECT 0
SET DELETED OFF
USE (m.base_path+"OPTKOEFF") SHARED AGAIN ALIAS RS_6508
SELECT KFF_6508
SCAN FOR .NOT. DELETED()
  SCATTER TO tmpArr
  SELECT RS_6508
  IF EOF()
    APPEND BLANK
  ELSE
    RECALL
  ENDIF
  GATHER FROM tmpArr
  SKIP
  SELECT KFF_6508
ENDSCAN
USE
DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
DELETE FILE (m.tmpo_path+m.tmpName+".cdx")
SELECT RS_6508
IF .NOT. EOF()
  DELETE REST
ENDIF
SET DELETED ON
USE
SELECT (m.s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Prc_B        ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                               �キ� ぎ...                               �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 08.05.2006 陳�
PROCEDURE Prc_B
PARAMETERS prmPrc 


IF m.prmPrc = m.maxBound
  RETURN "               "
ENDIF

RETURN TRANSFORM(m.prmPrc, "9999999999.9999")

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� ModiLine     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      ��玻オ罔�/ぎ��←キ┘ 痰牀��                       �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 08.05.2006 陳�
PROCEDURE ModiLine
PARAMETER addMode

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�                                     �
*�   ������� 罐� ($) 9999999999.9999   �
*�                                     �
*�        ��辟筥罔キ� 999.9999         �
*�                                     �
*�  < OK Ctrl-W > < �皖���碎瘴 Esc >   �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex, tmpB, tmpK, letMax
m.ex = 1
IF m.addMode
  STORE 0 TO m.tmpB, m.tmpK
ELSE
  m.tmpB = PRC_BOUND
  m.tmpK = PRC_K
ENDIF
m.letMax = PRC_BOUND=m.maxBound
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 9, 41, "��辟筥罔キ� か� 罐��〓� �珮���"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@  3,  5 SAY "������� 罐� ($) "
IF m.tmpB < m.maxBound
  @  3, COL() GET m.tmpB PICTURE "@Z 9999999999.9999"
ENDIF
@  5, 10 SAY "��辟筥罔キ�" GET m.tmpK PICTURE "@Z 999.9999"

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID TstBound()

IF m.ex = 1
  *
  * �矗���硅��ガ ゛���
  IF m.addMode
    APPEND BLANK
  ENDIF
  REPLACE PRC_BOUND WITH m.tmpB, PRC_K WITH m.tmpK
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� TstBound     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      蹍▲爲� ��玻オ皚�痰� ゛����                      �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 08.05.2006 陳�
PROCEDURE TstBound

PRIVATE mss

IF m.ex # 1
  RETURN .T.
ENDIF

IF m.tmpB <= 0 .OR. m.tmpB > 9999999999 .AND. .NOT. m.letMax
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"�キ���� ������� ぎ�Ν� °碎 ｀�跏� �祀� � �キ跏� 9999999999"
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF m.tmpK < 0
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"��辟筥罔キ� ぎ�Ε� °碎 ｀�跏� ┼� ��▲� �祀�."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.
