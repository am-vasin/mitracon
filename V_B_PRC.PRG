*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� V_B_Prc      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                      ���祚�膈襯 罐�� �� ��珥�鍖.                       �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 13.10.2006 様�
PROCEDURE V_B_Prc
PARAMETERS prmCode	&& ��� ��Ж罔�


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
SELECT 0
USE (m.base_path+"ACCOUNT") SHARED AGAIN ORDER TAG CUS_CODE ALIAS ACC_61013
SELECT 0
USE (m.base_path+"STOCK") SHARED AGAIN ORDER TAG CODE ALIAS STO_61013
SEEK m.prmCode
SELECT 0
USE (m.base_path+"ACT_S_T") SHARED AGAIN ORDER TAG DOC_NUM ALIAS TIT_61013
SET RELATION TO SUP_CODE INTO ACC_61013
SELECT 0
USE (m.base_path+"ACT_S") SHARED AGAIN ORDER TAG SALE ALIAS DET_61013
SET RELATION TO FIRM+LEFT(DTOS(DOC_DATE), 4)+DOC_NUM INTO TIT_61013

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Filter OF (menu_name) PROMPT "�瓮 ��珥┬" KEY Ctrl-A, "Tab"
ON SELECTION PAD Filter OF (menu_name) DO Sw_Mode WITH "Filter"

DEFINE PAD Price OF (menu_name) PROMPT "�キ�"
ON PAD Price OF (menu_name) ACTIVATE POPUP Price

  DEFINE POPUP Price MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Price PROMPT "� � ���鈞� ぎ�祠キ��" KEY Ctrl-A, "F1"
  ON SELECTION BAR 1 OF Price DO Sw_Mode WITH "Price1"

  DEFINE BAR  2 OF Price PROMPT "  � 珮゛閻" KEY Ctrl-A, "F2"
  ON SELECTION BAR 2 OF Price DO Sw_Mode WITH "Price2"

  DEFINE BAR  3 OF Price PROMPT "  � � �" KEY Ctrl-A, "F3"
  ON SELECTION BAR 3 OF Price DO Sw_Mode WITH "Price3"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
m.ln = WROWS("")-20   && ����腑痰〓 ※え�諷 痰牀� BROWSE
m.wd = 62

*  ���祚�膈�� 罐�� か� ��Ж罔�
* ��Ж罔� PPPP nnnnnnnnnNnnnnnnnnnNnnnnnnnnnN ppppPppp
* �爼陳諦 ぎ�-��陳陳�����陳陳�踸絎つ��痰�皰�陳陳陳陳�キ�陳陳陳�
* FRM NNNNNNNNNN 99.99.9999 99999999 99999999 9999999999.9999V 
DO D_Wins WITH m.ln, m.wd, "���祚�膈襯 罐�� �� ��珥�鍖", 1, 1
@ 1, 2 SAY "��Ж罔� "+IIF(EMPTY(STO_61013.PREFIX), "", ALLTRIM(STO_61013.PREFIX)+" ")+ ;
                     ALLTRIM(STO_61013.NAME)+ ;
                     IIF(EMPTY(STO_61013.PRODUCER), "", +" /"+ALLTRIM(STO_61013.PRODUCER)+"/")
@ 2, 2 SAY "�爼陳諦 ぎ�-��陳陳�����陳陳�踸絎つ��痰�皰�陳陳陳陳�キ�陳陳陳�"
m.what_do = "List"

PRIVATE tmpKey, prcType
m.tmpKey = STR(STO_61013.CODE, 7)
m.prcType = 1

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Tab DO Sw_Mode WITH "Filter"
    ON KEY LABEL F1  DO Sw_Mode WITH "Price1"
    ON KEY LABEL F2  DO Sw_Mode WITH "Price2"
    ON KEY LABEL F3  DO Sw_Mode WITH "Price3"

*
*    BROWSE - �キ�
*
    BROWSE REST KEY m.tmpKey ;
           FIELDS FIRM:H="", ;
                  DOC_NUM:H= "",  ;
                  DOC_DATE:H= "", ;
                  QNT_INC:P="@Z 99999999":H= "",  ;
                  QNT_REST:P="@Z 99999999":H= "", ;
                  PRC_FIELD = ShowPrc():16:H= "", ;
                  IS_REAL   = IIF(TIT_61013.FLG_BUY = "R", "�", " "):1:H="", ;
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

    m.what_do = "List"

  CASE m.what_do = "Filter"    && �┼赳�

    IF TYPE("m.tmpKey") = "C"
      DEFINE PAD Filter OF (menu_name) PROMPT "�ク竅硅� ��珥┬" KEY Ctrl-A, "Tab" BEFORE PRice
      m.tmpKey = STO_61013.CODE
      SET ORDER TO TAG CODE
    ELSE
      DEFINE PAD Filter OF (menu_name) PROMPT "�瓮 ��珥┬" KEY Ctrl-A, "Tab" BEFORE PRice
      m.tmpKey = STR(STO_61013.CODE, 7)
      SET ORDER TO TAG SALE
    ENDIF  
    m.what_do = "List"

  CASE m.what_do = "Price1"    && �キ� � ���鈞� ぎ�祠キ��

    m.prcType = 1
    DEFINE BAR  1 OF Price PROMPT "� � ���鈞� ぎ�祠キ��" KEY Ctrl-A, "F1"
    DEFINE BAR  2 OF Price PROMPT "  � 珮゛閻" KEY Ctrl-A, "F2"
    DEFINE BAR  3 OF Price PROMPT "  � � �" KEY Ctrl-A, "F3"
    m.what_do = "List"

  CASE m.what_do = "Price2"    && �キ� � 珮゛閻

    m.prcType = 2
    DEFINE BAR  1 OF Price PROMPT "  � ���鈞� ぎ�祠キ��" KEY Ctrl-A, "F1"
    DEFINE BAR  2 OF Price PROMPT "� � 珮゛閻" KEY Ctrl-A, "F2"
    DEFINE BAR  3 OF Price PROMPT "  � � �" KEY Ctrl-A, "F3"
    m.what_do = "List"

  CASE m.what_do = "Price3"    && �キ� � USD

    m.prcType = 3
    DEFINE BAR  1 OF Price PROMPT "  � ���鈞� ぎ�祠キ��" KEY Ctrl-A, "F1"
    DEFINE BAR  2 OF Price PROMPT "  � 珮゛閻" KEY Ctrl-A, "F2"
    DEFINE BAR  3 OF Price PROMPT "� � � �" KEY Ctrl-A, "F3"
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
USE IN ACC_61013
USE IN STO_61013
USE IN TIT_61013
USE IN DET_61013

SELECT (m.s_sav)

RETURN

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
@ WROWS()-2, 3 SAY ACC_61013.CUS_NAME

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.


*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� ShowPrc      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                          ������錺� ���� 罐��                           �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 13.10.2006 陳�
PROCEDURE ShowPrc

PRIVATE retVal

m.retVal = SPACE(16)
DO CASE
CASE m.prcType = 1		&& � ���鈞� ぎ�祠キ��
  DO CASE
  CASE DET_61013.PRICE_R = 0 .AND. DET_61013.PRICE_D = 0 .AND. TIT_61013.FLG_BUY = "R"
    m.retVal = STR(STO_61013.PRICE_B, 15, 4)+"$"
  CASE TIT_61013.VALUTA = "�"
    m.retVal = STR(DET_61013.PRICE_R, 15, 4)+"�"
  CASE TIT_61013.VALUTA = "$"
    m.retVal = STR(DET_61013.PRICE_D, 15, 4)+"$"
  ENDCASE
CASE m.prcType = 2		&& � 珮゛閻
  DO CASE
  CASE DET_61013.PRICE_R = 0 .AND. DET_61013.PRICE_D = 0 .AND. TIT_61013.FLG_BUY = "R"
    m.retVal = STR(STO_61013.PRICE_B*m.kurs, 15, 4)+"�"
  CASE TIT_61013.VALUTA = "�"
    m.retVal = STR(DET_61013.PRICE_R, 15, 4)+"�"
  CASE TIT_61013.VALUTA = "$"
    m.retVal = STR(DET_61013.PRICE_D*m.kurs, 15, 4)+"�"
  ENDCASE
CASE m.prcType = 3		&& � USD
  DO CASE
  CASE DET_61013.PRICE_R = 0 .AND. DET_61013.PRICE_D = 0 .AND. TIT_61013.FLG_BUY = "R"
    m.retVal = STR(STO_61013.PRICE_B, 15, 4)+"$"
  CASE TIT_61013.VALUTA = "�"
    m.retVal = STR(DET_61013.PRICE_R/m.kurs, 15, 4)+"$"
  CASE TIT_61013.VALUTA = "$"
    m.retVal = STR(DET_61013.PRICE_D, 15, 4)+"$"
  ENDCASE
ENDCASE

RETURN m.retVal
