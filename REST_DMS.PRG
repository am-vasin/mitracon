*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Rest_DMS     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                             ����腮� � ���                              �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 28.05.2009 様�
PROCEDURE Rest_DMS
PARAMETERS prmCode

PRIVATE s_sav, tmpKey

m.s_sav = SELECT()
SELECT 0
USE (m.base_path+"STOCK") SHARED AGAIN ORDER TAG CODE ALIAS DR_STO
SELECT 0
DO Use_DMS WITH "DR_DMS"
SET ORDER TO TAG CODE
= SEEK(m.prmCode, "DR_STO")
IF TYPE("CODE") = "C"
  m.tmpKey = ALLTRIM(STR(m.prmCode))
  m.tmpKey = PADR(m.tmpKey, FSIZE("CODE"))
ELSE
  m.tmpKey = m.prmCode
ENDIF
IF .NOT. SEEK(m.tmpKey, "DR_DMS")
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�皰� ��Ж罔� �モ � ���"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE IN DR_STO
  USE IN DR_DMS
  SELECT(m.s_sav)
  RETURN
ENDIF

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


SELECT 0
DO Use_Dummy
SELECT DR_DMS

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
m.ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
m.wd = FSIZE("QNT")+FSIZE("SUP_DATE")+12

DO D_Wins WITH m.ln, m.wd, "����腮� � ���", 1, 0
m.what_do = "List"
@ 1, 2 SAY ALLTRIM(DR_STO.PREFIX)+ALLTRIM(DR_STO.NAME)+" "
IF .NOT. EMPTY(DR_STO.PRODUCER)
  @ 1, COL() SAY "/"+ALLTRIM(DR_STO.PRODUCER)+"/"
ENDIF
@ 2, 2 SAY PADC("���-〓", FSIZE("QNT"), "�")+"�"+ ;
           PADC("�キ�", 10, "�")+"�"+ ;
           PADC("�牀� ��痰�→�", FSIZE("SUP_DATE"), "�")
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    m.statys_type = 3
    DO Prp_Nav_1
*    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE KEY m.tmpKey	;
           FIELDS QNT:H="",    ;
           PR = PRICE1*m.kurs:H="":P="@Z 9999999.99",   ;
           SUP_DATE:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")

*           WHEN Draw_Foot()    

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

USE IN DR_STO
USE IN DR_DMS
SELECT (m.s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Draw_Foot    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                   ��痺ガ ぎ�����皀�讚竡 ┃筮爼�罔�.                    �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 20.10.1998 陳�
*PROCEDURE Draw_Foot

*ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� Гメ�, 爬痺ガ...
*

*ACTIVATE WINDOW (ALIAS()) SAME

*RETURN .T.
