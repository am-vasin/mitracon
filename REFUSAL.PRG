*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Refusal      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                           ����→� �� 甎牀痺                            �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 07/25/2001 様�
PROCEDURE Refusal

PRIVATE dt_s, dt_e, tmpName

dt_e = DATE()
dt_s = GOMONTH(dt_e, -1)

IF .NOT. G_Period(dt_s, dt_e)
  RETURN
ENDIF
tmpName = SYS(3)

CREATE DBF (tmpo_path+tmpName) ;
  ( PREFIX   C( 4), ;
    NAME     C(35), ;
    PRODUCER C( 8), ;
    DATE     D,     ;
    QNT      N( 6), ;
    PRICE    N(8,2),;
    PRICE_D  N(10,4) )
USE (tmpo_path+tmpName) EXCLUSIVE ALIAS TMP_REF

SELECT 0
USE base_path+"REFUSAL" ORDER TAG NAME_DATE

SCAN FOR BETWEEN(DATE, dt_s, dt_e)
  SELECT TMP_REF
  APPEND BLANK
  REPLACE PREFIX   WITH REFUSAL.PREFIX,  ;
          NAME     WITH REFUSAL.NAME,    ;
          PRODUCER WITH REFUSAL.PRODUCER,;
          DATE     WITH REFUSAL.DATE,    ;
          QNT      WITH REFUSAL.QTY,     ;
          PRICE    WITH REFUSAL.PRICE,   ;
          PRICE_D  WITH REFUSAL.PRICE_D
  SELECT REFUSAL
ENDSCAN
USE
SELECT TMP_REF
INDEX ON NAME+PREFIX+PRODUCER+DTOS(DATE) TAG TMP
GO TOP


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

SELECT 0
DO Use_Dummy
SELECT TMP_REF

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
*
PRIVATE ln, wd
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 77

DO D_Wins WITH ln, wd, "�皖�щ �� 罐�� � "+DTOC(dt_s)+" �� "+DTOC(dt_e), 0, 1
@ 1, 2 SAY ;
"陳陳���Ж罔鐵陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳����陳陳��キ�(�)陳陳�キ�($)����-〓"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE FIELDS PREFIX:H="",  ;
                  Brw_Name=Nm_Part(NAME):25:H="", ;
                  PRODUCER:H="",;
                  DATE:H="",    ;
                  PRICE:H="",   ;
                  PRICE_D:H="", ;
                  QNT:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           WHEN Draw_Foot()    ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    what_do = "Exit"
*    IF what_do = "List"
*      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
*    ENDIF

*  CASE what_do = "Choose"    && 蹍甃�矗 甎�瓷�
*
*    DO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*    what_do = "List"
*
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
CLOSE DATABASES
DELETE FILE (tmpo_path+tmpName+".dbf")
DELETE FILE (tmpo_path+tmpName+".cdx")
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
@ WROWS()-2, 3 SAY PREFIX COLOR SCHEME 1
@ ROW(), COL()+1 SAY NAME COLOR SCHEME 1
@ ROW(), COL()+1 SAY PRODUCER COLOR SCHEME 1

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.
