*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Ch_Chang     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                      ��皀���皋↓覃 ��｀� ���キ�.                       �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 09/24/1999 様�
PROCEDURE Ch_Chang
PARAMETERS s_name, v_zero

PRIVATE s_sav, r

s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS ST_9924 AGAIN
SELECT 0
USE (tmpo_path+"CHANGES")
SET RELATION TO CODE INTO ST_9924
IF .NOT. v_zero
  SET FILTER TO ST_9924.QNT > 0 .AND. ST_9924.PRICE > 0
ENDIF
COUNT TO r
IF r = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�モ �� �き�� ���キ�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT ST_9924
  USE
  SELECT (s_sav)
  RETURN 0
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
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

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
ln = MIN(r,WROWS("")-9)   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 76

DO D_Wins WITH ln, wd, "�������"+IIF(.NOT. EMPTY(s_name), " か� "+ALLTRIM(s_name), ""), 0, 0
@ 1, 2 SAY ;
"陳���━キ����┘陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳����-〓陳��キ� � ���キ� 珮�"
*PPPP nnnnnnnnnNnnnnnnnnnNnnnnnnnnnNnnnnn PPPPPPPP
what_do = "List"
GO TOP
PUSH KEY CLEAR
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE FIELDS ST_9924.PREFIX:4:H="",  ;
                  ST_9924.NAME:35:H="",   ;
                  ST_9924.PRODUCER:8:H="",;
                  ST_9924.QNT:P="@Z 999999":H="",   ;
                  ST_9924.PRICE:P="@Z 99999.9999":H="",   ;
                  ST_9924.PRICE_R:P="@Z 99999.99":H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && �襦��

    r = ST_9924.CODE
    EXIT

  OTHERWISE

    r = 0
    EXIT

  ENDCASE

ENDDO

POP KEY
RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   ���窶�皚キ讓� (���窶�皚キ讓�!) ���琺��ガ DBF-�����.
*
SELECT CHANGES
USE
SELECT ST_9924
USE
SELECT (s_sav)

RETURN r
