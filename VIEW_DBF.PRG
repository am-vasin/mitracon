*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� View_DBF     ��о�｀砒┴ ��むォ ��瓱�           01.10.98 15:24:58 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                          蹍甃�矗 DBF-�����.                           �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE View_DBF
PARAMETERS f_name, tit

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
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*
PRIVATE s_sav
s_sav = SELECT()
SELECT 0

USE (ALLTRIM(f_name)) ALIAS View

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd, i, nm
ln = WROWS()-7   && ����腑痰〓 ※え�諷 痰牀� BROWSE
i = 1
wd = 0
nm = FIELD(i)
DO WHILE .NOT. EMPTY(nm)
  DO CASE
  CASE TYPE(nm) = "M"
    wd = wd+MAX(4, LEN(nm))
  CASE TYPE(nm) = "D"
    wd = wd+MAX(IIF(SET("CENTURY") = "ON", 10, 8), LEN(nm))
  OTHERWISE
    wd = wd+MAX(FSIZE(nm), LEN(nm))
  ENDCASE
  i = i+1
  nm = FIELD(i)
ENDDO
wd = MIN(wd+i-2, WCOLS()-4)

DO D_Wins WITH ln, wd, ALLTRIM(tit), 0, 0
ZOOM WINDOW (win_name+"_I") NORM AT WLROW()+1, WLCOL() ;
                            SIZE WROWS()-1, WCOLS()-1
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
*    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE NOMODIFY NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    what_do = "Exit"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
USE
SELECT (s_sav)
RELEASE MENU (menu_name) EXTENDED

RETURN
