*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Ch_GTD       ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                               �襦�� ���.                               �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 14.02.2000 様�
PROCEDURE Ch_GTD
PARAMETERS StartVal

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
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*
s_sav = SELECT()
PRIVATE retVal
retVal = startVal

SELECT 0
DO Use_Dummy
SELECT 0
USE (path_comm+"GTD") ORDER TAG GTD_NUM ALIAS GTD_0214 AGAIN
IF EOF() .OR. BOF()
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� �� ����皰※�� �� �き�� ���!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  stat_type = SPACE(FSIZE("GTD_NUM"))+CHR(13)+SPACE(FSIZE("COUNTRY"))+CHR(13)+SPACE(FSIZE("GTD_LINE"))
  USE
  SELECT (s_sav)
  RETURN stat_type
ENDIF

IF .NOT. SEEK(startVal)
  GO TOP
ENDIF
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
wd = FSIZE("GTD_NUM")+FSIZE("COUNTRY")+1

DO D_Wins WITH ln, wd, "�襦ム�皀 ���", 0, 0
@ 1, 2 SAY PADR("���ム ���", FSIZE("GTD_NUM")+1, "�")+  ;
           PADR("�矗���", FSIZE("COUNTRY"), "�")
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
    BROWSE FIELDS GTD_NUM:H="", COUNTRY:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F        ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && �襦��

    retVal = GTD_NUM+CHR(13)+COUNTRY+CHR(13)+GTD_LINE+CHR(13)+COUNTRYC
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
USE
SELECT (s_sav)

RETURN retVal
