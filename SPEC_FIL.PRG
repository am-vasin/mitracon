*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Spec_Fil     ��о�｀砒┴                                          �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                            ���甌� 甎ユ筥������.                        �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Spec_Fil
PARAMETERS c_code, c_name

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

PRIVATE s_sav

s_sav = SELECT()
*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*

DO Use_Dummy
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS A_1411 AGAIN
SET FILTER TO EMPTY(DATE_OFF)

IF CUS_CODE >= 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�モ �� �き��� 甎ユ��┘���!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
ENDIF

IF .NOT. SEEK(c_code)
  GO TOP
ENDIF

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�襦��" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "�皖���碎瘴" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = 15   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 24

DO D_Wins WITH ln, wd, "��ユ.��┘�硅" , 0, 0

what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter      KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE KEY -9999999, -1 ;
           FIELDS F000 = LEFT(CUS_NAME,24):24:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && ��爼�牀���┘ 甎�瓷� � 竄▲爨ぅ�┘
    c_code = CUS_CODE
   IF TYPE("c_name") = "C"
     c_name = CUS_NAME
   ENDIF

   EXIT

  OTHERWISE

    EXIT

  ENDCASE

ENDDO
USE

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

SELECT(s_sav)

RETURN
