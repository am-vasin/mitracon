*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Ch_Bank      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                              �襦�� �����.                              �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 14.01.1999 様�
PROCEDURE Ch_Bank
PARAMETERS f_code, f_name_b, f_name_l, b_code, b_name_b, b_name_l, all_f
*
*  f_code   - ��� �爛く爬閧��, メ�� �� 礫���� ┼� �竅�, 皰 ≡�;
*  f_name_b - ��� �爛く爬閧��, ������錺矚� �牀������� メ�� ��� �竅�;
*  f_name_l - ��� �爛く爬閧��, ������錺矚� �牀������� メ�� ��� �竅�;
*  b_code   - ��� �����;
*  b_name_b - ��牀皖�� ━� �����;
*  b_name_l - ��┃��� ━� �����;
*  all_f    - ��� ≡ュ �爛く爬閧┤.
*

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
PRIVATE k_w, mss
SELECT 0
DO Use_Dummy
SELECT 0
IF all_f .OR. EMPTY(f_code)
  k_w = ""
  USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS F_9114 AGAIN
  SELECT 0
  USE (base_path+"BANKS") ORDER TAG ALL_NAMES ALIAS B_9114 AGAIN
  SET FILTER TO EMPTY(DATE_OFF) .AND. EMPTY(HIDE)
  SET RELATION TO FIRM_CODE INTO F_9114
  IF .NOT. EMPTY(f_code) .AND. .NOT. EMPTY(b_code)
    SET ORDER TO TAG BANK
    IF .NOT. SEEK(f_code+b_code) .OR. .NOT. EMPTY(DATE_OFF)
      SET ORDER TO TAG ALL_NAMES
      GO TOP
    ELSE
      SET ORDER TO TAG ALL_NAMES
    ENDIF
  ENDIF
ELSE
  k_w = f_code
  USE (base_path+"BANKS") ORDER TAG NAME ALIAS B_9114 AGAIN
  IF .NOT. SEEK(k_w)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"��� �爛く爬皋� �モ �� �き��� �����!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    USE
    SELECT (s_sav)
    RETURN
  ENDIF
  IF .NOT. EMPTY(b_code)
    SET ORDER TO TAG BANK
    IF .NOT. SEEK(f_code+b_code)
      SET ORDER TO TAG NAME
      SEEK k_w
    ELSE
      IF .NOT. EMPTY(DATE_OFF)
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"�皰� ���� 磴��キ!"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        USE
        SELECT (s_sav)
        RETURN
      ENDIF
      SET ORDER TO TAG NAME
    ENDIF
  ENDIF
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
PRIVATE ln, wd, tit
ln = WROWS("")-IIF(EMPTY(k_w), 14, 11)   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 16

DO D_Wins WITH ln, wd, "���甌� ������", 0, IIF(EMPTY(k_w), 1, 0)
IF EMPTY(k_w)
  @ WROWS()-3, 3 SAY "踳く爬閧┘"
ENDIF
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
    BROWSE KEY k_w FIELDS B_NAME:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && �襦��

    IF EMPTY(k_w)
      f_code   = F_9114.FIRM_CODE
      f_name_b = F_9114.B_NAME
      f_name_l = F_9114.L_NAME
      b_code   = B_9114.BANK_CODE
      IF TYPE("b_name_b") = "C"
        b_name_b = B_9114.B_NAME
      ENDIF
      IF TYPE("b_name_l") = "C"
        b_name_l = B_9114.L_NAME
      ENDIF
    ELSE
      b_code   = B_9114.BANK_CODE
      IF TYPE("b_name_b") = "C"
        b_name_b = B_9114.B_NAME
      ENDIF
      IF TYPE("b_name_l") = "C"
        b_name_l = B_9114.L_NAME
      ENDIF
    ENDIF
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
IF EMPTY(k_w)
  SELECT F_9114
  USE
ENDIF
SELECT (s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Draw_Foot    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                   ��痺ガ ぎ�����皀�讚竡 ┃筮爼�罔�.                    �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 20.10.1998 陳�
PROCEDURE Draw_Foot

IF EMPTY(k_w)
  ACTIVATE WINDOW (win_name+"_M") SAME

  *
  *   ��� Гメ�, 爬痺ガ...
  *
  @ WROWS()-2, 2 SAY F_9114.B_NAME

  ACTIVATE WINDOW (ALIAS()) SAME
ENDIF

RETURN .T.
