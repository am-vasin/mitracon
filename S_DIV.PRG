*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� S_Div        ��о�｀砒┴ ���┃� ���矗┘���                 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                             �祚ム��Гカ�.                              �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 18.01.2000 様�
PROCEDURE S_Div

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

SELECT 0
DO Use_Dummy
SELECT 0
USE (base_path+"POPUL") ORDER TAG CODE ALIAS P0118 AGAIN

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Corr OF (menu_name) PROMPT "��玻オ皋牀��碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Corr OF (menu_name) DO Sw_Mode WITH "Corr"

DEFINE PAD Add OF (menu_name) PROMPT "����※碎" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"

DEFINE PAD Cont OF (menu_name) PROMPT "��Гカ�" KEY Ctrl-A, "Tab"
ON SELECTION PAD Cont OF (menu_name) DO Sw_Mode WITH "Cont"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = FSIZE("NAME")

DO D_Wins WITH ln, wd, "�祚ム��Гカ�", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins   DO Sw_Mode WITH "Add"
    ON KEY LABEL Tab   DO Sw_Mode WITH "Cont"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS NAME:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Corr")
    ENDIF

  CASE what_do = "Corr"    && ��玻オ罔�

    DO Modi_S
    what_do = "List"

  CASE what_do = "Cont"    && ��Гカ�

    DO Cont WITH CODE, NAME
    what_do = "List"

  CASE what_do = "Add"     && ����←キ┘

    DO Modi_S WITH .T.
    what_do = "List"

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

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Modi_S       ��о�｀砒┴ ���┃� ���矗┘���                 �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                   ��玻オ罔�/ぎ��←キ┘ 痺�ム��Гカ�.                   �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 18.01.2000 陳�
PROCEDURE Modi_S
PARAMETERS new

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
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ��ぅ爨�皀�讚�� ��痰� �牀������:
*

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�                                  �
*�     ��Б��┘ ....:....!....:     �
*�                                  �
*� < OK Ctrl-W > < �皖���碎瘴 Esc > �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, nm
ex = 1
nm = IIF(new, SPACE(FSIZE("NAME")), NAME)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 7, 38, IIF(new, "����←キ┘ ", "��玻オ罔� ")+"痺�ム��Гカ�"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 3, 7 SAY "��Б��┘" GET nm
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Tst_Blank()

IF ex = 1
  *
  * �矗���硅��ガ ゛���
  PRIVATE cd
  IF new
    GO BOTT
    IF BOF() .OR. EOF()
      cd = 1
    ELSE
      cd = CODE+1
    ENDIF
    APPEND BLANK
    REPLACE CODE WITH cd
  ENDIF
  REPLACE NAME WITH nm
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Tst_Blank    ��о�｀砒┴ ���┃� ���矗┘���                 �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                蹍▲爲� ���※�讚�痰� ������キ�� ゛����.                �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 18.01.2000 陳�
PROCEDURE Tst_Blank

PRIVATE mss

IF ex = 2
  RETURN .T.
ENDIF

IF EMPTY(nm)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��� �� ��Ε� °碎 �竅硅�..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Cont         ��о�｀砒┴ ���┃� ���矗┘���                 �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                        ��ぅ爨━�� 痺�ム��Гカ�.                        �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 18.01.2000 陳�
PROCEDURE Cont
PARAMETERS s_cd, s_nm

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

SELECT 0
DO Use_Dummy
SELECT 0
USE (base_path+"POPUL") ORDER TAG CODE ALIAS P_0118 AGAIN
SELECT 0
USE (base_path+"DIV_DET") ORDER TAG DIV ALIAS L_0118 AGAIN
SELECT 0
USE (base_path+"DIVISION") ORDER TAG B_NAME ALIAS D_0118 AGAIN
SET RELATION TO CODE INTO L_0118
SELECT 0
DELETE FILE (tmpo_path+"DIV_TMP.DBF")
CREATE DBF (tmpo_path+"DIV_TMP") ;
  ( DIV      N( 3), ;
    DIV_NAME C(FSIZE("DIV_NAME", "L_0118")), ;
    SUP_DIV  N( 2) )
SELECT D_0118
SCAN
  SELECT DIV_TMP
  APPEND BLANK
  REPLACE DIV      WITH D_0118.CODE,   ;
          DIV_NAME WITH D_0118.B_NAME, ;
          SUP_DIV  WITH IIF(FOUND("L_0118"), L_0118.SUP_DIV, 99)
  SELECT D_0118
ENDSCAN
USE
SELECT DIV_TMP
SET RELATION TO SUP_DIV INTO P_0118
GO TOP

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Insert OF (menu_name) PROMPT "���鈑�碎 � 痺�ム��Гカ" KEY Ctrl-A, "Enter"
ON SELECTION PAD Insert OF (menu_name) DO Sw_Mode WITH "Insert"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = FSIZE("DIV_NAME")+FSIZE("NAME","P_0118")+3

DO D_Wins WITH ln, wd, ALLTRIM(s_nm), 0, 0
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
    BROWSE FIELDS DIV_NAME:H="",  ;
                  MARK0 = IIF(SUP_DIV = s_cd, "�", " "):1:H="", ;
                  P_0118.NAME:H="",    ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Insert")
    ENDIF

  CASE what_do = "Insert"    && �痰�※碎!

    REPLACE SUP_DIV WITH s_cd
    SKIP
    what_do = "List"

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
PRIVATE mss
DIMENSION mss(3)
mss(1) = ""
mss(2) = CHR(0)+"  ��縲��━ ├�キキ��? [ �� ] [ �モ ]"
mss(3) = ""
IF Out_Mess(5, "mss") = 1
  SELECT L_0118
  SET ORDER TO
  GO TOP
  SELECT DIV_TMP
  GO TOP
  SCAN
    SELECT L_0118
    IF EOF() .OR. BOF()
      APPEND BLANK
    ENDIF
    REPLACE DIV      WITH DIV_TMP.DIV,  ;
            DIV_NAME WITH DIV_TMP.DIV_NAME, ;
            SUP_DIV  WITH DIV_TMP.SUP_DIV
    SKIP
    SELECT DIV_TMP
  ENDSCAN
  SELECT L_0118
  IF .NOT. BOF() .AND. .NOT. EOF()
    SKIP
    DELETE REST
  ENDIF
ENDIF
SELECT DIV_TMP
USE
SELECT P_0118
USE
SELECT L_0118
USE
DELETE FILE (tmpo_path+"DIV_TMP.DBF")
SELECT (s_sav)

RETURN
