*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� To_Dev       ��о�｀砒┴ ��むォ ��瓱�           02.09.98 12:05:59 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                          ��瘡�� ��о�｀砒┴�.                          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE To_Dev
PRIVATE stack, titles
DIMENSION titles(1)
titles(1) = "�ムョキ� �甅�↓諷 皀�"

*DO Dev_Mess

stack = ""

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

DEFINE PAD View OF (menu_name) PROMPT "蹍甃�矗" KEY Ctrl-A, "Enter"
ON SELECTION PAD View OF (menu_name) DO Sw_Mode WITH "View"

DEFINE PAD Return OF (menu_name) PROMPT "�ム�竄赱�" KEY Ctrl-A, "Esc"
ON SELECTION PAD Return OF (menu_name) DO Sw_Mode WITH "Return"

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*

DO Use_Dummy
SELECT 0
USE (base_path+"T_LIST")

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = MIN(100, RECCOUNT())   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = FSIZE("P_NAME")+FSIZE("TERM")

DO D_Wins WITH ln, wd, "", 0, 0
what_do = IIF(RECCOUNT("T_LIST") > 1, "List", "View")

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    ACTIVATE WINDOW (win_name+"_M") SAME
    @ 0, 0 SAY PADC(titles(ALEN(titles)), WCOLS()) COLOR SCHEME 5
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE FIELDS F000 = P_NAME+TERM,  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           FOR LEFT(LEVEL,LEN(stack)) == stack .AND. EMPTY(RIGHT(LEVEL,6-LEN(stack))) ;
               .AND. .NOT. EMPTY(SUBSTR(LEVEL,LEN(stack)+1,3)) ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           REST ;
           WHEN Md_Menu()  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Return", "View")
    ENDIF

  CASE what_do = "View"    && 蹍甃�矗 甎�瓷�

    IF EMPTY(TERM)
      DO Mss_List WITH T_LIST.LEVEL, T_LIST.P_NAME, user
    ELSE
      DIMENSION titles(ALEN(titles)+1)
      titles(ALEN(titles)) = ALLTRIM(P_NAME)
      stack = stack+SUBSTR(LEVEL,LEN(stack)+1,3)
    ENDIF
    what_do = IIF(RECCOUNT("T_LIST") > 1, "List", "Return")

  OTHERWISE

    IF ALEN(titles) = 1
      EXIT
    ELSE
      DIMENSION titles(ALEN(titles)-1)
      LOCATE FOR LEVEL = PADR(stack,9)
      stack = LEFT(stack,LEN(stack)-3)
      what_do = "List"
    ENDIF
  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
CLOSE DATABASES
RELEASE MENU (menu_name) EXTENDED

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�              ��玻オ罔� �キ� � ��※瓱��痰� �� 皋�� �祗���.              �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 02.09.98 13:54:11 夕
PROCEDURE Md_Menu

IF EMPTY(TERM)
  DEFINE PAD View OF (menu_name) PROMPT "蹍甃�矗" KEY Ctrl-A, "Enter" BEFORE Return
  ON SELECTION PAD View OF (menu_name) DO Sw_Mode WITH "View"
ELSE
  DEFINE PAD View OF (menu_name) PROMPT "�襦��" KEY Ctrl-A, "Enter" BEFORE Return
  ON SELECTION PAD View OF (menu_name) DO Sw_Mode WITH "View"
ENDIF

ACTIVATE SCREEN
@ 0,0 SAY SPACE(WCOLS()-15) COLOR SCHEME 13
ACTIVATE WINDOW (ALIAS())
SHOW MENU (menu_name)

RETURN .T.
