*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                     ��玻オ罔� 筮爼��� ぎ�祠キ皰�.                     �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE NEW_WIND
PRIVATE _file,_d_file, mss, mn_w, ye_w

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
what_do    = "list"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Corr OF (menu_name) PROMPT IIF(RECCOUNT() = RECNO(), ;
               "��玻.", "蹍甃�矗") KEY Ctrl-A, "Enter"
ON SELECTION PAD Corr OF (menu_name) Do Sw_Mode WITH "Corr"

IF RECCOUNT() = 1

  DEFINE PAD Aff OF (menu_name) PROMPT "�癶ムえ碎" KEY Ctrl-A, "F5"
  ON SELECTION PAD Aff OF (menu_name) Do Sw_Mode WITH "Aff"

ELSE

  DEFINE PAD Ls_Tab OF (menu_name) PROMPT "��．��" KEY Ctrl-A, "F3"
  ON SELECTION PAD Ls_Tab OF (menu_name) Do Sw_Mode WITH "Ls_tab"

ENDIF

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"


*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*

***ｯ爛ぅ�錺� メ碎 �� 礒� ����� ����  
_file = base_path + "MOULD"
d_file = _file +".dbf"
 
 IF NOT FILE_O (d_file)
	DO MOULD WITH MONTH(DATE()),YEAR(DATE())
 ENDIF
 
 IF NOT USED("MOULD")
   SELECT 0
   USE (base_path+"MOULD") ORDER TAG MONTH
 ENDIF
 IF RECCOUNT() = 0
   DO Ad_Month
 ENDIF

 IF NOT USED("DUMMY")
 DO USE_DUMMY
 ENDIF

 SELECT MOULD
 GO BOTTOM
*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd, f_name, cl
*ln = MIN(100, RECCOUNT())   && ����腑痰〓 ※え�諷 痰牀� BROWSE
ln = 20   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 13

DO D_Wins WITH ln, wd, "��゛��� ", 0, 0
what_do = "list"
@ 1, 2 SAY "��メ閹陳����"
cl = SCHEME(13,1)+","+SCHEME(13,2)+",,,,,"+SCHEME(14,2)

f_name = ""

DO WHILE .T.
  DO CASE

  CASE what_do = "list"    && 蹍甃�矗 甎�瓷�

	statys_type = 3    
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F1    Do Sw_Mode WITH "Parms"
	ON KEY LABEL F5    Do Sw_Mode WITH "Aff"
*    ON KEY LABEL F6    Do Sw_Mode WITH "Add"
	ON KEY LABEL F3    Do Sw_Mode WITH "Ls_tab"

  SELECT MOULD
*
*    BROWSE - �キ�
*
    BROWSE FIELDS M000 = Month_R(MONTH) :8 :W=.F.:H="", ;
                  YEAR:H="",      ;
                  DUMMY.F FREEZE DUMMY.F ;
           WHEN Modi_Menu()       ;
           NOAPPEND NODELETE NOMODIFY NORGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")  &&COLOR (cl)
    ON KEY

    IF what_do = "list"
      what_do = IIF(READKEY() % 256 = 12, "exit", "Corr")
    ENDIF

  CASE what_do = "Corr"      && �襦�� ┃皀爛痺鉗ィ� �メ閹�
    ye_w = YEAR
    mn_w = MONTH
    DO Mould_A WITH ye_w, mn_w
    SEEK STR(ye_w,4)+STR(mn_w,2)
    what_do = "list"

  CASE what_do = "Ls_tab"    && ���甌� 甌矗磴�┴��

    IF RECNO() # RECCOUNT()
      DO Ls_Table 
    ENDIF
    what_do = "list"

  CASE what_do = "Aff"    && �癶ムえ碎 ��゛��
    IF RECNO() = RECCOUNT()
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0) + "  �癶ムΔ�ガ ��゛��? [ �� ] [ �モ ]"
      mss(3) = ""
      d = OUT_Mess(7,"mss")
      IF d = 1
        DO Ad_Month
      ENDIF
    ENDIF
    what_do = "list"

  OTHERWISE
	
    EXIT

  ENDCASE

ENDDO

CLEAR WINDOWS
CLOSE DATABASES
RELEASE MENU (menu_name) EXTENDED

RETURN
*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                     ��玻オ罔� �ム〓�� �祗��� �キ�.                     �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 25.09.97 18:44:21 夕
PROCEDURE Modi_Menu

IF RECCOUNT() = RECNO()
  DEFINE PAD Corr OF (menu_name) PROMPT "��玻." KEY Ctrl-A, "Enter"
  ON SELECTION PAD Corr OF (menu_name) Do Sw_Mode WITH "Corr"
  DEFINE PAD Aff OF (menu_name) BEFORE Exit PROMPT "�癶ムえ碎" KEY Ctrl-A, "F5"
  ON SELECTION PAD Aff OF (menu_name) Do Sw_Mode WITH "Aff"
  RELEASE PAD Ls_Tab OF (menu_name)
ELSE
  DEFINE PAD Ls_Tab OF (menu_name) AFTER Corr PROMPT "��．��" KEY Ctrl-A, "F3"
  ON SELECTION PAD Ls_Tab OF (menu_name) Do Sw_Mode WITH "Ls_tab"

  DEFINE PAD Corr OF (menu_name) PROMPT "蹍甃�矗" KEY Ctrl-A, "Enter"
  ON SELECTION PAD Corr OF (menu_name) Do Sw_Mode WITH "Corr"
  RELEASE PAD Aff OF (menu_name)
ENDIF

ACTIVATE SCREEN
@ 0, 0 SAY SPACE(WCOLS("")-11) COLOR (SCHEME(1,2))
SHOW MENU (menu_name)
ACTIVATE WINDOW (ALIAS()) SAME
RETURN
