*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Cus_Type     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                          ��皀��爬� ��┘�皰�.                           �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 07/05/2000 様�
PROCEDURE Cus_Type

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
USE (base_path+"CUS_TYPE") ALIAS C_T_0705 AGAIN ORDER TAG CODE ASCENDING

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Modify OF (menu_name) PROMPT "��� ��皀��爬�" KEY Ctrl-A, "Enter"
ON SELECTION PAD Modify OF (menu_name) DO Sw_Mode WITH "Modify"

DEFINE PAD Add OF (menu_name) PROMPT "����※碎" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"

DEFINE PAD Corr OF (menu_name) PROMPT "踳く爬閧��" KEY Ctrl-A, "Tab"
ON SELECTION PAD Corr OF (menu_name) DO Sw_Mode WITH "Corr"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = FSIZE("NAME")

DO D_Wins WITH ln, wd, "�┓� ��┘�皰�", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins   DO Sw_Mode WITH "Add"
    ON KEY LABEL Tab   DO Sw_Mode WITH "Corr"

*
*    BROWSE - �キ�
*
    BROWSE KEY STR(user_dep,3)   ;
           FIELDS NAME:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Modify")
    ENDIF

  CASE what_do = "Modify"    && ��玻オ罔� ━キ�

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO Modi_Item
    ENDIF
    what_do = "List"

  CASE what_do = "Add"        && ����←キ┘ 皋��

    DO Modi_Item WITH .T.
    what_do = "List"

  CASE what_do = "Corr"        && 踳く爬閧��

    DO Modi_List
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
*�   蹍罐ゃ�� Modi_Item    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�          ��Г��┘ ��〓� ��皀��爬� ┼� ��玻オ罔� ��━キ������.          �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 07/05/2000 陳�
PROCEDURE Modi_Item
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�  敖 ��Б��┘ 陳陳陳陳陳陳陳朕  �
*�  � ....:....!....:....!.... �  �
*�  青陳陳陳陳陳陳陳陳陳陳陳陳潰  �
*�< OK Ctrl-W > < �皖���碎瘴 Esc >�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, nm
ex = 1
IF new
  nm = SPACE(24)
ELSE
  nm = NAME
ENDIF
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 7, 36, IIF(new, "����←キ┘ ", "��玻オ罔� ")+"皋��"
DO Sun_Bord WITH  2,  4,  4, 31, " ��Б��┘ "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@ 3, 6 GET nm
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Tst_Name()

IF ex = 1
  *
  * �矗���硅��ガ ゛���
  IF new
    SET ORDER TO TAG CODE DESCENDING
    IF SEEK(STR(user_dep,3))
      ex = CODE+1
    ELSE
      ex = 0
    ENDIF
    SET ORDER TO TAG CODE ASCENDING
    APPEND BLANK
    REPLACE CODE WITH ex, DEP WITH STR(user_dep,3)
  ENDIF
  REPLACE NAME WITH nm
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Tst_Name     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      蹍▲爲� ��玻オ皚�痰� ━キ�.                      �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 07/05/2000 陳�
PROCEDURE Tst_Name

IF ex = 2
  RETURN .T.
ENDIF

IF EMPTY(nm)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��Б��┘ �� ��Ε� °碎 �竅硅�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Modi_List    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                  ���甌� �爛く爬閧┤ か� 皋�� ��┘���.                  �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 05.07.2000 陳�
PROCEDURE Modi_List

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
PRIVATE s_sav, p_list, n_list, i
s_sav = SELECT()

SELECT 0
USE (base_path+"CUS_LINK") ORDER TAG TYPE ALIAS LNK_0705 AGAIN

SELECT 0
USE (base_path+"FIRMS") ORDER TAG NAME ALIAS FRM_0705 AGAIN

n_list = 0
SCAN
  n_list = n_list+1
  DIMENSION p_list(n_list)
  p_list(n_list) = IIF(.NOT. SEEK(STR(user_dep,3)+STR(C_T_0705.CODE,3)+FIRM_CODE, "LNK_0705"), "�", " ")+ ;
                   FIRM_CODE+" "+B_NAME
ENDSCAN
USE
SELECT LNK_0705

*
*敖陳陳陳陳陳陳陳陳陳陳�
*鴨CCC NNNNNNNNNNNNNNNN�
*青陳陳陳陳陳陳陳陳陳陳�
*  Enter  - �皙モ�碎
*  Ctrl_W - ��縲���碎
*  Esc    - �皖���碎瘴

PRIVATE ex, ln, i, sw
ex = 1
ln = MIN(n_list+6, WROWS("")-4)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH ln, 25, ALLTRIM(C_T_0705.NAME)
@ 1, 0 CLEAR TO WROWS()-1, WCOLS()-1
@ WROWS()-3, 3 SAY "Enter  - �皙モ�碎"
@ WROWS()-2, 3 SAY "Ctrl_W - ��縲���碎"
@ WROWS()-1, 3 SAY "Esc    - �皖���碎瘴"
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
ON KEY LABEL INS KEYBOARD "{Tab}"
DO WHILE .T.
  ex = MAX(ex, 1)
  ex = MIN(ex, n_list)
  @ 1, 1 MENU p_list, n_list, ln-6
  READ MENU TO ex
  IF READKEY() % 256 = 12
    ex = 2
    EXIT
  ENDIF
  IF READKEY() % 256 = 14
    IF C_T_0705.CODE = 0
      sw = .T.
      FOR i = 1 TO n_list
        IF .NOT. EMPTY(LEFT(p_list(i),1))
          sw = .F.
          EXIT
        ENDIF
      ENDFOR
      IF sw
        PRIVATE mss
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"��� 轤��� 皋�� ぎ�Ν� °碎 ぎ痰祚�� 絎硼 ° �き� �爛く爬閧┘!"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        LOOP
      ENDIF
    ENDIF
    ex = 1
    EXIT
  ENDIF
  IF READKEY() % 256 = 15
    IF EMPTY(LEFT(p_list(ex),1))
      p_list(ex) = "�"+SUBSTR(p_list(ex),2)
    ELSE
      p_list(ex) = " "+SUBSTR(p_list(ex),2)
    ENDIF
  ENDIF
ENDDO
*--------------------------------------------------------------------------
IF ex = 1
  DO WHILE SEEK(STR(user_dep,3)+STR(C_T_0705.CODE,3))
    REPLACE DEP WITH "", TYPE WITH "", FIRM WITH ""
  ENDDO
  FOR i = 1 TO n_list
    IF .NOT. EMPTY(LEFT(p_list(i),1))
      LOOP
    ENDIF
    GO TOP
    IF .NOT. EMPTY(TYPE) .OR. BOF() .OR. EOF()
      APPEND BLANK
    ENDIF
    REPLACE DEP  WITH STR(user_dep, 3),     ;
            TYPE WITH STR(C_T_0705.CODE,3), ;
            FIRM WITH SUBSTR(p_list(i),2,3)
  ENDFOR
ENDIF

POP KEY
USE
SELECT (s_sav)
RELEASE WINDOW (win_name)
RETURN
