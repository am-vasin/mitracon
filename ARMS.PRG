*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Arms         ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                         ｯ����┘ ��｀腮� �メ�.                         �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 03.09.1999 様�
PROCEDURE Arms

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
USE (base_path+"PLACES")
PRIVATE p_names, p_codes, p_count, f_char, f_name
f_char = ""
f_name = "�瓮 �����お�            "
p_count = 0
SCAN
  p_count = p_count+1
  DIMENSION p_names(p_count), p_codes(p_count)
  p_names(p_count) = P_NAME
  p_codes(p_count) = P_CHAR
ENDSCAN
USE (base_path+"PERSONS") ORDER TAG CODE
SELECT 0
USE (base_path+"ARM_INFO") ORDER TAG ARM_C
SET RELATION TO WHO INTO PERSONS
SELECT 0
USE (base_path+"ARM_LST") ORDER TAG NAME
SET RELATION TO ARM_C INTO ARM_INFO

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Modify OF (menu_name) PROMPT "��玻オ罔�" KEY Ctrl-A, "Enter"
ON SELECTION PAD Modify OF (menu_name) DO Sw_Mode WITH "Modify"

DEFINE PAD Add OF (menu_name) PROMPT "����※碎" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = WROWS("")-14   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 47
*����. �メ皰陳陳陳����讌���皀�貭陳陳陳陳陳陳陳陳朕
*�....:....!....:. ....:....!....:....!....:....!�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*敖��筮爼�罔鐵陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�  �ぅ ���. ....:....!....:....!                �
*�  �カヤ��  ....:....!....:.                    �
*�  �モ. ━� ....:....!....:....!                �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
DO D_Wins WITH ln, wd, "", 0, 3
@ 1, 2 SAY "���. �メ皰陳陳陳����讌���皀��"
@ WROWS()-5, 3 SAY "��筮爼�罔�"
@ WROWS()-4, 4 SAY "�ぅ ���."
@ WROWS()-3, 4 SAY "�カヤ��"
@ WROWS()-2, 4 SAY "�モ. ━�"

what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    ACTIVATE WINDOW (win_name+"_M") SAME
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins   DO Sw_Mode WITH "Add"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS ARM_N:H="", F000 = P_F():H="":30,  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Modify")
    ENDIF

  CASE what_do = "Modify"    && ��玻オ罔�

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO Arm_Info WITH ARM_C
    ENDIF
    what_do = "List"

  CASE what_do = "S_Filter"  && �痰���→� 筥�赳��

    DO S_Filter
    what_do = "List"

  CASE what_do = "C_Filter"  && �腮痰�� 筥�赳��

    f_char = ""
    f_name = "�瓮 �����お�            "
    SET FILTER TO
    what_do = "List"

  CASE what_do = "Add"       && ����←キ┘ ��｀腑�� �メ��

      DO Arm_Info WITH "   "
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
CLOSE DATABASES
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

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� Гメ�, 爬痺ガ...
*
@ WROWS()-4, 13 CLEAR TO WROWS()-2, WCOLS()-3
IF FOUND("ARM_INFO")
  @ WROWS()-4, 13 SAY ARM_INFO.LOCATE
  @ WROWS()-3, 13 SAY ARM_INFO.PHONE
  @ WROWS()-2, 13 SAY ARM_INFO.NET_NAME
ENDIF

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� P_F          ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      ç��お� か� ��｀���� �メ��.                      �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 03.09.1999 陳�
PROCEDURE P_F
PRIVATE i

RETURN ALLTRIM(PERSONS.FAMILY)+" "+LEFT(PERSONS.NAME,1)+" "+LEFT(PERSONS.S_NAME,1)

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� S_Filter     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           �痰���→� 筥�赳��.                           �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 03.09.1999 陳�
PROCEDURE S_Filter

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

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH MIN(WROWS("")-10, p_count+3), 28, "ç��お�"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
IF .NOT. EMPTY(f_char)
  ex = ASCAN(p_codes, f_char)
ENDIF
DO WHILE .T.
  ex = MIN(p_count, ex)
  ex = MAX(ex, 1)
  @ 1, 1 MENU p_names, p_count, WROWS()-3
  READ MENU TO ex
  IF READKEY() % 256 = 15
    f_char = p_codes(ex)
    f_name = p_names(ex)
    SET FILTER TO ARM_S = f_char
    EXIT
  ENDIF
  IF READKEY() % 256 = 12
    EXIT
  ENDIF
ENDDO    
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Add_P        ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                       ����←キ┘ ��｀腑�� �メ��.                       �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 03.09.1999 陳�
PROCEDURE Add_P
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�      ��Б��┘ ....:....!....:.      �
*�          敖陳陳陳陳陳陳陳陳陳陳陳朕 �
*� ç��お� �....:....!....:....!....� �
*�          青陳陳陳陳陳陳陳陳陳陳陳潰 �
*�  < OK Ctrl-W > < �皖���碎瘴 Esc >   �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex, p, nm
ex = 1
nm = SPACE(16)
p = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 8, 41, "��Г��┘ ��｀腑�� �メ��"
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@ 4,  3 SAY "ç��お�"
@ 2,  8 SAY "��Б��┘" GET nm
@ 3, 12 GET p PICTURE "@^" FROM p_names SIZE 3, 26
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Tst_Blank()

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Tst_Blank    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                            �諷�� ├ ゛����.                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 03.09.1999 陳�
PROCEDURE Tst_Blank

PRIVATE r_sav, to_add

IF ex = 2
  RETURN .T.
ENDIF

IF EMPTY(nm)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��Б��┘ �����お� �� ��Ε� °碎 �竅硅�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN 1
ENDIF

PRIVATE c_s
to_add = .T.
r_sav = RECNO()
SET ORDER TO ARM_C
IF .NOT. EMPTY(f_char)
  SET FILTER TO
ENDIF
GO BOTT
IF BOF() .OR. EOF()
  c_s = "  1"
ELSE
  c_s = RIGHT(STR(VAL(ARM_C)+1),3)
ENDIF
IF to_add
  APPEND BLANK
  REPLACE ARM_C    WITH c_s,        ;
          ARM_CODE WITH VAL(ALLTRIM(c_s)),   ;
          ARM_S    WITH p_codes(p), ;
          ARM_N    WITH nm
ENDIF

IF .NOT. EMPTY(f_char)
  IF f_char # ARM_S
    f_char = ""
    f_name = "�瓮 �����お�            "
  ELSE
    SET FILTER TO ARM_S = f_char
  ENDIF
ENDIF
SET ORDER TO TAG NAME

RETURN .T.
