*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Old_Docs     ��о�｀砒┴ ��むォ ��瓱�           03.02.97 23:48:28 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                     蹍甃�矗 � �ョ�碎 ぎ�祠キ皰�.                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Old_Docs
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
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*

DO Use_Dummy

SELECT 0
USE PATTERN ORDER TAG CODE

PRIVATE key_l, doc_l, f_len
f_len = 1
DIMENSION key_l(f_len), doc_l(f_len)
key_l(f_len) = "     "
doc_l(f_len) = PADR("�瓮 ぎ�祠キ硅", FSIZE("DOC_PROMPT"))
SCAN
  f_len = f_len+1
  DIMENSION key_l(f_len), doc_l(f_len)
  key_l(f_len) = STR(DOC_CODE,5)
  doc_l(f_len) = DOC_PROMPT
ENDSCAN

SELECT 0
USE TXT_DOCS
SET RELATION TO DOC_CODE INTO PATTERN
GO BOTTOM

IF BOF() .OR. EOF()
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�モ �� �き��� ぎ�祠キ��!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  RETURN
ENDIF

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Print OF (menu_name) PROMPT "�ョ�碎 ぎ�祠キ��" KEY Ctrl-A, "Enter"
ON SELECTION PAD Print OF (menu_name) Do Sw_Mode WITH "Print"

DEFINE PAD Filter OF (menu_name) PROMPT "�┼赳�" KEY Ctrl-A, "F1"
ON SELECTION PAD Filter OF (menu_name) Do Sw_Mode WITH "Filter"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd, set_f, pic
ln = WROWS("")-9   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = FSIZE("DOC_PROMPT", "PATTERN")+ ;
     FSIZE("VISUAL_L", "TXT_DOCS")+ ;
     FSIZE("DOC_TIME", "TXT_DOCS")+ ;
     FSIZE("DOC_NUM", "TXT_DOCS")+ ;
     IIF(SET("CENTURY") = "ON", 14, 12)
pic = "@S"+ALLTRIM(STR(FSIZE("DOC_PROMPT", "PATTERN")+  ;
                      FSIZE("VISUAL_L", "TXT_DOCS")+1))
set_f = key_l(1)

DEFINE WINDOW Filter FROM 1, WCOLS("")-24 TO  ;
                          3, WCOLS("")-4 SHADOW COLOR SCHEME 5
ACTIVATE WINDOW Filter NOSHOW
@ 0, 1 SAY "�┼赳� 竅����←キ"
ACTIVATE SCREEN

DO D_Wins WITH ln, wd, "���甌� ぎ�祠キ皰�", 1, 1
@ 1, 2 SAY PADC("����", IIF(SET("CENTURY") = "ON", 10, 8))+" "+  ;
           PADC("�爛��", FSIZE("DOC_TIME", "TXT_DOCS"))+" "+     ;
           PADC("N ぎ�", FSIZE("DOC_NUM", "TXT_DOCS"))+" "+      ;
           "�┓ ぎ�祠キ��"

what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F1    Do Sw_Mode WITH "Filter"

*
*    BROWSE - �キ�
*
    IF EMPTY(set_f)
      SET ORDER TO 0
      BROWSE FIELDS DOC_DATE:H="",     ;
                    DOC_TIME:H="",     ;
                    DOC_NUM:H="",      ;
                    F001 = ALLTRIM(PATTERN.DOC_PROMPT)+" "+ ;
                           ALLTRIM(VISUAL_L):H="":P=pic, ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             WHEN Visual_F()             ;
             NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ELSE
      SHOW WINDOW Filter TOP
      SET ORDER TO TAG DOC_NUM
      BROWSE KEY set_f                 ;
             FIELDS DOC_DATE:H="",     ;
                    DOC_TIME:H="",     ;
                    DOC_NUM:H="",      ;
                    F001 = ALLTRIM(PATTERN.DOC_PROMPT)+" "+ ;
                           ALLTRIM(VISUAL_L):H="":P=pic, ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             WHEN Visual_F()             ;
             NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ENDIF
    ON KEY
    HIDE WINDOW Filter
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "exit", "Print")
    ENDIF

  CASE what_do = "Print"    && �ョ�碎 ぎ�祠キ��

    DO P_Doc_T
    what_do = "List"

  CASE what_do = "Filter"   && �痰���→�/甅閧┘ 筥�赳��

    set_f = Set_Fl(set_f)
    what_do = "List"

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
*�                  蹍������ 竅����→�/甅閧�� 筥�赳��.                   �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 04.02.97 00:40:41 夕
PROCEDURE Set_Fl
PARAMETER old_v
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

PRIVATE i, ln, wd, s
wd = LEN(doc_l(1))+4
ln = MIN(WROWS("")-10, f_len+3)
i = MAX(1, ASCAN(key_l, old_v))

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH ln, wd, "�┼赳�"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
DO WHILE .T.
  i = MAX(i, 1)
  i = MIN(i, f_len)
  @ 1, 1 MENU doc_l, f_len, wd-4
  READ MENU TO i
  s = READKEY() % 256
  IF s = 12
    i = old_v
    EXIT
  ENDIF
  IF s = 15
    i = key_l(i)
    EXIT
  ENDIF
ENDDO

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN i

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�              �ョ�碎 ※с��├�珮ガ��� ���� ↓├� ��゛�肓.               �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 23.02.97 12:05:43 夕
PROCEDURE Visual_F

ACTIVATE WINDOW (win_name+"_M") BOTTOM
@ WROWS()-2, 2 SAY PADC(VISUAL_F,WCOLS()-4)
ACTIVATE WINDOW (ALIAS())

RETURN .T.
