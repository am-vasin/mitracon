*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Skl_Type     ��о�｀砒┴ ��むォ ��瓱�           23.10.97 08:59:08 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                              �┓� �覩���.                              �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Slr_Type

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

DEFINE PAD Corr OF (menu_name) PROMPT "��玻オ罔�" KEY Ctrl-A, "Enter"
ON SELECTION PAD Corr OF (menu_name) Do Sw_Mode WITH "Corr"

DEFINE PAD Add OF (menu_name) PROMPT "����←キ┘" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) Do Sw_Mode WITH "Add"

DEFINE PAD Move_T OF (menu_name) PROMPT "�ムガメ皋碎"
ON PAD Move_T OF (menu_name) ACTIVATE POPUP Move_T

  DEFINE POPUP Move_T MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR 1 OF Move_T PROMPT "�ムガメ皋碎 ↓├" KEY Ctrl-A, "F1"
  ON SELECTION BAR 1 OF Move_T Do Sw_Mode WITH "move_dn"
  
  DEFINE BAR 2 OF Move_T PROMPT "�ムガメ皋碎 □ム�" KEY Ctrl-A, "F2"
  ON SELECTION BAR 2 OF Move_T Do Sw_Mode WITH "move_up"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "Exit"

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*

USE (base_path+"SLR_TYPE") ORDER TAG RANG

DO Use_Dummy
SELECT SLR_TYPE

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = 100   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 24

DO D_Wins WITH ln, wd, "�┓� �覩���", 0, 0
what_do = IIF(BETWEEN(RECNO("SLR_TYPE"), 1, RECCOUNT("SLR_TYPE")),   ;
          "List", "Add")

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins   Do Sw_Mode WITH "Add"
    ON KEY LABEL F1    Do Sw_Mode WITH "move_dn"
    ON KEY LABEL F2    Do Sw_Mode WITH "move_up"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS NAME:24:H="",   ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Corr")
    ENDIF

  CASE what_do = "Corr"   && ��玻オ罔�

    IF CODE < 0
      PRIVATE mss
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"�皰� 皋� ��玻オ罔� �� ��かウ��!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
    ELSE
      DO Modi_T WITH "C"
    ENDIF
    what_do = "List"

  CASE what_do = "Add"    && ����←キ┘

    DO Modi_T WITH "A"
    what_do = IIF(BETWEEN(RECNO("SLR_TYPE"), 1, RECCOUNT("SLR_TYPE")),  ;
               "List", "Exit")

  CASE what_do = "move_dn"   && �ムガメ皋碎 ↓├

    DO DownUp WITH 1
    what_do = "List"

  CASE what_do = "move_up"   && �ムガメ皋碎 □ム�

    DO DownUp WITH -1
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
*�                  ��え筥��罔�/ぎ��←キ┘ 皋�� �覩��硅.                  �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 23.10.97 09:52:46 夕
PROCEDURE Modi_T
PARAMETER a_or_c
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

*
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�        敖 ��� ��腮甄キ�� 陳陳陳陳朕         �
*�        � 旭旭碓旭芦旭旭碓旭芦旭旭 �         �
*�        青陳陳陳陳陳陳陳陳陳陳陳陳潰         �
*楽� ���讓祀閹�� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕�
*崖敖 �┓ 陳陳陳陳陳陳朕敖 �����モ琺 陳陳陳陳審�
*崖�( ) �祠��          崖                    崖�
*崖�( ) �┴瓱牀�. 罐�� 崖�え��肓 旭旭碓旭芦旭崖�
*崖�( ) �ムガキ��� 罐��崖                    崖�
*崖�( ) ｪ���          崖  ��辟�. 9999.999   崖�
*崖�( ) ���. ぅ��      崖  �キ� 9999999.99   崖�
*崖�( ) ���. ���       崖                    崖�
*崖青陳陳陳陳陳陳陳陳陳拈陳陳陳陳陳陳陳陳陳陳抒�
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰�
*�      < OK Ctrl-W > < �皖���碎瘴 Esc >       �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�


PRIVATE nm, s_type, unt, k, pr, ex
IF a_or_c = "A"
  nm     = SPACE(24)
  s_type = 1
  unt    = SPACE(12)
  k      = 1
  pr     = 0
ELSE
  nm     = SLR_TYPE.NAME
  s_type = VAL(ALLTRIM(SLR_TYPE.TYPE))
  unt    = SLR_TYPE.UNIT
  k      = SLR_TYPE.KOEFF
  pr     = SLR_TYPE.PRICE
ENDIF
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 17, 49, "�┓ ��腮甄キ��"
DO Sun_Bord WITH  2, 10,  4, 37, " ��� ��腮甄キ�� "
DO Sun_Bord WITH  5,  2, 14, 46, " ���讓祀閹�� "
DO Sun_Bord WITH  6,  3, 13, 23, " �┓ "
DO Sun_Bord WITH  6, 24, 13, 45, " �����モ琺 "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@  3, 12 GET nm WHEN Show_P()
@  7,  4 GET s_type PICTURE   ;
   "@*RV �祠��;�┴瓱牀�. 罐��;�ムガキ��� 罐��;ｪ���;���. ぅ��;���. ���" ;
   VALID Show_P()
@  8, 33 GET unt WHEN INLIST(s_type, 2, 3, 4, 5, 6)
@ 10, 34 GET k PICTURE "@Z 9999.999"  WHEN INLIST(s_type, 4, 5, 6)
@ 10, 32 GET pr PICTURE "@Z 9999999.99"  WHEN INLIST(s_type, 2, 3)
@ 15,  8 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Tst_Blank()

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                     �├���├�罔� ��� "�����モ琺".                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 23.10.97 14:59:08 夕
PROCEDURE Show_P

@ 7, 25 CLEAR TO 12, 44

IF INLIST(s_type, 2, 3, 4, 5, 6)
  @ 8, 25 SAY "�え��肓"
  SHOW GET unt
ENDIF

IF INLIST(s_type, 4, 5, 6)
  @ 10, 27 SAY "��辟�."
  SHOW GET k
ENDIF

IF INLIST(s_type, 2, 3)
  @ 10, 27 SAY "�キ�"
  SHOW GET pr
ENDIF

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                            �諷�� ├ ゛����.                            �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 23.10.97 16:06:24 夕
PROCEDURE Tst_Blank
PRIVATE cd

IF ex = 2 .OR. MOD(READKEY(), 256) = 12
  RETURN .T.
ENDIF

IF EMPTY(nm)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��Б��┘ 皋�� ��腮甄キ�� ぎΝ� °碎!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF s_type = 1
  k = 0
  pr = 0
ENDIF


IF INLIST(s_type, 2, 3)
  IF pr <= 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"��瘋キ�� ぎ�Ν� °碎 ｀�跏� �祀�!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
  k = 0
ENDIF

IF INLIST(s_type, 4, 5, 6)
  IF k <= 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"��辟筥罔モ ぎ�Ε� °碎 ｀�跏� �祀�!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
  pr = 0
ENDIF

IF a_or_c = "A"
  PRIVATE o_save, n_rang
  o_save = TAG()
  SET ORDER TO TAG CODE
  GO BOTTOM
  cd = CODE+1
  cd = MAX(cd, 1)
  SET ORDER TO RANG
  GO BOTTOM
  n_rang = 1
  IF .NOT. EOF() .AND. .NOT. BOF()
    n_rang = RANG+1
  ENDIF
  SET ORDER TO TAG (o_save)
  APPEND BLANK
ELSE
  n_rang = RANG
  cd = CODE
ENDIF

REPLACE CODE  WITH cd,   ;
        NAME  WITH nm,   ;
        TYPE  WITH STR(s_type,10), ;
        PRICE WITH pr,   ;
        KOEFF WITH k,    ;
        UNIT  WITH unt,  ;
        RANG  WITH n_rang

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                        �ムガラキ┘ 皋�� ��腮甄キ��.                    �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様 20.03.2002 夕
PROCEDURE DownUp
PARAMETERS rng
PRIVATE r_sav

IF EOF() .OR. BOF()
  RETURN
ENDIF
r_sav = RECNO()

SKIP rng
IF BOF() .OR. EOF()
  GO r_sav
  RETURN
ENDIF

REPLACE RANG WITH RANG - rng
GO r_sav
REPLACE RANG WITH RANG + rng

RETURN