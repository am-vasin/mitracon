*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Patterns     ��о�｀砒┴ ��むォ ��瓱�           31.01.97 20:12:55 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�          蹍������ 甌Г���� � ��玻オ罔� ��゛���� ぎ�祠キ皰�.           �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Patterns

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

DEFINE PAD Del OF (menu_name) PROMPT "����キ┘" KEY Ctrl-A, "Del"
ON SELECTION PAD Del OF (menu_name) Do Sw_Mode WITH "Del"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "Exit"

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*

DO Use_Dummy

SELECT 0
USE PATTERN

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = 100   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = FSIZE("DOC_PROMPT")+2

DO D_Wins WITH ln, wd, "��゛��� ぎ�祠キ皰�", 0, 0
what_do = IIF(RECCOUNT() > 0, "List", "Add")

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins   Do Sw_Mode WITH "Add"
    ON KEY LABEL Del   Do Sw_Mode WITH "Del"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS F001 = IIF(EMPTY(DATE_OFF), " ", ""):H="",  ;
           DOC_PROMPT:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Corr")
    ENDIF

  CASE what_do = "Corr"    && ��玻オ罔� ぎ�祠キ��

    DO Corr_Doc WITH "Corr"
    what_do = "List"

  CASE what_do = "Add"     && ����←キ┘ ぎ�祠キ��

    DO Corr_Doc WITH "Add"
    what_do = IIF(RECCOUNT() = 0, "Exit", "List")

  CASE what_do = "Del"     && ����キ┘/〓瘁����←キ┘ ぎ�祠キ��

    IF EMPTY(DATE_OFF)
      REPLACE DATE_OFF WITH DATE()
    ELSE
      REPLACE DATE_OFF WITH {}
    ENDIF
    what_do = IIF(RECCOUNT() = 0, "Exit", "List")

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
*�                    ��玻オ罔� ぎ��←キ┘ ぎ�祠キ��.                     �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 31.01.97 20:29:51 夕
PROCEDURE Corr_Doc
PARAMETERS mode

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

PRIVATE ex, d_name, d_text, p_text, p_numb, p_len, fnt, ff_sw, ff_ln
PRIVATE i
                     &&   ｡褊←錺� � ������錺� ���� ゛����
ex   = 1             &&
IF mode = "Add"
  d_name = SPACE(40)
  d_text = ""
  p_numb = 1
  p_len  = 66
  fnt    = 1
  ff_sw  = 1
  ff_ln  = 0
  DIMENSION p_text(1)
  p_text(1) = ""
ELSE
  d_name = DOC_PROMPT
  d_text = PATTERN
  p_len  = PAGE_LEN
  fnt    = IIF(EMPTY(FONT), 1, 2)
  ff_sw  = IIF(F_FEED < 0, 1, 2)
  ff_ln  = MAX(F_FEED, 0)
  p_numb = 0
  i = AT(CHR(12),d_text)
  DO WHILE i # 0
    p_numb = p_numb+1
    DIMENSION p_text(p_numb)
    p_text(p_numb) = LEFT(d_text, i-1)
    d_text = SUBSTR(d_text, i+1)
    i = AT(CHR(12),d_text)
  ENDDO
  p_numb = p_numb+1
  DIMENSION p_text(p_numb)
  p_text(p_numb) = d_text
ENDIF
  
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 13, 57, "�����モ琺 ��゛���"
DO Sun_Bord WITH 3, 7, 5, 49
@ 3, 9 SAY " �爬籵 "
DO Sun_Bord WITH 6, 7, 8, 49
@ 6, 9 SAY " 蹍��� � ���罐 ぎ�祠キ�� "
i = .F.

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�  ���祠キ� ....:....!....:....!....:....!....:....!  �
*�     敖 �爬籵 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕     �
*�     �      ( ) ��爼��讚覃     ( ) ���硅�      �     �
*�     青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�     �
*�     敖 蹍��� � ���罐 ぎ�祠キ�� 陳陳陳陳陳陳陳�     �
*�     �     ( ) ��爼���    ( ) �矗��    999     �     �
*�     青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�     �
*�   ��甄� 痰����� 99    ��┃� 痰����肓 � 痰牀��� 99   �
*�               [ ] ��ぅ爨━�� ��゛���                �
*�          < OK Ctrl-W > < �皖���碎瘴 Esc >           �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

@ 2,  4 SAY "���祠キ�" GET d_name VALID Tst_F(1)
@ 4, 14 GET fnt PICTURE "@*RH ��爼��讚覃    ;���硅�"
@ 7, 13 GET ff_sw PICTURE "@*RH ��爼���   ;�矗��" VALID Tst_F(2)
@ 7, 41 GET ff_ln PICTURE "@Z 999" WHEN ff_sw = 2  VALID Tst_F(3)
@ 9,  5 SAY "��甄� 痰�����" GET p_numb PICTURE "@Z 99" VALID Tst_F(4)
@ 9, 25 SAY "��┃� 痰����肓 � 痰牀���" GET p_len PICTURE "@Z 99" VALID Tst_F(5)
@ 10, 17 GET i PICTURE "@*C ��ぅ爨━�� ��゛���" VALID Tst_F(6)
@ 11, 14 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "
READ CYCLE

IF ex = 1
  IF mode = "Add"
    SET ORDER TO TAG CODE
    GO BOTTOM
    IF BOF() .OR. EOF()
      ex = 1
    ELSE
      ex = DOC_CODE+1
    ENDIF
    APPEND BLANK
    REPLACE DOC_CODE WITH ex
  ENDIF
  d_text = ""
  FOR ex = 1 TO ALEN(p_text)-1
    d_text = d_text+p_text(ex)+CHR(12)
  ENDFOR
  d_text = d_text+p_text(ALEN(p_text))
  REPLACE DOC_PROMPT WITH d_name,  ;
          PAGE_LEN   WITH p_len,   ;
          FONT       WITH IIF(fnt = 1, " ", "1"), ;
          F_FEED     WITH IIF(ff_sw = 1, -1, ff_ln),  ;
          PATTERN    WITH d_text
ENDIF

*--------------------------------------------------------------------------
POP KEY
RELEASE WINDOW (win_name)
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�               蹍������ �牀▲爲� �諷��� ├ ���ォ ゛����.               �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 31.01.97 22:33:49 夕
PROCEDURE Tst_F
PARAMETER fld

PRIVATE mss, m, w

IF READKEY() % 256 = 12 .OR. ex = 2
  RETURN .T.
ENDIF

DO CASE
CASE fld = 1        && ��Б��┘ ぎ�祠キ��
  IF EMPTY(d_name)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"鉑皰� ��Б��┘?... �� �� ��ぎ °..."
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
CASE fld = 2        && �ムオ�鈑�皀�� �牀����
  IF ff_sw = 1
    ff_ln = 0
    SHOW GET ff_ln
  ENDIF
CASE fld = 3        && ��甄� 痰牀� �牀����
  IF ff_ln < 0
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"��� 瓷��讓� 痰牀� �牀���碎 � ���罐 ぎ�祠キ��?"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
CASE fld = 4        && ��甄� 痰����� ぎ�祠キ��
  IF p_numb <= 0
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"��� 瓷��讓� 痰����� ぎ�Ν� °碎 � ぎ�祠キ皀?"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
  IF p_numb < ALEN(p_text)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"  �� ��皀瑙モ� �き� ┼� �メ���讓� 痰�����. �皰 ���※�讚�? [ �� ] [ �モ ]"
    mss(3) = ""
    IF Out_Mess(7, "mss") = 2
      RETURN .F.
    ENDIF
    DIMENSION p_text(p_numb)
  ENDIF
  IF p_numb > ALEN(p_text)
    DIMENSION p_text(p_numb)
    FOR m = 1 TO ALEN(p_text)
      w = p_text(m)
      IF TYPE("w") # "C"
        p_text(m) = ""
      ENDIF
    ENDFOR
  ENDIF
CASE fld = 5        && ��┃� 痰����肓
  IF p_len <= 3
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"�� ��牀皖� �� 痰������?"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
CASE fld = 6        && ��ぅ爨━�� ��゛���
  IF i
    DO Modi_Shab
    i = .F.
    SHOW GET i
*    KEYBOARD "{Tab}"
  ENDIF
ENDCASE

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                           ��玻オ罔� ��゛���.                           �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 31.01.97 23:05:20 夕
PROCEDURE Modi_Shab

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

PRIVATE ex, p_flt, s, mss
ex    = 0
p_flt = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH WROWS("")-4, WCOLS("")-2, ALLTRIM(d_name)
@ 1, 1 CLEAR
DO Sun_Bord WITH 1, 1, WROWS()-2, WCOLS()-2
@ WROWS()-1, 0 SAY PADC("PgUp - �爛�. 痰������,  PgDn - 甄イ. 痰������, Esc - �諷��", WCOLS())

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
ON KEY LABEL Escape KEYBOARD CHR(23)
ON KEY LABEL PgUp   DO Mov_P WITH -1
ON KEY LABEL PgDn   DO Mov_P WITH  1
DO WHILE .T.
  ex = 0
  s = PADL("�矗. "+ALLTRIM(STR(p_flt))+" ",9)
  @ 0, WCOLS()-10 SAY s COLOR SCHEME 5
  @ 2,2 EDIT p_text(p_flt) SIZE WROWS()-4, WCOLS()-4
  READ CYCLE
  s = 1
  DO WHILE AT(CHR(13)+CHR(10), p_text(p_flt), s) # 0
    s = s+1
  ENDDO
  IF s > p_len
    DIMENSION mss(5)
    mss(1) = ""
    mss(2) = CHR(0)+"��������!  ��甄�  痰牀� �� 痰����罐 �爛�謌�モ 礫�����竡  ���� Ε"
    mss(3) = CHR(0)+"か┃� 痰����肓! �� ぎ痰�皰膈� 絎牀莅 �爛め��←錺皀 ��� 轤� ＜ぅ�"
    mss(4) = CHR(0)+"� ぎ�祠キ皀?                                                    "
    mss(5) = ""
    DO Out_Mess WITH 7, "mss"
  ENDIF
  IF ex = 0
    EXIT
  ENDIF
  p_flt = p_flt+ex
  p_flt = MAX(p_flt,1)
  p_flt = MIN(p_flt,ALEN(p_text))
ENDDO
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                   ｡��｀砒┴ ���※荐 甃キ� 痰����肓.                   �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 31.01.97 23:24:07 夕
PROCEDURE Mov_p
PARAMETER n

ex = n
KEYBOARD CHR(23)

RETURN
