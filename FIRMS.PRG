*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Firms        ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                     ��玻オ罔� 甎�瓷� �爛く爬閧┤.                      �
*� ��え筥罔牀���� �.���腑�覓 - 竅���.���� DEF_WCHK � LET_WCHK � FIRMS     �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 01/12/1999 様�
PROCEDURE Firms

PRIVATE shab1, shab2
shab1 = "abcdefghijklmnopqrstuvwxyz�、�ぅ颱Ж�������珀矣粤肄蓍裨跋鈿"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZ���������������������������������"
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
USE (base_path+"FIRMS") ORDER TAG ALL_NAMES ALIAS F_9112 AGAIN

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Corr OF (menu_name) PROMPT "��玻." KEY Ctrl-A, "Enter"
ON SELECTION PAD Corr OF (menu_name) DO Sw_Mode WITH "Corr"

DEFINE PAD Add OF (menu_name) PROMPT "����←." KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"

DEFINE PAD Del OF (menu_name) PROMPT "����./��瘁�." KEY Ctrl-A, "Del"
ON SELECTION PAD Del OF (menu_name) DO Sw_Mode WITH "Del"

DEFINE PAD Pay OF (menu_name) PROMPT "���皀Θ" KEY Ctrl-A, "Tab"
ON SELECTION PAD Pay OF (menu_name) DO Sw_Mode WITH "Pay"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
*���つ������ ��Б��┘陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳����キ�陳朕
*�999 99999999999999999999999999999999999999999999999999 99.99.9999�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�   ��牀皖�� ��Б��┘ 999999999999999     �カヤ�� 999999999999    �
*� �むメ 釵.    99999999999999999999999999999999999999999999999999 �
*� �むメ ����.  99999999999999999999999999999999999999999999999999 �
*� ��� 999999999999   �オ�: [ ] �� 祠�������,  [ ] �襦�� ��оヨキ  � && NEW 22.12.1999
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
PRIVATE ln, wd
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = IIF(SET("CENTURY") = "ON", 65, 63)

DO D_Wins WITH ln, wd, "���甌� �爛く爬閧┤", 0, 4
@ 1, 2 SAY "��つ������ ��Б��┘陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳����キ�"
@ WROWS()-5,  5 SAY "��牀皖�� ��Б��┘                     �カヤ��"
@ WROWS()-4,  3 SAY "�むメ 釵."
@ WROWS()-3,  3 SAY "�むメ ����."
@ WROWS()-2,  3 SAY "��� 999999999999   �オ�: [ ] �� 祠�������   [ ] �襦�� ��оヨキ"   && NEW 22.12.1999

what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Del   DO Del_Firm
    ON KEY LABEL Ins   DO Sw_Mode WITH "Add"
    ON KEY LABEL Tab   DO Sw_Mode WITH "Pay"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS FIRM_CODE:H="", ;
                  L_NAME:H="",    ;
                  DATE_OFF:H="",  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Corr")
    ENDIF

  CASE what_do = "Corr"    && ��玻オ罔� 筥爼�

    IF BETWEEN(RECNO(), 1, RECCOUNT())
      DO Modi_Line
    ENDIF
    what_do = "List"

  CASE what_do = "Del"     && ����キ┘/〓瘁����←キ┘

    DO Del_Firm
    what_do = "List"

  CASE what_do = "Add"     && ����←キ┘ 筥爼�

    DO Modi_Line WITH .T.
    what_do = "List"

  CASE what_do = "Pay"     && ���竅皋�襯 皋�� ���皀Ε�

    DO Let_Pay WITH FIRM_CODE
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
@ WROWS()-5, 23 SAY B_NAME
@ WROWS()-5, 51 SAY PHONE
@ WROWS()-4, 16 SAY ADDRESS
@ WROWS()-3, 16 SAY ADDR_FACT
@ WROWS()-2,  7 SAY INN                   && NEW 22.12.1999
@ WROWS()-2, 29 SAY IIF(DEF_WCHK,"X"," ") && NEW 22.12.1999
@ WROWS()-2, 48 SAY IIF(LET_WCHK,"X"," ") && NEW 22.12.1999

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Modi_Line    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                   ����←キ┘/��玻オ罔� �爛く爬閧��.                    �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 01/12/1999 陳�
PROCEDURE Modi_Line
PARAMETERS new

*
*  �ムガキ�襯 甌痰�鎰�� か� ��※��罔�
*
IF .NOT. EMPTY(DATE_OFF) .AND. .NOT. new
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��爼� 磴��キ�, ����� 甃諱� ゥ ��玻オ皋牀��碎?!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� 敖 踳く爬閧┘ 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*� �            ��� 999   ��牀皖�� ��Б��┘ 9999999999999999            � �
*� �                                                                    � �
*� � ������ ��Б��┘ 99999999999999999999999999999999999999999999999999 � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�      �むメ 釵. 99999999999999999999999999999999999999999999999999      �
*�                                                                        �
*�     �むメ ����. 99999999999999999999999999999999999999999999999999     �
*�                                                                        �
*�      �カヤ�� 999999999999   ��� 9999999999   ��� ....:....!....:.      �
*�                                                                        �
*�             [ ] �痰� ��瘁��覃 �������    ���ム �������� ##             �
*�                                                                        �
*�            �オ�:   [ ] �� 祠�������    [ ] �襦�� ��оヨキ              �&& NEW 22.12.1999
*�                                                                        �
*�   ����� � �牀��� 999.9   ���瓱���讚�� 痺��� ����膈�� ����硅 99999999   �
*�                                                                        �
*� �礫�〓�. xxxxxxxxxXxxxxxxxxxXxxxx   ��. ＜紕. xxxxxxxxxXxxxxxxxxxXxxxx �
*�                                                                        �
*�                    ��瘁�� xxxxxxxxxXxxxxxxxxxXxxxx                     �
*�                                                                        �
*�                    < OK Ctrl-W > < �皖���碎瘴 Esc >                    �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰


PRIVATE ex, cd, n0, n1, ad, ad_f, c_i, ph, nl, sum, d_chk, l_chk   && NEW 22.12.1999
PRIVATE is_kkm, kkm_no, face1, face2, face3, tmpKPP
IF new
  cd  = "   "
  n0  = SPACE(16)
  n1  = SPACE(50)
  ad  = SPACE(50)
  ad_f  = SPACE(50)
  tmpKPP = SPACE(16)
  tmpKPP = KPP
  ph  = SPACE(12)
  nl  = 0
  sum = 0
  d_chk = .F.                                         && NEW 22.12.1999
  l_chk = .F.                                         && NEW 22.12.1999
  is_kkm = .F.
  kkm_no = "  "
  face1 = SPACE(24)
  face2 = SPACE(24)
  face3 = SPACE(24)
ELSE
  cd  = FIRM_CODE
  n0  = B_NAME
  n1  = L_NAME
  ad  = ADDRESS
  ad_f  = ADDR_FACT
  c_i = INN
  tmpKPP = KPP
  ph  = PHONE
  nl  = P_NALOG
  sum = M_SUM
  d_chk = DEF_WCHK                                    && NEW 22.12.1999
  l_chk = LET_WCHK                                    && NEW 22.12.1999
  is_kkm = .NOT. EMPTY(KKM)
  kkm_no = KKM
  face1 = BOSS
  face2 = ABAK
  face3 = KASSIR
ENDIF

ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 25, 76, IIF(new, "����←キ┘ ", "��玻オ罔� ")+"�爛く爬閧��" && NEW 22.12.1999
DO Sun_Bord WITH  2,  3,  6, 72, " 踳く爬閧┘ "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@  3, 16 SAY "���" GET cd WHEN new VALID Up_Case()
@  3, 26 SAY "��牀皖�� ��Б��┘" GET n0
@  5,  5 SAY "������ ��Б��┘" GET n1
@  7,  8 SAY "�むメ 釵." GET ad
@  9,  8 SAY "�むメ ����." GET ad_f
@ 11,  7 SAY "�カヤ��" GET ph
@ 11, 30 SAY "���" GET c_i
@ 11, 48 SAY "���" GET tmpKPP
@ 13, 15 GET is_kkm PICTURE "@*C �痰� ��瘁��覃 �������" VALID TstKKM()
@ 13, 44 SAY "���ム ��������" GET kkm_no PICTURE "##" WHEN is_kkm
@ 15, 14 SAY "�オ�:   " GET  d_chk PICTURE "@*C �� 祠�������" WHEN is_kkm    && NEW 22.12.1999
@ 15, 42 GET  l_chk PICTURE "@*C �襦�� ��оヨキ" WHEN is_kkm   && NEW 22.12.1999
@ 17,  5 SAY "����� � �牀���" GET nl PICTURE "@Z 999.9"
@ 17, 28 SAY "���瓱���讚�� 痺��� ����膈�� ����硅" GET sum PICTURE "@Z 99999999"
@ 19,  3 SAY "�礫�〓�." GET face1
@ 19, 39 SAY "��. ＜紕." GET face2
@ 21, 22 SAY "��瘁��" GET face3
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Tst_Blank()

IF ex = 1
  *
  * �矗���硅��ガ ゛���
  IF new
    APPEND BLANK
  ENDIF
  REPLACE FIRM_CODE WITH cd, ;
          B_NAME    WITH n0, ;
          L_NAME    WITH n1, ;
          ADDRESS   WITH ad, ;
          ADDR_FACT WITH ad_f, ;
          PHONE     WITH ph, ;
          INN       WITH c_i,;
          KPP       WITH tmpKPP,;
          P_NALOG   WITH nl, ;
          M_SUM     WITH sum,;
          KKM       WITH kkm_no, ;
          BOSS      WITH face1,;
          ABAK      WITH face2,;
          KASSIR    WITH face3,;
          DEF_WCHK  WITH d_chk,;
          LET_WCHK  WITH l_chk      && NEW 22.12.1999
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Up_Case      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                    �ムア�� ���� � ▲璢�┤ 爛��痰�.                     �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 01/12/1999 陳�
PROCEDURE Up_Case

cd = CHRTRAN(cd, shab1, shab2)
SHOW GET cd
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Tst_Blank    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                蹍▲爲� ��玻オ皚�痰� ������キ�� ゛����.                �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 01/12/1999 陳�
PROCEDURE Tst_Blank

PRIVATE rc_sav, mss
IF ex = 2
  RETURN .T.
ENDIF

IF new
  IF EMPTY(cd)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"��� �爛く爬閧�� �� ��Ε� °碎 �竅硅�!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    KEYBOARD REPLICATE("{Tab}",2)
    RETURN .F.
  ENDIF
  rc_sav = RECNO()
  SET ORDER TO TAG FIRM_CODE
  IF SEEK(cd)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"��� �爛く爬閧�� ぎ�Ε� °碎 祗┴��讚覓!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    SET ORDER TO TAG ALL_NAMES
    IF BETWEEN(rc_sav, 1, RECCOUNT())
      GO rc_sav
    ENDIF
    KEYBOARD REPLICATE("{Tab}",2)
    RETURN .F.
  ENDIF
  SET ORDER TO TAG ALL_NAMES
  IF BETWEEN(rc_sav, 1, RECCOUNT())
    GO rc_sav
  ENDIF
ENDIF
IF EMPTY(n0)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��牀皖�� ━� �� ��Ε� °碎 �竅硅�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  KEYBOARD REPLICATE("{Tab}",3)
  RETURN .F.
ENDIF
IF EMPTY(n1)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"������ ━� �� ��Ε� °碎 �竅硅�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  KEYBOARD REPLICATE("{Tab}",4)
  RETURN .F.
ENDIF
IF nl < 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"����� ������� �� °��モ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  KEYBOARD REPLICATE("{Tab}",4)
  RETURN .F.
ENDIF

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Del_Firm     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                  ����キ┘/〓瘁����←キ┘ �爛く爬閧��.                  �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 01/12/1999 陳�
PROCEDURE Del_Firm

IF BETWEEN(RECNO(), 1, RECCOUNT())
  IF EMPTY(DATE_OFF)
    REPLACE DATE_OFF WITH DATE()
  ELSE
    REPLACE DATE_OFF WITH {}
  ENDIF
ENDIF
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� TstKKM       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           ���� ����腮� ���.                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 14.09.2000 陳�
PROCEDURE TstKKM

IF is_kkm
  kkm_no = " 0"
  SHOW GET kkm_no
ELSE
  kkm_no = "  "
  l_chk  = .F.
  d_chk  = .F.
  SHOW GET kkm_no
  SHOW GET l_chk
  SHOW GET d_chk
ENDIF

RETURN .T.

