*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� GTD_Link     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                   ������キ┘ ��� か� 甎�瓷� ��Ж罔�.                   �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 26.02.2000 様�
PROCEDURE GTD_Link
PARAMETERS view

PRIVATE s_sav, cnt_hole, cnt_cur, tmpName, p, nameWd, cnt_found, r_sav, flt_val, swFull
PRIVATE period
PRIVATE p_num, p_country, p_cntC, p_line, p_badG, p_GTDName
period = 120       &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
swFull = .F.
p_GTDName = ""
flt_val = SPACE(25)
s_sav = SELECT()

SELECT GTD_LINK
cnt_cur = 0
cnt_found = 0
cnt_hole = RECCOUNT()
DO Wt_Mess WITH "   ��ぁ�� ���...   "

SCAN
  p_num     = ""
  p_country = ""
  p_line    = ""
  p_badG    = ""
  p_cntC    = ""
  DO Set_Gtd WITH GTD_LINK.CODE, GTD_LINK.PREFIX, GTD_LINK.NAME, GTD_LINK.PRODUCER, ;
             p_num, p_country, p_cntC, p_line, p_badG, p_GTDName
  IF .NOT. EMPTY(p_num)
    cnt_found = cnt_found+1
    REPLACE GTD_NAME WITH IIF(p_badG, "��. ���瘁", p_GTDName), ;
            GTD_NUM  WITH p_num,      ;
            GTD_LINE WITH p_line,     ;
            COUNTRY  WITH p_country,  ;
            COUNTRYC WITH p_cntC,     ;
            B_GTD    WITH p_badG
  ENDIF
  SELECT GTD_LINK
  cnt_cur = cnt_cur+1
  IF cnt_cur % 10 = 0
    p = ALLTRIM(STR(cnt_cur))+"/"+ALLTRIM(STR(cnt_hole))
    @ WROWS()-1, 0 SAY PADC(p, WCOLS())
  ENDIF
ENDSCAN

DO Wt_Mess

IF view
  PRIVATE mss0
  DIMENSION mss0(3)
  mss0(1) = ""
  mss0(2) = CHR(0)+"｡��｀���� "+ALLTRIM(STR(cnt_hole))+" ��Ж罔�, ���ぅ�� ��� か� "+ALLTRIM(STR(cnt_found))+"."
  mss0(3) = ""
  DO Out_Mess WITH 5, "mss0"
  DO V_GTD
ENDIF

SELECT (s_sav)

RETURN cnt_found

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� V_GTD        ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                   蹍甃�矗 爛с�赳��� �爬�錻�� ���.                    �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 26.02.2000 陳�
PROCEDURE V_GTD

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
SELECT 0
DO Use_Dummy
SELECT 0
DO Use_Link
ZAP
SELECT GTD_LINK

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Mark OF (menu_name) PROMPT "�皙モ��"
ON PAD Mark OF (menu_name) ACTIVATE POPUP Mark

  DEFINE POPUP Mark MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Mark PROMPT "��▲珥�牀��碎" KEY Ctrl-A, "Ins"
  ON SELECTION BAR 1 OF Mark DO Sw_Mode WITH "MarkOne"

  DEFINE BAR  2 OF Mark PROMPT "��▲珥�牀��碎 ≡�" KEY Ctrl-A, "*"
  ON SELECTION BAR 2 OF Mark DO Sw_Mode WITH "MarkAll"

  DEFINE BAR  3 OF Mark PROMPT "�痰���※碎 �痰�皰�" KEY Ctrl-A, "+"
  ON SELECTION BAR 3 OF Mark DO Sw_Mode WITH "Set"

  DEFINE BAR  4 OF Mark PROMPT "�腮痰�碎 �痰�皰�" KEY Ctrl-A, "-"
  ON SELECTION BAR 4 OF Mark DO Sw_Mode WITH "Clear"

DEFINE PAD GTD OF (menu_name) PROMPT "���" KEY Ctrl-A, "Enter"
ON SELECTION PAD GTD OF (menu_name) DO Sw_Mode WITH "GTD"

DEFINE PAD Filter OF (menu_name) PROMPT "�┼赳�" KEY Ctrl-A, "F7"
ON SELECTION PAD Filter OF (menu_name) DO Sw_Mode WITH "Filter"

DEFINE PAD Copy OF (menu_name) PROMPT "����牀��碎"
ON PAD Copy OF (menu_name) ACTIVATE POPUP Copy

  DEFINE POPUP Copy MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Copy PROMPT "������" KEY Ctrl-A, "Ctrl-C"
  ON SELECTION BAR 1 OF Copy DO Sw_Mode WITH "Copy"

  DEFINE BAR  2 OF Copy PROMPT "�痰�※碎" KEY Ctrl-A, "Ctrl-V"
  ON SELECTION BAR 2 OF Copy DO Sw_Mode WITH "PasteOne"

  DEFINE BAR  3 OF Copy PROMPT "�痰�※碎 �� 皀�竕ォ 痰牀��"
  ON SELECTION BAR 3 OF Copy DO Sw_Mode WITH "PasteNext"

  DEFINE BAR  4 OF Copy PROMPT "�痰�※碎 � �皙ョキ�襯"
  ON SELECTION BAR 4 OF Copy DO Sw_Mode WITH "PasteMark"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE g_sav, c_sav, cc_sav, l_sav
STORE 0 TO g_sav, c_sav, cc_sav, l_sav
PRIVATE tmpGTD, point1, point2, point3
PRIVATE ln, wd
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 51

DO D_Wins WITH ln, wd, "��� か� 甎�瓷� ��Ж罔�", 1, 2
what_do = "List"
@ WROWS()-4, 2 SAY "陳 ���ム ��� 陳陳陳陳陳陳陳 �矗��� "
@ WROWS()-2, 3 SAY "��� ���"
@ 1, 2 SAY "�┼赳�:"
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�XXXX XXXXXXXXXXXXXXXXXXXXXXXXX XXXXXXXX�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*敖� ���ム ��� 陳陳陳陳陳陳陳 �矗��� 陳陳�
*�XXXXXXXXXXXXXXXXXXXXXXXXXX XXXXXXXXXXXX�
*� ��� ��� XXXXXXXXXXXXXXXXXXXXXXXXXXXXX �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

GO TOP
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Home   GO TOP
    ON KEY LABEL End    GO BOTTOM
    ON KEY LABEL Enter  KEYBOARD CHR(23)
    ON KEY LABEL Ins    DO Sw_Mode WITH "MarkOne"
    ON KEY LABEL "*"    DO Sw_Mode WITH "MarkAll"
    ON KEY LABEL "+"    DO Sw_Mode WITH "Set"
    ON KEY LABEL "-"    DO Sw_Mode WITH "Clear"
    ON KEY LABEL F7     DO Sw_Mode WITH "Filter"
    ON KEY LABEL Ctrl+C DO Get_Copy
    ON KEY LABEL Ctrl+V DO P_One

*
*    BROWSE - �キ�
*
    BROWSE FIELDS M000 = IIF(GTD_LINK.MARK, "�", " "):1:H="", ;
                  GTD_LINK.PREFIX:4:H="", ;
                  GTD_LINK.NAME:35:H="",  ;
                  GTD_LINK.PRODUCER:8:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "GTD")
    ENDIF

  CASE what_do = "MarkOne"    && ��▲珥�牀��碎

    IF .NOT. BOF() .AND. .NOT. EOF()
      REPLACE GTD_LINK.MARK WITH .NOT. GTD_LINK.MARK
      SKIP
      IF EOF()
        GO BOTTOM
      ENDIF
    ENDIF
    what_do = "List"
      
  CASE what_do = "MarkAll" && ��▲珥�牀��碎 ≡�

    IF .NOT. BOF() .AND. .NOT. EOF()
      r_sav = RECNO()
      REPLACE ALL GTD_LINK.MARK WITH .NOT. GTD_LINK.MARK
      GO r_sav
    ENDIF
    what_do = "List"
      
  CASE what_do = "Set"     && �痰���※碎 �痰�皰�

    IF .NOT. BOF() .AND. .NOT. EOF()
      r_sav = RECNO()
      REPLACE REST GTD_LINK.MARK WITH .T.
      GO r_sav
    ENDIF
    what_do = "List"
      
  CASE what_do = "Clear"   && �腮痰�碎 �痰�皰�

    IF .NOT. BOF() .AND. .NOT. EOF()
      r_sav = RECNO()
      REPLACE REST GTD_LINK.MARK WITH .F.
      GO r_sav
    ENDIF
    what_do = "List"
      
  CASE what_do = "GTD"        && ���甌� ���

    IF .NOT. BOF() .AND. .NOT. EOF()
      tmpGTD = Get_GTD(GTD_LINK.PREFIX, GTD_LINK.NAME, GTD_LINK.PRODUCER)
      point1 = AT(CHR(13), tmpGTD)
      point2 = AT(CHR(13), tmpGTD, 2)
      point3 = AT(CHR(13), tmpGTD, 3)
      IF point1 # 0
        REPLACE GTD_LINK.GTD_NUM  WITH LEFT(tmpGTD,point1-1), ;
                GTD_LINK.COUNTRY  WITH SUBSTR(tmpGTD,point1+1, point2-point1-1), ;
                GTD_LINK.GTD_LINE WITH SUBSTR(tmpGTD,point2+1, point3-point2-1), ;
                GTD_LINK.COUNTRYC WITH SUBSTR(tmpGTD,point3+1)
      ENDIF
    ENDIF
    what_do = "List"

  CASE what_do = "Filter"        && �┼赳�

    DO Set_Fltr
    ACTIVATE WINDOW (win_name+"_M") SAME
    @ 1, 10 SAY flt_val
    what_do = "List"

  CASE what_do = "Copy"        && ����牀��碎

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO Get_Copy
    ENDIF
    what_do = "List"

  CASE what_do = "PasteOne"    && �痰�※碎 � 皀�竕窿 痰牀��

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO P_One
    ENDIF
    what_do = "List"

  CASE what_do = "PasteNext"    && �痰�※碎 �� 皀�竕ォ 痰牀��

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO P_Next
    ENDIF
    what_do = "List"

  CASE what_do = "PasteMark"    && �痰�※碎 � �皙ョキ�襯

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO P_Mark
    ENDIF
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

SELECT LINK
ZAP
SET RELATION TO
USE
SELECT GTD_LINK

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

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
@ WROWS()-3, 2 SAY GTD_LINK.GTD_NUM COLOR SCHEME 13
@ WROWS()-3, COL()+1 SAY LEFT(GTD_LINK.COUNTRY,12) COLOR SCHEME 13
@ WROWS()-2, 11 SAY GTD_LINK.GTD_NAME COLOR SCHEME 13

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Set_Fltr     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                       �痰���→�/�腮痰�� 筥�赳��.                       �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 05.04.2000 陳�
PROCEDURE Set_Fltr
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�      敖 ��゛�� 陳陳陳陳陳陳陳陳陳陳陳�      �
*�      �   ....:....!....:....!....:   �      �
*�      青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�      �
*�              [ ] ���讓� � ���               �
*�< OK Ctrl-W > < �皖���碎瘴 Esc > < �腮痰�碎 >�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex, tmpVal, tmpSw
ex = 1
tmpVal = flt_val
tmpSw = swFull
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 8, 49, "�┼赳�"
DO Sun_Bord WITH  2,  8,  4, 40, " ��゛�� "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 3, 12 GET tmpVal
@ 5, 16 GET tmpSw PICTURE "@*C ���讓� � ���"
IF EMPTY(flt_val)
  @ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "
ELSE
  @ WROWS()-2, 2 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc ;\ �腮痰�碎 "
ENDIF

READ CYCLE

IF ex # 2 .AND. EMPTY(tmpVal) .AND. .NOT. tmpSw
  ex = 3
ENDIF

DO CASE
CASE ex = 1
  *
  * �矗���硅��ガ ゛���
  tmpVal = ALLTRIM(tmpVal)
  swFull = tmpSw
  SELECT LINK
  SET RELATION TO
  ZAP
  SELECT GTD_LINK
  DO Wt_Mess WITH "���瓷..."
  SCAN
    IF .NOT. EMPTY(tmpVal)
      IF .NOT. tmpVal $ ALLTRIM(GTD_LINK.PREFIX)+ALLTRIM(GTD_LINK.NAME)+ALLTRIM(GTD_LINK.PRODUCER)
        LOOP
      ENDIF
    ENDIF
    IF swFull
      IF EMPTY(GTD_LINK.GTD_NUM)
        LOOP
      ENDIF
    ENDIF
    SELECT LINK
    APPEND BLANK
    REPLACE LNK WITH RECNO("GTD_LINK")
    SELECT GTD_LINK
  ENDSCAN
  DO Wt_Mess
  IF RECCOUNT("LINK") = 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"��腑�� �� 磴���瘡 ���皋. �腮��ガ 筥�赳�!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    SELECT GTD_LINK
    flt_val = SPACE(LEN(flt_val))
  ELSE
    SELECT LINK
    SET RELATION TO LNK INTO GTD_LINK
    flt_val = PADR(tmpVal, LEN(flt_val))
  ENDIF
CASE ex = 3
  *
  * �矗���硅��ガ ゛���
  swFull = tmpSw
  SELECT LINK
  ZAP
  SET RELATION TO
  SELECT GTD_LINK
  flt_val = SPACE(LEN(flt_val))
  swFull = .F.
ENDCASE
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Get_Copy     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                               ����牀��碎                               �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 05.04.2000 陳�
PROCEDURE Get_Copy

IF .NOT. BOF() .AND. .NOT. EOF()
  g_sav = GTD_LINK.GTD_NUM
  c_sav = GTD_LINK.COUNTRY
  cc_sav = GTD_LINK.COUNTRYC
  l_sav = GTD_LINK.GTD_LINE
ENDIF

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� P_One        ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                 �痰�※碎 ├ ��爼��� � 皀�竕竡 痰牀��.                  �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 05.04.2000 陳�
PROCEDURE P_One

IF .NOT. BOF() .AND. .NOT. EOF()
  IF TYPE("g_sav") = "C"
    REPLACE GTD_LINK.GTD_NUM  WITH g_sav, ;
            GTD_LINK.COUNTRY  WITH c_sav, ;
            GTD_LINK.COUNTRYC WITH cc_sav,;
            GTD_LINK.GTD_LINE WITH l_sav
    DO Draw_Foot
  ENDIF
ENDIF

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� P_Next       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�              �痰�※碎 ├ ��爼��� � 甄イ竡薑� 痰牀��.                   �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 05.04.2000 陳�
PROCEDURE P_Next

PRIVATE r_sav

IF .NOT. BOF() .AND. .NOT. EOF()
  IF TYPE("g_sav") = "C"
    r_sav = RECNO()
    REPLACE REST GTD_LINK.GTD_NUM  WITH g_sav, ;
                 GTD_LINK.COUNTRY  WITH c_sav, ;
                 GTD_LINK.COUNTRYC WITH cc_sav,;
                 GTD_LINK.GTD_LINE WITH l_sav
    GO r_sav
  ENDIF
ENDIF

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� P_Mark       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�             �痰�※碎 ├ ��爼��� � �皙ョキ�襯 痰牀��.                   �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 05.04.2000 陳�
PROCEDURE P_Mark

PRIVATE r_sav

IF .NOT. BOF() .AND. .NOT. EOF()
  IF TYPE("g_sav") = "C"
    r_sav = RECNO()
    REPLACE ALL FOR GTD_LINK.MARK  ;
                 GTD_LINK.GTD_NUM  WITH g_sav, ;
                 GTD_LINK.COUNTRY  WITH c_sav, ;
                 GTD_LINK.COUNTRYC WITH cc_sav,;
                 GTD_LINK.GTD_LINE WITH l_sav
    GO r_sav
  ENDIF
ENDIF

RETURN
