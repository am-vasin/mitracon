*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Modi_Pro     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                        ��玻オ罔� 甎�瓷� ÷キぎ�                        �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 24.12.2005 様�
PROCEDURE Modi_Pro

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
m.stat_type  = 3
m.menu_name  = PROGRAM()
m.last_mouse = 0
m.win_name   = PROGRAM()

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*
m.s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT 0
USE (m.base_path+"DEVELOP") SHARED AGAIN ALIAS DV_5C24 ORDER TAG B_NAME


*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Modify OF (menu_name) PROMPT "��玻." KEY Ctrl-A, "Tab"
ON SELECTION PAD Modify OF (menu_name) DO Sw_Mode WITH "Modify"

DEFINE PAD Add OF (menu_name) PROMPT "����※碎" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"

DEFINE PAD Delete OF (menu_name) PROMPT "�����碎" KEY Ctrl-A, "Del"
ON SELECTION PAD Delete OF (menu_name) DO Sw_Mode WITH "Delete"

DEFINE PAD Seek OF (menu_name) PROMPT "����. ���瓷" KEY Ctrl-A, "F7"
ON SELECTION PAD Seek OF (menu_name) DO Sw_Mode WITH "Seek"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd, seekTxt, seekMode, r_sav, mss, new_rec
m.seekTxt  = ""
m.seekMode = 1
m.ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
m.wd = 69

DO D_Wins WITH m.ln, m.wd, "��爼�-�牀├〓え皀��", 1, 1
@ 2, 2 SAY "��Б.陳陳������ ��Б��┘"
@ 1, 2 SAY "���瓷"
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    m.statys_type = 3
    ACTIVATE WINDOW (win_name+"_M") SAME
    @ 1, 8 SAY PADR("Enter", 8) COLOR (SCHEME(14,2))
    DO Prp_Nav_1

    ON KEY LABEL Tab   DO Sw_Mode WITH "Modify"
    ON KEY LABEL Ins   DO Sw_Mode WITH "Add"
    ON KEY LABEL Del   DO Sw_Mode WITH "Delete"
    ON KEY LABEL F7    DO Sw_Mode WITH "Seek"
    ON KEY LABEL Enter DO Locator WITH 1, 8, 8

*
*    BROWSE - �キ�
*
    BROWSE FIELDS F000 = LEFT(B_NAME, 8):H="", ;
                  L_NAME:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Modify"    && ��玻オ罔�

    IF .NOT. BOF() .AND. .NOT. EOF() .AND. CODE # 1
      DO Prod1 WITH CODE
    ENDIF
    m.what_do = "List"

  CASE m.what_do = "Add"       && ����←キ┘

    m.new_rec = 0
    DO Prod1
    IF BETWEEN(m.new_rec, 1, RECCOUNT())
      GO m.new_rec
    ENDIF
    m.what_do = "List"

  CASE m.what_do = "Delete"    && ����キ┘

    IF .NOT. BOF() .AND. .NOT. EOF() .AND. CODE # 1
      DIMENSION mss[3]
      mss[1] = ""
      mss[2] = CHR(0)+"�ォ痰※皀�讚� 磴��錺�? [ �� ] [ �モ ]"
      mss[3] = ""
      IF Out_Mess(5, "mss") = 1
        REPLACE DATE_OFF WITH DATE(), ;
                WHO_DEL  WITH m.user
      ENDIF
    ENDIF
    m.what_do = "List"

  CASE m.what_do = "Seek"      && ���皀�痰�覃 ���瓷

    IF Get_Shab()
      IF .NOT. EMPTY(m.seekTxt)
        m.r_sav = RECNO()
        IF m.seekMode = 1
          GO TOP
        ELSE
          IF .NOT. EOF()
            SKIP
          ENDIF
        ENDIF
        LOCATE REST FOR m.seekTxt $ B_NAME .OR. m.seekTxt $ L_NAME
        IF .NOT. FOUND()
          DIMENSION mss[3]
          mss[1] = ""
          mss[2] = CHR(0)+""
          mss[3] = ""
          DO Out_Mess WITH 7, "mas"
          IF BETWEEN(m.r_sav, 1, RECCOUNT())
            GO m.r_sav
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    m.what_do = "List"

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
SELECT (m.s_sav)

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
GO RECNO()		&& ��° �´�※碎 Л�腑�┘ ���� PERIOD (��������!)
@ WROWS()-2, 3     SAY "�牀� ��痰�→� "
@ WROWS()-2, COL() SAY PERIOD COLOR SCHEME 1

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Locator      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           ��瓱�〓�讚覃 ���瓷                           �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 24.12.2005 陳�
PROCEDURE Locator
PARAMETERS l001, c001, ln

PRIVATE prf_flt, is_ins, c_flt, chr_type, c, c_c, c_t
PRIVATE rc_near, flt_sav, p_sav

is_ins = INSMODE()   && �ウ━ ≡��→�/���キ�

*
*   "*" - ��る, 甌�癶モ痰≪鉗┘ �ョ�皚覓 瓱�〓���
*
chr_type = SPACE(31)+     REPLICATE("*",126-31 )+ ;
           SPACE(127-126)+REPLICATE("*",175-127)+ ;
           SPACE(223-175)+REPLICATE("*",241-223)+ ;
           SPACE(255-223)
*
*   "I" - ��る, 甌�癶モ痰≪鉗┘ 瓱�〓��� ↓竄爛��ォ ��※��罔�
*
chr_type = STUFF(chr_type,   1, 1, "I")  &&  Home
chr_type = STUFF(chr_type,   4, 1, "I")  &&  �矗カ�� ����〓
chr_type = STUFF(chr_type,   6, 1, "I")  &&  End
chr_type = STUFF(chr_type,   7, 1, "I")  &&  Del
chr_type = STUFF(chr_type,  13, 1, "I")  &&  Enter
chr_type = STUFF(chr_type,  19, 1, "I")  &&  �矗カ�� ←ア�
chr_type = STUFF(chr_type,  22, 1, "I")  &&  Ins
chr_type = STUFF(chr_type, 127, 1, "I")  &&  BackSpace

*
*   "E" - ��る, 甌�癶モ痰≪鉗┘ 瓱�〓��� ↓ヨ�ォ ��※��罔� ( �� ��Д )
*

chr_type = STUFF(chr_type,  3, 1, "E")  &&  PgDn
chr_type = STUFF(chr_type,  5, 1, "E")  &&  �矗カ�� □ム�
chr_type = STUFF(chr_type, 18, 1, "E")  &&  PgUp
chr_type = STUFF(chr_type, 24, 1, "E")  &&  �矗カ�� ↓├

prf_flt = ""    && 踳筥��
flt_sav = prf_flt
c_flt   = 0     && �オ竕�� ��Ж罔� �窶甌��

DO WHILE .T.
  IF .NOT. flt_sav == prf_flt
    rc_near = RECNO()
    IF .NOT. SEEK(prf_flt)
      prf_flt = flt_sav
      c_flt   = p_sav
      IF BETWEEN(rc_near, 1, RECCOUNT())
        GO rc_near
      ENDIF
    ENDIF
    SHOW WINDOW (ALIAS()) REFRESH
  ENDIF
  @ l001, c001 SAY PADR(prf_flt, ln, "�")
  @ l001, c001+c_flt SAY ""
  flt_sav = prf_flt
  p_sav   = c_flt
  c_c = INKEY(0)                && ��� 瓱�〓��
  IF c_c >= 0
    c = CHR(c_c)                && �━〓�
    c_t = SYS(15, chr_type, c)  && �┓ 瓱�〓��
  ENDIF
  DO CASE
  CASE c_c < 0                  && �祗�罔����讚�� ���※��
  CASE c_t = "*"                && �ョ�皚覃 瓱�〓�
    prf_flt = LEFT(prf_flt, c_flt)+c+  ;
              SUBSTR(prf_flt, c_flt+IIF(is_ins,1,2))
    prf_flt = LEFT(prf_flt, ln)
    c_flt = c_flt+1
    c_flt = MIN(c_flt, ln)
    LOOP
  CASE c_t = "E"                && ��※��罔� �� ��Д
  CASE c_t = "I"                && ��竄爛��闖 ��※��罔�
    DO CASE
    CASE c_c = 7                && Del
      prf_flt = LEFT(prf_flt, c_flt)+SUBSTR(prf_flt, c_flt+2)
    CASE c_c = 127              && BackSpace
      prf_flt = LEFT(prf_flt, c_flt-1)+SUBSTR(prf_flt, c_flt+1)
      c_flt = MAX(c_flt-1,0)
    CASE c_c = 19               && �矗カ�� ←ア�
      c_flt = MAX(c_flt-1,0)
    CASE c_c = 4                && �矗カ�� ����〓
      c_flt = MIN(c_flt+1,LEN(prf_flt))
    CASE c_c = 22               && Ins
      is_ins = .NOT. is_ins
      = INSMODE(is_ins)
    CASE c_c = 13
      EXIT
    ENDCASE
  OTHERWISE
    EXIT
  ENDCASE
ENDDO

ACTIVATE WINDOW (win_name+"_M") SAME
@ 1, 8 SAY PADR("Enter", 8) COLOR (SCHEME(14,2))
ACTIVATE WINDOW (ALIAS()) SAME

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Get_Shab     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                        �〓� ��゛��� か� ���瓷�                         �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 24.12.2005 陳�
PROCEDURE Get_Shab
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
m.stat_type  = 1
m.what_do    = ""
m.menu_name  = ""
m.last_mouse = 0
m.win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ��ぅ爨�皀�讚�� ��痰� �牀������:
*

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� 敖 ��゛�� か� ���瓷� 陳陳陳陳陳陳� �
*� �                                � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�( ) �瓷�碎 甅�����  ( ) �瓷�碎 ���ゥ�
*�  < OK Ctrl-W > < �皖���碎瘴 Esc >  �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, ret, tmpTxt, tmpMode
m.ex = 1
m.ret = .F.
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 8, 40, "���皀�痰�覃 ���瓷"
DO Sun_Bord WITH  2,  3,  4, 36, " ��゛�� か� ���瓷� "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
m.tmpTxt  = m.seekTxt
m.tmpMode = m.seekMode
@ 3, 4 EDIT m.tmpTxt SIZE 1, 32 COLOR (","+SCHEME(1,1))
@ 5, 2 GET  m.tmpMode PICTURE "@*RH �瓷�碎 甅�����; �瓷�碎 ���ゥ"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

IF m.ex = 1
  *
  * �矗���硅��ガ ゛���
  m.seekTxt  = m.tmpTxt
  m.seekMode = m.tmpMode
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)

RETURN m.ex = 1
