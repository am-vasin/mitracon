*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Get_Corp     ��о�｀砒┴ ��むォ ��瓱�           04.04.2004        �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                             �襦�� ��牆竅�.                             �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Get_Corp
PARAMETERS prmCode, prmName

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
USE (path_comm+"CORPUS") ORDER TAG CODE ALIAS TMP_4404 AGAIN
IF prmCode > 0 .AND. SEEK(prmCode)
  SET ORDER TO TAG NAME
ELSE
  SET ORDER TO TAG NAME
  GO TOP
ENDIF

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�襦��碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Seek OF (menu_name) PROMPT "���瓷"
ON PAD Seek OF (menu_name) ACTIVATE POPUP Seek

  DEFINE POPUP Seek MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Seek PROMPT "�� ���皀�痰�" KEY Ctrl-A, "F7"
  ON SELECTION BAR 1 OF Seek DO Sw_Mode WITH "Seek_1"
  
  DEFINE BAR  2 OF Seek PROMPT "��≒��" KEY Ctrl-A, "Ctrl-F7"
  ON SELECTION BAR 2 OF Seek DO Sw_Mode WITH "Again"
  
DEFINE PAD Exit OF (menu_name) PROMPT "�皖���碎瘴" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd, tmpShab, r_sav, mss
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = FSIZE("NAME")

DO D_Wins WITH ln, wd, "��牆竅�", 1, 0
@ 1, 2 SAY "���瓷 "
@ 1, 8 SAY "Tab" COLOR (SCHEME(14,2))
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter   KEYBOARD CHR(23)
    ON KEY LABEL Tab     DO Locator WITH 1, 8, 10
    ON KEY LABEL F7      DO Sw_Mode WITH "Seek_1"
    ON KEY LABEL Ctrl+F7 DO Sw_Mode WITH "Again"
*
*    BROWSE - �キ�
*
    BROWSE FIELDS NAME:H="",       ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      IF READKEY() % 256 = 12
        what_do = "Exit"
      ENDIF
      IF READKEY() % 256 = 14
        what_do = "Choose"
      ENDIF
    ENDIF

  CASE what_do = "Seek_1"    && ���皀�痰�覃 ���瓷

    m.tmpShab = Get_Shab()
    IF .NOT. EMPTY(m.tmpShab)
      m.r_sav = RECNO()
      LOCATE FOR m.tmpShab $ NAME
      IF .NOT. FOUND()
        DIMENSION mss[3]
        mss[1] = ""
        mss[2] = CHR(0)+"��腑�� �� ���ぅ��..."
        mss[3] = ""
        DO Out_Mess WITH 7, "mss"
        IF BETWEEN(m.r_sav, 1, RECCOUNT())
          GO m.r_sav
        ENDIF
      ENDIF
    ENDIF
    what_do = "List"

  CASE what_do = "Again"    && ��≒�� ���瓷�

    
    IF .NOT. EMPTY(m.tmpShab)
      m.r_sav = RECNO()
      IF .NOT. EOF()
        SKIP
      ENDIF
      LOCATE REST FOR m.tmpShab $ NAME
      IF .NOT. FOUND()
        DIMENSION mss[3]
        mss[1] = ""
        mss[2] = CHR(0)+"��腑�� �� ���ぅ��..."
        mss[3] = ""
        DO Out_Mess WITH 7, "mss"
        IF BETWEEN(m.r_sav, 1, RECCOUNT())
          GO m.r_sav
        ENDIF
      ENDIF
    ENDIF
    what_do = "List"

  CASE what_do = "Choose"    && �襦��

    prmCode = CODE
    IF TYPE("prmName") = "C"
      prmName = NAME
    ENDIF
    EXIT

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
SELECT TMP_4404
USE

SELECT (s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Locator      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                       ���瓷 �� �����讚覓 ＜����                        �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 04.05.2001 陳�
PROCEDURE Locator
PARAMETERS l001, c001, ln

PRIVATE prf_flt, is_ins, c_flt, chr_type, c, c_c, c_t, v_sav, r_sav, c_sav

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
c_flt   = 0     && �オ竕�� ��Ж罔� �窶甌��

DO WHILE .T.
  ACTIVATE WINDOW (win_name+"_M") SAME
  @ l001, c001 SAY PADR(prf_flt, ln, "�")
  @ l001, c001+c_flt SAY ""
  v_sav   = prf_flt
  c_sav   = c_flt

  c_c = INKEY(0)                && ��� 瓱�〓��
  IF c_c >= 0
    c = CHR(c_c)                && �━〓�
    c_t = SYS(15, chr_type, c)  && �┓ 瓱�〓��
  ENDIF
  IF INLIST(c_c, 27, 9)
    EXIT
  ENDIF
  DO CASE
  CASE c_c < 0                  && �祗�罔����讚�� ���※��
  CASE c_t = "*"                && �ョ�皚覃 瓱�〓�
    prf_flt = LEFT(prf_flt, c_flt)+c+  ;
              SUBSTR(prf_flt, c_flt+IIF(is_ins,1,2))
    prf_flt = LEFT(prf_flt, ln)
    c_flt = c_flt+1
    c_flt = MIN(c_flt, ln)
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
    LOOP
  ENDCASE
  IF .NOT. v_sav == prf_flt
    r_sav   = RECNO()
    IF .NOT. SEEK(prf_flt)
      prf_flt = v_sav
       c_flt   = c_sav
       IF BETWEEN(r_sav, 1, RECCOUNT())
          GO r_sav
       ENDIF
    ENDIF
    SHOW WINDOW (ALIAS()) SAME REFRESH
  ENDIF

ENDDO

@ 1, 8 SAY PADR("Tab", ln) COLOR (SCHEME(14,2))

RETURN
