*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Corpuses     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                       ��玻オ罔� 甎�瓷� ��牆竅��                        �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 17.12.2005 様�
PROCEDURE Corpuses

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
USE (m.path_comm+"CORPUS") SHARED AGAIN ALIAS COR_5C17 ORDER TAG NAME

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Modify OF (menu_name) PROMPT "��玻オ皋牀��碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Modify OF (menu_name) DO Sw_Mode WITH "Modify"

DEFINE PAD Add OF (menu_name) PROMPT "����※碎" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd, pic, new_rec
m.ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
m.wd = FSIZE("NAME")+FSIZE("WEIGHT")+1
m.pic = REPLICATE("9", FSIZE("WEIGHT")-4)+".999"

DO D_Wins WITH m.ln, m.wd, "�┓� ��牆竅��", 1, 0
@ 1, 2 SAY "���瓷 Tab"
@ 2, 2 SAY "��Б��┘"
@ 2, WCOLS()-5 SAY "�メ"
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins   DO Sw_Mode WITH "Add"
    ON KEY LABEL Tab   DO Locator WITH 1, 2, 16

*
*    BROWSE - �キ�
*
    BROWSE FIELDS NAME:H="", ;
                  WEIGHT:H="":P=M.pic, ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Modify")
    ENDIF

  CASE m.what_do = "Modify"    && ��玻オ罔�

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO Corpus WITH CODE
    ENDIF
    m.what_do = "List"

  CASE m.what_do = "Add"       && ����←キ┘

    m.new_rec = 0
    DO Corpus WITH 0
    IF BETWEEN(m.new_rec, 1, RECCOUNT())
      GO m.new_rec
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
USE IN COR_5C17
SELECT (m.s_sav)

RETURN


*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Locator      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           ��瓱�〓�讚覃 ���瓷                           �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 17.12.2005 陳�
PROCEDURE Locator

PROCEDURE Locator
PARAMETERS l001, c001, ln

PRIVATE prf_flt, is_ins, c_flt, chr_type, c, c_c, c_t
PRIVATE prf_sav, c_sav, r_sav

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
prf_sav = prf_flt
c_sav   = c_flt

ACTIVATE WINDOW (m.win_name+"_M") SAME
DO WHILE .T.
  IF .NOT. prf_sav == prf_flt
    r_sav = RECNO()
    IF .NOT. SEEK(prf_flt)
      prf_flt = prf_sav
      c_flt   = c_sav
      IF BETWEEN(r_sav, 1, RECCOUNT())
        GO r_sav
      ENDIF
    ELSE
      prf_sav = prf_flt
      c_sav   = c_flt
      SHOW WINDOW (ALIAS()) REFRESH
    ENDIF
  ENDIF
  c_sav   = c_flt
  @ l001, c001 SAY PADR(prf_flt, ln, "�")
  @ l001, c001+c_flt SAY ""

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

@ l001, c001 SAY PADR("���瓷 Tab", ln)

ACTIVATE WINDOW (ALIAS())

RETURN

