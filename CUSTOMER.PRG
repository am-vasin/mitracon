*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Customer     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                             �襦�� ��┘���.                             �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 04.05.2001 様�
PROCEDURE Customer
PARAMETERS parmCode, parmName, parmFlags, A000, B000, C000
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
PRIVATE tmpFlags
IF TYPE("parmFlags") = "C"
  tmpFlags = parmFlags
ELSE
  tmpFlags = ""
ENDIF

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
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS TMP_1504 AGAIN
IF parmCode > 0 .AND. SEEK(parmCode)
  SET ORDER TO TAG CUS_NAME
ELSE
  SET ORDER TO TAG CUS_NAME
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
  
IF "I" $ UPPER(tmpFlags)

  DEFINE PAD Info OF (menu_name) PROMPT "��筮爼�罔�"
  ON PAD Info OF (menu_name) ACTIVATE POPUP Info

    DEFINE POPUP Info MARGIN RELATIVE SHADOW COLOR SCHEME 4

    DEFINE BAR  1 OF Info PROMPT "�痰�爬�" KEY Ctrl-A, "F1"
    ON SELECTION BAR 1 OF Info DO Sw_Mode WITH "History"

    DEFINE BAR  2 OF Info PROMPT "�ク�Ж�" KEY Ctrl-A, "F2"
    ON SELECTION BAR 2 OF Info DO Sw_Mode WITH "Deposit"

    DEFINE BAR  3 OF Info PROMPT "��爲モ┃����� ┃筮爼�罔�" KEY Ctrl-A, "Shift-F1"
    ON SELECTION BAR 3 OF Info DO Sw_Mode WITH "Market"

    DEFINE BAR  4 OF Info PROMPT "�矗─竄� か� 瘍モ�-���矣琺" KEY Ctrl-A, "Shift-F2"
    ON SELECTION BAR 4 OF Info DO Sw_Mode WITH "Cus_Bil"

    DEFINE BAR  5 OF Info PROMPT "��瘡�" KEY Ctrl-A, "Ctrl-F1"
    ON SELECTION BAR 5 OF Info DO Sw_Mode WITH "Dossier"

    DEFINE BAR  6 OF Info PROMPT "��甎�珥�襯 ����襯" KEY Ctrl-A, "Ctrl-F2"
    ON SELECTION BAR 6 OF Info DO Sw_Mode WITH "Pasport"

ENDIF

IF "S" $ UPPER(tmpFlags)
  DEFINE PAD Add OF (menu_name) PROMPT "����※碎" KEY Ctrl-A, "Ins"
  ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"
ENDIF

DEFINE PAD Exit OF (menu_name) PROMPT "�皖���碎瘴" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = FSIZE("CUS_NAME")

DO D_Wins WITH ln, wd, IIF("S" $ UPPER(tmpFlags),parmName,"��┘�硅"), 1, 0
@ 1, 2 SAY "���瓷 "
@ 1, 8 SAY "Tab" COLOR (SCHEME(14,2))
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F7       DO Sw_Mode WITH "Seek_1"
    ON KEY LABEL Ctrl+F7  DO Sw_Mode WITH "Again"
    IF "I" $ UPPER(tmpFlags)
      ON KEY LABEL Shift+F1 DO Sw_Mode WITH "Market"
      ON KEY LABEL F2       DO Sw_Mode WITH "Deposit"
      ON KEY LABEL Shift+F2 DO Sw_Mode WITH "Cus_Bil"
      ON KEY LABEL Ctrl+F2  DO Sw_Mode WITH "Pasport"
      ON KEY LABEL F1       DO Sw_Mode WITH "History"
      ON KEY LABEL Ctrl+F1  DO Sw_Mode WITH "Dossier"
    ENDIF
    IF "S" $ UPPER(tmpFlags)
      ON KEY LABEL Ins DO Sw_Mode WITH "Add"
    ENDIF    
    ON KEY LABEL Tab      DO Locator WITH 1, 8, 30
*
*    BROWSE - �キ�
*
    BROWSE FIELDS CUS_NAME:H="",       ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
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

  CASE what_do = "Choose"    && �襦��

    parmCode = CUS_CODE
    IF TYPE("parmName") = "C"
      parmName = CUS_NAME
    ENDIF
    EXIT

  CASE what_do = "Seek_1"    && ���皀�痰�覃 ���瓷

    IF Seek_1()
      what_do = "Again"
    ELSE
      what_do = "List"
    ENDIF

  CASE what_do = "Again"      && ��≒��

    = Ch_Found()
    what_do = "List"

  CASE what_do = "Market"     && ��爲モ┃����� ┃筮爼�罔�
   
  IF "I" $ tmpFlags
    DO Cli_Info WITH "", "I" $ tmpFlags, CUS_CODE
    what_do = "List"
  ENDIF

  CASE what_do = "History"    && �痰�爬�

    DO Acc_Hist WITH CUS_CODE
    what_do = "List"

  CASE what_do = "Deposit"    && �ク�Ж�

    DO V_Note WITH "I" $ tmpFlags, CUS_CODE
    what_do = "List"

  CASE what_do = "Dossier"     && ��瘡�

    DO Dossier WITH CUS_CODE, IIF("I" $ tmpFlags, "C", "")
    what_do = "List"

  CASE what_do = "Cus_Bil"     && �矗─竄� か� �-�

    DO Cus_Bil WITH CUS_CODE, CUS_NAME, "i" $ tmpFlags
    what_do = "List"

  CASE what_do = "Pasport"     && ��甎�珥�襯 ����襯

    DO Pasport WITH CUS_CODE, .F., .F., .F., .F., .F., .F., .F., ;
                    IIF("i" $ tmpFlags, "", "C")
    what_do = "List"

  CASE what_do = "Add"         && ���覃 ��┘��
  
    DO Add_Cus WITH parmName, parmCode
    SET ORDER TO TAG CUS_CODE
    IF SEEK(parmCode)
      = RECNO()
    ENDIF
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
SELECT TMP_1504
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

*ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� Гメ�, 爬痺ガ...
*

*ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Seek_1       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           ���皀�痰�覃 ���瓷                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 04.05.2001 陳�
PROCEDURE Seek_1

PRIVATE v_shab, r_sav, s_sav

v_shab = Get_Shab()
v_shab = SYS(15, lwr, v_shab)

IF EMPTY(v_shab)
  RETURN .F.
ENDIF

r_sav = RECNO()
s_sav = SELECT()
SELECT 0
DELETE FILE (tmpo_path+"LINK.DBF")
CREATE DBF (tmpo_path+"LINK") ( LNK N(10) )
USE (tmpo_path+"LINK")
SELECT TMP_1504
DO Wt_Mess WITH "�薀�..."
SCAN FOR v_shab $ SYS(15, lwr, CUS_NAME)
  SELECT LINK
  APPEND BLANK
  REPLACE LNK WITH RECNO("TMP_1504")
  SELECT TMP_1504
ENDSCAN

SELECT LINK
USE

SELECT (s_sav)
IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF

DO Wt_Mess

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Ch_Found     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                          �襦�� ├ ���ぅ�����.                          �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 04.05.2001 陳�
PROCEDURE Ch_Found

PRIVATE mss, s_sav, r_sav, ret_v

ret_v = .F.

IF .NOT. File_O(tmpo_path+"LINK.DBF")
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��腑�� �� ���ぅ��..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

r_sav = RECNO()
s_sav = SELECT()

SELECT 0
USE (tmpo_path+"LINK")

IF RECCOUNT() = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��腑�� �� ���ぅ��..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  RETURN .F.
ENDIF

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
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*

SELECT 0
DO Use_Dummy

SELECT TMP_1504
SET ORDER TO

SELECT LINK
SET RELATION TO LNK INTO TMP_1504
GO TOP
*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�襦��碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "�皖���碎瘴" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
ln = MIN(ln, RECCOUNT())
wd = FSIZE("CUS_NAME", "TMP_1504")

DO D_Wins WITH ln, wd, "�ェ祀赳�� ���皀�痰���� ���瓷�", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
*
*    BROWSE - �キ�
*
    BROWSE FIELDS TMP_1504.CUS_NAME:H="",       ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && �襦��

    ret_v = .T.
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
SELECT LINK
SET RELATION TO
USE

SELECT TMP_1504
SET ORDER TO TAG CUS_NAME
IF .NOT. ret_v
  IF BETWEEN(r_sav, 1, RECCOUNT())
    GO r_sav
  ENDIF
ENDIF

SELECT (s_sav)

RETURN ret_v

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
    IF .NOT. SEEK(SYS(15, lwr, prf_flt))
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

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                        蹍������ 甌Г���� ��┘���.                    �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Add_Cus
PARAMETER cc_name, cc_code
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
PRIVATE c_sav
*
*   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()
c_sav = SELECT()
*------------------------------------------------------------------------
*    ��ぅ爨�皀�讚�� ��痰� �牀������:
*

PRIVATE ex, nm_w, sw_i, sw_a, cd, rc_sav, typ_cli, tmpINN, tmpKPP
                     &&   ｡褊←錺� � ������錺� ���� ゛����
ex   = 1             &&
m.tmpINN = ""
m.tmpKPP = ""
DO Get_INN WITH 0, m.tmpINN, m.tmpKPP

IF EMPTY(cc_name)
  nm_w = SPACE(50)
ELSE
  nm_w = cc_name
ENDIF  
sw_i = .F.
sw_a = .F.
SELECT 0
USE (A000) ALIAS ACC_C AGAIN
IF EMPTY(MARK1)
  typ_cli = "0"
ELSE
  typ_cli = MARK1
ENDIF
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE SHARED ALIAS ACC_2832 AGAIN
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
*
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� 敖 ��� ��┘��� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*� � ....:....!....:....!....:....!....:....!....:....! � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�        ��� ....:....!..    ��� ....:....!....:.        �
*�      [ ] ��珥�腦� ��┘���    [ ] �矗─竄� ��┘���      �
*�                                                        �
*�            < OK Ctrl-W > < �皖���碎瘴 Esc >            �
*�                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

DO Prp_Nav_2
DO D_Win_N WITH 11, 60, "��┘��"
DO Sun_Bord WITH 2, 3, 4, WCOLS()-4
@ 2, 5 SAY " ��� ��┘��� "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 3,  5 GET nm_w
@ 5, 10 SAY "���" GET m.tmpINN
@ 5, 30 SAY "���" GET m.tmpKPP
@ 6,  8 GET sw_i PICTURE "@*C ��珥�腦� ��┘���"
@ 6, 32 GET sw_a PICTURE "@*C �矗─竄� ��┘���"
@ 8, 14 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Tst_Name()

IF ex = 1
  IF sys_char = "A" .OR. sale_mode = "Mitin"   && �甅�↓�� �����お�: ぎ��←錺� ����瘡

    cd = Doc_Num("CUS_CODE  ", "", arm, DATE())
    SELECT ACC_2832
    APPEND BLANK
    REPLACE SUBSYS   WITH sys_char,;
            CUS_CODE WITH cd,      ;
            CUS_NAME WITH nm_w,    ;
            DATE_ON  WITH DATE(),  ;
            WHO      WITH user,    ;
            WHERE    WITH arm,     ;
            MARK1    WITH typ_cli
    rc_sav = RECNO()
    DO Put_INN WITH CUS_CODE, m.tmpINN, m.tmpKPP

  ELSE     && �� �甅�↓�� �����お�: �甎��讌礇� ����皰→�

    rc_sav = RECNO()
    SET ORDER TO TAG EMP_NAME
    GO TOP
    IF EOF() .OR. BOF()
      PRIVATE mss
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"�� �瘍ム���� ≡� ����皰→�!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      sw_i = .F.
      sw_a = .F.
    ELSE
      REPLACE SUBSYS   WITH sys_char, ;
              CUS_NAME WITH nm_w,     ;
              DATE_ON  WITH DATE(),   ;
              WHO      WITH user,     ;
              WHERE    WITH arm,      ;
              MARK1    WITH typ_cli
      rc_sav = RECNO()
    ENDIF
    SET ORDER TO TAG CUS_NAME
    IF rc_sav > 0 .AND. rc_sav <= RECCOUNT()
      GO rc_sav
    ENDIF
  ENDIF
  cc_code = CUS_CODE
  IF sw_i
    DO Cli_Inf WITH "", CUS_CODE
  ENDIF
  IF sw_a
    DO Cus_Bilm WITH ACC_2832.CUS_CODE, ACC_2832.CUS_NAME
  ENDIF
ENDIF
*--------------------------------------------------------------------------
SELECT ACC_2832
USE
SELECT ACC_C
USE
POP KEY
RELEASE WINDOW (win_name)
SELECT(c_sav)

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                    蹍▲爲� ━キ� ��┘���.                            �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Tst_Name

IF ex = 2 .OR. READKEY() % 256 = 12
  RETURN .T.
ENDIF
IF EMPTY(nm_w)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"鉑皰� ━� ��┘���? ��� �� ���ぅ�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN Tst_INN(0, m.tmpINN)

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                  蹍������ 甌Г���� �矗─竄�� ��┘���.                �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Cus_Bilm
PARAMETERS cd, nm
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
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�      ���祚�皀�� ....:....!....:....!....:....!....:....!....:....!       �
*�                                                                          �
*�    �むメ ....:....!....:....!....:....!....:....!....:....!....:....!    �
*�                                                                          �
*��カ. ....:....!....:. �/� ....:....!....:....! �/� N ....:....!....:....! �
*�                                                                          �
*�             � ����� ....:....!....:....!....:....!....:....!             �
*�                                                                          �
*�          ��� ....:....!....:....!        ��牀� ....:....!....:.          �
*�                                                                          �
*�       ��� ....:....!..   ��� �� ����� ....:   ��� �� ���� ....:...       �
*楽� 踸�ョ��┘ 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕�
*崖....:....!....:....!....:....!....:....!....:....!....:....!....:....!..崖
*崖                                                                        崖
*崖                   < OK Ctrl-W > < �皖���碎瘴 Esc >                     崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳抒
*� �皰 甌Г��                                 ���� 甌Г����                 �
*� �皰 ��玻オ皋牀���                          ���� ��玻オ罔�                �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
PRIVATE f01, f02, f03, f04, f05, f06, f07, f08, f09, f10, f11, f12, ex
PRIVATE sel, us, new
PRIVATE c01, c02, c03, c04, c11, c12

sel = SELECT()
**************************
SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS P_2833 AGAIN

c01 = ""
c02 = ""
c03 = {}
c04 = {}
c11 = " "
c12 = " "

SELECT 0
USE (B000) ALIAS BILL_C AGAIN
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS ACC_2833 AGAIN
SET RELATION TO WHO_CORR INTO P_2833
SET RELATION TO WHO INTO P_2833

IF SEEK (cd)
  c01 = ACC_2833.WHO
  c02 = ACC_2833.WHO_CORR
  c03 = DATE_ON
  c04 = DATE_CORR
ENDIF
USE

SELECT P_2833
IF SEEK (c01)
  c11 = ALLTRIM(P_2833.FAMILY)+" "+LEFT(P_2833.NAME,1)+". "+ ;
              LEFT(P_2833.S_NAME,1)+"."
ENDIF
IF SEEK (c02)              
  c12 = ALLTRIM(P_2833.FAMILY)+" "+LEFT(P_2833.NAME,1)+". "+ ;
              LEFT(P_2833.S_NAME,1)+"."
ENDIF              
USE

********************************
IF USED("CUS_BIL")
  us = .T.
  SELECT CUS_BIL
ELSE
  us = .F.
  SELECT 0
  USE CUS_BIL ORDER TAG CUS_CODE
ENDIF

SELECT BILL_C
IF EMPTY(ADDRESS)
 f02 = SPACE(FSIZE("ADDRESS"))
ELSE
  f02 = ADDRESS
ENDIF

IF EMPTY(INN)
  f09 = SPACE(FSIZE("INN"))
ELSE
  f09 = INN
ENDIF
ex   = 1
IF EMPTY(nm)
  f01 = PADR(nm, FSIZE("CUS_NAME"))
ELSE
  f01 = nm  
ENDIF
f03 = SPACE(FSIZE("PHONE"))
f04 = SPACE(FSIZE("ACC_BILL"))
f05 = SPACE(FSIZE("BANK"))
f06 = SPACE(FSIZE("CORR_BILL"))
f07 = SPACE(FSIZE("BIK"))
f08 = SPACE(FSIZE("CITY"))
f10 = SPACE(FSIZE("OKOHX"))
f11 = SPACE(FSIZE("OKPO"))
f12 = SPACE(FSIZE("NOTE"))
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 22, 78, "�矗─竄� ��┘���"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@  2,  8 SAY "���祚�皀��" GET f01
@  4,  6 SAY "�むメ" GET f02
@  6,  2 SAY "�カ." GET f03
@  6, 24 SAY "�/�" GET f06
@  6, 49 SAY "�/�" GET f04
@  8, 15 SAY "� �����" GET f05
@ 10, 12 SAY "���" GET f07
@ 10, 44 SAY "��牀�" GET f08
@ 12,  9 SAY "���" GET f09
@ 12, 28 SAY "��� �� �����" GET f10
@ 12, 49 SAY "��� �� ����" GET f11
DO Sun_Bord WITH 13, 2, 15, 75
@ 13, 4 SAY " 踸�ョ��┘ "
@ 14,  3 GET f12
DO Sun_Bord WITH 16, 2, 19, 75
@ WROWS()-5,  3 SAY "�皰 甌Г��         "+c11
@ WROWS()-4,  3 SAY "�皰 ��玻オ皋牀���  "+c12
@ WROWS()-5, 47 SAY "���� 甌Г����   "+DTOC(c03)
@ WROWS()-4, 47 SAY "���� ��玻オ罔�  "+DTOC(c04)
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "
READ CYCLE

IF ex = 1
  APPEND BLANK
  REPLACE CUS_CODE  WITH cd,  ;
          CUS_NAME  WITH f01, ;
          ADDRESS   WITH f02, ;
          PHONE     WITH f03, ;
          ACC_BILL  WITH f04, ;
          BANK      WITH f05, ;
          CORR_BILL WITH f06, ;
          BIK       WITH f07, ;
          CITY      WITH f08, ;
          INN       WITH f09, ;
          OKOHX     WITH f10, ;
          OKPO      WITH f11, ;
          NOTE      WITH f12, ;
          DATE_CORR  WITH DATE(),  ;
          WHO_CORR   WITH user,    ;
          WHERE_CORR WITH arm
ENDIF
*------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
IF .NOT. us
  USE
ENDIF
IF USED("CUS_BIL")
  SELECT CUS_BIL
  APPEND FROM (B000)
  USE
ENDIF
SELECT (sel)

RETURN ex = 1
*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                    蹍������ 甌Г���� ┃筮爼�罔� � ��┘�皀.           �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Cli_Inf
PARAMETERS ext_menu, parmCode

PRIVATE ln, wd, sel_sav, mss, ex, crsr, manager_n, k_sw, k_code, k_name
PRIVATE tmpName

sel_sav = SELECT()
manager_n = ""

IF EMPTY(parmCode)
  RETURN
  SELECT (sel_sav)
ENDIF
SELECT 0

USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS ACC_2901 AGAIN
IF .NOT. SEEK(parmCode)
  USE
  SELECT (sel_sav)
  RETURN
ENDIF

tmpName = CUS_NAME
IF .NOT. EMPTY(ACC_2901.WHO_NEW)
  SELECT 0
  USE (base_path+"PERSONS") ORDER TAG CODE ALIAS PRS_2901
  SEEK ACC_2901.WHO_NEW
  manager_n = ALLTRIM(FAMILY)+" "+LEFT(NAME,1)+" "+LEFT(S_NAME,1)
  USE
ENDIF
manager_n = PADR(manager_n, 28)
IF TYPE("ext_menu") = "C"
  IF .NOT. EMPTY(ext_menu)
    DEACTIVATE MENU (ext_menu)
  ENDIF
ENDIF

DEFINE WINDOW H_Wind FROM 0, 0 TO 0, WCOLS("") NONE COLOR SCHEME 13
SHOW WINDOW H_Wind
SELECT 0
USE (C000) ALIAS INF_C AGAIN
SELECT 0
USE (base_path+"CLI_KIND") ORDER TAG CODE ALIAS KIND_2901 AGAIN
SELECT 0
USE (base_path+"CLI_INFO") ORDER TAG CUS_CODE ALIAS INFO_2901 AGAIN
k_sw   = .F.
k_code = SPACE(FSIZE("KIND_CODE", "KIND_2901"))
k_name = SPACE(FSIZE("KIND_NAME", "KIND_2901"))
 
PUSH KEY CLEAR
SCATTER MEMVAR BLANK

SELECT INF_C
m.address = ADDRESS
m.fax = FAX
ln = 24
wd = 78

DO D_W WITH "Cli_Info", ln, wd, "���祚�皀��: "+ALLTRIM(tmpName), 0
@ 0, 0 SAY ""
*       ���祚�皀��: < ��� ��┘��� >
? "敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�"
? "�    敖陳� �矗��� 陳陳陳陳陳 �ィ┏� 陳陳陳陳陳陳陳陳陳 ��牀� 陳陳陳陳朕     �"
? "�    �  ....:....1....:.  ....:....1....:....1....  ....:....1....:.  �     �"
? "�    青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰     �"
? "楽陳 �むメ 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳審"
? "崖....:.  ....:....1....:....1....:....1....:....1....:....1....:....1.... 崖"
? "崖              �カヤ�� ....:....1....       ���� ....:....1               崖"
? "崖               ��. ��艪� ....:....1....:....1....:....1..                崖"
? "垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰�"
? "� �.�.�. ....:....1....:....1....:....1..   ���Ν�痰� ....:....1....:....1  �"
? "楽�[ ] �筌�� ぅ閧. 陳陳笥陳 ��ユ���├�罔� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕�"
? "崖                     崖[ ] �磴┏ & �祀赳━イ��  [ ] �イ. �｀珮ぎ���┘    崖"
? "崖                     崖[ ] �┐ギ & �謔. 皀紲��  [ ] �≒�-皀絖┴�         崖"
? "崖                     崖[ ] �爛め癶� 瓣錻�       [ ] �カオ���祗┴�罔�     崖"
? "崖                     崖[ ] �К. 皀絖┴�         [ ] ｡�珮�.��え�▲�.� TV 崖"
? "崖                     崖[ ] �痰�膈┴� �������    [ ] ��※���.�｀珮ぎ���┘ 崖"
? "崖                     崖[ ] ����跪皀爿�� 皀絖┴�     ....:....1....:.     崖"
? "崖                     崖[ ] �縲���襯 瓱痰ガ�                              崖"
? "垣陳陳陳陳陳陳陳陳陳陳潰青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰�"
? "�踸�ョ��┘ ....:....1....:....1....:....1....:....1....:....1....:....1....�"
? "�                   �キイΕ� ....:....!....:....!....:...                   �"
? "�                     < OK Ctrl-W > < �皖���碎瘴 Esc >                      �"
? "青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�"

ex = 1

@  3,  8 GET m.country
@  3, 26 GET m.region
@  3, 52 GET m.city

@  6,  2 GET m.index
@  6, 10 GET m.address PICTURE "@S64"
@  7, 24 GET m.phone
@  7, 50 GET m.fax
@  8, 27 GET m.eml

@ 10,  9 GET m.contact
@ 10, 54 GET m.job

@ 11, 3  SAY " �筌�� ぅ閧. "
@ 12, 3 GET m.production PICTURE "@*C 蹍├〓め癶�"
@ 13, 3 GET m.work       PICTURE "@*C ��о�｀皖�"
@ 14, 3 GET m.sale       PICTURE "@*C 蹍����"
@ 15, 3 GET m.repair     PICTURE "@*C �ガ���"
@ 16, 7 GET m.f_other

@ 12, 25 GET m.audio      PICTURE "@*C �磴┏ & �祀赳━イ��"
@ 13, 25 GET m.video      PICTURE "@*C �┐ギ & �謔���� 皀�."
@ 14, 25 GET m.connect    PICTURE "@*C �爛め癶� 瓣錻�"
@ 15, 25 GET m.meastech   PICTURE "@*C �К. 皀絖┴�"
@ 16, 25 GET m.power      PICTURE "@*C �痰�膈┴� �������"
@ 17, 25 GET m.computer   PICTURE "@*C ����跪皀爿�� 皀絖┴�"
@ 18, 25 GET m.quardsys   PICTURE "@*C �縲���襯 瓱痰ガ�"

@ 12, 50 GET m.medequip   PICTURE "@*C �イ. �｀珮ぎ���┘"
@ 13, 50 GET m.auto       PICTURE "@*C �≒�-皀絖┴�"
@ 14, 50 GET m.telecom    PICTURE "@*C �カオ���祗┴�罔�"
@ 15, 50 GET m.radio      PICTURE "@*C ｡�珮�.��え�▲�.� TV"
@ 16, 50 GET m.navig      PICTURE "@*C ��※���.�｀珮ぎ���┘"
@ 17, 54 GET m.p_other

@ 20, 12 GET m.note
@ 21, 20 SAY "�キイΕ� " GET manager_n WHEN .F.

crsr = SET("CURSOR") = "OFF"
SET CURSOR ON
@ 22, 23 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "
READ CYCLE VALID Tst_Blank()
IF ex = 1
  m.who = user
  m.where = arm
  m.cus_code = parmCode
  m.name     = tmpName
  APPEND BLANK
  m.kind_code = k_code
  GATHER MEMVAR
  REPLACE CUS_CODE WITH m.parmCode
ENDIF
IF crsr
  SET CURSOR OFF
ENDIF

USE
SELECT KIND_2901
USE
SELECT ACC_2901
USE
SELECT INFO_2901
APPEND FROM (C000)
USE
RELEASE WINDOW H_Wind
RELEASE WINDOW Cli_Info
SELECT (sel_sav)
POP KEY

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Ch_Kind      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                          �襦�� 皋�� ��┘���.                           �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 20.03.2001 陳�
PROCEDURE Ch_Kind

IF .NOT. k_sw
  RETURN .T.
ENDIF

k_sw = .F.
SHOW GET k_sw

DO Cli_Kind WITH k_code, k_name

SHOW GET k_name
RETURN .T.
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�                                                                        �
*�                            �諷�� ├ ゛����.                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 25.03.2003 陳�
PROCEDURE Tst_Blank
PRIVATE mss

IF EMPTY(m.country) .AND. ex = 1
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��Б��┘ 痰���� ぎ�Ν� °碎 ������キ�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.
