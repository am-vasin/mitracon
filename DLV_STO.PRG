*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Dlv_Sto      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                         ���キ���矣�� ぎ痰�→�.                         �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 29.03.2000 様�
PROCEDURE Dlv_Sto

PRIVATE chr_shab, int_1, int_2, tmpValue
DIMENSION int_1(2), int_2(2)   && ｯ����┘ �祗�皰� �キ� か� �牀������ ���瓷�
int_1(1) = 27      && Esc
int_2(1) = "{Esc}"
int_1(2) = -6      && F7
int_2(2) = "{F7}"
chr_shab = ' !"'+    ;
           "#$%&'()*+,-./0123456789:;<=>?@"+  ;
           "ABCDEFGHIJKLMNOPQRSTUVWXYZ"+         ;
           "[\]^_`"+                             ;
           "abcdefghijklmnopqrstuvwxyz"+         ;
           "{|}~"+                               ;
           "��������������������������������"+   ;
           "�、�ぅΗ┤������珀矣粤肄蓍裨跋鈿"

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
USE (base_path+"DLV_STO") ORDER TAG CODE ALIAS DSTO_0329 AGAIN

SELECT 0
USE (base_path+"STOCK") ORDER TAG NAME ALIAS STO_0329 AGAIN
SET RELATION TO CODE INTO DSTO_0329

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�痰�※碎/�瓷�鈑�碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Seek OF (menu_name) PROMPT "���皀�痰�覃 ���瓷" KEY Ctrl-A, "F7"
ON SELECTION PAD Seek OF (menu_name) DO Sw_Mode WITH "Seek"

DEFINE PAD Stock OF (menu_name) PROMPT "���讓� ぎ痰�→�" KEY Ctrl-A, "F2"
ON SELECTION PAD Stock OF (menu_name) DO Sw_Mode WITH "Stock"

DEFINE PAD Exit OF (menu_name) PROMPT "�皖���碎瘴" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE tmpName, seek_shab

seek_shab = SPACE(25)
PRIVATE ln, wd
ln = WROWS("")-8   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 51
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�.... ....:....!....:....!....: ....:... V�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

DO D_Wins WITH ln, wd, "���キ���矣�� ぎ痰�→�", 1, 0
@ 1, 3 SAY "���瓷:"
@ 1, 10 SAY PADR("Tab",WCOLS()-10) COLOR (SCHEME(14,2))

what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Tab DO Seek_
    ON KEY LABEL F7  DO Sw_Mode WITH "Seek"
    ON KEY LABEL F2  DO Sw_Mode WITH "Stock"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS PREFIX:4:H="",   ;
                  NAME:35:H="",    ;
                  PRODUCER:8:H="", ;
                  MARK = IIF(FOUND("DSTO_0329"), "�", " "):1:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && �襦��

    IF .NOT. FOUND("DSTO_0329")
      SELECT DSTO_0329
      IF .NOT. SEEK(0)
        APPEND BLANK
      ENDIF
      REPLACE CODE WITH STO_0329.CODE, ;
              NAME WITH ALLTRIM(STO_0329.PREFIX)+ALLTRIM(STO_0329.NAME)
      SELECT STO_0329
      GO RECNO()
    ELSE
      REPLACE DSTO_0329.CODE WITH 0
    ENDIF
        
    what_do = "List"

  CASE what_do = "Seek"      && ���皀�痰�覃 ���瓷

    tmpValue = Seek_C()
    IF tmpValue > 0
      GO tmpValue
    ENDIF
    what_do = "List"

  CASE what_do = "Stock"      && ���讓� ぎ痰�→�

    DO Sto_Dlv
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE WINDOW (win_name+"_E1")
RELEASE WINDOW (win_name+"_I1")
RELEASE MENU (menu_name) EXTENDED
*
*   ���窶�皚キ讓� (���窶�皚キ讓�!) ���琺��ガ DBF-�����.
*
SELECT DSTO_0329
USE
SELECT STO_0329
USE
SELECT (s_sav)

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                 蹍������ ���瓷� �� �����讚覓 ＜����.                  �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 06.05.97 22:05:53 夕
PROCEDURE Seek_

PRIVATE s, prf, ln, bg, rc_s, i

ln = 29
ACTIVATE WINDOW (win_name+"_M") SAME
prf = ""
@ 1, 3 SAY "���瓷:"
@ 1, 10 SAY PADR("Tab",ln) COLOR (SCHEME(14,2))
bg = REPLICATE("�", ln)
DO WHILE .T.
  @ 1, 10 SAY bg
  @ 1, 10 SAY prf
  s = INKEY(0)
  i = ASCAN(int_1, s)
  IF i # 0
    KEYBOARD int_2(i)
    EXIT
  ENDIF
  IF s < 0     && �祗�罔����讚�� ���※��
    IF s = -9  && F10 - ���竅��ガ �キ�
      KEYBOARD "{F10}"
      EXIT
    ENDIF
    LOOP
  ENDIF
  s = CHR(s)
  IF s $ chr_shab .AND. LEN(prf) < ln   && �━〓�
    rc_s = RECNO()
    IF SEEK(prf+s)
      prf = prf+s
    ELSE
      IF rc_s < RECCOUNT() .AND. rc_s > 0
        GO rc_s
      ENDIF
    ENDIF
    SHOW WINDOW (ALIAS()) REFRESH
    LOOP
  ENDIF
  IF s = CHR(13)       && Enter
    KEYBOARD CHR(23)
    EXIT
  ENDIF
  IF s = CHR(3)        && PgDn
    KEYBOARD "{PgDn}"
    EXIT
  ENDIF
  IF s = CHR(18)       && PgUp
    KEYBOARD "{PgUp}"
    EXIT
  ENDIF
  IF s = CHR(5)        && UpArrow
    KEYBOARD "{UpArrow}"
    EXIT
  ENDIF
  IF s = CHR(24)       && DnArrow
    KEYBOARD "{DnArrow}"
    EXIT
  ENDIF
  IF s = CHR(127)      && BackSpace
    IF LEN(prf) > 0
      prf = LEFT(prf, LEN(prf)-1)
      SEEK prf
      SHOW WINDOW (ALIAS()) REFRESH
    ENDIF
    LOOP
  ENDIF
ENDDO

@ 1, 10 SAY PADR("Tab",ln) COLOR (SCHEME(14,2))

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Seek_C       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           ���皀�痰�覃 ���瓷.                           �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 26.02.2000 陳�
PROCEDURE Seek_C

IF .NOT. Get_Shab()
  RETURN ""
ENDIF
IF EMPTY(seek_shab)
  RETURN ""
ENDIF

PRIVATE s_sav, tmpShab
tmpShab = ALLTRIM(seek_shab)
s_sav = SELECT()
SELECT 0
DO Use_Link
ZAP
SELECT 0
USE (base_path+"STOCK") ORDER TAG NAME ALIAS ST_0329 AGAIN
DO Wt_Mess WITH "���瓷..."
SCAN
  IF tmpShab $ ALLTRIM(PREFIX)+ALLTRIM(NAME)
    SELECT LINK
    APPEND BLANK
    REPLACE LNK WITH RECNO("ST_0329")
    SELECT ST_0329
  ENDIF
ENDSCAN
DO Wt_Mess

tmpShab = Ch_P()

SELECT ST_0329
USE
SELECT (s_sav)

RETURN tmpShab

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Get_Shab     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           ��゛�� か� ���瓷�.                           �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 26.02.2000 陳�
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
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ��ぅ爨�皀�讚�� ��痰� �牀������:
*

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�  敖 ��゛�� か� ���瓷� 陳陳陳朕  �
*�  � ....:....!....:....!....: �  �
*�  青陳陳陳陳陳陳陳陳陳陳陳陳陳�  �
*�< OK Ctrl-W > < �皖���碎瘴 Esc > �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 7, 37, "���皀�痰�覃 ���瓷"
DO Sun_Bord WITH  2,  4,  4, 32, " ��゛�� か� ���瓷� "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 3, 6 GET seek_shab
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Choose_P     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�               �襦�� ��Ж罔� ├ �皰÷���諷 �� ���皀�痰�.                �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 26.02.2000 陳�
PROCEDURE Ch_P

SET ORDER TO
SELECT LINK
IF RECCOUNT() = 0
  PRIVATE mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��腑�� �� ���ぅ��!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN ""
ENDIF

SET RELATION TO LNK INTO ST_0329
GO TOP

PRIVATE retVal
retVal = ""
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
ln = WROWS("")-10   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 49

DO D_Wins WITH ln, wd, "��Ж罔� ���キ���矣琺", 0, 0
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
    BROWSE FIELDS ST_0329.PREFIX:H="", ;
                  ST_0329.NAME:H="", ;
                  ST_0329.PRODUCER:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && �襦��

    retVal = LINK.LNK
    EXIT

  OTHERWISE

    retVal = 0
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

RETURN retVal
