*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Ch_Class     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                       �襦�� ���瘁� ���キ���矣琺.                       �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 03.09.1999 様�
PROCEDURE Ch_Class
PARAMETERS c_code, c_name

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
PRIVATE i, flt_code, sav_code
s_sav = SELECT()
flt_code = ALLTRIM(c_code)

SELECT 0
DO Use_Dummy
SELECT 0
USE (base_path+"CLASSES") ORDER TAG CODE DESCENDING ALIAS Cl_9903 AGAIN
SET FILTER TO EMPTY(DATE_OFF)
IF .NOT. SEEK(flt_code) .OR. EMPTY(flt_code)
  flt_code = ""
ENDIF
SELECT 0
CREATE DBF (tmpo_path+"TMP____1") ;
   ( CODE C(12),  ;
     NAME C(FSIZE("NAME", "Cl_9903")+2), ;
     DEL  L )
INDEX ON SYS(15, lwr, NAME) TAG NAME
FOR i = 2 TO 6
  COPY STRUCTURE TO (tmpo_path+"TMP____"+STR(i,1)) WITH CDX
ENDFOR
FOR i = 2 TO 6
  SELECT 0
  USE (tmpo_path+"TMP____"+STR(i,1)) ORDER TAG NAME EXCLUSIVE
ENDFOR

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�襦��碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Dn_Level OF (menu_name) PROMPT "��イ. 窶�▲��" KEY Ctrl-A, "Tab"
ON SELECTION PAD Dn_Level OF (menu_name) DO Sw_Mode WITH "Dn_Level"

DEFINE PAD Up_Level OF (menu_name) PROMPT "踳�. 窶�▲��" KEY Ctrl-A, "Shift-Tab"
ON SELECTION PAD Up_Level OF (menu_name) DO Sw_Mode WITH "Up_Level"

DEFINE PAD Exit OF (menu_name) PROMPT "�皖���碎瘴" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = WROWS("")-15   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 66

DO D_Wins WITH ln, wd, "���瘁�", 0, 6
@ WROWS()-8, 3 SAY " �牀↓� "
sav_code = flt_code
flt_code = ""
DO WHILE .T.
  DO Prp_Level
  flt_code = LEFT(sav_code, LEN(flt_code)+2)
  LOCATE FOR ALLTRIM(CODE) = flt_code
  IF .NOT. FOUND()
    GO TOP
  ENDIF
  @ WROWS()-7+LEN(flt_code)/2-1, 2 SAY LEFT(NAME,64)
  IF LEN(flt_code) >= LEN(sav_code)
    EXIT
  ENDIF
ENDDO
flt_code = LEFT(sav_code, LEN(sav_code)-2)
@ WROWS()-7+LEN(flt_code)/2, WCOLS()-3 SAY ""
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3

    DO Prp_Nav_1
    ON KEY LABEL Enter   KEYBOARD CHR(23)
    ON KEY LABEL Tab     DO Sw_Mode WITH "Dn_Level"
    ON KEY LABEL BackTab DO Sw_Mode WITH "Up_Level"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS NAME:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && �襦��

    c_code = CODE
    IF TYPE("c_name") = "C"
      c_name = LEFT(NAME,64)
    ENDIF
    EXIT

  CASE what_do = "Dn_Level"  && ��イ. 窶�▲��

    DO Dn_Level
    what_do = "List"

  CASE what_do = "Up_Level"  && 踳�. 窶�▲��

    DO Up_Level
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

SELECT Cl_9903
USE
FOR i = 1 TO 6
  flt_code = "TMP____"+STR(i,1)
  SELECT (flt_code)
  USE
  DELETE FILE (tmpo_path+flt_code+".DBF")
  DELETE FILE (tmpo_path+flt_code+".CDX")
ENDFOR

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
@ WROWS()-7+LEN(flt_code)/2, 2 SAY LEFT(NAME, FSIZE("NAME")-2)
ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Prp_Level    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�             ��爼�牀���┘ ∇ガキ���� ����� 皀�竕ィ� 窶�↓�.             �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 03.09.1999 陳�
PROCEDURE Prp_Level

PRIVATE a_name, ln, sw_char

a_name = "TMP____"+STR(LEN(flt_code)/2+1,1)
SELECT Cl_9903
ln = LEN(flt_code)
SEEK LEFT(flt_code,ln)
sw_char = "  "
SCAN REST WHILE LEFT(CODE, ln) = LEFT(flt_code,ln) ;
     FOR .NOT. EMPTY(SUBSTR(CODE, ln+1, 2))
  IF .NOT. EMPTY(SUBSTR(CODE, ln+3))
    sw_char = " "
    LOOP
  ENDIF
  SELECT (a_name)
  APPEND BLANK
  REPLACE CODE WITH Cl_9903.CODE, ;
          NAME WITH Cl_9903.NAME+sw_char, ;
          DEL  WITH .NOT. EMPTY(Cl_9903.DATE_OFF)
  SELECT Cl_9903
  sw_char = "  "
ENDSCAN

SELECT (a_name)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Dn_Level     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           �� 窶�▲�� ↓├...                           �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 03.09.1999 陳�
PROCEDURE Dn_Level

IF RIGHT(NAME, 1) = " "
  RETURN
ENDIF

ACTIVATE WINDOW (win_name+"_M") SAME
@ WROWS()-7+LEN(flt_code)/2, WCOLS()-3 SAY " "
flt_code = ALLTRIM(CODE)
DO Prp_Level
@ WROWS()-7+LEN(flt_code)/2, WCOLS()-3 SAY ""

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Up_Level     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           �� 窶�▲�� □ム�...                          �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 03.09.1999 陳�
PROCEDURE Up_Level
PRIVATE a_name
IF LEN(flt_code) = 0
  RETURN
ENDIF

ACTIVATE WINDOW (win_name+"_M") SAME
@ WROWS()-7+LEN(flt_code)/2, 2 CLEAR TO WROWS()-2, WCOLS()-3
flt_code = LEFT(flt_code, LEN(flt_code)-2)
ZAP
a_name = "TMP____"+STR(LEN(flt_code)/2+1,1)
SELECT (a_name)

@ WROWS()-7+LEN(flt_code)/2, WCOLS()-3 SAY ""

RETURN
