*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Get_GTD      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                         �襦�� ��� か� ��Ж罔�.                         �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 26.02.2000 様�
PROCEDURE Get_GTD
PARAMETERS pPref, pName, pProd

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
USE (path_comm+"GTD_TIT") ORDER TAG GTD_NUM ALIAS TIT_0226 AGAIN

SELECT 0
USE (path_comm+"GTD") ORDER TAG NAME DESCENDING ALIAS DET_0226 AGAIN
SET RELATION TO GTD_NUM INTO TIT_0226
PRIVATE GTD_name, tmpName, retValue
GTD_name = PADR(ALLTRIM(pPref)+ALLTRIM(pName), FSIZE("NAME"))
tmpName  = IIF(EMPTY(pPref), "", ALLTRIM(pPref)+" ")+ALLTRIM(pName)+ ;
           IIF(EMPTY(pProd), "", " /"+ALLTRIM(pProd)+"/")
retValue = ""

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�襦��碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Seek OF (menu_name) PROMPT "���瓷" KEY Ctrl-A, "F7"
ON SELECTION PAD Seek OF (menu_name) DO Sw_Mode WITH "Seek"

DEFINE PAD Clear OF (menu_name) PROMPT "�腮痰�碎" KEY Ctrl-A, "蹍．�"
ON SELECTION PAD Clear OF (menu_name) DO Sw_Mode WITH "Clear"

DEFINE PAD Exit OF (menu_name) PROMPT "�皖���碎瘴" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd, tmpValue
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 65
*敖�����陳陳����ム ���陳陳陳陳陳陳陳陳陳�矗���陳陳陳陳陳陳����-〓陳�
*���.��.���� GTL----------------------- -----�矗���----- 99999999 *�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
DO D_Wins WITH ln, wd, "�襦�� ���", 2
@ 3, 2 SAY "陳����陳陳����ム ���陳陳陳陳陳陳陳陳陳�矗���陳陳陳陳陳陳����-〓"
what_do = "List"
PRIVATE lTmp
lTmp = LEN(ALLTRIM(GTD_name))
DO WHILE lTmp > 2
  IF SEEK(GTD_name)
    EXIT
  ENDIF
  lTmp = lTmp-1
  GTD_name = PADR(LEFT(GTD_name,lTmp), FSIZE("NAME"))
ENDDO

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    ACTIVATE WINDOW (win_name+"_M") SAME
    @ 1, 2 SAY "��Ж罔�    "+tmpName
    @ 2, 2 SAY "��� �� ��� "+GTD_name
    
    DO Prp_Nav_1
    ON KEY LABEL Enter    KEYBOARD CHR(23)
    ON KEY LABEL F7       DO Sw_Mode WITH "Seek"
    ON KEY LABEL SpaceBar DO Sw_Mode WITH "Clear"
    IF .NOT. SEEK(GTD_name)
      WAIT WINDOW "� ��� �モ ��ゅ�わ薀� ��Ж罔�!" NOWAIT
    ENDIF

*
*    BROWSE - �キ�
*
    BROWSE KEY GTD_name   ;
           FIELDS GTD_DATE:10:H="",  ;
                  GTD_NUM:26:H="",   ;
                  COUNTRY:16:H="",   ;
                  QNT:H="":P="@Z 99999999", ;
                  TIT_0226.GTD_TYPE:1:H="",   ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && �襦��

    IF .NOT. BOF() .AND. .NOT. EOF()
      retValue = GTD_NUM+CHR(13)+COUNTRY+CHR(13)+GTD_LINE+CHR(13)+COUNTRC
    ENDIF
    EXIT
    
  CASE what_do = "Clear"     && �腮痰�碎

*    IF .NOT. BOF() .AND. .NOT. EOF()
      retValue = " "+CHR(13)+" "
*    ENDIF
    EXIT
    
  CASE what_do = "Seek"      && ���瓷

    tmpValue = GTD_Sto(pPref, pName, pProd)
    IF .NOT. EMPTY(tmpValue)
      GTD_name = tmpValue
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
USE
SELECT TIT_0226
USE
SELECT (s_sav)

RETURN retValue
